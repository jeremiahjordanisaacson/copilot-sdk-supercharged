--- CopilotClient: manages the connection to the Copilot CLI server.
--
-- Spawns the CLI process, communicates over JSON-RPC 2.0 via stdio,
-- and provides session lifecycle management.
--
-- Usage:
--   local CopilotClient = require("copilot.client")
--   local client = CopilotClient.new()
--   client:start()
--   local session = client:create_session({ model = "gpt-4" })
--   -- ...
--   client:stop()

local cjson               = require("cjson")
local JsonRpcClient       = require("copilot.json_rpc_client")
local CopilotSession      = require("copilot.session")
local types               = require("copilot.types")
local sdk_protocol_version = require("copilot.sdk_protocol_version")
local session_params      = require("copilot.session_params")

-- ---------------------------------------------------------------------------
-- CopilotClient class
-- ---------------------------------------------------------------------------

local CopilotClient = {}
CopilotClient.__index = CopilotClient

--- Create a new CopilotClient.
-- @param options table|nil  ClientOptions (see types.ClientOptions)
-- @return CopilotClient
function CopilotClient.new(options)
    options = options or {}
    local self = setmetatable({}, CopilotClient)

    -- Resolve options with defaults
    local cli_path = options.cliPath or os.getenv("COPILOT_CLI_PATH") or "copilot"
    if cli_path == "" then
        cli_path = "copilot"
    end
    -- A .js entry point (such as @github/copilot/npm-loader.js) runs with node;
    -- anything else (the native binary or the npm bin shim) runs directly.
    if cli_path:match("%.js$") and not cli_path:match("^node%s") then
        cli_path = 'node "' .. cli_path .. '"'
    end
    self._cli_path       = cli_path
    self._cwd            = options.cwd
    self._log_level      = options.logLevel or "info"
    self._env            = options.env
    self._github_token   = options.githubToken
    self._use_logged_in  = options.useLoggedInUser
    self._auto_start     = (options.autoStart ~= false)
    self._auto_restart   = (options.autoRestart ~= false)
    self._client_info    = options.clientInfo

    -- Client mode: "coding-agent" (default) or "empty"; "copilot-cli" is a legacy alias.
    local mode = options.mode or types.ClientMode.CODING_AGENT
    if mode == types.ClientMode.COPILOT_CLI then
        mode = types.ClientMode.CODING_AGENT
    end
    if mode ~= types.ClientMode.CODING_AGENT and mode ~= types.ClientMode.EMPTY then
        error('mode must be "coding-agent" or "empty"', 2)
    end
    self._mode = mode

    -- Host network callbacks (NetworkConfig)
    self._network = options.network
    if self._network and self._network.scope ~= nil
        and self._network.scope ~= types.NetworkScope.CONNECTION
        and self._network.scope ~= types.NetworkScope.RUNTIME then
        error('network scope must be "connection" or "runtime"', 2)
    end

    -- State
    self._state          = types.ConnectionState.DISCONNECTED
    self._process        = nil    -- popen handle
    self._rpc_client     = nil    -- JsonRpcClient instance
    self._sessions       = {}     -- sessionId -> CopilotSession
    self._models_cache   = nil    -- cached array of ModelInfo
    self._stdin_handle   = nil
    self._stdout_handle  = nil
    self._read_thread    = nil    -- coroutine for the read loop
    self._on_get_trace_context = options.onGetTraceContext  -- optional trace context provider
    self._github_token_providers = {}  -- registrationId -> { provider = fn, sessionId = id }

    return self
end

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

--- Start the CLI server process and connect.
-- @return boolean success
-- @return string|nil error message
function CopilotClient:start()
    if self._state == types.ConnectionState.CONNECTED then
        return true, nil
    end

    self._state = types.ConnectionState.CONNECTING

    -- Build command line arguments
    local args = {
        self._cli_path,
        "--headless",
        "--no-auto-update",
        "--log-level", self._log_level,
        "--stdio",
    }

    -- Authentication flags
    if self._github_token and self._github_token ~= "" then
        table.insert(args, "--auth-token-env")
        table.insert(args, "COPILOT_SDK_AUTH_TOKEN")
    end

    local use_logged_in = true
    if self._use_logged_in ~= nil then
        use_logged_in = self._use_logged_in
    elseif self._github_token and self._github_token ~= "" then
        use_logged_in = false
    end
    if not use_logged_in then
        table.insert(args, "--no-auto-login")
    end

    -- Build command string
    local cmd = table.concat(args, " ")

    -- Cap the CLI process lifetime with coreutils `timeout` (POSIX, when
    -- available). Applied BEFORE prepending environment variables so the
    -- executable itself -- not a `VAR=value` env assignment -- is timeout's
    -- direct child, ensuring the process (and thus the stdout pipe) is actually
    -- killed if it stalls. Without this, a mismatched replay can block the
    -- blocking reader for the entire CI timeout. `-k` force-kills if the process
    -- ignores the initial signal.
    if package.config:sub(1, 1) == "/" then
        local has_timeout = os.execute("command -v timeout >/dev/null 2>&1")
        if has_timeout == true or has_timeout == 0 then
            cmd = "timeout -k 10 90 " .. cmd
        end
    end

    -- Prepend custom environment variables (from options.env)
    if self._env and type(self._env) == "table" then
        local is_windows = (package.config:sub(1, 1) == "\\")
        for k, v in pairs(self._env) do
            if is_windows then
                cmd = string.format("set %s=%s && %s", k, v, cmd)
            else
                cmd = string.format("%s=%s %s", k, v, cmd)
            end
        end
    end

    -- Set environment variable for auth token if needed
    if self._github_token and self._github_token ~= "" then
        -- On POSIX systems, we can prepend env vars
        if package.config:sub(1, 1) == "/" then
            cmd = string.format("COPILOT_SDK_AUTH_TOKEN=%s %s",
                self._github_token, cmd)
        else
            -- On Windows, use set command
            cmd = string.format("set COPILOT_SDK_AUTH_TOKEN=%s && %s",
                self._github_token, cmd)
        end
    end

    -- Open the process with bidirectional stdio via io.popen
    -- Lua 5.1/5.2 io.popen only gives one direction. We need a workaround.
    -- Strategy: use a pair of popen calls or the luaposix / LuaSocket approach.
    -- For maximum portability, we use os.tmpname + shell redirect approach:
    --   - Create a named pipe (FIFO) for writing to the process
    --   - popen gives us the reading end
    -- On Windows, we rely on a helper script or _popen2 if available.
    --
    -- Simplest cross-platform approach: use io.popen in read mode and write
    -- to a temporary fifo. But for the initial release, we use the Lua 5.3+
    -- io.popen("cmd", "w") for stdin and a separate mechanism for stdout.
    --
    -- PRACTICAL APPROACH: Use popen2-style function via os-level pipes.
    -- We provide a portable implementation using temporary files and coroutines.

    local ok, stdin_w, stdout_r, err = self:_spawn_process(cmd)
    if not ok then
        self._state = types.ConnectionState.ERROR
        return false, "Failed to start CLI server: " .. tostring(err)
    end

    self._stdin_handle  = stdin_w
    self._stdout_handle = stdout_r

    -- Create JSON-RPC client
    self._rpc_client = JsonRpcClient.new(stdin_w, stdout_r)

    -- Setup notification / request handlers
    self:_setup_handlers()

    -- Start the read loop in a coroutine
    self._read_thread = coroutine.create(function()
        self._rpc_client:run()
    end)

    -- Kick off the read loop coroutine (it will yield on I/O)
    -- For synchronous mode, the read loop runs inline inside request()

    -- Declare the integrating application's identity (connect handshake)
    local info_ok, info_err = self:_send_client_info()
    if not info_ok then
        self._state = types.ConnectionState.ERROR
        return false, info_err
    end

    -- Verify protocol version
    local ping_ok, ping_err = self:_verify_protocol_version()
    if not ping_ok then
        self._state = types.ConnectionState.ERROR
        return false, ping_err
    end

    -- Register host network callbacks, if any
    local net_ok, net_err = self:_register_network_provider()
    if not net_ok then
        self._state = types.ConnectionState.ERROR
        return false, net_err
    end

    self._state = types.ConnectionState.CONNECTED
    return true, nil
end

--- Stop the CLI server and clean up all sessions.
-- @return boolean success
-- @return string|nil error message
function CopilotClient:stop()
    local errors = {}

    -- Destroy all active sessions
    for session_id, session in pairs(self._sessions) do
        local ok, err = session:destroy()
        if not ok then
            table.insert(errors, string.format(
                "failed to destroy session %s: %s", session_id, tostring(err)
            ))
        end
    end
    self._sessions = {}

    -- Stop JSON-RPC client
    if self._rpc_client then
        self._rpc_client:stop()
        self._rpc_client = nil
    end

    -- Close process handles
    if self._stdin_handle then
        pcall(function() self._stdin_handle:close() end)
        self._stdin_handle = nil
    end
    if self._stdout_handle then
        pcall(function() self._stdout_handle:close() end)
        self._stdout_handle = nil
    end
    if self._process then
        pcall(function() self._process:close() end)
        self._process = nil
    end

    -- Clear models cache
    self._models_cache = nil

    -- Drop all GitHub token provider registrations
    self._github_token_providers = {}

    self._state = types.ConnectionState.DISCONNECTED

    if #errors > 0 then
        return false, table.concat(errors, "; ")
    end
    return true, nil
end

--- Get the current connection state.
-- @return string  One of the ConnectionState values
function CopilotClient:state()
    return self._state
end

--- Ensure the client is connected, auto-starting if configured.
-- @return boolean success
-- @return string|nil error
function CopilotClient:_ensure_connected()
    if self._rpc_client then
        return true, nil
    end
    if self._auto_start then
        return self:start()
    end
    return false, "Client not connected. Call start() first."
end

--- Ping the server.
-- @param message string|nil  Optional message to echo
-- @return table|nil PingResponse
-- @return string|nil error
function CopilotClient:ping(message)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err = self._rpc_client:request("ping", {
        message = message or "",
    })
    if req_err then
        return nil, req_err
    end
    return result, nil
end

--- Get server status.
-- @return table|nil GetStatusResponse
-- @return string|nil error
function CopilotClient:get_status()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    return self._rpc_client:request("status.get", {})
end

--- Get authentication status.
-- @return table|nil GetAuthStatusResponse
-- @return string|nil error
function CopilotClient:get_auth_status()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    return self._rpc_client:request("auth.getStatus", {})
end

--- List available models (cached after first call).
-- @return table|nil  Array of ModelInfo
-- @return string|nil error
function CopilotClient:list_models()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    if self._models_cache then
        return self._models_cache, nil
    end

    local result, req_err = self._rpc_client:request("models.list", {})
    if req_err then
        return nil, req_err
    end

    if result and result.models then
        self._models_cache = result.models
        return result.models, nil
    end
    return {}, nil
end

--- Get the ID of the most recently updated session.
-- @return string|nil sessionId
-- @return string|nil error
function CopilotClient:get_last_session_id()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err = self._rpc_client:request("session.getLastId", {})
    if req_err then
        return nil, req_err
    end
    return result and result.sessionId, nil
end

--- Get metadata for a session by ID.
-- @param session_id string  The session ID
-- @return table|nil metadata
-- @return string|nil error
function CopilotClient:get_session_metadata(session_id)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err = self._rpc_client:request("session.getMetadata", {
        sessionId = session_id,
    })
    if req_err then
        return nil, req_err
    end
    return result, nil
end

--- Create a new session.
-- @param config table|nil  SessionConfig (see types.SessionConfig)
-- @return CopilotSession|nil
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data, policyHelperFailure }
--   when session.create failed with an error response
function CopilotClient:create_session(config)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    config = config or {}
    local valid, valid_err = self:_validate_session_config(config)
    if not valid then return nil, valid_err end

    local registration_id, reg_err = self:_register_github_token_provider(config, config.sessionId)
    if reg_err then return nil, reg_err end

    local req = self:_build_create_session_request(config)
    if registration_id then
        req.gitHubTokenProviderRegistrationId = registration_id
    end
    -- SDK protocol 4: move the flat options into their option groups.
    req = session_params.group(req, false)
    if self._mode == types.ClientMode.EMPTY then
        session_params.apply_empty_mode_defaults(req)
    end

    local result, req_err, rpc_err = self._rpc_client:request("session.create", req)
    if req_err then
        self:_unregister_github_token_provider(registration_id)
        return nil, "failed to create session: " .. tostring(req_err), self:_session_open_error(rpc_err)
    end

    if not result or not result.sessionId then
        self:_unregister_github_token_provider(registration_id)
        return nil, "invalid create session response"
    end

    local session = CopilotSession.new(
        result.sessionId,
        self._rpc_client,
        result.workspacePath
    )

    -- Register tools, permission handler, user input handler, hooks
    self:_register_session_handlers(session, config, registration_id)

    self._sessions[result.sessionId] = session
    return session, nil
end

--- Resume an existing session.
-- @param session_id string  The session ID to resume
-- @param config table|nil   ResumeSessionConfig
-- @return CopilotSession|nil
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data, policyHelperFailure }
--   when session.resume failed with an error response
function CopilotClient:resume_session(session_id, config)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    config = config or {}
    local valid, valid_err = self:_validate_session_config(config)
    if not valid then return nil, valid_err end

    local registration_id, reg_err = self:_register_github_token_provider(config, session_id)
    if reg_err then return nil, reg_err end

    local req = self:_build_resume_session_request(session_id, config)
    if registration_id then
        req.gitHubTokenProviderRegistrationId = registration_id
    end
    -- SDK protocol 4: move the flat options into their option groups.
    req = session_params.group(req, true)
    if self._mode == types.ClientMode.EMPTY then
        session_params.apply_empty_mode_defaults(req)
    end

    local result, req_err, rpc_err = self._rpc_client:request("session.resume", req)
    if req_err then
        self:_unregister_github_token_provider(registration_id)
        return nil, "failed to resume session: " .. tostring(req_err), self:_session_open_error(rpc_err)
    end

    if not result or not result.sessionId then
        self:_unregister_github_token_provider(registration_id)
        return nil, "invalid resume session response"
    end

    local session = CopilotSession.new(
        result.sessionId,
        self._rpc_client,
        result.workspacePath
    )

    self:_register_session_handlers(session, config, registration_id)

    self._sessions[result.sessionId] = session
    return session, nil
end

--- Clear the runtime's cached managed (enterprise policy) settings so they are
-- re-read on next use.
-- @return table|nil result
-- @return string|nil error
function CopilotClient:clear_managed_settings_cache()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    return self._rpc_client:request("managedSettings.clearCache", {})
end

--- Save a legacy MCP configuration credential in the runtime-owned secret store
-- (mcp.secrets.save). Requires Copilot CLI 1.0.116 or newer.
-- @param secret_id string  Identifier of the credential
-- @param value string      The credential; it is never logged
-- @param config_dir string|nil  Configuration directory (the runtime's default when nil)
-- @return table|nil McpSecretsSaveResult { usedKeychain }; usedKeychain is true when the
--   OS keychain stored the credential and false for the file backend
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data }
function CopilotClient:mcp_secrets_save(secret_id, value, config_dir)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err, rpc_err = self._rpc_client:request("mcp.secrets.save", {
        secretId  = secret_id,
        value     = value,
        configDir = config_dir,
    })
    if req_err then
        return nil, req_err, rpc_err
    end
    return types.McpSecretsSaveResult(type(result) == "table" and result or {}), nil
end

--- Delete a legacy MCP configuration credential from the runtime-owned secret store
-- (mcp.secrets.delete). Requires Copilot CLI 1.0.116 or newer.
-- @param secret_id string  Identifier of the credential
-- @param config_dir string|nil  Configuration directory (the runtime's default when nil)
-- @return boolean success
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data }
function CopilotClient:mcp_secrets_delete(secret_id, config_dir)
    local ok, err = self:_ensure_connected()
    if not ok then return false, err end

    local _, req_err, rpc_err = self._rpc_client:request("mcp.secrets.delete", {
        secretId  = secret_id,
        configDir = config_dir,
    })
    if req_err then
        return false, req_err, rpc_err
    end
    return true, nil
end

--- Delete the legacy credentials indexed for an MCP server (mcp.secrets.deleteServer).
-- Requires Copilot CLI 1.0.116 or newer.
-- @param server_name string  The MCP server name
-- @param config_dir string|nil  Configuration directory (the runtime's default when nil)
-- @return boolean success
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data }
function CopilotClient:mcp_secrets_delete_server(server_name, config_dir)
    local ok, err = self:_ensure_connected()
    if not ok then return false, err end

    local _, req_err, rpc_err = self._rpc_client:request("mcp.secrets.deleteServer", {
        serverName = server_name,
        configDir  = config_dir,
    })
    if req_err then
        return false, req_err, rpc_err
    end
    return true, nil
end

--- Persist a pending plugin extension permission request as a user-scoped approval,
-- before the host answers that request normally (permissions.extensionApprovals.grant).
-- Requires Copilot CLI 1.0.116 or newer.
-- @param session_id string  Session that owns the pending request
-- @param request_id string  Id of the pending extension permission request
-- @return table|nil PermissionsExtensionApprovalsGrantResult { granted, approval }
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data }
function CopilotClient:extension_approvals_grant(session_id, request_id)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err, rpc_err = self._rpc_client:request("permissions.extensionApprovals.grant", {
        sessionId = session_id,
        requestId = request_id,
    })
    if req_err then
        return nil, req_err, rpc_err
    end
    return types.PermissionsExtensionApprovalsGrantResult(type(result) == "table" and result or {}), nil
end

--- List the user-scoped plugin extension approvals (permissions.extensionApprovals.list).
-- Requires Copilot CLI 1.0.116 or newer.
-- @param session_id string|nil  Attached session whose configuration profile to list
--   (the server's profile when nil)
-- @return table|nil PermissionsExtensionApprovalsListResult { approvals }
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data }
function CopilotClient:extension_approvals_list(session_id)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err, rpc_err = self._rpc_client:request("permissions.extensionApprovals.list", {
        sessionId = session_id,
    })
    if req_err then
        return nil, req_err, rpc_err
    end
    return types.PermissionsExtensionApprovalsListResult(type(result) == "table" and result or {}), nil
end

--- Revoke one user-scoped plugin extension approval (permissions.extensionApprovals.revoke).
-- Requires Copilot CLI 1.0.116 or newer.
-- @param extension_name string    The approval's extensionName
-- @param installation_key string  The approval's installationKey, unchanged
-- @param session_id string|nil    Attached session whose configuration profile to use
-- @return table|nil PermissionsExtensionApprovalsRevokeResult { revoked }
-- @return string|nil error
-- @return table|nil structured JSON-RPC error { code, message, data }
function CopilotClient:extension_approvals_revoke(extension_name, installation_key, session_id)
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err, rpc_err = self._rpc_client:request("permissions.extensionApprovals.revoke", {
        extensionName   = extension_name,
        installationKey = installation_key,
        sessionId       = session_id,
    })
    if req_err then
        return nil, req_err, rpc_err
    end
    return types.PermissionsExtensionApprovalsRevokeResult(type(result) == "table" and result or {}), nil
end

--- List all sessions.
-- @return table|nil  Array of SessionMetadata
-- @return string|nil error
function CopilotClient:list_sessions()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err = self._rpc_client:request("session.list", {})
    if req_err then
        return nil, req_err
    end

    if result and result.sessions then
        return result.sessions, nil
    end
    return {}, nil
end

--- Sets the session filesystem provider configuration.
-- @param config table|nil  Optional table with keys: initialCwd, sessionStatePath, conventions
-- @return boolean success
-- @return string|nil error
function CopilotClient:set_session_fs_provider(config)
    local ok, err = self:_ensure_connected()
    if not ok then return false, err end

    config = config or {}
    local params = {}
    if config.initialCwd then
        params.initialCwd = config.initialCwd
    end
    if config.sessionStatePath then
        params.sessionStatePath = config.sessionStatePath
    end
    if config.conventions then
        params.conventions = config.conventions
    end

    local _, req_err = self._rpc_client:request("sessionFs.setProvider", params)
    if req_err then
        return false, "failed to set session fs provider: " .. tostring(req_err)
    end

    return true, nil
end

--- Delete a session permanently.
-- @param session_id string
-- @return boolean success
-- @return string|nil error
function CopilotClient:delete_session(session_id)
    local ok, err = self:_ensure_connected()
    if not ok then return false, err end

    local result, req_err = self._rpc_client:request("session.delete", {
        sessionId = session_id,
    })
    if req_err then
        return false, req_err
    end

    if result and result.success then
        self._sessions[session_id] = nil
        return true, nil
    end

    local error_msg = "unknown error"
    if result and result.error then
        error_msg = result.error
    end
    return false, string.format("failed to delete session %s: %s", session_id, error_msg)
end

--- Get the foreground session ID (TUI+server mode only).
-- @return string|nil sessionId
-- @return string|nil error
function CopilotClient:get_foreground_session_id()
    local ok, err = self:_ensure_connected()
    if not ok then return nil, err end

    local result, req_err = self._rpc_client:request("session.getForeground", {})
    if req_err then
        return nil, req_err
    end
    return result and result.sessionId, nil
end

--- Set the foreground session (TUI+server mode only).
-- @param session_id string
-- @return boolean success
-- @return string|nil error
function CopilotClient:set_foreground_session_id(session_id)
    local ok, err = self:_ensure_connected()
    if not ok then return false, err end

    local result, req_err = self._rpc_client:request("session.setForeground", {
        sessionId = session_id,
    })
    if req_err then
        return false, req_err
    end
    if result and result.success then
        return true, nil
    end
    return false, result and result.error or "unknown error"
end

-- ---------------------------------------------------------------------------
-- Internal: Process spawning
-- ---------------------------------------------------------------------------

--- Spawn the CLI process with bidirectional stdio.
-- Returns write handle (to process stdin) and read handle (from process stdout).
--
-- This uses a cross-platform approach:
--   - On POSIX: uses popen with a FIFO for the write direction
--   - On Windows: uses a temp file relay approach
--
-- @param cmd string  The command to execute
-- @return boolean success
-- @return file|nil  write handle (to process stdin)
-- @return file|nil  read handle (from process stdout)
-- @return string|nil error
function CopilotClient:_spawn_process(cmd)
    local is_windows = (package.config:sub(1, 1) == "\\")

    if is_windows then
        return self:_spawn_process_windows(cmd)
    else
        return self:_spawn_process_posix(cmd)
    end
end

--- POSIX process spawn using a named pipe (FIFO).
function CopilotClient:_spawn_process_posix(cmd)
    -- Create a FIFO for writing to the process's stdin
    local fifo_path = os.tmpname() .. "_copilot_fifo"
    local mkfifo_ok = os.execute("mkfifo " .. fifo_path .. " 2>/dev/null")
    if not mkfifo_ok then
        -- Fallback: use /tmp path
        fifo_path = "/tmp/copilot_sdk_fifo_" .. tostring(os.time()) .. "_" .. tostring(math.random(100000))
        os.execute("mkfifo " .. fifo_path)
    end

    -- Launch the process: stdin reads from fifo, stdout is captured by popen
    local full_cmd = string.format("%s < %s", cmd, fifo_path)
    local stdout_handle = io.popen(full_cmd, "r")
    if not stdout_handle then
        os.remove(fifo_path)
        return false, nil, nil, "failed to popen CLI process"
    end

    -- Open the FIFO for writing (this will block until the reader is connected,
    -- which happens when the process starts reading from stdin)
    local stdin_handle = io.open(fifo_path, "w")
    if not stdin_handle then
        stdout_handle:close()
        os.remove(fifo_path)
        return false, nil, nil, "failed to open FIFO for writing"
    end

    self._process = stdout_handle
    self._fifo_path = fifo_path

    return true, stdin_handle, stdout_handle, nil
end

--- Windows process spawn using a temp file relay.
-- On Windows, we use io.popen with read mode and write to a temp file
-- that is piped to the process stdin.
function CopilotClient:_spawn_process_windows(cmd)
    -- On Windows, use a simpler approach with named pipes via cmd.exe
    -- We create a background process and communicate via temp files.
    -- For a production SDK, one would use luaposix or a C extension.
    --
    -- Practical approach: use io.popen in read mode. For writes, we
    -- create a wrapper that sends data via a temp file and pipe.

    -- Create temp file for stdin relay
    local stdin_pipe_path = os.tmpname()

    -- The command reads from the pipe file
    -- We use 'type' on Windows to feed the pipe
    local full_cmd = string.format('cmd /c "type %s | %s"', stdin_pipe_path, cmd)

    -- Open stdin pipe for writing
    local stdin_handle = io.open(stdin_pipe_path, "w")
    if not stdin_handle then
        return false, nil, nil, "failed to create stdin temp file"
    end

    local stdout_handle = io.popen(full_cmd, "r")
    if not stdout_handle then
        stdin_handle:close()
        os.remove(stdin_pipe_path)
        return false, nil, nil, "failed to popen CLI process on Windows"
    end

    self._process = stdout_handle
    self._stdin_pipe_path = stdin_pipe_path

    return true, stdin_handle, stdout_handle, nil
end

-- ---------------------------------------------------------------------------
-- Internal: handler setup
-- ---------------------------------------------------------------------------

--- Set up JSON-RPC handlers for server-initiated requests and notifications.
function CopilotClient:_setup_handlers()
    -- session.event notification
    self._rpc_client:set_request_handler("session.event", function(params)
        self:_handle_session_event(params)
        return nil, nil
    end)

    -- session.lifecycle notification
    self._rpc_client:set_request_handler("session.lifecycle", function(params)
        -- Lifecycle events are informational; could add handlers here
        return nil, nil
    end)

    -- tool.call request
    self._rpc_client:set_request_handler("tool.call", function(params)
        return self:_handle_tool_call(params)
    end)

    -- permission.request
    self._rpc_client:set_request_handler("permission.request", function(params)
        return self:_handle_permission_request(params)
    end)

    -- userInput.request
    self._rpc_client:set_request_handler("userInput.request", function(params)
        return self:_handle_user_input_request(params)
    end)

    -- hooks.invoke
    self._rpc_client:set_request_handler("hooks.invoke", function(params)
        return self:_handle_hooks_invoke(params)
    end)

    -- exitPlanMode.request
    self._rpc_client:set_request_handler("exitPlanMode.request", function(params)
        return self:_handle_exit_plan_mode_request(params)
    end)

    -- gitHubToken.getToken (sessions created with a gitHubTokenProvider)
    self._rpc_client:set_request_handler("gitHubToken.getToken", function(params)
        return self:_handle_github_token_request(params)
    end)

    -- shellAuthentication.request / .cancel (sessions with onShellAuthenticationRequest)
    self._rpc_client:set_request_handler("shellAuthentication.request", function(params)
        return self:_handle_shell_authentication_request(params), nil
    end)
    self._rpc_client:set_request_handler("shellAuthentication.cancel", function(params)
        if type(params) == "table" and type(params.sessionId) == "string"
            and type(params.requestId) == "string" then
            local session = self._sessions[params.sessionId]
            if session then
                session:_cancel_shell_authentication_request(params.requestId)
            end
        end
        return {}, nil
    end)

    -- memoryStore.store (sessions with storeMemory); answers JSON null
    self._rpc_client:set_request_handler("memoryStore.store", function(params)
        return self:_handle_memory_store_request(params)
    end)

    -- network.* callbacks (client network option)
    self:_setup_network_handlers()

    -- sessionFs.* provider requests (serviced against the real filesystem)
    self._rpc_client:set_request_handler("sessionFs.readFile", function(params)
        return self:_handle_session_fs_read_file(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.writeFile", function(params)
        return self:_handle_session_fs_write_file(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.appendFile", function(params)
        return self:_handle_session_fs_append_file(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.exists", function(params)
        return self:_handle_session_fs_exists(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.stat", function(params)
        return self:_handle_session_fs_stat(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.mkdir", function(params)
        return self:_handle_session_fs_mkdir(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.readdir", function(params)
        return self:_handle_session_fs_readdir(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.readdirWithTypes", function(params)
        return self:_handle_session_fs_readdir_with_types(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.rm", function(params)
        return self:_handle_session_fs_rm(params)
    end)
    self._rpc_client:set_request_handler("sessionFs.rename", function(params)
        return self:_handle_session_fs_rename(params)
    end)
end

--- Dispatch a session event to the appropriate session.
function CopilotClient:_handle_session_event(params)
    if not params or not params.sessionId then
        return
    end
    local session = self._sessions[params.sessionId]
    if session then
        session:_dispatch_event(params.event)
    end
end

--- Handle a tool call request from the server.
function CopilotClient:_handle_tool_call(params)
    if not params or not params.sessionId or not params.toolCallId or not params.toolName then
        return nil, { code = -32602, message = "invalid tool call payload" }
    end

    local session = self._sessions[params.sessionId]
    if not session then
        return nil, { code = -32602, message = "unknown session " .. tostring(params.sessionId) }
    end

    local handler = session:_get_tool_handler(params.toolName)
    if not handler then
        return {
            result = {
                textResultForLlm = string.format(
                    "Tool '%s' is not supported by this client instance.", params.toolName
                ),
                resultType    = "failure",
                error         = string.format("tool '%s' not supported", params.toolName),
                toolTelemetry = {},
            },
        }, nil
    end

    -- Execute the tool handler
    local invocation = {
        sessionId  = params.sessionId,
        toolCallId = params.toolCallId,
        toolName   = params.toolName,
        arguments  = params.arguments,
    }

    local ok, result = pcall(handler, invocation)
    if not ok then
        return {
            result = {
                textResultForLlm = "Invoking this tool produced an error. Detailed information is not available.",
                resultType    = "failure",
                error         = tostring(result),
                toolTelemetry = {},
            },
        }, nil
    end

    -- Normalize result
    if type(result) == "string" then
        result = {
            textResultForLlm = result,
            resultType       = "success",
        }
    elseif type(result) ~= "table" then
        result = {
            textResultForLlm = tostring(result or ""),
            resultType       = "success",
        }
    end

    -- Ensure required fields
    if not result.resultType then
        result.resultType = "success"
    end
    if not result.textResultForLlm then
        result.textResultForLlm = ""
    end

    return { result = result }, nil
end

--- Handle a permission request from the server.
function CopilotClient:_handle_permission_request(params)
    if not params or not params.sessionId then
        return nil, { code = -32602, message = "invalid permission request payload" }
    end

    local session = self._sessions[params.sessionId]
    if not session then
        return nil, { code = -32602, message = "unknown session " .. tostring(params.sessionId) }
    end

    local handler = session:_get_permission_handler()
    if not handler then
        return {
            result = {
                kind = "denied-no-approval-rule-and-could-not-request-from-user",
            },
        }, nil
    end

    local permission_request = params.permissionRequest or {}
    local invocation = { sessionId = params.sessionId }

    local ok, result = pcall(handler, permission_request, invocation)
    if not ok then
        return {
            result = {
                kind = "denied-no-approval-rule-and-could-not-request-from-user",
            },
        }, nil
    end

    return { result = result }, nil
end

--- Handle a user input request from the server.
function CopilotClient:_handle_user_input_request(params)
    if not params or not params.sessionId or not params.question then
        return nil, { code = -32602, message = "invalid user input request payload" }
    end

    local session = self._sessions[params.sessionId]
    if not session then
        return nil, { code = -32602, message = "unknown session " .. tostring(params.sessionId) }
    end

    local handler = session:_get_user_input_handler()
    if not handler then
        return nil, { code = -32603, message = "no user input handler registered" }
    end

    local request = {
        question      = params.question,
        choices       = params.choices,
        allowFreeform = params.allowFreeform,
        requestId     = type(params.requestId) == "string" and params.requestId or nil,
        -- nil unless the runtime accepts image attachments in the answer
        imageAttachmentConstraints = types.UserInputImageAttachmentConstraints(params.imageAttachmentConstraints),
        _meta         = type(params._meta) == "table" and params._meta or nil,
    }
    local invocation = { sessionId = params.sessionId }

    local ok, result = pcall(handler, request, invocation)
    if not ok then
        return nil, { code = -32603, message = tostring(result) }
    end

    local response = {
        answer      = result.answer or "",
        wasFreeform = result.wasFreeform or false,
    }
    -- Image attachments are sent only when set; an empty Lua table would encode as {}
    if type(result.attachments) == "table" and #result.attachments > 0 then
        local attachments = {}
        for i, attachment in ipairs(result.attachments) do
            attachments[i] = types.UserInputImageAttachment(attachment)
        end
        response.attachments = attachments
    end
    return response, nil
end

--- Handle a hooks invocation from the server.
function CopilotClient:_handle_hooks_invoke(params)
    if not params or not params.sessionId or not params.hookType then
        return nil, { code = -32602, message = "invalid hooks invoke payload" }
    end

    local session = self._sessions[params.sessionId]
    if not session then
        return nil, { code = -32602, message = "unknown session " .. tostring(params.sessionId) }
    end

    local output, err = session:_handle_hooks_invoke(params.hookType, params.input)
    if err then
        return nil, { code = -32603, message = tostring(err) }
    end

    local result = {}
    if output then
        result.output = output
    end
    return result, nil
end

--- Handle an exit plan mode request from the server.
function CopilotClient:_handle_exit_plan_mode_request(params)
    if not params or not params.sessionId then
        return nil, { code = -32602, message = "invalid exit plan mode request payload" }
    end

    local session = self._sessions[params.sessionId]
    if not session then
        return nil, { code = -32602, message = "unknown session " .. tostring(params.sessionId) }
    end

    local handler = session:_get_exit_plan_mode_handler()
    if not handler then
        return { approved = true }, nil
    end

    local request = { sessionId = params.sessionId }
    local ok, result = pcall(handler, request)
    if not ok then
        return { approved = true }, nil
    end

    return result, nil
end

-- ---------------------------------------------------------------------------
-- Internal: v3.0.0 session callbacks, client mode and network callbacks
-- ---------------------------------------------------------------------------

--- Validate session callbacks and, in "empty" mode, require availableTools.
-- @return boolean ok
-- @return string|nil error
function CopilotClient:_validate_session_config(config)
    if config.onShellAuthenticationRequest ~= nil and type(config.onShellAuthenticationRequest) ~= "function" then
        return false, "onShellAuthenticationRequest must be a function"
    end
    if config.storeMemory ~= nil and type(config.storeMemory) ~= "function" then
        return false, "storeMemory must be a function"
    end
    if self._mode == types.ClientMode.EMPTY and config.availableTools == nil then
        return false, 'CopilotClient is in mode "empty" but the session config did not specify '
            .. "availableTools. Empty mode requires every session to explicitly opt into the "
            .. "tools it wants."
    end
    return true, nil
end

--- Register all session-scoped handlers on a newly created or resumed session.
function CopilotClient:_register_session_handlers(session, config, registration_id)
    session:_register_tools(config.tools)
    if config.onPermissionRequest then
        session:_register_permission_handler(config.onPermissionRequest)
    end
    if config.onUserInputRequest then
        session:_register_user_input_handler(config.onUserInputRequest)
    end
    if config.hooks then
        session:_register_hooks(config.hooks)
    end
    if config.onExitPlanMode then
        session:_register_exit_plan_mode_handler(config.onExitPlanMode)
    end
    if config.onShellAuthenticationRequest then
        session:_register_shell_authentication_handler(config.onShellAuthenticationRequest)
    end
    if config.storeMemory then
        session:_register_memory_store(config.storeMemory)
    end
    if self._on_get_trace_context then
        session:_register_trace_context_provider(self._on_get_trace_context)
    end
    if registration_id then
        local entry = self._github_token_providers[registration_id]
        if entry then
            entry.sessionId = session.session_id
        end
        session:_on_destroy(function()
            self._github_token_providers[registration_id] = nil
        end)
    end
end

--- Build the structured error returned as the third value of create_session /
-- resume_session, including a parsed policyHelperFailure when present.
function CopilotClient:_session_open_error(rpc_err)
    if type(rpc_err) ~= "table" then
        return nil
    end
    return {
        code                = rpc_err.code,
        message             = rpc_err.message,
        data                = rpc_err.data,
        policyHelperFailure = types.policy_helper_failure(rpc_err),
    }
end

--- Validate a session's gitHubTokenProvider and store it in the client registry.
-- @return string|nil registration id sent on the wire
-- @return string|nil error
function CopilotClient:_register_github_token_provider(config, session_id)
    local provider = config.gitHubTokenProvider
    if provider == nil then
        return nil, nil
    end
    if config.gitHubToken ~= nil then
        return nil, "gitHubToken and gitHubTokenProvider are mutually exclusive"
    end
    if type(provider) ~= "function" then
        return nil, "gitHubTokenProvider must be a function"
    end
    local registration_id = JsonRpcClient.generate_uuid()
    self._github_token_providers[registration_id] = { provider = provider, sessionId = session_id }
    return registration_id, nil
end

--- Release a GitHub token provider registration.
function CopilotClient:_unregister_github_token_provider(registration_id)
    if registration_id then
        self._github_token_providers[registration_id] = nil
    end
end

--- Answer gitHubToken.getToken with { kind = "token", accessToken, tokenType?, expiresIn }
-- or { kind = "cancelled" }.
function CopilotClient:_handle_github_token_request(params)
    local registration_id = type(params) == "table" and params.registrationId or nil
    local registration = type(registration_id) == "string" and self._github_token_providers[registration_id] or nil
    if not registration then
        return nil, {
            code = -32603,
            message = 'No GitHub token provider registered for registration ID "' .. tostring(registration_id) .. '"',
        }
    end

    local session_id = params.sessionId
    if type(session_id) ~= "string" then
        session_id = registration.sessionId
    end
    local args = { host = params.host, sessionId = session_id, reason = params.reason }
    local ok, result = pcall(registration.provider, args)
    if not ok then
        return nil, { code = -32603, message = "gitHubTokenProvider failed" }
    end
    if type(result) ~= "table" then
        return nil, { code = -32603, message = "gitHubTokenProvider must return a table" }
    end
    if result.kind == "cancelled" then
        return { kind = "cancelled" }, nil
    end
    local wire = {
        kind        = "token",
        accessToken = result.accessToken or result.access_token,
        expiresIn   = result.expiresIn or result.expires_in,
    }
    local token_type = result.tokenType or result.token_type
    if token_type then
        wire.tokenType = token_type
    end
    return wire, nil
end

--- Answer shellAuthentication.request. Malformed requests and unknown sessions cancel.
-- @return table { action = "accept", value } or { action = "cancel" }
function CopilotClient:_handle_shell_authentication_request(params)
    local input_kinds = { secret = true, text = true, confirm = true }
    if type(params) ~= "table"
        or type(params.sessionId) ~= "string" or type(params.requestId) ~= "string"
        or type(params.program) ~= "string" or type(params.prompt) ~= "string"
        or not input_kinds[params.inputKind] then
        return { action = "cancel" }
    end

    local session = self._sessions[params.sessionId]
    if not session then
        return { action = "cancel" }
    end

    return session:_handle_shell_authentication_request({
        sessionId = params.sessionId,
        requestId = params.requestId,
        program   = params.program,
        prompt    = params.prompt,
        inputKind = params.inputKind,
        command   = type(params.command) == "string" and params.command or "",
        cwd       = type(params.cwd) == "string" and params.cwd or "",
    })
end

--- Answer memoryStore.store by invoking the session's storeMemory callback
-- (JSON null on success).
function CopilotClient:_handle_memory_store_request(params)
    local session = type(params) == "table" and type(params.sessionId) == "string"
        and self._sessions[params.sessionId] or nil
    if not session then
        return nil, { code = -32603, message = "Memory store owner is unavailable" }
    end
    local ok, err = session:_handle_memory_store({
        sessionId = params.sessionId,
        subject   = params.subject,
        fact      = params.fact,
        citations = params.citations,
        reason    = params.reason,
    })
    if not ok then
        return nil, { code = -32603, message = tostring(err) }
    end
    return nil, nil
end

--- Declare the integrating application's identity on the connect handshake.
-- Skipped when no clientInfo field is set; older CLIs without connect are tolerated.
-- @return boolean ok
-- @return string|nil error
function CopilotClient:_send_client_info()
    local wire = types.client_info_to_wire(self._client_info)
    if not wire then
        return true, nil
    end
    local _, err = self._rpc_client:request("connect", { clientInfo = wire })
    if err and not tostring(err):find("-32601", 1, true)
        and not tostring(err):find("Unhandled method connect", 1, true) then
        return false, "connect failed: " .. tostring(err)
    end
    return true, nil
end

--- Run a network callback. Failures are reported by label only so that secrets
-- in an error message are never logged or sent back over the wire.
local function invoke_network_callback(label, callback, request)
    local ok, result = pcall(callback, request)
    if not ok then
        return nil, { code = -32603, message = label .. " callback failed" }
    end
    return result, nil
end

--- Register the host network callbacks with network.setProvider. An empty
-- object marks each callback the client answers; the scope is sent only when set.
-- @return boolean ok
-- @return string|nil error
function CopilotClient:_register_network_provider()
    local network = self._network
    if not network or not (network.resolveProxy or network.authenticateNegotiate or network.authenticateBasic) then
        return true, nil
    end
    local params = {}
    if network.scope then params.scope = network.scope end
    if network.resolveProxy then params.resolveProxy = {} end
    if network.authenticateNegotiate then params.authenticateNegotiate = {} end
    if network.authenticateBasic then params.authenticateBasic = {} end
    local _, err = self._rpc_client:request("network.setProvider", params)
    if err then
        return false, "network.setProvider failed: " .. tostring(err)
    end
    return true, nil
end

--- Register the server -> client handlers for the configured network callbacks.
function CopilotClient:_setup_network_handlers()
    local network = self._network
    if not network then
        return
    end

    if network.resolveProxy then
        self._rpc_client:set_request_handler("network.resolveProxy", function(params)
            local url = type(params) == "table" and params.url or nil
            local proxy, err = invoke_network_callback("Proxy resolution", network.resolveProxy, { url = url })
            if err then return nil, err end
            if type(proxy) == "string" and proxy ~= "" then
                return { proxy = proxy }, nil
            end
            return {}, nil
        end)
    end
    if network.authenticateNegotiate then
        self._rpc_client:set_request_handler("network.authenticateNegotiate", function(params)
            local spn = type(params) == "table" and params.spn or nil
            local token, err = invoke_network_callback("Negotiate authentication", network.authenticateNegotiate, { spn = spn })
            if err then return nil, err end
            if type(token) == "string" and token ~= "" then
                return { token = token }, nil
            end
            return {}, nil
        end)
    end
    if network.authenticateBasic then
        self._rpc_client:set_request_handler("network.authenticateBasic", function(params)
            params = type(params) == "table" and params or {}
            local realm = params.realm
            if type(realm) ~= "string" then realm = nil end
            local credentials, err = invoke_network_callback("Basic authentication", network.authenticateBasic, {
                proxy            = params.proxy,
                realm            = realm,
                previousRejected = params.previousRejected == true,
            })
            if err then return nil, err end
            -- Send only the credential fields, never other entries of the caller's table.
            local result = {}
            if type(credentials) == "table" then
                if credentials.username ~= nil then result.username = tostring(credentials.username) end
                if credentials.password ~= nil then result.password = tostring(credentials.password) end
            end
            return result, nil
        end)
    end
end

-- ---------------------------------------------------------------------------
-- Internal: sessionFs provider request handlers
-- ---------------------------------------------------------------------------
--
-- When a session filesystem provider is registered via
-- set_session_fs_provider, the CLI drives filesystem access by sending
-- "sessionFs.*" requests back to the client. These handlers service those
-- requests against the real filesystem using the absolute paths supplied by
-- the CLI. Errors are returned in-band as { code =, message = } (never raised)
-- so the CLI can distinguish a missing file (ENOENT) from other failures
-- without the JSON-RPC call itself failing, matching the reference SDK.
--
-- Lua's standard library lacks directory listing, recursive mkdir/remove and a
-- stat() that exposes type/size/mtime, so those primitives are serviced via
-- POSIX shell utilities (the SDK's supported test targets are POSIX).

-- Quote a path for safe single-quoted POSIX shell interpolation.
local function _sfs_quote(s)
    return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

-- Map an error message to a SessionFsError, classifying missing paths as ENOENT.
local function _sfs_error(message)
    local msg = tostring(message or "unknown error")
    local code = msg:lower():find("no such file or directory", 1, true) and "ENOENT" or "UNKNOWN"
    return { code = code, message = msg }
end

-- Return the parent directory portion of a path, or nil when there is none.
local function _sfs_dirname(path)
    return tostring(path):match("^(.*)[/\\][^/\\]+[/\\]?$")
end

-- Best-effort recursive parent directory creation (POSIX mkdir -p).
local function _sfs_mkparent(path)
    local dir = _sfs_dirname(path)
    if dir and dir ~= "" then
        os.execute("mkdir -p " .. _sfs_quote(dir))
    end
end

-- Existence check that works for both files and directories.
local function _sfs_path_exists(path)
    if not path then return false end
    if os.rename(path, path) then return true end
    local f = io.open(path, "r")
    if f then f:close(); return true end
    return false
end

-- Return "file", "directory", or nil for the given path (POSIX test).
local function _sfs_kind(path)
    local h = io.popen(
        "if [ -d " .. _sfs_quote(path) .. " ]; then echo directory; " ..
        "elif [ -e " .. _sfs_quote(path) .. " ]; then echo file; " ..
        "else echo none; fi 2>/dev/null"
    )
    if not h then return nil end
    local out = (h:read("*a") or ""):gsub("%s+$", "")
    h:close()
    if out == "directory" or out == "file" then return out end
    return nil
end

-- List a directory's entries (excluding . and ..) via POSIX ls.
local function _sfs_list(path)
    if _sfs_kind(path) ~= "directory" then
        return nil, "no such file or directory"
    end
    local h = io.popen("ls -1A " .. _sfs_quote(path) .. " 2>/dev/null")
    if not h then return nil, "cannot list directory" end
    local names = {}
    for line in h:lines() do
        if line ~= "" then names[#names + 1] = line end
    end
    h:close()
    return names
end

-- Return an ISO 8601 UTC timestamp string for the given epoch seconds.
local function _sfs_iso(epoch)
    return os.date("!%Y-%m-%dT%H:%M:%SZ", epoch or os.time())
end

-- Best-effort file modification time (epoch seconds) via POSIX stat.
local function _sfs_mtime(path)
    local h = io.popen(
        "stat -c %Y " .. _sfs_quote(path) .. " 2>/dev/null || " ..
        "stat -f %m " .. _sfs_quote(path) .. " 2>/dev/null"
    )
    if not h then return nil end
    local out = (h:read("*a") or ""):gsub("%s+$", "")
    h:close()
    return tonumber(out)
end

-- Tag a Lua table so cjson encodes it as a JSON array even when empty.
local function _sfs_as_array(t)
    if cjson.array_mt then
        setmetatable(t, cjson.array_mt)
    end
    return t
end

-- Format a numeric mode as an octal string for chmod, else pass through.
local function _sfs_mode_str(mode)
    if type(mode) == "number" then
        return string.format("%o", mode)
    end
    return tostring(mode)
end

function CopilotClient:_handle_session_fs_read_file(params)
    local path = params and params.path
    local f, oerr = io.open(path, "r")
    if not f then
        return { content = "", error = _sfs_error(oerr) }, nil
    end
    local content = f:read("*a") or ""
    f:close()
    return { content = content }, nil
end

function CopilotClient:_handle_session_fs_write_file(params)
    local path = params and params.path
    _sfs_mkparent(path)
    local f, oerr = io.open(path, "w")
    if not f then
        return _sfs_error(oerr), nil
    end
    f:write(tostring((params and params.content) or ""))
    f:close()
    if params and params.mode then
        os.execute("chmod " .. _sfs_mode_str(params.mode) .. " " .. _sfs_quote(path))
    end
    return nil, nil
end

function CopilotClient:_handle_session_fs_append_file(params)
    local path = params and params.path
    _sfs_mkparent(path)
    local f, oerr = io.open(path, "a")
    if not f then
        return _sfs_error(oerr), nil
    end
    f:write(tostring((params and params.content) or ""))
    f:close()
    if params and params.mode then
        os.execute("chmod " .. _sfs_mode_str(params.mode) .. " " .. _sfs_quote(path))
    end
    return nil, nil
end

function CopilotClient:_handle_session_fs_exists(params)
    return { exists = _sfs_path_exists(params and params.path) }, nil
end

function CopilotClient:_handle_session_fs_stat(params)
    local path = params and params.path
    local kind = _sfs_kind(path)
    if not kind then
        local now = _sfs_iso()
        return {
            isFile = false,
            isDirectory = false,
            size = 0,
            mtime = now,
            birthtime = now,
            error = _sfs_error("no such file or directory"),
        }, nil
    end
    local size = 0
    if kind == "file" then
        local f = io.open(path, "r")
        if f then
            size = f:seek("end") or 0
            f:close()
        end
    end
    local iso = _sfs_iso(_sfs_mtime(path) or os.time())
    return {
        isFile = (kind == "file"),
        isDirectory = (kind == "directory"),
        size = size,
        mtime = iso,
        birthtime = iso,
    }, nil
end

function CopilotClient:_handle_session_fs_mkdir(params)
    local path = params and params.path
    local cmd = (params and params.recursive) and ("mkdir -p " .. _sfs_quote(path))
        or ("mkdir " .. _sfs_quote(path))
    local ok = os.execute(cmd)
    if ok ~= true and ok ~= 0 and not _sfs_path_exists(path) then
        return _sfs_error("failed to create directory: " .. tostring(path)), nil
    end
    if params and params.mode then
        os.execute("chmod " .. _sfs_mode_str(params.mode) .. " " .. _sfs_quote(path))
    end
    return nil, nil
end

function CopilotClient:_handle_session_fs_readdir(params)
    local names, err = _sfs_list(params and params.path)
    if not names then
        return { entries = _sfs_as_array({}), error = _sfs_error(err) }, nil
    end
    return { entries = _sfs_as_array(names) }, nil
end

function CopilotClient:_handle_session_fs_readdir_with_types(params)
    local path = params and params.path
    local names, err = _sfs_list(path)
    if not names then
        return { entries = _sfs_as_array({}), error = _sfs_error(err) }, nil
    end
    local base = tostring(path):gsub("[/\\]$", "")
    local entries = {}
    for _, name in ipairs(names) do
        local kind = _sfs_kind(base .. "/" .. name)
        entries[#entries + 1] = { name = name, type = (kind == "directory") and "directory" or "file" }
    end
    return { entries = _sfs_as_array(entries) }, nil
end

function CopilotClient:_handle_session_fs_rm(params)
    local path = params and params.path
    local force = params and params.force
    if params and params.recursive then
        os.execute((force and "rm -rf " or "rm -r ") .. _sfs_quote(path))
        if not force and _sfs_path_exists(path) then
            return _sfs_error("failed to remove: " .. tostring(path)), nil
        end
        return nil, nil
    end
    local ok, rerr = os.remove(path)
    if not ok then
        if force then
            return nil, nil
        end
        return _sfs_error(rerr), nil
    end
    return nil, nil
end

function CopilotClient:_handle_session_fs_rename(params)
    local ok, rerr = os.rename(params and params.src, params and params.dest)
    if not ok then
        return _sfs_error(rerr), nil
    end
    return nil, nil
end

-- ---------------------------------------------------------------------------
-- Internal: request building
-- ---------------------------------------------------------------------------

--- Return t marked so cjson encodes it as a JSON array even when empty.
local function as_array(t)
    if type(t) == "table" and next(t) == nil and cjson.array_mt then
        setmetatable(t, cjson.array_mt)
    end
    return t
end

--- True when any hook handler is set.
local function has_any_hook(h)
    return h.onPreToolUse or h.onPostToolUse or h.onUserPromptSubmitted
        or h.onUserPromptTransformed or h.onSessionStart or h.onSessionEnd
        or h.onErrorOccurred or h.onPreMcpToolCall or h.onSubagentStart
        or h.onSubagentStop
end

--- Build the flat session.create / session.resume options shared by both RPCs.
-- The result is translated to the grouped SDK protocol 4 shape by
-- session_params.group() right before it is sent.
-- @param config table  SessionConfig / ResumeSessionConfig
-- @param resume boolean
-- @return table flat request
function CopilotClient:_build_session_request(config, resume)
    local req = {}

    if config.model then req.model = config.model end
    if config.reasoningEffort then req.reasoningEffort = config.reasoningEffort end
    if config.configDir then req.configDir = config.configDir end
    if config.workingDirectory then req.workingDirectory = config.workingDirectory end
    if config.systemMessage then
        -- A plain string is appended to the default system message.
        if type(config.systemMessage) == "string" then
            req.systemMessage = { mode = "append", content = config.systemMessage }
        else
            req.systemMessage = config.systemMessage
        end
    end
    if config.provider then req.provider = config.provider end
    if config.instructions then req.instructions = config.instructions end
    if config.agent then req.agent = config.agent end

    -- Tools: serialize without the handler function
    if config.tools and #config.tools > 0 then
        local tool_defs = {}
        for _, tool in ipairs(config.tools) do
            table.insert(tool_defs, {
                name        = tool.name,
                description = tool.description,
                parameters  = tool.parameters,
                idempotent  = tool.idempotent,
            })
        end
        req.tools = tool_defs
    end

    if config.availableTools then req.availableTools = as_array(config.availableTools) end
    if config.excludedTools then req.excludedTools = as_array(config.excludedTools) end
    if config.mcpServers then req.mcpServers = config.mcpServers end
    if config.customAgents then req.customAgents = config.customAgents end
    if config.skillDirectories then req.skillDirectories = config.skillDirectories end
    if config.disabledSkills then req.disabledSkills = config.disabledSkills end
    if config.infiniteSessions then req.infiniteSessions = config.infiniteSessions end
    if config.includeSubAgentStreamingEvents ~= nil then
        req.includeSubAgentStreamingEvents = config.includeSubAgentStreamingEvents
    end
    if config.enableConfigDiscovery ~= nil then req.enableConfigDiscovery = config.enableConfigDiscovery end
    if config.gitHubToken then req.gitHubToken = config.gitHubToken end
    if config.instructionDirectories then req.instructionDirectories = config.instructionDirectories end

    -- Upstream-sync session options (parity with @github/copilot-sdk)
    if config.enableCitations ~= nil then req.enableCitations = config.enableCitations end
    if config.excludedBuiltinAgents then req.excludedBuiltinAgents = config.excludedBuiltinAgents end
    if config.sessionLimits then req.sessionLimits = config.sessionLimits end
    if config.memory then req.memory = config.memory end
    if config.otlpProtocol then req.otlpProtocol = config.otlpProtocol end
    if config.expAssignments then req.expAssignments = config.expAssignments end
    -- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk)
    if config.rewindEnabled ~= nil then req.rewindEnabled = config.rewindEnabled end
    if config.additionalDirectories then req.additionalDirectories = config.additionalDirectories end
    if config.disabledMcpServers then req.disabledMcpServers = config.disabledMcpServers end
    if config.githubMcpToolConfig then req.githubMcpToolConfig = config.githubMcpToolConfig end
    if config.canvasProvider then req.canvasProvider = config.canvasProvider end
    if config.customAgentsLocalOnly ~= nil then req.customAgentsLocalOnly = config.customAgentsLocalOnly end
    if config.builtinPluginDirectories then req.builtinPluginDirectories = config.builtinPluginDirectories end
    if config.argsSchema then req.argsSchema = config.argsSchema end
    if config.decisionContext then req.decisionContext = config.decisionContext end
    if config.toolSearch ~= nil then req.toolSearch = config.toolSearch end
    if config.inProcess ~= nil then req.inProcess = config.inProcess end
    if config.experimentalMode ~= nil then req.experimentalMode = config.experimentalMode end
    if config.contentExclusion ~= nil then req.contentExclusion = config.contentExclusion end
    if config.onMcpAuthRequest then req.mcpAuthHandler = true end

    -- v2.6.0 upstream-sync session options. An explicit capi ({ enabled = ... })
    -- is merged with the Auto tier / WebSocket options.
    local capi, has_capi = {}, false
    if type(config.capi) == "table" then
        for k, v in pairs(config.capi) do
            capi[k] = v
            has_capi = true
        end
    end
    if config.autoTier then
        capi.autoTier = config.autoTier
        has_capi = true
    end
    if config.enableWebSocketResponses ~= nil then
        capi.enableWebSocketResponses = config.enableWebSocketResponses
        has_capi = true
    end
    if has_capi then req.capi = capi end
    if config.askUserVariant then req.askUserVariant = config.askUserVariant end
    if config.sandboxConfig then req.sandboxConfig = config.sandboxConfig end
    if config.featureFlags then req.featureFlags = config.featureFlags end
    if config.allowedModels then req.allowedModels = as_array(config.allowedModels) end
    if config.authClientIdMetadataUrl then req.authClientIdMetadataUrl = config.authClientIdMetadataUrl end
    if config.agentDirectories then req.agentDirectories = config.agentDirectories end
    if config.hookDirectories then req.hookDirectories = config.hookDirectories end
    if config.interactiveUser ~= nil then req.interactiveUser = config.interactiveUser end
    if config.excludedManagedSettings then req.excludedManagedSettings = config.excludedManagedSettings end
    if config.enforceManagedModelDefaults ~= nil then
        req.enforceManagedModelDefaults = config.enforceManagedModelDefaults
    end
    if config.imageGeneration then req.imageGeneration = config.imageGeneration end
    if config.autopilot then req.autopilot = config.autopilot end

    if config.streaming ~= nil then req.streaming = config.streaming end
    if config.onPermissionRequest then req.requestPermission = true end
    if config.onUserInputRequest then req.requestUserInput = true end
    if config.onExitPlanMode then req.requestExitPlanMode = true end
    if config.modelCapabilities then req.modelCapabilities = config.modelCapabilities end

    if config.hooks and has_any_hook(config.hooks) then
        req.hooks = true
    end

    -- v3.0.0 (SDK protocol 4) session options
    if config.providers then req.providers = config.providers end
    if config.models then req.models = config.models end
    if config.defaultProvider then
        req.defaultProvider = config.defaultProvider
    elseif resume and (config.clearDefaultProvider or config.defaultProvider == cjson.null) then
        -- An explicit null clears the session's default provider on resume.
        req.defaultProvider = cjson.null
    end
    if config.onShellAuthenticationRequest then req.requestShellAuthentication = true end
    if config.storeMemory then req.hasMemoryStore = true end
    if config.permissionMode then req.permissionMode = config.permissionMode end

    if resume then
        if config.disableResume then req.disableResume = true end
        if config.allowTranscriptRecovery ~= nil then
            req.allowTranscriptRecovery = config.allowTranscriptRecovery
        end
    else
        if config.sessionId then req.sessionId = config.sessionId end
        if config.name then req.name = config.name end
        if config.trustWorkingDirectory then req.trustWorkingDirectory = config.trustWorkingDirectory end
        if config.refreshCustomInstructions ~= nil then
            req.refreshCustomInstructions = config.refreshCustomInstructions
        end
    end

    return req
end

--- Build the request body for session.create.
function CopilotClient:_build_create_session_request(config)
    return self:_build_session_request(config, false)
end

--- Build the request body for session.resume.
function CopilotClient:_build_resume_session_request(session_id, config)
    local req = self:_build_session_request(config, true)
    req.sessionId = session_id
    return req
end

-- ---------------------------------------------------------------------------
-- Internal: protocol version verification
-- ---------------------------------------------------------------------------

--- Verify that the server protocol version matches the SDK expectation.
-- @return boolean success
-- @return string|nil error
function CopilotClient:_verify_protocol_version()
    local expected = sdk_protocol_version.get()
    local result, err = self:ping("")
    if err then
        return false, "ping failed during protocol verification: " .. tostring(err)
    end

    if not result then
        return false, "ping returned nil result"
    end

    local server_version = result.protocolVersion
    if server_version == nil or server_version == cjson.null then
        return false, string.format(
            "SDK protocol version mismatch: SDK supports version %d, but server does not report a protocol version. Please update your server to ensure compatibility.",
            expected
        )
    end

    if server_version ~= expected then
        return false, string.format(
            "SDK protocol version mismatch: SDK supports version %d, but server reports version %s. Please update your SDK or server to ensure compatibility.",
            expected, tostring(server_version)
        )
    end

    return true, nil
end

return CopilotClient

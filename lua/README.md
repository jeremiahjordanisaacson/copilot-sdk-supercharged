# Copilot Supercharged SDK for Lua

A Lua SDK for interacting with the GitHub Copilot CLI server. Communicates via JSON-RPC 2.0 over stdio using Content-Length header framing.

## Requirements

- Lua 5.1+ (tested with 5.1, 5.2, 5.3, 5.4, and LuaJIT)
- [lua-cjson](https://www.kyne.com.au/~mark/software/lua-cjson.php) for JSON encoding/decoding
- The `copilot` CLI installed and available on `PATH` (or specify the path via `cliPath`)

## Installation

### Via LuaRocks

```bash
luarocks install copilot-sdk-supercharged
```

LuaRocks currently serves version 2.5.3 (SDK protocol 3). For version 3.1.0, which this README documents, install the rock from a clone of this repository instead:

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
cd copilot-sdk-supercharged/lua
luarocks make copilot-sdk-supercharged-3.1.0-1.rockspec
```

### Manual

Copy the `copilot/` directory into your Lua module path.

## Quick Start

```lua
local CopilotClient = require("copilot.client")
local define_tool   = require("copilot.define_tool")

-- Create and start the client
local client = CopilotClient.new({ logLevel = "info" })
local ok, err = client:start()
if not ok then
    error("Failed to start: " .. tostring(err))
end

-- Create a session
local session, err = client:create_session()
if not session then
    error("Failed to create session: " .. tostring(err))
end

-- Listen to events
local unsubscribe = session:on(function(event)
    print(event.type, event.data and event.data.content)
end)

-- Send a message and wait for the response
local response, err = session:send_and_wait({ prompt = "What is 2+2?" })
if response then
    print("Answer:", response.data.content)
end

-- Clean up
unsubscribe()
session:destroy()
client:stop()
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, returned like any other RPC error (`nil` or `false`, the error message, and the structured `{ code, message, data }` error).

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret value is never logged.

```lua
local result, err = client:mcp_secrets_save("my-server-token", os.getenv("MY_SERVER_TOKEN"))  -- optional config_dir
if result then
    print(result.usedKeychain and "Stored in the OS keychain" or "Stored in the file backend")
end

client:mcp_secrets_delete("my-server-token")
client:mcp_secrets_delete_server("my-server")  -- every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions. `extension_approvals_list` and `extension_approvals_revoke` take an optional trailing `session_id` to use an attached session's configuration profile.

```lua
-- Persist a pending extension permission request before answering it normally
local grant = client:extension_approvals_grant(session.session_id, request_id)
if grant and grant.granted then
    print("Approved " .. grant.approval.extensionName)
end

local list = client:extension_approvals_list()
for _, approval in ipairs(list and list.approvals or {}) do
    -- extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
    client:extension_approvals_revoke(approval.extensionName, approval.installationKey)
end
```

### Ask-user image attachments

When the runtime accepts images in an answer, the user input request carries `imageAttachmentConstraints` (`allowedMimeTypes`, `maxBytesPerImage`, `maxCount`); it is `nil` otherwise. The request also carries `requestId` and the opaque `_meta` table when sent. Answer with `attachments`, an array built with `types.UserInputImageAttachment` (base64 `data`, `mimeType`, optional `description` and `displayName`; `type` is always `"image"`). Image-only answers use `answer = ""` and `wasFreeform = true`; the runtime enforces the limits.

```lua
local session = client:create_session({
    onUserInputRequest = function(request, invocation)
        local constraints = request.imageAttachmentConstraints
        if constraints then
            for _, mime_type in ipairs(constraints.allowedMimeTypes) do
                if mime_type == "image/png" then
                    return types.UserInputResponse({
                        answer      = "",
                        wasFreeform = true,
                        attachments = {
                            types.UserInputImageAttachment({
                                data        = base64_encode(read_file("screenshot.png")),
                                mimeType    = "image/png",
                                displayName = "screenshot.png",
                            }),
                        },
                    })
                end
            end
        end
        return types.UserInputResponse({ answer = "No screenshot available", wasFreeform = true })
    end,
})
```

## What's New in v3.0.0

This release speaks **SDK protocol v4**, the protocol of the Microsoft Copilot SDK 2.0 runtime. It requires **Copilot CLI 1.0.113 or newer**; older CLIs are rejected by `client:start()` with an `SDK protocol version mismatch` error.

Existing options keep working: you keep passing the same flat session options, and the SDK translates them into the grouped v4 wire payload right before `session.create` and `session.resume` (for example `streaming` becomes `events.streaming` and `onPermissionRequest` becomes `permissions.request`). A legacy single `provider` (BYOK) is translated into a named `default` provider with Copilot API models disabled, matching v1 behavior. When `cliPath` (or `COPILOT_CLI_PATH`) points to a `.js` entry point such as `@github/copilot/npm-loader.js`, the SDK runs it with `node`.

### Named BYOK providers

```lua
local types = require("copilot.types")

local session, err = client:create_session({
    model     = "gateway/chat",
    providers = {
        types.NamedProviderConfig({
            name          = "gateway",
            type          = "openai",
            baseUrl       = "https://models.example.com/v1",
            apiKeyCommand = "print-model-key",
        }),
    },
    models = {
        types.ProviderModelConfig({
            provider = "gateway", id = "chat", modelId = "gpt-4o", maxOutputTokens = 4096,
        }),
    },
    capi = types.CapiConfig({ enabled = false }),  -- BYOK-only catalog
})

-- Clear the default provider when resuming (sends defaultProvider = null)
local resumed = client:resume_session(session.session_id, {
    model                = "gpt-5.4",
    capi                 = types.CapiConfig({ enabled = true }),
    clearDefaultProvider = true,
})
```

### Idempotent tools

```lua
local lookup_order = define_tool("lookup_order", {
    description = "Look up an order",
    idempotent  = true,
    parameters  = { type = "object", properties = { id = { type = "string" } }, required = { "id" } },
    handler     = function(args) return "Order " .. args.id .. " shipped" end,
})
```

### Client turn IDs

```lua
session:send_and_wait({ prompt = "Summarize the diff", clientTurnId = "turn-42" })
```

### Client mode

`mode = "coding-agent"` (the default) keeps today's behavior. `mode = "empty"` requires every session to set `availableTools` (otherwise `create_session` / `resume_session` return an error) and applies conservative defaults: no telemetry, in-memory OAuth and embedding storage, no cross-session store or memory, no on-demand instruction discovery, repository hooks or host git operations. Values you pass win. `"copilot-cli"` is accepted as an alias of `"coding-agent"`.

```lua
local client = CopilotClient.new({ mode = types.ClientMode.EMPTY })
local session, err = client:create_session({ model = "gpt-5.4", availableTools = { "view" } })
```

### Shell output events

```lua
session:on(function(event)
    if event.type == types.SessionEventType.TOOL_SHELL_OUTPUT then
        local chunk = types.ShellOutputData(event.data)
        io.write(chunk.text)  -- chunk.stream is "stdout", "stderr" or nil (stdout)
    end
end)
```

### Shell authentication

Answer credential prompts from shell programs (for example `git` asking for a password). Errors and malformed answers cancel the prompt; the answer is never logged.

```lua
local session = client:create_session({
    onShellAuthenticationRequest = function(request, ctx)
        -- request.program, request.prompt, request.inputKind ("secret", "text" or "confirm"),
        -- request.command, request.cwd
        return types.ShellAuthenticationResponse.accept(fetch_secret(request.program))
    end,
})
```

### Memory store

```lua
local session = client:create_session({
    storeMemory = function(memory, ctx)
        -- memory.subject, memory.fact, memory.citations, memory.reason
        save_memory(memory.subject, memory.fact)
    end,
})
```

### Network callbacks

Resolve proxies and answer proxy authentication challenges from your application. Tokens and passwords are never logged.

```lua
local client = CopilotClient.new({
    network = types.NetworkConfig({
        scope        = types.NetworkScope.RUNTIME,
        resolveProxy = function(request)
            if request.url:find("^https://internal") then return "DIRECT" end
            return "PROXY proxy.example:8080"
        end,
        authenticateBasic = function(request)
            if request.previousRejected then return nil end
            return { username = "svc", password = os.getenv("PROXY_PASSWORD") }
        end,
    }),
})
```

### Policy helper failures

`create_session` and `resume_session` return a third value with the structured JSON-RPC error when the runtime rejects the request.

```lua
local session, err, rpc_error = client:create_session({ model = "gpt-5.4" })
if not session then
    local failure = types.policy_helper_failure(rpc_error)
    if failure then
        print(string.format("Policy helper failed (%s, %s): %s",
            failure.category, failure.failureMode, failure.message))
    end
    error(err)
end
```

### Permission mode refusal reason

```lua
local result, err = session:set_permission_mode(types.PermissionMode.ALLOW_ALL)
if result and result.reason == types.PermissionModeRefusalReason.MANAGED_POLICY then
    print("Blocked by managed policy")
end
```

### v2.6.0 parity, now sent to the runtime

These session options and methods are now implemented end to end in the Lua SDK:

```lua
local client = CopilotClient.new({
    clientInfo = types.ClientInfo({ applicationName = "my-app", applicationVersion = "1.2.0" }),
})

local session = client:create_session({
    model                   = "auto",
    autoTier                = types.AutoTier.BALANCE,
    askUserVariant          = types.AskUserVariant.ELICITATION,
    gitHubTokenProvider     = function(args)  -- args.host, args.sessionId, args.reason
        return types.GitHubTokenResult.token(fetch_token(args.host), 3600)
    end,
    sandboxConfig           = types.SandboxConfig({ enabled = true, allowBypass = true }),
    featureFlags            = { some_flag = true },
    allowedModels           = { "auto", "gpt-5.4" },
    authClientIdMetadataUrl = "https://example.com/oauth/client.json",
    modelCapabilities       = types.ModelCapabilitiesOverride({
        limits = types.ModelLimitsOverride({ maxOutputTokens = 8192 }),
    }),
    agentDirectories        = { "./agents" },
    hookDirectories         = { "./hooks" },
    workingDirectory        = "/path/to/project",
    trustWorkingDirectory   = types.WorkingDirectoryTrust.SESSION,  -- "session" or "user"; needs workingDirectory
    imageGeneration         = types.ImageGenerationConfig({ enabled = true, saveToDisk = true }),
    autopilot               = types.AutopilotOptions({ continuationEnabled = true, maxContinues = 5 }),
    hooks                   = types.SessionHooks({
        onSubagentStart = function(input, invocation) print("subagent started") end,
        onSubagentStop  = function(input, invocation) print("subagent stopped") end,
    }),
})

session:set_auto_tier(types.AutoTier.FAST)    -- nil returns to the default Auto routing
session:send_and_wait({
    prompt         = "List three colors as JSON",
    source         = types.MessageSource.USER,
    responseSchema = { type = "object", properties = { colors = { type = "array" } } },
})
session:recheck_policy_helper()
client:clear_managed_settings_cache()

-- Resume with transcript recovery and without a session.resume event
local resumed = client:resume_session(session.session_id, {
    allowTranscriptRecovery = true,
    disableResume           = true,
})

-- Detach (keeps on-disk state so the session can be resumed); destroy() does the same
resumed:detach()
```

### Known limitations

* The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not available: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process runtime hosting is not the default connection: the Lua SDK keeps spawning the CLI.
* The client is single-threaded, so server callbacks (tools, permissions, shell authentication, memory store, GitHub token and network callbacks) are answered only while the client processes messages: while it waits for a request's response or inside `send_and_wait`. A `shellAuthentication.cancel` that arrives while your handler runs is applied when the handler returns, and the prompt then answers `cancel`.
* A plain string `instructions` option (not part of the documented session configuration) has no SDK protocol 4 equivalent and is no longer sent; use `systemMessage` instead.

## Architecture

The SDK follows the same architecture as all other SDKs in this repository (40 languages supported):

```
CopilotClient (client.lua)
    |
    |-- spawns CLI process with --headless --no-auto-update --stdio
    |-- manages JsonRpcClient for communication
    |-- dispatches incoming server requests to sessions
    |
    +-- CopilotSession (session.lua)
            |-- event subscriptions (on / unsubscribe)
            |-- send / send_and_wait
            |-- tool handlers
            |-- permission handler
            |-- user input handler
            |-- hooks (pre/post tool use, session start/end, etc.)
```

Communication uses **JSON-RPC 2.0** with **Content-Length header framing**:

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","id":"...","method":"...","params":{...}}
```

## Modules

| Module | Description |
|--------|-------------|
| `copilot.client` | `CopilotClient` -- manages CLI process and sessions |
| `copilot.session` | `CopilotSession` -- conversation session with event handling |
| `copilot.json_rpc_client` | `JsonRpcClient` -- JSON-RPC 2.0 over stdio transport |
| `copilot.types` | Type constructors and constants |
| `copilot.define_tool` | Helper for defining tools with result normalization |
| `copilot.sdk_protocol_version` | Protocol version constant (must match server) |
| `copilot.session_params` | Translates session options into the grouped SDK protocol 4 payload (internal) |

## Custom Tools

Define tools that the Copilot agent can invoke:

```lua
local define_tool = require("copilot.define_tool")

local weather_tool = define_tool("get_weather", {
    description = "Get current weather for a city",
    parameters = {
        type = "object",
        properties = {
            city = { type = "string", description = "City name" },
            unit = {
                type = "string",
                enum = { "celsius", "fahrenheit" },
                description = "Temperature unit",
            },
        },
        required = { "city" },
    },
    handler = function(args, invocation)
        -- args.city, args.unit are parsed from the JSON arguments
        -- invocation has sessionId, toolCallId, toolName, arguments
        return string.format("Weather in %s: 22 degrees %s",
            args.city, args.unit or "celsius")
    end,
})

local session = client:create_session({
    tools = { weather_tool },
})
```

The handler can return:

- A **string** -- becomes `textResultForLlm` with `resultType = "success"`
- A **ToolResult table** -- passed through directly
- A **table** -- JSON-serialized to `textResultForLlm`
- **nil** -- empty success result

If the handler raises an error, a failure `ToolResult` is returned automatically.

## Permission Handling

Register a handler to approve or deny tool permission requests:

```lua
local types = require("copilot.types")

local session = client:create_session({
    onPermissionRequest = function(request, invocation)
        print("Permission requested:", request.kind)
        -- Return approval
        return types.PermissionRequestResult({ kind = "approved" })
    end,
})
```

## User Input Handling

Register a handler to respond to user input requests (enables the `ask_user` tool):

```lua
local types = require("copilot.types")

local session = client:create_session({
    onUserInputRequest = function(request, invocation)
        print("Question:", request.question)
        -- Return a response
        return types.UserInputResponse({
            answer      = "Yes, proceed.",
            wasFreeform = true,
        })
    end,
})
```

## Hooks

Register lifecycle hooks for observability and customization:

```lua
local session = client:create_session({
    hooks = {
        onPreToolUse = function(input, invocation)
            print("About to use tool:", input.toolName)
            return { permissionDecision = "allow" }
        end,
        onPostToolUse = function(input, invocation)
            print("Tool completed:", input.toolName)
            return nil
        end,
        onSessionStart = function(input, invocation)
            print("Session started, source:", input.source)
            return nil
        end,
        onSessionEnd = function(input, invocation)
            print("Session ended, reason:", input.reason)
            return nil
        end,
    },
})
```

Available hooks:
- `onPreToolUse` -- before a tool is executed
- `onPostToolUse` -- after a tool completes
- `onUserPromptSubmitted` -- when the user submits a prompt
- `onSessionStart` -- when a session starts
- `onSessionEnd` -- when a session ends
- `onErrorOccurred` -- when an error occurs

## Client Options

```lua
local client = CopilotClient.new({
    cliPath         = "/usr/local/bin/copilot",  -- default: "copilot"
    cwd             = "/path/to/project",         -- working directory
    logLevel        = "debug",                    -- "info", "debug", "warn", "error"
    githubToken     = "ghp_...",                  -- GitHub auth token
    useLoggedInUser = true,                       -- use stored OAuth tokens
    autoStart       = true,                       -- auto-start on first use
    autoRestart     = true,                       -- restart if CLI crashes
    mode            = "coding-agent",             -- or "empty" (see What's New in v3.0.0)
    network         = nil,                        -- optional types.NetworkConfig callbacks
})
```

## Session Configuration

```lua
local session = client:create_session({
    model               = "gpt-4",
    reasoningEffort     = "medium",            -- "low", "medium", "high", "xhigh"
    tools               = { my_tool },
    systemMessage       = { mode = "append", content = "Extra instructions" },
    availableTools      = { "Read", "Write" }, -- allowlist
    excludedTools       = { "Bash" },          -- blocklist (ignored if availableTools set)
    workingDirectory    = "/path/to/project",
    streaming           = true,                -- enable streaming deltas
    onPermissionRequest = my_permission_handler,
    onUserInputRequest  = my_input_handler,
    hooks               = my_hooks,
    provider            = {                    -- BYOK
        type    = "openai",
        baseUrl = "https://api.example.com/v1",
        apiKey  = "sk-...",
    },
    mcpServers = {
        myServer = {
            type    = "local",
            command = "node",
            args    = { "server.js" },
            tools   = { "tool1", "tool2" },
        },
    },
    infiniteSessions = {
        enabled = true,
        backgroundCompactionThreshold = 0.80,
        bufferExhaustionThreshold     = 0.95,
    },
})
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```lua
local client = CopilotClient:new({ session_idle_timeout_seconds = 300 })
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```lua
local client = CopilotClient:new({
    session_fs = {
        initial_cwd = "/repo",
        session_state_path = "/tmp/state",
        conventions = "posix",
    },
})
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```lua
local meta = client:get_session_metadata("session-123")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```lua
local session = client:create_session({
    skill_directories = { "./skills" },
    disabled_skills = { "test-skill" },
    include_sub_agent_streaming_events = true,
})
```

- `skill_directories` - directories to scan for skill definitions
- `disabled_skills` - skills to exclude from the session
- `include_sub_agent_streaming_events` - receive streaming events from sub-agents

## Recent Features (v2.4–v2.5)

Recent upstream syncs added a range of session options, passed in the table given to `client:create_session{...}`. Constant and config helpers live in `copilot.types`.

### New in v2.5

- **Reasoning effort** — `reasoningEffort` — set the model's reasoning budget (e.g. `"high"`).
- **Tool search** — `toolSearch` — discover tools on demand.
- **Content exclusion** — `contentExclusion` — honor content-exclusion rules.
- **Session rewind** — `rewindEnabled` — allow rolling back to an earlier snapshot; observe `types.SessionEventType.SESSION_SNAPSHOT_REWIND` events.
- **Additional directories** — `additionalDirectories` — extra workspace directories.
- **Disabled MCP servers** — `disabledMcpServers`.
- **GitHub MCP tool config** — `githubMcpToolConfig`.
- **Canvas provider** — `canvasProvider`.
- **Custom agents local-only** — `customAgentsLocalOnly`.
- **Built-in plugin directories** — `builtinPluginDirectories`.
- **Agent-factory authoring** — `argsSchema`.
- **Permission decision context** — `decisionContext`.
- **In-process transport** — `inProcess`.
- **Experimental mode** — `experimentalMode`.
- **MCP OAuth handler** — `onMcpAuthRequest`.

### New in v2.4

- **Session citations** — `enableCitations`.
- **Excluded built-in agents** — `excludedBuiltinAgents`.
- **Spending limits** — `sessionLimits` via `types.SessionLimitsConfig({ maxAiCredits = ... })`.
- **Session memory** — `memory` via `types.MemoryConfiguration({ enabled = ... })`.
- **OTLP protocol** — `otlpProtocol`.
- **WebSocket responses** — `enableWebSocketResponses`.
- **Experiment assignments** — `expAssignments`.
- **Hooks** — `onPostToolUse`, `onUserPromptTransformed`, `onPreMcpToolCall`.
- **BYOK fields** — `requestHandler` / `bearerTokenProvider` on `types.ClientOptions`.
- **Tool defer loading** — `types.ToolDefer`.
- **System-message sections** — `types.SystemMessageSection` (`PREAMBLE` / `PRESERVE`).
- **GitHub attachments** — `types.GitHubAttachment` (`GITHUB_COMMIT`, `GITHUB_REPOSITORY`).
- **Per-message overrides** — `agentMode`, `displayPrompt`.

### Examples

```lua
-- Reasoning effort + on-demand tool search + content exclusion
local session = client:create_session({
    reasoningEffort  = "high",
    toolSearch       = true,
    contentExclusion = true,
})
```

```lua
local types = require("copilot.types")

-- Enable session rewind and observe rewind snapshots
local session = client:create_session({ rewindEnabled = true })
session:on(function(event)
    if event.type == types.SessionEventType.SESSION_SNAPSHOT_REWIND then
        print("Rewound to snapshot")
    end
end)
```

```lua
-- Grant the agent access to extra workspace directories
local session = client:create_session({
    additionalDirectories = { "../shared-lib", "../docs" },
})
```

```lua
local types = require("copilot.types")

-- Cap AI-credit spend and enable persistent memory
local session = client:create_session({
    sessionLimits = types.SessionLimitsConfig({ maxAiCredits = 500 }),
    memory        = types.MemoryConfiguration({ enabled = true }),
})
```

## Session Event Types

All event type constants are available in `types.SessionEventType`:

```lua
local types = require("copilot.types")

session:on(function(event)
    if event.type == types.SessionEventType.ASSISTANT_MESSAGE then
        print(event.data.content)
    elseif event.type == types.SessionEventType.SESSION_IDLE then
        print("Session is idle")
    elseif event.type == types.SessionEventType.TOOL_EXECUTION_COMPLETE then
        print("Tool finished:", event.data.toolName)
    end
end)
```

## Protocol Version

The SDK protocol version must match the server. The current version is **4** (Copilot CLI 1.0.113 or newer).

```lua
local version = require("copilot.sdk_protocol_version")
print(version.get())  -- 4
```

The client automatically verifies protocol compatibility on startup via the `ping` response.

## API Reference

### CopilotClient

| Method | Description |
|--------|-------------|
| `CopilotClient.new(options)` | Create a new client |
| `client:start()` | Start CLI server and connect |
| `client:stop()` | Stop server and clean up |
| `client:state()` | Get connection state |
| `client:ping(message)` | Ping server |
| `client:get_status()` | Get server status |
| `client:get_auth_status()` | Get auth status |
| `client:list_models()` | List available models (cached) |
| `client:create_session(config)` | Create a new session (returns `session, err, rpc_error`) |
| `client:resume_session(id, config)` | Resume an existing session (returns `session, err, rpc_error`) |
| `client:list_sessions()` | List all sessions |
| `client:delete_session(id)` | Delete a session |
| `client:get_foreground_session_id()` | Get TUI foreground session |
| `client:set_foreground_session_id(id)` | Set TUI foreground session |
| `client:clear_managed_settings_cache()` | Clear the runtime's cached managed settings |
| `client:mcp_secrets_save(secret_id, value, config_dir)` | Save an MCP credential (result has `usedKeychain`) |
| `client:mcp_secrets_delete(secret_id, config_dir)` | Delete an MCP credential |
| `client:mcp_secrets_delete_server(server_name, config_dir)` | Delete the MCP credentials indexed for a server |
| `client:extension_approvals_grant(session_id, request_id)` | Persist a pending plugin extension approval (result has `granted`, `approval`) |
| `client:extension_approvals_list(session_id)` | List plugin extension approvals (result has `approvals`) |
| `client:extension_approvals_revoke(extension_name, installation_key, session_id)` | Revoke a plugin extension approval (result has `revoked`) |

### CopilotSession

| Method | Description |
|--------|-------------|
| `session:send(options)` | Send a message |
| `session:send_and_wait(options, timeout)` | Send and wait for idle |
| `session:on(handler)` | Subscribe to events (returns unsubscribe fn) |
| `session:get_messages()` | Get conversation history |
| `session:destroy()` | Detach from the session (`session.detach`, falling back to `session.destroy` on older CLIs) |
| `session:detach()` | Same as `destroy()` |
| `session:abort()` | Abort current message processing |
| `session:set_auto_tier(tier)` | Change the Auto routing tier (`nil` restores the default) |
| `session:get_managed_settings()` | Get the managed settings in effect |
| `session:recheck_policy_helper()` | Re-run the managed-settings policy helper |
| `session:set_permission_mode(mode)` | Change the permission mode (result has `success`, `mode`, `reason`) |

### define_tool

```lua
local tool = define_tool(name, { description, parameters, handler, idempotent })
```

## Image Generation

Request image responses using `responseFormat` and `imageOptions`:

```lua
local response = session:send_and_wait({
    prompt         = "Generate a sunset over mountains",
    responseFormat = types.ResponseFormat.IMAGE,
    imageOptions   = types.image_options({ size = "1024x1024", quality = "hd", style = "natural" }),
})
```

## License

MIT

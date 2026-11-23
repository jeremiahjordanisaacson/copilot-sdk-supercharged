--- CopilotSession: represents a single conversation session with the Copilot CLI.
--
-- Sessions maintain conversation state, handle events, and manage tool execution.
-- Sessions are created via CopilotClient:create_session() or resumed via
-- CopilotClient:resume_session().
--
-- Usage:
--   local session = client:create_session({ model = "gpt-4" })
--   session:on(function(event) print(event.type) end)
--   session:send({ prompt = "Hello!" })
--   session:destroy()

local cjson = require("cjson")
local types = require("copilot.types")

-- ---------------------------------------------------------------------------
-- CopilotSession class
-- ---------------------------------------------------------------------------

local CopilotSession = {}
CopilotSession.__index = CopilotSession

--- Create a new CopilotSession (internal, called by CopilotClient).
-- @param session_id string      The unique session ID
-- @param rpc_client JsonRpcClient  The JSON-RPC client
-- @param workspace_path string|nil  The workspace path for infinite sessions
-- @return CopilotSession
function CopilotSession.new(session_id, rpc_client, workspace_path)
    local self = setmetatable({}, CopilotSession)

    self.session_id        = session_id
    self.workspace_path    = workspace_path or ""
    self._rpc_client       = rpc_client
    self._handlers         = {}           -- array of {id=number, fn=function}
    self._next_handler_id  = 1
    self._tool_handlers    = {}           -- toolName -> handler function
    self._permission_handler = nil        -- function or nil
    self._user_input_handler = nil        -- function or nil
    self._hooks            = nil          -- SessionHooks table or nil
    self._exit_plan_mode_handler = nil    -- function or nil
    self._trace_context_provider = nil    -- function or nil
    self._shell_authentication_handler = nil  -- function or nil
    self._pending_shell_authentications = {}  -- requestId -> true once cancelled
    self._memory_store_handler = nil      -- function or nil
    self._destroy_callbacks = {}          -- functions run once after destroy()

    return self
end

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

--- Send a message to this session.
-- @param options table  MessageOptions with fields: prompt, attachments, mode, responseFormat,
--   imageOptions, source ("user", "system" or "agent-<id>"), responseSchema (JSON Schema
--   requesting structured output) and clientTurnId (opaque turn id echoed on events)
-- @return string|nil messageId
-- @return string|nil error
function CopilotSession:send(options)
    options = options or {}
    local req = {
        sessionId      = self.session_id,
        prompt         = options.prompt or "",
        attachments    = options.attachments,
        mode           = options.mode,
        agentMode      = options.agentMode,
        displayPrompt  = options.displayPrompt,
        requestHeaders = options.requestHeaders,
        responseFormat = options.responseFormat,
        imageOptions   = options.imageOptions,
        source         = options.source,
        clientTurnId   = options.clientTurnId,
    }
    -- Structured output takes precedence over the plain response format.
    if options.responseSchema then
        req.responseFormat = {
            type       = "json_schema",
            jsonSchema = { name = "response", strict = true, schema = options.responseSchema },
        }
    end

    -- Inject trace context if provider is available
    if self._trace_context_provider then
        local tc_ok, tc = pcall(self._trace_context_provider)
        if tc_ok and tc then
            if tc.traceparent then req.traceparent = tc.traceparent end
            if tc.tracestate then req.tracestate = tc.tracestate end
        end
    end

    local result, err = self._rpc_client:request("session.send", req)
    if err then
        return nil, "failed to send message: " .. tostring(err)
    end

    if result and result.messageId then
        return result.messageId, nil
    end
    return nil, nil
end

--- Send a message and wait until the session becomes idle.
-- Returns the final assistant message event, or nil if none was received.
-- @param options table     MessageOptions
-- @param timeout number|nil  Timeout in seconds (default 60)
-- @return table|nil  The last assistant.message SessionEvent
-- @return string|nil error
function CopilotSession:send_and_wait(options, timeout)
    timeout = timeout or 60

    local last_assistant_message = nil
    local idle_received = false
    local error_received = nil

    -- Events read while send() waits for its RPC response can still belong to
    -- an earlier turn (for example a late session.idle). Buffer them until the
    -- message id is known, and accept session.idle only after this turn has
    -- visibly started, so a stale idle cannot end the wait early.
    local message_id = nil
    local sent = false
    local turn_started = false
    local buffered = {}

    -- Live events without correlation ids are attributed to this turn (older
    -- runtimes omit them); buffered ones must match the message id exactly.
    local function belongs_to_turn(id, live)
        if id ~= nil and message_id ~= nil then
            return id == message_id
        end
        return live
    end

    local function observe(event, live)
        local data = event.data or {}
        if event.type == types.SessionEventType.USER_MESSAGE then
            if belongs_to_turn(data.messageId, live) then
                turn_started = true
            end
        elseif event.type == types.SessionEventType.ASSISTANT_TURN_START then
            if live then
                turn_started = true
            end
        elseif event.type == types.SessionEventType.ASSISTANT_MESSAGE then
            if belongs_to_turn(data.originatingMessageId, live) then
                turn_started = true
                last_assistant_message = event
            end
        elseif event.type == types.SessionEventType.SESSION_IDLE then
            if turn_started then
                idle_received = true
            end
        elseif event.type == types.SessionEventType.SESSION_ERROR then
            if belongs_to_turn(data.originatingMessageId, live) then
                error_received = data.message or "session error"
            end
        end
    end

    -- Subscribe to events
    local unsubscribe = self:on(function(event)
        if sent then
            observe(event, true)
        else
            table.insert(buffered, event)
        end
    end)

    -- Send the message
    local sent_id, send_err = self:send(options)
    if send_err then
        unsubscribe()
        return nil, send_err
    end
    message_id = sent_id
    sent = true
    for _, event in ipairs(buffered) do
        observe(event, false)
    end
    buffered = nil
    -- Without a message id there is nothing to correlate against, so treat
    -- the send itself as the start of the turn.
    if message_id == nil then
        turn_started = true
    end

    -- Wait for idle or error, polling the read loop
    local start_time = os.time()
    while not idle_received and not error_received do
        -- Check timeout
        if os.time() - start_time >= timeout then
            unsubscribe()
            return nil, "timeout waiting for session.idle"
        end

        -- Drive the read loop for one message
        if self._rpc_client and self._rpc_client._read_one_message then
            local ok, read_err = self._rpc_client:_read_one_message()
            if not ok then
                unsubscribe()
                -- The connection closed (EOF) before the session went idle. This
                -- happens when the CLI/replay-proxy cannot service the request and
                -- the process ends. Surface it as a timeout-style condition so
                -- callers waiting for idle can degrade gracefully instead of
                -- treating an unexpected disconnect as a fatal error.
                return nil, "timeout waiting for session.idle (connection closed: " .. tostring(read_err) .. ")"
            end
        else
            -- Fallback: small sleep to avoid busy-waiting
            -- In a real async environment, this would yield to the event loop
            local sleep_until = os.clock() + 0.01
            while os.clock() < sleep_until do end
        end
    end

    unsubscribe()

    if error_received then
        return nil, "session error: " .. tostring(error_received)
    end

    return last_assistant_message, nil
end

--- Subscribe to events from this session.
-- Returns an unsubscribe function.
-- @param handler function(event)  Event handler callback
-- @return function  Call to unsubscribe
function CopilotSession:on(handler)
    local id = self._next_handler_id
    self._next_handler_id = self._next_handler_id + 1

    table.insert(self._handlers, { id = id, fn = handler })

    -- Return unsubscribe function
    return function()
        for i, h in ipairs(self._handlers) do
            if h.id == id then
                table.remove(self._handlers, i)
                break
            end
        end
    end
end

--- Retrieve all events and messages from this session's history.
-- @return table|nil  Array of SessionEvent
-- @return string|nil error
function CopilotSession:get_messages()
    local result, err = self._rpc_client:request("session.getMessages", {
        sessionId = self.session_id,
    })
    if err then
        return nil, "failed to get messages: " .. tostring(err)
    end

    if result and result.events then
        return result.events, nil
    end
    return {}, nil
end

--- Retrieve metadata for this session.
-- @return table|nil  The session metadata
-- @return string|nil error
function CopilotSession:get_metadata()
    local result, err = self._rpc_client:request("session.getMetadata", {
        sessionId = self.session_id,
    })
    if err then
        return nil, "failed to get metadata: " .. tostring(err)
    end

    return result, nil
end

--- Destroy this session and release resources.
-- Sends session.detach (falling back to session.destroy on CLIs that do not
-- implement it), so on-disk state is preserved and the session can be resumed.
-- @return boolean success
-- @return string|nil error
function CopilotSession:destroy()
    local _, err = self._rpc_client:request("session.detach", {
        sessionId = self.session_id,
    })
    if err and tostring(err):find("-32601", 1, true) then
        _, err = self._rpc_client:request("session.destroy", {
            sessionId = self.session_id,
        })
    end
    if err then
        return false, "failed to destroy session: " .. tostring(err)
    end

    -- Clear all handlers
    self._handlers           = {}
    self._tool_handlers      = {}
    self._permission_handler = nil
    self._user_input_handler = nil
    self._hooks              = nil
    self._exit_plan_mode_handler = nil
    self._trace_context_provider = nil
    self._shell_authentication_handler = nil
    self._memory_store_handler = nil
    for request_id in pairs(self._pending_shell_authentications) do
        self._pending_shell_authentications[request_id] = true
    end

    local callbacks = self._destroy_callbacks
    self._destroy_callbacks = {}
    for _, callback in ipairs(callbacks) do
        pcall(callback)
    end

    return true, nil
end

--- Detach from this session (same as destroy()): sends session.detach so the
-- session state is kept and the session can be resumed later.
-- @return boolean success
-- @return string|nil error
function CopilotSession:detach()
    return self:destroy()
end

--- Change the Auto routing preference without changing the selected model
-- (experimental). The runtime commits it on a later turn that uses the "auto" model.
-- @param auto_tier string|nil  "efficiency", "balance", "intelligence" or "fast"
--   (see types.AutoTier); nil returns to the provider's default Auto routing
-- @return table|nil result (status, effectiveAutoTier, pendingAutoTier, ...)
-- @return string|nil error
function CopilotSession:set_auto_tier(auto_tier)
    local params = { sessionId = self.session_id, autoTier = auto_tier }
    if auto_tier == nil then
        params.autoTier = cjson.null
    end
    return self._rpc_client:request("session.model.switchAutoTier", params)
end

--- Retrieve the managed (enterprise policy) settings in effect for this session.
-- @return table|nil settings
-- @return string|nil error
function CopilotSession:get_managed_settings()
    return self._rpc_client:request("session.managedSettings.get", {
        sessionId = self.session_id,
    })
end

--- Ask the runtime to re-run the managed-settings policy helper for this session.
-- @return table|nil result
-- @return string|nil error
function CopilotSession:recheck_policy_helper()
    return self._rpc_client:request("session.managedSettings.recheckPolicyHelper", {
        sessionId = self.session_id,
    })
end

--- Change the session's permission mode.
-- @param mode string  "manual", "assisted" or "allow-all" (see types.PermissionMode)
-- @return table|nil PermissionModeSetResult { success, mode, reason }; reason is
--   "managed_policy" when enterprise policy refused the mode
-- @return string|nil error
function CopilotSession:set_permission_mode(mode)
    local result, err = self._rpc_client:request("session.permissions.setMode", {
        sessionId = self.session_id,
        mode      = mode,
    })
    if err then
        return nil, err
    end
    return types.PermissionModeSetResult(type(result) == "table" and result or {}), nil
end

--- Abort the currently processing message in this session.
-- @return boolean success
-- @return string|nil error
function CopilotSession:abort()
    local _, err = self._rpc_client:request("session.abort", {
        sessionId = self.session_id,
    })
    if err then
        return false, "failed to abort session: " .. tostring(err)
    end
    return true, nil
end

-- ---------------------------------------------------------------------------
-- Internal: event dispatch
-- ---------------------------------------------------------------------------

--- Dispatch an event to all registered handlers.
-- @param event table  SessionEvent
function CopilotSession:_dispatch_event(event)
    if not event then return end

    -- Copy handler list to avoid issues if handlers unsubscribe during iteration
    local handlers = {}
    for _, h in ipairs(self._handlers) do
        table.insert(handlers, h.fn)
    end

    for _, fn in ipairs(handlers) do
        local ok, err = pcall(fn, event)
        if not ok then
            io.stderr:write(string.format(
                "Error in session event handler: %s\n", tostring(err)
            ))
        end
    end
end

-- ---------------------------------------------------------------------------
-- Internal: tool registration
-- ---------------------------------------------------------------------------

--- Register tool handlers for this session.
-- @param tools table|nil  Array of Tool tables (with handler field)
function CopilotSession:_register_tools(tools)
    self._tool_handlers = {}
    if not tools then return end

    for _, tool in ipairs(tools) do
        if tool.name and tool.name ~= "" and tool.handler then
            self._tool_handlers[tool.name] = tool.handler
        end
    end
end

--- Get a registered tool handler by name.
-- @param name string
-- @return function|nil
function CopilotSession:_get_tool_handler(name)
    return self._tool_handlers[name]
end

-- ---------------------------------------------------------------------------
-- Internal: permission handler
-- ---------------------------------------------------------------------------

--- Register a permission handler.
-- @param handler function(request, invocation) -> PermissionRequestResult
function CopilotSession:_register_permission_handler(handler)
    self._permission_handler = handler
end

--- Get the registered permission handler.
-- @return function|nil
function CopilotSession:_get_permission_handler()
    return self._permission_handler
end

-- ---------------------------------------------------------------------------
-- Internal: user input handler
-- ---------------------------------------------------------------------------

--- Register a user input handler.
-- @param handler function(request, invocation) -> UserInputResponse
function CopilotSession:_register_user_input_handler(handler)
    self._user_input_handler = handler
end

--- Get the registered user input handler.
-- @return function|nil
function CopilotSession:_get_user_input_handler()
    return self._user_input_handler
end

-- ---------------------------------------------------------------------------
-- Internal: hooks
-- ---------------------------------------------------------------------------

--- Register hooks for this session.
-- @param hooks table  SessionHooks
function CopilotSession:_register_hooks(hooks)
    self._hooks = hooks
end

--- Get the registered hooks.
-- @return table|nil
function CopilotSession:_get_hooks()
    return self._hooks
end

-- ---------------------------------------------------------------------------
-- Internal: exit plan mode handler
-- ---------------------------------------------------------------------------

--- Register an exit plan mode handler.
-- @param handler function(request) -> ExitPlanModeResponse
function CopilotSession:_register_exit_plan_mode_handler(handler)
    self._exit_plan_mode_handler = handler
end

--- Get the registered exit plan mode handler.
-- @return function|nil
function CopilotSession:_get_exit_plan_mode_handler()
    return self._exit_plan_mode_handler
end

-- ---------------------------------------------------------------------------
-- Internal: trace context provider
-- ---------------------------------------------------------------------------

--- Register a trace context provider.
-- @param provider function() -> TraceContext
function CopilotSession:_register_trace_context_provider(provider)
    self._trace_context_provider = provider
end

--- Register a callback run once after destroy() releases the session.
-- @param callback function
function CopilotSession:_on_destroy(callback)
    if type(callback) == "function" then
        table.insert(self._destroy_callbacks, callback)
    end
end

-- ---------------------------------------------------------------------------
-- Internal: shell authentication and memory store
-- ---------------------------------------------------------------------------

--- Register the shell authentication handler.
-- @param handler function(ShellAuthenticationRequest, ctx) -> ShellAuthenticationResponse
function CopilotSession:_register_shell_authentication_handler(handler)
    self._shell_authentication_handler = handler
end

--- Ask the registered handler for a shell program's credentials. A failure, a
-- malformed answer, or a cancellation answers "cancel" so the program fails
-- instead of receiving a wrong value. The answer is never logged.
-- @param request table { sessionId, requestId, program, prompt, inputKind, command, cwd }
-- @return table { action = "accept", value = ... } or { action = "cancel" }
function CopilotSession:_handle_shell_authentication_request(request)
    local handler = self._shell_authentication_handler
    local request_id = request.requestId
    if not handler or self._pending_shell_authentications[request_id] ~= nil then
        return { action = "cancel" }
    end

    self._pending_shell_authentications[request_id] = false
    local ok, result = pcall(handler, request, { sessionId = self.session_id })
    local cancelled = self._pending_shell_authentications[request_id]
    self._pending_shell_authentications[request_id] = nil

    if not ok then
        -- The error text could contain the answer, so it is not logged.
        io.stderr:write("[CopilotSDK] Shell authentication handler failed\n")
        return { action = "cancel" }
    end
    if cancelled ~= false then
        return { action = "cancel" }
    end
    if type(result) == "table" and result.action == "accept" and type(result.value) == "string" then
        return { action = "accept", value = result.value }
    end
    return { action = "cancel" }
end

--- Cancel a pending shell authentication prompt; it then answers "cancel".
-- @param request_id string
function CopilotSession:_cancel_shell_authentication_request(request_id)
    if self._pending_shell_authentications[request_id] ~= nil then
        self._pending_shell_authentications[request_id] = true
    end
end

--- Register the memory store callback.
-- @param handler function(StoredMemory, ctx)
function CopilotSession:_register_memory_store(handler)
    self._memory_store_handler = handler
end

--- Hand a memory the agent asked to store to the registered callback.
-- @param memory table { sessionId, subject, fact, citations, reason }
-- @return boolean success
-- @return string|nil error
function CopilotSession:_handle_memory_store(memory)
    local handler = self._memory_store_handler
    if not handler then
        return false, "No memory store is registered"
    end
    local ok, err = pcall(handler, memory, { sessionId = self.session_id })
    if not ok then
        -- The runtime reports a generic failure to the model; keep the cause visible here.
        io.stderr:write("[CopilotSDK] Memory store callback failed: " .. tostring(err) .. "\n")
        return false, "Memory store callback failed"
    end
    return true, nil
end

--- Handle a hooks invocation from the server.
-- @param hook_type string    The hook type (e.g. "preToolUse", "postToolUse")
-- @param input table         The hook input data
-- @return table|nil output
-- @return string|nil error
function CopilotSession:_handle_hooks_invoke(hook_type, input)
    local hooks = self._hooks
    if not hooks then
        return nil, nil
    end

    local invocation = { sessionId = self.session_id }

    if hook_type == "preToolUse" then
        if not hooks.onPreToolUse then return nil, nil end
        local ok, result = pcall(hooks.onPreToolUse, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "postToolUse" then
        if not hooks.onPostToolUse then return nil, nil end
        local ok, result = pcall(hooks.onPostToolUse, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "userPromptSubmitted" then
        if not hooks.onUserPromptSubmitted then return nil, nil end
        local ok, result = pcall(hooks.onUserPromptSubmitted, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "userPromptTransformed" then
        if not hooks.onUserPromptTransformed then return nil, nil end
        local ok, result = pcall(hooks.onUserPromptTransformed, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "sessionStart" then
        if not hooks.onSessionStart then return nil, nil end
        local ok, result = pcall(hooks.onSessionStart, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "sessionEnd" then
        if not hooks.onSessionEnd then return nil, nil end
        local ok, result = pcall(hooks.onSessionEnd, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "errorOccurred" then
        if not hooks.onErrorOccurred then return nil, nil end
        local ok, result = pcall(hooks.onErrorOccurred, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "subagentStart" then
        if not hooks.onSubagentStart then return nil, nil end
        local ok, result = pcall(hooks.onSubagentStart, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    elseif hook_type == "subagentStop" then
        if not hooks.onSubagentStop then return nil, nil end
        local ok, result = pcall(hooks.onSubagentStop, input, invocation)
        if not ok then return nil, tostring(result) end
        return result, nil

    else
        return nil, "unknown hook type: " .. tostring(hook_type)
    end
end

return CopilotSession

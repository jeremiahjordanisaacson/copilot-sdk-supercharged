# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# CopilotSession -- represents a single conversation session with the Copilot CLI.

"""
    CopilotSession

A live conversation session backed by a JSON-RPC connection to the Copilot CLI.

Use `CopilotClient.create_session` to obtain an instance.  The session supports
event subscriptions via `on`, sending messages with `send` / `send_and_wait`,
and graceful teardown with `disconnect`.
"""
mutable struct CopilotSession
    session_id::String
    rpc::JsonRpcClient
    config::SessionConfig
    listeners::Vector{Function}
    tools::Dict{String, Tool}
    connected::Bool
    listener_lock::ReentrantLock
    close_callbacks::Vector{Function}  # run once by `disconnect`
end

function CopilotSession(session_id::String, rpc::JsonRpcClient, config::SessionConfig)
    tools = Dict{String, Tool}()
    for t in config.tools
        if t isa Tool
            tools[t.name] = t
        end
    end
    CopilotSession(session_id, rpc, config, Function[], tools, true, ReentrantLock(), Function[])
end

# -- Event handling ---------------------------------------------------------------

"""
    on(session, handler) -> Nothing

Register `handler(event::SessionEvent)` for all session events.
"""
function on(session::CopilotSession, handler::Function)
    lock(session.listener_lock) do
        push!(session.listeners, handler)
    end
    return nothing
end

"""
    on(handler, session) -> Nothing

Do-block form: `on(session) do event ... end`.
"""
on(handler::Function, session::CopilotSession) = on(session, handler)

"""Fire an event to all registered listeners."""
function _emit(session::CopilotSession, event::SessionEvent)
    handlers = lock(session.listener_lock) do
        copy(session.listeners)
    end
    for h in handlers
        try
            h(event)
        catch e
            @error "Session event handler error" event_type=event.type exception=(e, catch_backtrace())
        end
    end

    # Also call the config-level handler if present
    if session.config.on_event !== nothing
        try
            session.config.on_event(event)
        catch e
            @error "Config on_event handler error" exception=(e, catch_backtrace())
        end
    end
end

# -- Sending messages -------------------------------------------------------------

"""
    send(session, prompt::String; kwargs...)
    send(session, opts::MessageOptions)

Send a message to the session (`session.send`). Returns the message ID (or `nothing`);
listen for events via `on`.
"""
function send(session::CopilotSession, opts::MessageOptions)
    session.connected || throw(ErrorException("Session is disconnected"))
    params = Dict{String, Any}(
        "sessionId" => session.session_id,
        "prompt"    => opts.prompt,
    )
    if opts.attachments !== nothing
        params["attachments"] = opts.attachments
    end
    if opts.mode !== nothing
        params["mode"] = opts.mode
    end
    if opts.agent_mode !== nothing
        params["agentMode"] = opts.agent_mode
    end
    if opts.display_prompt !== nothing
        params["displayPrompt"] = opts.display_prompt
    end
    if opts.request_headers !== nothing
        params["requestHeaders"] = opts.request_headers
    end
    if opts.source !== nothing
        params["source"] = opts.source
    end
    if opts.client_turn_id !== nothing
        params["clientTurnId"] = opts.client_turn_id
    end
    if opts.response_schema !== nothing
        # Structured output: the reply is a JSON document matching the schema.
        params["responseFormat"] = Dict{String, Any}(
            "type" => "json_schema",
            "jsonSchema" => Dict{String, Any}(
                "name" => "response",
                "strict" => true,
                "schema" => opts.response_schema,
            ),
        )
    end
    result = send_request(session.rpc, "session.send", params)
    return result isa AbstractDict ? get(result, "messageId", nothing) : nothing
end

send(session::CopilotSession, prompt::String; kwargs...) =
    send(session, MessageOptions(; prompt, kwargs...))

"""
    send_and_wait(session, opts; timeout=120) -> SessionEvent

Send a message and block until the session becomes idle, returning the final
`session.idle` event. Raises on timeout.
"""
function send_and_wait(session::CopilotSession, opts::MessageOptions; timeout::Real=120)
    ch = Channel{SessionEvent}(1)
    on(session) do event
        if event.type == "session.idle"
            try put!(ch, event) catch; end
        end
    end

    send(session, opts)

    result = timedwait(timeout) do
        isready(ch)
    end
    if result === :timed_out
        throw(ErrorException("send_and_wait timed out after $(timeout)s"))
    end
    return take!(ch)
end

send_and_wait(session::CopilotSession, prompt::String; timeout::Real=120, kwargs...) =
    send_and_wait(session, MessageOptions(; prompt, kwargs...); timeout)

# -- Tool handling ----------------------------------------------------------------

"""Look up and invoke a tool handler registered in this session."""
function _handle_tool_call(session::CopilotSession, params::Dict{String, Any})
    tool_name = get(params, "toolName", get(params, "name", ""))
    tool = get(session.tools, tool_name, nothing)
    if tool === nothing
        return Dict{String, Any}(
            "resultType"       => "failure",
            "textResultForLlm" => "Unknown tool: $tool_name",
        )
    end

    invocation = ToolInvocation(;
        session_id   = session.session_id,
        tool_call_id = get(params, "toolCallId", ""),
        tool_name    = tool_name,
        arguments    = get(params, "arguments", nothing),
    )

    try
        result = tool.handler(invocation)
        if !(result isa ToolResult)
            result = ToolResult(text_result_for_llm=string(result))
        end
        return Dict{String, Any}(
            "resultType"       => TOOL_RESULT_STRINGS[result.result_type],
            "textResultForLlm" => result.text_result_for_llm,
        )
    catch e
        return Dict{String, Any}(
            "resultType"       => "failure",
            "textResultForLlm" => "Tool error: $(sprint(showerror, e))",
        )
    end
end

# -- Permission handling ----------------------------------------------------------

function _handle_permission(session::CopilotSession, params::Dict{String, Any})
    handler = session.config.on_permission_request
    if handler === nothing
        return Dict{String, Any}("decision" => "deny")
    end

    req = PermissionRequest(params)
    try
        decision = handler(req)
        if decision isa Bool
            decision = decision ? "allow" : "deny"
        end
        return Dict{String, Any}("decision" => string(decision))
    catch e
        @error "Permission handler error" exception=(e, catch_backtrace())
        return Dict{String, Any}("decision" => "deny")
    end
end

# -- Lifecycle --------------------------------------------------------------------

"""
    disconnect(session)

Disconnect from the session and release resources. Sends `session.detach` (falling back to
`session.destroy` on CLIs without it), so the session can be resumed later. Safe to call
multiple times.
"""
function disconnect(session::CopilotSession)
    session.connected || return nothing
    session.connected = false
    try
        detach_session(session)
    catch; end
    callbacks = copy(session.close_callbacks)
    empty!(session.close_callbacks)
    for callback in callbacks
        try callback() catch; end
    end
    return nothing
end

"""
    detach_session(session) -> Any

Send `session.detach` for the session (keeping its state for a later resume). Falls back to
`session.destroy` when the CLI does not implement `session.detach`.
"""
function detach_session(session::CopilotSession)
    params = Dict{String, Any}("sessionId" => session.session_id)
    try
        return send_request(session.rpc, "session.detach", params; timeout=10)
    catch e
        if e isa JsonRpcError && e.code == -32601
            return send_request(session.rpc, "session.destroy", params; timeout=10)
        end
        rethrow()
    end
end

"""
    switch_auto_tier(session, tier) -> Any

Change the experimental Auto routing preference (`"fast"`, `"efficiency"`, `"balance"`, or
`"intelligence"`) without changing the model; `nothing` restores the provider default.
"""
function switch_auto_tier(session::CopilotSession, tier::Union{AbstractString, Nothing})
    send_request(session.rpc, "session.model.switchAutoTier",
        Dict{String, Any}("sessionId" => session.session_id, "autoTier" => tier); timeout=30)
end

"""
    get_managed_settings(session) -> Any

Managed (enterprise) settings resolved for the session; read a `policyHelperFailure`
member with `policy_helper_failure`.
"""
get_managed_settings(session::CopilotSession) =
    send_request(session.rpc, "session.managedSettings.get",
        Dict{String, Any}("sessionId" => session.session_id); timeout=30)

"""
    recheck_policy_helper(session) -> Any

Re-run the managed-settings policy helper for the session.
"""
recheck_policy_helper(session::CopilotSession) =
    send_request(session.rpc, "session.managedSettings.recheckPolicyHelper",
        Dict{String, Any}("sessionId" => session.session_id); timeout=60)

"""
    set_permission_mode(session, mode) -> PermissionModeSetResult

Change the session's permission mode (`"manual"`, `"assisted"`, or `"allow-all"`). A refused
mode returns `success == false` and, when known, a `reason` such as
`PERMISSION_MODE_REFUSAL_MANAGED_POLICY`.
"""
function set_permission_mode(session::CopilotSession, mode::AbstractString)
    result = send_request(session.rpc, "session.permissions.setMode",
        Dict{String, Any}("sessionId" => session.session_id, "mode" => mode); timeout=30)
    result isa AbstractDict || return PermissionModeSetResult(mode=String(mode))
    reason = get(result, "reason", nothing)
    return PermissionModeSetResult(
        success = get(result, "success", false) === true,
        mode    = string(get(result, "mode", mode)),
        reason  = reason === nothing ? nothing : string(reason),
    )
end

# -- Permission requests (SDK protocol 4) -----------------------------------------

# PermissionRequest for a `permission.requested` event's data.
function _permission_request_from_event(data::AbstractDict)
    raw = get(data, "permissionRequest", nothing)
    details = raw isa AbstractDict ? Dict{String, Any}(raw) : Dict{String, Any}()
    kind = _str(get(details, "kind", ""))
    args = get(details, "args", nothing)
    return PermissionRequest(
        permission_id = _str(get(data, "requestId", "")),
        tool_name     = _str(get(details, "toolName", kind)),
        description   = _str(something(get(details, "intention", nothing),
                                       get(details, "toolDescription", nothing), "")),
        arguments     = args isa AbstractDict ? Dict{String, Any}(args) : Dict{String, Any}(),
        kind          = kind,
        tool_call_id  = _str(get(details, "toolCallId", "")),
        details       = details,
    )
end

# Wire decision (`result`) for a permission handler's return value: `true`/"allow" approve
# once, `false`/"deny" reject, `nothing` reports that no user is available, and a Dict or
# NamedTuple with a `kind` is sent as given. "no-result" sends nothing (the request stays
# pending for another client).
function _permission_decision_wire(decision)
    if decision isa AbstractDict || decision isa NamedTuple
        wire = Dict{String, Any}(String(k) => v for (k, v) in pairs(decision))
        return get(wire, "kind", nothing) == "no-result" ? nothing : wire
    end
    decision isa Bool && return Dict{String, Any}("kind" => decision ? "approve-once" : "reject")
    choice = decision === nothing ? "user-not-available" : string(decision)
    choice == "no-result" && return nothing
    if choice in ("allow", "approve", "approved", "approve-once")
        return Dict{String, Any}("kind" => "approve-once")
    end
    choice == "user-not-available" && return Dict{String, Any}("kind" => "user-not-available")
    return Dict{String, Any}("kind" => "reject")
end

"""Answer a `permission.requested` event with the session's `on_permission_request` handler
(`session.permissions.handlePendingPermissionRequest`)."""
function _handle_permission_requested(session::CopilotSession, event::SessionEvent)
    data = event.data
    get(data, "resolvedByHook", false) === true && return nothing
    handler = session.config.on_permission_request
    handler === nothing && return nothing
    request_id = get(data, "requestId", nothing)
    request_id isa AbstractString || return nothing

    result = try
        _permission_decision_wire(handler(_permission_request_from_event(data)))
    catch e
        @error "Permission handler error" exception=(e, catch_backtrace())
        Dict{String, Any}("kind" => "user-not-available")
    end
    (result === nothing || !session.connected) && return nothing
    try
        send_request(session.rpc, "session.permissions.handlePendingPermissionRequest",
            Dict{String, Any}("sessionId" => session.session_id, "requestId" => request_id,
                              "result" => result); timeout=30)
    catch e
        @error "Permission decision delivery failed" exception=(e, catch_backtrace())
    end
    return nothing
end

# -- External tools (SDK protocol 4) ----------------------------------------------

"""Run a registered tool for an `external_tool.requested` event and answer it with
`session.tools.handlePendingToolCall`."""
function _handle_external_tool(session::CopilotSession, event::SessionEvent)
    data = event.data
    get(data, "hasHandler", true) === false && return nothing
    request_id = get(data, "requestId", nothing)
    tool_name = get(data, "toolName", nothing)
    (request_id === nothing || tool_name === nothing) && return nothing
    haskey(session.tools, tool_name) || return nothing

    reply = _handle_tool_call(session, Dict{String, Any}(
        "toolName"   => tool_name,
        "toolCallId" => get(data, "toolCallId", ""),
        "arguments"  => get(data, "arguments", nothing),
    ))
    params = Dict{String, Any}("sessionId" => session.session_id, "requestId" => request_id)
    if get(reply, "resultType", "success") == "success"
        params["result"] = get(reply, "textResultForLlm", "")
    else
        params["error"] = get(reply, "textResultForLlm", "Tool failed")
    end
    @async try
        send_request(session.rpc, "session.tools.handlePendingToolCall", params; timeout=30)
    catch e
        @error "External tool result delivery failed" tool=tool_name exception=(e, catch_backtrace())
    end
    return nothing
end

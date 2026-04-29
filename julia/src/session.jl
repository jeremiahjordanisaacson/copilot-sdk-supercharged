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
end

function CopilotSession(session_id::String, rpc::JsonRpcClient, config::SessionConfig)
    tools = Dict{String, Tool}()
    for t in config.tools
        if t isa Tool
            tools[t.name] = t
        end
    end
    CopilotSession(session_id, rpc, config, Function[], tools, true, ReentrantLock())
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

Send a message to the session. Returns immediately; listen for events via `on`.
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
    send_request(session.rpc, "session/send", params)
    return nothing
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

send_and_wait(session::CopilotSession, prompt::String; kwargs...) =
    send_and_wait(session, MessageOptions(; prompt); kwargs...)

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

Disconnect from the session and release resources. Safe to call multiple times.
"""
function disconnect(session::CopilotSession)
    session.connected || return nothing
    session.connected = false
    try
        send_request(session.rpc, "session/disconnect",
            Dict{String, Any}("sessionId" => session.session_id); timeout=5)
    catch; end
    return nothing
end

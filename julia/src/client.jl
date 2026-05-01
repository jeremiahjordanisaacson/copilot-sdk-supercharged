# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# CopilotClient -- main entry point for the Copilot SDK.
#
# Manages the connection to the Copilot CLI server (spawned or external) and
# provides session creation, status queries, and lifecycle management.

const MIN_PROTOCOL_VERSION = 2
const SDK_PROTOCOL_VERSION = 3

"""
    CopilotClient

Main client for interacting with the Copilot CLI.

# Example

```julia
client = CopilotClient(CopilotClientOptions(cli_path="/path/to/copilot-cli"))
start!(client)
session = create_session(client, SessionConfig(model="gpt-4", on_permission_request=_ -> true))
on(session) do event
    if event.type == "assistant.message"
        println(get(event.data, "content", ""))
    end
end
send(session, "Hello!")
disconnect(session)
stop!(client)
```
"""
mutable struct CopilotClient
    options::CopilotClientOptions
    rpc::Union{JsonRpcClient, Nothing}
    process::Union{Base.Process, Nothing}
    state::ConnectionState
    sessions::Dict{String, CopilotSession}

    CopilotClient(opts::CopilotClientOptions=CopilotClientOptions()) =
        new(opts, nothing, nothing, DISCONNECTED, Dict{String, CopilotSession}())
end

# Allow keyword construction: CopilotClient(cli_path="...", ...)
CopilotClient(; kwargs...) = CopilotClient(CopilotClientOptions(; kwargs...))

# -- Lifecycle --------------------------------------------------------------------

"""
    start!(client) -> CopilotClient

Start the CLI server process (or connect to an external one). Must be called
before `create_session`.
"""
function start!(client::CopilotClient)
    client.state == CONNECTED && return client
    client.state = CONNECTING

    if client.options.cli_url !== nothing
        _connect_tcp!(client)
    else
        _spawn_stdio!(client)
    end

    client.state = CONNECTED
    return client
end

"""
    stop!(client)

Stop the CLI server and close all sessions.
"""
function stop!(client::CopilotClient)
    for (_, session) in client.sessions
        try disconnect(session) catch; end
    end
    empty!(client.sessions)

    if client.rpc !== nothing
        stop!(client.rpc)
        client.rpc = nothing
    end

    client.process = nothing
    client.state = DISCONNECTED
    return nothing
end

# -- Session management -----------------------------------------------------------

"""
    create_session(client, config::SessionConfig) -> CopilotSession
    create_session(client; kwargs...) -> CopilotSession

Create a new conversation session.

# Required keyword
- `on_permission_request` -- callback `(PermissionRequest) -> Bool/"allow"/"deny"`.
"""
function create_session(client::CopilotClient, config::SessionConfig)
    _ensure_started!(client)

    # Build the params dict for the RPC call
    params = Dict{String, Any}(
        "model"     => config.model,
        "streaming" => config.streaming,
    )

    if config.system_message !== nothing
        params["systemMessage"] = config.system_message
    end
    if config.instructions !== nothing
        params["instructions"] = config.instructions
    end
    if config.reasoning_effort !== nothing
        params["reasoningEffort"] = config.reasoning_effort
    end
    if config.agent !== nothing
        params["agent"] = config.agent
    end

    # Convert tools
    tool_list = Tool[]
    for t in config.tools
        if t isa Tool
            push!(tool_list, t)
        end
    end
    if !isempty(tool_list)
        params["tools"] = [tool_to_wire(t) for t in tool_list]
    end

    result = send_request(client.rpc, "session/create", params; timeout=30)

    session_id = if result isa Dict
        get(result, "sessionId", get(result, "session_id", string(UUIDs.uuid4())))
    else
        string(UUIDs.uuid4())
    end

    session = CopilotSession(session_id, client.rpc, config)
    client.sessions[session_id] = session

    # Register session-level RPC handlers
    _register_session_handlers!(client, session)

    return session
end

create_session(client::CopilotClient; kwargs...) =
    create_session(client, SessionConfig(; kwargs...))

# -- Server queries ---------------------------------------------------------------

"""
    get_status(client) -> ServerStatus

Query the server for version and authentication status.
"""
function get_status(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "getStatus", nothing; timeout=10)
    if result isa Dict
        ServerStatus(
            get(result, "version", ""),
            get(result, "protocolVersion", 0),
            get(result, "authenticated", false),
        )
    else
        ServerStatus()
    end
end

"""
    get_models(client) -> Vector{ModelInfo}

List available models from the server.
"""
function get_models(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "getModels", nothing; timeout=10)
    models = ModelInfo[]
    if result isa AbstractVector
        for m in result
            if m isa Dict
                push!(models, ModelInfo(;
                    id         = get(m, "id", ""),
                    name       = get(m, "name", ""),
                    vendor     = get(m, "vendor", ""),
                    family     = get(m, "family", ""),
                    version    = get(m, "version", ""),
                    is_default = get(m, "isDefault", false),
                ))
            end
        end
    end
    return models
end

"""
    list_sessions(client) -> Vector{SessionMetadata}

List sessions known to the server.
"""
function list_sessions(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "session/list", nothing; timeout=10)
    sessions = SessionMetadata[]
    if result isa AbstractVector
        for s in result
            if s isa Dict
                push!(sessions, SessionMetadata(;
                    session_id = get(s, "sessionId", ""),
                    title      = get(s, "title", nothing),
                    created_at = get(s, "createdAt", nothing),
                    updated_at = get(s, "updatedAt", nothing),
                    model      = get(s, "model", nothing),
                ))
            end
        end
    end
    return sessions
end

"""
    get_foreground_session_id(client) -> String

Return the session ID of the current foreground session.
"""
function get_foreground_session_id(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.getForeground", Dict{String,Any}(); timeout=10)
    if result isa Dict
        return get(result, "sessionId", "")
    end
    return ""
end

"""
    set_foreground_session_id(client, session_id)

Set the foreground session to the given session ID.
"""
function set_foreground_session_id(client::CopilotClient, session_id::AbstractString)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.setForeground",
        Dict{String,Any}("sessionId" => session_id); timeout=10)
    if result isa Dict && get(result, "success", false) != true
        error_msg = get(result, "error", "Unknown")
        throw(ErrorException("Failed to set foreground session: $error_msg"))
    end
end

# -- Internal helpers -------------------------------------------------------------

function _ensure_started!(client::CopilotClient)
    if client.state != CONNECTED
        if client.options.auto_start
            start!(client)
        else
            throw(ErrorException("Client is not connected. Call start!(client) first."))
        end
    end
end

function _spawn_stdio!(client::CopilotClient)
    cli_path = _resolve_cli_path(client.options)

    args = copy(client.options.cli_args)
    push!(args, "--stdio")
    log_str = get(LOG_LEVEL_STRINGS, client.options.log_level, "error")
    push!(args, "--log-level=$log_str")

    env_pairs = something(client.options.env, copy(ENV))

    cmd = Cmd(`$cli_path $args`; dir=something(client.options.cwd, pwd()))
    proc = open(cmd, "r+"; env=env_pairs)

    client.process = proc.process
    client.rpc = JsonRpcClient(proc.process, proc.in, proc.out)
    start!(client.rpc)

    _verify_protocol!(client)
end

function _connect_tcp!(client::CopilotClient)
    url = client.options.cli_url
    host, port = _parse_url(url)

    sock = Sockets.connect(host, port)
    # Wrap socket in a pseudo-process-like pipeline
    proc = _NullProcess()
    client.rpc = JsonRpcClient(proc, sock, sock)
    start!(client.rpc)

    _verify_protocol!(client)
end

"""Thin wrapper so the JsonRpcClient can treat a TCP socket like a process."""
mutable struct _NullProcess <: Base.AbstractPipe
    running::Bool
    _NullProcess() = new(true)
end
Base.process_running(p::_NullProcess) = p.running
Base.kill(p::_NullProcess) = (p.running = false; nothing)
Base.pipe_reader(p::_NullProcess) = devnull
Base.pipe_writer(p::_NullProcess) = devnull

function _parse_url(url::AbstractString)
    url = strip(url)
    url = replace(url, r"^https?://" => "")
    parts = split(url, ":")
    if length(parts) == 1
        return ("127.0.0.1", parse(Int, parts[1]))
    end
    host = String(parts[1])
    port = parse(Int, parts[2])
    return (host, port)
end

function _resolve_cli_path(opts::CopilotClientOptions)
    if opts.cli_path !== nothing
        return opts.cli_path
    end
    env_path = get(ENV, "COPILOT_CLI_PATH", "")
    if !isempty(env_path)
        return env_path
    end
    # Try to find `copilot-cli` on PATH
    found = Sys.which("copilot-cli")
    if found !== nothing
        return found
    end
    throw(ErrorException(
        "Could not find Copilot CLI. Set cli_path, COPILOT_CLI_PATH, or " *
        "ensure copilot-cli is on your PATH."
    ))
end

function _verify_protocol!(client::CopilotClient)
    try
        status = get_status(client)
        if status.protocol_version < MIN_PROTOCOL_VERSION
            @warn "Server protocol version $(status.protocol_version) < minimum $MIN_PROTOCOL_VERSION"
        end
    catch e
        @debug "Could not verify protocol version" exception=(e, catch_backtrace())
    end
end

function _register_session_handlers!(client::CopilotClient, session::CopilotSession)
    rpc = client.rpc
    rpc === nothing && return

    # Session events
    on_notification!(rpc, "session/event") do params
        sid = get(params, "sessionId", "")
        s = get(client.sessions, sid, nothing)
        s === nothing && return
        event_data = get(params, "event", params)
        event = SessionEvent(event_data)
        _emit(s, event)
    end

    # Tool calls
    on_request!(rpc, "session/toolCall") do params
        sid = get(params, "sessionId", "")
        s = get(client.sessions, sid, nothing)
        if s === nothing
            return Dict{String, Any}("resultType" => "failure", "textResultForLlm" => "Unknown session")
        end
        _handle_tool_call(s, params)
    end

    # Permission requests
    on_request!(rpc, "session/permissionRequest") do params
        sid = get(params, "sessionId", "")
        s = get(client.sessions, sid, nothing)
        if s === nothing
            return Dict{String, Any}("decision" => "deny")
        end
        _handle_permission(s, params)
    end
end

# Convenience permission handlers

"""Approve all permission requests. Pass as `on_permission_request=approve_all`."""
approve_all(::PermissionRequest) = "allow"

"""Deny all permission requests. Pass as `on_permission_request=deny_all`."""
deny_all(::PermissionRequest) = "deny"

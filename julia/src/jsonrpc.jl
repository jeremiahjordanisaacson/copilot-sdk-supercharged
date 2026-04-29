# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# Minimal JSON-RPC 2.0 client for stdio transport with Content-Length framing (LSP style).

"""
    JsonRpcError <: Exception

Represents a JSON-RPC error response from the server.
"""
struct JsonRpcError <: Exception
    code::Int
    message::String
    data::Any
end

function Base.showerror(io::IO, e::JsonRpcError)
    print(io, "JsonRpcError($(e.code)): $(e.message)")
    if e.data !== nothing
        print(io, " | data=$(e.data)")
    end
end

"""
    ProcessExitedError <: Exception

Raised when the CLI process exits unexpectedly.
"""
struct ProcessExitedError <: Exception
    message::String
end

Base.showerror(io::IO, e::ProcessExitedError) = print(io, "ProcessExitedError: $(e.message)")

"""
    JsonRpcClient

Async JSON-RPC 2.0 client that communicates over stdio using Content-Length framing.
"""
mutable struct JsonRpcClient
    process::Base.Process
    input::IO
    output::IO
    pending::Dict{String, Channel{Any}}
    notification_handlers::Dict{String, Function}
    request_handlers::Dict{String, Function}
    running::Bool
    read_task::Union{Task, Nothing}
    write_lock::ReentrantLock
    pending_lock::ReentrantLock

    function JsonRpcClient(process::Base.Process, input::IO, output::IO)
        new(
            process, input, output,
            Dict{String, Channel{Any}}(),
            Dict{String, Function}(),
            Dict{String, Function}(),
            false, nothing,
            ReentrantLock(), ReentrantLock(),
        )
    end
end

"""Start the background reader task."""
function start!(client::JsonRpcClient)
    client.running = true
    client.read_task = @async _read_loop(client)
    return client
end

"""Stop the client and close the process."""
function stop!(client::JsonRpcClient)
    client.running = false
    try
        close(client.input)
    catch; end
    try
        if process_running(client.process)
            kill(client.process)
        end
    catch; end
    # Resolve all pending requests with an error
    lock(client.pending_lock) do
        for (_, ch) in client.pending
            try
                put!(ch, ProcessExitedError("Client stopped"))
            catch; end
        end
        empty!(client.pending)
    end
    return nothing
end

"""Register a handler for server-initiated notifications."""
function on_notification!(client::JsonRpcClient, method::String, handler::Function)
    client.notification_handlers[method] = handler
end

"""Register a handler for server-initiated requests."""
function on_request!(client::JsonRpcClient, method::String, handler::Function)
    client.request_handlers[method] = handler
end

"""
    send_request(client, method, params; timeout=60) -> Any

Send a JSON-RPC request and wait for the response.
"""
function send_request(client::JsonRpcClient, method::String, params::Any=nothing; timeout::Real=60)
    id = string(UUIDs.uuid4())
    ch = Channel{Any}(1)

    lock(client.pending_lock) do
        client.pending[id] = ch
    end

    msg = Dict{String, Any}("jsonrpc" => "2.0", "id" => id, "method" => method)
    if params !== nothing
        msg["params"] = params
    end

    _write_message(client, msg)

    # Wait for the response with timeout
    result = timedwait(timeout) do
        isready(ch)
    end

    lock(client.pending_lock) do
        delete!(client.pending, id)
    end

    if result === :timed_out
        throw(JsonRpcError(-32000, "Request timed out after $(timeout)s", nothing))
    end

    response = take!(ch)
    if response isa Exception
        throw(response)
    end
    return response
end

"""Send a JSON-RPC notification (no response expected)."""
function send_notification(client::JsonRpcClient, method::String, params::Any=nothing)
    msg = Dict{String, Any}("jsonrpc" => "2.0", "method" => method)
    if params !== nothing
        msg["params"] = params
    end
    _write_message(client, msg)
    return nothing
end

# -- Internal helpers --

function _write_message(client::JsonRpcClient, msg::Dict)
    body = JSON3.write(msg)
    header = "Content-Length: $(sizeof(body))\r\n\r\n"
    lock(client.write_lock) do
        write(client.input, header)
        write(client.input, body)
        flush(client.input)
    end
end

function _read_loop(client::JsonRpcClient)
    try
        while client.running && process_running(client.process)
            msg = _read_message(client)
            msg === nothing && continue
            _dispatch(client, msg)
        end
    catch e
        if client.running && !(e isa EOFError || e isa Base.IOError)
            @error "JsonRpcClient read loop error" exception=(e, catch_backtrace())
        end
    finally
        client.running = false
        lock(client.pending_lock) do
            for (_, ch) in client.pending
                try
                    put!(ch, ProcessExitedError("Process exited"))
                catch; end
            end
            empty!(client.pending)
        end
    end
end

function _read_message(client::JsonRpcClient)
    # Read Content-Length header
    content_length = -1
    while true
        line = readline(client.output)
        isempty(line) && content_length >= 0 && break
        if startswith(line, "Content-Length:")
            content_length = parse(Int, strip(replace(line, "Content-Length:" => "")))
        end
        # Skip blank separator lines when we already parsed a header
        isempty(line) && content_length >= 0 && break
    end
    content_length < 0 && return nothing

    body = read(client.output, content_length)
    return JSON3.read(String(body), Dict{String, Any})
end

function _dispatch(client::JsonRpcClient, msg::Dict{String, Any})
    if haskey(msg, "id") && haskey(msg, "method")
        # Server request
        _handle_server_request(client, msg)
    elseif haskey(msg, "id")
        # Response to our request
        _handle_response(client, msg)
    elseif haskey(msg, "method")
        # Notification
        _handle_notification(client, msg)
    end
end

function _handle_response(client::JsonRpcClient, msg::Dict{String, Any})
    id = string(msg["id"])
    ch = lock(client.pending_lock) do
        get(client.pending, id, nothing)
    end
    ch === nothing && return

    if haskey(msg, "error")
        err = msg["error"]
        put!(ch, JsonRpcError(
            get(err, "code", -1),
            get(err, "message", "Unknown error"),
            get(err, "data", nothing),
        ))
    else
        put!(ch, get(msg, "result", nothing))
    end
end

function _handle_notification(client::JsonRpcClient, msg::Dict{String, Any})
    method = msg["method"]
    params = get(msg, "params", Dict{String, Any}())
    handler = get(client.notification_handlers, method, nothing)
    if handler !== nothing
        @async try
            handler(params)
        catch e
            @error "Notification handler error" method exception=(e, catch_backtrace())
        end
    end
end

function _handle_server_request(client::JsonRpcClient, msg::Dict{String, Any})
    id = msg["id"]
    method = msg["method"]
    params = get(msg, "params", Dict{String, Any}())
    handler = get(client.request_handlers, method, nothing)

    @async try
        if handler === nothing
            _send_error_response(client, id, -32601, "Method not found: $method")
            return
        end
        result = handler(params)
        _send_response(client, id, result)
    catch e
        if e isa JsonRpcError
            _send_error_response(client, id, e.code, e.message, e.data)
        else
            _send_error_response(client, id, -32603, "Internal error: $(sprint(showerror, e))")
        end
    end
end

function _send_response(client::JsonRpcClient, id, result)
    msg = Dict{String, Any}("jsonrpc" => "2.0", "id" => id, "result" => result)
    _write_message(client, msg)
end

function _send_error_response(client::JsonRpcClient, id, code::Int, message::String, data=nothing)
    err = Dict{String, Any}("code" => code, "message" => message)
    data !== nothing && (err["data"] = data)
    msg = Dict{String, Any}("jsonrpc" => "2.0", "id" => id, "error" => err)
    _write_message(client, msg)
end

"""Helper that waits for a condition with a timeout, returning :ok or :timed_out."""
function timedwait(cond::Function, timeout::Real)
    deadline = time() + timeout
    while !cond()
        if time() >= deadline
            return :timed_out
        end
        sleep(0.01)
    end
    return :ok
end

# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

"""
    TestHarness

Manages the replaying CAPI proxy for Julia E2E tests.

Spawns the shared test harness server (`test/harness/server.ts`) via `npx tsx`
and provides `start_proxy()` / `stop_proxy()` helpers.
"""
module TestHarness

using HTTP

# Module-level state for the proxy process and URL
const _process = Ref{Union{Base.Process, Nothing}}(nothing)
const _proxy_url = Ref{Union{String, Nothing}}(nothing)
const _stdout_pipe = Ref{Union{IO, Nothing}}(nothing)

"""
    start_proxy() -> String

Launch the replaying CAPI proxy and return its URL (e.g. `"http://localhost:PORT"`).
Subsequent calls return the cached URL if the proxy is already running.
"""
function start_proxy()::String
    if _proxy_url[] !== nothing
        return _proxy_url[]
    end

    # Resolve the server.ts path relative to this file:
    #   julia/e2e/testharness/proxy.jl  →  test/harness/server.ts
    harness_dir = normpath(joinpath(@__DIR__, "..", "..", "..", "test", "harness"))
    server_path = joinpath(harness_dir, "server.ts")
    isfile(server_path) || error("Harness server not found at $server_path")

    # On Windows we need `cmd /c npx` so the shell can resolve npx.cmd
    use_shell = Sys.iswindows()
    cmd = if use_shell
        Cmd(`cmd /c npx tsx $server_path`; dir=harness_dir)
    else
        Cmd(`npx tsx $server_path`; dir=harness_dir)
    end

    # Spawn with a readable stdout pipe; inherit stderr for debug output
    pipe = Pipe()
    proc = run(pipeline(cmd; stdout=pipe, stderr=stderr); wait=false)
    close(pipe.in)  # we only read from the pipe

    _process[] = proc
    _stdout_pipe[] = pipe

    # Read the first line — expect "Listening: http://localhost:XXXX"
    line = readline(pipe)
    if isempty(line)
        kill(proc)
        error("Failed to read proxy URL — process produced no output")
    end

    m = match(r"Listening:\s+(http://[^\s]+)", line)
    if m === nothing
        kill(proc)
        error("Unexpected proxy output: $line")
    end

    _proxy_url[] = String(m.captures[1])
    return _proxy_url[]
end

"""
    stop_proxy(; skip_writing_cache::Bool=false)

Gracefully shut down the replaying proxy.

Sends a POST to `/stop` first, then waits for the process to exit.
"""
function stop_proxy(; skip_writing_cache::Bool=false)
    if _process[] === nothing
        return
    end

    # Best-effort graceful shutdown via HTTP
    if _proxy_url[] !== nothing
        try
            stop_url = _proxy_url[]::String * "/stop"
            if skip_writing_cache
                stop_url *= "?skipWritingCache=true"
            end
            HTTP.post(stop_url; readtimeout=5, connect_timeout=5)
        catch
            # Ignore — we'll force-kill below if needed
        end
    end

    proc = _process[]
    if proc !== nothing && process_running(proc)
        try
            wait(proc)
        catch
            kill(proc)
        end
    end

    _process[] = nothing
    _proxy_url[] = nothing
    _stdout_pipe[] = nothing
    return nothing
end

"""
    cli_url_from_proxy(proxy_url::String) -> String

Strip the `http://` scheme from a proxy URL so it can be used as a `cli_url`
value (e.g. `"http://localhost:3000"` → `"localhost:3000"`).
"""
function cli_url_from_proxy(proxy_url::String)::String
    return replace(proxy_url, r"^https?://" => "")
end

end # module TestHarness

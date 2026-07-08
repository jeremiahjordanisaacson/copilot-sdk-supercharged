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
const _connect_proxy_url = Ref{Union{String, Nothing}}(nothing)
const _ca_file_path = Ref{Union{String, Nothing}}(nothing)
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

    # Read the first line — expect "Listening: http://localhost:XXXX {...}"
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

    # Parse connect proxy metadata JSON from the same line
    meta_match = match(r"(\{.*\})\s*$", line)
    if meta_match !== nothing
        try
            meta_json = JSON_parse(String(meta_match.captures[1]))
            if haskey(meta_json, "connectProxyUrl")
                _connect_proxy_url[] = meta_json["connectProxyUrl"]
            end
            if haskey(meta_json, "caFilePath")
                _ca_file_path[] = meta_json["caFilePath"]
            end
        catch
            # Non-fatal
        end
    end

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
    _connect_proxy_url[] = nothing
    _ca_file_path[] = nothing
    _stdout_pipe[] = nothing
    return nothing
end

"""
    cli_path_from_repo() -> String

Resolve the Copilot CLI executable path.
"""
function cli_path_from_repo()::String
    env_path = get(ENV, "COPILOT_CLI_PATH", "")
    if !isempty(env_path) && isfile(env_path)
        return abspath(env_path)
    end

    repo_root = normpath(joinpath(@__DIR__, "..", "..", ".."))
    node_cli = joinpath(repo_root, "nodejs", "node_modules", "@github", "copilot", "index.js")
    if isfile(node_cli)
        return abspath(node_cli)
    end

    return "copilot"
end

"""
    proxy_test_env(work_dir::String) -> Dict{String,String}

Return environment variables that route CLI traffic through the proxy.
"""
function proxy_test_env(work_dir::String)::Dict{String,String}
    env = Dict{String,String}()
    for (k, v) in ENV
        env[k] = v
    end
    env["COPILOT_API_URL"] = _proxy_url[] !== nothing ? _proxy_url[] : ""
    env["COPILOT_HOME"] = work_dir
    env["XDG_CONFIG_HOME"] = work_dir
    env["XDG_STATE_HOME"] = work_dir
    env["GH_TOKEN"] = get(ENV, "GH_TOKEN", "fake-test-token")
    env["GITHUB_TOKEN"] = get(ENV, "GITHUB_TOKEN", "fake-test-token")

    if _connect_proxy_url[] !== nothing
        url = _connect_proxy_url[]
        env["HTTP_PROXY"] = url
        env["HTTPS_PROXY"] = url
        env["http_proxy"] = url
        env["https_proxy"] = url
        env["NO_PROXY"] = "127.0.0.1,localhost,::1"
        env["no_proxy"] = "127.0.0.1,localhost,::1"
    end
    if _ca_file_path[] !== nothing
        path = _ca_file_path[]
        env["NODE_EXTRA_CA_CERTS"] = path
        env["SSL_CERT_FILE"] = path
    end

    return env
end

# Simple JSON parser for the metadata line (avoids adding JSON dependency)
function JSON_parse(s::String)
    result = Dict{String,Any}()
    # Match "key":"value" pairs
    for m in eachmatch(r"\"([^\"]+)\"\s*:\s*\"([^\"]+)\"", s)
        result[m.captures[1]] = m.captures[2]
    end
    return result
end

end # module TestHarness

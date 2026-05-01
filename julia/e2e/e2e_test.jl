# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

#=
  Julia E2E tests for the Copilot SDK.

  Runs against the shared replaying CAPI proxy (test/harness/server.ts).
  Execute from the repo root:

      cd julia && julia --project=. e2e/e2e_test.jl
=#

using Test

# ── Bootstrap: load the test harness and the SDK ────────────────────────────
include(joinpath(@__DIR__, "testharness", "proxy.jl"))
using .TestHarness: start_proxy, stop_proxy, cli_url_from_proxy

# Add the parent julia/ dir to LOAD_PATH so `using CopilotSDK` resolves
push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

# ── Shared setup / teardown ─────────────────────────────────────────────────
proxy_url = start_proxy()
cli_url   = cli_url_from_proxy(proxy_url)

# Ensure the proxy is torn down no matter what
atexit() do
    stop_proxy(; skip_writing_cache=true)
end

# ── Tests ───────────────────────────────────────────────────────────────────

@testset "Julia SDK E2E" begin

    @testset "Session create and disconnect" begin
        client = CopilotClient(CopilotClientOptions(cli_url=cli_url))
        start!(client)

        session = create_session(client, SessionConfig(model="gpt-4"))

        @test session isa CopilotSession
        @test !isempty(session.session_id)

        disconnect(session)
        stop!(client)
    end

    @testset "Send message and receive response" begin
        client = CopilotClient(cli_url=cli_url)
        start!(client)

        # Collect assistant messages via event handler
        messages = String[]
        config = SessionConfig(
            model="gpt-4",
            on_event=function (event)
                if event.type == "assistant.message"
                    content = get(event.data, "content", "")
                    if !isempty(content)
                        push!(messages, content)
                    end
                end
            end,
        )

        session = create_session(client, config)

        # send_and_wait blocks until the session goes idle
        send_and_wait(session, "Hello!")

        @test length(messages) > 0
        @test any(m -> !isempty(m), messages)

        disconnect(session)
        stop!(client)
    end

    @testset "SessionFs provider configuration" begin
        fs_config = SessionFsConfig(
            initial_cwd="/home/user/project",
            session_state_path="/home/user/.copilot/session-state",
            conventions="posix",
        )

        client = CopilotClient(CopilotClientOptions(
            cli_url=cli_url,
            session_fs=fs_config,
        ))

        @test client.options.session_fs !== nothing
        @test client.options.session_fs.initial_cwd == "/home/user/project"
        @test client.options.session_fs.session_state_path == "/home/user/.copilot/session-state"
        @test client.options.session_fs.conventions == "posix"

        start!(client)

        session = create_session(client, SessionConfig(model="gpt-4"))
        @test session isa CopilotSession

        disconnect(session)
        stop!(client)
    end

end  # top-level testset

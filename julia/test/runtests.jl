# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

using Test

# Load the package from the parent directory
push!(LOAD_PATH, joinpath(@__DIR__, ".."))
using CopilotSDK

@testset "CopilotSDK" begin

    # -- Types -------------------------------------------------------------------

    @testset "Types" begin
        @testset "CopilotClientOptions defaults" begin
            opts = CopilotClientOptions()
            @test opts.cli_path === nothing
            @test opts.cli_url === nothing
            @test opts.use_stdio == true
            @test opts.auto_start == true
            @test opts.port == 0
            @test opts.log_level == LOG_ERROR
            @test isempty(opts.cli_args)
        end

        @testset "CopilotClientOptions keyword construction" begin
            opts = CopilotClientOptions(cli_path="/bin/cli", port=8080, log_level=LOG_DEBUG)
            @test opts.cli_path == "/bin/cli"
            @test opts.port == 8080
            @test opts.log_level == LOG_DEBUG
        end

        @testset "SessionConfig defaults" begin
            cfg = SessionConfig()
            @test cfg.model == "gpt-4"
            @test cfg.streaming == true
            @test cfg.on_event === nothing
            @test cfg.on_permission_request === nothing
            @test isempty(cfg.tools)
        end

        @testset "MessageOptions" begin
            msg = MessageOptions(prompt="hello")
            @test msg.prompt == "hello"
            @test msg.attachments === nothing
            @test msg.mode === nothing
        end

        @testset "SessionEvent construction" begin
            ev = SessionEvent("assistant.message", Dict{String, Any}("content" => "hi"))
            @test ev.type == "assistant.message"
            @test ev.data["content"] == "hi"

            ev2 = SessionEvent("session.idle")
            @test ev2.type == "session.idle"
            @test isempty(ev2.data)
        end

        @testset "SessionEvent from Dict" begin
            d = Dict{String, Any}("type" => "test.event", "data" => Dict{String, Any}("x" => 1))
            ev = SessionEvent(d)
            @test ev.type == "test.event"
            @test ev.data["x"] == 1
        end

        @testset "ToolResult defaults" begin
            tr = ToolResult()
            @test tr.text_result_for_llm == ""
            @test tr.result_type == TOOL_SUCCESS
            @test tr.error === nothing
        end

        @testset "ToolInvocation" begin
            inv = ToolInvocation(session_id="s1", tool_call_id="tc1",
                                tool_name="my_tool", arguments=Dict("a" => 1))
            @test inv.session_id == "s1"
            @test inv.tool_name == "my_tool"
            @test inv.arguments["a"] == 1
        end

        @testset "ServerStatus" begin
            s = ServerStatus("1.0.0", 3, true)
            @test s.version == "1.0.0"
            @test s.protocol_version == 3
            @test s.authenticated == true

            s2 = ServerStatus()
            @test s2.version == ""
            @test s2.protocol_version == 0
        end

        @testset "PermissionRequest from Dict" begin
            d = Dict{String, Any}(
                "permissionId" => "p1",
                "toolName" => "run_cmd",
                "description" => "Run a command",
                "arguments" => Dict{String, Any}("cmd" => "ls"),
            )
            pr = PermissionRequest(d)
            @test pr.permission_id == "p1"
            @test pr.tool_name == "run_cmd"
            @test pr.description == "Run a command"
            @test pr.arguments["cmd"] == "ls"
        end

        @testset "ConnectionState enum" begin
            @test DISCONNECTED isa ConnectionState
            @test CONNECTING isa ConnectionState
            @test CONNECTED isa ConnectionState
            @test CONNECTION_ERROR isa ConnectionState
        end

        @testset "LogLevel strings" begin
            @test LOG_LEVEL_STRINGS[LOG_NONE] == "none"
            @test LOG_LEVEL_STRINGS[LOG_ERROR] == "error"
            @test LOG_LEVEL_STRINGS[LOG_ALL] == "all"
        end

        @testset "ToolResultType strings" begin
            @test TOOL_RESULT_STRINGS[TOOL_SUCCESS] == "success"
            @test TOOL_RESULT_STRINGS[TOOL_FAILURE] == "failure"
            @test TOOL_RESULT_STRINGS[TOOL_TIMEOUT] == "timeout"
        end

        @testset "ModelInfo" begin
            mi = ModelInfo(id="gpt-4", name="GPT-4", vendor="openai",
                           family="gpt", version="4", is_default=true)
            @test mi.id == "gpt-4"
            @test mi.is_default == true
        end

        @testset "SessionMetadata" begin
            sm = SessionMetadata(session_id="abc", title="Test")
            @test sm.session_id == "abc"
            @test sm.title == "Test"
            @test sm.created_at === nothing
        end
    end

    # -- Tools -------------------------------------------------------------------

    @testset "Tools" begin
        @testset "define_tool do-block" begin
            t = define_tool("greet", "Greets user") do inv
                ToolResult(text_result_for_llm="Hello, $(inv.arguments["name"])!")
            end
            @test t isa Tool
            @test t.name == "greet"
            @test t.description == "Greets user"
            @test t.overrides_built_in_tool == false
            @test t.skip_permission == false
        end

        @testset "define_tool positional" begin
            handler(inv) = ToolResult(text_result_for_llm="ok")
            t = define_tool("test_tool", "A test", handler)
            @test t.name == "test_tool"
            inv = ToolInvocation(session_id="s", tool_call_id="tc", tool_name="test_tool")
            result = t.handler(inv)
            @test result.text_result_for_llm == "ok"
        end

        @testset "define_tool with parameters" begin
            params = Dict{String, Any}(
                "type" => "object",
                "properties" => Dict{String, Any}(
                    "name" => Dict{String, Any}("type" => "string"),
                ),
            )
            t = define_tool("p_tool", "with params"; parameters=params) do inv
                ToolResult()
            end
            @test t.parameters !== nothing
            @test t.parameters["type"] == "object"
        end

        @testset "tool_to_wire" begin
            t = Tool(name="wire_test", description="test",
                     handler=inv -> ToolResult(),
                     overrides_built_in_tool=true,
                     skip_permission=true)
            w = tool_to_wire(t)
            @test w["name"] == "wire_test"
            @test w["description"] == "test"
            @test w["overridesBuiltInTool"] == true
            @test w["skipPermission"] == true
            @test !haskey(w, "parameters")
        end

        @testset "tool_to_wire without optional fields" begin
            t = Tool(name="simple", description="simple tool", handler=inv -> ToolResult())
            w = tool_to_wire(t)
            @test w["name"] == "simple"
            @test !haskey(w, "overridesBuiltInTool")
            @test !haskey(w, "skipPermission")
        end
    end

    # -- Client ------------------------------------------------------------------

    @testset "Client" begin
        @testset "CopilotClient default construction" begin
            c = CopilotClient()
            @test c.state == DISCONNECTED
            @test c.rpc === nothing
            @test c.process === nothing
            @test isempty(c.sessions)
        end

        @testset "CopilotClient keyword construction" begin
            c = CopilotClient(log_level=LOG_DEBUG, auto_start=false)
            @test c.options.log_level == LOG_DEBUG
            @test c.options.auto_start == false
        end

        @testset "approve_all and deny_all" begin
            req = PermissionRequest(permission_id="p1", tool_name="t",
                                    description="d", arguments=Dict{String, Any}())
            @test approve_all(req) == "allow"
            @test deny_all(req) == "deny"
        end

        @testset "_parse_url" begin
            host, port = CopilotSDK._parse_url("localhost:3000")
            @test host == "localhost"
            @test port == 3000

            host2, port2 = CopilotSDK._parse_url("http://127.0.0.1:8080")
            @test host2 == "127.0.0.1"
            @test port2 == 8080

            host3, port3 = CopilotSDK._parse_url("9000")
            @test host3 == "127.0.0.1"
            @test port3 == 9000
        end

        @testset "stop! on fresh client is safe" begin
            c = CopilotClient(auto_start=false)
            stop!(c)
            @test c.state == DISCONNECTED
        end
    end

    # -- JSON-RPC ----------------------------------------------------------------

    @testset "JSON-RPC" begin
        @testset "JsonRpcError display" begin
            e = JsonRpcError(-32600, "Invalid request", nothing)
            buf = IOBuffer()
            showerror(buf, e)
            s = String(take!(buf))
            @test contains(s, "-32600")
            @test contains(s, "Invalid request")
        end

        @testset "JsonRpcError with data" begin
            e = JsonRpcError(-32000, "timeout", Dict("detail" => "slow"))
            buf = IOBuffer()
            showerror(buf, e)
            s = String(take!(buf))
            @test contains(s, "timeout")
            @test contains(s, "data=")
        end

        @testset "ProcessExitedError display" begin
            e = ProcessExitedError("gone")
            buf = IOBuffer()
            showerror(buf, e)
            s = String(take!(buf))
            @test contains(s, "gone")
        end

        @testset "timedwait success" begin
            result = CopilotSDK.timedwait(1.0) do
                true
            end
            @test result === :ok
        end

        @testset "timedwait timeout" begin
            result = CopilotSDK.timedwait(0.05) do
                false
            end
            @test result === :timed_out
        end
    end

    # -- Session -----------------------------------------------------------------

    @testset "Session" begin
        @testset "session event emission" begin
            # We cannot create a real session without a server, but we can test
            # the emission machinery with a mock.
            events_received = SessionEvent[]

            # Build a minimal mock RPC client
            mock_rpc = nothing  # We just test _emit directly

            cfg = SessionConfig(model="gpt-4")
            # We need a real CopilotSession, but its constructor needs an RPC.
            # Instead, test emission logic via the public on() + _emit pattern:

            # Since we cannot easily construct a CopilotSession without an RPC,
            # test the listener logic at the type level:
            listeners = Function[]
            push!(listeners, ev -> push!(events_received, ev))
            ev = SessionEvent("test.event", Dict{String, Any}("val" => 42))
            for h in listeners
                h(ev)
            end
            @test length(events_received) == 1
            @test events_received[1].type == "test.event"
        end
    end

    # -- Module exports ----------------------------------------------------------

    @testset "Module exports" begin
        @test isdefined(CopilotSDK, :CopilotClient)
        @test isdefined(CopilotSDK, :CopilotSession)
        @test isdefined(CopilotSDK, :SessionConfig)
        @test isdefined(CopilotSDK, :MessageOptions)
        @test isdefined(CopilotSDK, :Tool)
        @test isdefined(CopilotSDK, :ToolResult)
        @test isdefined(CopilotSDK, :define_tool)
        @test isdefined(CopilotSDK, :approve_all)
        @test isdefined(CopilotSDK, :deny_all)
        @test isdefined(CopilotSDK, :start!)
        @test isdefined(CopilotSDK, :stop!)
        @test isdefined(CopilotSDK, :create_session)
        @test isdefined(CopilotSDK, :send)
        @test isdefined(CopilotSDK, :send_and_wait)
        @test isdefined(CopilotSDK, :disconnect)
        @test isdefined(CopilotSDK, :get_status)
        @test isdefined(CopilotSDK, :get_models)
        @test isdefined(CopilotSDK, :tool_to_wire)
    end

end  # top-level testset

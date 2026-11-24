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

    # -- SDK protocol 4 -------------------------------------------------------------

    @testset "SDK protocol 4" begin
        @testset "protocol version" begin
            @test CopilotSDK.SDK_PROTOCOL_VERSION == 4
            @test CopilotSDK.MIN_PROTOCOL_VERSION == 4
        end

        @testset "group_session_params" begin
            flat = Dict{String, Any}("model" => "m", "streaming" => true, "requestPermission" => true,
                "tools" => Any[Dict{String, Any}("name" => "t")], "hooks" => true, "disableResume" => true)
            @test group_session_params(flat, true) == Dict{String, Any}(
                "model" => "m",
                "events" => Dict{String, Any}("streaming" => true),
                "permissions" => Dict{String, Any}("request" => true),
                "tools" => Dict{String, Any}("definitions" => Any[Dict{String, Any}("name" => "t")]),
                "hooks" => Dict{String, Any}("hasHandlers" => true),
                "suppressResumeEvent" => true,
            )

            flat = Dict{String, Any}("trustWorkingDirectory" => "session", "openCanvases" => Any[],
                "customKey" => 1, "agent" => nothing, "mcpServers" => Dict{String, Any}(),
                "defaultProvider" => missing)
            created = group_session_params(flat, false)
            @test created["filesystem"] == Dict{String, Any}("trustWorkingDirectory" => "session")
            @test !haskey(created, "canvases")
            @test created["customKey"] == 1
            @test !haskey(created, "agents")
            @test created["mcp"]["envValueMode"] == "direct"
            @test created["inference"]["defaultProvider"] === nothing
            resumed = group_session_params(flat, true)
            @test !haskey(resumed, "filesystem")
            @test resumed["canvases"] == Dict{String, Any}("open" => Any[])
        end

        @testset "legacy provider" begin
            flat = Dict{String, Any}("model" => "m1", "provider" => Dict{String, Any}(
                "type" => "openai", "baseUrl" => "https://b", "maxOutputTokens" => 10))
            p = group_session_params(flat)
            @test p["inference"]["providers"] == Any[Dict{String, Any}(
                "name" => "default", "type" => "openai", "baseUrl" => "https://b")]
            @test p["inference"]["defaultProvider"] == "default"
            @test p["inference"]["models"] == Any[Dict{String, Any}(
                "provider" => "default", "id" => "m1", "maxOutputTokens" => 10)]
            @test p["github"]["capi"] == Dict{String, Any}("enabled" => false)
            @test !haskey(p, "provider")

            flat["providers"] = Any[Dict{String, Any}("name" => "azure", "baseUrl" => "https://x")]
            p = group_session_params(flat)
            @test p["inference"]["providers"] == flat["providers"]
            @test !haskey(p, "github")
        end

        @testset "client mode" begin
            @test CopilotClient().options.mode == CLIENT_MODE_CODING_AGENT
            @test CopilotClient(mode="copilot-cli").options.mode == CLIENT_MODE_CODING_AGENT
            @test_throws ArgumentError CopilotClient(mode="bogus")

            c = CopilotClient(mode=CLIENT_MODE_EMPTY)
            @test_throws ArgumentError CopilotSDK._require_available_tools(c, SessionConfig())
            cfg = SessionConfig(available_tools=["view"], memory=CopilotSDK.MemoryConfiguration(enabled=true))
            p = CopilotSDK._session_wire_params(c,
                CopilotSDK._session_flat_params(c, cfg; resume=false), false)
            @test p["tools"]["availableTools"] == ["view"]
            @test p["state"]["memory"] == Dict{String, Any}("enabled" => true)
            @test p["state"]["enableCrossSessionStore"] == false
            @test p["github"]["telemetryEnabled"] == false
        end

        @testset "server requests" begin
            rpc = CopilotSDK.JsonRpcClient(nothing, IOBuffer(), IOBuffer())
            c = CopilotClient(network=NetworkConfig(
                authenticate_basic = req -> (username="u", password="p", other="x")))
            cfg = SessionConfig(on_shell_authentication_request = req -> (action="accept", value="pw"))
            c.sessions["s1"] = CopilotSession("s1", rpc, cfg)
            request = Dict{String, Any}("sessionId" => "s1", "requestId" => "r1", "program" => "sudo",
                "prompt" => "Password:", "inputKind" => "secret", "command" => "sudo ls", "cwd" => "/")
            @test CopilotSDK._handle_shell_authentication(c, request) ==
                Dict{String, Any}("action" => "accept", "value" => "pw")
            request["inputKind"] = "bogus"
            @test CopilotSDK._handle_shell_authentication(c, request) ==
                Dict{String, Any}("action" => "cancel")
            @test CopilotSDK._handle_network_request(c, :authenticate_basic,
                Dict{String, Any}("proxy" => "p")) == Dict{String, Any}("username" => "u", "password" => "p")

            err = JsonRpcError(-32603, "denied", Dict{String, Any}("policyHelperFailure" =>
                Dict{String, Any}("category" => "c", "failureMode" => "f", "message" => "m",
                    "previousPolicyRetained" => true, "reason" => "r")))
            failure = policy_helper_failure(err)
            @test failure.category == "c" && failure.previous_policy_retained
            @test policy_helper_failure(JsonRpcError(-1, "x", nothing)) === nothing
        end

        @testset "permission requests" begin
            req = CopilotSDK._permission_request_from_event(Dict{String, Any}("requestId" => "r1",
                "permissionRequest" => Dict{String, Any}("kind" => "write", "toolCallId" => "tc1",
                    "intention" => "Edit file")))
            @test (req.permission_id, req.kind, req.tool_call_id) == ("r1", "write", "tc1")
            @test req.tool_name == "write" && req.description == "Edit file"
            @test CopilotSDK._permission_decision_wire(approve_all(req)) == Dict{String, Any}("kind" => "approve-once")
            @test CopilotSDK._permission_decision_wire(false) == Dict{String, Any}("kind" => "reject")
            @test CopilotSDK._permission_decision_wire(nothing) == Dict{String, Any}("kind" => "user-not-available")
            @test CopilotSDK._permission_decision_wire("no-result") === nothing
            @test CopilotSDK._permission_decision_wire((kind="reject", feedback="no")) ==
                Dict{String, Any}("kind" => "reject", "feedback" => "no")
            p = CopilotSDK._session_flat_params(CopilotClient(),
                SessionConfig(on_permission_request=approve_all); resume=false)
            @test group_session_params(p)["permissions"] == Dict{String, Any}("request" => true)
        end

        @testset "user input requests" begin
            rpc = CopilotSDK.JsonRpcClient(nothing, IOBuffer(), IOBuffer())
            c = CopilotClient()
            received = Ref{Any}(nothing)
            cfg = SessionConfig(on_user_input_request = req -> begin
                received[] = req
                UserInputResponse(answer = "", was_freeform = true, attachments = [UserInputImageAttachment(
                    data = "aGk=", mime_type = "image/png", display_name = "shot.png")])
            end)
            c.sessions["s1"] = CopilotSession("s1", rpc, cfg)
            params = Dict{String, Any}("sessionId" => "s1", "question" => "Screenshot?",
                "requestId" => "ui-1", "_meta" => Dict{String, Any}("k" => "v"),
                "imageAttachmentConstraints" => Dict{String, Any}("allowedMimeTypes" => Any["image/png"],
                    "maxBytesPerImage" => 1048576, "maxCount" => 2))
            @test CopilotSDK._handle_user_input_request(c, params) == Dict{String, Any}(
                "answer" => "", "wasFreeform" => true, "attachments" => Any[Dict{String, Any}(
                    "type" => "image", "data" => "aGk=", "mimeType" => "image/png", "displayName" => "shot.png")])
            req = received[]
            @test (req.session_id, req.question, req.request_id, req.allow_freeform) ==
                ("s1", "Screenshot?", "ui-1", true)
            @test req.meta == Dict{String, Any}("k" => "v")
            @test req.image_attachment_constraints.allowed_mime_types == ["image/png"]
            @test req.image_attachment_constraints.max_bytes_per_image == 1048576
            @test CopilotSDK._user_input_response_wire(UserInputResponse(answer = "yes")) ==
                Dict{String, Any}("answer" => "yes", "wasFreeform" => false)
            p = CopilotSDK._session_flat_params(c, cfg; resume=false)
            @test group_session_params(p)["interaction"] == Dict{String, Any}("requestUserInput" => true)
        end

        @testset "MCP secrets and extension approvals" begin
            # Run f(client) against an in-memory transport that answers its single request with
            # `result`; returns f's value and the request's method and params.
            function mock_request(f, result)
                input = IOBuffer()
                rpc = CopilotSDK.JsonRpcClient(nothing, input, IOBuffer())
                c = CopilotClient()
                c.rpc = rpc
                c.state = CONNECTED
                @async begin
                    deadline = time() + 10
                    while isempty(rpc.pending) && time() < deadline
                        sleep(0.01)
                    end
                    isempty(rpc.pending) || put!(first(values(rpc.pending)), result)
                end
                value = f(c)
                body = split(String(take!(input)), "\r\n\r\n"; limit=2)[2]
                msg = CopilotSDK.JSON3.read(String(body), Dict{String, Any})
                return value, msg["method"], msg["params"]
            end

            saved, method, params = mock_request(Dict{String, Any}("usedKeychain" => true)) do c
                mcp_secrets_save(c, "my-token", "s3cret"; config_dir="/cfg")
            end
            @test method == "mcp.secrets.save"
            @test params == Dict{String, Any}("secretId" => "my-token", "value" => "s3cret", "configDir" => "/cfg")
            @test saved.used_keychain

            _, method, params = mock_request(nothing) do c
                mcp_secrets_delete_server(c, "my-server")
            end
            @test (method, params) == ("mcp.secrets.deleteServer", Dict{String, Any}("serverName" => "my-server"))

            revoked, method, params = mock_request(Dict{String, Any}("revoked" => true)) do c
                extension_approvals_revoke(c, "plugin:p:e", "key-1"; session_id="s1")
            end
            @test method == "permissions.extensionApprovals.revoke"
            @test params == Dict{String, Any}("extensionName" => "plugin:p:e",
                "installationKey" => "key-1", "sessionId" => "s1")
            @test revoked.revoked

            approval = Dict{String, Any}("extensionName" => "plugin:p:e", "pluginName" => "p",
                "capabilities" => Any["network"], "grantedAt" => "2026-11-24T00:00:00Z",
                "installationKey" => "key-1")
            listed, method, params = mock_request(Dict{String, Any}("approvals" => Any[approval])) do c
                extension_approvals_list(c)
            end
            @test (method, params) == ("permissions.extensionApprovals.list", Dict{String, Any}())
            @test listed.approvals[1].capabilities == ["network"]
            @test listed.approvals[1].installation_label === nothing
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

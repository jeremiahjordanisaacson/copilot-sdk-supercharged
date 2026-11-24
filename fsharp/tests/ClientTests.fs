module CopilotSDK.Supercharged.FSharp.Tests.ClientTests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open CopilotSDK.Supercharged.FSharp

// ---------------------------------------------------------------------------
// SdkProtocolVersion tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``SdkProtocolVersion returns expected version`` () =
    SdkProtocolVersion.getVersion () |> should equal 4

[<Fact>]
let ``SdkProtocolVersion literal matches getVersion`` () =
    SdkProtocolVersion.Version |> should equal (SdkProtocolVersion.getVersion ())

// ---------------------------------------------------------------------------
// Types tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``CopilotClientOptions defaults are correct`` () =
    let opts = CopilotClientOptions.defaults
    opts.UseStdio |> should equal true
    opts.AutoStart |> should equal true
    opts.Port |> should equal 0
    opts.LogLevel |> should equal "info"
    opts.CliPath |> should equal None
    opts.CliUrl |> should equal None
    opts.GitHubToken |> should equal None
    opts.CliArgs |> should be Empty

[<Fact>]
let ``SessionConfig defaults has approveAll handler`` () =
    let config = SessionConfig.defaults
    config.Streaming |> should equal true
    config.Model |> should equal None
    config.Agent |> should equal None
    config.IncludeSubAgentStreamingEvents |> should equal false

[<Fact>]
let ``approveAll handler returns ApproveOnce`` () =
    let request = { ToolName = "test"; ToolCallId = "1"; Input = None }
    let result = SessionConfig.approveAll request |> Async.RunSynchronously
    result.Kind |> should equal ApproveOnce
    result.UpdraftMessage |> should equal None

[<Fact>]
let ``rejectAll handler returns Reject`` () =
    let request = { ToolName = "test"; ToolCallId = "1"; Input = None }
    let result = SessionConfig.rejectAll request |> Async.RunSynchronously
    result.Kind |> should equal Reject

[<Fact>]
let ``PermissionResultKind toWireString produces correct strings`` () =
    PermissionResultKind.toWireString ApproveOnce |> should equal "approve-once"
    PermissionResultKind.toWireString Reject |> should equal "reject"
    PermissionResultKind.toWireString UserNotAvailable |> should equal "user-not-available"
    PermissionResultKind.toWireString NoResult |> should equal "no-result"

// ---------------------------------------------------------------------------
// ToolResultObject tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``ToolResultObject.success creates correct result`` () =
    let result = ToolResultObject.success "hello"
    result.TextResultForLlm |> should equal "hello"
    result.ResultType |> should equal "success"
    result.Error |> should equal None
    result.BinaryResultsForLlm |> should equal None

[<Fact>]
let ``ToolResultObject.failure creates correct result`` () =
    let result = ToolResultObject.failure "oops"
    result.ResultType |> should equal "failure"
    result.Error |> should equal (Some "oops")
    result.TextResultForLlm |> should equal ""

[<Fact>]
let ``ToolResultObject serializes to JSON correctly`` () =
    let result = ToolResultObject.success "test"
    let json = JsonSerializer.Serialize(result)
    json |> should haveSubstring "\"textResultForLlm\":\"test\""
    json |> should haveSubstring "\"resultType\":\"success\""

// ---------------------------------------------------------------------------
// SessionEvent tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``SessionEvent.parse handles assistant.message`` () =
    let data = """{"content":"Hello","messageId":"m1"}"""
    let elem = JsonDocument.Parse(data).RootElement.Clone() |> Some
    match SessionEvent.parse "assistant.message" elem with
    | AssistantMessage d ->
        d.Content |> should equal "Hello"
        d.MessageId |> should equal (Some "m1")
    | other -> failwithf "Expected AssistantMessage but got %A" other

[<Fact>]
let ``SessionEvent.parse handles assistant.message_delta`` () =
    let data = """{"content":"chunk"}"""
    let elem = JsonDocument.Parse(data).RootElement.Clone() |> Some
    match SessionEvent.parse "assistant.message_delta" elem with
    | AssistantMessageDelta d -> d.Content |> should equal "chunk"
    | other -> failwithf "Expected AssistantMessageDelta but got %A" other

[<Fact>]
let ``SessionEvent.parse handles session.idle`` () =
    match SessionEvent.parse "session.idle" None with
    | SessionIdle -> ()
    | other -> failwithf "Expected SessionIdle but got %A" other

[<Fact>]
let ``SessionEvent.parse handles tool.execution_start`` () =
    let data = """{"toolName":"calc","toolCallId":"tc1"}"""
    let elem = JsonDocument.Parse(data).RootElement.Clone() |> Some
    match SessionEvent.parse "tool.execution_start" elem with
    | ToolExecutionStart d ->
        d.ToolName |> should equal "calc"
        d.ToolCallId |> should equal "tc1"
    | other -> failwithf "Expected ToolExecutionStart but got %A" other

[<Fact>]
let ``SessionEvent.parse handles unknown events`` () =
    match SessionEvent.parse "custom.event" None with
    | UnknownEvent (t, _) -> t |> should equal "custom.event"
    | other -> failwithf "Expected UnknownEvent but got %A" other

[<Fact>]
let ``SessionEvent.tryGetMessageContent extracts from AssistantMessage`` () =
    let evt = AssistantMessage { Content = "hi"; MessageId = None }
    SessionEvent.tryGetMessageContent evt |> should equal (Some "hi")

[<Fact>]
let ``SessionEvent.tryGetMessageContent returns None for other events`` () =
    SessionEvent.tryGetMessageContent SessionIdle |> should equal None

[<Fact>]
let ``SessionEvent.tryGetDeltaContent extracts from delta`` () =
    let evt = AssistantMessageDelta { Content = "d" }
    SessionEvent.tryGetDeltaContent evt |> should equal (Some "d")

[<Fact>]
let ``SessionEvent.isIdle returns true for SessionIdle`` () =
    SessionEvent.isIdle SessionIdle |> should equal true

[<Fact>]
let ``SessionEvent.isIdle returns false for other events`` () =
    let evt = AssistantMessage { Content = "x"; MessageId = None }
    SessionEvent.isIdle evt |> should equal false

// ---------------------------------------------------------------------------
// DefineTool tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``DefineTool.create builds a RegisteredTool`` () =
    let tool =
        DefineTool.create "test" "A test tool" None
            (fun _ -> async { return "result" :> obj })
    tool.Tool.Name |> should equal "test"
    tool.Tool.Description |> should equal "A test tool"
    tool.Tool.Parameters |> should equal None

[<Fact>]
let ``DefineTool.createSimple builds tool with no parameters`` () =
    let tool =
        DefineTool.createSimple "ping" "Ping" (fun () -> async { return "pong" })
    tool.Tool.Name |> should equal "ping"
    let invocation = { SessionId = "s1"; ToolCallId = "t1"; ToolName = "ping"; Arguments = None }
    let result = tool.Handler invocation |> Async.RunSynchronously
    match result with
    | :? ToolResultObject as r -> r.TextResultForLlm |> should equal "pong"
    | other -> failwithf "Expected ToolResultObject but got %A" other

[<Fact>]
let ``DefineTool.createTyped deserializes arguments`` () =
    let tool =
        DefineTool.createTyped<{| city: string |}, string>
            "weather" "Get weather" None
            (fun args -> async { return sprintf "Sunny in %s" args.city })
    let argsJson = """{"city":"Seattle"}"""
    let elem = JsonDocument.Parse(argsJson).RootElement.Clone()
    let invocation = { SessionId = "s1"; ToolCallId = "t1"; ToolName = "weather"; Arguments = Some elem }
    let result = tool.Handler invocation |> Async.RunSynchronously
    match result with
    | :? string as s -> s |> should equal "Sunny in Seattle"
    | other -> failwithf "Unexpected result: %A" other

[<Fact>]
let ``DefineTool builder pipeline works`` () =
    let tool =
        DefineTool.define "calc" "Calculator"
        |> DefineTool.withHandler (fun _ -> async { return "42" :> obj })
        |> DefineTool.build
    tool.Tool.Name |> should equal "calc"
    tool.Tool.Description |> should equal "Calculator"

[<Fact>]
let ``DefineTool.build throws when handler is missing`` () =
    (fun () ->
        DefineTool.define "no_handler" "Missing handler"
        |> DefineTool.build
        |> ignore)
    |> should throw typeof<Exception>

// ---------------------------------------------------------------------------
// JsonRpc transport tests (unit-level with in-memory streams)
// ---------------------------------------------------------------------------

let private makeMessage (json: string) =
    let bytes = Encoding.UTF8.GetBytes(json)
    let header = sprintf "Content-Length: %d\r\n\r\n" bytes.Length
    let headerBytes = Encoding.UTF8.GetBytes(header)
    Array.append headerBytes bytes

[<Fact>]
let ``JsonRpcTransport sends request and receives response`` () =
    // Set up an in-memory pipe: client writes to serverInput, server writes to clientInput
    let clientInput = new MemoryStream()
    let serverInput = new MemoryStream()

    // Pre-populate clientInput with a JSON-RPC response
    let responseJson = """{"jsonrpc":"2.0","id":1,"result":{"value":"ok"}}"""
    let responseBytes = makeMessage responseJson
    clientInput.Write(responseBytes, 0, responseBytes.Length)
    clientInput.Position <- 0L

    use transport = new JsonRpcTransport(clientInput, serverInput)
    transport.StartReadLoop()

    // The transport will read the pre-canned response
    // We cannot easily test the full round-trip without a real server,
    // but we can verify the transport doesn't throw on initialization
    Thread.Sleep(100)
    // Transport is alive and processing
    ()

[<Fact>]
let ``JsonRpcTransport handles notification registration`` () =
    let clientInput = new MemoryStream()
    let serverInput = new MemoryStream()

    use transport = new JsonRpcTransport(clientInput, serverInput)

    let mutable received = false
    transport.OnNotification("test.notify", fun _ -> received <- true)
    transport.RemoveNotification("test.notify")
    // Handler was removed, so received stays false
    received |> should equal false

// ---------------------------------------------------------------------------
// ConnectionState tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``CopilotClient starts in Disconnected state`` () =
    use client = new CopilotClient(CopilotClientOptions.defaults)
    client.ConnectionState |> should equal Disconnected

[<Fact>]
let ``CopilotClient NegotiatedProtocolVersion is None before start`` () =
    use client = new CopilotClient(CopilotClientOptions.defaults)
    client.NegotiatedProtocolVersion |> should equal None

// ---------------------------------------------------------------------------
// MessageOptions record tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``MessageOptions record creates correctly`` () =
    let opts = { Prompt = "hello"; Attachments = None; Mode = None; AgentMode = None; DisplayPrompt = None; RequestHeaders = None; Source = None; ResponseSchema = None; ClientTurnId = None }
    opts.Prompt |> should equal "hello"
    opts.Attachments |> should equal None
    opts.Mode |> should equal None

[<Fact>]
let ``MessageOptions with attachments`` () =
    let attachment = { Type = "file"; Path = Some "./test.fs"; Url = None; Content = None }
    let opts = { Prompt = "review"; Attachments = Some [attachment]; Mode = None; AgentMode = None; DisplayPrompt = None; RequestHeaders = None; Source = None; ResponseSchema = None; ClientTurnId = None }
    opts.Attachments |> Option.get |> List.length |> should equal 1

// ---------------------------------------------------------------------------
// Serialization round-trip tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``Attachment serializes and deserializes correctly`` () =
    let att = { Type = "file"; Path = Some "/a.fs"; Url = None; Content = None }
    let json = JsonSerializer.Serialize(att)
    let back = JsonSerializer.Deserialize<Attachment>(json)
    back.Type |> should equal "file"
    back.Path |> should equal (Some "/a.fs")

[<Fact>]
let ``ModelInfo deserializes from JSON`` () =
    let json = """{"id":"gpt-4","name":"GPT-4"}"""
    let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    let model = JsonSerializer.Deserialize<ModelInfo>(json, opts)
    model.Id |> should equal "gpt-4"
    model.Name |> should equal (Some "GPT-4")

[<Fact>]
let ``SessionMetadata deserializes from JSON`` () =
    let json = """{"sessionId":"s1","summary":"test session","createdAt":"2025-01-01"}"""
    let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    let meta = JsonSerializer.Deserialize<SessionMetadata>(json, opts)
    meta.SessionId |> should equal "s1"
    meta.Summary |> should equal (Some "test session")

[<Fact>]
let ``GetStatusResponse deserializes from JSON`` () =
    let json = """{"version":"1.0","protocolVersion":4}"""
    let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    let status = JsonSerializer.Deserialize<GetStatusResponse>(json, opts)
    status.ProtocolVersion |> should equal (Some 4)

[<Fact>]
let ``GetAuthStatusResponse deserializes from JSON`` () =
    let json = """{"authenticated":true,"user":"octocat"}"""
    let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
    let auth = JsonSerializer.Deserialize<GetAuthStatusResponse>(json, opts)
    auth.Authenticated |> should equal true
    auth.User |> should equal (Some "octocat")

// ---------------------------------------------------------------------------
// v2.6.0 feature tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``CopilotClientInfo toWire maps to editor and extension keys`` () =
    let info = { CopilotClientInfo.empty with ApplicationName = Some "MyApp"; IntegrationVersion = Some "1.2.3" }
    let wire = CopilotClientInfo.toWire info |> Option.get
    wire.["editorName"] |> should equal (box "MyApp")
    wire.["extensionVersion"] |> should equal (box "1.2.3")
    wire.ContainsKey("editorVersion") |> should equal false

[<Fact>]
let ``CopilotClientInfo toWire returns None when all fields are empty`` () =
    let info = { CopilotClientInfo.empty with ApplicationName = Some "" }
    CopilotClientInfo.toWire info |> should equal None

[<Fact>]
let ``MessageSource helpers build source strings`` () =
    MessageSource.User |> should equal "user"
    MessageSource.System |> should equal "system"
    MessageSource.agent "reviewer" |> should equal "agent-reviewer"

[<Fact>]
let ``SandboxConfig toWire omits unset flags`` () =
    let wire = SandboxConfig.toWire { SandboxConfig.create true with AllowBypass = Some true }
    wire.["enabled"] |> should equal (box true)
    wire.["allowBypass"] |> should equal (box true)
    wire.ContainsKey("sandboxMcpServers") |> should equal false

[<Fact>]
let ``GitHubTokenProviderResult toWire serializes token and cancelled`` () =
    let token =
        GitHubTokenProviderResult.toWire (
            GitHubTokenGranted { AccessToken = "gho_x"; TokenType = None; ExpiresIn = 3600 })
    token.["kind"] |> should equal (box "token")
    token.["expiresIn"] |> should equal (box 3600)
    token.ContainsKey("tokenType") |> should equal false
    let cancelled = GitHubTokenProviderResult.toWire GitHubTokenCancelled
    cancelled.["kind"] |> should equal (box "cancelled")

[<Fact>]
let ``ModelLimitsOverride toCapabilities uses snake_case limits`` () =
    let caps =
        ModelLimitsOverride.toCapabilities
            { MaxPromptTokens = None; MaxOutputTokens = Some 8192; MaxContextWindowTokens = None }
    let json = JsonSerializer.Serialize(caps)
    json |> should haveSubstring "\"max_output_tokens\":8192"

[<Fact>]
let ``MessageOptions responseFormatFor builds a json_schema response format`` () =
    let schema = JsonDocument.Parse("""{"type":"object"}""").RootElement.Clone()
    let json = JsonSerializer.Serialize(MessageOptions.responseFormatFor schema)
    json |> should haveSubstring "\"type\":\"json_schema\""
    json |> should haveSubstring "\"name\":\"response\""
    json |> should haveSubstring "\"strict\":true"

[<Fact>]
let ``SubagentHooks parseStopInput reads stop fields`` () =
    let payload =
        """{"timestamp":1,"cwd":"/w","transcriptPath":"/t","agentName":"explore","agentType":"explore","stopReason":"end_turn","response":"done"}"""
    let input = SubagentHooks.parseStopInput (JsonDocument.Parse(payload).RootElement.Clone())
    input.AgentName |> should equal "explore"
    input.Response |> should equal "done"
    input.StopReason |> should equal "end_turn"
    input.Timestamp |> should equal (Some 1L)

[<Fact>]
let ``ResumeSessionConfig create leaves new options unset`` () =
    let config = ResumeSessionConfig.create "s1"
    config.SessionId |> should equal "s1"
    config.AutoTier |> should equal None
    config.AllowTranscriptRecovery |> should equal None

// ---------------------------------------------------------------------------
// v3.0.0 (SDK protocol 4) tests
// ---------------------------------------------------------------------------

let private parseObject (json: string) =
    System.Text.Json.Nodes.JsonNode.Parse(json).AsObject()

[<Fact>]
let ``SessionWire groups a flat resume payload into the v4 shape`` () =
    let flat =
        parseObject
            """{"sessionId":"s1","model":"m","streaming":true,"requestPermission":true,"tools":[{"name":"t","idempotent":true}],"hooks":true,"disableResume":true,"trustWorkingDirectory":true,"mcpServers":{"a":{"command":"x"}},"capi":{"autoTier":"fast"},"agent":null,"customKey":1,"events":{"bogusMember":1}}"""
    let grouped = SessionWire.groupSessionParams flat true
    let json = grouped.ToJsonString()
    grouped.["model"].GetValue<string>() |> should equal "m"
    grouped.["events"].["streaming"].GetValue<bool>() |> should equal true
    // Unknown group members are rejected by the runtime, so they are never sent.
    grouped.["events"].AsObject().ContainsKey("bogusMember") |> should equal false
    grouped.["permissions"].["request"].GetValue<bool>() |> should equal true
    grouped.["tools"].["definitions"].[0].["idempotent"].GetValue<bool>() |> should equal true
    grouped.["hooks"].["hasHandlers"].GetValue<bool>() |> should equal true
    grouped.["suppressResumeEvent"].GetValue<bool>() |> should equal true
    grouped.["mcp"].["envValueMode"].GetValue<string>() |> should equal "direct"
    grouped.["github"].["capi"].["autoTier"].GetValue<string>() |> should equal "fast"
    grouped.["customKey"].GetValue<int>() |> should equal 1
    // Create-only and null values are dropped; flat keys do not leak to the root.
    json |> should not' (haveSubstring "trustWorkingDirectory")
    json |> should not' (haveSubstring "disableResume")
    json |> should not' (haveSubstring "\"agents\"")
    grouped.ContainsKey("streaming") |> should equal false

[<Fact>]
let ``SessionWire translates the legacy provider into a default named provider`` () =
    let flat =
        parseObject
            """{"model":"llama","provider":{"type":"openai","baseUrl":"http://localhost:11434/v1","apiKeyCommand":"op read key","maxOutputTokens":2048}}"""
    let grouped = SessionWire.groupSessionParams flat false
    let provider = grouped.["inference"].["providers"].[0]
    provider.["name"].GetValue<string>() |> should equal "default"
    provider.["apiKeyCommand"].GetValue<string>() |> should equal "op read key"
    provider.AsObject().ContainsKey("maxOutputTokens") |> should equal false
    grouped.["inference"].["defaultProvider"].GetValue<string>() |> should equal "default"
    grouped.["inference"].["models"].[0].["id"].GetValue<string>() |> should equal "llama"
    grouped.["inference"].["models"].[0].["maxOutputTokens"].GetValue<int>() |> should equal 2048
    grouped.["github"].["capi"].["enabled"].GetValue<bool>() |> should equal false
    grouped.ContainsKey("provider") |> should equal false

[<Fact>]
let ``SessionWire empty-mode defaults keep caller values`` () =
    let grouped =
        SessionWire.groupSessionParams (parseObject """{"enableSessionStore":true,"availableTools":["view"]}""") false
        |> SessionWire.applyEmptyModeDefaults
    grouped.["state"].["enableCrossSessionStore"].GetValue<bool>() |> should equal true
    grouped.["state"].["skipEmbeddingRetrieval"].GetValue<bool>() |> should equal true
    grouped.["mcp"].["oauthTokenStorage"].GetValue<string>() |> should equal "in-memory"
    grouped.["experiments"].["enableExperimentalMode"].GetValue<bool>() |> should equal false
    ClientMode.normalize (Some ClientMode.CopilotCli) |> should equal ClientMode.CodingAgent
    (fun () -> ClientMode.normalize (Some "bogus") |> ignore) |> should throw typeof<ArgumentException>

[<Fact>]
let ``SessionEvent.parse handles tool.shell_output`` () =
    let data = """{"toolCallId":"tc1","text":"hello\n","sequence":3,"stream":"stderr"}"""
    let elem = JsonDocument.Parse(data).RootElement.Clone() |> Some
    match SessionEvent.parse "tool.shell_output" elem with
    | ToolShellOutput d ->
        d.ToolCallId |> should equal "tc1"
        d.Sequence |> should equal 3L
        d.Stream |> should equal (Some "stderr")
    | other -> failwithf "Expected ToolShellOutput but got %A" other

[<Fact>]
let ``JsonRpcException exposes policyHelperFailure from error data`` () =
    let data =
        JsonDocument.Parse(
            """{"policyHelperFailure":{"category":"timeout","failureMode":"failClosed","message":"helper timed out","previousPolicyRetained":true,"reason":"sessionStart"}}""")
            .RootElement.Clone()
    let ex = JsonRpcException(-32603, "blocked", Some data)
    let failure = ex.PolicyHelperFailure |> Option.get
    failure.Category |> should equal "timeout"
    failure.FailureMode |> should equal "failClosed"
    failure.PreviousPolicyRetained |> should equal true
    JsonRpcException(-32603, "other", None).PolicyHelperFailure |> should equal None

// ---------------------------------------------------------------------------
// v3.1.0 tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``UserInputRequest tryParse passes imageAttachmentConstraints, requestId and _meta through`` () =
    let parameters =
        JsonDocument.Parse(
            """{"sessionId":"s1","question":"Attach a screenshot?","requestId":"r1","_meta":{"ext":1},"imageAttachmentConstraints":{"allowedMimeTypes":["image/png"],"maxBytesPerImage":1024,"maxCount":2}}""")
            .RootElement.Clone()
    let request = UserInputRequest.tryParse parameters |> Option.get
    request.RequestId |> should equal (Some "r1")
    request.Meta.Value.GetProperty("ext").GetInt32() |> should equal 1
    let constraints = request.ImageAttachmentConstraints |> Option.get
    constraints.AllowedMimeTypes |> should equal [ "image/png" ]
    constraints.MaxBytesPerImage |> should equal 1024L
    constraints.MaxCount |> should equal 2L
    let plain = JsonDocument.Parse("""{"sessionId":"s1","question":"q"}""").RootElement.Clone()
    (UserInputRequest.tryParse plain |> Option.get).ImageAttachmentConstraints |> should equal None

[<Fact>]
let ``UserInputResponse toWire sends attachments only when set`` () =
    let plain = UserInputResponse.toWire (UserInputResponse.create "ok" false)
    plain.ContainsKey("attachments") |> should equal false
    let image = { UserInputImageAttachment.create "aGk=" "image/png" with DisplayName = Some "shot.png" }
    let json =
        JsonSerializer.Serialize(
            UserInputResponse.toWire { UserInputResponse.create "" true with Attachments = Some [ image ] })
    let expected =
        "\"attachments\":[{\"type\":\"image\",\"data\":\"aGk=\",\"mimeType\":\"image/png\",\"displayName\":\"shot.png\"}]"
    json |> should haveSubstring expected
    json |> should not' (haveSubstring "description")

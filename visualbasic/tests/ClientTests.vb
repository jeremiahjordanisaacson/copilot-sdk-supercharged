' Copyright (c) Microsoft Corporation. All rights reserved.

Imports Xunit
Imports GitHub.Copilot.SDK

Public Class ClientTests

    <Fact>
    Public Sub Constructor_WithDefaultOptions_SetsDefaults()
        Dim client As New CopilotClient()
        Assert.NotNull(client)
        Assert.False(client.IsConnected)
    End Sub

    <Fact>
    Public Sub Constructor_WithCustomOptions_AppliesOptions()
        Dim opts As New CopilotClientOptions With {
            .LogLevel = "debug",
            .UseStdio = True,
            .AutoStart = False,
            .Port = 9090
        }

        Dim client As New CopilotClient(opts)
        Assert.NotNull(client)
    End Sub

    <Fact>
    Public Sub Constructor_WithCliUrlAndCliPath_Throws()
        Dim opts As New CopilotClientOptions With {
            .CliUrl = "localhost:3000",
            .CliPath = "/usr/bin/copilot"
        }

        Assert.Throws(Of ArgumentException)(Sub()
                                                Dim c = New CopilotClient(opts)
                                            End Sub)
    End Sub

    <Fact>
    Public Sub Constructor_WithCliUrlAndGitHubToken_Throws()
        Dim opts As New CopilotClientOptions With {
            .CliUrl = "localhost:3000",
            .GitHubToken = "ghp_abc123"
        }

        Assert.Throws(Of ArgumentException)(Sub()
                                                Dim c = New CopilotClient(opts)
                                            End Sub)
    End Sub

    <Fact>
    Public Sub Constructor_WithCliUrl_DisablesStdio()
        Dim opts As New CopilotClientOptions With {
            .CliUrl = "localhost:3000",
            .UseStdio = True
        }

        ' Should not throw; CliUrl forces UseStdio = False internally
        Dim client As New CopilotClient(opts)
        Assert.NotNull(client)
    End Sub

    <Fact>
    Public Sub Dispose_CalledTwice_DoesNotThrow()
        Dim client As New CopilotClient()
        client.Dispose()
        client.Dispose() ' second call is a no-op
    End Sub

    <Fact>
    Public Sub SdkProtocolVersion_ReturnsExpectedVersion()
        Assert.Equal(4, SdkProtocolVersion.Version)
        Assert.Equal(4, SdkProtocolVersion.GetVersion())
    End Sub

End Class

Public Class ProtocolV4Tests

    <Fact>
    Public Sub GroupSessionParams_GroupsFlatResumePayload()
        Dim tool As New DefineTool With {.Name = "lookup", .Description = "Look up", .Idempotent = True}
        Dim flat As New Dictionary(Of String, Object) From {
            {"sessionId", "s1"},
            {"model", "m"},
            {"streaming", True},
            {"requestPermission", True},
            {"tools", New List(Of Dictionary(Of String, Object)) From {tool.ToWire()}},
            {"hooks", True},
            {"disableResume", True},
            {"trustWorkingDirectory", "session"},
            {"mcpServers", New Dictionary(Of String, Object) From {{"a", New Dictionary(Of String, Object)}}},
            {"capi", New Dictionary(Of String, Object) From {{"autoTier", "fast"}}},
            {"agent", Nothing},
            {"customKey", 1},
            {"events", New Dictionary(Of String, Object) From {{"bogusMember", 1}}}
        }

        Dim grouped = SessionWire.GroupSessionParams(flat, isResume:=True)

        Assert.Equal(Of Object)("m", grouped("model"))
        Assert.Equal(Of Object)(True, DirectCast(grouped("events"), Dictionary(Of String, Object))("streaming"))
        ' Unknown group members are rejected by the runtime, so they are never sent.
        Assert.False(DirectCast(grouped("events"), Dictionary(Of String, Object)).ContainsKey("bogusMember"))
        Assert.Equal(Of Object)(True, DirectCast(grouped("permissions"), Dictionary(Of String, Object))("request"))
        Dim definitions = DirectCast(DirectCast(grouped("tools"), Dictionary(Of String, Object))("definitions"), List(Of Dictionary(Of String, Object)))
        Assert.Equal(Of Object)(True, definitions(0)("idempotent"))
        Assert.Equal(Of Object)(True, DirectCast(grouped("hooks"), Dictionary(Of String, Object))("hasHandlers"))
        Assert.Equal(Of Object)(True, grouped("suppressResumeEvent"))
        Assert.Equal(Of Object)("direct", DirectCast(grouped("mcp"), Dictionary(Of String, Object))("envValueMode"))
        Dim github = DirectCast(grouped("github"), Dictionary(Of String, Object))
        Assert.Equal(Of Object)("fast", DirectCast(github("capi"), Dictionary(Of String, Object))("autoTier"))
        Assert.Equal(Of Object)(1, grouped("customKey"))
        Assert.False(grouped.ContainsKey("filesystem"))
        Assert.False(grouped.ContainsKey("agents"))
        Assert.False(grouped.ContainsKey("streaming"))
        Assert.False(grouped.ContainsKey("disableResume"))
    End Sub

    <Fact>
    Public Sub GroupSessionParams_TranslatesLegacyProvider()
        Dim provider As New ProviderConfig With {
            .Type = "openai",
            .BaseUrl = "http://localhost:11434/v1",
            .ApiKeyCommand = "op read key"
        }
        Dim wireProvider = provider.ToWire()
        wireProvider("maxOutputTokens") = 2048
        Dim flat As New Dictionary(Of String, Object) From {{"model", "llama"}, {"provider", wireProvider}}

        Dim grouped = SessionWire.GroupSessionParams(flat, isResume:=False)

        Dim inference = DirectCast(grouped("inference"), Dictionary(Of String, Object))
        Dim connection = DirectCast(DirectCast(inference("providers"), List(Of Object))(0), Dictionary(Of String, Object))
        Assert.Equal(Of Object)("default", connection("name"))
        Assert.Equal(Of Object)("op read key", connection("apiKeyCommand"))
        Assert.False(connection.ContainsKey("maxOutputTokens"))
        Assert.Equal(Of Object)("default", inference("defaultProvider"))
        Dim model = DirectCast(DirectCast(inference("models"), List(Of Object))(0), Dictionary(Of String, Object))
        Assert.Equal(Of Object)("llama", model("id"))
        Assert.Equal(Of Object)(2048, model("maxOutputTokens"))
        Dim capi = DirectCast(DirectCast(grouped("github"), Dictionary(Of String, Object))("capi"), IDictionary(Of String, Object))
        Assert.Equal(Of Object)(False, capi("enabled"))
        Assert.False(grouped.ContainsKey("provider"))
    End Sub

    <Fact>
    Public Sub ApplyEmptyModeDefaults_KeepsCallerValues()
        Dim flat As New Dictionary(Of String, Object) From {
            {"enableSessionStore", True},
            {"availableTools", New List(Of String) From {"view"}}
        }

        Dim grouped = SessionWire.ApplyEmptyModeDefaults(SessionWire.GroupSessionParams(flat, isResume:=False))

        Dim state = DirectCast(grouped("state"), IDictionary(Of String, Object))
        Assert.Equal(Of Object)(True, state("enableCrossSessionStore"))
        Assert.Equal(Of Object)(True, state("skipEmbeddingRetrieval"))
        Assert.Equal(Of Object)("in-memory", DirectCast(grouped("mcp"), IDictionary(Of String, Object))("oauthTokenStorage"))
        Assert.Equal(Of Object)(False, DirectCast(grouped("experiments"), IDictionary(Of String, Object))("enableExperimentalMode"))
        Assert.Equal(ClientMode.CodingAgent, ClientMode.Normalize(ClientMode.CopilotCli))
        Assert.Throws(Of ArgumentException)(Sub() ClientMode.Normalize("bogus"))
    End Sub

    <Fact>
    Public Sub JsonRpcException_ExposesPolicyHelperFailure()
        Dim data = System.Text.Json.JsonDocument.Parse(
            "{""policyHelperFailure"":{""category"":""timeout"",""failureMode"":""failClosed"",""message"":""helper timed out"",""previousPolicyRetained"":true,""reason"":""sessionStart""}}").RootElement.Clone()
        Dim ex As New JsonRpcException("blocked") With {.Code = -32603, .ErrorData = data}

        Assert.Equal("timeout", ex.PolicyHelperFailure.Category)
        Assert.Equal("failClosed", ex.PolicyHelperFailure.FailureMode)
        Assert.True(ex.PolicyHelperFailure.PreviousPolicyRetained)
        Assert.Null(New JsonRpcException("other").PolicyHelperFailure)
    End Sub

    <Fact>
    Public Sub UserInput_PassesImageConstraintsThroughAndSendsAttachmentsOnlyWhenSet()
        Dim request = System.Text.Json.JsonSerializer.Deserialize(Of UserInputRequest)(
            "{""sessionId"":""s1"",""question"":""Screenshot?"",""requestId"":""r1"",""_meta"":{""k"":1}," &
            """imageAttachmentConstraints"":{""allowedMimeTypes"":[""image/png""],""maxBytesPerImage"":1024,""maxCount"":2}}")

        Assert.Equal("r1", request.RequestId)
        Assert.True(request.Meta.ContainsKey("k"))
        Assert.Equal("image/png", request.ImageAttachmentConstraints.AllowedMimeTypes(0))
        Assert.Equal(1024L, request.ImageAttachmentConstraints.MaxBytesPerImage)
        Assert.Equal(2L, request.ImageAttachmentConstraints.MaxCount)

        Dim plain = New UserInputResponse With {.Answer = "no", .WasFreeform = True}.ToWire()
        Assert.False(plain.ContainsKey("attachments"))

        Dim withImage = New UserInputResponse With {
            .WasFreeform = True,
            .Attachments = New List(Of UserInputImageAttachment) From {
                New UserInputImageAttachment With {.Data = "iVBORw0=", .MimeType = "image/png"}}
        }.ToWire()
        Assert.Equal(Of Object)("", withImage("answer"))
        Dim attachment = DirectCast(withImage("attachments"), List(Of Dictionary(Of String, Object)))(0)
        Assert.Equal(Of Object)("image", attachment("type"))
        Assert.Equal(Of Object)("iVBORw0=", attachment("data"))
        Assert.Equal(Of Object)("image/png", attachment("mimeType"))
        Assert.False(attachment.ContainsKey("displayName"))
    End Sub

End Class

Public Class TypesTests

    <Fact>
    Public Sub CopilotClientOptions_DefaultValues()
        Dim opts As New CopilotClientOptions()

        Assert.True(opts.UseStdio)
        Assert.True(opts.AutoStart)
        Assert.Equal("info", opts.LogLevel)
        Assert.Equal(0, opts.Port)
        Assert.Null(opts.CliPath)
        Assert.Null(opts.CliUrl)
        Assert.Null(opts.GitHubToken)
        Assert.False(opts.Streaming)
    End Sub

    <Fact>
    Public Sub SessionConfig_DefaultValues()
        Dim cfg As New SessionConfig()

        Assert.Null(cfg.Model)
        Assert.Null(cfg.SystemMessage)
        Assert.Null(cfg.OnPermissionRequest)
        Assert.Null(cfg.Tools)
        Assert.Null(cfg.Skills)
        Assert.False(cfg.CustomizeMode)
        Assert.False(cfg.AgentMode)
    End Sub

    <Fact>
    Public Sub MessageOptions_CanSetPrompt()
        Dim opts As New MessageOptions With {.Prompt = "Hello world"}
        Assert.Equal("Hello world", opts.Prompt)
    End Sub

    <Fact>
    Public Sub PermissionHandlers_ApproveAll_ReturnsAllowed()
        Dim handler = PermissionHandlers.ApproveAll()
        Dim response = handler(New PermissionRequest()).Result
        Assert.True(response.Allowed)
    End Sub

    <Fact>
    Public Sub PermissionHandlers_DenyAll_ReturnsDenied()
        Dim handler = PermissionHandlers.DenyAll()
        Dim response = handler(New PermissionRequest()).Result
        Assert.False(response.Allowed)
        Assert.Equal("denied by policy", response.Reason)
    End Sub

    <Fact>
    Public Sub Attachment_Properties()
        Dim att As New Attachment With {
            .Type = "file",
            .Path = "./test.vb",
            .Language = "vb"
        }

        Assert.Equal("file", att.Type)
        Assert.Equal("./test.vb", att.Path)
        Assert.Equal("vb", att.Language)
    End Sub

    <Fact>
    Public Sub SessionCapabilities_DefaultValues()
        Dim caps As New SessionCapabilities()
        Assert.False(caps.Streaming)
        Assert.False(caps.Tools)
        Assert.False(caps.Skills)
    End Sub

End Class

Public Class DefineToolTests

    <Fact>
    Public Sub DefineTool_DefaultConstructor()
        Dim tool As New DefineTool()
        Assert.Null(tool.Name)
        Assert.Null(tool.Description)
        Assert.Null(tool.ParametersSchema)
        Assert.Null(tool.Handler)
    End Sub

    <Fact>
    Public Sub DefineTool_ParameterizedConstructor()
        Dim tool As New DefineTool(
            "test_tool",
            "A test tool",
            New Dictionary(Of String, Object) From {{"type", "object"}},
            Async Function(input) "result"
        )

        Assert.Equal("test_tool", tool.Name)
        Assert.Equal("A test tool", tool.Description)
        Assert.NotNull(tool.Handler)
    End Sub

    <Fact>
    Public Async Function DefineTool_HandlerReturnsExpectedResult() As Task
        Dim tool As New DefineTool With {
            .Name = "echo",
            .Description = "Echo tool",
            .Handler = Async Function(input) "echoed"
        }

        Dim result = Await tool.Handler("hello")
        Assert.Equal("echoed", result)
    End Function

End Class

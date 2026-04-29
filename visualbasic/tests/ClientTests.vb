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
        Assert.Equal("2", SdkProtocolVersion.Version)
        Assert.Equal(2, SdkProtocolVersion.GetVersion())
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

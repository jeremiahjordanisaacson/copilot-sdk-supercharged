/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.Diagnostics;
using System.Globalization;
using System.Net;
using System.Net.Sockets;
using System.Text.Json;
using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

public class ClientOptionsE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : E2ETestBase(fixture, "client_options", output)
{
    [Fact]
    public async Task Should_Fail_When_Configured_Tcp_Port_Is_In_Use()
    {
        var listener = new TcpListener(IPAddress.Loopback, 0) { ExclusiveAddressUse = true };
        listener.Start();
        try
        {
            var port = ((IPEndPoint)listener.LocalEndpoint).Port;
            await using var client = Ctx.CreateClient(
                options: new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForSpawnedTcp(port: port) });

            var ex = await Assert.ThrowsAsync<IOException>(() => client.StartAsync());
            Assert.Contains($"failed to bind TCP server at 127.0.0.1:{port}:", ex.Message, StringComparison.Ordinal);
        }
        finally
        {
            listener.Stop();
        }
    }

    [Fact]
    public async Task Should_Use_Client_Cwd_For_Default_WorkingDirectory()
    {
        var clientCwd = Path.Join(Ctx.WorkDir, "client-cwd");
        Directory.CreateDirectory(clientCwd);
        await File.WriteAllTextAsync(Path.Join(clientCwd, "marker.txt"), "I am in the client cwd");

        await using var client = Ctx.CreateClient(workingDirectory: clientCwd);

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        var message = await session.SendAndWaitAsync(new MessageOptions
        {
            Prompt = "Read the file marker.txt and tell me what it says",
        });

        Assert.Contains("client cwd", message?.Data.Content ?? string.Empty);

        await session.DisposeAsync();
    }

    [Theory]
    [InlineData(true, "SPAN_ONLY")]
    [InlineData(false, "NO_CONTENT")]
    public async Task Should_Propagate_Process_Options_To_Spawned_Cli(bool captureContent, string captureMode)
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();
        var telemetryPath = Path.Join(Ctx.WorkDir, "telemetry.jsonl");
        var copilotHomeFromEnv = Path.Join(Ctx.WorkDir, "copilot-home-from-env");
        var copilotHomeFromOption = Path.Join(Ctx.WorkDir, "copilot-home-from-option");
        var clientEnv = Ctx.GetEnvironment().ToDictionary(pair => pair.Key, pair => pair.Value);
        clientEnv["COPILOT_HOME"] = copilotHomeFromEnv;
        clientEnv["COPILOT_RUNTIME_PROCESS_FILE_LOGGING"] = "0";
        clientEnv["OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"] = "SPAN_AND_EVENT";
        await File.WriteAllTextAsync(cliPath, FakeStdioCliScript);

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath], baseDirectory: copilotHomeFromOption, sessionIdleTimeoutSeconds: 17),
            GitHub = new ClientGitHubOptions { Token = "process-option-token", UseLoggedInUser = false },
            Observability = new ClientObservabilityOptions
            {
                LogLevel = CopilotLogLevel.Debug,
                Telemetry = new TelemetryConfig
                {
                    OtlpEndpoint = "http://127.0.0.1:4318",
                    OtlpProtocol = "http/protobuf",
                    FilePath = telemetryPath,
                    ExporterType = "file",
                    SourceName = "dotnet-sdk-e2e",
                    CaptureContent = captureContent,
                }
            }
        }, environment: clientEnv);

        await client.StartAsync();

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var root = capture.RootElement;
        var args = root.GetProperty("args").EnumerateArray().Select(e => e.GetString()).ToArray();
        var capturedEnv = root.GetProperty("env");

        AssertArgumentValue(args, "--log-level", "debug");
        Assert.Contains("--stdio", args);
        AssertArgumentValue(args, "--auth-token-env", "COPILOT_SDK_AUTH_TOKEN");
        Assert.Contains("--no-auto-login", args);
        AssertArgumentValue(args, "--session-idle-timeout", "17");
        Assert.Equal(Path.GetFullPath(Ctx.WorkDir), root.GetProperty("cwd").GetString());

        Assert.Equal(copilotHomeFromOption, capturedEnv.GetProperty("COPILOT_HOME").GetString());
        Assert.Equal("process-option-token", capturedEnv.GetProperty("COPILOT_SDK_AUTH_TOKEN").GetString());
        Assert.Equal("1", capturedEnv.GetProperty("COPILOT_RUNTIME_PROCESS_FILE_LOGGING").GetString());
        Assert.Equal("true", capturedEnv.GetProperty("COPILOT_OTEL_ENABLED").GetString());
        Assert.Equal("http://127.0.0.1:4318", capturedEnv.GetProperty("OTEL_EXPORTER_OTLP_ENDPOINT").GetString());
        Assert.Equal("http/protobuf", capturedEnv.GetProperty("OTEL_EXPORTER_OTLP_PROTOCOL").GetString());
        Assert.Equal(telemetryPath, capturedEnv.GetProperty("COPILOT_OTEL_FILE_EXPORTER_PATH").GetString());
        Assert.Equal("file", capturedEnv.GetProperty("COPILOT_OTEL_EXPORTER_TYPE").GetString());
        Assert.Equal("dotnet-sdk-e2e", capturedEnv.GetProperty("COPILOT_OTEL_SOURCE_NAME").GetString());
        Assert.Equal(captureMode, capturedEnv.GetProperty("OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT").GetString());
        Assert.Equal(captureContent ? "true" : "false", capturedEnv.GetProperty("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT").GetString());

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { EnableConfigDiscovery = true, Events = new SessionEventOptions { IncludeSubAgentStreamingEvents = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var updatedCapture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(updatedCapture.RootElement, "session.create");
        Assert.True(createRequest.GetProperty("enableConfigDiscovery").GetBoolean());
        Assert.False(createRequest.GetProperty("events").GetProperty("includeSubAgentStreamingEvents").GetBoolean());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Forward_EnableSessionTelemetry_In_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        // When explicitly set to false, it should appear in the wire request
        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { GitHub = new SessionGitHubOptions { TelemetryEnabled = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        Assert.False(createRequest.GetProperty("github").GetProperty("telemetryEnabled").GetBoolean());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Omit_EnableSessionTelemetry_When_Not_Set()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        // When omitted (null/default), the field should not be present in the wire request
        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        Assert.False(createRequest.GetProperty("github").TryGetProperty("telemetryEnabled", out _));

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Forward_CustomAgentsLocalOnly_In_Create_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await client.CreateSessionAsync(new CreateSessionOptions
        { Agents = new SessionAgentOptions { LocalDiscoveryOnly = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        Assert.False(createRequest.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Forward_CustomAgentsLocalOnly_In_Resume_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var createSession = await client.CreateSessionAsync(new CreateSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });
        var sessionId = createSession.SessionId;
        await createSession.DisposeAsync();

        var resumeSession = await client.ResumeSessionAsync(sessionId, new ResumeSessionOptions
        { Agents = new SessionAgentOptions { LocalDiscoveryOnly = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var resumeRequest = GetCapturedRequestParams(capture.RootElement, "session.resume");
        Assert.False(resumeRequest.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());

        await resumeSession.DisposeAsync();
    }

    [Fact]
    public async Task Should_Forward_Granular_Multitenancy_Fields_In_Create_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { Name = "named at create", State = new SessionStateOptions { SkipEmbeddingRetrieval = false, EmbeddingCacheStorage = EmbeddingCacheStorageMode.Persistent, EnableCrossSessionStore = true }, Instructions = new CreateSessionInstructionOptions { OrganizationText = "Follow org policy.", OnDemandDiscovery = true }, Hooks = new SessionHookOptions { EnableRepositoryHooks = true }, Git = new SessionGitOptions { EnableHostGitOperations = false }, Skills = new SessionSkillOptions { Enabled = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        Assert.Equal("named at create", createRequest.GetProperty("name").GetString());
        Assert.False(createRequest.GetProperty("state").GetProperty("skipEmbeddingRetrieval").GetBoolean());
        Assert.Equal("Follow org policy.", createRequest.GetProperty("instructions").GetProperty("organizationText").GetString());
        Assert.True(createRequest.GetProperty("instructions").GetProperty("onDemandDiscovery").GetBoolean());
        Assert.Equal("persistent", createRequest.GetProperty("state").GetProperty("embeddingCacheStorage").GetString());
        Assert.True(createRequest.GetProperty("hooks").GetProperty("enableRepositoryHooks").GetBoolean());
        Assert.False(createRequest.GetProperty("git").GetProperty("enableHostGitOperations").GetBoolean());
        Assert.True(createRequest.GetProperty("state").GetProperty("enableCrossSessionStore").GetBoolean());
        Assert.False(createRequest.GetProperty("skills").GetProperty("enabled").GetBoolean());

        await session.DisposeAsync();
    }

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.SelfConfiguredBackend)]
    public async Task Should_Forward_Advanced_Session_Options_In_Create_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();
        var outputDirectory = Path.Join(Ctx.WorkDir, "large-output-create");

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { ClientName = "advanced-create-client", Model = "claude-sonnet-5", Cloud = new CloudSessionOptions { Repository = new CloudSessionRepository { Owner = "github", Name = "copilot-sdk", Branch = "main", }, }, Inference = new SessionInferenceOptions { ReasoningEffort = "medium", ReasoningSummary = ReasoningSummary.Detailed, ContextTier = ContextTier.LongContext, EnableCitations = true, Providers = [new NamedProviderConfig { Name = "create-provider", Type = "openai", WireApi = "responses", BaseUrl = "https://create-provider.example.test/v1", ApiKey = "create-provider-key", Headers = new Dictionary<string, string> { ["X-Create-Provider"] = "yes" }, },], Models = [new ProviderModelConfig { Provider = "create-provider", Id = "create-model", Name = "Create Model", ModelId = "claude-sonnet-5", WireModel = "create-wire-model", MaxContextWindowTokens = 12_000, MaxPromptTokens = 10_000, MaxOutputTokens = 2_000, },] }, GitHub = new SessionGitHubOptions { Capi = new CapiSessionOptions { EnableWebSocketResponses = false }, Token = "session-create-token" }, Mcp = new SessionMcpOptions { OauthTokenStorage = McpOAuthTokenStorageMode.Persistent, EnableApps = true }, Agents = new SessionAgentOptions { Definitions = [new CustomAgentConfig { Name = "agent-one", DisplayName = "Agent One", Description = "Handles agent-one tasks.", Prompt = "Be agent one.", Tools = ["view"], Infer = true, Skills = ["create-skill"], Model = "claude-haiku-4.5", },], Default = new DefaultAgentConfig { ExcludedTools = ["edit"] }, Selected = "agent-one" }, Skills = new SessionSkillOptions { Directories = ["skills-create"], DisabledSkills = ["disabled-create-skill"] }, Plugins = new SessionPluginOptions { Directories = ["plugins-create"] }, State = new SessionStateOptions { InfiniteSessions = new InfiniteSessionConfig { Enabled = false, BackgroundCompactionThreshold = 0.5, BufferExhaustionThreshold = 0.9, }, Memory = new MemoryConfiguration { Enabled = true } }, Tools = new SessionToolOptions { LargeOutput = new LargeToolOutputConfig { Enabled = true, MaxSizeBytes = 4096, OutputDirectory = outputDirectory, } }, Canvases = new SessionCanvasOptions { RequestRenderer = true, Definitions = [new CanvasDeclaration { Id = "advanced-create-canvas", DisplayName = "Advanced Create Canvas", Description = "Covers create-time canvas options.", },] }, Extensions = new SessionExtensionOptions { Enabled = true, SdkPath = "custom-extension-sdk", Identity = new ExtensionInfo { Source = "dotnet-sdk-tests", Name = "advanced-create-extension" } }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        Assert.Equal("advanced-create-client", createRequest.GetProperty("clientName").GetString());
        Assert.Equal("claude-sonnet-5", createRequest.GetProperty("model").GetString());
        Assert.Equal("medium", createRequest.GetProperty("inference").GetProperty("reasoningEffort").GetString());
        Assert.Equal("detailed", createRequest.GetProperty("inference").GetProperty("reasoningSummary").GetString());
        Assert.Equal("long_context", createRequest.GetProperty("inference").GetProperty("contextTier").GetString());
        Assert.True(createRequest.GetProperty("inference").GetProperty("enableCitations").GetBoolean());
        Assert.False(createRequest.GetProperty("github").GetProperty("capi").GetProperty("enableWebSocketResponses").GetBoolean());
        Assert.Equal("persistent", createRequest.GetProperty("mcp").GetProperty("oauthTokenStorage").GetString());
        Assert.Equal("agent-one", createRequest.GetProperty("agents").GetProperty("selected").GetString());
        Assert.Equal("edit", createRequest.GetProperty("agents").GetProperty("default").GetProperty("excludedTools")[0].GetString());
        Assert.Equal("agent-one", createRequest.GetProperty("agents").GetProperty("definitions")[0].GetProperty("name").GetString());
        Assert.Equal("plugins-create", createRequest.GetProperty("plugins").GetProperty("directories")[0].GetString());
        Assert.Equal("disabled-create-skill", createRequest.GetProperty("skills").GetProperty("disabledSkills")[0].GetString());
        Assert.False(createRequest.GetProperty("state").GetProperty("infiniteSessions").GetProperty("enabled").GetBoolean());
        Assert.True(createRequest.GetProperty("tools").GetProperty("largeOutput").GetProperty("enabled").GetBoolean());
        Assert.Equal(4096, createRequest.GetProperty("tools").GetProperty("largeOutput").GetProperty("maxSizeBytes").GetInt64());
        Assert.Equal(outputDirectory, createRequest.GetProperty("tools").GetProperty("largeOutput").GetProperty("outputDir").GetString());
        Assert.True(createRequest.GetProperty("state").GetProperty("memory").GetProperty("enabled").GetBoolean());
        Assert.Equal("session-create-token", createRequest.GetProperty("github").GetProperty("token").GetString());
        Assert.False(createRequest.TryGetProperty("remoteSession", out _));
        Assert.Equal("github", createRequest.GetProperty("cloud").GetProperty("repository").GetProperty("owner").GetString());
        Assert.True(createRequest.GetProperty("mcp").GetProperty("enableApps").GetBoolean());
        Assert.True(createRequest.GetProperty("canvases").GetProperty("requestRenderer").GetBoolean());
        Assert.True(createRequest.GetProperty("extensions").GetProperty("enabled").GetBoolean());
        Assert.Equal("custom-extension-sdk", createRequest.GetProperty("extensions").GetProperty("sdkPath").GetString());
        Assert.Equal("advanced-create-extension", createRequest.GetProperty("extensions").GetProperty("identity").GetProperty("name").GetString());
        Assert.Equal("advanced-create-canvas", createRequest.GetProperty("canvases").GetProperty("definitions")[0].GetProperty("id").GetString());
        Assert.Equal("create-provider", createRequest.GetProperty("inference").GetProperty("providers")[0].GetProperty("name").GetString());
        Assert.Equal("responses", createRequest.GetProperty("inference").GetProperty("providers")[0].GetProperty("wireApi").GetString());
        Assert.Equal("create-model", createRequest.GetProperty("inference").GetProperty("models")[0].GetProperty("id").GetString());
        Assert.Equal(12000, createRequest.GetProperty("inference").GetProperty("models")[0].GetProperty("maxContextWindowTokens").GetInt32());

        await session.DisposeAsync();
    }

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.SelfConfiguredBackend)]
    public async Task Should_Forward_Singular_Provider_Options_In_Create_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { GitHub = new SessionGitHubOptions { Capi = new CapiSessionOptions { Enabled = false } }, Model = "claude-sonnet-5", Inference = ByokTestOptions.Inference(new ProviderConfig { Type = "azure", WireApi = "responses", Transport = "http", BaseUrl = "https://azure-provider.example.test/openai", ApiKey = "provider-api-key", BearerToken = "provider-bearer-token", Azure = new AzureOptions { ApiVersion = "2024-02-15-preview" }, Headers = new Dictionary<string, string> { ["X-Provider-Wire"] = "yes" }, ModelId = "claude-sonnet-5", WireModel = "azure-deployment", MaxPromptTokens = 8192, MaxOutputTokens = 1024, }, new SessionInferenceOptions { }), Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var inference = GetCapturedRequestParams(capture.RootElement, "session.create").GetProperty("inference");
        var provider = inference.GetProperty("providers")[0];
        var model = inference.GetProperty("models")[0];
        Assert.Equal("azure", provider.GetProperty("type").GetString());
        Assert.Equal("responses", provider.GetProperty("wireApi").GetString());
        Assert.Equal("http", provider.GetProperty("transport").GetString());
        Assert.Equal("https://azure-provider.example.test/openai", provider.GetProperty("baseUrl").GetString());
        Assert.Equal("provider-api-key", provider.GetProperty("apiKey").GetString());
        Assert.Equal("provider-bearer-token", provider.GetProperty("bearerToken").GetString());
        Assert.Equal("2024-02-15-preview", provider.GetProperty("azure").GetProperty("apiVersion").GetString());
        Assert.Equal("yes", provider.GetProperty("headers").GetProperty("X-Provider-Wire").GetString());
        Assert.Equal("claude-sonnet-5", model.GetProperty("modelId").GetString());
        Assert.Equal("azure-deployment", model.GetProperty("wireModel").GetString());
        Assert.Equal(8192, model.GetProperty("maxPromptTokens").GetInt32());
        Assert.Equal(1024, model.GetProperty("maxOutputTokens").GetInt32());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Apply_Empty_Mode_Defaults_To_CreateSession_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.Empty,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath], baseDirectory: Ctx.WorkDir),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll }, Tools = new SessionToolOptions { AvailableTools = new ToolSet().AddBuiltIn(BuiltInTools.Isolated) } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        Assert.False(createRequest.GetProperty("github").GetProperty("telemetryEnabled").GetBoolean());
        Assert.True(createRequest.GetProperty("state").GetProperty("skipEmbeddingRetrieval").GetBoolean());
        Assert.False(createRequest.GetProperty("instructions").GetProperty("onDemandDiscovery").GetBoolean());
        Assert.Equal("in-memory", createRequest.GetProperty("state").GetProperty("embeddingCacheStorage").GetString());
        Assert.False(createRequest.GetProperty("hooks").GetProperty("enableRepositoryHooks").GetBoolean());
        Assert.False(createRequest.GetProperty("git").GetProperty("enableHostGitOperations").GetBoolean());
        Assert.False(createRequest.GetProperty("state").GetProperty("enableCrossSessionStore").GetBoolean());
        Assert.False(createRequest.GetProperty("skills").GetProperty("enabled").GetBoolean());
        Assert.True(createRequest.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());
        Assert.False(createRequest.GetProperty("instructions").TryGetProperty("organizationText", out _));
        Assert.False(createRequest.TryGetProperty("name", out _));

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Propagate_Activity_TraceContext_To_Session_Create_And_Send()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        using var activity = new Activity("dotnet-sdk-trace-create-send");
        activity.SetIdFormat(ActivityIdFormat.W3C);
        activity.TraceStateString = "vendor=create-send";
        activity.Start();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        var messageId = await session.SendAsync(new MessageOptions
        {
            Prompt = "Trace this message.",
        });

        Assert.Equal("fake-message", messageId);

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var createRequest = GetCapturedRequestParams(capture.RootElement, "session.create");
        var sendRequest = GetCapturedRequestParams(capture.RootElement, "session.send");

        Assert.Equal(activity.Id, createRequest.GetProperty("traceparent").GetString());
        Assert.Equal("vendor=create-send", createRequest.GetProperty("tracestate").GetString());
        Assert.Equal(activity.Id, sendRequest.GetProperty("traceparent").GetString());
        Assert.Equal("vendor=create-send", sendRequest.GetProperty("tracestate").GetString());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task ForceStop_Does_Not_Rethrow_When_Tcp_Cli_Drops_During_Startup()
    {
        var cliPath = Path.Join(Ctx.WorkDir, $"fake-tcp-drop-cli-{Guid.NewGuid():N}.js");
        await File.WriteAllTextAsync(cliPath, FakeTcpDropDuringStartupCliScript);

        await using var client = Ctx.CreateClient(
            options: new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = RuntimeConnection.ForSpawnedTcp(path: cliPath),
                GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
            });

        var ex = await Assert.ThrowsAsync<IOException>(() => client.StartAsync());
        Assert.Contains("Communication error", ex.Message, StringComparison.Ordinal);

        await client.ForceStopAsync();
    }

    [Fact]
    public async Task StartAsync_Cleans_Up_Tcp_Cli_Process_When_Connect_Fails()
    {
        var cliPath = Path.Join(Ctx.WorkDir, $"fake-tcp-unavailable-port-cli-{Guid.NewGuid():N}.js");
        var pidPath = Path.Join(Ctx.WorkDir, $"fake-tcp-unavailable-port-cli-{Guid.NewGuid():N}.pid");
        var unavailablePort = GetAvailableTcpPort();
        await File.WriteAllTextAsync(cliPath, FakeTcpUnavailablePortCliScript);

        using var observer = new RuntimeStartupProcessObserver(pidPath, useStdio: false, Logger);
        await using var client = Ctx.CreateClient(
            options: new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = RuntimeConnection.ForSpawnedTcp(path: cliPath, args: ["--pid-file", pidPath, "--announce-port", unavailablePort.ToString(CultureInfo.InvariantCulture)]),
                GitHub = new ClientGitHubOptions { UseLoggedInUser = false },
                Observability = new ClientObservabilityOptions { Logger = observer }
            });

        await Assert.ThrowsAnyAsync<Exception>(() => client.StartAsync());

        Assert.True(observer.Process.HasExited, "Failed TCP connection retained the runtime process.");

        await client.ForceStopAsync();
    }

    [Fact]
    public async Task Should_Propagate_Activity_TraceContext_To_Session_Resume()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        using var activity = new Activity("dotnet-sdk-trace-resume");
        activity.SetIdFormat(ActivityIdFormat.W3C);
        activity.TraceStateString = "vendor=resume";
        activity.Start();

        var session = await Ctx.ResumeSessionAsync(client, "trace-resume-session", new ResumeSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var resumeRequest = GetCapturedRequestParams(capture.RootElement, "session.resume");

        Assert.Equal(activity.Id, resumeRequest.GetProperty("traceparent").GetString());
        Assert.Equal("vendor=resume", resumeRequest.GetProperty("tracestate").GetString());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Forward_Granular_Multitenancy_Fields_In_Resume_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await Ctx.ResumeSessionAsync(client, "resume-session", new ResumeSessionOptions
        { State = new SessionStateOptions { SkipEmbeddingRetrieval = false, EmbeddingCacheStorage = EmbeddingCacheStorageMode.Persistent, EnableCrossSessionStore = true }, Instructions = new SessionInstructionOptions { OrganizationText = "Resume org policy.", OnDemandDiscovery = true }, Hooks = new SessionHookOptions { EnableRepositoryHooks = true }, Git = new SessionGitOptions { EnableHostGitOperations = false }, Skills = new SessionSkillOptions { Enabled = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var resumeRequest = GetCapturedRequestParams(capture.RootElement, "session.resume");
        Assert.False(resumeRequest.GetProperty("state").GetProperty("skipEmbeddingRetrieval").GetBoolean());
        Assert.Equal("Resume org policy.", resumeRequest.GetProperty("instructions").GetProperty("organizationText").GetString());
        Assert.True(resumeRequest.GetProperty("instructions").GetProperty("onDemandDiscovery").GetBoolean());
        Assert.Equal("persistent", resumeRequest.GetProperty("state").GetProperty("embeddingCacheStorage").GetString());
        Assert.True(resumeRequest.GetProperty("hooks").GetProperty("enableRepositoryHooks").GetBoolean());
        Assert.False(resumeRequest.GetProperty("git").GetProperty("enableHostGitOperations").GetBoolean());
        Assert.True(resumeRequest.GetProperty("state").GetProperty("enableCrossSessionStore").GetBoolean());
        Assert.False(resumeRequest.GetProperty("skills").GetProperty("enabled").GetBoolean());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Forward_Advanced_Session_Options_In_Resume_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();
        var outputDirectory = Path.Join(Ctx.WorkDir, "large-output-resume");
        using var canvasInput = JsonDocument.Parse("{\"start\":41}");

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await client.StartAsync();

        var session = await Ctx.ResumeSessionAsync(client, "advanced-resume-session", new ResumeSessionOptions
        { ClientName = "advanced-resume-client", Model = "claude-haiku-4.5", SuppressResumeEvent = true, ContinuePendingWork = true, Inference = new SessionInferenceOptions { ReasoningEffort = "low", ReasoningSummary = ReasoningSummary.None, ContextTier = ContextTier.Default }, Mcp = new SessionMcpOptions { OauthTokenStorage = McpOAuthTokenStorageMode.Persistent }, Plugins = new SessionPluginOptions { Directories = ["plugins-resume"] }, Tools = new SessionToolOptions { LargeOutput = new LargeToolOutputConfig { Enabled = false, MaxSizeBytes = 2048, OutputDirectory = outputDirectory, } }, State = new SessionStateOptions { Memory = new MemoryConfiguration { Enabled = false } }, Canvases = new ResumeSessionCanvasOptions { Open = [new Microsoft.Copilot.Rpc.OpenCanvasInstance { CanvasId = "resume-canvas", ExtensionId = "dotnet-sdk-tests/resume-extension", ExtensionName = "Resume Extension", InstanceId = "resume-canvas-1", Input = canvasInput.RootElement.Clone(), Status = "ready", Title = "Resume Canvas", Url = "https://example.com/resume-canvas", },] }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        var resumeRequest = GetCapturedRequestParams(capture.RootElement, "session.resume");
        Assert.Equal("advanced-resume-session", resumeRequest.GetProperty("sessionId").GetString());
        Assert.Equal("advanced-resume-client", resumeRequest.GetProperty("clientName").GetString());
        Assert.Equal("claude-haiku-4.5", resumeRequest.GetProperty("model").GetString());
        Assert.Equal("low", resumeRequest.GetProperty("inference").GetProperty("reasoningEffort").GetString());
        Assert.Equal("none", resumeRequest.GetProperty("inference").GetProperty("reasoningSummary").GetString());
        Assert.Equal("default", resumeRequest.GetProperty("inference").GetProperty("contextTier").GetString());
        Assert.True(resumeRequest.GetProperty("suppressResumeEvent").GetBoolean());
        Assert.True(resumeRequest.GetProperty("continuePendingWork").GetBoolean());
        Assert.Equal("persistent", resumeRequest.GetProperty("mcp").GetProperty("oauthTokenStorage").GetString());
        Assert.Equal("plugins-resume", resumeRequest.GetProperty("plugins").GetProperty("directories")[0].GetString());
        Assert.False(resumeRequest.GetProperty("tools").GetProperty("largeOutput").GetProperty("enabled").GetBoolean());
        Assert.Equal(2048, resumeRequest.GetProperty("tools").GetProperty("largeOutput").GetProperty("maxSizeBytes").GetInt64());
        Assert.Equal(outputDirectory, resumeRequest.GetProperty("tools").GetProperty("largeOutput").GetProperty("outputDir").GetString());
        Assert.False(resumeRequest.GetProperty("state").GetProperty("memory").GetProperty("enabled").GetBoolean());
        Assert.False(resumeRequest.TryGetProperty("remoteSession", out _));

        var openCanvas = resumeRequest.GetProperty("canvases").GetProperty("open")[0];
        Assert.Equal("resume-canvas", openCanvas.GetProperty("canvasId").GetString());
        Assert.Equal("dotnet-sdk-tests/resume-extension", openCanvas.GetProperty("extensionId").GetString());
        Assert.Equal("Resume Extension", openCanvas.GetProperty("extensionName").GetString());
        Assert.Equal("resume-canvas-1", openCanvas.GetProperty("instanceId").GetString());
        Assert.Equal(41, openCanvas.GetProperty("input").GetProperty("start").GetInt32());
        Assert.Equal("ready", openCanvas.GetProperty("status").GetString());
        Assert.Equal("Resume Canvas", openCanvas.GetProperty("title").GetString());
        Assert.Equal("https://example.com/resume-canvas", openCanvas.GetProperty("url").GetString());

        await session.DisposeAsync();
    }

    [Fact]
    public async Task Should_Apply_Empty_Mode_Defaults_To_ResumeSession_Wire_Request()
    {
        var (cliPath, capturePath) = await CreateFakeCliCaptureAsync();
        var clientEnv = Ctx.GetEnvironment().ToDictionary(pair => pair.Key, pair => pair.Value);
        clientEnv["COPILOT_RUNTIME_PROCESS_FILE_LOGGING"] = "0";

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.Empty,
            Runtime = RuntimeConnection.ForSpawnedStdio(path: cliPath, args: ["--capture-file", capturePath], baseDirectory: Ctx.WorkDir),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        }, environment: clientEnv);

        await client.StartAsync();

        var session = await Ctx.ResumeSessionAsync(client, "resume-empty-session", new ResumeSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll }, Tools = new SessionToolOptions { AvailableTools = new ToolSet().AddBuiltIn(BuiltInTools.Isolated) } });

        using var capture = JsonDocument.Parse(await File.ReadAllTextAsync(capturePath));
        Assert.Equal("0", capture.RootElement.GetProperty("env").GetProperty("COPILOT_RUNTIME_PROCESS_FILE_LOGGING").GetString());
        var resumeRequest = GetCapturedRequestParams(capture.RootElement, "session.resume");
        Assert.False(resumeRequest.GetProperty("github").GetProperty("telemetryEnabled").GetBoolean());
        Assert.True(resumeRequest.GetProperty("state").GetProperty("skipEmbeddingRetrieval").GetBoolean());
        Assert.False(resumeRequest.GetProperty("instructions").GetProperty("onDemandDiscovery").GetBoolean());
        Assert.Equal("in-memory", resumeRequest.GetProperty("state").GetProperty("embeddingCacheStorage").GetString());
        Assert.False(resumeRequest.GetProperty("hooks").GetProperty("enableRepositoryHooks").GetBoolean());
        Assert.False(resumeRequest.GetProperty("git").GetProperty("enableHostGitOperations").GetBoolean());
        Assert.False(resumeRequest.GetProperty("state").GetProperty("enableCrossSessionStore").GetBoolean());
        Assert.False(resumeRequest.GetProperty("skills").GetProperty("enabled").GetBoolean());
        Assert.True(resumeRequest.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());
        Assert.False(resumeRequest.GetProperty("instructions").TryGetProperty("organizationText", out _));

        await session.DisposeAsync();
    }

    [Fact]
    public void Should_Accept_GitHubToken_Option()
    {
        var options = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            GitHub = new ClientGitHubOptions { Token = "gho_test_token" }
        };

        Assert.Equal("gho_test_token", options.GitHub.Token);
    }

    [Fact]
    public void Should_Default_UseLoggedInUser_To_Null()
    {
        var options = new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent };

        Assert.Null(options.GitHub.UseLoggedInUser);
    }

    [Fact]
    public void Should_Allow_Explicit_UseLoggedInUser_False()
    {
        var options = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        };

        Assert.False(options.GitHub.UseLoggedInUser);
    }

    [Fact]
    public void Should_Allow_Explicit_UseLoggedInUser_True_With_GitHubToken()
    {
        var options = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            GitHub = new ClientGitHubOptions { Token = "gho_test_token", UseLoggedInUser = true }
        };

        Assert.True(options.GitHub.UseLoggedInUser);
    }

    [Fact]
    public void Should_Throw_When_GitHubToken_Used_With_UriConnection()
    {
        Assert.Throws<ArgumentException>(() =>
        {
            _ = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp("localhost", 8080), GitHub = new ClientGitHubOptions { Token = "gho_test_token" } });
        });
    }

    [Fact]
    public void Should_Throw_When_UseLoggedInUser_Used_With_UriConnection()
    {
        Assert.Throws<ArgumentException>(() =>
        {
            _ = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp("localhost", 8080), GitHub = new ClientGitHubOptions { UseLoggedInUser = false } });
        });
    }

    [Fact]
    public void Should_Default_SessionIdleTimeoutSeconds_To_Null()
    {
        var options = new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent };

        Assert.Null((options.Runtime as OwnedRuntimeConnection)?.SessionIdleTimeoutSeconds);
    }

    [Fact]
    public void Should_Accept_SessionIdleTimeoutSeconds_Option()
    {
        var options = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForDefault(sessionIdleTimeoutSeconds: 600)
        };

        Assert.Equal(600, (options.Runtime as OwnedRuntimeConnection)?.SessionIdleTimeoutSeconds);
    }

    private static int GetAvailableTcpPort()
    {
        var listener = new TcpListener(IPAddress.Loopback, 0);
        listener.Start();
        try
        {
            return ((IPEndPoint)listener.LocalEndpoint).Port;
        }
        finally
        {
            listener.Stop();
        }
    }

    private static void AssertArgumentValue(string?[] args, string name, string expectedValue)
    {
        var index = Array.IndexOf(args, name);
        Assert.True(index >= 0, $"Expected argument '{name}' was not present. Args: {string.Join(" ", args)}");
        Assert.True(index + 1 < args.Length, $"Expected argument '{name}' to have a value.");
        Assert.Equal(expectedValue, args[index + 1]);
    }

    private async Task<(string CliPath, string CapturePath)> CreateFakeCliCaptureAsync()
    {
        var cliPath = Path.Join(Ctx.WorkDir, $"fake-cli-{Guid.NewGuid():N}.js");
        var capturePath = Path.Join(Ctx.WorkDir, $"fake-cli-capture-{Guid.NewGuid():N}.json");
        await File.WriteAllTextAsync(cliPath, FakeStdioCliScript);
        return (cliPath, capturePath);
    }

    private static JsonElement GetCapturedRequestParams(JsonElement captureRoot, string method)
    {
        return captureRoot
            .GetProperty("requests")
            .EnumerateArray()
            .Single(request => request.GetProperty("method").GetString() == method)
            .GetProperty("params");
    }

    private const string FakeTcpUnavailablePortCliScript = """
        const fs = require("fs");

        const pidFileIndex = process.argv.indexOf("--pid-file");
        const portIndex = process.argv.indexOf("--announce-port");

        fs.writeFileSync(process.argv[pidFileIndex + 1], String(process.pid));
        console.log(`listening on port ${process.argv[portIndex + 1]}`);

        setInterval(() => {}, 1000);
        """;

    private const string FakeTcpDropDuringStartupCliScript = """
        const net = require("net");

        const server = net.createServer(socket => {
          socket.on("data", () => {
            socket.destroy();
            server.close(() => process.exit(0));
          });
        });

        server.listen(0, "localhost", () => {
          const address = server.address();
          console.log(`listening on port ${address.port}`);
        });

        """;

    private const string FakeStdioCliScript = """
        const fs = require("fs");

        const captureIndex = process.argv.indexOf("--capture-file");
        const captureFile = captureIndex >= 0 ? process.argv[captureIndex + 1] : undefined;
        const requests = [];

        function saveCapture() {
          if (!captureFile) {
            return;
          }

          fs.writeFileSync(captureFile, JSON.stringify({
            args: process.argv.slice(2),
            cwd: process.cwd(),
            requests,
            env: {
              COPILOT_HOME: process.env.COPILOT_HOME,
              COPILOT_SDK_AUTH_TOKEN: process.env.COPILOT_SDK_AUTH_TOKEN,
              COPILOT_RUNTIME_PROCESS_FILE_LOGGING: process.env.COPILOT_RUNTIME_PROCESS_FILE_LOGGING,
              COPILOT_OTEL_ENABLED: process.env.COPILOT_OTEL_ENABLED,
              OTEL_EXPORTER_OTLP_ENDPOINT: process.env.OTEL_EXPORTER_OTLP_ENDPOINT,
              OTEL_EXPORTER_OTLP_PROTOCOL: process.env.OTEL_EXPORTER_OTLP_PROTOCOL,
              COPILOT_OTEL_FILE_EXPORTER_PATH: process.env.COPILOT_OTEL_FILE_EXPORTER_PATH,
              COPILOT_OTEL_EXPORTER_TYPE: process.env.COPILOT_OTEL_EXPORTER_TYPE,
              COPILOT_OTEL_SOURCE_NAME: process.env.COPILOT_OTEL_SOURCE_NAME,
              OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT: process.env.OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT,
              OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT: process.env.OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT
            }
          }));
        }

        saveCapture();

        let buffer = Buffer.alloc(0);

        process.stdin.on("data", chunk => {
          buffer = Buffer.concat([buffer, chunk]);
          processBuffer();
        });

        process.stdin.resume();

        function processBuffer() {
          while (true) {
            const headerEnd = buffer.indexOf("\r\n\r\n");
            if (headerEnd < 0) {
              return;
            }

            const header = buffer.subarray(0, headerEnd).toString("utf8");
            const match = /Content-Length:\s*(\d+)/i.exec(header);
            if (!match) {
              throw new Error("Missing Content-Length header");
            }

            const length = Number(match[1]);
            const bodyStart = headerEnd + 4;
            const bodyEnd = bodyStart + length;
            if (buffer.length < bodyEnd) {
              return;
            }

            const body = buffer.subarray(bodyStart, bodyEnd).toString("utf8");
            buffer = buffer.subarray(bodyEnd);
            handleMessage(JSON.parse(body));
          }
        }

        function handleMessage(message) {
          if (!Object.prototype.hasOwnProperty.call(message, "id")) {
            return;
          }

          requests.push({ method: message.method, params: message.params });
          saveCapture();

          if (message.method === "connect") {
            writeResponse(message.id, { ok: true, protocolVersion: 4, version: "fake" });
            return;
          }

          if (message.method === "ping") {
            writeResponse(message.id, { message: "pong", protocolVersion: 4 });
            return;
          }

          if (message.method === "session.create") {
            const sessionId = message.params?.sessionId ?? message.params?.[0]?.sessionId ?? "fake-session";
            writeResponse(message.id, { sessionId, workspacePath: null, capabilities: null });
            return;
          }

          if (message.method === "session.resume") {
            const sessionId = message.params?.sessionId ?? message.params?.[0]?.sessionId ?? "fake-session";
            writeResponse(message.id, { sessionId, workspacePath: null, capabilities: null });
            return;
          }

          if (message.method === "session.send") {
            writeResponse(message.id, { messageId: "fake-message" });
            return;
          }

          writeResponse(message.id, {});
        }

        function writeResponse(id, result) {
          const body = JSON.stringify({ jsonrpc: "2.0", id, result });
          process.stdout.write(`Content-Length: ${Buffer.byteLength(body, "utf8")}\r\n\r\n${body}`);
        }
        """;
}

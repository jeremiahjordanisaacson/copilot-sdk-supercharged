/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.Collections.Concurrent;
using System.ComponentModel;
using System.Text.Json;
using Microsoft.Copilot.Rpc;
using Microsoft.Copilot.Test.Harness;
using Microsoft.Extensions.AI;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

#pragma warning disable MCPSDK001

public class ScenarioTestingSessionSetupE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : ScenarioTestingE2ETestBase(fixture, "scenario_testing_session_setup", output)
{
    private static readonly TimeSpan TestTimeout = TimeSpan.FromSeconds(60);

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.SelfConfiguredBackend)]
    public async Task Should_Round_Trip_Full_Composed_Scenario_Session_Config()
    {
        var (cliPath, capturePath) = await CreateFakeRuntimeAsync("capture");
        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(
                path: cliPath,
                args: ["--capture-file", capturePath, "--behavior", "capture"]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        var sessionId = $"scenario-client-composed-{Guid.NewGuid():N}";
        await using var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        {
            SessionId = sessionId,
            ClientName = "scenario-client",
            Model = "claude-sonnet-5",
            SystemMessage = new SystemMessageConfig { Mode = SystemMessageMode.Append, Content = "SCENARIO_COMPOSED_SYSTEM_MESSAGE", },
            Inference = new SessionInferenceOptions { ReasoningEffort = "high", ReasoningSummary = ReasoningSummary.Detailed, ContextTier = ContextTier.LongContext, Providers = [new NamedProviderConfig { Name = "scenario-provider", Type = "openai", WireApi = "responses", BaseUrl = "https://provider.example.test/v1", BearerTokenProvider = _ => Task.FromResult("scenario-provider-token"), },], Models = [new ProviderModelConfig { Provider = "scenario-provider", Id = "scenario-model", ModelId = "claude-sonnet-5", WireModel = "scenario-wire-model", },] },
            Events = new SessionEventOptions
            {
                Streaming = true,
                IncludeSubAgentStreamingEvents = false,
                OnEvent = _ =>
        {
        }
            },
            EnableConfigDiscovery = true,
            GitHub = new SessionGitHubOptions { TelemetryEnabled = false, Mcp = new GitHubMcpToolConfig { EnableAllTools = false, AdditionalTools = ["issues.get"], DisableFormDeferral = true, } },
            Experiments = new SessionExperimentOptions { EnableExperimentalMode = true },
            Instructions = new CreateSessionInstructionOptions { SkipCustomInstructions = false, OrganizationText = "SCENARIO_ORG_INSTRUCTIONS", OnDemandDiscovery = false },
            Agents = new SessionAgentOptions { LocalDiscoveryOnly = true, Definitions = [new CustomAgentConfig { Name = "scenario-agent", DisplayName = "Scenario Agent", Description = "scenario client agent", Prompt = "Act as the scenario agent.", Tools = ["scenario_tool"], },], Default = new DefaultAgentConfig { ExcludedTools = ["edit"] }, Selected = "scenario-agent" },
            Git = new SessionGitOptions { CoauthorEnabled = false, EnableHostGitOperations = false },
            Tools = new SessionToolOptions { ManageScheduleEnabled = false, AvailableTools = ["scenario_tool"], ExcludedTools = ["shell"], Definitions = [AIFunctionFactory.Create(() => "unused", "scenario_tool")] },
            State = new SessionStateOptions { SkipEmbeddingRetrieval = true, EmbeddingCacheStorage = EmbeddingCacheStorageMode.InMemory, EnableCrossSessionStore = false },
            Hooks = new SessionHookOptions { EnableRepositoryHooks = false },
            Skills = new SessionSkillOptions { Enabled = false },
            Interaction = new SessionInteractionOptions { Commands = [new CommandDefinition { Name = "scenario-command", Description = "Scenario command", Handler = _ => Task.CompletedTask, },], OnUserInputRequest = (_, _) => Task.FromResult(new UserInputResponse { Answer = "yes" }), OnElicitationRequest = _ => Task.FromResult(new ElicitationResult { Action = UIElicitationResponseAction.Accept }), OnExitPlanModeRequest = (_, _) => Task.FromResult(new ExitPlanModeResult { Approved = true }), OnAutoModeSwitchRequest = (_, _) => Task.FromResult(AutoModeSwitchResponse.No) },
            Mcp = new SessionMcpOptions { Servers = new Dictionary<string, McpServerConfig> { ["scenario-mcp"] = new McpStdioServerConfig { Command = "node", Args = ["scenario-mcp.mjs"], Tools = ["*"], }, }, EnableApps = true, OnAuthRequest = _ => Task.FromResult<McpAuthResult?>(McpAuthResult.Cancel()) },
            Canvases = new SessionCanvasOptions { RequestRenderer = true, Provider = new CanvasProviderIdentity { Id = "scenario:builtin:desktop", Name = "scenario client" }, Definitions = [new CanvasDeclaration { Id = "scenario-canvas", DisplayName = "Scenario Canvas", Description = "Scenario-hosted canvas", },], Handler = new NoOpCanvasHandler() },
            Extensions = new SessionExtensionOptions { Enabled = true, SdkPath = "scenario-extension-sdk", Identity = new ExtensionInfo { Source = "scenario-client", Name = "desktop" } },
            Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll }
        });

        using var capture = await WaitForCaptureAsync(
            capturePath,
            root => GetRequests(root, "session.create").Count == 1
                && GetRequests(root, "session.options.update").Count == 1);
        var request = Assert.Single(GetRequests(capture.RootElement, "session.create")).GetProperty("params");
        var optionsUpdate = Assert.Single(GetRequests(capture.RootElement, "session.options.update")).GetProperty("params");

        Assert.Equal(sessionId, request.GetProperty("sessionId").GetString());
        Assert.Equal("scenario-client", request.GetProperty("clientName").GetString());
        Assert.Equal("claude-sonnet-5", request.GetProperty("model").GetString());
        Assert.Equal("high", request.GetProperty("inference").GetProperty("reasoningEffort").GetString());
        Assert.Equal("detailed", request.GetProperty("inference").GetProperty("reasoningSummary").GetString());
        Assert.Equal("long_context", request.GetProperty("inference").GetProperty("contextTier").GetString());
        Assert.True(request.GetProperty("events").GetProperty("streaming").GetBoolean());
        Assert.False(request.GetProperty("events").GetProperty("includeSubAgentStreamingEvents").GetBoolean());
        Assert.Equal("SCENARIO_COMPOSED_SYSTEM_MESSAGE", request.GetProperty("systemMessage").GetProperty("content").GetString());
        Assert.True(request.GetProperty("enableConfigDiscovery").GetBoolean());
        Assert.False(request.GetProperty("github").GetProperty("telemetryEnabled").GetBoolean());
        Assert.True(request.GetProperty("experiments").GetProperty("enableExperimentalMode").GetBoolean());
        Assert.True(request.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());
        Assert.True(request.GetProperty("state").GetProperty("skipEmbeddingRetrieval").GetBoolean());
        Assert.Equal("in-memory", request.GetProperty("state").GetProperty("embeddingCacheStorage").GetString());
        Assert.Equal("SCENARIO_ORG_INSTRUCTIONS", request.GetProperty("instructions").GetProperty("organizationText").GetString());
        Assert.False(request.GetProperty("instructions").GetProperty("onDemandDiscovery").GetBoolean());
        Assert.False(request.GetProperty("hooks").GetProperty("enableRepositoryHooks").GetBoolean());
        Assert.False(request.GetProperty("git").GetProperty("enableHostGitOperations").GetBoolean());
        Assert.False(request.GetProperty("state").GetProperty("enableCrossSessionStore").GetBoolean());
        Assert.False(request.GetProperty("skills").GetProperty("enabled").GetBoolean());
        Assert.Equal("scenario_tool", request.GetProperty("tools").GetProperty("availableTools")[0].GetString());
        Assert.Equal("shell", request.GetProperty("tools").GetProperty("excludedTools")[0].GetString());
        Assert.Equal("scenario_tool", request.GetProperty("tools").GetProperty("definitions")[0].GetProperty("name").GetString());
        Assert.Equal("scenario-command", request.GetProperty("interaction").GetProperty("commands")[0].GetProperty("name").GetString());
        Assert.Equal("node", request.GetProperty("mcp").GetProperty("servers").GetProperty("scenario-mcp").GetProperty("command").GetString());
        Assert.Equal("scenario-agent", request.GetProperty("agents").GetProperty("definitions")[0].GetProperty("name").GetString());
        Assert.Equal("scenario-agent", request.GetProperty("agents").GetProperty("selected").GetString());
        Assert.Equal("edit", request.GetProperty("agents").GetProperty("default").GetProperty("excludedTools")[0].GetString());
        Assert.Equal("scenario-provider", request.GetProperty("inference").GetProperty("providers")[0].GetProperty("name").GetString());
        Assert.True(request.GetProperty("inference").GetProperty("providers")[0].GetProperty("hasBearerTokenProvider").GetBoolean());
        Assert.Equal("scenario-model", request.GetProperty("inference").GetProperty("models")[0].GetProperty("id").GetString());
        Assert.False(request.TryGetProperty("remoteSession", out _));
        Assert.True(request.GetProperty("mcp").GetProperty("enableApps").GetBoolean());
        Assert.False(request.GetProperty("github").GetProperty("mcp").GetProperty("enableAllTools").GetBoolean());
        Assert.True(request.GetProperty("github").GetProperty("mcp").GetProperty("disableFormDeferral").GetBoolean());
        Assert.True(request.GetProperty("canvases").GetProperty("requestRenderer").GetBoolean());
        Assert.True(request.GetProperty("extensions").GetProperty("enabled").GetBoolean());
        Assert.Equal("scenario-extension-sdk", request.GetProperty("extensions").GetProperty("sdkPath").GetString());
        Assert.Equal("desktop", request.GetProperty("extensions").GetProperty("identity").GetProperty("name").GetString());
        Assert.Equal("scenario:builtin:desktop", request.GetProperty("canvases").GetProperty("provider").GetProperty("id").GetString());
        Assert.Equal("scenario-canvas", request.GetProperty("canvases").GetProperty("definitions")[0].GetProperty("id").GetString());
        Assert.True(request.GetProperty("permissions").GetProperty("request").GetBoolean());
        Assert.True(request.GetProperty("interaction").GetProperty("requestUserInput").GetBoolean());
        Assert.True(request.GetProperty("interaction").GetProperty("requestElicitation").GetBoolean());
        Assert.True(request.GetProperty("interaction").GetProperty("requestExitPlanMode").GetBoolean());
        Assert.True(request.GetProperty("interaction").GetProperty("requestAutoModeSwitch").GetBoolean());
        Assert.Equal(sessionId, optionsUpdate.GetProperty("sessionId").GetString());
        Assert.False(optionsUpdate.GetProperty("instructions").GetProperty("skipCustomInstructions").GetBoolean());
        Assert.True(optionsUpdate.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());
        Assert.False(optionsUpdate.GetProperty("git").GetProperty("coauthorEnabled").GetBoolean());
        Assert.False(optionsUpdate.GetProperty("tools").GetProperty("manageScheduleEnabled").GetBoolean());
    }

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.SelfConfiguredBackend)]
    public async Task Should_Preserve_Omitted_Versus_Disabled_Scenario_Semantics()
    {
        var (cliPath, capturePath) = await CreateFakeRuntimeAsync("capture");
        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(
                path: cliPath,
                args: ["--capture-file", capturePath, "--behavior", "capture"]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        await using var sparse = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { SessionId = "scenario-sparse", Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });
        await using var disabled = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { SessionId = "scenario-disabled", GitHub = new SessionGitHubOptions { TelemetryEnabled = false }, Experiments = new SessionExperimentOptions { EnableExperimentalMode = false }, Instructions = new CreateSessionInstructionOptions { SkipCustomInstructions = false, OnDemandDiscovery = false }, Agents = new SessionAgentOptions { LocalDiscoveryOnly = false }, Git = new SessionGitOptions { CoauthorEnabled = false, EnableHostGitOperations = false }, Tools = new SessionToolOptions { ManageScheduleEnabled = false }, EnableConfigDiscovery = false, State = new SessionStateOptions { SkipEmbeddingRetrieval = false, EnableCrossSessionStore = false }, Hooks = new SessionHookOptions { EnableRepositoryHooks = false }, Skills = new SessionSkillOptions { Enabled = false }, Canvases = new SessionCanvasOptions { RequestRenderer = false }, Extensions = new SessionExtensionOptions { Enabled = false }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        using var capture = await WaitForCaptureAsync(
            capturePath,
            root => GetRequests(root, "session.create").Count == 2
                && GetRequests(root, "session.options.update").Count == 1);
        var requests = GetRequests(capture.RootElement, "session.create")
            .Select(item => item.GetProperty("params"))
            .ToDictionary(item => item.GetProperty("sessionId").GetString()!, StringComparer.Ordinal);
        var sparseRequest = requests["scenario-sparse"];
        var disabledRequest = requests["scenario-disabled"];

        (string Group, string Field)[] fields =
        [
            ("github", "telemetryEnabled"),
            ("experiments", "enableExperimentalMode"),
            ("agents", "localDiscoveryOnly"),
            ("state", "skipEmbeddingRetrieval"),
            ("instructions", "onDemandDiscovery"),
            ("hooks", "enableRepositoryHooks"),
            ("git", "enableHostGitOperations"),
            ("state", "enableCrossSessionStore"),
            ("skills", "enabled"),
            ("canvases", "requestRenderer"),
            ("extensions", "enabled"),
        ];

        Assert.All(fields, field => Assert.False(sparseRequest.GetProperty(field.Group).TryGetProperty(field.Field, out _)));
        Assert.All(fields, field => Assert.False(disabledRequest.GetProperty(field.Group).GetProperty(field.Field).GetBoolean()));
        Assert.False(sparseRequest.TryGetProperty("enableConfigDiscovery", out _));
        Assert.False(disabledRequest.GetProperty("enableConfigDiscovery").GetBoolean());

        var optionsUpdate = Assert.Single(GetRequests(capture.RootElement, "session.options.update"))
            .GetProperty("params");
        Assert.Equal("scenario-disabled", optionsUpdate.GetProperty("sessionId").GetString());
        Assert.False(optionsUpdate.GetProperty("instructions").GetProperty("skipCustomInstructions").GetBoolean());
        Assert.False(optionsUpdate.GetProperty("agents").GetProperty("localDiscoveryOnly").GetBoolean());
        Assert.False(optionsUpdate.GetProperty("git").GetProperty("coauthorEnabled").GetBoolean());
        Assert.False(optionsUpdate.GetProperty("tools").GetProperty("manageScheduleEnabled").GetBoolean());
    }

    [Fact]
    public async Task Should_Use_Preallocated_Id_For_Subscribed_Session_Start_Event()
    {
        var requestedSessionId = Guid.NewGuid().ToString();
        var sessionStarted = new TaskCompletionSource<SessionStartEvent>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var session = await CreateSessionAsync(new CreateSessionOptions
        {
            SessionId = requestedSessionId,
            Events = new SessionEventOptions
            {
                OnEvent = evt =>
        {
            if (evt is SessionStartEvent start)
                sessionStarted.TrySetResult(start);
        }
            }
        });

        var start = await sessionStarted.Task.WaitAsync(TestTimeout);
        Assert.Equal(requestedSessionId, session.SessionId);
        Assert.Equal(requestedSessionId, start.Data.SessionId);
    }

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.SelfConfiguredBackend)]
    public async Task Should_Invoke_All_Scenario_Handler_Kinds()
    {
        var (cliPath, capturePath) = await CreateFakeRuntimeAsync("callbacks");
        var observed = new ConcurrentDictionary<string, byte>(StringComparer.Ordinal);
        var allObserved = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        string[] expected =
        [
            "event",
            "permission",
            "user-input",
            "elicitation",
            "exit-plan",
            "auto-mode",
            "mcp-auth",
            "tool",
            "command",
            "canvas",
            "provider-token",
        ];

        void Mark(string name)
        {
            observed.TryAdd(name, 0);
            if (expected.All(observed.ContainsKey))
            {
                allObserved.TrySetResult();
            }
        }

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(
                path: cliPath,
                args: ["--capture-file", capturePath, "--behavior", "callbacks"]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });
        await using var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        {
            SessionId = "scenario-handler-session",
            Tools = new SessionToolOptions
            {
                Definitions = [AIFunctionFactory.Create(() =>
{
    Mark("tool");
    return "tool-result";
}, "scenario_tool")]
            },
            Interaction = new SessionInteractionOptions
            {
                Commands = [new CommandDefinition { Name = "scenario-command", Handler = _ =>
{
    Mark("command");
    return Task.CompletedTask;
}, }, ],
                OnUserInputRequest = (_, _) =>
{
    Mark("user-input");
    return Task.FromResult(new UserInputResponse { Answer = "approved" });
},
                OnElicitationRequest = _ =>
{
    Mark("elicitation");
    return Task.FromResult(new ElicitationResult { Action = UIElicitationResponseAction.Accept, Content = new Dictionary<string, object> { ["value"] = "accepted" }, });
},
                OnExitPlanModeRequest = (_, _) =>
{
    Mark("exit-plan");
    return Task.FromResult(new ExitPlanModeResult { Approved = true, SelectedAction = "interactive", });
},
                OnAutoModeSwitchRequest = (_, _) =>
{
    Mark("auto-mode");
    return Task.FromResult(AutoModeSwitchResponse.No);
}
            },
            Inference = new SessionInferenceOptions
            {
                Providers = [new NamedProviderConfig { Name = "scenario-provider", Type = "openai", BaseUrl = "https://provider.example.test/v1", BearerTokenProvider = _ =>
{
    Mark("provider-token");
    return Task.FromResult("provider-token");
}, }, ],
                Models = [new ProviderModelConfig { Provider = "scenario-provider", Id = "scenario-model", ModelId = "claude-sonnet-5", },]
            },
            Canvases = new SessionCanvasOptions { Definitions = [new CanvasDeclaration { Id = "scenario-canvas", DisplayName = "Scenario Canvas" }], Provider = new CanvasProviderIdentity { Id = "scenario:builtin:desktop", Name = "scenario client" }, Handler = new CallbackCanvasHandler(() => Mark("canvas")) },
            Permissions = new SessionPermissionOptions
            {
                OnRequest = (_, _) =>
{
    Mark("permission");
    return Task.FromResult(PermissionDecision.ApproveOnce());
}
            },
            Mcp = new SessionMcpOptions
            {
                OnAuthRequest = _ =>
{
    Mark("mcp-auth");
    return Task.FromResult<McpAuthResult?>(McpAuthResult.Cancel());
}
            },
            Events = new SessionEventOptions
            {
                OnEvent = evt =>
{
    if (evt is SessionInfoEvent { Data.Message: "SCENARIO_HANDLER_EVENT" })
    {
        Mark("event");
    }
}
            }
        });

        await allObserved.Task.WaitAsync(TestTimeout);
        Assert.Equal(
            expected.OrderBy(value => value, StringComparer.Ordinal),
            observed.Keys.OrderBy(value => value, StringComparer.Ordinal));

        using var capture = await WaitForCaptureAsync(
            capturePath,
            root => root.GetProperty("clientResponses").GetArrayLength() == 5
                && GetRequests(root, "session.permissions.handlePendingPermissionRequest").Count == 1
                && GetRequests(root, "session.ui.handlePendingElicitation").Count == 1
                && GetRequests(root, "session.mcp.oauth.handlePendingRequest").Count == 1
                && GetRequests(root, "session.tools.handlePendingToolCall").Count == 1
                && GetRequests(root, "session.commands.handlePendingCommand").Count == 1);
        var responses = capture.RootElement.GetProperty("clientResponses")
            .EnumerateArray()
            .ToDictionary(item => item.GetProperty("id").GetInt32());

        Assert.Equal("approved", responses[1000].GetProperty("result").GetProperty("answer").GetString());
        Assert.False(responses[1000].GetProperty("result").GetProperty("wasFreeform").GetBoolean());
        Assert.True(responses[1001].GetProperty("result").GetProperty("approved").GetBoolean());
        Assert.Equal("interactive", responses[1001].GetProperty("result").GetProperty("selectedAction").GetString());
        Assert.Equal("no", responses[1002].GetProperty("result").GetProperty("response").GetString());
        Assert.Equal("ready", responses[1003].GetProperty("result").GetProperty("status").GetString());
        Assert.Equal("Scenario Canvas", responses[1003].GetProperty("result").GetProperty("title").GetString());
        Assert.Equal("provider-token", responses[1004].GetProperty("result").GetProperty("token").GetString());

        Assert.Equal(
            ["permission-1", "elicitation-1", "mcp-auth-1", "tool-1", "command-1"],
            new[]
            {
                GetRequests(capture.RootElement, "session.permissions.handlePendingPermissionRequest").Single(),
                GetRequests(capture.RootElement, "session.ui.handlePendingElicitation").Single(),
                GetRequests(capture.RootElement, "session.mcp.oauth.handlePendingRequest").Single(),
                GetRequests(capture.RootElement, "session.tools.handlePendingToolCall").Single(),
                GetRequests(capture.RootElement, "session.commands.handlePendingCommand").Single(),
            }.Select(item => item.GetProperty("params").GetProperty("requestId").GetString()));
    }

    [Fact]
    public async Task Should_Create_Then_Reload_Mcp_In_Order()
    {
        const string ServerName = "scenario-client-reload";
        var milestones = new List<string>();
        var milestonesLock = new object();
        var startObserved = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var session = await CreateSessionAsync(new CreateSessionOptions
        {
            Mcp = new SessionMcpOptions { Servers = CreateTestMcpServers(ServerName) },
            Events = new SessionEventOptions
            {
                OnEvent = evt =>
        {
            if (evt is SessionStartEvent)
            {
                lock (milestonesLock)
                {
                    milestones.Add("session-start");
                }

                startObserved.TrySetResult();
            }
        }
            }
        });

        await startObserved.Task.WaitAsync(TestTimeout);
        lock (milestonesLock)
        {
            milestones.Add("create-returned");
            milestones.Add("reload-requested");
        }

        await session.Rpc.Mcp.ReloadAsync();
        await WaitForMcpServerStatusAsync(session, ServerName, McpServerStatus.Connected);

        lock (milestonesLock)
        {
            milestones.Add("reload-completed");
            Assert.Equal(
                ["session-start", "create-returned", "reload-requested", "reload-completed"],
                milestones);
        }
    }

    private async Task<(string CliPath, string CapturePath)> CreateFakeRuntimeAsync(string behavior)
    {
        var cliPath = Path.Join(Ctx.WorkDir, $"scenario-client-session-{behavior}-{Guid.NewGuid():N}.js");
        var capturePath = Path.Join(Ctx.WorkDir, $"scenario-client-session-{behavior}-{Guid.NewGuid():N}.json");
        await File.WriteAllTextAsync(cliPath, FakeRuntimeScript);
        return (cliPath, capturePath);
    }

    private static List<JsonElement> GetRequests(JsonElement root, string method) =>
        root.GetProperty("requests")
            .EnumerateArray()
            .Where(item => item.GetProperty("method").GetString() == method)
            .ToList();

    private static async Task<JsonDocument> WaitForCaptureAsync(
        string path,
        Func<JsonElement, bool> predicate)
    {
        JsonDocument? result = null;
        await TestHelper.WaitForConditionAsync(
            async () =>
            {
                try
                {
                    using var stream = new FileStream(
                        path,
                        FileMode.Open,
                        FileAccess.Read,
                        FileShare.ReadWrite | FileShare.Delete);
                    using var reader = new StreamReader(stream);
                    var document = JsonDocument.Parse(await reader.ReadToEndAsync());
                    if (!predicate(document.RootElement))
                    {
                        document.Dispose();
                        return false;
                    }

                    result = document;
                    return true;
                }
                catch (Exception ex) when (ex is IOException or JsonException)
                {
                    return false;
                }
            },
            timeout: TestTimeout,
            pollInterval: TimeSpan.FromMilliseconds(50),
            timeoutMessage: $"Timed out waiting for fake runtime capture at {path}.");
        return result!;
    }

    private sealed class NoOpCanvasHandler : CanvasHandlerBase
    {
        public override Task<CanvasProviderOpenResult> OnOpenAsync(
            CanvasProviderOpenRequest request,
            CancellationToken cancellationToken) =>
            Task.FromResult(new CanvasProviderOpenResult { Status = "ready" });
    }

    private sealed class CallbackCanvasHandler(Action callback) : CanvasHandlerBase
    {
        public override Task<CanvasProviderOpenResult> OnOpenAsync(
            CanvasProviderOpenRequest request,
            CancellationToken cancellationToken)
        {
            callback();
            return Task.FromResult(new CanvasProviderOpenResult { Status = "ready", Title = "Scenario Canvas" });
        }
    }

    private const string FakeRuntimeScript = """
        const fs = require("fs");

        function argument(name) {
          const index = process.argv.indexOf(name);
          return index >= 0 ? process.argv[index + 1] : undefined;
        }

        const captureFile = argument("--capture-file");
        const behavior = argument("--behavior") || "capture";
        const requests = [];
        const clientResponses = [];
        let nextRequestId = 1000;
        let callbackSessionId;
        let callbacksStarted = false;
        let buffer = Buffer.alloc(0);

        function saveCapture() {
          fs.writeFileSync(captureFile, JSON.stringify({ requests, clientResponses }));
        }

        function write(message) {
          const body = JSON.stringify(message);
          process.stdout.write(`Content-Length: ${Buffer.byteLength(body, "utf8")}\r\n\r\n${body}`);
        }

        function respond(id, result) {
          write({ jsonrpc: "2.0", id, result });
        }

        function request(method, params) {
          write({ jsonrpc: "2.0", id: nextRequestId++, method, params });
        }

        function notify(method, params) {
          write({ jsonrpc: "2.0", method, params });
        }

        function event(type, data, ordinal) {
          return {
            id: `00000000-0000-0000-0000-${String(ordinal).padStart(12, "0")}`,
            timestamp: "2026-09-17T20:00:00Z",
            parentId: null,
            type,
            data
          };
        }

        function fireCallbacks() {
          if (callbacksStarted || behavior !== "callbacks") return;
          callbacksStarted = true;
          const sessionId = callbackSessionId;

          request("userInput.request", {
            sessionId,
            question: "Continue?",
            choices: ["approved", "declined"],
            allowFreeform: false
          });
          request("exitPlanMode.request", {
            sessionId,
            summary: "Scenario plan",
            planContent: "# Scenario plan",
            actions: ["interactive", "exit_only"],
            recommendedAction: "interactive"
          });
          request("autoModeSwitch.request", {
            sessionId,
            errorCode: "scenario-rate-limit",
            retryAfterSeconds: 1
          });
          request("canvas.open", {
            sessionId,
            canvasId: "scenario-canvas",
            extensionId: "scenario:builtin:desktop",
            instanceId: "scenario-canvas-1",
            input: { start: 1 }
          });
          request("providerToken.getToken", {
            sessionId,
            providerName: "scenario-provider"
          });

          notify("session.event", {
            sessionId,
            event: event("session.info", {
              infoType: "notification",
              message: "SCENARIO_HANDLER_EVENT"
            }, 1)
          });
          notify("session.event", {
            sessionId,
            event: event("permission.requested", {
              requestId: "permission-1",
              permissionRequest: {
                kind: "read",
                intention: "Read the scenario README",
                path: "README.md"
              }
            }, 2)
          });
          notify("session.event", {
            sessionId,
            event: event("elicitation.requested", {
              requestId: "elicitation-1",
              message: "Provide a value",
              mode: "form",
              requestedSchema: {
                type: "object",
                properties: { value: { type: "string" } },
                required: ["value"]
              }
            }, 3)
          });
          notify("session.event", {
            sessionId,
            event: event("mcp.oauth_required", {
              requestId: "mcp-auth-1",
              reason: "initial",
              serverName: "scenario-mcp",
              serverUrl: "https://example.test/mcp"
            }, 4)
          });
          notify("session.event", {
            sessionId,
            event: event("external_tool.requested", {
              requestId: "tool-1",
              sessionId,
              toolCallId: "tool-call-1",
              toolName: "scenario_tool",
              arguments: {}
            }, 5)
          });
          notify("session.event", {
            sessionId,
            event: event("command.execute", {
              requestId: "command-1",
              commandName: "scenario-command",
              command: "/scenario-command value",
              args: "value"
            }, 6)
          });
        }

        function handle(message) {
          if (!Object.prototype.hasOwnProperty.call(message, "method")) {
            clientResponses.push(message);
            saveCapture();
            return;
          }

          requests.push({ method: message.method, params: message.params });
          saveCapture();

          if (message.method === "connect") {
            respond(message.id, { ok: true, protocolVersion: 4, version: "fake" });
            return;
          }

          if (message.method === "session.create") {
            callbackSessionId = message.params?.sessionId ?? "fake-session";
            respond(message.id, {
              sessionId: callbackSessionId,
              workspacePath: null,
              capabilities: { ui: { elicitation: true } }
            });
            return;
          }

          if (message.method === "session.eventLog.registerInterest") {
            respond(message.id, { handle: "scenario-handler-interest" });
            setTimeout(fireCallbacks, 10);
            return;
          }

          if (message.method === "session.detach") {
            respond(message.id, { success: true });
            return;
          }

          if (message.method === "runtime.shutdown") {
            respond(message.id, {});
            setTimeout(() => process.exit(0), 10);
            return;
          }

          if (message.method === "ping") {
            respond(message.id, {
              message: "pong",
              timestamp: new Date().toISOString(),
              protocolVersion: 4
            });
            return;
          }

          respond(message.id, { success: true });
        }

        process.stdin.on("data", chunk => {
          buffer = Buffer.concat([buffer, chunk]);
          while (true) {
            const headerEnd = buffer.indexOf("\r\n\r\n");
            if (headerEnd < 0) return;
            const header = buffer.subarray(0, headerEnd).toString("utf8");
            const match = /Content-Length:\s*(\d+)/i.exec(header);
            if (!match) throw new Error("Missing Content-Length");
            const length = Number(match[1]);
            const bodyStart = headerEnd + 4;
            const bodyEnd = bodyStart + length;
            if (buffer.length < bodyEnd) return;
            const body = buffer.subarray(bodyStart, bodyEnd).toString("utf8");
            buffer = buffer.subarray(bodyEnd);
            handle(JSON.parse(body));
          }
        });

        process.stdin.resume();
        saveCapture();
        setInterval(() => {}, 1000);
        """;
}

#pragma warning restore MCPSDK001

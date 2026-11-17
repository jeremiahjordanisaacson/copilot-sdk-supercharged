/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Extensions.AI;
using System.ComponentModel;
using System.Text.Json;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

public class ScenarioTestingLifecycleRecoveryE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : ScenarioTestingE2ETestBase(fixture, "scenario_testing_lifecycle_recovery", output)
{
    private static readonly TimeSpan LifecycleTimeout = TimeSpan.FromSeconds(60);

    [Fact]
    public async Task Should_Abort_Active_Scenario_Turn_And_Remain_Usable()
    {
        var toolStarted = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);
        var releaseTool = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var session = await CreateSessionAsync(new CreateSessionOptions
        { Events = new SessionEventOptions { Streaming = true }, Tools = new SessionToolOptions { Definitions = [AIFunctionFactory.Create(BlockingLookup, "scenario_blocking_lookup")] } });

        _ = session.SendAsync(new MessageOptions
        {
            Prompt = "Call scenario_blocking_lookup with key 'abort', then reply with the result.",
            DisplayPrompt = "Run cancellable scenario lookup",
            Source = MessageSource.Agent("scenario-client"),
        });

        Assert.Equal("abort", await toolStarted.Task.WaitAsync(LifecycleTimeout));
        await session.AbortAsync();
        releaseTool.TrySetResult("SCENARIO_ABORTED_TOOL_RESULT");

        var recovery = new TaskCompletionSource<AssistantMessageEvent>(TaskCreationOptions.RunContinuationsAsynchronously);
        using var recoverySubscription = session.On<AssistantMessageEvent>(message =>
        {
            if (message.Data.Content?.Contains("SCENARIO_ABORT_RECOVERY_OK", StringComparison.Ordinal) == true)
            {
                recovery.TrySetResult(message);
            }
        });
        await session.SendAsync(new MessageOptions
        {
            Prompt = "Reply with exactly SCENARIO_ABORT_RECOVERY_OK.",
            DisplayPrompt = "Verify scenario session recovery",
            Source = MessageSource.Agent("scenario-client"),
        });
        Assert.Contains(
            "SCENARIO_ABORT_RECOVERY_OK",
            (await recovery.Task.WaitAsync(LifecycleTimeout)).Data.Content ?? string.Empty,
            StringComparison.Ordinal);

        [Description("Blocks a scenario-owned lookup until released")]
        async Task<string> BlockingLookup(
            [Description("Lookup key")] string key,
            CancellationToken cancellationToken)
        {
            toolStarted.TrySetResult(key);
            return await releaseTool.Task.WaitAsync(Timeout.InfiniteTimeSpan, cancellationToken);
        }
    }

    [Fact]
    public async Task Should_Suspend_Disconnect_And_Resume_Scenario_State_Without_Delete()
    {
        const string connectionToken = "scenario-client-lifecycle-token";
        await using var server = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedTcp(connectionToken: connectionToken)
        });
        await server.StartAsync();
        var runtimePort = server.RuntimePort!.Value;

        string sessionId;
        await using (var firstClient = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp("localhost", runtimePort, connectionToken: connectionToken)
        }))
        {
            var firstSession = await Ctx.CreateSessionAsync(firstClient, new CreateSessionOptions
            { Events = new SessionEventOptions { Streaming = true }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });
            sessionId = firstSession.SessionId;
            var initialized = await firstSession.SendAndWaitAsync(new MessageOptions
            {
                Prompt = "Remember SCENARIO_LIFECYCLE_MEMORY and reply with exactly SCENARIO_LIFECYCLE_INITIALIZED.",
                Source = MessageSource.Agent("scenario-client"),
            });
            Assert.Contains("SCENARIO_LIFECYCLE_INITIALIZED", initialized?.Data.Content ?? string.Empty, StringComparison.Ordinal);

            await firstSession.Rpc.SuspendAsync();
            await firstSession.DisposeAsync();
            await firstClient.ForceStopAsync();
        }

        await using var secondClient = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp("localhost", runtimePort, connectionToken: connectionToken),
        });
        await using var resumed = await Ctx.ResumeSessionAsync(secondClient, sessionId, new ResumeSessionOptions
        { Events = new SessionEventOptions { Streaming = true }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        var response = await resumed.SendAndWaitAsync(new MessageOptions
        {
            Prompt = "Reply with exactly the scenario lifecycle memory value from the earlier turn.",
            Source = MessageSource.Agent("scenario-client"),
        });
        Assert.Contains("SCENARIO_LIFECYCLE_MEMORY", response?.Data.Content ?? string.Empty, StringComparison.Ordinal);
        Assert.Contains((await resumed.GetEventsAsync()).OfType<SessionResumeEvent>(), _ => true);
    }

    [Fact]
    public async Task Should_Classify_Delete_Not_Found_For_Scenario_Cleanup()
    {
        var (cliPath, capturePath) = await ScenarioTestingTestCli.CreateAsync(Ctx);
        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(
                path: cliPath,
                args: ["--capture-file", capturePath, "--behavior", "delete-not-found"]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        const string missingId = "missing-scenario-client-session";
        var exception = await Assert.ThrowsAsync<InvalidOperationException>(() => client.DeleteSessionAsync(missingId));
        Assert.Equal(
            $"Failed to delete session {missingId}: Session file not found",
            exception.Message);

        var requests = await ScenarioTestingTestCli.ReadRequestsAsync(capturePath);
        var delete = Assert.Single(requests, request => request.GetProperty("method").GetString() == "session.delete");
        var parameters = delete.GetProperty("params");
        var request = parameters.ValueKind == JsonValueKind.Array ? parameters[0] : parameters;
        Assert.Equal(missingId, request.GetProperty("sessionId").GetString());
    }

    [Fact]
    public async Task Should_Allow_Caller_Retry_After_Preacceptance_Session_Not_Found()
    {
        var (cliPath, capturePath) = await ScenarioTestingTestCli.CreateAsync(Ctx);
        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(
                path: cliPath,
                args: ["--capture-file", capturePath, "--behavior", "resume-not-found-once"]),
            GitHub = new ClientGitHubOptions { UseLoggedInUser = false }
        });

        const string sessionId = "scenario-client-retry-session";
        var first = await Assert.ThrowsAnyAsync<Exception>(() =>
            Ctx.ResumeSessionAsync(client, sessionId, new ResumeSessionOptions
            { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } }));
        Assert.Contains("Session not found", first.ToString(), StringComparison.OrdinalIgnoreCase);

        await using var resumed = await Ctx.ResumeSessionAsync(client, sessionId, new ResumeSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });
        Assert.Equal(sessionId, resumed.SessionId);

        var requests = await ScenarioTestingTestCli.ReadRequestsAsync(capturePath);
        Assert.Equal(2, requests.Count(request => request.GetProperty("method").GetString() == "session.resume"));
    }
}

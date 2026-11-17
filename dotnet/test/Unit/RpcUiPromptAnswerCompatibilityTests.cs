/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using Microsoft.Copilot.Rpc;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Fact]
    public async Task Prompt_Answers_Preserve_Positional_Calls_And_Carry_A_Label_Through_The_Request_Overload()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = request => request.Method.StartsWith("session.ui.handlePending", StringComparison.Ordinal)
            ? new Dictionary<string, object?> { ["success"] = true }
            : throw new InvalidOperationException($"Unexpected prompt answer request '{request.Method}'.");
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions());
        server.ClearRequests();
        using var cancellation = new CancellationTokenSource();
        var label = new PermissionDecisionContext
        {
            Outcome = PermissionDecisionOutcome.PromptedUser,
            ResponseCapability = PermissionResponseCapability.Interactive,
            Source = PermissionDecisionSource.HumanResponse,
            Surface = PermissionDecisionSurface.Sdk,
        };

        await session.Rpc.Ui.HandlePendingUserInputAsync(
            "input", new UIUserInputResponse { Answer = "yes" }, cancellationToken: cancellation.Token);
        await session.Rpc.Ui.HandlePendingExitPlanModeAsync(
            "plan", new UIExitPlanModeResponse { Approved = true }, cancellationToken: cancellation.Token);
        await session.Rpc.Ui.HandlePendingElicitationAsync(
            "ask", new UIElicitationResponse { Action = UIElicitationResponseAction.Decline }, cancellationToken: cancellation.Token);
        await session.Rpc.Ui.HandlePendingUserInputAsync(requestId: "labeled-input", response: new UIUserInputResponse { Answer = "yes" }, decisionContext: label, cancellationToken: cancellation.Token);
        await session.Rpc.Ui.HandlePendingExitPlanModeAsync(requestId: "labeled-plan", response: new UIExitPlanModeResponse { Approved = true }, decisionContext: label, cancellationToken: cancellation.Token);
        await session.Rpc.Ui.HandlePendingElicitationAsync(requestId: "labeled-ask", result: new UIElicitationResponse { Action = UIElicitationResponseAction.Accept }, decisionContext: label, cancellationToken: cancellation.Token);

        var requests = server.Requests.ToArray();
        Assert.Equal(
            [
                "session.ui.handlePendingUserInput",
                "session.ui.handlePendingExitPlanMode",
                "session.ui.handlePendingElicitation",
                "session.ui.handlePendingUserInput",
                "session.ui.handlePendingExitPlanMode",
                "session.ui.handlePendingElicitation",
            ],
            requests.Select(request => request.Method));
        Assert.All(requests, request =>
            Assert.Equal(session.SessionId, request.Params.GetProperty("sessionId").GetString()));
        Assert.All(requests.Take(3), request =>
            Assert.False(request.Params.TryGetProperty("decisionContext", out _)));
        Assert.All(requests.Skip(3), request =>
        {
            var decisionContext = request.Params.GetProperty("decisionContext");
            Assert.Equal("human_response", decisionContext.GetProperty("source").GetString());
            Assert.Equal("interactive", decisionContext.GetProperty("responseCapability").GetString());
        });
        Assert.Equal("labeled-ask", requests[5].Params.GetProperty("requestId").GetString());
        Assert.False(requests[5].Params.GetProperty("result").TryGetProperty("decisionContext", out _));
    }
}
#endif

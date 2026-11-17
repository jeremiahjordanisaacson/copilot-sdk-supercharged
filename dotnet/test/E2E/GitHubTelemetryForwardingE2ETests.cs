/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Copilot.Rpc;
using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

#pragma warning disable MCPSDK001 // GitHub telemetry forwarding is experimental.

// TODO(BYOK): Anthropic Messages produced no GitHub telemetry notification. Determine whether
// provider-backed sessions should forward the same telemetry before keeping this CAPI-only.
[Trait(E2ETestTraits.Backend, E2ETestTraits.CapiOnly)]
public class GitHubTelemetryForwardingE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : E2ETestBase(fixture, "github_telemetry", output)
{
    [Fact]
    public async Task Should_Forward_GitHub_Telemetry_For_A_Live_Session()
    {
        var sessionId = Guid.NewGuid().ToString();
        var notificationReceived = new TaskCompletionSource<GitHubTelemetryNotification>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            GitHub = new ClientGitHubOptions
            {
                OnTelemetry = notification =>
                        {
                            if (notification.SessionId == sessionId)
                            {
                                notificationReceived.TrySetResult(notification);
                            }
                            return Task.CompletedTask;
                        }
            }
        });

        CopilotSession? session = null;
        try
        {
            session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
            { SessionId = sessionId, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

            var notification = await notificationReceived.Task.WaitAsync(TimeSpan.FromSeconds(30));
            Assert.False(string.IsNullOrEmpty(notification.SessionId));
            Assert.Equal(session.SessionId, notification.SessionId);
            Assert.NotNull(notification.Event);
            Assert.NotEmpty(notification.Event.Kind);
            Assert.IsType<bool>(notification.Restricted);
        }
        finally
        {
            if (session is not null)
            {
                await session.DisposeAsync();
            }

            await client.StopAsync();
        }
    }
}

#pragma warning restore MCPSDK001

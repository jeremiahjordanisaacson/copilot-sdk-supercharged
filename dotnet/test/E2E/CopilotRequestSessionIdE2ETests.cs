/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

#pragma warning disable MCPSDK001 // The LLM inference surface is intentionally experimental.

/// <summary>
/// Asserts the runtime threads its session id into the LLM inference callback
/// for BOTH a CAPI session and a BYOK session. The callback alone services
/// every model-layer request — no upstream server, no CAPI proxy acting as the
/// inference endpoint — so the only source of <c>req.SessionId</c> is the
/// runtime's own per-client threading.
/// </summary>
public class CopilotRequestSessionIdE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : E2ETestBase(fixture, "llm_inference_session_id", output)
{
    private CopilotClient CreateClientWith(RecordingRequestHandler provider) =>
        Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForSpawnedStdio(),
            Models = new ClientModelOptions { RequestHandler = provider }
        });

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.CapiOnly)]
    public async Task Threads_The_Session_Id_Into_A_Capi_Session_Inference_Request()
    {
        var provider = new RecordingRequestHandler();
        await using var client = CreateClientWith(provider);
        await client.StartAsync();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });
        var capiSessionId = session.SessionId;

        string content;
        try
        {
            var msg = await session.SendAndWaitAsync(new MessageOptions { Prompt = "Say OK." });
            content = msg?.Data.Content ?? string.Empty;
        }
        finally
        {
            await session.DisposeAsync();
        }

        var inference = provider.InferenceRequests;
        Assert.NotEmpty(inference);
        Assert.All(inference, r =>
        {
            Assert.Equal(capiSessionId, r.SessionId);
            AssertAgentMetadata(r);
        });

        // Validate the final assistant response arrived (guards against truncated captures)
        Assert.Contains("OK from the synthetic", content);
    }

    [Fact]
    [Trait(E2ETestTraits.Backend, E2ETestTraits.SelfConfiguredBackend)]
    public async Task Threads_The_Session_Id_Into_A_Byok_Session_Inference_Request()
    {
        var provider = new RecordingRequestHandler();
        await using var client = CreateClientWith(provider);
        await client.StartAsync();

        var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        {
            GitHub = new SessionGitHubOptions { Capi = new CapiSessionOptions { Enabled = false } },  // BYOK providers require an explicit model id.
            Model = "claude-sonnet-5",
            Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll },
            Inference = ByokTestOptions.Inference(new ProviderConfig { Type = "openai", WireApi = "responses", BaseUrl = "https://byok.invalid/v1", ApiKey = "byok-secret", ModelId = "claude-sonnet-5", WireModel = "claude-sonnet-5", }, new SessionInferenceOptions { })
        });
        var byokSessionId = session.SessionId;

        string content;
        try
        {
            var msg = await session.SendAndWaitAsync(new MessageOptions { Prompt = "Say OK." });
            content = msg?.Data.Content ?? string.Empty;
        }
        finally
        {
            await session.DisposeAsync();
        }

        var inference = provider.InferenceRequests;
        Assert.NotEmpty(inference);
        Assert.All(inference, r =>
        {
            Assert.Equal(byokSessionId, r.SessionId);
            AssertAgentMetadata(r);
        });

        // Validate the final assistant response arrived (guards against truncated captures)
        Assert.Contains("OK from the synthetic", content);
    }

    private static void AssertAgentMetadata(InterceptedRequest request)
    {
        Assert.False(string.IsNullOrEmpty(request.AgentId));
        Assert.False(string.IsNullOrEmpty(request.InteractionType));
    }
}

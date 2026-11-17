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
    public async Task Models_List_Sends_Account_And_Streamer_Mode_And_Honors_Cancellation()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = request => request.Method == "models.list"
            ? new Dictionary<string, object?> { ["models"] = Array.Empty<object>() }
            : throw new InvalidOperationException($"Unexpected models request '{request.Method}'.");
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await client.StartAsync();
        server.ClearRequests();
        using var cancellation = new CancellationTokenSource();

        await client.Rpc.Models.ListAsync(cancellation.Token);
        await client.Rpc.Models.ListAsync(new ModelsListRequest { SelectionId = "account", GitHubToken = "token" }, cancellation.Token);
        await client.Rpc.Models.ListAsync(new ModelsListRequest { StreamerMode = true });
        await client.Rpc.Models.ListAsync(new ModelsListRequest { StreamerMode = false }, cancellation.Token);
        await client.Rpc.Models.ListAsync();

        var requests = server.Requests.ToArray();
        Assert.Equal(5, requests.Length);
        Assert.All(requests, request => Assert.Equal("models.list", request.Method));
        Assert.False(requests[0].Params.TryGetProperty("streamerMode", out _));
        Assert.False(requests[1].Params.TryGetProperty("streamerMode", out _));
        Assert.Equal("account", requests[1].Params.GetProperty("selectionId").GetString());
        Assert.Equal("token", requests[1].Params.GetProperty("gitHubToken").GetString());
        Assert.True(requests[2].Params.GetProperty("streamerMode").GetBoolean());
        Assert.False(requests[3].Params.GetProperty("streamerMode").GetBoolean());
        Assert.False(requests[4].Params.TryGetProperty("streamerMode", out _));

        cancellation.Cancel();
        await Assert.ThrowsAnyAsync<OperationCanceledException>(
            () => client.Rpc.Models.ListAsync(cancellation.Token));
    }
}
#endif

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
    public async Task Request_Overload_Rejects_Missing_Required_Inputs()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = ServerMcpCompatibilityResponse;
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await client.StartAsync();
        server.ClearRequests();
        await Assert.ThrowsAsync<ArgumentNullException>(() => client.Rpc.Mcp.PlanInstallAsync(contract: null!, source: new McpPlanInstallSourceCandidate { CandidateHandle = "candidate", SearchId = "search" }));
        await Assert.ThrowsAsync<ArgumentNullException>(() => client.Rpc.Mcp.PlanInstallAsync(
            new CatalogClientContract { ProtocolVersion = 3, RequiredCapabilities = [] }, null!));
        Assert.Empty(server.Requests);
    }

    [Fact]
    public async Task Server_Mcp_Calls_Omit_Unspecified_Policy_Session()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = ServerMcpCompatibilityResponse;
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions());
        server.ClearRequests();
        var cancellation = CancellationToken.None;

        await client.Rpc.Mcp.PlanInstallAsync(
            new CatalogClientContract { ProtocolVersion = 3, RequiredCapabilities = ["mcp-install-planning"] },
            new McpPlanInstallSourceCandidate { CandidateHandle = "candidate", SearchId = "search" },
            McpPlanScope.User, cancellationToken: cancellation);
        await client.Rpc.Catalog.SearchAsync(
            new CatalogClientContract { ProtocolVersion = 3, RequiredCapabilities = ["mcp-install-planning"] },
            query: "catalogue query",
            limit: 4, cancellationToken: cancellation);

        var requests = server.Requests.ToArray();
        Assert.Equal(2, requests.Length);
        Assert.All(requests, request => Assert.False(request.Params.TryGetProperty("policySessionId", out _)));
        var plan = Assert.Single(requests, request => request.Method == "mcp.planInstall");
        Assert.Equal("user", plan.Params.GetProperty("scope").GetString());
        var search = Assert.Single(requests, request => request.Method == "catalog.search");
        Assert.Equal("catalogue query", search.Params.GetProperty("query").GetString());
        Assert.Equal(4, search.Params.GetProperty("limit").GetInt32());
    }

    [Fact]
    public async Task Server_Mcp_Calls_Keep_The_Selected_Policy_Session()
    {
        // This fixture checks SDK wire encoding, not runtime admission.
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = ServerMcpCompatibilityResponse;
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions());
        server.ClearRequests();

        await client.Rpc.Mcp.PlanInstallAsync(contract: new CatalogClientContract { ProtocolVersion = 3, RequiredCapabilities = ["mcp-install-planning"] }, source: new McpPlanInstallSourceCandidate { CandidateHandle = "candidate", SearchId = "search" }, scope: McpPlanScope.User, policySessionId: session.SessionId);
        await client.Rpc.Catalog.SearchAsync(contract: new CatalogClientContract { ProtocolVersion = 3, RequiredCapabilities = ["mcp-install-planning"] }, query: "catalogue query", policySessionId: session.SessionId);

        var requests = server.Requests.ToArray();
        Assert.Equal(2, requests.Length);
        var plan = requests[0];
        Assert.Equal("mcp.planInstall", plan.Method);
        Assert.Equal("user", plan.Params.GetProperty("scope").GetString());
        Assert.Equal(session.SessionId, plan.Params.GetProperty("policySessionId").GetString());
        var search = requests[1];
        Assert.Equal("catalog.search", search.Method);
        Assert.Equal("catalogue query", search.Params.GetProperty("query").GetString());
        Assert.Equal(session.SessionId, search.Params.GetProperty("policySessionId").GetString());
    }

    private static Dictionary<string, object?> ServerMcpCompatibilityResponse(RpcRequestRecord request) =>
        request.Method switch
        {
            "mcp.planInstall" or "catalog.search" => new Dictionary<string, object?>
            {
                ["kind"] = "unavailable",
                ["reason"] = "host-not-available",
                ["message"] = "SDK wire fixture",
            },
            _ => throw new InvalidOperationException($"Unexpected server MCP compatibility request '{request.Method}'."),
        };
}
#endif

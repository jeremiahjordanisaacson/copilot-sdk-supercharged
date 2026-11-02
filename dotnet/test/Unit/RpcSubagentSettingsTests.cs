/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using System.Text.Json;
using System.Text.Json.Nodes;
using GitHub.Copilot.Rpc;
using Xunit;

namespace GitHub.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Fact]
    public void Subagent_Settings_Getter_And_Setter_Share_Settings_Type()
    {
        var property = typeof(ToolsGetSubagentSettingsResult).GetProperty("Subagents");
        var method = typeof(ToolsApi).GetMethod("UpdateSubagentSettingsAsync");
        Assert.NotNull(property);
        Assert.NotNull(method);
        Assert.Equal(property.PropertyType, method.GetParameters()[0].ParameterType);
    }

    [Fact]
    public async Task Subagent_Settings_Can_Be_Read_Modified_And_Written_Without_Copying()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        using var response = JsonDocument.Parse("""
            {
                "subagents": {
                    "agents": {
                        "rubber-duck": { "autoInvoke": false, "model": "review-model", "providerId": "review-provider" },
                        "task": { "model": "task-model" }
                    },
                    "disabledSubagents": ["explore"],
                    "maxConcurrency": 7,
                    "maxDepth": 3
                }
            }
            """);
        server.ResponseFactory = request => request.Method switch
        {
            "session.tools.getSubagentSettings" => response.RootElement,
            "session.tools.updateSubagentSettings" => new Dictionary<string, object?>(),
            _ => throw new InvalidOperationException($"Unexpected RPC method '{request.Method}'."),
        };
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Connection = RuntimeConnection.ForUri(server.Url)
        });
        await using var session = await client.CreateSessionAsync(new SessionConfig());
        server.ClearRequests();

        var result = await session.Rpc.Tools.GetSubagentSettingsAsync();
        var settings = result.Subagents;
        Assert.NotNull(settings);
        Assert.NotNull(settings.Agents);
        settings.Agents["rubber-duck"].AutoInvoke = true;
        await session.Rpc.Tools.UpdateSubagentSettingsAsync(settings);

        Assert.Collection(
            server.Requests,
            request => AssertConnectorRequest(request, "session.tools.getSubagentSettings", session.SessionId),
            request =>
            {
                Assert.Equal("session.tools.updateSubagentSettings", request.Method);
                Assert.Equal(session.SessionId, request.Params.GetProperty("sessionId").GetString());
                Assert.Equal(2, request.Params.EnumerateObject().Count());
                var subagents = request.Params.GetProperty("subagents");
                var expected = JsonNode.Parse(response.RootElement.GetProperty("subagents").GetRawText())!;
                expected["agents"]!["rubber-duck"]!["autoInvoke"] = true;
                var actual = JsonNode.Parse(subagents.GetRawText());
                Assert.True(JsonNode.DeepEquals(expected, actual), $"Expected settings: {expected}\nActual settings: {actual}");
            });
    }
}
#endif

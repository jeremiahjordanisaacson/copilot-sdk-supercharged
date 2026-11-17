/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using System.Text.Json;
using Microsoft.Copilot.Rpc;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Fact]
    public async Task Options_Update_Preserves_Grouped_Calls_And_Omitted_Settings()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions());
        server.ClearRequests();

        await session.Rpc.Options.UpdateAsync(model: "configured-model");
        await session.Rpc.Options.UpdateAsync(null);
        await session.Rpc.Options.UpdateAsync();
        await session.Rpc.Options.UpdateAsync(settings: new SessionSettingsPatch { Memory = true });
        await session.Rpc.Options.UpdateAsync(tools: new SessionUpdateToolOptions { ImageGeneration = new ProtocolImageGenerationOptions() });

        await session.Rpc.Options.UpdateAsync(cancellationToken: CancellationToken.None);

        var requests = server.Requests.ToArray();
        Assert.Equal(6, requests.Length);
        Assert.Equal("configured-model", requests[0].Params.GetProperty("model").GetString());
        Assert.Equal(JsonValueKind.Object, requests[4].Params.GetProperty("tools").GetProperty("imageGeneration").ValueKind);
        Assert.True(requests[3].Params.GetProperty("settings").GetProperty("memory").GetBoolean());
        foreach (var request in requests.Where((_, index) => index != 3))
        {
            Assert.False(request.Params.TryGetProperty("settings", out _));
        }
    }

    [Fact]
    public void Model_Price_Units_Keep_Public_Request_And_Cancellation_Overloads()
    {
        var noRequest = typeof(ServerModelsApi).GetMethod("ListTokenPriceUnitsAsync", [typeof(CancellationToken)]);
        var request = typeof(ServerModelsApi).GetMethod("ListTokenPriceUnitsAsync",
            [typeof(ModelsListTokenPriceUnitsRequest), typeof(CancellationToken)]);
        Assert.NotNull(noRequest);
        Assert.NotNull(request);
        Assert.True(Assert.Single(noRequest.GetParameters()).IsOptional);
        Assert.False(request.GetParameters()[0].IsOptional);
        Assert.True(request.GetParameters()[1].IsOptional);
        Assert.True(typeof(ModelsListTokenPriceUnitsRequest).IsPublic);
        Assert.Null(typeof(ModelsListTokenPriceUnitsRequest).GetProperty("SessionId"));
    }

    [Fact]
    public async Task Model_Price_Units_Preserve_Default_And_Selected_Account_Calls()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        using var response = JsonDocument.Parse("""
            {"units":[{"modelId":"from-server","unit":"nano_aiu"}]}
            """);
        server.ResponseFactory = request => request.Method == "models.listTokenPriceUnits"
            ? response.RootElement
            : throw new InvalidOperationException($"Unexpected price-unit request '{request.Method}'.");
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await client.StartAsync();
        server.ClearRequests();
        using var cancellation = new CancellationTokenSource();
        var models = client.Rpc.Models;
        Func<CancellationToken, Task<ModelTokenPriceUnitList>> defaultAccountCall = models.ListTokenPriceUnitsAsync;
        Func<ModelsListTokenPriceUnitsRequest, CancellationToken, Task<ModelTokenPriceUnitList>> requestCall =
            models.ListTokenPriceUnitsAsync;
        var selection = new ModelsListTokenPriceUnitsRequest { SelectionId = "selected-account" };
        var token = new ModelsListTokenPriceUnitsRequest { GitHubToken = "selected-test-token" };

        ModelTokenPriceUnitList[] results =
        [
            await models.ListTokenPriceUnitsAsync(),
            await models.ListTokenPriceUnitsAsync(cancellation.Token),
            await models.ListTokenPriceUnitsAsync(cancellationToken: cancellation.Token),
            await defaultAccountCall(cancellation.Token),
            await models.ListTokenPriceUnitsAsync(selection),
            await models.ListTokenPriceUnitsAsync(token, cancellation.Token),
            await requestCall(selection, cancellation.Token),
            await models.ListTokenPriceUnitsAsync(new ModelsListTokenPriceUnitsRequest()),
        ];
        Assert.All(results, result =>
        {
            var unit = Assert.Single(result.Units);
            Assert.Equal("from-server", unit.ModelId);
            Assert.Equal(TokenPriceUnit.NanoAiu, unit.Unit);
        });
        string[] expectedParams =
        [
            "{}", "{}", "{}", "{}",
            """{"selectionId":"selected-account"}""",
            """{"gitHubToken":"selected-test-token"}""",
            """{"selectionId":"selected-account"}""",
            "{}",
        ];
        var requests = server.Requests.ToArray();
        Assert.Equal(expectedParams.Length, requests.Length);
        for (var index = 0; index < requests.Length; index++)
        {
            Assert.Equal("models.listTokenPriceUnits", requests[index].Method);
            Assert.Equal(JsonValueKind.Object, requests[index].Params.ValueKind);
            using var expected = JsonDocument.Parse(expectedParams[index]);
            Assert.Equal(
                expected.RootElement.EnumerateObject().Select(property => (property.Name, property.Value.GetString())),
                requests[index].Params.EnumerateObject().Select(property => (property.Name, property.Value.GetString())));
        }
        Assert.Equal("selected-account", selection.SelectionId);
        Assert.Null(selection.GitHubToken);
        Assert.Equal("selected-test-token", token.GitHubToken);
        Assert.Null(token.SelectionId);
    }

    [Fact]
    public void Model_Switch_Has_Optional_Last_Cancellation()
    {
        var method = typeof(ModelApi).GetMethod("SwitchToAsync");
        Assert.NotNull(method);
        var parameters = method.GetParameters();
        Assert.False(parameters[0].IsOptional);
        Assert.Equal("cancellationToken", parameters[^1].Name);
        Assert.Equal(typeof(CancellationToken), parameters[^1].ParameterType);
        Assert.All(parameters.Skip(1), parameter =>
        {
            Assert.True(parameter.IsOptional);
            Assert.True(parameter.HasDefaultValue);
            Assert.Null(parameter.DefaultValue);
        });
    }

    [Fact]
    public async Task Model_Switch_Encodes_Selected_Options_And_Omits_Unspecified_Provider()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions());
        server.ClearRequests();

        await session.Rpc.Model.SwitchToAsync("auto", autoTier: AutoTier.Balance);
        await session.Rpc.Model.SwitchToAsync(
            "auto", autoTier: AutoTier.Efficiency, reasoningEffort: "high",
            deferIfModelChangeQueued: true, compactionDecision: "compact", runCompactionPreflight: true,
            repoScope: "repo", modelChangeScope: "global", requireAvailable: true,
            cancellationToken: CancellationToken.None);

        var requests = server.Requests.ToArray();
        Assert.Equal(2, requests.Length);
        foreach (var request in requests)
        {
            Assert.Equal("session.model.switchTo", request.Method);
            Assert.Equal(session.SessionId, request.Params.GetProperty("sessionId").GetString());
            Assert.Equal("auto", request.Params.GetProperty("modelId").GetString());
            Assert.False(request.Params.TryGetProperty("providerId", out _));
        }
        Assert.Equal("balance", requests[0].Params.GetProperty("autoTier").GetString());
        Assert.False(requests[0].Params.TryGetProperty("reasoningEffort", out _));
        Assert.Equal("efficiency", requests[1].Params.GetProperty("autoTier").GetString());
        Assert.Equal("high", requests[1].Params.GetProperty("reasoningEffort").GetString());
        Assert.True(requests[1].Params.GetProperty("deferIfModelChangeQueued").GetBoolean());
        Assert.Equal("compact", requests[1].Params.GetProperty("compactionDecision").GetString());
        Assert.True(requests[1].Params.GetProperty("runCompactionPreflight").GetBoolean());
        Assert.Equal("repo", requests[1].Params.GetProperty("repoScope").GetString());
        Assert.Equal("global", requests[1].Params.GetProperty("modelChangeScope").GetString());
        Assert.True(requests[1].Params.GetProperty("requireAvailable").GetBoolean());
    }

    [Fact]
    public async Task Provider_Qualified_Model_Switch_Uses_Sdk_Owned_Session()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions());
        server.ClearRequests();

        var result = await session.Rpc.Model.SwitchToAsync(
            modelId: "shared-model",
            providerId: "provider-a",
            reasoningEffort: "high",
            cancellationToken: CancellationToken.None);

        var request = Assert.Single(server.Requests);
        Assert.Equal("session.model.switchTo", request.Method);
        Assert.Equal(session.SessionId, request.Params.GetProperty("sessionId").GetString());
        Assert.Equal("shared-model", request.Params.GetProperty("modelId").GetString());
        Assert.Equal("provider-a", request.Params.GetProperty("providerId").GetString());
        Assert.Equal("high", request.Params.GetProperty("reasoningEffort").GetString());
        Assert.False(request.Params.TryGetProperty("autoTier", out _));
        // The fake reports "auto"; the SDK must return the server's result, not the requested id.
        Assert.Equal("auto", result.ModelId);
    }
}
#endif

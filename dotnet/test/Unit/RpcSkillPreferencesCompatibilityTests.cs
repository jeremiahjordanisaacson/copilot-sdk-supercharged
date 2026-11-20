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
    public async Task Skill_Preferences_Preserve_Positional_Tokens_And_Explicit_Profiles()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = request => request.Method == "skills.config.setSkillDisabled"
            ? new Dictionary<string, object?>()
            : throw new InvalidOperationException($"Unexpected skill preference request '{request.Method}'.");
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await client.StartAsync();
        server.ClearRequests();

        await client.Rpc.Skills.Config.SetSkillDisabledAsync("legacy", true, CancellationToken.None);
        await client.Rpc.Skills.Config.SetSkillDisabledAsync(
            new SkillsConfigSetSkillDisabledRequest
            {
                Name = "profile",
                Disabled = false,
                ConfigDir = "/caller-profile"
            },
            CancellationToken.None);

        var requests = server.Requests.ToArray();
        Assert.Equal(2, requests.Length);
        var request = requests[0];
        Assert.Equal("skills.config.setSkillDisabled", request.Method);
        Assert.Equal("legacy", request.Params.GetProperty("name").GetString());
        Assert.True(request.Params.GetProperty("disabled").GetBoolean());
        Assert.False(request.Params.TryGetProperty("configDir", out _));
        Assert.NotNull(typeof(ServerSkillsConfigApi).GetMethod(
            nameof(ServerSkillsConfigApi.SetSkillDisabledAsync),
            [typeof(string), typeof(bool), typeof(CancellationToken)]));
        Assert.Equal("skills.config.setSkillDisabled", requests[1].Method);
        Assert.Equal("profile", requests[1].Params.GetProperty("name").GetString());
        Assert.False(requests[1].Params.GetProperty("disabled").GetBoolean());
        Assert.Equal("/caller-profile", requests[1].Params.GetProperty("configDir").GetString());
    }
}
#endif

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

public class ScenarioTestingEmptyRuntimeE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : ScenarioTestingE2ETestBase(fixture, "scenario_testing_empty_runtime", output)
{
    [Fact]
    public async Task Empty_Mode_Minimal_Toolless_Session_Has_No_Tools()
    {
        await using var client = Ctx.CreateClient(options: new CopilotClientOptions
        {
            Mode = CopilotClientMode.Empty,
            Runtime = RuntimeConnection.ForDefault(baseDirectory: Ctx.HomeDir)
        });
        await using var session = await Ctx.CreateSessionAsync(client, new CreateSessionOptions
        { SystemMessage = new SystemMessageConfig { Mode = SystemMessageMode.Replace, Content = "Reply to every request with exactly EMPTY_SCENARIO_READY.", }, Tools = new SessionToolOptions { AvailableTools = new ToolSet() }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });

        var response = await session.SendAndWaitAsync(new MessageOptions { Prompt = "Start." });
        Assert.Contains("EMPTY_SCENARIO_READY", response?.Data.Content ?? string.Empty, StringComparison.Ordinal);

        var exchanges = await Ctx.GetExchangesAsync();
        Assert.Empty(GetToolNames(exchanges[^1]));
        Assert.DoesNotContain("Current working directory:", GetSystemMessage(exchanges[^1]), StringComparison.OrdinalIgnoreCase);
    }
}

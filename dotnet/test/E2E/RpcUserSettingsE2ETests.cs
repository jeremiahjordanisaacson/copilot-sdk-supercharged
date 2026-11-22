/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.Text.Json;
using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

public class RpcUserSettingsE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : E2ETestBase(fixture, "rpc_user_settings", output)
{
    [Fact]
    public async Task Should_Preserve_Maximum_Unsigned_Settings_Integer()
    {
        var file = Path.Join(Ctx.HomeDir, "settings.json");
        const string original = """{"statusLine":{"command":"echo before","padding":3}}""";
        await File.WriteAllTextAsync(file, original);
        await Client.StartAsync();

        using var settings = JsonDocument.Parse(
            """{"statusLine":{"command":"echo boundary","padding":18446744073709551615}}""");
        await Client.Rpc.User.Settings.SetAsync(settings.RootElement);
        using var stored = JsonDocument.Parse(await File.ReadAllTextAsync(file));
        Assert.Equal(ulong.MaxValue, stored.RootElement.GetProperty("statusLine").GetProperty("padding").GetUInt64());
    }
}

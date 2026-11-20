/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.Reflection;
using System.Text.Json;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

// Not gated on NET8_0_OR_GREATER: the legacy double formatting these cases guard against only exists on .NET Framework.
public class ToolSchemaDefaultsTests
{
    private static JsonElement WithoutRoundedDefaults(string schema)
    {
        var method = typeof(CopilotClient).Assembly
            .GetType("Microsoft.Copilot.ToolSchemaDefaults", throwOnError: true)!
            .GetMethod("WithoutRoundedDefaults", BindingFlags.Public | BindingFlags.Static)!;
        using var document = JsonDocument.Parse(schema);
        return (JsonElement)method.Invoke(null, [document.RootElement.Clone()])!;
    }

    [Theory]
    [InlineData("3.141592653589793")]
    [InlineData("0.1")]
    [InlineData("0.30000000000000004")]
    [InlineData("1E+300")]
    [InlineData("1.7e308")]
    [InlineData("5e-324")]
    [InlineData("-0.5")]
    [InlineData("6.617444900424222e-24")]
    [InlineData("18446744073709551615")]
    [InlineData("-9223372036854775808")]
    public void A_Default_The_Runtime_Reads_Back_Unchanged_Is_Kept(string literal)
    {
        var schema = """{"type":"object","properties":{"value":{"type":"number","default":""" + literal + "}}}";

        var sent = WithoutRoundedDefaults(schema);

        Assert.Equal(literal, sent.GetProperty("properties").GetProperty("value").GetProperty("default").GetRawText());
    }

    [Theory]
    [InlineData("18446744073709551617")]
    [InlineData("-9223372036854775809")]
    [InlineData("0.1234567890123456789")]
    [InlineData("12345678901234567.5")]
    [InlineData("1.7976931348623158e308")]
    public void A_Default_The_Runtime_Would_Round_Is_Left_Out(string literal)
    {
        var schema = """{"type":"object","properties":{"value":{"type":"number","default":""" + literal + "}}}";

        var sent = WithoutRoundedDefaults(schema);

        Assert.False(sent.GetProperty("properties").GetProperty("value").TryGetProperty("default", out _));
    }
}

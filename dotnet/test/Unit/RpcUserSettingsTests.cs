/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET
using System.Text.Json;
using System.Text.Json.Serialization;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Fact]
    public async Task Settings_Set_Preserves_Supported_Numbers_And_Serializes_Custom_Values_Once()
    {
        await using var server = await FakeCopilotServer.StartAsync();
        server.ResponseFactory = _ => null;
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(server.Host, server.Port)
        });
        await client.StartAsync();
        server.ClearRequests();
        var maximum = new SettingsIntegerValue("18446744073709551615");
        var settings = new Dictionary<string, object?>
        {
            ["minimum"] = long.MinValue,
            ["maximum"] = maximum,
            ["floating"] = 1e100,
            ["text"] = "18446744073709551617",
            ["empty"] = null
        };
        object payload = JsonSerializer.IsReflectionEnabledByDefault
            ? settings
            : JsonSerializer.SerializeToElement(settings, SettingsIntegerJsonContext.Default.Settings);

        await client.Rpc.User.Settings.SetAsync(payload);

        var sent = Assert.Single(server.Requests).Params.GetProperty("settings");
        Assert.Equal(long.MinValue, sent.GetProperty("minimum").GetInt64());
        Assert.Equal(ulong.MaxValue, sent.GetProperty("maximum").GetUInt64());
        Assert.Equal(1e100, sent.GetProperty("floating").GetDouble());
        Assert.Equal("18446744073709551617", sent.GetProperty("text").GetString());
        Assert.Equal(JsonValueKind.Null, sent.GetProperty("empty").ValueKind);
        Assert.Equal(1, maximum.Calls);
        Assert.Same(maximum, settings["maximum"]);
    }

    [JsonConverter(typeof(SettingsIntegerValueConverter))]
    private sealed class SettingsIntegerValue(string value)
    {
        public string Value { get; } = value;
        public int Calls { get; set; }
    }

    private sealed class SettingsIntegerValueConverter : JsonConverter<SettingsIntegerValue>
    {
        public SettingsIntegerValueConverter() { }

        public override SettingsIntegerValue Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
            => throw new NotSupportedException();

        public override void Write(Utf8JsonWriter writer, SettingsIntegerValue value, JsonSerializerOptions options)
        {
            value.Calls++;
            writer.WriteRawValue(value.Value);
        }
    }

    [JsonSerializable(typeof(Dictionary<string, object?>), TypeInfoPropertyName = "Settings")]
    [JsonSerializable(typeof(SettingsIntegerValue))]
    [JsonSerializable(typeof(long))]
    [JsonSerializable(typeof(double))]
    private sealed partial class SettingsIntegerJsonContext : JsonSerializerContext;
}
#endif

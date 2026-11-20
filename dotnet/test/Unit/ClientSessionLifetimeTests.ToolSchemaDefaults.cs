/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using System.Globalization;
using System.Text.Json;
using Microsoft.Extensions.AI;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    // The runtime reads a JSON integer exactly only within the signed and unsigned 64-bit ranges, and any other
    // number as a double. These defaults would reach the model as a different number, so they are not advertised.
    private static readonly string[] s_roundedDefaultParameters =
        ["p2_64", "p2_64_plus_1", "neg_p2_63_minus_1", "hi_prec_fraction", "big_fraction"];

    private static string NumericDefaults(
        string required,
        decimal small = 7m,
        decimal p2_53_plus_1 = 9007199254740993m,
        decimal neg_p2_63 = -9223372036854775808m,
        decimal p2_64_minus_1 = 18446744073709551615m,
        decimal p2_64 = 18446744073709551616m,
        decimal p2_64_plus_1 = 18446744073709551617m,
        decimal neg_p2_63_minus_1 = -9223372036854775809m,
        decimal tenth = 0.1m,
        decimal hi_prec_fraction = 0.1234567890123456789012345678m,
        decimal big_fraction = 12345678901234567.5m,
        long long_min = long.MinValue,
        ulong ulong_max = ulong.MaxValue,
        double dbl_tenth = 0.1,
        double dbl_big = 1e300) => required;

    private sealed class SchemaDeclaration(string name, string schema) : AIFunctionDeclaration
    {
        private readonly JsonElement _schema = ParseSchema(schema);

        public override string Name => name;
        public override string Description => "Declared schema";
        public override JsonElement JsonSchema => _schema;

        private static JsonElement ParseSchema(string schema)
        {
            using var document = JsonDocument.Parse(schema);
            return document.RootElement.Clone();
        }
    }

    [Theory]
    [InlineData("DefineTool")]
    [InlineData("AIFunctionFactory")]
    public async Task SessionRequests_Leave_Out_Numeric_Defaults_The_Runtime_Would_Round(string kind)
    {
        AIFunction tool = kind == "DefineTool"
            ? CopilotTool.DefineTool(NumericDefaults, factoryOptions: new AIFunctionFactoryOptions { Name = "numeric_defaults" })
            : AIFunctionFactory.Create(NumericDefaults, new AIFunctionFactoryOptions { Name = "numeric_defaults" });
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });

        await using var created = await client.CreateSessionAsync(new CreateSessionOptions { Tools = new SessionToolOptions { Definitions = [tool] } });
        var createRequest = Assert.Single(server.Requests, request => request.Method == "session.create");
        server.ClearRequests();
        await using var resumed = await client.ResumeSessionAsync("resume-numeric-defaults", new ResumeSessionOptions { Tools = new SessionToolOptions { Definitions = [tool] } });
        var resumeRequest = Assert.Single(server.Requests, request => request.Method == "session.resume");

        var local = tool.JsonSchema;
        foreach (var sent in new[] { createRequest, resumeRequest }.Select(request => request.Params.GetProperty("tools").GetProperty("definitions")[0].GetProperty("parameters")))
        {
            Assert.Equal(local.GetProperty("required").GetRawText(), sent.GetProperty("required").GetRawText());
            Assert.Equal(
                local.GetProperty("properties").EnumerateObject().Select(property => property.Name),
                sent.GetProperty("properties").EnumerateObject().Select(property => property.Name));
            foreach (var localProperty in local.GetProperty("properties").EnumerateObject())
            {
                var sentProperty = sent.GetProperty("properties").GetProperty(localProperty.Name);
                if (s_roundedDefaultParameters.Contains(localProperty.Name))
                {
                    Assert.True(localProperty.Value.TryGetProperty("default", out _), localProperty.Name);
                    Assert.False(sentProperty.TryGetProperty("default", out var advertised), $"{localProperty.Name} advertised {advertised}");
                    Assert.Equal(localProperty.Value.GetProperty("type").GetRawText(), sentProperty.GetProperty("type").GetRawText());
                }
                else
                {
                    // Every other default, including 2^64-1, -2^63, 0.1 and 1E+300, keeps its exact JSON literal.
                    Assert.Equal(localProperty.Value.GetRawText(), sentProperty.GetRawText());
                }
            }
        }
    }

    [Fact]
    public async Task SessionRequests_Leave_Out_Rounded_Defaults_Wherever_A_Declared_Schema_Holds_Them()
    {
        var tool = new SchemaDeclaration(
            "declared_defaults",
            """{"type":"object","properties":{"big":{"type":"integer","default":18446744073709551617},"power_of_ten":{"type":"integer","default":100000000000000000000},"exponent":{"type":"number","default":1e2},"list":{"type":"array","items":{"type":"integer","default":18446744073709551617},"default":[1,18446744073709551617]},"small_list":{"type":"array","default":[1,2]},"default":{"type":"integer","default":18446744073709551617},"choice":{"anyOf":[{"type":"integer","default":-9223372036854775809},{"type":"string","default":"x"}]},"nested":{"type":"object","properties":{"inner":{"type":"number","default":0.1234567890123456789}}}},"$defs":{"d":{"type":"integer","default":18446744073709551616}}}""");
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });

        await using var session = await client.CreateSessionAsync(new CreateSessionOptions { Tools = new SessionToolOptions { Definitions = [tool] } });

        var createRequest = Assert.Single(server.Requests, request => request.Method == "session.create");
        Assert.Equal(
            """{"type":"object","properties":{"big":{"type":"integer"},"power_of_ten":{"type":"integer","default":100000000000000000000},"exponent":{"type":"number","default":1e2},"list":{"type":"array","items":{"type":"integer"}},"small_list":{"type":"array","default":[1,2]},"default":{"type":"integer"},"choice":{"anyOf":[{"type":"integer"},{"type":"string","default":"x"}]},"nested":{"type":"object","properties":{"inner":{"type":"number"}}}},"$defs":{"d":{"type":"integer"}}}""",
            createRequest.Params.GetProperty("tools").GetProperty("definitions")[0].GetProperty("parameters").GetRawText());
    }

    [Fact]
    public async Task SessionRequests_Send_A_Schema_Without_Rounded_Defaults_Byte_For_Byte()
    {
        var tool = CopilotTool.DefineTool(
            (string name, int count = 3, double ratio = 0.30000000000000004, long id = long.MaxValue) => name,
            factoryOptions: new AIFunctionFactoryOptions { Name = "plain_defaults" });
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });

        await using var session = await client.CreateSessionAsync(new CreateSessionOptions { Tools = new SessionToolOptions { Definitions = [tool] } });

        var createRequest = Assert.Single(server.Requests, request => request.Method == "session.create");
        Assert.Equal(tool.JsonSchema.GetRawText(), createRequest.Params.GetProperty("tools").GetProperty("definitions")[0].GetProperty("parameters").GetRawText());
    }

    [Fact]
    public async Task ExternalTool_Applies_The_Exact_Default_When_The_Argument_Is_Omitted()
    {
        var tool = CopilotTool.DefineTool(
            (decimal value = 18446744073709551617m) => value.ToString(CultureInfo.InvariantCulture),
            factoryOptions: new AIFunctionFactoryOptions { Name = "apply_default" });
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions { Tools = new SessionToolOptions { Definitions = [tool] } });

        await server.SendSessionEventAsync(session.SessionId, "external_tool.requested", new()
        {
            ["requestId"] = "default-request",
            ["sessionId"] = session.SessionId,
            ["toolCallId"] = "default-call",
            ["toolName"] = "apply_default",
            ["arguments"] = new Dictionary<string, object?>(),
        });
        var response = await WaitForRequestAsync(server, "session.tools.handlePendingToolCall");

        Assert.Equal("18446744073709551617", response.Params.GetProperty("result").GetProperty("textResultForLlm").GetString());
    }
}
#endif

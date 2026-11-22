/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Xunit;
using System.Reflection;

namespace Microsoft.Copilot.Test.Unit;

public class RuntimeConnectionTcpTests
{
    [Theory]
    [InlineData("GitHub.Token", "token")]
    [InlineData("GitHub.Token", "")]
    [InlineData("GitHub.UseLoggedInUser", null)]
    [InlineData("Observability.LogLevel", null)]
    [InlineData("Observability.Telemetry", null)]
    public void ForTcp_RejectsRuntimeOnlyOptionsByName(string option, string? value)
    {
        var options = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp("localhost", 9000),
        };
        switch (option)
        {
            case "GitHub.Token": options.GitHub.Token = value; break;
            case "GitHub.UseLoggedInUser": options.GitHub.UseLoggedInUser = false; break;
            case "Observability.LogLevel": options.Observability.LogLevel = CopilotLogLevel.None; break;
            case "Observability.Telemetry": options.Observability.Telemetry = new TelemetryConfig(); break;
            default: throw new ArgumentException(option);
        }

        var exception = Assert.Throws<ArgumentException>(() => new CopilotClient(options));
        Assert.Contains(option, exception.Message);
    }

    [Theory]
    [InlineData(nameof(OwnedRuntimeConnection.BaseDirectory))]
    [InlineData(nameof(OwnedRuntimeConnection.SessionIdleTimeoutSeconds))]
    public void ForTcp_DoesNotExposeOwnedStartupOptions(string option)
    {
        Assert.Null(typeof(TcpRuntimeConnection).GetProperty(option));
        Assert.Null(typeof(CopilotClientOptions).GetProperty(option));
        Assert.NotNull(typeof(OwnedRuntimeConnection).GetProperty(option));
    }

    [Fact]
    public async Task ForTcp_AllowsClientCallbacksAndDiagnosticsWithDefaultRuntimeOptions()
    {
        using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.Empty,
            Runtime = RuntimeConnection.ForTcp("localhost", 9000),
            Observability = new ClientObservabilityOptions
            {
                Logger = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance
            },
            Models = new ClientModelOptions
            {
                OnListModels = _ => Task.FromResult<IList<Rpc.Model>>([new() { Id = "host-model" }])
            },
        });

        var model = Assert.Single(await client.ListModelsAsync());
        Assert.Equal("host-model", model.Id);
    }

    [Theory]
    [InlineData("")]
    [InlineData(" ")]
    [InlineData("http://localhost")]
    [InlineData("https://localhost")]
    [InlineData("localhost:9000")]
    [InlineData("localhost/path")]
    [InlineData("[::1]")]
    public void ForTcp_RejectsInvalidHost(string host)
    {
        var exception = Assert.Throws<ArgumentException>(() => new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(host, 9000)
        }));
        Assert.Contains("Host must be a hostname or IP address", exception.Message);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(65536)]
    public void ForTcp_RejectsInvalidPort(int port)
    {
        Assert.Throws<ArgumentOutOfRangeException>(() => new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp("localhost", port)
        }));
    }

    [Theory]
    [InlineData("localhost")]
    [InlineData("127.0.0.1")]
    [InlineData("::1")]
    public void ForTcp_PreservesHostAndPort(string host)
    {
        using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = RuntimeConnection.ForTcp(host, 9000)
        });
        Assert.Equal(host, GetPrivateField<string>(client, "_optionsHost"));
        Assert.Equal(9000, GetPrivateField<int?>(client, "_optionsPort"));
    }

    private static T? GetPrivateField<T>(object instance, string name)
    {
        var field = instance.GetType().GetField(name, BindingFlags.Instance | BindingFlags.NonPublic);
        Assert.NotNull(field);
        return (T?)field.GetValue(instance);
    }
}

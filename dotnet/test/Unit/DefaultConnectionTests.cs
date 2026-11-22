/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.ComponentModel;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

[Collection(RuntimeWrapperIsolationCollection.Name)]
public sealed class DefaultConnectionTests
{
    [Theory]
    [InlineData(null, true)]
    [InlineData("", true)]
    [InlineData("InPrOcEsS", true)]
    [InlineData("STDIO", false)]
    public async Task Default_Runtime_Uses_Default_Or_Override(string? value, bool inProcess)
    {
        var previous = Environment.GetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION");
        var previousCliPath = Environment.GetEnvironmentVariable("COPILOT_CLI_PATH");
        try
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", value);
            var runtime = RuntimeConnection.ForDefault();
            if (inProcess)
            {
                Assert.IsType<InProcessRuntimeConnection>(runtime);
            }
            else
            {
                Assert.IsType<SpawnedStdioRuntimeConnection>(runtime);
            }
            Environment.SetEnvironmentVariable("COPILOT_CLI_PATH",
                Path.Combine(Path.GetTempPath(), $"copilot-default-connection-{Guid.NewGuid():N}", "copilot"));
            using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent });

            // Missing assets distinguish loading the in-process library from spawning an executable.
            if (inProcess)
            {
                var error = await Assert.ThrowsAsync<InvalidOperationException>(() => client.StartAsync());
                Assert.Contains("FFI runtime library not found", error.Message);
            }
            else
            {
                await Assert.ThrowsAsync<Win32Exception>(() => client.StartAsync());
            }
        }
        finally
        {
            Environment.SetEnvironmentVariable("COPILOT_CLI_PATH", previousCliPath);
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", previous);
        }
    }

    [Fact]
    public void Explicit_Connection_Ignores_Invalid_Override()
    {
        var previous = Environment.GetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION");
        try
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", "inprocess");
            var options = new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent };
            using var initialClient = new CopilotClient(options);
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", "invalid");
            var error = Assert.Throws<ArgumentException>(() => new CopilotClient(options));
            Assert.Contains("Invalid COPILOT_SDK_DEFAULT_CONNECTION", error.Message);
            foreach (var runtime in new RuntimeConnection[]
            {
                RuntimeConnection.ForSpawnedStdio(),
                RuntimeConnection.ForSpawnedTcp(),
                RuntimeConnection.ForInProcess(),
                RuntimeConnection.ForTcp("localhost", 8080),
            })
            {
                using var client = new CopilotClient(new CopilotClientOptions
                {
                    Mode = CopilotClientMode.CodingAgent,
                    Runtime = runtime,
                });
            }
        }
        finally
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", previous);
        }
    }

    [Theory]
    [InlineData("COPILOT_SDK_DEFAULT_CONNECTION")]
    [InlineData("copilot_sdk_default_connection")]
    public void Default_Runtime_Uses_Platform_Environment_Key_Casing(string name)
    {
        var previous = Environment.GetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION");
        var previousNamed = Environment.GetEnvironmentVariable(name);
        try
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", null);
            Environment.SetEnvironmentVariable(name, "stdio");
            var runtime = RuntimeConnection.ForDefault();
            if (OperatingSystem.IsWindows() || name == "COPILOT_SDK_DEFAULT_CONNECTION")
            {
                Assert.IsType<SpawnedStdioRuntimeConnection>(runtime);
            }
            else
            {
                Assert.IsType<InProcessRuntimeConnection>(runtime);
            }
        }
        finally
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", previous);
            Environment.SetEnvironmentVariable(name, previousNamed);
        }
    }

    [Fact]
    public void Default_Runtime_Captures_Environment_When_Factory_Is_Called()
    {
        var previous = Environment.GetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION");
        try
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", null);
            var runtime = RuntimeConnection.ForDefault(baseDirectory: "owned-state");
            Assert.IsType<InProcessRuntimeConnection>(runtime);
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", "invalid");
            using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = runtime,
            });
            Assert.Equal("owned-state", runtime.BaseDirectory);
            Assert.Throws<ArgumentException>(() => RuntimeConnection.ForDefault());
        }
        finally
        {
            Environment.SetEnvironmentVariable("COPILOT_SDK_DEFAULT_CONNECTION", previous);
        }
    }
}

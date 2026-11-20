/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using System.Diagnostics;
using System.Text;
using Microsoft.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.Unit;

public sealed class StdioShutdownTests(ITestOutputHelper output)
{
    [Theory]
    [InlineData("stop")]
    [InlineData("dispose")]
    [InlineData("force")]
    [InlineData("fallback")]
    [InlineData("start-failure")]
    public async Task Owned_Stdio_Runtime_Finishes_Host_Cleanup_Before_Graceful_Stop_Returns(string operation)
    {
        var directory = Path.Combine(Path.GetTempPath(), $"copilot-shutdown-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);
        var script = Path.Combine(E2ETestBase.FindTestHarnessDir(), "stdio-shutdown-runtime.cjs");
        var marker = Path.Combine(directory, "telemetry.jsonl");
        var pidPath = Path.Combine(directory, "runtime.pid");
        var tracePath = Path.Combine(directory, "startup-trace.jsonl");
        List<string> args = [script, marker, operation, pidPath];
        if (operation == "start-failure")
        {
            args.AddRange(["--fixture-trace", tracePath]);
        }

        try
        {
            using var observer = operation == "start-failure"
                ? new RuntimeStartupProcessObserver(pidPath, useStdio: true)
                : null;
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = RuntimeConnection.ForSpawnedStdio(path: "node", args: args),
                GitHub = new ClientGitHubOptions { UseLoggedInUser = false },
                Observability = new ClientObservabilityOptions { Logger = observer }
            });
            if (operation == "start-failure")
            {
                await WithFailureDiagnosticsAsync(async () =>
                {
                    // Failed startup includes owned-process cleanup, which has its own longer budgets.
                    var error = await Assert.ThrowsAsync<InvalidOperationException>(() => client.StartAsync());
                    Assert.Contains("protocol version mismatch", error.Message);
                    Assert.True(observer!.Process.HasExited, "Failed startup retained the runtime process.");
                    Assert.False(File.Exists(marker));
                }, pidPath, tracePath,
                    () => $"at failure: observed-pid={observer!.Process.Id}; has-exited={observer.Process.HasExited}",
                    output.WriteLine);
                return;
            }

            await client.StartAsync();
            using var process = Process.GetProcessById(
                int.Parse(await File.ReadAllTextAsync(pidPath), System.Globalization.CultureInfo.InvariantCulture));
            _ = process.Handle;
            var elapsed = Stopwatch.StartNew();

            switch (operation)
            {
                case "stop":
                    await client.StopAsync().WaitAsync(TimeSpan.FromSeconds(5));
                    break;
                case "dispose":
                    await client.DisposeAsync().AsTask().WaitAsync(TimeSpan.FromSeconds(5));
                    break;
                case "fallback":
                    // Allow shutdown RPC, graceful exit, kill/reap, and stderr drain their separate budgets.
                    await client.StopAsync().WaitAsync(TimeSpan.FromSeconds(40));
                    Assert.True(elapsed.Elapsed >= TimeSpan.FromSeconds(10),
                        "Graceful stop must wait for its exit timeout before terminating the child.");
                    break;
                default:
                    await client.ForceStopAsync().WaitAsync(TimeSpan.FromSeconds(5));
                    break;
            }

            Assert.True(process.HasExited);
            await client.DisposeAsync();
            await client.DisposeAsync();
            if (operation == "force")
            {
                Assert.False(File.Exists(marker));
            }
            else
            {
                Assert.Equal("{\"type\":\"span\"}\n", await File.ReadAllTextAsync(marker));
            }
        }
        catch (Exception failure) when (operation == "start-failure")
        {
            await ReportFailureDiagnosticsAsync(failure, pidPath, tracePath,
                () => "after client cleanup; before fixture removal", output.WriteLine);
            throw;
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Failure_Diagnostics_Preserve_The_Original_Error_When_Diagnostic_Io_Fails(bool outputFails)
    {
        var directory = Path.Combine(Path.GetTempPath(), $"copilot-shutdown-diagnostics-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);
        var pidPath = Path.Combine(directory, "runtime.pid");
        var original = new InvalidOperationException("controlled failure");
        List<string> messages = [];
        try
        {
            await File.WriteAllTextAsync(pidPath, "123");
            var actual = await Assert.ThrowsAsync<InvalidOperationException>(() =>
                WithFailureDiagnosticsAsync(
                    () => Task.FromException(original), pidPath, directory,
                    () => throw new InvalidOperationException("not admitted"),
                    message =>
                    {
                        messages.Add(message);
                        if (outputFails) throw new IOException("controlled output failure");
                    }));

            Assert.Same(original, actual);
            var diagnostic = Assert.Single(messages);
            Assert.Contains("pid: 123", diagnostic);
            Assert.Contains("trace: diagnostic-error=", diagnostic);
            Assert.Contains("observer: diagnostic-error=InvalidOperationException", diagnostic);
            Assert.Equal(diagnostic, original.Data["StdioShutdownDiagnostics"]);
            if (outputFails)
            {
                Assert.Equal("output: diagnostic-error=IOException", original.Data["StdioShutdownDiagnosticError"]);
            }
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    private static async Task WithFailureDiagnosticsAsync(
        Func<Task> action, string pidPath, string tracePath, Func<string> observe, Action<string> report)
    {
        try
        {
            await action();
        }
        catch (Exception failure)
        {
            await ReportFailureDiagnosticsAsync(failure, pidPath, tracePath, observe, report);
            throw;
        }
    }

    private static async Task ReportFailureDiagnosticsAsync(
        Exception failure, string pidPath, string tracePath, Func<string> observe, Action<string> report)
    {
        var diagnostic = new StringBuilder("stdio start-failure diagnostics\n");
        async Task ReadAsync(string label, string path)
        {
            try
            {
                var contents = await File.ReadAllTextAsync(path);
                diagnostic.Append(label).Append(": ").AppendLine(contents);
            }
            catch (Exception error)
            {
                diagnostic.Append(label).Append(": diagnostic-error=").AppendLine(error.GetType().Name);
            }
        }
        await ReadAsync("pid", pidPath);
        await ReadAsync("trace", tracePath);
        try
        {
            diagnostic.AppendLine(observe());
        }
        catch (Exception error)
        {
            diagnostic.Append("observer: diagnostic-error=").AppendLine(error.GetType().Name);
        }
        var text = diagnostic.ToString();
        failure.Data["StdioShutdownDiagnostics"] = text;
        try
        {
            report(text);
        }
        catch (Exception error)
        {
            failure.Data["StdioShutdownDiagnosticError"] = $"output: diagnostic-error={error.GetType().Name}";
        }
    }
}
#endif

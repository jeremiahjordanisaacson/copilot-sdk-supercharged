/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using Microsoft.Copilot.Rpc;
using System.Diagnostics;
using System.Globalization;
using System.Reflection;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

[CollectionDefinition(Name, DisableParallelization = true)]
public sealed class RuntimeWrapperIsolationCollection
{
    public const string Name = "Runtime wrapper isolation";
}

[Collection(RuntimeWrapperIsolationCollection.Name)]
public sealed class RuntimeWrapperTests
{
#if NET
    [Fact]
    public async Task Managed_Launch_Fails_When_Bundled_Runtime_Pair_Is_Missing()
    {
        var originalBaseDirectory = AppContext.GetData("APP_CONTEXT_BASE_DIRECTORY");
        var emptyBaseDirectory = Path.Combine(
            Path.GetTempPath(),
            $"missing-copilot-runtime-{Guid.NewGuid():N}");
        Directory.CreateDirectory(emptyBaseDirectory);

        try
        {
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", emptyBaseDirectory);
            var connection = RuntimeConnection.ForSpawnedStdio();
            connection.Environment = new Dictionary<string, string>();
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = connection,
            });

            var exception = await Assert.ThrowsAsync<InvalidOperationException>(() => client.StartAsync());

            Assert.Contains("runtime wrapper not found", exception.Message, StringComparison.OrdinalIgnoreCase);
        }
        finally
        {
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", originalBaseDirectory);
            Directory.Delete(emptyBaseDirectory);
        }
    }
#endif

    [Fact]
    public async Task Explicit_Path_Does_Not_Require_Adjacent_Runtime_Node()
    {
        var explicitPath = Path.Combine(
            Path.GetTempPath(),
            $"missing-explicit-copilot-{Guid.NewGuid():N}");
        var connection = RuntimeConnection.ForSpawnedStdio(path: explicitPath);
        connection.Environment = new Dictionary<string, string>();
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = connection,
        });

        var exception = await Assert.ThrowsAnyAsync<Exception>(() => client.StartAsync());

        Assert.DoesNotContain("runtime.node", exception.ToString(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public async Task Copilot_Cli_Path_Does_Not_Require_Adjacent_Runtime_Node()
    {
        var explicitPath = Path.Combine(
            Path.GetTempPath(),
            $"missing-environment-copilot-{Guid.NewGuid():N}");
        var connection = RuntimeConnection.ForSpawnedStdio();
        connection.Environment = new Dictionary<string, string> { ["COPILOT_CLI_PATH"] = explicitPath };
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = connection,
        });

        var exception = await Assert.ThrowsAnyAsync<Exception>(() => client.StartAsync());

        Assert.DoesNotContain("runtime.node", exception.ToString(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public async Task Copilot_Cli_Path_Uses_Platform_Key_Casing()
    {
        var directory = Path.Combine(Path.GetTempPath(), $"missing-copilot-{Guid.NewGuid():N}");
        var upper = Path.Combine(directory, "uppercase-copilot");
        var lower = Path.Combine(directory, "lowercase-copilot");
        Directory.CreateDirectory(directory);
        try
        {
            File.WriteAllText(upper, string.Empty);
            var connection = RuntimeConnection.ForSpawnedStdio();
            connection.Environment = new SortedDictionary<string, string>(StringComparer.Ordinal)
            {
                ["COPILOT_CLI_PATH"] = upper,
                ["copilot_cli_path"] = lower,
            };
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = connection,
            });

            var exception = await Assert.ThrowsAsync<System.ComponentModel.Win32Exception>(() => client.StartAsync());

            if (OperatingSystem.IsWindows())
            {
                Assert.Equal(2, exception.NativeErrorCode);
            }
            else
            {
                Assert.NotEqual(2, exception.NativeErrorCode);
            }
        }
        finally
        {
            File.Delete(upper);
            Directory.Delete(directory);
        }
    }

#if NET
    [Fact]
    public async Task ForceStop_Terminates_Descendants_Without_Pgrep()
    {
        if (OperatingSystem.IsWindows())
        {
            return;
        }

        var directory = Directory.CreateTempSubdirectory("copilot-process-tree-");
        var scriptPath = Path.Combine(directory.FullName, "runtime.cjs");
        var childPidPath = Path.Combine(directory.FullName, "child.pid");
        var script = (string)typeof(E2E.ScenarioTestingRuntimeE2ETests)
            .GetField("FakeRuntimeScript", BindingFlags.NonPublic | BindingFlags.Static)!.GetRawConstantValue()!;
        Process? descendant = null;
        try
        {
            await File.WriteAllTextAsync(scriptPath, """
                const child = require("child_process").spawn(process.execPath,
                  ["-e", "process.stdout.write('ready'); setInterval(() => {}, 1000)"],
                  { stdio: ["ignore", "pipe", "ignore"] });
                child.stdout.once("data", () => {
                  require("fs").writeFileSync(process.argv[process.argv.indexOf("--child-pid-file") + 1], String(child.pid));
                """ + script + "\n});");
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = RuntimeConnection.ForSpawnedStdio(path: "node", args:
                [
                    scriptPath,
                    "--child-pid-file", childPidPath,
                    "--pid-file", Path.Combine(directory.FullName, "runtime.pid"),
                    "--capture-file", Path.Combine(directory.FullName, "capture.json"),
                ]),
                GitHub = new ClientGitHubOptions { UseLoggedInUser = false },
            });
            await client.StartAsync();
            descendant = Process.GetProcessById(
                int.Parse(await File.ReadAllTextAsync(childPidPath), CultureInfo.InvariantCulture));
            _ = descendant.Handle;
            Assert.False(descendant.HasExited);
            var originalPath = System.Environment.GetEnvironmentVariable("PATH");
            try
            {
                System.Environment.SetEnvironmentVariable("PATH", directory.FullName);
                await client.ForceStopAsync();
            }
            finally
            {
                System.Environment.SetEnvironmentVariable("PATH", originalPath);
            }

            await descendant.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(5));
            Assert.True(descendant.HasExited, "Force-stop left a runtime descendant running without pgrep.");
        }
        finally
        {
            if (descendant is not null)
            {
                using (descendant)
                {
                    if (!descendant.HasExited)
                    {
                        descendant.Kill(entireProcessTree: true);
                        await descendant.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(5));
                    }
                }
            }
            directory.Delete(recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Bundled_Runtime_Repairs_Missing_Unix_Execute_Bits(bool repairDenied)
    {
        // macOS user-immutable files reject chmod without requiring elevated fixture privileges.
        if (OperatingSystem.IsWindows() || (repairDenied && !OperatingSystem.IsMacOS()))
        {
            return;
        }

        var originalBaseDirectory = AppContext.GetData("APP_CONTEXT_BASE_DIRECTORY");
        var baseDirectory = Path.Combine(Path.GetTempPath(), $"nonexecutable-runtime-{Guid.NewGuid():N}");
        var nativeDirectory = Path.Combine(baseDirectory, "runtimes", GetPortableRid(), "native");
        Directory.CreateDirectory(nativeDirectory);
        var wrapper = Path.Combine(nativeDirectory, "copilot-runtime");
        var immutable = false;

        try
        {
            File.Copy("/bin/sh", wrapper);
            await File.WriteAllTextAsync(Path.Combine(nativeDirectory, "runtime.node"), "unused");
            const UnixFileMode originalMode = UnixFileMode.UserRead | UnixFileMode.UserWrite;
            File.SetUnixFileMode(wrapper, originalMode);
            if (repairDenied)
            {
                await SetImmutableAsync(true);
                immutable = true;
            }
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", baseDirectory);
            var connection = RuntimeConnection.ForSpawnedStdio();
            connection.Environment = new Dictionary<string, string>();
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = connection,
            });

            var exception = await Assert.ThrowsAnyAsync<Exception>(() => client.StartAsync());

            if (repairDenied)
            {
                Assert.IsType<UnauthorizedAccessException>(exception);
                Assert.Equal(originalMode, File.GetUnixFileMode(wrapper));
            }
            else
            {
                // The shell rejects runtime arguments, but must first launch successfully.
                Assert.IsNotType<System.ComponentModel.Win32Exception>(exception);
                Assert.Equal(
                    originalMode | UnixFileMode.UserExecute | UnixFileMode.GroupExecute | UnixFileMode.OtherExecute,
                    File.GetUnixFileMode(wrapper));
            }
        }
        finally
        {
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", originalBaseDirectory);
            if (immutable)
            {
                await SetImmutableAsync(false);
            }
            Directory.Delete(baseDirectory, recursive: true);
        }

        async Task SetImmutableAsync(bool value)
        {
            using var process = Process.Start(new ProcessStartInfo("/usr/bin/chflags")
            {
                UseShellExecute = false,
                ArgumentList = { value ? "uchg" : "nouchg", wrapper },
            })!;
            await process.WaitForExitAsync();
            Assert.Equal(0, process.ExitCode);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Empty_Copilot_Cli_Path_Uses_Bundled_Runtime(bool tcp)
    {
        var originalBaseDirectory = AppContext.GetData("APP_CONTEXT_BASE_DIRECTORY");
        var emptyBaseDirectory = Path.Combine(
            Path.GetTempPath(),
            $"empty-cli-path-copilot-{Guid.NewGuid():N}");
        Directory.CreateDirectory(emptyBaseDirectory);
        var environment = new Dictionary<string, string> { ["COPILOT_CLI_PATH"] = string.Empty };
        ChildProcessRuntimeConnection connection = tcp ? RuntimeConnection.ForSpawnedTcp() : RuntimeConnection.ForSpawnedStdio();
        connection.Environment = environment;
        var originalCliPath = System.Environment.GetEnvironmentVariable("COPILOT_CLI_PATH");

        try
        {
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", emptyBaseDirectory);
            // The configured environment replaces the inherited one, so this value must not be used either.
            System.Environment.SetEnvironmentVariable(
                "COPILOT_CLI_PATH",
                Path.Combine(emptyBaseDirectory, "inherited-copilot"));
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = connection,
            });

            var exception = await Assert.ThrowsAsync<InvalidOperationException>(() => client.StartAsync());

            // The bundled runtime pair was looked up instead of launching an empty path.
            Assert.Contains("runtime wrapper not found", exception.Message, StringComparison.OrdinalIgnoreCase);
        }
        finally
        {
            System.Environment.SetEnvironmentVariable("COPILOT_CLI_PATH", originalCliPath);
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", originalBaseDirectory);
            Directory.Delete(emptyBaseDirectory);
        }
    }

    [Fact]
    public async Task Nonempty_Copilot_Cli_Path_Is_The_Launch_Path()
    {
        var explicitPath = Path.Combine(
            Path.GetTempPath(),
            $"missing-environment-copilot-{Guid.NewGuid():N}");
        var connection = RuntimeConnection.ForSpawnedStdio();
        connection.Environment = new Dictionary<string, string> { ["COPILOT_CLI_PATH"] = explicitPath };
        await using var client = new CopilotClient(new CopilotClientOptions
        {
            Mode = CopilotClientMode.CodingAgent,
            Runtime = connection,
        });

        var exception = await Assert.ThrowsAnyAsync<Exception>(() => client.StartAsync());

        Assert.Contains(explicitPath, exception.ToString(), StringComparison.OrdinalIgnoreCase);
        Assert.DoesNotContain("runtime wrapper", exception.ToString(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public async Task Marked_Bundled_Explicit_Cli_Does_Not_Require_Runtime_Pair()
    {
        var originalBaseDirectory = AppContext.GetData("APP_CONTEXT_BASE_DIRECTORY");
        var baseDirectory = Path.Combine(
            Path.GetTempPath(),
            $"explicit-bundled-copilot-{Guid.NewGuid():N}");
        var rid = GetPortableRid();
        var nativeDirectory = Path.Combine(baseDirectory, "runtimes", rid, "native");
        Directory.CreateDirectory(nativeDirectory);
        var cliPath = Path.Combine(nativeDirectory, OperatingSystem.IsWindows() ? "copilot.exe" : "copilot");
        // Nonempty invalid images can stall Windows process creation before it reports the launch error.
        await File.WriteAllTextAsync(cliPath, string.Empty);
        await File.WriteAllTextAsync(Path.Combine(nativeDirectory, ".copilot-explicit-cli"), "explicit");

        try
        {
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", baseDirectory);
            var connection = RuntimeConnection.ForSpawnedStdio();
            connection.Environment = new Dictionary<string, string>();
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = connection,
            });

            var exception = await Assert.ThrowsAnyAsync<Exception>(() => client.StartAsync());

            Assert.DoesNotContain("runtime wrapper", exception.ToString(), StringComparison.OrdinalIgnoreCase);
            Assert.Contains(cliPath, exception.ToString(), StringComparison.OrdinalIgnoreCase);
        }
        finally
        {
            AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", originalBaseDirectory);
            Directory.Delete(baseDirectory, recursive: true);
        }
    }
#endif

    private static string GetPortableRid()
    {
        var os = OperatingSystem.IsWindows() ? "win"
            : OperatingSystem.IsMacOS() ? "osx"
            : IsMusl()
                ? "linux-musl"
                : "linux";
        var architecture = System.Runtime.InteropServices.RuntimeInformation.OSArchitecture switch
        {
            System.Runtime.InteropServices.Architecture.X64 => "x64",
            System.Runtime.InteropServices.Architecture.Arm64 => "arm64",
            _ => throw new PlatformNotSupportedException(),
        };
        return $"{os}-{architecture}";
    }

    private static bool IsMusl()
    {
#if !NET
        return false;
#else
        return System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier.StartsWith(
            "linux-musl-",
            StringComparison.Ordinal);
#endif
    }
}

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using System.Text.Json;
using GitHub.Copilot.Rpc;
using Xunit;

namespace GitHub.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Theory]
    [InlineData(CopilotClientMode.CopilotCli)]
    [InlineData(CopilotClientMode.Empty)]
    public async Task Ahp_Requires_Authentication_Before_Connecting_In_Every_Mode(CopilotClientMode mode)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new() { Connection = RuntimeConnection.ForUri(server.Url), Mode = mode });
        await Assert.ThrowsAsync<ArgumentException>(() => client.StartAhpHostAsync(new() { LocalServer = new() }));
        await Assert.ThrowsAsync<ArgumentException>(() => client.StartAhpHostAsync(new()
        {
            Authentication = new AhpHostAuthentication.Custom(" ", (_, _) => Task.FromResult<AhpAuthenticationResult?>(null)),
            LocalServer = new()
        }));
        foreach (var policy in new AhpHostAuthentication[]
        {
            new AhpHostAuthentication.None(),
            new AhpHostAuthentication.Custom("owner", (_, _) => Task.FromResult<AhpAuthenticationResult?>(null))
        })
            await Assert.ThrowsAsync<ArgumentException>(() => client.StartAhpHostAsync(new()
            {
                Authentication = policy,
                GitHubEnvironment = new() { Name = "host", ComputeId = "compute" }
            }));
        Assert.Empty(server.Requests);
    }

    [Theory]
    [InlineData("github", CopilotClientMode.CopilotCli)]
    [InlineData("none", CopilotClientMode.CopilotCli)]
    [InlineData("custom", CopilotClientMode.CopilotCli)]
    [InlineData("github", CopilotClientMode.Empty)]
    [InlineData("none", CopilotClientMode.Empty)]
    [InlineData("custom", CopilotClientMode.Empty)]
    public async Task Ahp_Serializes_Only_Admission_Policy_And_Preserves_Listener_Secret(string mode, CopilotClientMode clientMode)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new() { Connection = RuntimeConnection.ForUri(server.Url), Mode = clientMode });
        AhpHostAuthentication policy = mode switch
        {
            "github" => new AhpHostAuthentication.GitHub(),
            "none" => new AhpHostAuthentication.None(),
            _ => new AhpHostAuthentication.Custom("stable-owner", (_, _) => throw new InvalidOperationException("Never serialize or invoke"))
        };
        await using var host = await client.StartAhpHostAsync(new()
        {
            Authentication = policy,
            LocalServer = new() { Token = "listener-secret", RequireConnectionToken = true }
        });
        var request = Assert.Single(server.Requests, request => request.Method == "host.start").Params;
        var auth = request.GetProperty("authentication");
        Assert.Equal(mode, auth.GetProperty("mode").GetString());
        Assert.Equal(mode == "custom" ? 2 : 1, auth.EnumerateObject().Count());
        if (mode == "custom") Assert.Equal("stable-owner", auth.GetProperty("requireIdentity").GetString());
        Assert.Equal("listener-secret", request.GetProperty("localServer").GetProperty("token").GetString());
        Assert.True(request.GetProperty("localServer").GetProperty("requireConnectionToken").GetBoolean());
    }

    [Theory]
    [InlineData("valid")]
    [InlineData("null")]
    [InlineData("throw")]
    [InlineData("mismatch")]
    [InlineData("malformed")]
    [InlineData("missing")]
    [InlineData("expired")]
    [InlineData("timezone-missing")]
    public async Task Ahp_Authentication_Validates_Decrypted_Token_And_Fails_Closed(string outcome)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new() { Connection = RuntimeConnection.ForUri(server.Url) });
        var expires = DateTimeOffset.UtcNow.AddMinutes(5).ToString("O");
        var completed = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        await using var host = await client.StartAhpHostAsync(new()
        {
            LocalServer = new(),
            Authentication = new AhpHostAuthentication.Custom("owner", (token, cancellation) =>
            {
                Assert.Equal("decrypted-admission-secret", token);
                Assert.False(cancellation.IsCancellationRequested);
                cancellation.Register(() => completed.TrySetResult());
                if (outcome == "throw") throw new InvalidOperationException("decrypted-admission-secret");
                return Task.FromResult(outcome == "null" ? null : new AhpAuthenticationResult(
                    outcome == "mismatch" ? "different-owner" : "owner",
                    outcome switch
                    {
                        "malformed" => "not-a-date",
                        "missing" => null!,
                        "expired" => "2000-01-01T00:00:00Z",
                        "timezone-missing" => "2099-01-01T00:00:00",
                        _ => expires
                    }));
            })
        });
        var pending = server.SendRequestAsync("host.validateAuthentication", AuthenticationRequest(host, "attempt"));
        if (outcome is "valid" or "null")
        {
            var result = await pending.WaitAsync(TimeSpan.FromSeconds(5));
            if (outcome == "null") Assert.Equal(JsonValueKind.Null, result.ValueKind);
            else
            {
                Assert.Equal("owner", result.GetProperty("identity").GetString());
                Assert.Equal(expires, result.GetProperty("expiresAt").GetString());
                Assert.DoesNotContain("decrypted-admission-secret", result.GetRawText());
            }
        }
        else
        {
            var error = await Assert.ThrowsAsync<InvalidOperationException>(() => pending.WaitAsync(TimeSpan.FromSeconds(5)));
            Assert.DoesNotContain("decrypted-admission-secret", error.Message);
        }
        await completed.Task.WaitAsync(TimeSpan.FromSeconds(5));
        Assert.DoesNotContain(server.Requests, request => request.Method.StartsWith("session.", StringComparison.Ordinal));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Ahp_Authentication_Cancellation_And_Disconnect_Fence_Late_Results(bool disconnect)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new() { Connection = RuntimeConnection.ForUri(server.Url) });
        var entered = new TaskCompletionSource<CancellationToken>(TaskCreationOptions.RunContinuationsAsynchronously);
        var cancelled = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var validation = new TaskCompletionSource<AhpAuthenticationResult?>(TaskCreationOptions.RunContinuationsAsynchronously);
        var host = await client.StartAhpHostAsync(new()
        {
            LocalServer = new(),
            Authentication = new AhpHostAuthentication.Custom("owner", (_, token) =>
            {
                token.Register(() => cancelled.TrySetResult());
                entered.TrySetResult(token);
                return validation.Task;
            })
        });
        var pending = server.SendRequestAsync("host.validateAuthentication", AuthenticationRequest(host, "pending"));
        var token = await entered.Task.WaitAsync(TimeSpan.FromSeconds(5));
        await Assert.ThrowsAsync<InvalidOperationException>(() =>
            server.SendRequestAsync("host.validateAuthentication", AuthenticationRequest(host, "pending")).WaitAsync(TimeSpan.FromSeconds(5)));
        await server.SendRequestAsync("host.authenticationCancelled", new() { ["hostId"] = "another-host", ["requestId"] = "pending" });
        Assert.False(token.IsCancellationRequested);
        if (disconnect) await client.StopAsync();
        else await server.SendRequestAsync("host.authenticationCancelled", new() { ["hostId"] = host.HostId, ["requestId"] = "pending" });
        await cancelled.Task.WaitAsync(TimeSpan.FromSeconds(5));
        Assert.True(token.IsCancellationRequested);
        if (!disconnect)
            await Assert.ThrowsAsync<InvalidOperationException>(() => pending.WaitAsync(TimeSpan.FromSeconds(5)));
        validation.TrySetResult(new("owner", DateTimeOffset.UtcNow.AddMinutes(5).ToString("O")));
        if (!disconnect)
        {
            // A completed late result cannot resurrect or retain the retired attempt.
            await server.SendRequestAsync("host.authenticationCancelled", new() { ["hostId"] = host.HostId, ["requestId"] = "pending" });
            await host.DisposeAsync();
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Ahp_Passes_Trusted_Identity_Outside_Create_And_Resume_Config(bool resume)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new() { Connection = RuntimeConnection.ForUri(server.Url) });
        CopilotSession? original = null;
        HostSessionAuthentication? authentication = null;
        await using var host = await client.StartAhpHostAsync(new()
        {
            LocalServer = new(),
            Authentication = new AhpHostAuthentication.Custom("owner", (_, _) => Task.FromResult<AhpAuthenticationResult?>(null)),
            CreateSession = async request =>
            {
                authentication = request.Authentication;
                return original = await client.CreateSessionAsync(request.Config);
            },
            ResumeSession = async request =>
            {
                authentication = request.Authentication;
                return original = await client.ResumeSessionAsync(request.SessionId, request.Config);
            }
        });
        var result = await server.SendRequestAsync("host.materializeSession", new()
        {
            ["hostId"] = host.HostId,
            ["handoffId"] = "trusted",
            ["resume"] = resume,
            ["authentication"] = new Dictionary<string, object?> { ["mode"] = "custom", ["identity"] = "owner" },
            ["config"] = new Dictionary<string, object?> { ["sessionId"] = "app-owned" }
        });
        Assert.Equal("app-owned", result.GetProperty("sessionId").GetString());
        Assert.Equal("owner", Assert.IsType<HostSessionAuthenticationCustom>(authentication).Identity);
        var config = Assert.Single(server.Requests, request => request.Method == (resume ? "session.resume" : "session.create")).Params;
        Assert.False(config.TryGetProperty("authentication", out _));
        Assert.False(config.TryGetProperty("gitHubToken", out _));
        await original!.DisposeAsync();
    }

    [Theory]
    [InlineData("github", "none", null)]
    [InlineData("github", "github", " ")]
    [InlineData("none", "custom", "owner")]
    [InlineData("custom", "custom", "other")]
    [InlineData("custom", "github", "owner")]
    public async Task Ahp_Rejects_Trusted_Authentication_That_Does_Not_Match_Host_Policy(string policy, string mode, string? identity)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new() { Connection = RuntimeConnection.ForUri(server.Url) });
        var invoked = false;
        await using var host = await client.StartAhpHostAsync(new()
        {
            LocalServer = new(),
            Authentication = policy switch
            {
                "github" => new AhpHostAuthentication.GitHub(),
                "none" => new AhpHostAuthentication.None(),
                _ => new AhpHostAuthentication.Custom("owner", (_, _) => Task.FromResult<AhpAuthenticationResult?>(null))
            },
            CreateSession = request =>
            {
                invoked = true;
                return client.CreateSessionAsync(request.Config);
            }
        });
        var authentication = new Dictionary<string, object?> { ["mode"] = mode };
        if (identity is not null) authentication["identity"] = identity;
        var error = await Assert.ThrowsAsync<InvalidOperationException>(() => server.SendRequestAsync("host.materializeSession", new()
        {
            ["hostId"] = host.HostId,
            ["handoffId"] = "mismatch",
            ["authentication"] = authentication,
            ["config"] = new Dictionary<string, object?> { ["sessionId"] = "requested" }
        }));
        Assert.Contains("does not match host policy", error.Message);
        Assert.False(invoked);
    }

    private static Dictionary<string, object?> AuthenticationRequest(AhpHost host, string requestId) => new()
    {
        ["hostId"] = host.HostId,
        ["requestId"] = requestId,
        ["token"] = "decrypted-admission-secret"
    };
}
#endif

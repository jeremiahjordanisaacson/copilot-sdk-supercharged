/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using System.Text.Json;
using GitHub.Copilot.Rpc;
using GitHub.Copilot.Test.Harness;
using Xunit;
using Xunit.Abstractions;

namespace GitHub.Copilot.Test.E2E;

public sealed class HostAuthenticationE2ETests(E2ETestFixture fixture, ITestOutputHelper output)
    : E2ETestBase(fixture, "host_authentication", output, replayOnly: true)
{
    private sealed class HostAuthenticationTheoryAttribute : TheoryAttribute
    {
        public HostAuthenticationTheoryAttribute()
        {
            if (Environment.GetEnvironmentVariable("COPILOT_RUNTIME_HOST_E2E") != "1")
                Skip = "Requires an integrated runtime; set COPILOT_RUNTIME_HOST_E2E=1";
        }
    }

    private CopilotClient CreateOwner()
    {
        var environment = Ctx.GetEnvironment();
        foreach (var key in new[] { "GH_TOKEN", "GITHUB_TOKEN", "COPILOT_GITHUB_TOKEN", "COPILOT_HMAC_KEY", "CAPI_HMAC_KEY" })
            environment[key] = "";
        return Ctx.CreateClient(new() { UseLoggedInUser = false }, autoInjectGitHubToken: false, environment: environment);
    }

    private static AhpHostAuthentication.Custom Custom(
        Func<string, CancellationToken, Task<AhpAuthenticationResult?>> validator) => new("application-owner", validator);

    private static Task<JsonElement> ConnectAsync(AhpTestClient ahp, AhpHost host) =>
        ahp.RequestAsync(new() { ["op"] = "connect", ["url"] = host.Url, ["token"] = host.Token });

    [HostAuthenticationTheory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Custom_And_None_Create_And_Cold_Resume_Byok_Without_GitHub(bool custom)
    {
        await using var ahp = await AhpTestClient.StartAsync(authentication: true);
        var endpoint = await ahp.RequestAsync(new() { ["op"] = "provider", ["responseText"] = "DOTNET_HOST_BYOK_OK" });
        var provider = new ProviderConfig
        {
            Type = "openai",
            WireApi = "completions",
            BaseUrl = endpoint.GetProperty("url").GetString()!,
            ApiKey = "inference-only-key"
        };
        AhpHostAuthentication policy = custom
            ? Custom((token, cancellation) =>
            {
                Assert.False(cancellation.IsCancellationRequested);
                Assert.Equal("admission-only-key", token);
                return Task.FromResult<AhpAuthenticationResult?>(new("application-owner", DateTimeOffset.UtcNow.AddMinutes(5).ToString("O")));
            })
            : new AhpHostAuthentication.None();
        void AssertAdmission(HostSessionAuthentication authentication)
        {
            if (custom) Assert.Equal("application-owner", Assert.IsType<HostSessionAuthenticationCustom>(authentication).Identity);
            else Assert.IsType<HostSessionAuthenticationNone>(authentication);
        }
        var owner = CreateOwner();
        CopilotSession? original = null;
        var creates = 0;
        string uri;
        await using (var host = await owner.StartAhpHostAsync(new()
        {
            Authentication = policy,
            LocalServer = new() { Token = "independent-listener-secret" },
            AllowCreatingNewSessions = true,
            CreateSession = async request =>
            {
                creates++;
                AssertAdmission(request.Authentication);
                request.Config.Provider = provider;
                request.Config.Model = "local-model";
                request.Config.OnPermissionRequest = PermissionHandler.ApproveAll;
                return original = await Ctx.CreateSessionAsync(owner, request.Config);
            }
        }))
        {
            Assert.Equal("independent-listener-secret", host.Token);
            await ConnectAsync(ahp, host);
            var authentication = await ahp.RequestAsync(new() { ["op"] = "authenticate", ["token"] = "admission-only-key" });
            Assert.Equal(custom ? 1 : 0, authentication.GetProperty("resources").GetArrayLength());
            var created = await ahp.RequestAsync(new() { ["op"] = "create", ["workDir"] = Ctx.WorkDir });
            uri = created.GetProperty("uri").GetString()!;
            Assert.Equal(1, creates);
            Assert.Equal("DOTNET_HOST_BYOK_OK", (await ahp.RequestAsync(new() { ["op"] = "turn" })).GetProperty("text").GetString());
            await host.DisposeAsync();
            await ahp.RequestAsync(new() { ["op"] = "close" });
        }
        await original!.DisposeAsync();
        await owner.StopAsync();

        var replacement = CreateOwner();
        CopilotSession? resumed = null;
        var resumes = 0;
        await using (var host = await replacement.StartAhpHostAsync(new()
        {
            Authentication = policy,
            LocalServer = new(),
            ResumeSession = async request =>
            {
                resumes++;
                AssertAdmission(request.Authentication);
                Assert.Equal(original!.SessionId, request.SessionId);
                request.Config.Provider = provider;
                request.Config.Model = "local-model";
                request.Config.OnPermissionRequest = PermissionHandler.ApproveAll;
                return resumed = await Ctx.ResumeSessionAsync(replacement, request.SessionId, request.Config);
            }
        }))
        {
            await ConnectAsync(ahp, host);
            await ahp.RequestAsync(new() { ["op"] = "authenticate", ["token"] = "admission-only-key" });
            await ahp.RequestAsync(new() { ["op"] = "attach", ["uri"] = uri });
            Assert.Equal(1, resumes);
            Assert.Equal("DOTNET_HOST_BYOK_OK", (await ahp.RequestAsync(new() { ["op"] = "turn" })).GetProperty("text").GetString());
            var requests = (await ahp.RequestAsync(new() { ["op"] = "requests" })).EnumerateArray().ToArray();
            Assert.True(requests.Length >= 2);
            foreach (var request in requests)
            {
                Assert.Equal("Bearer inference-only-key", request.GetProperty("authorization").GetString());
                Assert.DoesNotContain("admission-only-key", request.GetProperty("body").GetString());
            }
            await host.DisposeAsync();
            await ahp.RequestAsync(new() { ["op"] = "close" });
        }
        await resumed!.DisposeAsync();
    }

    [HostAuthenticationTheory]
    [InlineData("null")]
    [InlineData("throw")]
    [InlineData("mismatch")]
    [InlineData("malformed")]
    [InlineData("expired")]
    public async Task Rejected_Custom_Admission_Never_Invokes_Create(string failure)
    {
        var owner = CreateOwner();
        var creates = 0;
        var validations = 0;
        string? observedToken = null;
        bool? observedCancellation = null;
        await using var host = await owner.StartAhpHostAsync(new()
        {
            LocalServer = new(),
            Authentication = Custom((token, cancellation) =>
            {
                validations++;
                observedToken = token;
                observedCancellation = cancellation.IsCancellationRequested;
                if (failure == "throw") throw new InvalidOperationException("validator unavailable");
                return Task.FromResult(failure == "null" ? null : new AhpAuthenticationResult(
                    failure == "mismatch" ? "another-owner" : "application-owner",
                    failure == "expired" ? "2000-01-01T00:00:00Z" : failure == "malformed" ? "not-a-date" : DateTimeOffset.UtcNow.AddMinutes(5).ToString("O")));
            }),
            CreateSession = _ =>
            {
                Interlocked.Increment(ref creates);
                throw new InvalidOperationException("Rejected admission cannot create sessions");
            }
        });
        await using var ahp = await AhpTestClient.StartAsync(authentication: true);
        await ConnectAsync(ahp, host);
        await Assert.ThrowsAsync<InvalidOperationException>(() => ahp.RequestAsync(new() { ["op"] = "authenticate", ["token"] = "decrypted-admission" }));
        await Assert.ThrowsAsync<InvalidOperationException>(() => ahp.RequestAsync(new() { ["op"] = "create", ["workDir"] = Ctx.WorkDir }));
        Assert.Equal(1, validations);
        Assert.Equal(0, creates);
        Assert.Equal("decrypted-admission", observedToken);
        Assert.False(observedCancellation);
    }

    [HostAuthenticationTheory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Custom_Admission_Expiry_Or_Host_Disposal_Cancels_Credentials(bool dispose)
    {
        var owner = CreateOwner();
        var entered = new TaskCompletionSource<CancellationToken>(TaskCreationOptions.RunContinuationsAsynchronously);
        var cancelled = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var late = new TaskCompletionSource<AhpAuthenticationResult?>(TaskCreationOptions.RunContinuationsAsynchronously);
        var validations = 0;
        await using var host = await owner.StartAhpHostAsync(new()
        {
            LocalServer = new(),
            Authentication = Custom((_, cancellation) =>
            {
                validations++;
                cancellation.Register(() => cancelled.TrySetResult());
                entered.TrySetResult(cancellation);
                return dispose ? late.Task : Task.FromResult<AhpAuthenticationResult?>(new(
                    "application-owner", DateTimeOffset.UtcNow.AddSeconds(validations == 1 ? 2 : 60).ToString("O")));
            })
        });
        await using var ahp = await AhpTestClient.StartAsync(authentication: true);
        await ConnectAsync(ahp, host);
        var authentication = ahp.RequestAsync(new() { ["op"] = "authenticate", ["token"] = "admission" });
        await entered.Task.WaitAsync(TimeSpan.FromSeconds(10));
        if (dispose)
        {
            await host.DisposeAsync();
            await cancelled.Task.WaitAsync(TimeSpan.FromSeconds(10));
            await Assert.ThrowsAsync<InvalidOperationException>(() => authentication);
            late.TrySetResult(new("application-owner", DateTimeOffset.UtcNow.AddMinutes(1).ToString("O")));
        }
        else
        {
            await authentication;
            await ahp.RequestAsync(new() { ["op"] = "expired" });
            await Assert.ThrowsAsync<InvalidOperationException>(() => ahp.RequestAsync(new() { ["op"] = "create", ["workDir"] = Ctx.WorkDir }));
            await ahp.RequestAsync(new() { ["op"] = "authenticate", ["token"] = "renewed" });
            Assert.Equal(2, validations);
        }
    }
}
#endif

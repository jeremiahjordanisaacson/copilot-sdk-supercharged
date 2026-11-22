/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.ExceptionServices;
using Microsoft.Copilot.Test.Harness;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public async Task Session_Event_Reader_Does_Not_Retain_Starting_Context(bool resume, bool suppressFlow)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });
        await client.StartAsync();
        var ambient = new AsyncLocal<object?>();
        var (session, capture) = await RunWithCallerContextAsync(
            () => resume
                ? client.ResumeSessionAsync("context-test", new ResumeSessionOptions())
                : client.CreateSessionAsync(new CreateSessionOptions()),
            ambient, suppressFlow);
        await using var retainedSession = session;

        ForceCollect();
        Assert.False(CaptureIsAlive(capture), "The idle session reader retained its starting caller's AsyncLocal value.");

        var observed = new TaskCompletionSource<object?>(TaskCreationOptions.RunContinuationsAsynchronously);
        using var subscription = session.On<SessionIdleEvent>(_ => observed.TrySetResult(ambient.Value));
        await server.SendAndDrainSessionEventAsync(session, "session.idle", new(), TimeSpan.FromSeconds(5));
        Assert.Null(await observed.Task.WaitAsync(TimeSpan.FromSeconds(5)));
        GC.KeepAlive(session);
    }

    [Fact]
    public async Task Queued_First_Event_Preserves_Handler_Context()
    {
        var timeout = TimeSpan.FromSeconds(5);
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });
        await using var resident = await client.CreateSessionAsync(new CreateSessionOptions
        {
            SessionId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
        });
        server.ResumedSessionId = resident.SessionId;
        var ambient = new AsyncLocal<object?>();
        using var callerActivity = new Activity("reader-caller");
        using var releaseReader = new ManualResetEventSlim();
        var readerStarted = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var resumeSent = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var handlerStarted = new TaskCompletionSource<Task>(TaskCreationOptions.RunContinuationsAsynchronously);
        var scheduler = new ReaderStartupScheduler(() =>
        {
            // Hold the initial continuation so the channel has an event before consumption starts.
            readerStarted.TrySetResult();
            releaseReader.Wait();
        });
        server.BeforeResponseAsync = (request, _) =>
        {
            if (request.Method == "session.resume")
            {
                resumeSent.TrySetResult();
            }
            return Task.CompletedTask;
        };

        async Task ObserveHandlerContextAsync()
        {
            var inherited = ambient.Value;
            var inheritedActivity = Activity.Current;
            var suppressed = ExecutionContext.IsFlowSuppressed();
            var expected = new object();
            ambient.Value = expected;
            using var activity = new Activity("event-handler").Start();
            await Task.Yield();
            Assert.Same(expected, ambient.Value);
            Assert.Same(activity, Activity.Current);
            Assert.Null(inherited);
            Assert.Null(inheritedActivity);
            Assert.False(suppressed);
            Assert.Same(scheduler, TaskScheduler.Current);
        }

        Task<CopilotSession>? resuming = null;
        Exception? bodyFailure = null;
        try
        {
            resuming = Task.Factory.StartNew(async () =>
            {
                callerActivity.Start();
                ambient.Value = new object();
                return await client.ResumeSessionAsync(resident.SessionId.ToUpperInvariant(), new ResumeSessionOptions
                {
                    Events = new SessionEventOptions
                    {
                        OnEvent = evt =>
                        {
                            if (evt is SessionIdleEvent)
                            {
                                handlerStarted.TrySetResult(ObserveHandlerContextAsync());
                            }
                        },
                    },
                });
            }, CancellationToken.None, TaskCreationOptions.None, scheduler).Unwrap();
            await Task.WhenAny(readerStarted.Task, resumeSent.Task, resuming).WaitAsync(timeout);
            if (resuming.IsFaulted || resuming.IsCanceled)
            {
                await resuming;
            }
            Assert.True(readerStarted.Task.IsCompleted, "The session event reader must yield before resume sends its RPC.");
            await server.SendSessionEventAsync(resident.SessionId, "session.idle", new());
            // The later ping response proves the read loop has enqueued the event before we release the reader.
            await client.PingAsync().WaitAsync(timeout);
            releaseReader.Set();
            await resuming.WaitAsync(timeout);
            await (await handlerStarted.Task.WaitAsync(timeout)).WaitAsync(timeout);
        }
        catch (Exception error)
        {
            bodyFailure = error;
        }
        finally
        {
            releaseReader.Set();
            try
            {
                if (resuming is not null)
                {
                    await using var resumed = await resuming.WaitAsync(timeout);
                }
            }
            catch (Exception cleanupError)
            {
                bodyFailure = bodyFailure is null ? cleanupError : new AggregateException(bodyFailure, cleanupError);
            }
        }
        if (bodyFailure is not null)
        {
            ExceptionDispatchInfo.Capture(bodyFailure).Throw();
        }
    }

    private sealed class ReaderStartupScheduler(Action readerScheduled) : TaskScheduler
    {
        private int _paused;

        protected override void QueueTask(Task task)
        {
            if (ExecutionContext.IsFlowSuppressed() && Interlocked.CompareExchange(ref _paused, 1, 0) == 0)
            {
                readerScheduled();
            }
            ThreadPool.UnsafeQueueUserWorkItem(_ => TryExecuteTask(task), null);
        }

        protected override bool TryExecuteTaskInline(Task task, bool taskWasPreviouslyQueued) => false;

        protected override IEnumerable<Task> GetScheduledTasks() => Array.Empty<Task>();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Connection_Read_Lifetime_Does_Not_Retain_Starting_Context(bool suppressFlow)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });
        var ambient = new AsyncLocal<object?>();
        var capture = await StartWithCallerContextAsync(client, ambient, suppressFlow);

        ForceCollect();
        Assert.False(CaptureIsAlive(capture), "The open connection retained its starting caller's AsyncLocal value.");

        object? observed = new object();
        await using var session = await client.CreateSessionAsync(new CreateSessionOptions
        {
            Hooks = new SessionHookOptions
            {
                Handlers = new SessionHooks
                {
                    OnPreToolUse = (_, _) =>
                    {
                        observed = ambient.Value;
                        return Task.FromResult<PreToolUseHookOutput?>(null);
                    },
                },
            },
        });
        await server.SendRequestAsync("hooks.invoke", new()
        {
            ["sessionId"] = session.SessionId,
            ["hookType"] = "preToolUse",
            ["input"] = new Dictionary<string, object?>
            {
                ["sessionId"] = session.SessionId,
                ["timestamp"] = 1_730_000_000_000L,
                ["cwd"] = "workdir",
                ["toolName"] = "view",
                ["toolArgs"] = new Dictionary<string, object?> { ["path"] = "file.txt" },
            },
        }).WaitAsync(TimeSpan.FromSeconds(5));
        Assert.Null(observed);
        GC.KeepAlive(client);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Stdio_Readers_Do_Not_Retain_Starting_Context(bool suppressFlow)
    {
        var directory = Path.Combine(Path.GetTempPath(), $"copilot-reader-context-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);
        try
        {
            var script = Path.Combine(E2ETestBase.FindTestHarnessDir(), "stdio-shutdown-runtime.cjs");
            await using var client = new CopilotClient(new CopilotClientOptions
            {
                Mode = CopilotClientMode.CodingAgent,
                Runtime = RuntimeConnection.ForSpawnedStdio(path: "node", args:
                    [script, Path.Combine(directory, "cleanup"), "stop", Path.Combine(directory, "runtime.pid")]),
                GitHub = new ClientGitHubOptions { UseLoggedInUser = false },
            });
            var capture = await StartWithCallerContextAsync(client, new AsyncLocal<object?>(), suppressFlow);

            ForceCollect();
            Assert.False(CaptureIsAlive(capture), "The stdio readers retained their starting caller's AsyncLocal value.");
            var response = await client.PingAsync("still-open").WaitAsync(TimeSpan.FromSeconds(5));
            Assert.Equal("still-open", response.Message);
            await client.StopAsync().WaitAsync(TimeSpan.FromSeconds(5));
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    private static async Task<WeakReference<object>> StartWithCallerContextAsync(
        CopilotClient client, AsyncLocal<object?> ambient, bool suppressFlow)
    {
        var (_, capture) = await RunWithCallerContextAsync(async () =>
        {
            await client.StartAsync();
            return true;
        }, ambient, suppressFlow);
        return capture;
    }

    private static async Task<(T Result, WeakReference<object> Capture)> RunWithCallerContextAsync<T>(
        Func<Task<T>> action, AsyncLocal<object?> ambient, bool suppressFlow)
    {
        var scheduler = new ConcurrentExclusiveSchedulerPair(TaskScheduler.Default, maxConcurrencyLevel: 1);
        var factory = new TaskFactory(CancellationToken.None, TaskCreationOptions.None, TaskContinuationOptions.None, scheduler.ExclusiveScheduler);
        var result = await factory.StartNew(() => InvokeWithCallerContextAsync(action, ambient, suppressFlow)).Unwrap();
        // Let all starting frames unwind before checking what the background work retains.
        await factory.StartNew(static () => { });
        return result;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static async Task<(T Result, WeakReference<object> Capture)> InvokeWithCallerContextAsync<T>(
        Func<Task<T>> action, AsyncLocal<object?> ambient, bool suppressFlow)
    {
        ambient.Value = new object();
        var capture = new WeakReference<object>(ambient.Value);
        Task<T> operation;
        using (suppressFlow ? ExecutionContext.SuppressFlow() : default(AsyncFlowControl))
        {
            operation = action();
            Assert.True(capture.TryGetTarget(out var owner));
            Assert.Same(owner, ambient.Value);
            Assert.Equal(suppressFlow, ExecutionContext.IsFlowSuppressed());
        }
        var result = await operation;
        Assert.NotNull(ambient.Value);
        Assert.False(ExecutionContext.IsFlowSuppressed());
        return (result, capture);
    }
}
#endif

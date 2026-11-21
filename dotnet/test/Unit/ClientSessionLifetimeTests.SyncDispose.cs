/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET
using System.Collections.Concurrent;
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

public sealed partial class ClientSessionLifetimeTests
{
    // Callers: none = thread pool with no SynchronizationContext; pool-context = a context that posts to
    // the thread pool; single-thread-context = a UI-style context that runs posts on the blocked thread;
    // exclusive-scheduler = a task scheduler that runs one task at a time.
    // States: what Dispose has to stop, and so which requests it must send.
    [Theory]
    [InlineData("none", "never-started")]
    [InlineData("none", "session")]
    [InlineData("none", "runtime")]
    [InlineData("none", "session-and-runtime")]
    [InlineData("pool-context", "never-started")]
    [InlineData("pool-context", "session")]
    [InlineData("pool-context", "runtime")]
    [InlineData("pool-context", "session-and-runtime")]
    [InlineData("single-thread-context", "never-started")]
    [InlineData("single-thread-context", "session")]
    [InlineData("single-thread-context", "runtime")]
    [InlineData("single-thread-context", "session-and-runtime")]
    [InlineData("exclusive-scheduler", "never-started")]
    [InlineData("exclusive-scheduler", "session")]
    [InlineData("exclusive-scheduler", "runtime")]
    [InlineData("exclusive-scheduler", "session-and-runtime")]
    public async Task Sync_Dispose_Stops_The_Client_On_Any_Caller_Scheduler(string caller, string state)
    {
        await using var server = await FakeCopilotServer.StartAsync();
        var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForTcp(server.Host, server.Port) });
        using var process = StartExitedProcess();
        var hasSession = state is "session" or "session-and-runtime";
        var ownsRuntime = state is "runtime" or "session-and-runtime";
        if (state != "never-started")
        {
            await client.StartAsync();
        }
        if (hasSession)
        {
            await client.CreateSessionAsync(new CreateSessionOptions());
        }
        if (ownsRuntime)
        {
            await ReplaceConnectionResourcesAsync(client, process);
        }

        await RunOnCallerAsync(caller, client.Dispose).WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Throws<ObjectDisposedException>(() => client.Rpc);
        Assert.Equal(hasSession ? 1 : 0, server.Requests.Count(request => request.Method == "session.detach"));
        Assert.Equal(ownsRuntime ? 1 : 0, server.RuntimeShutdownCount);
        AssertSessionCount(client, sessions: 0);
    }

    private static Task RunOnCallerAsync(string caller, Action action)
    {
        switch (caller)
        {
            case "none":
                return Task.Run(action);
            case "pool-context":
                return Task.Run(() =>
                {
                    SynchronizationContext.SetSynchronizationContext(new ThreadPoolSynchronizationContext());
                    try
                    {
                        action();
                    }
                    finally
                    {
                        SynchronizationContext.SetSynchronizationContext(null);
                    }
                });
            case "single-thread-context":
                return SingleThreadSynchronizationContext.Run(action);
            case "exclusive-scheduler":
                var exclusive = new ConcurrentExclusiveSchedulerPair(TaskScheduler.Default, maxConcurrencyLevel: 1).ExclusiveScheduler;
                return Task.Factory.StartNew(action, CancellationToken.None, TaskCreationOptions.None, exclusive);
            default:
                throw new ArgumentOutOfRangeException(nameof(caller), caller, null);
        }
    }

    private sealed class ThreadPoolSynchronizationContext : SynchronizationContext
    {
        public override void Post(SendOrPostCallback d, object? state) => ThreadPool.QueueUserWorkItem(_ => d(state));
    }

    /// <summary>Runs posted callbacks one at a time on one dedicated thread, like a WinForms or WPF UI thread.</summary>
    private sealed class SingleThreadSynchronizationContext : SynchronizationContext
    {
        private readonly BlockingCollection<(SendOrPostCallback Callback, object? State)> _queue = new();

        public static Task Run(Action action)
        {
            var context = new SingleThreadSynchronizationContext();
            var completion = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
            context.Post(_ =>
            {
                try
                {
                    action();
                    completion.SetResult();
                }
                catch (Exception ex)
                {
                    completion.SetException(ex);
                }
                finally
                {
                    context._queue.CompleteAdding();
                }
            }, null);
            new Thread(context.Pump) { IsBackground = true, Name = "single-thread-context" }.Start();
            return completion.Task;
        }

        public override void Post(SendOrPostCallback d, object? state)
        {
            try
            {
                _queue.Add((d, state));
            }
            catch (InvalidOperationException)
            {
                // The UI thread has finished; a late post must still run.
                ThreadPool.QueueUserWorkItem(_ => d(state));
            }
        }

        private void Pump()
        {
            SetSynchronizationContext(this);
            foreach (var (callback, state) in _queue.GetConsumingEnumerable())
            {
                callback(state);
            }
        }
    }
}
#endif

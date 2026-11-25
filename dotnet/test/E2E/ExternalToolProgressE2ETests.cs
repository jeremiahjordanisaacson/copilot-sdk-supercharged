/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma warning disable MCPSDK001 // Tool progress is intentionally experimental.

using Microsoft.Copilot.Test.Harness;
using Microsoft.Extensions.AI;
using System.Text.Json;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.Copilot.Test.E2E;

public class ExternalToolProgressE2ETests(E2ETestFixture fixture, ITestOutputHelper output) : E2ETestBase(fixture, "external_tool_progress", output)
{
    [Fact]
    public async Task Canceled_Progress_Report_Throws_To_The_Handler()
    {
        var tool = CopilotTool.DefineTool(
            async (ToolInvocation invocation) =>
            {
                await Assert.ThrowsAnyAsync<OperationCanceledException>(
                    () => invocation.ReportProgressAsync("canceled report", new CancellationToken(canceled: true)));
                await invocation.ReportProgressAsync("working");
                return "PROGRESS_PROBE_DONE";
            },
            factoryOptions: new AIFunctionFactoryOptions { Name = "progress_probe", Description = "Reports progress" });
        var session = await CreateSessionAsync(new CreateSessionOptions
        { Tools = new SessionToolOptions { Definitions = [tool] }, Permissions = new SessionPermissionOptions { OnRequest = PermissionHandler.ApproveAll } });
        var working = new TaskCompletionSource<ToolExecutionProgressEvent>(TaskCreationOptions.RunContinuationsAsynchronously);
        using var subscription = session.On<SessionEvent>(evt =>
        {
            if (evt is ToolExecutionProgressEvent progress && progress.Data.ProgressMessage == "working")
            {
                working.TrySetResult(progress);
            }
        });

        await session.Rpc.Tools.InitializeAndValidateAsync();
        var result = await session.Rpc.Tools.ExecuteAsync("progress_probe", JsonElement.Parse("{}"), "progress-call");

        Assert.Equal("success", result.GetProperty("resultType").GetString());
        Assert.Equal("PROGRESS_PROBE_DONE", result.GetProperty("textResultForLlm").GetString());
        var reported = await working.Task.WaitAsync(TimeSpan.FromSeconds(30));
        Assert.Equal("progress-call", reported.Data.ToolCallId);
    }
}

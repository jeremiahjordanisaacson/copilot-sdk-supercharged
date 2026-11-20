/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#if NET8_0_OR_GREATER
using Xunit;

namespace Microsoft.Copilot.Test.Unit;

// Building the netstandard2.0 SDK keeps every core of a small CI runner busy for up to a minute and a
// half, and tests running beside it miss their startup deadlines. xUnit runs a collection that disables
// parallelization only after the parallel collections finish.
[CollectionDefinition(Name, DisableParallelization = true)]
public sealed class DownlevelSdkBuildCollection
{
    public const string Name = "Downlevel SDK build";
}

[Collection(DownlevelSdkBuildCollection.Name)]
public sealed class DownlevelSdkTests
{
    [Fact]
    public Task Stop_Cancels_Pending_Startup_Before_Queued_Work_Runs_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(ClientSessionLifetimeTests),
            nameof(ClientSessionLifetimeTests.Stop_Cancels_Pending_Startup_Before_Queued_Work_Runs),
            []);

    [Fact]
    public Task Stop_Marks_Connection_Closed_Before_Queued_Work_Runs_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(ClientSessionLifetimeTests),
            nameof(ClientSessionLifetimeTests.Stop_Marks_Connection_Closed_Before_Queued_Work_Runs),
            []);

    [Fact]
    public Task StopAsync_Rejects_External_Tools_While_Detach_Is_Pending_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(ClientSessionLifetimeTests),
            nameof(ClientSessionLifetimeTests.StopAsync_Rejects_External_Tools_While_Detach_Is_Pending),
            []);

    [Fact]
    public Task Bundled_Runtime_Uses_Host_Rid_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(RuntimeWrapperTests),
            nameof(RuntimeWrapperTests.Marked_Bundled_Explicit_Cli_Does_Not_Require_Runtime_Pair),
            []);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task Bundled_Runtime_Repairs_Unix_Execute_Bits_With_Netstandard_Assembly(bool repairDenied) =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(RuntimeWrapperTests),
            nameof(RuntimeWrapperTests.Bundled_Runtime_Repairs_Missing_Unix_Execute_Bits),
            [repairDenied]);

    [Fact]
    public Task ForceStop_Terminates_Descendants_Without_Pgrep_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(RuntimeWrapperTests),
            nameof(RuntimeWrapperTests.ForceStop_Terminates_Descendants_Without_Pgrep),
            []);

    [Fact]
    public Task ShellAuthentication_Cancellation_Is_Observable_Before_Callbacks_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(ClientSessionLifetimeTests),
            nameof(ClientSessionLifetimeTests.ShellAuthentication_Stop_Marks_Cancellation_Before_Queued_Work_Runs),
            []);

    [Fact]
    public Task ForceStop_Preserves_Process_Kill_Exception_Type_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(ClientSessionLifetimeTests),
            nameof(ClientSessionLifetimeTests.ForceStopAsync_Preserves_Process_Kill_Exception_Type),
            []);

    [Fact]
    public Task HttpRequest_GetPreservesContentHeaders_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(CopilotRequestHandlerProtocolTests),
            nameof(CopilotRequestHandlerProtocolTests.HttpRequest_GetPreservesContentHeaders),
            []);

    [Fact]
    public Task HttpResponse_Cancels_While_Opening_Stream_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(CopilotRequestHandlerProtocolTests),
            nameof(CopilotRequestHandlerProtocolTests.HttpResponse_RuntimeCancellationWhileOpeningStreamReportsCancellation),
            []);

    [Fact]
    public Task Ahp_Cancellation_Is_Observable_Before_Callbacks_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(ClientSessionLifetimeTests),
            nameof(ClientSessionLifetimeTests.Ahp_Stop_Marks_Cancellation_Before_Queued_Work_Runs),
            []);

    [Fact]
    public Task HttpResponse_Reads_Memory_Only_Stream_With_Netstandard_Assembly() =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(CopilotRequestHandlerProtocolTests),
            nameof(CopilotRequestHandlerProtocolTests.HttpResponse_ReadsAheadAndCoalescesUnderOneOutstandingDataRpc),
            []);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task HttpResponse_Shutdown_Cancellation_Is_Immediate_With_Netstandard_Assembly(bool force) =>
        ClientSessionLifetimeTests.RunWithDownlevelSdkAsync(
            typeof(CopilotRequestHandlerProtocolTests),
            nameof(CopilotRequestHandlerProtocolTests.HttpResponse_ClientShutdownDisposesIdleSource),
            [force]);
}
#endif

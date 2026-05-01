' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.IO
Imports Xunit
Imports GitHub.Copilot.SDK

Namespace GitHub.Copilot.SDK.VB.E2E

    ''' <summary>
    ''' End-to-end tests for the Visual Basic Copilot SDK.
    ''' These tests use the shared replay proxy from test/harness/server.ts
    ''' to verify SDK behaviour against recorded CAPI exchanges.
    ''' </summary>
    Public Class E2ETests
        Implements IAsyncLifetime

        Private _harness As TestHarness
        Private _homeDir As String
        Private _workDir As String

        Public Async Function InitializeAsync() As Task Implements IAsyncLifetime.InitializeAsync
            _harness = New TestHarness()
            Await _harness.StartAsync()

            ' Create isolated temp directories for each test run
            _homeDir = Path.Combine(Path.GetTempPath(), $"copilot-vb-e2e-home-{Guid.NewGuid():N}")
            _workDir = Path.Combine(Path.GetTempPath(), $"copilot-vb-e2e-work-{Guid.NewGuid():N}")
            Directory.CreateDirectory(_homeDir)
            Directory.CreateDirectory(_workDir)
        End Function

        Public Async Function DisposeAsync() As Task Implements IAsyncLifetime.DisposeAsync
            If _harness IsNot Nothing Then
                Await _harness.StopAsync(skipWritingCache:=True)
                _harness.Dispose()
            End If

            ' Clean up temp directories
            Try
                If Directory.Exists(_homeDir) Then Directory.Delete(_homeDir, recursive:=True)
            Catch
            End Try
            Try
                If Directory.Exists(_workDir) Then Directory.Delete(_workDir, recursive:=True)
            Catch
            End Try
        End Function

        Private Async Function StopClientSafe(client As CopilotClient) As Task
            Try
                Await client.StopAsync()
            Catch
                ' Best effort cleanup
            End Try
        End Function

        ''' <summary>
        ''' Verifies that we can create a session and get a valid session ID,
        ''' then disconnect cleanly.
        ''' </summary>
        <Fact>
        Public Async Function SessionCreateAndDisconnect() As Task
            ' Configure proxy for the session-create snapshot
            Dim snapshotPath = _harness.GetSnapshotPath(
                "session", "should_receive_session_events")
            Await _harness.ConfigureAsync(snapshotPath, _workDir)

            ' Build client options with env vars pointing to replay proxy
            Dim env = _harness.GetTestEnvironment(_homeDir)
            Dim options As New CopilotClientOptions() With {
                .Cwd = _workDir,
                .Environment = env
            }

            Dim client As New CopilotClient(options)
            Dim succeeded = False
            Try
                Await client.StartAsync()

                Dim session = Await client.CreateSessionAsync()
                Assert.NotNull(session)
                Assert.False(String.IsNullOrEmpty(session.SessionId),
                             "Session ID should not be empty")

                Await session.DisconnectAsync()
                succeeded = True
            Finally
                ' VB does not allow Await in Finally; use fire-and-forget cleanup
                client.StopAsync().GetAwaiter().GetResult()
            End Try
        End Function

        ''' <summary>
        ''' Verifies that we can send a message and receive a response
        ''' without throwing an exception.
        ''' </summary>
        <Fact>
        Public Async Function SendMessage() As Task
            ' Use the stateful conversation snapshot which has send/response exchanges
            Dim snapshotPath = _harness.GetSnapshotPath(
                "session", "should_have_stateful_conversation")
            Await _harness.ConfigureAsync(snapshotPath, _workDir)

            Dim env = _harness.GetTestEnvironment(_homeDir)
            Dim options As New CopilotClientOptions() With {
                .Cwd = _workDir,
                .Environment = env
            }

            Dim client As New CopilotClient(options)
            Try
                Await client.StartAsync()

                Dim session = Await client.CreateSessionAsync()
                Assert.NotNull(session)
                Assert.False(String.IsNullOrEmpty(session.SessionId))

                ' Send a message and wait for the assistant to respond
                Dim response = Await session.SendAndWaitAsync(
                    New MessageOptions With {.Prompt = "What is 1+1?"},
                    timeout:=TimeSpan.FromSeconds(30))

                ' The replay proxy returns a canned response; just verify we got one
                Assert.NotNull(response)
            Finally
                client.StopAsync().GetAwaiter().GetResult()
            End Try
        End Function

        ''' <summary>
        ''' Verifies that starting a client with SessionFs configuration
        ''' succeeds (the setProvider RPC call completes without error).
        ''' </summary>
        <Fact>
        Public Async Function SessionFsConfig() As Task
            ' Use the session_fs snapshot
            Dim snapshotPath = _harness.GetSnapshotPath(
                "session_fs", "should_use_session_fs_for_session_management")
            Await _harness.ConfigureAsync(snapshotPath, _workDir)

            Dim env = _harness.GetTestEnvironment(_homeDir)
            Dim sessionStatePath = Path.Combine(_workDir, "session-state")

            Dim options As New CopilotClientOptions() With {
                .Cwd = _workDir,
                .Environment = env,
                .SessionFs = New SessionFsConfig() With {
                    .InitialCwd = "/",
                    .SessionStatePath = sessionStatePath,
                    .Conventions = "posix"
                }
            }

            Dim client As New CopilotClient(options)
            Try
                ' StartAsync internally calls sessionFs.setProvider when SessionFs is configured.
                ' If this doesn't throw, the RPC call succeeded.
                Await client.StartAsync()
                Assert.True(True, "Client with SessionFs started successfully")
            Finally
                client.StopAsync().GetAwaiter().GetResult()
            End Try
        End Function

    End Class

End Namespace

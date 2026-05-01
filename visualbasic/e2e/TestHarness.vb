' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Diagnostics
Imports System.IO
Imports System.Net.Http
Imports System.Text
Imports System.Text.Json
Imports System.Text.RegularExpressions

Namespace GitHub.Copilot.SDK.VB.E2E

    ''' <summary>
    ''' Manages a replaying CAPI proxy for E2E tests.
    ''' Spawns the shared test harness server from test/harness/server.ts
    ''' and exposes the proxy URL for SDK client configuration.
    ''' </summary>
    Public Class TestHarness
        Implements IDisposable

        Private _process As Process
        Private _proxyUrl As String
        Private _disposed As Boolean = False

        ''' <summary>Gets the proxy URL after the harness has started.</summary>
        Public ReadOnly Property ProxyUrl As String
            Get
                Return _proxyUrl
            End Get
        End Property

        ''' <summary>Gets the absolute path to the repo root.</summary>
        Public ReadOnly Property RepoRoot As String

        Public Sub New()
            ' e2e dir is visualbasic/e2e, repo root is 3 levels up
            Dim e2eDir = Path.GetDirectoryName(GetType(TestHarness).Assembly.Location)
            ' At runtime the assembly is in bin/Debug/net8.0 under e2e, so walk up further
            ' Instead, compute from the project directory using a known marker
            Dim candidate = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."))
            If Not File.Exists(Path.Combine(candidate, "test", "harness", "server.ts")) Then
                ' Fallback: walk from assembly location
                candidate = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "..", ".."))
            End If
            RepoRoot = candidate
        End Sub

        ''' <summary>
        ''' Starts the replay proxy server and waits for it to be ready.
        ''' </summary>
        Public Async Function StartAsync() As Task
            If _proxyUrl IsNot Nothing Then Return

            Dim serverPath = Path.Combine(RepoRoot, "test", "harness", "server.ts")
            If Not File.Exists(serverPath) Then
                Throw New FileNotFoundException($"Test harness server not found at: {serverPath}")
            End If

            Dim harnessDir = Path.GetDirectoryName(serverPath)

            Dim psi As New ProcessStartInfo() With {
                .FileName = "npx",
                .Arguments = $"tsx ""{serverPath}""",
                .UseShellExecute = False,
                .RedirectStandardOutput = True,
                .RedirectStandardError = False,
                .CreateNoWindow = True,
                .StandardOutputEncoding = Encoding.UTF8,
                .WorkingDirectory = harnessDir
            }

            _process = Process.Start(psi)
            If _process Is Nothing Then
                Throw New InvalidOperationException("Failed to start the test harness process")
            End If

            ' Read the first line to get the listening URL
            Dim line = Await _process.StandardOutput.ReadLineAsync()
            If String.IsNullOrEmpty(line) Then
                Kill()
                Throw New InvalidOperationException("Failed to read proxy URL from harness stdout")
            End If

            Dim match = Regex.Match(line, "Listening: (http://[^\s]+)")
            If Not match.Success Then
                Kill()
                Throw New InvalidOperationException($"Unexpected harness output: {line}")
            End If

            _proxyUrl = match.Groups(1).Value
        End Function

        ''' <summary>
        ''' Configures the proxy for a specific test scenario.
        ''' </summary>
        ''' <param name="filePath">Absolute path to the YAML snapshot file.</param>
        ''' <param name="workDir">Working directory for the test.</param>
        Public Async Function ConfigureAsync(filePath As String, workDir As String) As Task
            If _proxyUrl Is Nothing Then
                Throw New InvalidOperationException("Harness not started. Call StartAsync first.")
            End If

            Using client As New HttpClient()
                Dim payload As New Dictionary(Of String, String) From {
                    {"filePath", filePath},
                    {"workDir", workDir}
                }
                Dim json = JsonSerializer.Serialize(payload)
                Dim content As New StringContent(json, Encoding.UTF8, "application/json")
                Dim response = Await client.PostAsync($"{_proxyUrl}/config", content)
                response.EnsureSuccessStatusCode()
            End Using
        End Function

        ''' <summary>
        ''' Stops the proxy server gracefully, then kills the process.
        ''' </summary>
        ''' <param name="skipWritingCache">If True, the proxy won't persist captured exchanges.</param>
        Public Async Function StopAsync(Optional skipWritingCache As Boolean = True) As Task
            If _process Is Nothing Then Return

            ' Send stop request
            If _proxyUrl IsNot Nothing Then
                Try
                    Using client As New HttpClient()
                        client.Timeout = TimeSpan.FromSeconds(5)
                        Dim stopUrl = $"{_proxyUrl}/stop"
                        If skipWritingCache Then
                            stopUrl &= "?skipWritingCache=true"
                        End If
                        Await client.PostAsync(stopUrl, Nothing)
                    End Using
                Catch
                    ' Best effort
                End Try
            End If

            ' Wait for process to exit
            Try
                If Not _process.HasExited Then
                    _process.WaitForExit(5000)
                End If
            Catch
                ' Ignore
            End Try

            Kill()
            _proxyUrl = Nothing
        End Function

        ''' <summary>
        ''' Returns the environment variables needed for isolated testing.
        ''' </summary>
        Public Function GetTestEnvironment(homeDir As String) As Dictionary(Of String, String)
            If _proxyUrl Is Nothing Then
                Throw New InvalidOperationException("Harness not started.")
            End If

            Return New Dictionary(Of String, String) From {
                {"COPILOT_API_URL", _proxyUrl},
                {"COPILOT_HOME", homeDir},
                {"XDG_CONFIG_HOME", homeDir},
                {"XDG_STATE_HOME", homeDir}
            }
        End Function

        ''' <summary>
        ''' Builds the absolute path to a snapshot YAML file.
        ''' </summary>
        ''' <param name="category">Snapshot subdirectory (e.g. "session").</param>
        ''' <param name="testName">Snapshot file name without extension.</param>
        Public Function GetSnapshotPath(category As String, testName As String) As String
            Return Path.Combine(RepoRoot, "test", "snapshots", category, $"{testName}.yaml")
        End Function

        Private Sub Kill()
            If _process Is Nothing Then Return
            Try
                If Not _process.HasExited Then
                    _process.Kill(entireProcessTree:=True)
                    _process.WaitForExit(3000)
                End If
            Catch
                ' Ignore
            End Try
            _process.Dispose()
            _process = Nothing
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If _disposed Then Return
            _disposed = True
            Kill()
        End Sub

    End Class

End Namespace

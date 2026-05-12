' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Collections.Concurrent
Imports System.Diagnostics
Imports System.IO
Imports System.Text
Imports System.Text.Json
Imports System.Threading

Namespace GitHub.Copilot.SDK

    ''' <summary>
    ''' Provides a client for interacting with the Copilot CLI server.
    ''' </summary>
    ''' <remarks>
    ''' The <see cref="CopilotClient"/> manages the connection to the Copilot CLI
    ''' server and provides methods to create and manage conversation sessions.
    ''' It can either spawn a CLI server process or connect to an existing server.
    ''' </remarks>
    ''' <example>
    ''' <code>
    ''' Dim client As New CopilotClient()
    ''' Await client.StartAsync()
    '''
    ''' Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    '''     .Model = "gpt-4",
    '''     .OnPermissionRequest = PermissionHandlers.ApproveAll()
    ''' })
    '''
    ''' session.On(Sub(evt)
    '''     If TypeOf evt Is AssistantMessageEvent Then
    '''         Console.WriteLine(DirectCast(evt, AssistantMessageEvent).Data?.Content)
    '''     End If
    ''' End Sub)
    '''
    ''' Await session.SendAsync(New MessageOptions With { .Prompt = "Hello!" })
    ''' </code>
    ''' </example>
    Public Class CopilotClient
        Implements IDisposable

        Private ReadOnly _sessions As New ConcurrentDictionary(Of String, CopilotSession)
        Private ReadOnly _options As CopilotClientOptions
        Private _process As Process
        Private _rpcClient As JsonRpcClient
        Private _started As Boolean = False
        Private _disposed As Boolean = False

        ''' <summary>
        ''' Gets whether the client is currently connected.
        ''' </summary>
        Public ReadOnly Property IsConnected As Boolean
            Get
                Return _started AndAlso _process IsNot Nothing AndAlso Not _process.HasExited
            End Get
        End Property

        ''' <summary>
        ''' Creates a new instance of <see cref="CopilotClient"/>.
        ''' </summary>
        ''' <param name="options">Options for creating the client. If Nothing, defaults are used.</param>
        Public Sub New(Optional options As CopilotClientOptions = Nothing)
            _options = If(options, New CopilotClientOptions())

            ' Validate mutually exclusive options
            If Not String.IsNullOrEmpty(_options.CliUrl) AndAlso Not String.IsNullOrEmpty(_options.CliPath) Then
                Throw New ArgumentException("CliUrl is mutually exclusive with CliPath")
            End If

            If Not String.IsNullOrEmpty(_options.CliUrl) Then
                _options.UseStdio = False
            End If

            If Not String.IsNullOrEmpty(_options.CliUrl) AndAlso
               (Not String.IsNullOrEmpty(_options.GitHubToken) OrElse _options.UseLoggedInUser IsNot Nothing) Then
                Throw New ArgumentException(
                    "GitHubToken and UseLoggedInUser cannot be used with CliUrl (external server manages its own auth)")
            End If
        End Sub

        ' -----------------------------------------------------------------------
        '  Lifecycle
        ' -----------------------------------------------------------------------

        ''' <summary>
        ''' Starts the Copilot client and connects to the server.
        ''' </summary>
        ''' <param name="cancellationToken">Cancellation token.</param>
        Public Async Function StartAsync(Optional cancellationToken As CancellationToken = Nothing) As Task
            ThrowIfDisposed()
            If _started Then Return

            If Not String.IsNullOrEmpty(_options.CliUrl) Then
                Throw New NotSupportedException(
                    "Connecting to an external CLI server via CliUrl is not yet supported in the VB SDK. " &
                    "Use stdio mode (default) instead.")
            End If

            Dim cliPath = ResolveCliPath()
            Dim args = BuildCliArgs()

            Dim psi As New ProcessStartInfo With {
                .FileName = cliPath,
                .Arguments = args,
                .UseShellExecute = False,
                .RedirectStandardInput = True,
                .RedirectStandardOutput = True,
                .RedirectStandardError = True,
                .CreateNoWindow = True,
                .StandardOutputEncoding = Encoding.UTF8,
                .StandardErrorEncoding = Encoding.UTF8
            }

            If Not String.IsNullOrEmpty(_options.Cwd) Then
                psi.WorkingDirectory = _options.Cwd
            End If

            ' Apply environment variables
            If _options.Environment IsNot Nothing Then
                For Each kvp In _options.Environment
                    psi.EnvironmentVariables(kvp.Key) = kvp.Value
                Next
            End If

            If Not String.IsNullOrEmpty(_options.GitHubToken) Then
                psi.EnvironmentVariables("GITHUB_TOKEN") = _options.GitHubToken
            End If

            If _options.Telemetry IsNot Nothing Then
                ApplyTelemetryEnv(psi, _options.Telemetry)
            End If

            _process = Process.Start(psi)
            If _process Is Nothing Then
                Throw New InvalidOperationException("Failed to start the Copilot CLI process")
            End If

            _rpcClient = New JsonRpcClient(_process.StandardOutput.BaseStream, _process.StandardInput.BaseStream)
            _rpcClient.StartListening()

            ' Verify protocol compatibility
            Await VerifyProtocolVersionAsync(cancellationToken)

            ' Set up session filesystem provider if configured
            If _options.SessionFs IsNot Nothing AndAlso
               Not String.IsNullOrEmpty(_options.SessionFs.InitialCwd) Then
                Dim fsParams As New Dictionary(Of String, Object) From {
                    {"initialCwd", _options.SessionFs.InitialCwd},
                    {"sessionStatePath", _options.SessionFs.SessionStatePath},
                    {"conventions", If(_options.SessionFs.Conventions, "posix")}
                }
                Await _rpcClient.InvokeAsync(Of Object)("sessionFs.setProvider", fsParams, cancellationToken)
            End If

            _started = True
        End Function

        ''' <summary>
        ''' Stops the client and cleans up all resources.
        ''' </summary>
        Public Async Function StopAsync() As Task
            Dim errors As New List(Of Exception)

            ' Dispose all sessions
            For Each session In _sessions.Values.ToArray()
                Try
                    session.Dispose()
                Catch ex As Exception
                    errors.Add(New Exception($"Failed to dispose session {session.SessionId}: {ex.Message}", ex))
                End Try
            Next
            _sessions.Clear()

            ' Shut down RPC
            If _rpcClient IsNot Nothing Then
                Try
                    _rpcClient.Dispose()
                Catch ex As Exception
                    errors.Add(ex)
                End Try
                _rpcClient = Nothing
            End If

            ' Kill CLI process
            If _process IsNot Nothing Then
                Try
                    If Not _process.HasExited Then
                        _process.Kill()
                        Await Task.Run(Sub() _process.WaitForExit(5000))
                    End If
                Catch ex As Exception
                    errors.Add(ex)
                End Try
                _process.Dispose()
                _process = Nothing
            End If

            _started = False

            If errors.Count > 0 Then
                Throw New AggregateException("Errors occurred during shutdown", errors)
            End If
        End Function

        ' -----------------------------------------------------------------------
        '  Session management
        ' -----------------------------------------------------------------------

        ''' <summary>
        ''' Creates a new conversation session.
        ''' </summary>
        ''' <param name="config">Session configuration.</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>The created <see cref="CopilotSession"/>.</returns>
        Public Async Function CreateSessionAsync(
            Optional config As SessionConfig = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of CopilotSession)

            Await EnsureStartedAsync(cancellationToken)

            Dim cfg = If(config, New SessionConfig())
            Dim request = BuildSessionCreateRequest(cfg)

            Dim response = Await _rpcClient.InvokeAsync(Of SessionCreateResponse)("session.create", request, cancellationToken)

            Dim session As New CopilotSession(response.SessionId, _rpcClient, response.WorkspacePath)
            If response.Capabilities IsNot Nothing Then
                session.Capabilities = response.Capabilities
            End If

            ' Wire up permission handler
            If cfg.OnPermissionRequest IsNot Nothing Then
                session.SetPermissionHandler(cfg.OnPermissionRequest)
            End If

            ' Wire up exit plan mode handler
            If cfg.OnExitPlanMode IsNot Nothing Then
                session.RegisterExitPlanModeHandler(cfg.OnExitPlanMode)
            End If

            ' Wire up trace context provider
            If _options.OnGetTraceContext IsNot Nothing Then
                session.SetTraceContextProvider(_options.OnGetTraceContext)
            End If

            ' Register tools
            If cfg.Tools IsNot Nothing Then
                For Each tool In cfg.Tools
                    session.RegisterTool(tool)
                Next
            End If

            _sessions.TryAdd(response.SessionId, session)
            Return session
        End Function

        ''' <summary>
        ''' Resumes a previously created session.
        ''' </summary>
        ''' <param name="sessionId">The session identifier to resume.</param>
        ''' <param name="config">Resume configuration.</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>The resumed <see cref="CopilotSession"/>.</returns>
        Public Async Function ResumeSessionAsync(
            sessionId As String,
            Optional config As ResumeSessionConfig = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of CopilotSession)

            Await EnsureStartedAsync(cancellationToken)

            Dim cfg = If(config, New ResumeSessionConfig())
            Dim request As New Dictionary(Of String, Object) From {
                {"sessionId", sessionId}
            }

            If cfg.Skills IsNot Nothing Then
                request("skills") = cfg.Skills
            End If

            Dim response = Await _rpcClient.InvokeAsync(Of SessionCreateResponse)("session.resume", request, cancellationToken)

            Dim session As New CopilotSession(response.SessionId, _rpcClient, response.WorkspacePath)
            If response.Capabilities IsNot Nothing Then
                session.Capabilities = response.Capabilities
            End If

            If cfg.OnPermissionRequest IsNot Nothing Then
                session.SetPermissionHandler(cfg.OnPermissionRequest)
            End If

            ' Wire up trace context provider
            If _options.OnGetTraceContext IsNot Nothing Then
                session.SetTraceContextProvider(_options.OnGetTraceContext)
            End If

            If cfg.Tools IsNot Nothing Then
                For Each tool In cfg.Tools
                    session.RegisterTool(tool)
                Next
            End If

            _sessions.TryAdd(response.SessionId, session)
            Return session
        End Function

        ''' <summary>
        ''' Retrieves metadata for a session.
        ''' </summary>
        Public Async Function GetSessionMetadataAsync(
            sessionId As String,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of SessionMetadata)

            Await EnsureStartedAsync(cancellationToken)
            Return Await _rpcClient.InvokeAsync(Of SessionMetadata)(
                "session.getMetadata", New With {.sessionId = sessionId}, cancellationToken)
        End Function

        ''' <summary>
        ''' Sets the foreground session for the server.
        ''' </summary>
        Public Async Function SetForegroundSessionIdAsync(
            sessionId As String,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task

            Await EnsureStartedAsync(cancellationToken)
            Await _rpcClient.InvokeAsync(Of Object)(
                "session.setForeground", New With {.sessionId = sessionId}, cancellationToken)
        End Function

        ''' <summary>
        ''' Gets the foreground session ID from the server.
        ''' </summary>
        Public Async Function GetForegroundSessionIdAsync(
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of String)

            Await EnsureStartedAsync(cancellationToken)
            Dim result = Await _rpcClient.InvokeAsync(Of JsonElement)(
                "session.getForeground", Nothing, cancellationToken)
            If result.TryGetProperty("sessionId", Nothing) Then
                Return result.GetProperty("sessionId").GetString()
            End If
            Return String.Empty
        End Function

        ''' <summary>
        ''' Lists all sessions on the server.
        ''' </summary>
        Public Async Function ListSessionsAsync(
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of List(Of SessionMetadata))

            Await EnsureStartedAsync(cancellationToken)
            Return Await _rpcClient.InvokeAsync(Of List(Of SessionMetadata))("session.list", Nothing, cancellationToken)
        End Function

        ''' <summary>
        ''' Permanently deletes a session and its data.
        ''' </summary>
        Public Async Function DeleteSessionAsync(
            sessionId As String,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task

            Await EnsureStartedAsync(cancellationToken)
            Await _rpcClient.InvokeAsync(Of Object)(
                "session.delete", New With {.sessionId = sessionId}, cancellationToken)

            Dim removed As CopilotSession = Nothing
            If _sessions.TryRemove(sessionId, removed) Then
                removed.Dispose()
            End If
        End Function

        ''' <summary>
        ''' Pings the CLI server and returns diagnostics.
        ''' </summary>
        Public Async Function PingAsync(
            Optional message As String = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of PingResponse)

            Await EnsureStartedAsync(cancellationToken)

            Dim params As Object = Nothing
            If message IsNot Nothing Then
                params = New With {.message = message}
            End If

            Return Await _rpcClient.InvokeAsync(Of PingResponse)("ping", params, cancellationToken)
        End Function

        ''' <summary>
        ''' Lists available models from the server.
        ''' </summary>
        Public Async Function ListModelsAsync(
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of List(Of ModelInfo))

            Await EnsureStartedAsync(cancellationToken)
            Return Await _rpcClient.InvokeAsync(Of List(Of ModelInfo))("models.list", Nothing, cancellationToken)
        End Function

        ''' <summary>
        ''' Returns the last-used session identifier, or Nothing if none exists.
        ''' </summary>
        Public Async Function GetLastSessionIdAsync(
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of String)

            Await EnsureStartedAsync(cancellationToken)
            Dim result = Await _rpcClient.InvokeAsync(Of JsonElement)("session.getLastId", Nothing, cancellationToken)
            If result.ValueKind = JsonValueKind.Object AndAlso
               result.TryGetProperty("sessionId", Nothing) Then
                Dim prop = result.GetProperty("sessionId")
                If prop.ValueKind = JsonValueKind.String Then
                    Return prop.GetString()
                End If
            End If
            Return Nothing
        End Function

        ''' <summary>
        ''' Returns the CLI server status information.
        ''' </summary>
        Public Async Function GetStatusAsync(
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of JsonElement)

            Await EnsureStartedAsync(cancellationToken)
            Return Await _rpcClient.InvokeAsync(Of JsonElement)("status.get", Nothing, cancellationToken)
        End Function

        ''' <summary>
        ''' Returns the current authentication status.
        ''' </summary>
        Public Async Function GetAuthStatusAsync(
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of JsonElement)

            Await EnsureStartedAsync(cancellationToken)
            Return Await _rpcClient.InvokeAsync(Of JsonElement)("auth.getStatus", Nothing, cancellationToken)
        End Function

        ' -----------------------------------------------------------------------
        '  Private helpers
        ' -----------------------------------------------------------------------

        Private Function ResolveCliPath() As String
            If Not String.IsNullOrEmpty(_options.CliPath) Then
                Return _options.CliPath
            End If

            Dim envPath = System.Environment.GetEnvironmentVariable("COPILOT_CLI_PATH")
            If Not String.IsNullOrEmpty(envPath) Then
                Return envPath
            End If

            ' Default: assume 'copilot' is on PATH
            Return "copilot"
        End Function

        Private Function BuildCliArgs() As String
            Dim parts As New List(Of String)

            ' Prepend any user-supplied args
            If _options.CliArgs IsNot Nothing Then
                parts.AddRange(_options.CliArgs)
            End If

            parts.Add("--server")
            parts.Add("--stdio")
            parts.Add($"--log-level={_options.LogLevel}")

            If _options.SessionIdleTimeoutSeconds.HasValue Then
                parts.Add($"--session-idle-timeout={_options.SessionIdleTimeoutSeconds.Value}")
            End If

            If _options.UseLoggedInUser.HasValue AndAlso _options.UseLoggedInUser.Value Then
                parts.Add("--use-logged-in-user")
            End If

            Return String.Join(" ", parts)
        End Function

        Private Function BuildSessionCreateRequest(cfg As SessionConfig) As Dictionary(Of String, Object)
            Dim request As New Dictionary(Of String, Object) From {
                {"protocolVersion", SdkProtocolVersion.Version}
            }

            If Not String.IsNullOrEmpty(cfg.Model) Then
                request("model") = cfg.Model
            End If

            If Not String.IsNullOrEmpty(cfg.SystemMessage) Then
                request("systemMessage") = cfg.SystemMessage
            End If

            If cfg.Skills IsNot Nothing AndAlso cfg.Skills.Count > 0 Then
                request("skills") = cfg.Skills
            End If

            If cfg.CustomizeMode Then
                request("customizeMode") = True
            End If

            If cfg.AgentMode Then
                request("agentMode") = True
            End If

            If cfg.Attachments IsNot Nothing AndAlso cfg.Attachments.Count > 0 Then
                request("attachments") = cfg.Attachments
            End If

            If cfg.Tools IsNot Nothing AndAlso cfg.Tools.Count > 0 Then
                Dim toolDefs As New List(Of Dictionary(Of String, Object))
                For Each tool In cfg.Tools
                    toolDefs.Add(New Dictionary(Of String, Object) From {
                        {"name", tool.Name},
                        {"description", tool.Description},
                        {"parameters", tool.ParametersSchema}
                    })
                Next
                request("tools") = toolDefs
            End If

            If cfg.OnExitPlanMode IsNot Nothing Then
                request("requestExitPlanMode") = True
            End If

            If cfg.EnableSessionTelemetry.HasValue Then
                request("enableSessionTelemetry") = cfg.EnableSessionTelemetry.Value
            End If

            Return request
        End Function

        Private Sub ApplyTelemetryEnv(psi As ProcessStartInfo, telemetry As TelemetryConfig)
            If Not String.IsNullOrEmpty(telemetry.OtlpEndpoint) Then
                psi.EnvironmentVariables("OTEL_EXPORTER_OTLP_ENDPOINT") = telemetry.OtlpEndpoint
            End If
            If Not String.IsNullOrEmpty(telemetry.FilePath) Then
                psi.EnvironmentVariables("COPILOT_OTEL_FILE_EXPORTER_PATH") = telemetry.FilePath
            End If
            If Not String.IsNullOrEmpty(telemetry.ExporterType) Then
                psi.EnvironmentVariables("COPILOT_OTEL_EXPORTER_TYPE") = telemetry.ExporterType
            End If
            If Not String.IsNullOrEmpty(telemetry.SourceName) Then
                psi.EnvironmentVariables("COPILOT_OTEL_SOURCE_NAME") = telemetry.SourceName
            End If
            If telemetry.CaptureContent.HasValue Then
                psi.EnvironmentVariables("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT") =
                    telemetry.CaptureContent.Value.ToString().ToLowerInvariant()
            End If
        End Sub

        Private Async Function VerifyProtocolVersionAsync(ct As CancellationToken) As Task
            Try
                Dim info = Await _rpcClient.InvokeAsync(Of Dictionary(Of String, Object))(
                    "getServerInfo", Nothing, ct)

                ' Check that server protocol version is compatible
                If info IsNot Nothing AndAlso info.ContainsKey("protocolVersion") Then
                    Dim serverVersion = Convert.ToInt32(info("protocolVersion"))
                    Dim clientVersion = SdkProtocolVersion.GetVersion()
                    If serverVersion < clientVersion Then
                        Throw New InvalidOperationException(
                            $"Server protocol version {serverVersion} is older than SDK version {clientVersion}")
                    End If
                End If
            Catch ex As JsonRpcException
                ' Server may not support getServerInfo yet; continue anyway
            End Try
        End Function

        Private Async Function EnsureStartedAsync(ct As CancellationToken) As Task
            If Not _started Then
                If _options.AutoStart Then
                    Await StartAsync(ct)
                Else
                    Throw New InvalidOperationException(
                        "Client is not started. Call StartAsync first, or set AutoStart = True.")
                End If
            End If
        End Function

        Private Sub ThrowIfDisposed()
            If _disposed Then Throw New ObjectDisposedException(NameOf(CopilotClient))
        End Sub

        ' -----------------------------------------------------------------------
        '  IDisposable
        ' -----------------------------------------------------------------------

        Public Sub Dispose() Implements IDisposable.Dispose
            If _disposed Then Return
            _disposed = True

            For Each session In _sessions.Values
                Try
                    session.Dispose()
                Catch
                End Try
            Next
            _sessions.Clear()

            _rpcClient?.Dispose()

            If _process IsNot Nothing Then
                Try
                    If Not _process.HasExited Then
                        _process.Kill()
                    End If
                Catch
                End Try
                _process.Dispose()
            End If
        End Sub

    End Class

End Namespace

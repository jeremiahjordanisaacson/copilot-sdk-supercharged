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
        Private ReadOnly _gitHubTokenProviders As New ConcurrentDictionary(Of String, GitHubTokenRegistration)
        Private ReadOnly _options As CopilotClientOptions
        Private ReadOnly _mode As String
        Private _process As Process
        Private _rpcClient As JsonRpcClient
        Private _negotiatedProtocolVersion As Integer? = Nothing
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
        ''' Gets the client mode in effect: <see cref="ClientMode.CodingAgent"/> or <see cref="ClientMode.Empty"/>.
        ''' </summary>
        Public ReadOnly Property Mode As String
            Get
                Return _mode
            End Get
        End Property

        ''' <summary>
        ''' Gets the SDK protocol version negotiated with the server (Nothing before start).
        ''' </summary>
        Public ReadOnly Property NegotiatedProtocolVersion As Integer?
            Get
                Return _negotiatedProtocolVersion
            End Get
        End Property

        ''' <summary>
        ''' Creates a new instance of <see cref="CopilotClient"/>.
        ''' </summary>
        ''' <param name="options">Options for creating the client. If Nothing, defaults are used.</param>
        Public Sub New(Optional options As CopilotClientOptions = Nothing)
            _options = If(options, New CopilotClientOptions())
            _mode = ClientMode.Normalize(_options.Mode)

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
            RegisterServerHandlers()
            _rpcClient.StartListening()

            ' Verify protocol compatibility (connect, falling back to ping on older CLIs)
            Await VerifyProtocolVersionAsync(cancellationToken)

            ' Register host network callbacks
            If _options.Network IsNot Nothing AndAlso _options.Network.HasCallbacks Then
                Await _rpcClient.InvokeAsync(Of JsonElement)(
                    "network.setProvider", _options.Network.ToSetProviderParams(), cancellationToken)
            End If

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
            _gitHubTokenProviders.Clear()

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
            If Not String.IsNullOrEmpty(cfg.GitHubToken) AndAlso cfg.GitHubTokenProvider IsNot Nothing Then
                Throw New ArgumentException("GitHubToken and GitHubTokenProvider are mutually exclusive")
            End If
            RequireAvailableToolsForMode(cfg.AvailableTools)

            Dim registrationId = RegisterGitHubTokenProvider(cfg.GitHubTokenProvider, Nothing)
            Dim request = BuildSessionCreateRequest(cfg, registrationId)
            Dim wireRequest = FinalizeSessionParams(request, isResume:=False)

            Dim response As SessionCreateResponse
            Try
                response = Await _rpcClient.InvokeAsync(Of SessionCreateResponse)("session.create", wireRequest, cancellationToken)
            Catch
                RemoveGitHubTokenProvider(registrationId)
                Throw
            End Try

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

            TrackSession(session, registrationId, cfg.OnSubagentStart, cfg.OnSubagentStop,
                         cfg.OnShellAuthenticationRequest, cfg.OnStoreMemory,
                         ResolveUserInputHandler(cfg.OnUserInputRequest, cfg.OnUserInput))
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
            RequireAvailableToolsForMode(cfg.AvailableTools)

            Dim request As New Dictionary(Of String, Object) From {
                {"sessionId", sessionId}
            }

            If Not String.IsNullOrEmpty(cfg.Model) Then
                request("model") = cfg.Model
            End If

            If cfg.Skills IsNot Nothing AndAlso cfg.Skills.Count > 0 Then
                request("includedBuiltinSkills") = cfg.Skills
            End If

            If cfg.IncludeSubAgentStreamingEvents Then
                request("includeSubAgentStreamingEvents") = True
            End If

            Dim registrationId = RegisterGitHubTokenProvider(cfg.GitHubTokenProvider, sessionId)
            AddSharedSessionOptions(
                request,
                cfg.AutoTier, cfg.EnableWebSocketResponses, cfg.CapiEnabled, cfg.AskUserVariant,
                cfg.SandboxConfig, cfg.FeatureFlags, cfg.AllowedModels, cfg.AuthClientIdMetadataUrl,
                cfg.AgentDirectories, cfg.HookDirectories, cfg.InteractiveUser,
                cfg.ExcludedManagedSettings, cfg.EnforceManagedModelDefaults,
                cfg.ImageGeneration, cfg.Autopilot, cfg.Provider,
                cfg.OnSubagentStart IsNot Nothing OrElse cfg.OnSubagentStop IsNot Nothing,
                registrationId, cfg.AvailableTools, cfg.ExcludedTools, cfg.Tools,
                cfg.Providers, cfg.Models, cfg.DefaultProvider,
                cfg.OnShellAuthenticationRequest IsNot Nothing, cfg.OnStoreMemory IsNot Nothing,
                cfg.OnUserInputRequest IsNot Nothing OrElse cfg.OnUserInput IsNot Nothing)

            ' Resume-only options.
            If cfg.AllowTranscriptRecovery.HasValue Then
                request("allowTranscriptRecovery") = cfg.AllowTranscriptRecovery.Value
            End If
            If cfg.SuppressResumeEvent.HasValue Then
                request("suppressResumeEvent") = cfg.SuppressResumeEvent.Value
            End If
            If cfg.ClearDefaultProvider Then
                ' An explicit null clears the provider stored with the session.
                request("inference") = New Dictionary(Of String, Object) From {{"defaultProvider", Nothing}}
            End If

            Dim wireRequest = FinalizeSessionParams(request, isResume:=True)

            Dim response As SessionCreateResponse
            Try
                response = Await _rpcClient.InvokeAsync(Of SessionCreateResponse)("session.resume", wireRequest, cancellationToken)
            Catch
                RemoveGitHubTokenProvider(registrationId)
                Throw
            End Try

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

            TrackSession(session, registrationId, cfg.OnSubagentStart, cfg.OnSubagentStop,
                         cfg.OnShellAuthenticationRequest, cfg.OnStoreMemory,
                         ResolveUserInputHandler(cfg.OnUserInputRequest, cfg.OnUserInput))
            Return session
        End Function

        ''' <summary>
        ''' Clears the server's managed-settings (enterprise policy) cache.
        ''' </summary>
        Public Async Function ClearManagedSettingsCacheAsync(Optional cancellationToken As CancellationToken = Nothing) As Task
            Await EnsureStartedAsync(cancellationToken)
            Await _rpcClient.InvokeAsync(Of Object)(
                "managedSettings.clearCache", New Dictionary(Of String, Object)(), cancellationToken)
        End Function

        ''' <summary>
        ''' Saves a legacy MCP configuration credential in the runtime-owned secret store
        ''' (<c>mcp.secrets.save</c>). Requires Copilot CLI 1.0.116 or newer.
        ''' </summary>
        ''' <param name="secretId">Identifier of the credential.</param>
        ''' <param name="value">The credential. It is never logged.</param>
        ''' <param name="configDir">Configuration directory (the runtime's default when Nothing).</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>
        ''' A <see cref="McpSecretsSaveResult"/> whose <c>UsedKeychain</c> is True when the OS keychain
        ''' stored the credential and False for the file backend.
        ''' </returns>
        Public Async Function McpSecretsSaveAsync(
            secretId As String,
            value As String,
            Optional configDir As String = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of McpSecretsSaveResult)

            Await EnsureStartedAsync(cancellationToken)
            Dim request As New Dictionary(Of String, Object) From {
                {"secretId", secretId},
                {"value", value}
            }
            If configDir IsNot Nothing Then request("configDir") = configDir
            Dim result = Await _rpcClient.InvokeAsync(Of McpSecretsSaveResult)("mcp.secrets.save", request, cancellationToken)
            Return If(result, New McpSecretsSaveResult())
        End Function

        ''' <summary>
        ''' Deletes a legacy MCP configuration credential from the runtime-owned secret store
        ''' (<c>mcp.secrets.delete</c>). Requires Copilot CLI 1.0.116 or newer.
        ''' </summary>
        ''' <param name="secretId">Identifier of the credential.</param>
        ''' <param name="configDir">Configuration directory (the runtime's default when Nothing).</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        Public Async Function McpSecretsDeleteAsync(
            secretId As String,
            Optional configDir As String = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task

            Await EnsureStartedAsync(cancellationToken)
            Dim request As New Dictionary(Of String, Object) From {{"secretId", secretId}}
            If configDir IsNot Nothing Then request("configDir") = configDir
            Await _rpcClient.InvokeAsync(Of Object)("mcp.secrets.delete", request, cancellationToken)
        End Function

        ''' <summary>
        ''' Deletes the legacy MCP configuration credentials indexed for a server
        ''' (<c>mcp.secrets.deleteServer</c>). Requires Copilot CLI 1.0.116 or newer.
        ''' </summary>
        ''' <param name="serverName">The MCP server name.</param>
        ''' <param name="configDir">Configuration directory (the runtime's default when Nothing).</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        Public Async Function McpSecretsDeleteServerAsync(
            serverName As String,
            Optional configDir As String = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task

            Await EnsureStartedAsync(cancellationToken)
            Dim request As New Dictionary(Of String, Object) From {{"serverName", serverName}}
            If configDir IsNot Nothing Then request("configDir") = configDir
            Await _rpcClient.InvokeAsync(Of Object)("mcp.secrets.deleteServer", request, cancellationToken)
        End Function

        ''' <summary>
        ''' Persists a pending plugin extension permission request as a user-scoped approval, before
        ''' the host answers that request normally (<c>permissions.extensionApprovals.grant</c>).
        ''' Requires Copilot CLI 1.0.116 or newer.
        ''' </summary>
        ''' <param name="sessionId">Session that owns the pending request.</param>
        ''' <param name="requestId">Id of the pending extension permission request.</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>Whether the request was eligible and persisted, and the stored approval.</returns>
        Public Async Function ExtensionApprovalsGrantAsync(
            sessionId As String,
            requestId As String,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of PermissionsExtensionApprovalsGrantResult)

            Await EnsureStartedAsync(cancellationToken)
            Dim request As New Dictionary(Of String, Object) From {
                {"sessionId", sessionId},
                {"requestId", requestId}
            }
            Dim result = Await _rpcClient.InvokeAsync(Of PermissionsExtensionApprovalsGrantResult)(
                "permissions.extensionApprovals.grant", request, cancellationToken)
            Return If(result, New PermissionsExtensionApprovalsGrantResult())
        End Function

        ''' <summary>
        ''' Lists the user-scoped plugin extension approvals (<c>permissions.extensionApprovals.list</c>).
        ''' Requires Copilot CLI 1.0.116 or newer.
        ''' </summary>
        ''' <param name="sessionId">
        ''' Attached session whose configuration profile to list (the server's profile when Nothing).
        ''' </param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        Public Async Function ExtensionApprovalsListAsync(
            Optional sessionId As String = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of PermissionsExtensionApprovalsListResult)

            Await EnsureStartedAsync(cancellationToken)
            Dim request As New Dictionary(Of String, Object)
            If sessionId IsNot Nothing Then request("sessionId") = sessionId
            Dim result = Await _rpcClient.InvokeAsync(Of PermissionsExtensionApprovalsListResult)(
                "permissions.extensionApprovals.list", request, cancellationToken)
            Return If(result, New PermissionsExtensionApprovalsListResult())
        End Function

        ''' <summary>
        ''' Revokes one user-scoped plugin extension approval (<c>permissions.extensionApprovals.revoke</c>).
        ''' Requires Copilot CLI 1.0.116 or newer.
        ''' </summary>
        ''' <param name="extensionName">The approval's <c>ExtensionName</c>.</param>
        ''' <param name="installationKey">The approval's <c>InstallationKey</c>, unchanged.</param>
        ''' <param name="sessionId">Attached session whose configuration profile to use (optional).</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>Whether a matching approval was revoked.</returns>
        Public Async Function ExtensionApprovalsRevokeAsync(
            extensionName As String,
            installationKey As String,
            Optional sessionId As String = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of PermissionsExtensionApprovalsRevokeResult)

            Await EnsureStartedAsync(cancellationToken)
            Dim request As New Dictionary(Of String, Object) From {
                {"extensionName", extensionName},
                {"installationKey", installationKey}
            }
            If sessionId IsNot Nothing Then request("sessionId") = sessionId
            Dim result = Await _rpcClient.InvokeAsync(Of PermissionsExtensionApprovalsRevokeResult)(
                "permissions.extensionApprovals.revoke", request, cancellationToken)
            Return If(result, New PermissionsExtensionApprovalsRevokeResult())
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
            Dim result = Await _rpcClient.InvokeAsync(Of JsonElement)(
                "session.setForeground", New With {.sessionId = sessionId}, cancellationToken)
            If ReadBool(result, "success") = False Then
                Throw New InvalidOperationException(If(ReadString(result, "error"), "Failed to set foreground session"))
            End If
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
            Dim result = Await _rpcClient.InvokeAsync(Of JsonElement)(
                "session.delete", New With {.sessionId = sessionId}, cancellationToken)
            If ReadBool(result, "success") = False Then
                Throw New InvalidOperationException(
                    $"Failed to delete session {sessionId}: {If(ReadString(result, "error"), "Unknown error")}")
            End If

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

        Private Function BuildSessionCreateRequest(cfg As SessionConfig, tokenProviderRegistrationId As String) As Dictionary(Of String, Object)
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
                request("includedBuiltinSkills") = cfg.Skills
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

            ' --- Upstream-sync session options (parity with @github/copilot-sdk) ---
            If cfg.EnableCitations.HasValue Then
                request("enableCitations") = cfg.EnableCitations.Value
            End If

            If cfg.ExcludedBuiltinAgents IsNot Nothing AndAlso cfg.ExcludedBuiltinAgents.Count > 0 Then
                request("excludedBuiltinAgents") = cfg.ExcludedBuiltinAgents
            End If

            If cfg.SessionLimits IsNot Nothing Then
                request("sessionLimits") = cfg.SessionLimits.ToWire()
            End If

            If cfg.Memory IsNot Nothing Then
                request("memory") = cfg.Memory.ToWire()
            End If

            If Not String.IsNullOrEmpty(cfg.OtlpProtocol) Then
                request("otlpProtocol") = cfg.OtlpProtocol
            End If

            If cfg.ExpAssignments IsNot Nothing AndAlso cfg.ExpAssignments.Count > 0 Then
                request("expAssignments") = cfg.ExpAssignments
            End If

            If cfg.RewindEnabled.HasValue Then
                request("rewindEnabled") = cfg.RewindEnabled.Value
            End If

            If cfg.AdditionalDirectories IsNot Nothing AndAlso cfg.AdditionalDirectories.Count > 0 Then
                request("additionalDirectories") = cfg.AdditionalDirectories
            End If

            If cfg.DisabledMcpServers IsNot Nothing AndAlso cfg.DisabledMcpServers.Count > 0 Then
                request("disabledMcpServers") = cfg.DisabledMcpServers
            End If

            If cfg.GithubMcpToolConfig IsNot Nothing AndAlso cfg.GithubMcpToolConfig.Count > 0 Then
                request("githubMcpToolConfig") = cfg.GithubMcpToolConfig
            End If

            If cfg.CanvasProvider IsNot Nothing AndAlso cfg.CanvasProvider.Count > 0 Then
                request("canvasProvider") = cfg.CanvasProvider
            End If

            If cfg.CustomAgentsLocalOnly.HasValue Then
                request("customAgentsLocalOnly") = cfg.CustomAgentsLocalOnly.Value
            End If

            If cfg.DecisionContext IsNot Nothing AndAlso cfg.DecisionContext.Count > 0 Then
                request("decisionContext") = cfg.DecisionContext
            End If

            ' Signal to the runtime that a user-prompt-transformed hook is registered.
            If cfg.OnUserPromptTransformed IsNot Nothing Then
                request("userPromptTransformed") = True
            End If

            If cfg.BuiltinPluginDirectories IsNot Nothing AndAlso cfg.BuiltinPluginDirectories.Count > 0 Then
                request("builtinPluginDirectories") = cfg.BuiltinPluginDirectories
            End If

            If cfg.ArgsSchema IsNot Nothing AndAlso cfg.ArgsSchema.Count > 0 Then
                request("argsSchema") = cfg.ArgsSchema
            End If

            If Not String.IsNullOrEmpty(cfg.ReasoningEffort) Then
                request("reasoningEffort") = cfg.ReasoningEffort
            End If

            If cfg.ToolSearch IsNot Nothing AndAlso cfg.ToolSearch.Count > 0 Then
                request("toolSearch") = cfg.ToolSearch
            End If

            If cfg.InProcess.HasValue Then
                request("inProcess") = cfg.InProcess.Value
            End If

            If cfg.ExperimentalMode.HasValue Then
                request("enableExperimentalMode") = cfg.ExperimentalMode.Value
            End If

            If cfg.ContentExclusion.HasValue Then
                request("contentExclusion") = cfg.ContentExclusion.Value
            End If

            ' Signal to the runtime that an MCP OAuth host-token handler is registered.
            If cfg.OnMcpAuthRequest IsNot Nothing Then
                request("mcpAuthHandler") = True
            End If

            If cfg.OnExitPlanMode IsNot Nothing Then
                request("requestExitPlanMode") = True
            End If

            If cfg.EnableSessionTelemetry.HasValue Then
                request("enableSessionTelemetry") = cfg.EnableSessionTelemetry.Value
            End If

            If Not String.IsNullOrEmpty(cfg.GitHubToken) Then
                request("gitHubToken") = cfg.GitHubToken
            End If

            If cfg.ModelCapabilities IsNot Nothing Then
                request("modelCapabilities") = cfg.ModelCapabilities
            End If

            If cfg.McpServers IsNot Nothing AndAlso cfg.McpServers.Count > 0 Then
                request("mcpServers") = cfg.McpServers
            End If

            If cfg.IncludeSubAgentStreamingEvents Then
                request("includeSubAgentStreamingEvents") = True
            End If

            AddSharedSessionOptions(
                request,
                cfg.AutoTier, cfg.EnableWebSocketResponses, cfg.CapiEnabled, cfg.AskUserVariant,
                cfg.SandboxConfig, cfg.FeatureFlags, cfg.AllowedModels, cfg.AuthClientIdMetadataUrl,
                cfg.AgentDirectories, cfg.HookDirectories, cfg.InteractiveUser,
                cfg.ExcludedManagedSettings, cfg.EnforceManagedModelDefaults,
                cfg.ImageGeneration, cfg.Autopilot, cfg.Provider,
                cfg.OnSubagentStart IsNot Nothing OrElse cfg.OnSubagentStop IsNot Nothing,
                tokenProviderRegistrationId, cfg.AvailableTools, cfg.ExcludedTools, cfg.Tools,
                cfg.Providers, cfg.Models, cfg.DefaultProvider,
                cfg.OnShellAuthenticationRequest IsNot Nothing, cfg.OnStoreMemory IsNot Nothing,
                cfg.OnUserInputRequest IsNot Nothing OrElse cfg.OnUserInput IsNot Nothing)

            ' Create-only options.
            If Not String.IsNullOrEmpty(cfg.Name) Then
                request("name") = cfg.Name
            End If
            If Not String.IsNullOrEmpty(cfg.TrustWorkingDirectory) Then
                request("trustWorkingDirectory") = cfg.TrustWorkingDirectory
            End If
            If cfg.RefreshCustomInstructions.HasValue Then
                request("refreshCustomInstructions") = cfg.RefreshCustomInstructions.Value
            End If

            Return request
        End Function

        ''' <summary>
        ''' Adds the options shared by <c>session.create</c> and <c>session.resume</c> to the flat payload.
        ''' </summary>
        Private Shared Sub AddSharedSessionOptions(
            request As Dictionary(Of String, Object),
            autoTier As String,
            enableWebSocketResponses As Boolean?,
            capiEnabled As Boolean?,
            askUserVariant As String,
            sandboxConfig As SandboxConfig,
            featureFlags As Dictionary(Of String, Boolean),
            allowedModels As List(Of String),
            authClientIdMetadataUrl As String,
            agentDirectories As List(Of String),
            hookDirectories As List(Of String),
            interactiveUser As Boolean?,
            excludedManagedSettings As List(Of String),
            enforceManagedModelDefaults As Boolean?,
            imageGeneration As ImageGenerationConfig,
            autopilot As AutopilotOptions,
            provider As ProviderConfig,
            hasSubagentHooks As Boolean,
            tokenProviderRegistrationId As String,
            availableTools As List(Of String),
            excludedTools As List(Of String),
            tools As List(Of DefineTool),
            providers As List(Of NamedProviderConfig),
            models As List(Of ProviderModelConfig),
            defaultProvider As String,
            hasShellAuthentication As Boolean,
            hasMemoryStore As Boolean,
            hasUserInput As Boolean)

            ' autoTier, enableWebSocketResponses and enabled share "capi" (github.capi on the wire).
            Dim capi As New Dictionary(Of String, Object)
            If Not String.IsNullOrEmpty(autoTier) Then capi("autoTier") = autoTier
            If enableWebSocketResponses.HasValue Then capi("enableWebSocketResponses") = enableWebSocketResponses.Value
            If capiEnabled.HasValue Then capi("enabled") = capiEnabled.Value
            If capi.Count > 0 Then request("capi") = capi

            If Not String.IsNullOrEmpty(askUserVariant) Then request("askUserVariant") = askUserVariant
            If sandboxConfig IsNot Nothing Then request("sandboxConfig") = sandboxConfig.ToWire()
            If featureFlags IsNot Nothing Then request("featureFlags") = featureFlags
            If allowedModels IsNot Nothing Then request("allowedModels") = allowedModels
            If Not String.IsNullOrEmpty(authClientIdMetadataUrl) Then request("authClientIdMetadataUrl") = authClientIdMetadataUrl
            If agentDirectories IsNot Nothing Then request("agentDirectories") = agentDirectories
            If hookDirectories IsNot Nothing Then request("hookDirectories") = hookDirectories
            If interactiveUser.HasValue Then request("interactiveUser") = interactiveUser.Value
            If excludedManagedSettings IsNot Nothing Then request("excludedManagedSettings") = excludedManagedSettings
            If enforceManagedModelDefaults.HasValue Then request("enforceManagedModelDefaults") = enforceManagedModelDefaults.Value
            If imageGeneration IsNot Nothing Then request("imageGeneration") = imageGeneration.ToWire()
            If autopilot IsNot Nothing Then request("autopilot") = autopilot.ToWire()
            If provider IsNot Nothing Then request("provider") = provider.ToWire()
            If hasSubagentHooks Then request("hooks") = True
            If tokenProviderRegistrationId IsNot Nothing Then request("gitHubTokenProviderRegistrationId") = tokenProviderRegistrationId
            If availableTools IsNot Nothing Then request("availableTools") = availableTools
            If excludedTools IsNot Nothing AndAlso excludedTools.Count > 0 Then request("excludedTools") = excludedTools
            If tools IsNot Nothing AndAlso tools.Count > 0 Then
                request("tools") = tools.Select(Function(t) t.ToWire()).ToList()
            End If
            If providers IsNot Nothing Then request("providers") = providers.Select(Function(p) p.ToWire()).ToList()
            If models IsNot Nothing Then request("models") = models.Select(Function(m) m.ToWire()).ToList()
            If Not String.IsNullOrEmpty(defaultProvider) Then request("defaultProvider") = defaultProvider
            If hasShellAuthentication Then request("requestShellAuthentication") = True
            If hasMemoryStore Then request("hasMemoryStore") = True
            If hasUserInput Then request("requestUserInput") = True
        End Sub

        ''' <summary>
        ''' Translates the flat payload into the SDK protocol 4 grouped payload and applies the
        ''' client-mode defaults (called right before sending create and resume).
        ''' </summary>
        Private Function FinalizeSessionParams(request As Dictionary(Of String, Object), isResume As Boolean) As Dictionary(Of String, Object)
            Dim grouped = SessionWire.GroupSessionParams(request, isResume)
            If _mode = ClientMode.Empty Then
                Return SessionWire.ApplyEmptyModeDefaults(grouped)
            End If
            Return grouped
        End Function

        Private Sub RequireAvailableToolsForMode(availableTools As List(Of String))
            If _mode = ClientMode.Empty AndAlso availableTools Is Nothing Then
                Throw New InvalidOperationException(
                    "CopilotClient is in mode 'empty' but the session config did not specify AvailableTools. " &
                    "Empty mode requires every session to opt into the tools it wants.")
            End If
        End Sub

        Private Function RegisterGitHubTokenProvider(
            provider As Func(Of GitHubTokenProviderArgs, Task(Of GitHubTokenProviderResult)),
            sessionId As String) As String

            If provider Is Nothing Then Return Nothing
            Dim registrationId = Guid.NewGuid().ToString()
            _gitHubTokenProviders(registrationId) = New GitHubTokenRegistration With {
                .Provider = provider,
                .SessionId = sessionId
            }
            Return registrationId
        End Function

        Private Sub RemoveGitHubTokenProvider(registrationId As String)
            If registrationId Is Nothing Then Return
            Dim removed As GitHubTokenRegistration = Nothing
            _gitHubTokenProviders.TryRemove(registrationId, removed)
        End Sub

        ''' <summary>
        ''' Wires the host callbacks into a created/resumed session and tracks it for server requests.
        ''' </summary>
        Private Sub TrackSession(
            session As CopilotSession,
            registrationId As String,
            onSubagentStart As Func(Of SubagentStartHookInput, Task(Of SubagentStartHookOutput)),
            onSubagentStop As Func(Of SubagentStopHookInput, Task(Of SubagentStopHookOutput)),
            onShellAuthentication As ShellAuthenticationHandler,
            onStoreMemory As Func(Of StoredMemory, Task),
            onUserInput As Func(Of UserInputRequest, Task(Of UserInputResponse)))

            Dim registration As GitHubTokenRegistration = Nothing
            If registrationId IsNot Nothing AndAlso _gitHubTokenProviders.TryGetValue(registrationId, registration) Then
                registration.SessionId = session.SessionId
            End If
            session.SetSubagentHooks(onSubagentStart, onSubagentStop)
            session.SetShellAuthenticationHandler(onShellAuthentication)
            session.SetMemoryStoreHandler(onStoreMemory)
            session.SetUserInputHandler(onUserInput)
            session.SetDetachedCallback(Sub()
                                            RemoveGitHubTokenProvider(registrationId)
                                            ' Only forget the session if a later create/resume did not replace it.
                                            DirectCast(_sessions, ICollection(Of KeyValuePair(Of String, CopilotSession))).Remove(
                                                New KeyValuePair(Of String, CopilotSession)(session.SessionId, session))
                                        End Sub)
            _sessions(session.SessionId) = session
        End Sub

        ''' <summary>
        ''' Picks the session's user input handler: <c>OnUserInputRequest</c> when set, otherwise the
        ''' answer-only <c>OnUserInput</c> adapted to a <see cref="UserInputResponse"/> (freeform unless
        ''' the answer is one of the offered choices).
        ''' </summary>
        Private Shared Function ResolveUserInputHandler(
            onUserInputRequest As Func(Of UserInputRequest, Task(Of UserInputResponse)),
            onUserInput As Func(Of UserInputRequest, Task(Of String))) As Func(Of UserInputRequest, Task(Of UserInputResponse))

            If onUserInputRequest IsNot Nothing OrElse onUserInput Is Nothing Then Return onUserInputRequest
            Return Async Function(request As UserInputRequest) As Task(Of UserInputResponse)
                       Dim answer = If(Await onUserInput(request), "")
                       Return New UserInputResponse With {
                           .Answer = answer,
                           .WasFreeform = request.Choices Is Nothing OrElse Not request.Choices.Contains(answer)
                       }
                   End Function
        End Function

        ' -----------------------------------------------------------------------
        '  Server-to-client requests
        ' -----------------------------------------------------------------------

        Private Sub RegisterServerHandlers()
            _rpcClient.OnRequest("gitHubToken.getToken", AddressOf HandleGitHubTokenRequestAsync)
            _rpcClient.OnRequest("hooks.invoke", AddressOf HandleHooksInvokeAsync)
            _rpcClient.OnRequest("exitPlanMode.request", AddressOf HandleExitPlanModeRequestAsync)
            _rpcClient.OnRequest("shellAuthentication.request", AddressOf HandleShellAuthenticationRequestAsync)
            _rpcClient.OnRequest("shellAuthentication.cancel", AddressOf HandleShellAuthenticationCancelAsync)
            _rpcClient.OnRequest("memoryStore.store", AddressOf HandleMemoryStoreAsync)
            _rpcClient.OnRequest("userInput.request", AddressOf HandleUserInputRequestAsync)
            If _options.Network IsNot Nothing Then
                If _options.Network.ResolveProxy IsNot Nothing Then
                    _rpcClient.OnRequest("network.resolveProxy", AddressOf HandleResolveProxyAsync)
                End If
                If _options.Network.AuthenticateNegotiate IsNot Nothing Then
                    _rpcClient.OnRequest("network.authenticateNegotiate", AddressOf HandleAuthenticateNegotiateAsync)
                End If
                If _options.Network.AuthenticateBasic IsNot Nothing Then
                    _rpcClient.OnRequest("network.authenticateBasic", AddressOf HandleAuthenticateBasicAsync)
                End If
            End If
        End Sub

        ''' <summary>Answers <c>gitHubToken.getToken</c> with the registered provider's token.</summary>
        Private Async Function HandleGitHubTokenRequestAsync(params As JsonElement) As Task(Of Object)
            Dim registrationId = ReadString(params, "registrationId")
            Dim registration As GitHubTokenRegistration = Nothing
            If registrationId Is Nothing OrElse Not _gitHubTokenProviders.TryGetValue(registrationId, registration) Then
                Throw New InvalidOperationException($"No GitHub token provider registered for registration ID ""{registrationId}""")
            End If
            Dim args As New GitHubTokenProviderArgs With {
                .Host = If(ReadString(params, "host"), ""),
                .SessionId = If(ReadString(params, "sessionId"), registration.SessionId),
                .Reason = If(ReadString(params, "reason"), "initial")
            }
            Dim result = Await registration.Provider(args)
            Return If(result, GitHubTokenProviderResult.Cancelled()).ToWire()
        End Function

        ''' <summary>Dispatches <c>hooks.invoke</c> to the target session ({output?}).</summary>
        Private Async Function HandleHooksInvokeAsync(params As JsonElement) As Task(Of Object)
            Dim sessionId = ReadString(params, "sessionId")
            Dim session As CopilotSession = Nothing
            If sessionId Is Nothing OrElse Not _sessions.TryGetValue(sessionId, session) Then
                Throw New InvalidOperationException($"Session not found: {sessionId}")
            End If
            Dim input As JsonElement = Nothing
            If params.ValueKind = JsonValueKind.Object Then params.TryGetProperty("input", input)
            Dim output = Await session.HandleHooksInvokeAsync(ReadString(params, "hookType"), input)
            Dim response As New Dictionary(Of String, Object)
            If output IsNot Nothing Then response("output") = output
            Return response
        End Function

        ''' <summary>Answers <c>exitPlanMode.request</c> with the session's exit-plan-mode handler.</summary>
        Private Async Function HandleExitPlanModeRequestAsync(params As JsonElement) As Task(Of Object)
            Dim sessionId = ReadString(params, "sessionId")
            Dim session As CopilotSession = Nothing
            If sessionId Is Nothing OrElse Not _sessions.TryGetValue(sessionId, session) Then
                Throw New InvalidOperationException($"Session not found: {sessionId}")
            End If
            Dim request = JsonSerializer.Deserialize(Of ExitPlanModeRequest)(params.GetRawText())
            Return Await session.HandleExitPlanModeRequest(request)
        End Function

        ''' <summary>Answers <c>shellAuthentication.request</c>; malformed requests are cancelled.</summary>
        Private Async Function HandleShellAuthenticationRequestAsync(params As JsonElement) As Task(Of Object)
            Dim inputKind = ReadString(params, "inputKind")
            Dim request As New ShellAuthenticationRequest With {
                .SessionId = ReadString(params, "sessionId"),
                .RequestId = ReadString(params, "requestId"),
                .Program = ReadString(params, "program"),
                .Prompt = ReadString(params, "prompt"),
                .InputKind = inputKind,
                .Command = If(ReadString(params, "command"), ""),
                .Cwd = If(ReadString(params, "cwd"), "")
            }
            Dim validKind = inputKind = ShellAuthenticationInputKind.Secret OrElse
                            inputKind = ShellAuthenticationInputKind.Text OrElse
                            inputKind = ShellAuthenticationInputKind.Confirm
            Dim session As CopilotSession = Nothing
            If Not validKind OrElse request.SessionId Is Nothing OrElse request.RequestId Is Nothing OrElse
               request.Program Is Nothing OrElse request.Prompt Is Nothing OrElse
               Not _sessions.TryGetValue(request.SessionId, session) Then
                Return ShellAuthenticationResponse.Cancel().ToWire()
            End If
            Dim answer = Await session.HandleShellAuthenticationAsync(request)
            Return answer.ToWire()
        End Function

        ''' <summary>Handles <c>shellAuthentication.cancel</c> by cancelling the pending prompt.</summary>
        Private Function HandleShellAuthenticationCancelAsync(params As JsonElement) As Task(Of Object)
            Dim sessionId = ReadString(params, "sessionId")
            Dim session As CopilotSession = Nothing
            If sessionId IsNot Nothing AndAlso _sessions.TryGetValue(sessionId, session) Then
                session.CancelShellAuthentication(ReadString(params, "requestId"))
            End If
            Return Task.FromResult(Of Object)(New Dictionary(Of String, Object)())
        End Function

        ''' <summary>Delivers <c>memoryStore.store</c> to the session's store-memory callback (replies null).</summary>
        Private Async Function HandleMemoryStoreAsync(params As JsonElement) As Task(Of Object)
            Dim sessionId = ReadString(params, "sessionId")
            Dim session As CopilotSession = Nothing
            If sessionId Is Nothing OrElse Not _sessions.TryGetValue(sessionId, session) Then
                Throw New InvalidOperationException("Memory store owner is unavailable")
            End If
            Await session.HandleMemoryStoreAsync(New StoredMemory With {
                .SessionId = sessionId,
                .Subject = If(ReadString(params, "subject"), ""),
                .Fact = If(ReadString(params, "fact"), ""),
                .Citations = If(ReadString(params, "citations"), ""),
                .Reason = If(ReadString(params, "reason"), "")
            })
            Return Nothing
        End Function

        ''' <summary>
        ''' Answers <c>userInput.request</c> (the ask_user tool) with the session's user input handler.
        ''' The request's <c>imageAttachmentConstraints</c>, <c>requestId</c> and <c>_meta</c> are passed
        ''' through; the reply carries <c>attachments</c> only when the handler set them.
        ''' </summary>
        Private Async Function HandleUserInputRequestAsync(params As JsonElement) As Task(Of Object)
            Dim sessionId = ReadString(params, "sessionId")
            Dim session As CopilotSession = Nothing
            If sessionId Is Nothing OrElse Not _sessions.TryGetValue(sessionId, session) Then
                Throw New InvalidOperationException($"Session not found: {sessionId}")
            End If
            Dim request = JsonSerializer.Deserialize(Of UserInputRequest)(params.GetRawText())
            If request.Prompt Is Nothing Then request.Prompt = request.Question
            Dim response = Await session.HandleUserInputRequestAsync(request)
            Return response.ToWire()
        End Function

        ''' <summary>Answers <c>network.resolveProxy</c> {url} with {proxy?}.</summary>
        Private Async Function HandleResolveProxyAsync(params As JsonElement) As Task(Of Object)
            Dim result As New Dictionary(Of String, Object)
            Dim proxy = Await _options.Network.ResolveProxy(If(ReadString(params, "url"), ""))
            If Not String.IsNullOrEmpty(proxy) Then result("proxy") = proxy
            Return result
        End Function

        ''' <summary>Answers <c>network.authenticateNegotiate</c> {spn} with {token?}. Tokens are never logged.</summary>
        Private Async Function HandleAuthenticateNegotiateAsync(params As JsonElement) As Task(Of Object)
            Dim result As New Dictionary(Of String, Object)
            Dim token = Await _options.Network.AuthenticateNegotiate(If(ReadString(params, "spn"), ""))
            If Not String.IsNullOrEmpty(token) Then result("token") = token
            Return result
        End Function

        ''' <summary>
        ''' Answers <c>network.authenticateBasic</c> {proxy, realm?, previousRejected} with
        ''' {username?, password?}. Credentials are never logged.
        ''' </summary>
        Private Async Function HandleAuthenticateBasicAsync(params As JsonElement) As Task(Of Object)
            Dim result As New Dictionary(Of String, Object)
            Dim request As New NetworkBasicAuthRequest With {
                .Proxy = If(ReadString(params, "proxy"), ""),
                .Realm = ReadString(params, "realm"),
                .PreviousRejected = ReadBool(params, "previousRejected").GetValueOrDefault(False)
            }
            Dim credentials = Await _options.Network.AuthenticateBasic(request)
            If credentials IsNot Nothing Then
                If credentials.Username IsNot Nothing Then result("username") = credentials.Username
                If credentials.Password IsNot Nothing Then result("password") = credentials.Password
            End If
            Return result
        End Function

        ' -----------------------------------------------------------------------
        '  JSON helpers
        ' -----------------------------------------------------------------------

        Private Shared Function ReadString(element As JsonElement, name As String) As String
            Dim value As JsonElement = Nothing
            If element.ValueKind = JsonValueKind.Object AndAlso element.TryGetProperty(name, value) AndAlso
               value.ValueKind = JsonValueKind.String Then
                Return value.GetString()
            End If
            Return Nothing
        End Function

        Private Shared Function ReadInt(element As JsonElement, name As String) As Integer?
            Dim value As JsonElement = Nothing
            Dim number As Integer
            If element.ValueKind = JsonValueKind.Object AndAlso element.TryGetProperty(name, value) AndAlso
               value.ValueKind = JsonValueKind.Number AndAlso value.TryGetInt32(number) Then
                Return number
            End If
            Return Nothing
        End Function

        Private Shared Function ReadBool(element As JsonElement, name As String) As Boolean?
            Dim value As JsonElement = Nothing
            If element.ValueKind = JsonValueKind.Object AndAlso element.TryGetProperty(name, value) Then
                If value.ValueKind = JsonValueKind.True Then Return True
                If value.ValueKind = JsonValueKind.False Then Return False
            End If
            Return Nothing
        End Function

        ''' <summary>A GitHub token provider registered for one session.</summary>
        Private Class GitHubTokenRegistration
            Public Property Provider As Func(Of GitHubTokenProviderArgs, Task(Of GitHubTokenProviderResult))
            Public Property SessionId As String
        End Class

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
            Dim serverVersion As Integer? = Nothing
            Dim connectUnsupported = False
            Try
                Dim connectParams As New Dictionary(Of String, Object)
                Dim clientInfo = _options.ClientInfo?.ToWire()
                If clientInfo IsNot Nothing AndAlso clientInfo.Count > 0 Then
                    connectParams("clientInfo") = clientInfo
                End If
                Dim result = Await _rpcClient.InvokeAsync(Of JsonElement)("connect", connectParams, ct)
                serverVersion = ReadInt(result, "protocolVersion")
            Catch ex As JsonRpcException When ex.Code = -32601 OrElse ex.Message.Contains("Unhandled method connect")
                ' Legacy server without connect: fall back to ping.
                connectUnsupported = True
            End Try

            If connectUnsupported Then
                Dim pong = Await _rpcClient.InvokeAsync(Of JsonElement)(
                    "ping", New Dictionary(Of String, Object) From {{"message", "ping"}}, ct)
                serverVersion = ReadInt(pong, "protocolVersion")
            End If

            Dim expected = SdkProtocolVersion.Version
            If Not serverVersion.HasValue Then
                Throw New InvalidOperationException(
                    $"SDK protocol version mismatch: SDK supports version {expected}, but server does not report a protocol version. " &
                    "Please update your SDK or server to ensure compatibility.")
            End If
            If serverVersion.Value <> expected Then
                Throw New InvalidOperationException(
                    $"SDK protocol version mismatch: SDK supports version {expected}, but server reports version {serverVersion.Value}. " &
                    "Please update your SDK or server to ensure compatibility.")
            End If
            _negotiatedProtocolVersion = serverVersion
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

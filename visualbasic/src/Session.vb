' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Text.Json
Imports System.Threading

Namespace GitHub.Copilot.SDK

    ''' <summary>
    ''' Represents a single conversation session with the Copilot CLI.
    ''' </summary>
    ''' <remarks>
    ''' Sessions are created via <see cref="CopilotClient.CreateSessionAsync"/> or
    ''' resumed via <see cref="CopilotClient.ResumeSessionAsync"/>.
    ''' Disposing a session releases in-memory resources but preserves session data
    ''' on disk so the conversation can be resumed later.
    ''' </remarks>
    Public Class CopilotSession
        Implements IDisposable

        Private ReadOnly _rpc As JsonRpcClient
        Private ReadOnly _eventHandlers As New List(Of Action(Of SessionEvent))
        Private ReadOnly _handlersLock As New Object()
        Private ReadOnly _toolHandlers As New Dictionary(Of String, Func(Of Object, Task(Of String)))
        Private _permissionHandler As PermissionHandler
        Private _exitPlanModeHandler As ExitPlanModeHandler
        Private _traceContextProvider As TraceContextProvider
        Private _disposed As Boolean = False

        ''' <summary>Gets the unique identifier for this session.</summary>
        Public ReadOnly Property SessionId As String

        ''' <summary>Gets the path to the session workspace directory (Nothing if infinite sessions are disabled).</summary>
        Public Property WorkspacePath As String

        ''' <summary>Gets the capabilities reported by the host for this session.</summary>
        Public Property Capabilities As SessionCapabilities

        ''' <summary>
        ''' Initializes a new session instance.
        ''' </summary>
        Friend Sub New(sessionId As String, rpc As JsonRpcClient, Optional workspacePath As String = Nothing)
            Me.SessionId = sessionId
            _rpc = rpc
            Me.WorkspacePath = workspacePath
            Me.Capabilities = New SessionCapabilities()

            SetupNotificationHandlers()
        End Sub

        ' -----------------------------------------------------------------------
        '  Send
        ' -----------------------------------------------------------------------

        ''' <summary>
        ''' Sends a message to the session and returns the message ID.
        ''' </summary>
        ''' <param name="options">Message options including the prompt.</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>The message ID for correlation.</returns>
        Public Async Function SendAsync(options As MessageOptions, Optional cancellationToken As CancellationToken = Nothing) As Task(Of String)
            ThrowIfDisposed()

            Dim request As New SendMessageRequest With {
                .SessionId = SessionId,
                .Prompt = options.Prompt,
                .Attachments = options.Attachments,
                .Mode = options.Mode
            }

            Dim response = Await _rpc.InvokeAsync(Of SendMessageResponse)("session.send", request, cancellationToken)
            Return response.MessageId
        End Function

        ''' <summary>
        ''' Sends a message and waits until the session becomes idle.
        ''' </summary>
        ''' <param name="options">Message options including the prompt.</param>
        ''' <param name="timeout">How long to wait (default: 60 seconds).</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>The final assistant message, or Nothing if none was received.</returns>
        Public Async Function SendAndWaitAsync(
            options As MessageOptions,
            Optional timeout As TimeSpan? = Nothing,
            Optional cancellationToken As CancellationToken = Nothing
        ) As Task(Of AssistantMessageEvent)

            ThrowIfDisposed()

            Dim effectiveTimeout = If(timeout, TimeSpan.FromSeconds(60))
            Dim tcs As New TaskCompletionSource(Of AssistantMessageEvent)(TaskCreationOptions.RunContinuationsAsynchronously)
            Dim lastMessage As AssistantMessageEvent = Nothing

            Dim handler As Action(Of SessionEvent) = Sub(evt)
                                                         If TypeOf evt Is AssistantMessageEvent Then
                                                             lastMessage = DirectCast(evt, AssistantMessageEvent)
                                                         ElseIf TypeOf evt Is SessionIdleEvent Then
                                                             tcs.TrySetResult(lastMessage)
                                                         ElseIf TypeOf evt Is SessionErrorEvent Then
                                                             Dim errEvt = DirectCast(evt, SessionErrorEvent)
                                                             Dim msg = If(errEvt.Data?.Message, "session error")
                                                             tcs.TrySetException(New InvalidOperationException($"Session error: {msg}"))
                                                         End If
                                                     End Sub

            Dim subscription = [On](handler)

            Try
                Await SendAsync(options, cancellationToken)

                Using cts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken)
                    cts.CancelAfter(effectiveTimeout)

                    Using reg = cts.Token.Register(Sub()
                                                       If cancellationToken.IsCancellationRequested Then
                                                           tcs.TrySetCanceled(cancellationToken)
                                                       Else
                                                           tcs.TrySetException(New TimeoutException(
                                                               $"SendAndWaitAsync timed out after {effectiveTimeout}"))
                                                       End If
                                                   End Sub)
                        Return Await tcs.Task
                    End Using
                End Using
            Finally
                subscription.Dispose()
            End Try
        End Function

        ' -----------------------------------------------------------------------
        '  Event subscription
        ' -----------------------------------------------------------------------

        ''' <summary>
        ''' Registers a callback for session events.
        ''' </summary>
        ''' <param name="handler">Callback invoked for each event.</param>
        ''' <returns>An <see cref="IDisposable"/> that unsubscribes when disposed.</returns>
        Public Function [On](handler As Action(Of SessionEvent)) As IDisposable
            SyncLock _handlersLock
                _eventHandlers.Add(handler)
            End SyncLock

            Return New EventSubscription(Sub()
                                             SyncLock _handlersLock
                                                 _eventHandlers.Remove(handler)
                                             End SyncLock
                                         End Sub)
        End Function

        ''' <summary>
        ''' Registers a callback for a specific event type.
        ''' </summary>
        ''' <typeparam name="T">The event type to listen for.</typeparam>
        ''' <param name="handler">Callback invoked when a matching event arrives.</param>
        ''' <returns>An <see cref="IDisposable"/> that unsubscribes when disposed.</returns>
        Public Function [On](Of T As SessionEvent)(handler As Action(Of T)) As IDisposable
            Return [On](Sub(evt)
                            If TypeOf evt Is T Then
                                handler(DirectCast(evt, T))
                            End If
                        End Sub)
        End Function

        ' -----------------------------------------------------------------------
        '  Tool registration
        ' -----------------------------------------------------------------------

        ''' <summary>
        ''' Registers a tool handler that the assistant can invoke.
        ''' </summary>
        ''' <param name="tool">The tool definition.</param>
        Public Sub RegisterTool(tool As DefineTool)
            If tool Is Nothing Then Throw New ArgumentNullException(NameOf(tool))
            _toolHandlers(tool.Name) = tool.Handler
        End Sub

        ''' <summary>
        ''' Registers a permission handler for this session.
        ''' </summary>
        Public Sub SetPermissionHandler(handler As PermissionHandler)
            _permissionHandler = handler
        End Sub

        ''' <summary>
        ''' Registers a handler for exit-plan-mode requests.
        ''' </summary>
        Friend Sub RegisterExitPlanModeHandler(handler As ExitPlanModeHandler)
            _exitPlanModeHandler = handler
        End Sub

        ''' <summary>
        ''' Handles an exit-plan-mode request from the CLI.
        ''' </summary>
        Friend Async Function HandleExitPlanModeRequest(request As ExitPlanModeRequest) As Task(Of ExitPlanModeResult)
            If _exitPlanModeHandler Is Nothing Then
                Return New ExitPlanModeResult With {.Approved = True}
            End If
            Return Await _exitPlanModeHandler(request)
        End Function

        ''' <summary>
        ''' Sets the W3C Trace Context provider.
        ''' </summary>
        Friend Sub SetTraceContextProvider(provider As TraceContextProvider)
            _traceContextProvider = provider
        End Sub

        ' -----------------------------------------------------------------------
        '  Session lifecycle
        ' -----------------------------------------------------------------------

        ''' <summary>
        ''' Aborts the current in-flight operation.
        ''' </summary>
        Public Async Function AbortAsync(Optional cancellationToken As CancellationToken = Nothing) As Task
            ThrowIfDisposed()
            Await _rpc.InvokeAsync(Of Object)("session.abort", New With {.sessionId = SessionId}, cancellationToken)
        End Function

        ''' <summary>
        ''' Retrieves the conversation message history.
        ''' </summary>
        Public Async Function GetMessagesAsync(Optional cancellationToken As CancellationToken = Nothing) As Task(Of List(Of Object))
            ThrowIfDisposed()
            Return Await _rpc.InvokeAsync(Of List(Of Object))("session.getMessages", New With {.sessionId = SessionId}, cancellationToken)
        End Function

        ''' <summary>
        ''' Disconnects from this session without deleting server-side data.
        ''' </summary>
        Public Async Function DisconnectAsync(Optional cancellationToken As CancellationToken = Nothing) As Task
            ThrowIfDisposed()
            Await _rpc.InvokeAsync(Of Object)("session.disconnect", New With {.sessionId = SessionId}, cancellationToken)
        End Function

        ' -----------------------------------------------------------------------
        '  Internal notification routing
        ' -----------------------------------------------------------------------

        Private Sub SetupNotificationHandlers()
            _rpc.OnNotification("session.event", Sub(params)
                                                     Dim evt = DeserializeEvent(params)
                                                     If evt Is Nothing Then Return
                                                     If evt.SessionId <> SessionId Then Return
                                                     DispatchEvent(evt)
                                                 End Sub)

            _rpc.OnNotification("session.toolCall", Sub(params)
                                                        HandleToolCall(params)
                                                    End Sub)

            _rpc.OnNotification("session.permissionRequest", Sub(params)
                                                                 HandlePermissionRequest(params)
                                                             End Sub)
        End Sub

        Private Sub DispatchEvent(evt As SessionEvent)
            Dim snapshot As Action(Of SessionEvent)()
            SyncLock _handlersLock
                snapshot = _eventHandlers.ToArray()
            End SyncLock

            For Each handler In snapshot
                Try
                    handler(evt)
                Catch
                    ' Swallow handler exceptions
                End Try
            Next
        End Sub

        Private Function DeserializeEvent(params As JsonElement) As SessionEvent
            Try
                Dim typeProp As JsonElement = Nothing
                If Not params.TryGetProperty("type", typeProp) Then Return Nothing

                Dim eventType = typeProp.GetString()
                Dim raw = params.GetRawText()

                Select Case eventType
                    Case "assistant.message"
                        Return JsonSerializer.Deserialize(Of AssistantMessageEvent)(raw)
                    Case "assistant.message_delta"
                        Return JsonSerializer.Deserialize(Of AssistantMessageDeltaEvent)(raw)
                    Case "assistant.reasoning"
                        Return JsonSerializer.Deserialize(Of AssistantReasoningEvent)(raw)
                    Case "session.idle"
                        Return JsonSerializer.Deserialize(Of SessionIdleEvent)(raw)
                    Case "session.error"
                        Return JsonSerializer.Deserialize(Of SessionErrorEvent)(raw)
                    Case "tool.call"
                        Return JsonSerializer.Deserialize(Of ToolCallEvent)(raw)
                    Case "session.compaction_start"
                        Return JsonSerializer.Deserialize(Of SessionCompactionStartEvent)(raw)
                    Case "session.compaction_complete"
                        Return JsonSerializer.Deserialize(Of SessionCompactionCompleteEvent)(raw)
                    Case Else
                        Return JsonSerializer.Deserialize(Of SessionEvent)(raw)
                End Select
            Catch
                Return Nothing
            End Try
        End Function

        Private Sub HandleToolCall(params As JsonElement)
            Task.Run(Async Function()
                         Try
                             Dim toolName As String = Nothing
                             Dim toolCallId As String = Nothing

                             Dim nameProp As JsonElement = Nothing
                             If params.TryGetProperty("toolName", nameProp) Then
                                 toolName = nameProp.GetString()
                             End If

                             Dim idProp As JsonElement = Nothing
                             If params.TryGetProperty("toolCallId", idProp) Then
                                 toolCallId = idProp.GetString()
                             End If

                             If toolName Is Nothing OrElse toolCallId Is Nothing Then Return

                             Dim handler As Func(Of Object, Task(Of String)) = Nothing
                             If Not _toolHandlers.TryGetValue(toolName, handler) Then
                                 Await _rpc.NotifyAsync("session.toolResult", New With {
                                     .sessionId = SessionId,
                                     .toolCallId = toolCallId,
                                     .result = "Tool not found",
                                     .resultType = "failure"
                                 })
                                 Return
                             End If

                             Dim inputProp As JsonElement = Nothing
                             params.TryGetProperty("input", inputProp)

                             Dim result = Await handler(inputProp)
                             Await _rpc.NotifyAsync("session.toolResult", New With {
                                 .sessionId = SessionId,
                                 .toolCallId = toolCallId,
                                 .result = result,
                                 .resultType = "success"
                             })
                         Catch ex As Exception
                             ' Best effort
                         End Try
                     End Function)
        End Sub

        Private Sub HandlePermissionRequest(params As JsonElement)
            Task.Run(Async Function()
                         Try
                             If _permissionHandler Is Nothing Then Return

                             Dim request = JsonSerializer.Deserialize(Of PermissionRequest)(params.GetRawText())
                             Dim response = Await _permissionHandler(request)

                             Await _rpc.NotifyAsync("session.permissionResponse", New With {
                                 .sessionId = SessionId,
                                 .allowed = response.Allowed,
                                 .reason = response.Reason
                             })
                         Catch
                             ' Best effort
                         End Try
                     End Function)
        End Sub

        Private Async Function GetTraceContextAsync() As Task(Of TraceContext)
            If _traceContextProvider Is Nothing Then
                Return New TraceContext()
            End If
            Try
                Return Await _traceContextProvider()
            Catch
                Return New TraceContext()
            End Try
        End Function

        ' -----------------------------------------------------------------------
        '  Disposal
        ' -----------------------------------------------------------------------

        Private Sub ThrowIfDisposed()
            If _disposed Then Throw New ObjectDisposedException(NameOf(CopilotSession))
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If _disposed Then Return
            _disposed = True

            SyncLock _handlersLock
                _eventHandlers.Clear()
            End SyncLock
        End Sub

        ' ----- helper -----

        Private Class EventSubscription
            Implements IDisposable

            Private ReadOnly _onDispose As Action

            Public Sub New(onDispose As Action)
                _onDispose = onDispose
            End Sub

            Public Sub Dispose() Implements IDisposable.Dispose
                _onDispose?.Invoke()
            End Sub
        End Class

    End Class

End Namespace

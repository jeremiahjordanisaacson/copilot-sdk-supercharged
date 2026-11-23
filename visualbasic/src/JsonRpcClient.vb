' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Collections.Concurrent
Imports System.IO
Imports System.Text
Imports System.Text.Json
Imports System.Threading

Namespace GitHub.Copilot.SDK

    ''' <summary>
    ''' JSON-RPC 2.0 client that communicates over stdin/stdout streams.
    ''' </summary>
    Public Class JsonRpcClient
        Implements IDisposable

        Private ReadOnly _pendingRequests As New ConcurrentDictionary(Of Integer, TaskCompletionSource(Of JsonElement))
        Private ReadOnly _notificationHandlers As New ConcurrentDictionary(Of String, List(Of Action(Of JsonElement)))
        Private ReadOnly _requestHandlers As New ConcurrentDictionary(Of String, Func(Of JsonElement, Task(Of Object)))
        Private ReadOnly _inputStream As Stream
        Private ReadOnly _outputStream As Stream
        Private ReadOnly _reader As StreamReader
        Private ReadOnly _writer As StreamWriter
        Private ReadOnly _writeLock As New SemaphoreSlim(1, 1)
        Private _nextId As Integer = 0
        Private _readTask As Task
        Private _disposed As Boolean = False
        Private _cts As New CancellationTokenSource()

        ''' <summary>
        ''' Creates a new JSON-RPC client over the given streams.
        ''' </summary>
        ''' <param name="inputStream">Stream to read responses and notifications from (CLI stdout).</param>
        ''' <param name="outputStream">Stream to write requests to (CLI stdin).</param>
        Public Sub New(inputStream As Stream, outputStream As Stream)
            _inputStream = inputStream
            _outputStream = outputStream
            _reader = New StreamReader(inputStream, Encoding.UTF8)
            _writer = New StreamWriter(outputStream, New UTF8Encoding(False)) With {
                .AutoFlush = True
            }
        End Sub

        ''' <summary>
        ''' Starts the background reader loop for incoming messages.
        ''' </summary>
        Public Sub StartListening()
            _readTask = Task.Run(Function() ReadLoopAsync(_cts.Token))
        End Sub

        ''' <summary>
        ''' Sends a JSON-RPC request and waits for the matching response.
        ''' </summary>
        ''' <typeparam name="T">Expected result type.</typeparam>
        ''' <param name="method">RPC method name.</param>
        ''' <param name="params">Parameters to include in the request.</param>
        ''' <param name="cancellationToken">Cancellation token.</param>
        ''' <returns>Deserialized result of type <typeparamref name="T"/>.</returns>
        Public Async Function InvokeAsync(Of T)(method As String, params As Object, Optional cancellationToken As CancellationToken = Nothing) As Task(Of T)
            Dim id = Interlocked.Increment(_nextId)
            Dim tcs As New TaskCompletionSource(Of JsonElement)(TaskCreationOptions.RunContinuationsAsynchronously)

            _pendingRequests.TryAdd(id, tcs)

            Try
                Dim request = New Dictionary(Of String, Object) From {
                    {"jsonrpc", "2.0"},
                    {"id", id},
                    {"method", method},
                    {"params", params}
                }

                Dim json = JsonSerializer.Serialize(request)
                Await WriteMessageAsync(json)

                ' Register cancellation
                Using reg = cancellationToken.Register(Sub() tcs.TrySetCanceled(cancellationToken))
                    Dim resultElement = Await tcs.Task
                    Return JsonSerializer.Deserialize(Of T)(resultElement.GetRawText())
                End Using
            Finally
                Dim removed As TaskCompletionSource(Of JsonElement) = Nothing
                _pendingRequests.TryRemove(id, removed)
            End Try
        End Function

        ''' <summary>
        ''' Sends a JSON-RPC notification (no response expected).
        ''' </summary>
        ''' <param name="method">RPC method name.</param>
        ''' <param name="params">Parameters to include.</param>
        Public Async Function NotifyAsync(method As String, params As Object) As Task
            Dim request = New Dictionary(Of String, Object) From {
                {"jsonrpc", "2.0"},
                {"method", method},
                {"params", params}
            }

            Dim json = JsonSerializer.Serialize(request)
            Await WriteMessageAsync(json)
        End Function

        ''' <summary>
        ''' Registers a handler for incoming notifications of the given method.
        ''' </summary>
        ''' <param name="method">Notification method name to listen for.</param>
        ''' <param name="handler">Callback invoked with the params element.</param>
        ''' <returns>An <see cref="IDisposable"/> that unsubscribes the handler when disposed.</returns>
        Public Function OnNotification(method As String, handler As Action(Of JsonElement)) As IDisposable
            Dim handlers = _notificationHandlers.GetOrAdd(method, Function(k) New List(Of Action(Of JsonElement)))

            SyncLock handlers
                handlers.Add(handler)
            End SyncLock

            Return New NotificationSubscription(Sub()
                                                    SyncLock handlers
                                                        handlers.Remove(handler)
                                                    End SyncLock
                                                End Sub)
        End Function

        ''' <summary>
        ''' Registers a handler for incoming server-to-client requests of the given method.
        ''' The handler's return value is sent back as the JSON-RPC result; an exception is
        ''' sent back as a JSON-RPC error.
        ''' </summary>
        ''' <param name="method">Request method name to handle.</param>
        ''' <param name="handler">Callback invoked with the params element.</param>
        Public Sub OnRequest(method As String, handler As Func(Of JsonElement, Task(Of Object)))
            _requestHandlers(method) = handler
        End Sub

        ' ----- internal helpers -----

        Private Async Function WriteMessageAsync(json As String) As Task
            Await _writeLock.WaitAsync()
            Try
                Await _writer.WriteLineAsync(json)
            Finally
                _writeLock.Release()
            End Try
        End Function

        Private Async Function ReadLoopAsync(ct As CancellationToken) As Task
            Try
                While Not ct.IsCancellationRequested
                    Dim line = Await _reader.ReadLineAsync()
                    If line Is Nothing Then Exit While ' stream closed
                    If String.IsNullOrWhiteSpace(line) Then Continue While

                    Try
                        Dim doc = JsonDocument.Parse(line)
                        Dim root = doc.RootElement

                        If root.TryGetProperty("method", Nothing) AndAlso root.TryGetProperty("id", Nothing) Then
                            HandleRequest(root)
                        ElseIf root.TryGetProperty("id", Nothing) Then
                            HandleResponse(root)
                        Else
                            HandleNotification(root)
                        End If
                    Catch ex As JsonException
                        ' Skip malformed lines
                    End Try
                End While
            Catch ex As OperationCanceledException
                ' Normal shutdown
            Catch ex As IOException
                ' Stream closed
            End Try
        End Function

        Private Sub HandleRequest(root As JsonElement)
            Dim idProp As JsonElement = root.GetProperty("id").Clone()
            Dim methodProp As JsonElement = Nothing
            If Not root.TryGetProperty("method", methodProp) OrElse methodProp.ValueKind <> JsonValueKind.String Then Return
            Dim method = methodProp.GetString()

            Dim paramsProp As JsonElement = Nothing
            If root.TryGetProperty("params", paramsProp) Then
                paramsProp = paramsProp.Clone()
            End If

            Dim handler As Func(Of JsonElement, Task(Of Object)) = Nothing
            If Not _requestHandlers.TryGetValue(method, handler) Then
                ' Legacy notification-style handlers still see the payload.
                HandleNotification(root)
                Dim unused = SendErrorResponseAsync(idProp, -32601, $"Method not found: {method}")
                Return
            End If

            Task.Run(Async Function()
                         Dim result As Object = Nothing
                         Dim failure As Exception = Nothing
                         Try
                             result = Await handler(paramsProp)
                         Catch ex As Exception
                             failure = ex
                         End Try
                         If failure Is Nothing Then
                             Await SendResultResponseAsync(idProp, result)
                         Else
                             Await SendErrorResponseAsync(idProp, -32603, failure.Message)
                         End If
                     End Function)
        End Sub

        Private Async Function SendResultResponseAsync(id As JsonElement, result As Object) As Task
            Dim response = New Dictionary(Of String, Object) From {
                {"jsonrpc", "2.0"},
                {"id", id},
                {"result", result}
            }
            Await WriteMessageAsync(JsonSerializer.Serialize(response))
        End Function

        Private Async Function SendErrorResponseAsync(id As JsonElement, code As Integer, message As String) As Task
            Try
                Dim response = New Dictionary(Of String, Object) From {
                    {"jsonrpc", "2.0"},
                    {"id", id},
                    {"error", New Dictionary(Of String, Object) From {
                        {"code", code},
                        {"message", If(message, "Internal error")}
                    }}
                }
                Await WriteMessageAsync(JsonSerializer.Serialize(response))
            Catch
                ' The connection may already be closed.
            End Try
        End Function

        Private Sub HandleResponse(root As JsonElement)
            Dim idProp As JsonElement = Nothing
            If Not root.TryGetProperty("id", idProp) Then Return

            Dim id As Integer
            If idProp.ValueKind = JsonValueKind.Number Then
                id = idProp.GetInt32()
            Else
                Return
            End If

            Dim tcs As TaskCompletionSource(Of JsonElement) = Nothing
            If Not _pendingRequests.TryRemove(id, tcs) Then Return

            Dim errorProp As JsonElement = Nothing
            If root.TryGetProperty("error", errorProp) Then
                Dim message = "JSON-RPC error"
                Dim msgProp As JsonElement = Nothing
                If errorProp.ValueKind = JsonValueKind.Object AndAlso errorProp.TryGetProperty("message", msgProp) Then
                    message = msgProp.GetString()
                End If
                Dim code = 0
                Dim codeProp As JsonElement = Nothing
                If errorProp.ValueKind = JsonValueKind.Object AndAlso
                   errorProp.TryGetProperty("code", codeProp) AndAlso
                   codeProp.ValueKind = JsonValueKind.Number Then
                    code = codeProp.GetInt32()
                End If
                Dim data As JsonElement? = Nothing
                Dim dataProp As JsonElement = Nothing
                If errorProp.ValueKind = JsonValueKind.Object AndAlso errorProp.TryGetProperty("data", dataProp) Then
                    data = dataProp.Clone()
                End If
                tcs.TrySetException(New JsonRpcException(message) With {.Code = code, .ErrorData = data})
            Else
                Dim resultProp As JsonElement = Nothing
                If root.TryGetProperty("result", resultProp) Then
                    tcs.TrySetResult(resultProp.Clone())
                Else
                    tcs.TrySetResult(Nothing)
                End If
            End If
        End Sub

        Private Sub HandleNotification(root As JsonElement)
            Dim methodProp As JsonElement = Nothing
            If Not root.TryGetProperty("method", methodProp) Then Return

            Dim method = methodProp.GetString()
            If method Is Nothing Then Return

            Dim handlers As List(Of Action(Of JsonElement)) = Nothing
            If Not _notificationHandlers.TryGetValue(method, handlers) Then Return

            Dim paramsProp As JsonElement = Nothing
            root.TryGetProperty("params", paramsProp)

            Dim snapshot As Action(Of JsonElement)()
            SyncLock handlers
                snapshot = handlers.ToArray()
            End SyncLock

            For Each h In snapshot
                Try
                    h(paramsProp)
                Catch
                    ' Swallow handler exceptions to keep the read loop alive
                End Try
            Next
        End Sub

        ' ----- IDisposable -----

        Public Sub Dispose() Implements IDisposable.Dispose
            If _disposed Then Return
            _disposed = True

            _cts.Cancel()

            ' Complete any pending requests
            For Each kvp In _pendingRequests
                kvp.Value.TrySetCanceled()
            Next
            _pendingRequests.Clear()

            _reader.Dispose()
            _writer.Dispose()
        End Sub

        ' ----- Subscription helper -----

        Private Class NotificationSubscription
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

    ''' <summary>
    ''' Exception thrown when the JSON-RPC server returns an error response.
    ''' </summary>
    Public Class JsonRpcException
        Inherits Exception

        Public Sub New(message As String)
            MyBase.New(message)
        End Sub

        Public Sub New(message As String, innerException As Exception)
            MyBase.New(message, innerException)
        End Sub

        ''' <summary>JSON-RPC error code (for example -32601 when the method is unknown).</summary>
        Public Property Code As Integer

        ''' <summary>The error's <c>data</c> member, when the server sent one.</summary>
        Public Property ErrorData As JsonElement?

        ''' <summary>
        ''' Policy Helper failure carried in <c>error.data.policyHelperFailure</c>, for example when
        ''' <c>session.create</c> or <c>session.resume</c> is blocked by managed policy. Nothing otherwise.
        ''' </summary>
        Public ReadOnly Property PolicyHelperFailure As PolicyHelperFailure
            Get
                If Not ErrorData.HasValue OrElse ErrorData.Value.ValueKind <> JsonValueKind.Object Then Return Nothing
                Dim failure As JsonElement = Nothing
                If Not ErrorData.Value.TryGetProperty("policyHelperFailure", failure) Then Return Nothing
                Return Global.GitHub.Copilot.SDK.PolicyHelperFailure.FromJson(failure)
            End Get
        End Property
    End Class

End Namespace

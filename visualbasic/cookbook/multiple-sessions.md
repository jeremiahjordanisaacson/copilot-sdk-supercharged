# Multiple Sessions

Recipes for running and managing multiple sessions in parallel.

## Creating Multiple Sessions

```vb
Using client As New CopilotClient()
    Await client.StartAsync()

    ' Create sessions with different models
    Dim session1 = Await client.CreateSessionAsync(New SessionConfig With {
        .Model = "gpt-4",
        .OnPermissionRequest = PermissionHandlers.ApproveAll()
    })

    Dim session2 = Await client.CreateSessionAsync(New SessionConfig With {
        .Model = "gpt-5",
        .OnPermissionRequest = PermissionHandlers.ApproveAll()
    })

    ' Send messages to both
    Dim task1 = session1.SendAndWaitAsync(
        New MessageOptions With { .Prompt = "Explain VB.NET" })

    Dim task2 = session2.SendAndWaitAsync(
        New MessageOptions With { .Prompt = "Explain F#" })

    Dim results = Await Task.WhenAll(task1, task2)

    Console.WriteLine($"Session 1: {results(0)?.Data?.Content}")
    Console.WriteLine($"Session 2: {results(1)?.Data?.Content}")
End Using
```

## Foreground Session

```vb
' Set which session is in the foreground for the server
Await client.SetForegroundSessionIdAsync(session1.SessionId)

' Switch foreground
Await client.SetForegroundSessionIdAsync(session2.SessionId)
```

## Listing All Sessions

```vb
Dim sessions = Await client.ListSessionsAsync()

For Each meta In sessions
    Console.WriteLine($"  {meta.SessionId}")
    Console.WriteLine($"    Model: {meta.Model}")
    Console.WriteLine($"    Created: {meta.CreatedAt}")
    Console.WriteLine($"    Last active: {meta.LastActiveAt}")
Next
```

## Session Pool Pattern

```vb
Public Class SessionPool
    Implements IDisposable

    Private ReadOnly _client As CopilotClient
    Private ReadOnly _available As New Queue(Of CopilotSession)
    Private ReadOnly _lock As New Object()

    Public Sub New(client As CopilotClient)
        _client = client
    End Sub

    Public Async Function AcquireAsync() As Task(Of CopilotSession)
        SyncLock _lock
            If _available.Count > 0 Then
                Return _available.Dequeue()
            End If
        End SyncLock

        ' Create a new session if none available
        Return Await _client.CreateSessionAsync(New SessionConfig With {
            .OnPermissionRequest = PermissionHandlers.ApproveAll()
        })
    End Function

    Public Sub Release(session As CopilotSession)
        SyncLock _lock
            _available.Enqueue(session)
        End SyncLock
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        SyncLock _lock
            For Each session In _available
                session.Dispose()
            Next
            _available.Clear()
        End SyncLock
    End Sub
End Class
```

## Cleaning Up Stale Sessions

```vb
Dim sessions = Await client.ListSessionsAsync()
For Each meta In sessions
    If meta.LastActiveAt IsNot Nothing Then
        Dim lastActive = DateTime.Parse(meta.LastActiveAt)
        If (DateTime.UtcNow - lastActive).TotalHours > 24 Then
            Console.WriteLine($"Deleting stale session: {meta.SessionId}")
            Await client.DeleteSessionAsync(meta.SessionId)
        End If
    End If
Next
```

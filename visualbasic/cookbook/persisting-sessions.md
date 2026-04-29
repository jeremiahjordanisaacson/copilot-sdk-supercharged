# Persisting Sessions

Recipes for saving and resuming sessions across application restarts.

## Basic Persist and Resume

Sessions are persisted automatically by the CLI server (infinite sessions). To resume
a session in a later run, save its `SessionId` and use `ResumeSessionAsync`.

```vb
' --- First run: create and save ---
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Model = "gpt-4",
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})

' Persist the session ID (file, database, etc.)
File.WriteAllText("last-session.txt", session.SessionId)

' ... use the session ...

' Dispose releases in-memory resources but data stays on disk
session.Dispose()
```

```vb
' --- Second run: resume ---
Dim sessionId = File.ReadAllText("last-session.txt")

Dim session = Await client.ResumeSessionAsync(sessionId, New ResumeSessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})

' Conversation history is intact
Dim messages = Await session.GetMessagesAsync()
Console.WriteLine($"Resumed with {messages.Count} messages")
```

## Workspace Path

When infinite sessions are enabled, each session has a workspace directory:

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})

If session.WorkspacePath IsNot Nothing Then
    Console.WriteLine($"Workspace: {session.WorkspacePath}")
    ' Workspace contains checkpoints/, plan.md, files/
End If
```

## Retrieving Metadata Without Resuming

```vb
Dim metadata = Await client.GetSessionMetadataAsync(savedSessionId)
Console.WriteLine($"Model: {metadata.Model}")
Console.WriteLine($"Created: {metadata.CreatedAt}")
Console.WriteLine($"Last active: {metadata.LastActiveAt}")
```

## Permanent Deletion

To permanently remove a session and all its on-disk data:

```vb
Await client.DeleteSessionAsync(sessionId)
```

## Compaction Events

Long-running sessions may be compacted to reduce context size. Listen for
compaction events to update your UI:

```vb
session.On(Sub(evt)
    If TypeOf evt Is SessionCompactionStartEvent Then
        Console.WriteLine("Compacting session history...")
    ElseIf TypeOf evt Is SessionCompactionCompleteEvent Then
        Console.WriteLine("Compaction complete.")
    End If
End Sub)
```

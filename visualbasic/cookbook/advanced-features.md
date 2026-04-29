# Advanced Features

Complete examples covering all v2.0 features of the Visual Basic Copilot SDK.

## 1. Session Filesystem (SessionFs)

Route session-scoped file I/O through a custom provider:

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .SessionFs = New SessionFsConfig With {
        .InitialCwd = "C:\Projects\MyApp",
        .SessionStatePath = "C:\Users\me\.copilot\session-state"
    }
})
```

## 2. Idle Timeout

Automatically clean up sessions that have been inactive:

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .SessionIdleTimeoutSeconds = 600  ' 10 minutes
})
```

## 3. Skills

Enable named skill sets for the session:

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Skills = New List(Of String) From {"code-review", "testing", "refactoring"},
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

## 4. Session Metadata

Retrieve detailed information about any session:

```vb
Dim metadata = Await client.GetSessionMetadataAsync(session.SessionId)
Console.WriteLine($"Session ID:    {metadata.SessionId}")
Console.WriteLine($"Model:         {metadata.Model}")
Console.WriteLine($"Created:       {metadata.CreatedAt}")
Console.WriteLine($"Last active:   {metadata.LastActiveAt}")
Console.WriteLine($"Workspace:     {metadata.WorkspacePath}")
```

## 5. Commands and Elicitation

Handle structured user input requests from the assistant:

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll(),
    .OnUserInput = Async Function(req)
                       Console.Write($"{req.Prompt}: ")
                       Return Console.ReadLine()
                   End Function,
    .OnElicitation = Async Function(req)
                         Console.WriteLine($"[{req.Title}] {req.Message}")
                         For Each opt In req.Options
                             Console.WriteLine($"  [{opt.Id}] {opt.Label}")
                         Next
                         Console.Write("Choose: ")
                         Dim choice = Console.ReadLine()
                         Return New ElicitationResponse With { .Result = choice }
                     End Function
})
```

## 6. Customize Mode

Enable edit/customize mode for file modifications:

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .CustomizeMode = True,
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})

Await session.SendAsync(New MessageOptions With {
    .Prompt = "Add error handling to this function",
    .Attachments = New List(Of Attachment) From {
        New Attachment With { .Type = "file", .Path = "./src/Main.vb" }
    }
})
```

## 7. Streaming

Receive real-time token-by-token output:

```vb
session.On(Sub(evt)
    If TypeOf evt Is AssistantMessageDeltaEvent Then
        Dim delta = DirectCast(evt, AssistantMessageDeltaEvent)
        Console.Write(delta.Data?.Delta)
    ElseIf TypeOf evt Is AssistantMessageEvent Then
        Console.WriteLine() ' newline after streaming completes
    End If
End Sub)
```

## 8. Infinite Sessions

Sessions persist to disk automatically. Resume later:

```vb
' Create
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
Dim id = session.SessionId
session.Dispose()

' ... application restarts ...

' Resume
Dim resumed = Await client.ResumeSessionAsync(id, New ResumeSessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
Console.WriteLine($"Workspace: {resumed.WorkspacePath}")
```

## 9. Tools

Register custom tools the assistant can call:

```vb
Dim dateTool As New DefineTool With {
    .Name = "get_current_date",
    .Description = "Returns the current date and time",
    .ParametersSchema = New Dictionary(Of String, Object) From {
        {"type", "object"},
        {"properties", New Dictionary(Of String, Object)()}
    },
    .Handler = Async Function(input)
                   Return DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
               End Function
}

Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Tools = New List(Of DefineTool) From {dateTool},
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

## 10. Permissions

Control which actions the assistant is allowed to perform:

```vb
' Selective permission handler
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .OnPermissionRequest = Async Function(req)
        ' Allow read operations, deny writes
        If req.Tool IsNot Nothing AndAlso req.Tool.StartsWith("write") Then
            Return New PermissionResponse With {
                .Allowed = False,
                .Reason = "Write operations are not allowed"
            }
        End If
        Return New PermissionResponse With { .Allowed = True }
    End Function
})
```

## 11. Agent Mode

Enable full agent capabilities:

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .AgentMode = True,
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

## 12. Custom Providers

Use a custom model provider by connecting to an external server:

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .CliUrl = "localhost:8080"
})
```

## 13. Telemetry

Export OpenTelemetry traces for observability:

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .Telemetry = New TelemetryConfig With {
        .ExporterType = "otlp-http",
        .OtlpEndpoint = "http://localhost:4318",
        .SourceName = "my-vb-app",
        .CaptureContent = True
    }
})
```

## 14. Reasoning Events

Listen for the model's chain-of-thought reasoning:

```vb
session.On(Of AssistantReasoningEvent)(Sub(evt)
    Console.WriteLine($"[Reasoning] {evt.Data?.Content}")
End Sub)
```

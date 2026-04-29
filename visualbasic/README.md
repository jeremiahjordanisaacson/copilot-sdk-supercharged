# Copilot SDK for Visual Basic .NET

Visual Basic .NET SDK for programmatic control of the GitHub Copilot CLI.

> **Note:** This SDK is in public preview and may change in breaking ways.

## Installation

```bash
dotnet add package CopilotSDK.Supercharged.VB
```

Or add a project reference when building from source:

```xml
<ProjectReference Include="..\visualbasic\CopilotSDK.Supercharged.VB.vbproj" />
```

## Quick Start

```vb
Imports GitHub.Copilot.SDK

' Create and start client
Using client As New CopilotClient()
    Await client.StartAsync()

    ' Create a session (OnPermissionRequest is required)
    Dim session = Await client.CreateSessionAsync(New SessionConfig With {
        .Model = "gpt-4",
        .OnPermissionRequest = PermissionHandlers.ApproveAll()
    })

    ' Listen for events
    Dim done As New TaskCompletionSource(Of Boolean)()

    session.On(Sub(evt)
        If TypeOf evt Is AssistantMessageEvent Then
            Console.WriteLine(DirectCast(evt, AssistantMessageEvent).Data?.Content)
        ElseIf TypeOf evt Is SessionIdleEvent Then
            done.TrySetResult(True)
        End If
    End Sub)

    ' Send a message and wait for completion
    Await session.SendAsync(New MessageOptions With { .Prompt = "What is 2+2?" })
    Await done.Task
End Using
```

## API Reference

### CopilotClient

#### Constructor

```vb
Dim client As New CopilotClient(Optional options As CopilotClientOptions = Nothing)
```

**Options:**

| Property | Type | Default | Description |
|---|---|---|---|
| `CliPath` | `String` | env or `"copilot"` | Path to CLI executable |
| `CliArgs` | `String()` | `Nothing` | Extra arguments for the CLI process |
| `CliUrl` | `String` | `Nothing` | URL of existing CLI server |
| `Port` | `Integer` | `0` | Server port (0 = random) |
| `UseStdio` | `Boolean` | `True` | Use stdio transport |
| `LogLevel` | `String` | `"info"` | Log level |
| `AutoStart` | `Boolean` | `True` | Auto-start server when needed |
| `Cwd` | `String` | `Nothing` | Working directory |
| `Environment` | `Dictionary(Of String, String)` | `Nothing` | Env vars for CLI |
| `GitHubToken` | `String` | `Nothing` | GitHub token for auth |
| `UseLoggedInUser` | `Boolean?` | `Nothing` | Use logged-in user auth |
| `Streaming` | `Boolean` | `False` | Enable streaming deltas |
| `SessionFs` | `SessionFsConfig` | `Nothing` | Session filesystem config |
| `Telemetry` | `TelemetryConfig` | `Nothing` | OpenTelemetry config |
| `SessionIdleTimeoutSeconds` | `Integer?` | `Nothing` | Idle timeout in seconds |

#### Methods

##### StartAsync

```vb
Await client.StartAsync(Optional cancellationToken As CancellationToken = Nothing)
```

Starts the CLI server process and connects. Called automatically if `AutoStart` is `True`.

##### StopAsync

```vb
Await client.StopAsync()
```

Gracefully shuts down all sessions and kills the CLI process.

##### CreateSessionAsync

```vb
Dim session = Await client.CreateSessionAsync(Optional config As SessionConfig = Nothing)
```

Creates a new conversation session. See [SessionConfig](#sessionconfig) for options.

##### ResumeSessionAsync

```vb
Dim session = Await client.ResumeSessionAsync(sessionId, Optional config As ResumeSessionConfig = Nothing)
```

Resumes a previously created session by its ID.

##### GetSessionMetadataAsync

```vb
Dim metadata = Await client.GetSessionMetadataAsync(sessionId)
```

Returns metadata (creation time, model, workspace path) for a session.

##### SetForegroundSessionIdAsync

```vb
Await client.SetForegroundSessionIdAsync(sessionId)
```

Tells the server which session is in the foreground.

##### ListSessionsAsync

```vb
Dim sessions = Await client.ListSessionsAsync()
```

Lists all sessions on the server.

##### DeleteSessionAsync

```vb
Await client.DeleteSessionAsync(sessionId)
```

Permanently deletes a session and its on-disk data.

##### PingAsync

```vb
Dim response = Await client.PingAsync(Optional message As String = Nothing)
```

Pings the CLI server. Returns a `PingResponse` with `Message` and `ServerTime`.

##### ListModelsAsync

```vb
Dim models = Await client.ListModelsAsync()
```

Lists available models from the server.

---

### CopilotSession

#### Properties

| Property | Type | Description |
|---|---|---|
| `SessionId` | `String` | Unique session identifier |
| `WorkspacePath` | `String` | Workspace path (infinite sessions) |
| `Capabilities` | `SessionCapabilities` | Host capabilities |

#### Methods

##### SendAsync

```vb
Dim messageId = Await session.SendAsync(New MessageOptions With { .Prompt = "Hello" })
```

Sends a message and returns immediately with the message ID.

##### SendAndWaitAsync

```vb
Dim response = Await session.SendAndWaitAsync(
    New MessageOptions With { .Prompt = "Hello" },
    Optional timeout As TimeSpan? = Nothing
)
```

Sends a message and waits until the session becomes idle. Returns the final `AssistantMessageEvent`.

##### On

```vb
Dim subscription = session.On(Sub(evt)
    ' Handle all events
End Sub)

' Or typed:
Dim sub2 = session.On(Of AssistantMessageEvent)(Sub(msg)
    Console.WriteLine(msg.Data?.Content)
End Sub)
```

Registers a callback for session events. Returns an `IDisposable` for unsubscribing.

##### RegisterTool

```vb
session.RegisterTool(New DefineTool With {
    .Name = "my_tool",
    .Description = "Does something",
    .Handler = Async Function(input) "result"
})
```

##### AbortAsync

```vb
Await session.AbortAsync()
```

Aborts the current in-flight operation.

##### GetMessagesAsync

```vb
Dim messages = Await session.GetMessagesAsync()
```

Retrieves the conversation message history.

##### DisconnectAsync

```vb
Await session.DisconnectAsync()
```

Disconnects from the session without deleting server-side data.

---

### SessionConfig

| Property | Type | Description |
|---|---|---|
| `Model` | `String` | Model to use (e.g. "gpt-4") |
| `SystemMessage` | `String` | System message prepended to conversation |
| `OnPermissionRequest` | `PermissionHandler` | Permission request handler |
| `OnUserInput` | `Func(Of UserInputRequest, Task(Of String))` | User input handler |
| `OnElicitation` | `Func(Of ElicitationRequest, Task(Of ElicitationResponse))` | Elicitation handler |
| `Tools` | `List(Of DefineTool)` | Custom tools |
| `Skills` | `List(Of String)` | Enabled skills |
| `CustomizeMode` | `Boolean` | Enable customize/edit mode |
| `AgentMode` | `Boolean` | Enable agent mode |
| `Attachments` | `List(Of Attachment)` | Initial attachments |

---

### Event Types

| Event Class | Type String | Description |
|---|---|---|
| `AssistantMessageEvent` | `assistant.message` | Complete assistant message |
| `AssistantMessageDeltaEvent` | `assistant.message_delta` | Streaming partial message |
| `AssistantReasoningEvent` | `assistant.reasoning` | Reasoning content |
| `SessionIdleEvent` | `session.idle` | Session finished processing |
| `SessionErrorEvent` | `session.error` | Session error |
| `ToolCallEvent` | `tool.call` | Tool invocation |
| `SessionCompactionStartEvent` | `session.compaction_start` | Compaction started |
| `SessionCompactionCompleteEvent` | `session.compaction_complete` | Compaction finished |

---

### DefineTool

```vb
Dim tool As New DefineTool With {
    .Name = "calculator",
    .Description = "Evaluate a math expression",
    .ParametersSchema = New Dictionary(Of String, Object) From {
        {"type", "object"},
        {"properties", New Dictionary(Of String, Object) From {
            {"expression", New Dictionary(Of String, Object) From {
                {"type", "string"},
                {"description", "Math expression to evaluate"}
            }}
        }},
        {"required", New String() {"expression"}}
    },
    .Handler = Async Function(input)
                   Return "42"
               End Function
}
```

---

### Permission Handling

```vb
' Approve everything
Dim config As New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
}

' Deny everything
Dim config2 As New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.DenyAll()
}

' Custom logic
Dim config3 As New SessionConfig With {
    .OnPermissionRequest = Async Function(req)
        If req.Tool = "dangerous_tool" Then
            Return New PermissionResponse With { .Allowed = False, .Reason = "blocked" }
        End If
        Return New PermissionResponse With { .Allowed = True }
    End Function
}
```

---

## v2.0 Features

### Session Filesystem (SessionFs)

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .SessionFs = New SessionFsConfig With {
        .InitialCwd = "C:\MyProject",
        .SessionStatePath = "C:\SessionData"
    }
})
```

### Idle Timeout

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .SessionIdleTimeoutSeconds = 300
})
```

### Skills

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Skills = New List(Of String) From {"code-review", "testing"}
})
```

### Session Metadata

```vb
Dim metadata = Await client.GetSessionMetadataAsync(session.SessionId)
Console.WriteLine($"Created: {metadata.CreatedAt}")
Console.WriteLine($"Model: {metadata.Model}")
```

### Customize Mode

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .CustomizeMode = True
})
```

### Agent Mode

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .AgentMode = True,
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### Streaming

Subscribe to delta events for real-time streaming:

```vb
session.On(Of AssistantMessageDeltaEvent)(Sub(delta)
    Console.Write(delta.Data?.Delta)
End Sub)
```

### Infinite Sessions

Sessions persist by default. Resume any session later:

```vb
Dim session = Await client.ResumeSessionAsync("previous-session-id")
Console.WriteLine($"Workspace: {session.WorkspacePath}")
```

### Tools

Register custom tools for the assistant to invoke:

```vb
Dim tool As New DefineTool("search_db", "Search the database",
    schema, Async Function(input)
        Return "found 42 results"
    End Function)

Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Tools = New List(Of DefineTool) From {tool},
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### Telemetry

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .Telemetry = New TelemetryConfig With {
        .ExporterType = "otlp-http",
        .OtlpEndpoint = "http://localhost:4318",
        .SourceName = "my-app"
    }
})
```

### Multiple Sessions

```vb
Dim session1 = Await client.CreateSessionAsync(New SessionConfig With { .Model = "gpt-4" })
Dim session2 = Await client.CreateSessionAsync(New SessionConfig With { .Model = "gpt-5" })

Await client.SetForegroundSessionIdAsync(session1.SessionId)
```

### Listing and Deleting Sessions

```vb
Dim sessions = Await client.ListSessionsAsync()
For Each meta In sessions
    Console.WriteLine($"{meta.SessionId} - {meta.Model}")
Next

Await client.DeleteSessionAsync(sessions(0).SessionId)
```

---

## Building from Source

```bash
cd visualbasic
dotnet restore
dotnet build
```

## Running Tests

```bash
cd visualbasic
dotnet test tests/CopilotSDK.Supercharged.VB.Tests.vbproj
```

## License

MIT

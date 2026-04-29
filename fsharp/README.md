# Copilot Supercharged SDK for F#

An idiomatic F# client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

Add a project reference (from the repo):

```bash
dotnet add reference ../fsharp/src/CopilotSDK.Supercharged.FSharp.fsproj
```

Or if published to NuGet:

```bash
dotnet add package CopilotSDK.Supercharged.FSharp
```

## Quick Start

```fsharp
open CopilotSDK.Supercharged.FSharp

async {
    // Create and start the client
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    // Create a session with default settings (approves all permissions)
    let! session =
        client.CreateSessionAsync(SessionConfig.defaults)

    // Subscribe to events using pattern matching
    use _ = session.On(function
        | AssistantMessage data -> printfn "Assistant: %s" data.Content
        | AssistantReasoning data -> printfn "[reasoning] %s" data.Content
        | ToolExecutionStart data -> printfn "[tool] %s" data.ToolName
        | SessionIdle -> printfn "-- idle --"
        | _ -> ())

    // Send a message and wait for the response
    let! reply =
        session.SendAndWaitAsync({ Prompt = "What is 2 + 2?"; Attachments = None; Mode = None })

    reply |> Option.iter (fun data -> printfn "Final answer: %s" data.Content)

    // Clean up
    do! client.StopAsync()
} |> Async.RunSynchronously
```

## API Reference

### CopilotClient

The main entry point for connecting to the Copilot CLI server.

#### Creating a Client

```fsharp
// With default options (spawns CLI server, stdio transport)
use client = CopilotClient.create CopilotClientOptions.defaults

// With a custom CLI path
use client =
    CopilotClient.create
        { CopilotClientOptions.defaults with
            CliPath = Some "/usr/local/bin/copilot-cli" }

// Connect to an existing server
use client =
    CopilotClient.create
        { CopilotClientOptions.defaults with
            CliUrl = Some "localhost:3000" }
```

#### CopilotClientOptions

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `CliPath` | `string option` | `None` | Path to CLI executable (falls back to `COPILOT_CLI_PATH` env) |
| `CliArgs` | `string list` | `[]` | Extra CLI arguments |
| `Cwd` | `string option` | `None` | Working directory for the CLI process |
| `Port` | `int` | `0` | Server port (0 = random) |
| `UseStdio` | `bool` | `true` | Use stdio transport instead of TCP |
| `CliUrl` | `string option` | `None` | URL of existing CLI server |
| `LogLevel` | `string` | `"info"` | Log level |
| `AutoStart` | `bool` | `true` | Auto-start server |
| `Environment` | `IDictionary option` | `None` | Environment variables for the CLI |
| `GitHubToken` | `string option` | `None` | GitHub token for authentication |
| `UseLoggedInUser` | `bool option` | `None` | Use stored OAuth/gh CLI tokens |
| `Telemetry` | `TelemetryConfig option` | `None` | OpenTelemetry configuration |
| `SessionIdleTimeoutSeconds` | `int option` | `None` | Idle timeout in seconds |

#### Client Methods

```fsharp
// Start and stop
do! client.StartAsync()
do! client.StopAsync()

// Session management
let! session = client.CreateSessionAsync(config)
let! session = client.ResumeSessionAsync(resumeConfig)
let! meta = client.GetSessionMetadataAsync(sessionId)
do! client.DeleteSessionAsync(sessionId)

// Server info
let! models = client.ListModelsAsync()
let! authStatus = client.GetAuthStatusAsync()
let! status = client.GetStatusAsync()
```

### CopilotSession

Represents a single conversation session.

#### Sending Messages

```fsharp
// Fire and forget (returns message ID)
let! messageId = session.SendAsync({ Prompt = "Hello"; Attachments = None; Mode = None })

// Send and wait for the response (blocks until session.idle)
let! reply =
    session.SendAndWaitAsync(
        { Prompt = "Hello"; Attachments = None; Mode = None },
        timeout = TimeSpan.FromSeconds(120.0))
```

#### Subscribing to Events

F# discriminated unions make event handling clean and exhaustive:

```fsharp
// Pattern matching handler
use _ = session.On(function
    | AssistantMessage data ->
        printfn "Message: %s" data.Content
    | AssistantMessageDelta data ->
        printf "%s" data.Content  // streaming delta
    | AssistantReasoning data ->
        printfn "[thinking] %s" data.Content
    | ToolExecutionStart data ->
        printfn "[tool start] %s" data.ToolName
    | ToolExecutionComplete data ->
        printfn "[tool done] %s" data.ToolName
    | SessionIdle ->
        printfn "-- session idle --"
    | SessionCompactionStart _ ->
        printfn "[compaction started]"
    | SessionCompactionComplete _ ->
        printfn "[compaction complete]"
    | _ -> ())
```

Using the `IObservable` interface with `|>` pipes:

```fsharp
open System

session.Events
|> Observable.filter SessionEvent.isIdle
|> Observable.subscribe (fun _ -> printfn "Session is idle")
|> ignore
```

#### SessionEvent (Discriminated Union)

| Case | Payload | Description |
|------|---------|-------------|
| `AssistantMessage` | `AssistantMessageData` | Final assistant response |
| `AssistantMessageDelta` | `AssistantMessageDeltaData` | Streaming text chunk |
| `AssistantReasoning` | `AssistantReasoningData` | Final reasoning text |
| `AssistantReasoningDelta` | `AssistantMessageDeltaData` | Streaming reasoning chunk |
| `ToolExecutionStart` | `ToolExecutionStartData` | Tool invocation started |
| `ToolExecutionComplete` | `ToolExecutionCompleteData` | Tool invocation finished |
| `SessionIdle` | (none) | Session finished processing |
| `SessionCompactionStart` | `SessionCompactionData` | Compaction started |
| `SessionCompactionComplete` | `SessionCompactionData` | Compaction finished |
| `UnknownEvent` | `string * JsonElement option` | Unrecognized event type |

Helper functions in the `SessionEvent` module:

```fsharp
SessionEvent.tryGetMessageContent event  // string option
SessionEvent.tryGetDeltaContent event    // string option
SessionEvent.isIdle event               // bool
```

### Tools

Register tools that the assistant can call using the `DefineTool` module:

```fsharp
// Simple tool with no parameters
let greetTool =
    DefineTool.createSimple "greet" "Say hello" (fun () -> async {
        return "Hello from F#!"
    })

// Tool with typed arguments
type WeatherArgs = { City: string }

let weatherTool =
    DefineTool.createTyped<WeatherArgs, ToolResultObject>
        "get_weather"
        "Get weather for a city"
        None
        (fun args -> async {
            return ToolResultObject.success (sprintf "Sunny in %s" args.City)
        })

// Pipe-friendly builder
let calculatorTool =
    DefineTool.define "calculate" "Evaluate a math expression"
    |> DefineTool.withHandler (fun inv -> async {
        return ToolResultObject.success "42" :> obj
    })
    |> DefineTool.build

// Register tools on a session
session.RegisterTools [greetTool; weatherTool; calculatorTool]
```

### Permission Handling

```fsharp
// Built-in handlers
SessionConfig.approveAll   // Approves every permission request
SessionConfig.rejectAll    // Rejects every permission request

// Custom handler
let myHandler : PermissionRequestHandler =
    fun request -> async {
        if request.ToolName = "dangerous_tool" then
            return { Kind = Reject; UpdraftMessage = Some "Not allowed" }
        else
            return { Kind = ApproveOnce; UpdraftMessage = None }
    }
```

## F# Idioms

This SDK is designed around F# best practices:

- **Discriminated Unions** for `SessionEvent` - enables exhaustive pattern matching
- **Immutable Records** for all configuration types
- **Async Workflows** (`async { }`) for all asynchronous operations
- **Pipe Operators** (`|>`) for composable tool building and event filtering
- **Option Types** instead of nulls
- **Module Functions** (`CopilotClient.create`, `SessionConfig.defaults`) for a functional API
- **IObservable** integration for reactive event streams
- **Result<'T,'E>** pattern available via `ToolResultObject.success` / `ToolResultObject.failure`

## Building

```bash
cd fsharp/src
dotnet build
```

## Testing

```bash
cd fsharp/tests
dotnet test
```

## Examples

See [`examples/BasicExample.fsx`](examples/BasicExample.fsx) for a runnable F# script.

See the [`cookbook/`](cookbook/) directory for recipes covering common patterns.

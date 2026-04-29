# Advanced Features

Advanced patterns for the F# Copilot SDK.

## Custom Permission Handler

```fsharp
open CopilotSDK.Supercharged.FSharp

let auditingHandler : PermissionRequestHandler =
    fun request -> async {
        printfn "[AUDIT] Tool: %s, CallId: %s" request.ToolName request.ToolCallId
        // Approve read-only tools, reject everything else
        if request.ToolName.StartsWith("read_") || request.ToolName.StartsWith("get_") then
            return { Kind = ApproveOnce; UpdraftMessage = None }
        else
            return { Kind = Reject; UpdraftMessage = Some "Only read operations are allowed" }
    }

let config =
    { SessionConfig.defaults with
        OnPermissionRequest = auditingHandler }
```

## Custom System Messages

```fsharp
let expertConfig =
    { SessionConfig.defaults with
        SystemMessage = Some "You are an expert F# developer. Always use idiomatic F# patterns including computation expressions, pipe operators, and discriminated unions. Prefer immutable data." }
```

## Connecting to an External Server

```fsharp
let remoteOptions =
    { CopilotClientOptions.defaults with
        CliUrl = Some "my-server.example.com:8080" }

use client = CopilotClient.create remoteOptions
// No CLI process is spawned; connects to the remote server.
```

## Authentication with GitHub Token

```fsharp
let tokenOptions =
    { CopilotClientOptions.defaults with
        GitHubToken = Some (System.Environment.GetEnvironmentVariable("GITHUB_TOKEN")) }
```

## Session with Attachments

```fsharp
async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()
    let! session = client.CreateSessionAsync(SessionConfig.defaults)

    let! reply =
        session.SendAndWaitAsync(
            { Prompt = "Review this code"
              Attachments = Some [
                { Type = "file"; Path = Some "./src/Client.fs"; Url = None; Content = None }
              ]
              Mode = None })

    reply |> Option.iter (fun d -> printfn "%s" d.Content)
    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Conversation History

```fsharp
async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()
    let! session = client.CreateSessionAsync(SessionConfig.defaults)

    // Have a conversation
    let! _ =
        session.SendAndWaitAsync(
            { Prompt = "My name is Ada"; Attachments = None; Mode = None })
    let! _ =
        session.SendAndWaitAsync(
            { Prompt = "What is 2 + 2?"; Attachments = None; Mode = None })

    // Retrieve history
    let! history = session.GetHistoryAsync()
    printfn "History: %s" (history.GetRawText())

    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Listing Models

```fsharp
async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let! models = client.ListModelsAsync()
    models |> List.iter (fun m ->
        let name = m.Name |> Option.defaultValue "(unnamed)"
        printfn "Model: %s (%s)" m.Id name)

    do! client.StopAsync()
} |> Async.RunSynchronously
```

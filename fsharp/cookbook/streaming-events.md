# Streaming Events

Handle real-time streaming deltas and events from the assistant.

## Basic Streaming

```fsharp
open CopilotSDK.Supercharged.FSharp

async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let! session =
        client.CreateSessionAsync(
            { SessionConfig.defaults with Streaming = true })

    // Stream deltas to the console in real time
    use _ = session.On(function
        | AssistantMessageDelta data ->
            printf "%s" data.Content
        | AssistantMessage _ ->
            printfn ""  // newline after the final message
        | SessionIdle ->
            printfn "\n-- done --"
        | _ -> ())

    let! _ =
        session.SendAndWaitAsync(
            { Prompt = "Write a haiku about functional programming"
              Attachments = None; Mode = None })

    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Collecting Streaming Content

```fsharp
open System.Text

async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let! session =
        client.CreateSessionAsync(
            { SessionConfig.defaults with Streaming = true })

    let buffer = StringBuilder()

    use _ = session.On(function
        | AssistantMessageDelta data ->
            buffer.Append(data.Content) |> ignore
        | SessionIdle ->
            printfn "Full response:\n%s" (buffer.ToString())
            buffer.Clear() |> ignore
        | _ -> ())

    let! _ =
        session.SendAndWaitAsync(
            { Prompt = "Explain pattern matching in F#"
              Attachments = None; Mode = None })

    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Observable Filtering with Pipes

```fsharp
open System

async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let! session =
        client.CreateSessionAsync(SessionConfig.defaults)

    // Only observe tool events using pipes
    session.Events
    |> Observable.choose (function
        | ToolExecutionStart data -> Some (sprintf "START: %s" data.ToolName)
        | ToolExecutionComplete data -> Some (sprintf "DONE:  %s" data.ToolName)
        | _ -> None)
    |> Observable.subscribe (printfn "[Tool] %s")
    |> ignore

    // Only observe reasoning
    session.Events
    |> Observable.choose (fun evt ->
        match evt with
        | AssistantReasoning data -> Some data.Content
        | _ -> None)
    |> Observable.subscribe (printfn "[Thinking] %s")
    |> ignore

    let! _ =
        session.SendAndWaitAsync(
            { Prompt = "What tools are available?"
              Attachments = None; Mode = None })

    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Reasoning Events

```fsharp
use _ = session.On(function
    | AssistantReasoningDelta data ->
        printf "[think] %s" data.Content
    | AssistantReasoning data ->
        printfn "\n[thought complete] %s" data.Content
    | _ -> ())
```

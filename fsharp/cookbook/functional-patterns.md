# Functional Patterns

Leverage F# functional programming patterns with the Copilot SDK.

## Computation Expression Wrappers

```fsharp
open CopilotSDK.Supercharged.FSharp

/// Send multiple prompts in sequence, collecting all replies.
let conversation (session: CopilotSession) (prompts: string list) = async {
    let results = ResizeArray<string option>()
    for prompt in prompts do
        let! reply =
            session.SendAndWaitAsync(
                { Prompt = prompt; Attachments = None; Mode = None })
        results.Add(reply |> Option.map (fun d -> d.Content))
    return results |> Seq.toList
}
```

## Railway-Oriented Session Management

```fsharp
open System

type SessionError =
    | ConnectionFailed of string
    | SessionCreationFailed of string
    | SendFailed of string
    | Timeout

let runSession prompt = async {
    try
        use client = CopilotClient.create CopilotClientOptions.defaults
        try
            do! client.StartAsync()
        with ex ->
            return Error (ConnectionFailed ex.Message)

        let! session =
            try
                client.CreateSessionAsync(SessionConfig.defaults) |> Async.map Ok
            with ex ->
                async { return Error (SessionCreationFailed ex.Message) }

        match session with
        | Error e -> return Error e
        | Ok session ->
            try
                let! reply =
                    session.SendAndWaitAsync(
                        { Prompt = prompt; Attachments = None; Mode = None },
                        timeout = TimeSpan.FromSeconds(30.0))
                match reply with
                | Some data -> return Ok data.Content
                | None -> return Error (SendFailed "No response")
            with
            | :? TimeoutException -> return Error Timeout
            | ex -> return Error (SendFailed ex.Message)
    with ex ->
        return Error (ConnectionFailed ex.Message)
}
```

## Event Aggregation with Pipes

```fsharp
open System.Collections.Generic

/// Collect all events for a single send-and-wait cycle.
let collectEvents (session: CopilotSession) prompt = async {
    let events = ResizeArray<SessionEvent>()
    use _ = session.On(events.Add)

    let! _ =
        session.SendAndWaitAsync(
            { Prompt = prompt; Attachments = None; Mode = None })

    return events |> Seq.toList
}

/// Summarize which event types were seen.
let summarizeEvents (events: SessionEvent list) =
    events
    |> List.choose (function
        | AssistantMessage _ -> Some "assistant.message"
        | AssistantMessageDelta _ -> Some "assistant.message_delta"
        | ToolExecutionStart d -> Some (sprintf "tool.start(%s)" d.ToolName)
        | ToolExecutionComplete d -> Some (sprintf "tool.complete(%s)" d.ToolName)
        | SessionIdle -> Some "session.idle"
        | _ -> None)
    |> List.countBy id
    |> List.sortByDescending snd
```

## Composable Tool Pipelines

```fsharp
/// Chain multiple tool results by piping through transformations.
let chainTools (session: CopilotSession) steps = async {
    let mutable context = ""
    for (prompt, transform) in steps do
        let fullPrompt =
            if context = "" then prompt
            else sprintf "%s\n\nContext: %s" prompt context
        let! reply =
            session.SendAndWaitAsync(
                { Prompt = fullPrompt; Attachments = None; Mode = None })
        context <-
            reply
            |> Option.map (fun d -> d.Content)
            |> Option.defaultValue ""
            |> transform
    return context
}

// Usage:
// let! result =
//     chainTools session [
//         "Generate a list of 5 topics", id
//         "Pick the most interesting topic", fun s -> s.Trim()
//         "Write a paragraph about it", id
//     ]
```

## Active Patterns for Events

```fsharp
let (|TextContent|_|) = function
    | AssistantMessage data -> Some data.Content
    | AssistantMessageDelta data -> Some data.Content
    | _ -> None

let (|ToolEvent|_|) = function
    | ToolExecutionStart data -> Some (data.ToolName, "start")
    | ToolExecutionComplete data -> Some (data.ToolName, "complete")
    | _ -> None

// Usage:
use _ = session.On(function
    | TextContent text -> printf "%s" text
    | ToolEvent (name, phase) -> printfn "[%s] %s" phase name
    | SessionIdle -> printfn "\ndone"
    | _ -> ())
```

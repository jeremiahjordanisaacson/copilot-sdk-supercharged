# Multiple Sessions

Manage multiple concurrent conversations with the Copilot CLI.

## Creating Multiple Sessions

```fsharp
open CopilotSDK.Supercharged.FSharp

async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    // Create two sessions with different models
    let! codeSession =
        client.CreateSessionAsync(
            { SessionConfig.defaults with Model = Some "gpt-4" })

    let! chatSession =
        client.CreateSessionAsync(
            { SessionConfig.defaults with
                Model = Some "gpt-4"
                SystemMessage = Some "You are a friendly chat assistant." })

    printfn "Code session: %s" codeSession.SessionId
    printfn "Chat session: %s" chatSession.SessionId

    // Use them independently
    let! codeReply =
        codeSession.SendAndWaitAsync(
            { Prompt = "Write a fibonacci function in F#"
              Attachments = None; Mode = None })

    let! chatReply =
        chatSession.SendAndWaitAsync(
            { Prompt = "Tell me a programming joke"
              Attachments = None; Mode = None })

    codeReply |> Option.iter (fun d -> printfn "Code: %s" d.Content)
    chatReply |> Option.iter (fun d -> printfn "Chat: %s" d.Content)

    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Parallel Sessions

```fsharp
async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let prompts = [
        "Explain monads in 2 sentences"
        "What is tail recursion?"
        "Compare async and task in F#"
    ]

    // Create sessions and send messages in parallel
    let! results =
        prompts
        |> List.map (fun prompt -> async {
            let! session = client.CreateSessionAsync(SessionConfig.defaults)
            let! reply =
                session.SendAndWaitAsync(
                    { Prompt = prompt; Attachments = None; Mode = None })
            return prompt, reply
        })
        |> Async.Parallel

    results
    |> Array.iter (fun (prompt, reply) ->
        printfn "Q: %s" prompt
        match reply with
        | Some data -> printfn "A: %s\n" data.Content
        | None -> printfn "A: (no response)\n")

    do! client.StopAsync()
} |> Async.RunSynchronously
```

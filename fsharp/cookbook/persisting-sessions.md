# Persisting Sessions

Resume conversations across process restarts using infinite sessions.

## Saving and Resuming

```fsharp
open CopilotSDK.Supercharged.FSharp
open System.IO

let sessionIdFile = "last-session-id.txt"

/// Start a new session and save its ID to disk.
let startNewSession () = async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let! session = client.CreateSessionAsync(SessionConfig.defaults)
    File.WriteAllText(sessionIdFile, session.SessionId)
    printfn "Created session: %s" session.SessionId

    // Send an initial message
    let! reply =
        session.SendAndWaitAsync(
            { Prompt = "Remember that my name is Ada."
              Attachments = None; Mode = None })
    reply |> Option.iter (fun d -> printfn "Assistant: %s" d.Content)

    // Session data persists on disk
    session.WorkspacePath |> Option.iter (fun p -> printfn "Workspace: %s" p)

    do! client.StopAsync()
}

/// Resume a previously saved session.
let resumeSession () = async {
    if not (File.Exists sessionIdFile) then
        printfn "No saved session found."
    else
        let savedId = File.ReadAllText(sessionIdFile).Trim()
        printfn "Resuming session: %s" savedId

        use client = CopilotClient.create CopilotClientOptions.defaults
        do! client.StartAsync()

        let! session =
            client.ResumeSessionAsync(
                { SessionId = savedId
                  OnPermissionRequest = SessionConfig.approveAll
                  Model = None })

        let! reply =
            session.SendAndWaitAsync(
                { Prompt = "What is my name?"
                  Attachments = None; Mode = None })
        reply |> Option.iter (fun d -> printfn "Assistant: %s" d.Content)

        do! client.StopAsync()
}
```

## Session Metadata

```fsharp
async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    let! session = client.CreateSessionAsync(SessionConfig.defaults)

    // After some conversation...
    let! _ =
        session.SendAndWaitAsync(
            { Prompt = "Help me write a web scraper in F#"
              Attachments = None; Mode = None })

    // Retrieve metadata
    let! meta = client.GetSessionMetadataAsync(session.SessionId)
    meta |> Option.iter (fun m ->
        printfn "Session: %s" m.SessionId
        m.Summary |> Option.iter (fun s -> printfn "Summary: %s" s)
        m.CreatedAt |> Option.iter (fun t -> printfn "Created: %s" t))

    do! client.StopAsync()
} |> Async.RunSynchronously
```

## Deleting Sessions

```fsharp
async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    // Delete a session permanently (removes persisted data)
    do! client.DeleteSessionAsync("session-id-to-delete")
    printfn "Session deleted."

    do! client.StopAsync()
} |> Async.RunSynchronously
```

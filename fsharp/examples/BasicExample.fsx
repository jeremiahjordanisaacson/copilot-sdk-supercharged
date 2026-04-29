#!/usr/bin/env dotnet fsi

// Basic F# example for the Copilot Supercharged SDK
//
// Prerequisites:
//   1. Build the SDK: cd fsharp/src && dotnet build
//   2. Set COPILOT_CLI_PATH or pass CliPath in options
//
// Run:
//   dotnet fsi examples/BasicExample.fsx

#r "../src/bin/Debug/net8.0/CopilotSDK.Supercharged.FSharp.dll"

open CopilotSDK.Supercharged.FSharp
open System

// ---------------------------------------------------------------
// 1. Create and start the client
// ---------------------------------------------------------------
let runExample () = async {
    printfn "Starting Copilot client..."

    let options =
        { CopilotClientOptions.defaults with
            LogLevel = "info" }

    use client = CopilotClient.create options
    do! client.StartAsync()
    printfn "Client connected."

    // ---------------------------------------------------------------
    // 2. Create a session
    // ---------------------------------------------------------------
    let sessionConfig =
        { SessionConfig.defaults with
            Model = Some "gpt-4"
            OnPermissionRequest = SessionConfig.approveAll }

    let! session = client.CreateSessionAsync(sessionConfig)
    printfn "Session created: %s" session.SessionId

    // ---------------------------------------------------------------
    // 3. Subscribe to events with pattern matching
    // ---------------------------------------------------------------
    use _ = session.On(function
        | AssistantMessage data ->
            printfn "\n[Assistant] %s" data.Content
        | AssistantMessageDelta data ->
            printf "%s" data.Content
        | AssistantReasoning data ->
            printfn "[Reasoning] %s" data.Content
        | ToolExecutionStart data ->
            printfn "[Tool] Starting: %s" data.ToolName
        | ToolExecutionComplete data ->
            printfn "[Tool] Completed: %s" data.ToolName
        | SessionIdle ->
            printfn "\n-- Session idle --"
        | SessionCompactionStart _ ->
            printfn "[Compaction started]"
        | SessionCompactionComplete _ ->
            printfn "[Compaction complete]"
        | UnknownEvent (t, _) ->
            printfn "[Unknown event: %s]" t
        | _ -> ())

    // ---------------------------------------------------------------
    // 4. Register a simple tool
    // ---------------------------------------------------------------
    let timeTool =
        DefineTool.createSimple "current_time" "Get the current time" (fun () -> async {
            return DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
        })

    session.RegisterTool(timeTool)
    printfn "Registered tool: current_time"

    // ---------------------------------------------------------------
    // 5. Send a message and wait for the reply
    // ---------------------------------------------------------------
    printfn "\nSending message..."
    let! reply =
        session.SendAndWaitAsync(
            { Prompt = "What time is it? Use the current_time tool."
              Attachments = None
              Mode = None },
            timeout = TimeSpan.FromSeconds(30.0))

    match reply with
    | Some data -> printfn "\nFinal response: %s" data.Content
    | None -> printfn "\nNo response received."

    // ---------------------------------------------------------------
    // 6. Interactive loop (optional)
    // ---------------------------------------------------------------
    printfn "\nType a message (or 'quit' to exit):"
    let mutable keepGoing = true
    while keepGoing do
        printf "You: "
        let input = Console.ReadLine()
        if isNull input || input.Trim().ToLower() = "quit" then
            keepGoing <- false
        else
            let! reply =
                session.SendAndWaitAsync(
                    { Prompt = input.Trim()
                      Attachments = None
                      Mode = None })
            reply |> Option.iter (fun d -> printfn "Assistant: %s\n" d.Content)

    // ---------------------------------------------------------------
    // 7. Clean up
    // ---------------------------------------------------------------
    do! session.StopAsync()
    do! client.StopAsync()
    printfn "Done."
}

runExample () |> Async.RunSynchronously

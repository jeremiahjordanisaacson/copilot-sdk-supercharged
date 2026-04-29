# Error Handling

Patterns for handling errors in the F# Copilot SDK.

## Connection Errors

```fsharp
open CopilotSDK.Supercharged.FSharp

let connectSafely () = async {
    use client = CopilotClient.create CopilotClientOptions.defaults
    try
        do! client.StartAsync()
        printfn "Connected successfully"
        return Ok client
    with
    | :? JsonRpcException as ex ->
        printfn "JSON-RPC error %d: %s" ex.Code ex.Message
        return Error (sprintf "RPC error: %s" ex.Message)
    | ex ->
        printfn "Connection failed: %s" ex.Message
        return Error ex.Message
}
```

## Session-Level Error Handling

```fsharp
let safeSend (session: CopilotSession) prompt = async {
    try
        let! reply =
            session.SendAndWaitAsync(
                { Prompt = prompt; Attachments = None; Mode = None },
                timeout = System.TimeSpan.FromSeconds(30.0))
        return
            reply
            |> Option.map (fun d -> Ok d.Content)
            |> Option.defaultValue (Error "No response received")
    with
    | :? System.TimeoutException ->
        return Error "Request timed out"
    | :? JsonRpcException as ex ->
        return Error (sprintf "RPC error: %s" ex.Message)
    | ex ->
        return Error (sprintf "Unexpected error: %s" ex.Message)
}
```

## Result Pipeline

```fsharp
let processWithRetry (session: CopilotSession) prompt retries = async {
    let rec loop attempt = async {
        if attempt > retries then
            return Error "Max retries exceeded"
        else
            let! result = safeSend session prompt
            match result with
            | Ok content -> return Ok content
            | Error msg when attempt < retries ->
                printfn "Attempt %d failed: %s. Retrying..." attempt msg
                do! Async.Sleep 1000
                return! loop (attempt + 1)
            | Error msg ->
                return Error msg
    }
    return! loop 1
}
```

## Tool Error Results

```fsharp
let robustTool =
    DefineTool.create "fetch_data" "Fetch data from an API" None
        (fun inv -> async {
            try
                // Simulate API call
                let data = "{ \"value\": 42 }"
                return ToolResultObject.success data :> obj
            with
            | :? System.Net.Http.HttpRequestException as ex ->
                return ToolResultObject.failure
                    (sprintf "HTTP error: %s" ex.Message) :> obj
            | ex ->
                return ToolResultObject.failure
                    (sprintf "Unexpected: %s" ex.Message) :> obj
        })
```

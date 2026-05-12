namespace CopilotSDK.Supercharged.FSharp

open System
open System.Buffers
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks

// ---------------------------------------------------------------------------
// JSON-RPC 2.0 transport with Content-Length header framing (LSP style)
// ---------------------------------------------------------------------------

/// A JSON-RPC 2.0 request envelope.
type JsonRpcRequest =
    { [<System.Text.Json.Serialization.JsonPropertyName("jsonrpc")>]
      JsonRpc: string
      [<System.Text.Json.Serialization.JsonPropertyName("id")>]
      Id: int
      [<System.Text.Json.Serialization.JsonPropertyName("method")>]
      Method: string
      [<System.Text.Json.Serialization.JsonPropertyName("params")>]
      Params: obj option }

/// A JSON-RPC 2.0 response envelope.
type JsonRpcResponse =
    { [<System.Text.Json.Serialization.JsonPropertyName("jsonrpc")>]
      JsonRpc: string
      [<System.Text.Json.Serialization.JsonPropertyName("id")>]
      Id: int option
      [<System.Text.Json.Serialization.JsonPropertyName("result")>]
      Result: JsonElement option
      [<System.Text.Json.Serialization.JsonPropertyName("error")>]
      Error: JsonRpcError option }

/// A JSON-RPC 2.0 error object.
and JsonRpcError =
    { [<System.Text.Json.Serialization.JsonPropertyName("code")>]
      Code: int
      [<System.Text.Json.Serialization.JsonPropertyName("message")>]
      Message: string
      [<System.Text.Json.Serialization.JsonPropertyName("data")>]
      Data: JsonElement option }

/// A JSON-RPC 2.0 notification (no id, no response expected).
type JsonRpcNotification =
    { [<System.Text.Json.Serialization.JsonPropertyName("jsonrpc")>]
      JsonRpc: string
      [<System.Text.Json.Serialization.JsonPropertyName("method")>]
      Method: string
      [<System.Text.Json.Serialization.JsonPropertyName("params")>]
      Params: obj option }

/// Exception raised when a JSON-RPC error response is received.
type JsonRpcException(code: int, message: string, data: JsonElement option) =
    inherit Exception(sprintf "JSON-RPC error %d: %s" code message)
    member _.Code = code
    member _.ErrorData = data

/// Low-level JSON-RPC transport over streams with Content-Length framing.
type JsonRpcTransport(input: Stream, output: Stream) =
    let jsonOpts = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
    let mutable nextId = 0
    let pendingRequests = System.Collections.Concurrent.ConcurrentDictionary<int, TaskCompletionSource<JsonRpcResponse>>()
    let notificationHandlers = System.Collections.Concurrent.ConcurrentDictionary<string, Action<JsonElement option>>()
    let requestHandlers = System.Collections.Concurrent.ConcurrentDictionary<string, JsonElement option -> Async<obj>>()
    let writeLock = new SemaphoreSlim(1, 1)
    let cts = new CancellationTokenSource()

    let readHeaderLine (reader: StreamReader) = async {
        let! line = reader.ReadLineAsync() |> Async.AwaitTask
        return line
    }

    let readMessage (reader: StreamReader) = async {
        let mutable contentLength = -1
        let mutable keepReading = true

        while keepReading do
            let! line = readHeaderLine reader
            if isNull line then
                keepReading <- false
            elif line = "" then
                keepReading <- false
            elif line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase) then
                let value = line.Substring("Content-Length:".Length).Trim()
                contentLength <- Int32.Parse(value)
            else
                ()

        if contentLength <= 0 then
            return None
        else
            let buffer = Array.zeroCreate<char> contentLength
            let mutable totalRead = 0
            while totalRead < contentLength do
                let! read = reader.ReadAsync(buffer, totalRead, contentLength - totalRead) |> Async.AwaitTask
                if read = 0 then
                    failwith "Unexpected end of stream"
                totalRead <- totalRead + read
            let json = new string(buffer, 0, contentLength)
            return Some json
    }

    let writeMessage (json: string) = async {
        let bytes = Encoding.UTF8.GetBytes(json)
        let header = sprintf "Content-Length: %d\r\n\r\n" bytes.Length
        let headerBytes = Encoding.UTF8.GetBytes(header)
        do! writeLock.WaitAsync() |> Async.AwaitTask
        try
            do! output.WriteAsync(headerBytes, 0, headerBytes.Length) |> Async.AwaitTask
            do! output.WriteAsync(bytes, 0, bytes.Length) |> Async.AwaitTask
            do! output.FlushAsync() |> Async.AwaitTask
        finally
            writeLock.Release() |> ignore
    }

    let sendJsonRpcResponse (idRawText: string) (result: obj) = async {
        let resultJson = JsonSerializer.Serialize(result, jsonOpts)
        let responseMsg = sprintf """{"jsonrpc":"2.0","id":%s,"result":%s}""" idRawText resultJson
        do! writeMessage responseMsg
    }

    let sendJsonRpcErrorResponse (idRawText: string) (code: int) (errMessage: string) = async {
        let escapedMsg = JsonSerializer.Serialize(errMessage)
        let responseMsg = sprintf """{"jsonrpc":"2.0","id":%s,"error":{"code":%d,"message":%s}}""" idRawText code escapedMsg
        do! writeMessage responseMsg
    }

    /// Dispatch an incoming JSON message to pending requests or notification handlers.
    let dispatchMessage (json: string) =
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        // Check if this is a response (has "id" and either "result" or "error")
        let hasId =
            match root.TryGetProperty("id") with
            | true, idElem when idElem.ValueKind <> JsonValueKind.Null -> true
            | _ -> false

        let hasResult =
            match root.TryGetProperty("result") with
            | true, _ -> true
            | _ -> false

        let hasError =
            match root.TryGetProperty("error") with
            | true, _ -> true
            | _ -> false

        if hasId && (hasResult || hasError) then
            // It is a response
            let response = JsonSerializer.Deserialize<JsonRpcResponse>(json, jsonOpts)
            match response.Id with
            | Some id ->
                match pendingRequests.TryRemove(id) with
                | true, tcs -> tcs.SetResult(response)
                | _ -> ()
            | None -> ()
        else
            // It is a notification or a server-initiated request
            match root.TryGetProperty("method") with
            | true, methodElem ->
                let methodName = methodElem.GetString()
                let paramData =
                    match root.TryGetProperty("params") with
                    | true, p -> Some (p.Clone())
                    | _ -> None
                if hasId then
                    // Server-initiated request — dispatch and send response
                    let idRawText =
                        match root.TryGetProperty("id") with
                        | true, idElem -> idElem.GetRawText()
                        | _ -> "null"
                    match requestHandlers.TryGetValue(methodName) with
                    | true, handler ->
                        async {
                            try
                                let! result = handler paramData
                                do! sendJsonRpcResponse idRawText result
                            with ex ->
                                do! sendJsonRpcErrorResponse idRawText -32000 ex.Message
                        } |> Async.Start
                    | _ ->
                        match notificationHandlers.TryGetValue(methodName) with
                        | true, handler -> handler.Invoke(paramData)
                        | _ -> ()
                else
                    match notificationHandlers.TryGetValue(methodName) with
                    | true, handler -> handler.Invoke(paramData)
                    | _ -> ()
            | _ -> ()

    /// Start the read loop that dispatches incoming messages.
    member _.StartReadLoop() =
        let reader = new StreamReader(input, Encoding.UTF8)
        let loop = async {
            try
                let mutable running = true
                while running && not cts.Token.IsCancellationRequested do
                    let! msg = readMessage reader
                    match msg with
                    | Some json -> dispatchMessage json
                    | None -> running <- false
            with
            | :? OperationCanceledException -> ()
            | ex ->
                // Signal all pending requests
                for kvp in pendingRequests do
                    kvp.Value.TrySetException(ex) |> ignore
        }
        Async.Start(loop, cts.Token)

    /// Send a JSON-RPC request and wait for the response.
    member _.SendRequestAsync<'T>(method: string, ?parameters: obj) : Async<'T> = async {
        let id = Interlocked.Increment(&nextId)
        let request =
            { JsonRpc = "2.0"
              Id = id
              Method = method
              Params = parameters }
        let tcs = TaskCompletionSource<JsonRpcResponse>()
        pendingRequests.TryAdd(id, tcs) |> ignore

        let json = JsonSerializer.Serialize(request, jsonOpts)
        do! writeMessage json

        let! response = tcs.Task |> Async.AwaitTask

        match response.Error with
        | Some err ->
            return raise (JsonRpcException(err.Code, err.Message, err.Data))
        | None ->
            match response.Result with
            | Some result ->
                let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                return JsonSerializer.Deserialize<'T>(result.GetRawText(), opts)
            | None ->
                return Unchecked.defaultof<'T>
    }

    /// Send a JSON-RPC notification (no response expected).
    member _.SendNotificationAsync(method: string, ?parameters: obj) : Async<unit> = async {
        let notification =
            { JsonRpc = "2.0"
              Method = method
              Params = parameters }
        let json = JsonSerializer.Serialize(notification, jsonOpts)
        do! writeMessage json
    }

    /// Register a handler for a server-initiated notification method.
    member _.OnNotification(method: string, handler: JsonElement option -> unit) =
        notificationHandlers.[method] <- Action<JsonElement option>(handler)

    /// Remove a previously registered notification handler.
    member _.RemoveNotification(method: string) =
        notificationHandlers.TryRemove(method) |> ignore

    /// Register a handler for a server-initiated request method.
    /// The handler receives params and returns a result sent back as the JSON-RPC response.
    member _.OnRequest(method: string, handler: JsonElement option -> Async<obj>) =
        requestHandlers.[method] <- handler

    /// Remove a previously registered request handler.
    member _.RemoveRequest(method: string) =
        requestHandlers.TryRemove(method) |> ignore

    /// Shut down the transport, cancelling any pending reads.
    member _.Dispose() =
        cts.Cancel()
        cts.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

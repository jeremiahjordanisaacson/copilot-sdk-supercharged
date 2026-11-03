namespace CopilotSDK.Supercharged.FSharp

open System
open System.Collections.Generic
open System.Text.Json
open System.Threading
open System.Threading.Tasks

/// A Copilot conversation session.
///
/// Sessions are created via CopilotClient.createSession or resumed via
/// CopilotClient.resumeSession. They manage event subscriptions, tool
/// registration, and message exchange with the assistant.
type CopilotSession internal (sessionId: string, rpc: JsonRpcTransport, initialWorkspacePath: string option) =

    let eventHandlers = ResizeArray<SessionEvent -> unit>()
    let handlersLock = obj()
    let toolHandlers = Dictionary<string, ToolHandler>()
    let mutable workspacePath = initialWorkspacePath
    let mutable subagentStartHook : SubagentStartHandler option = None
    let mutable subagentStopHook : SubagentStopHandler option = None
    let mutable disconnectCallbacks : (unit -> unit) list = []

    let dispatchEvent (evt: SessionEvent) =
        let handlers =
            lock handlersLock (fun () -> eventHandlers |> Seq.toList)
        for handler in handlers do
            try handler evt
            with _ -> ()

    do
        // Register for session event notifications from the server.
        rpc.OnNotification("session.event", fun paramData ->
            match paramData with
            | Some elem ->
                let eventType =
                    match elem.TryGetProperty("type") with
                    | true, t -> t.GetString()
                    | _ -> "unknown"
                let data =
                    match elem.TryGetProperty("data") with
                    | true, d -> Some (d.Clone())
                    | _ -> None
                let evt = SessionEvent.parse eventType data
                dispatchEvent evt
            | None -> ()
        )

        // Register for tool call requests from the server.
        rpc.OnNotification("session.toolCall", fun paramData ->
            match paramData with
            | Some elem ->
                let toolName =
                    match elem.TryGetProperty("toolName") with
                    | true, t -> t.GetString()
                    | _ -> ""
                let toolCallId =
                    match elem.TryGetProperty("toolCallId") with
                    | true, t -> t.GetString()
                    | _ -> ""
                let args =
                    match elem.TryGetProperty("arguments") with
                    | true, a -> Some (a.Clone())
                    | _ -> None

                match toolHandlers.TryGetValue(toolName) with
                | true, handler ->
                    let invocation =
                        { SessionId = sessionId
                          ToolCallId = toolCallId
                          ToolName = toolName
                          Arguments = args }
                    async {
                        try
                            let! result = handler invocation
                            let resultObj =
                                match result with
                                | :? ToolResultObject as tro -> tro
                                | :? string as s -> ToolResultObject.success s
                                | other ->
                                    let json = JsonSerializer.Serialize(other)
                                    ToolResultObject.success json
                            do! rpc.SendNotificationAsync(
                                "session.toolCallResult",
                                {| sessionId = sessionId
                                   toolCallId = toolCallId
                                   result = resultObj |})
                        with ex ->
                            do! rpc.SendNotificationAsync(
                                "session.toolCallResult",
                                {| sessionId = sessionId
                                   toolCallId = toolCallId
                                   result = ToolResultObject.failure ex.Message |})
                    } |> Async.Start
                | _ -> ()
            | None -> ()
        )

    /// Alternate constructor without workspace path.
    internal new(sessionId: string, rpc: JsonRpcTransport) =
        CopilotSession(sessionId, rpc, None)

    /// The unique identifier for this session.
    member _.SessionId = sessionId

    /// The workspace path if infinite sessions are enabled.
    member _.WorkspacePath = workspacePath

    /// Subscribe to all session events. Returns an IDisposable that
    /// removes the handler when disposed.
    ///
    /// Use pattern matching in F# to handle events idiomatically:
    ///   session.On(function
    ///       | AssistantMessage data -> printfn "%s" data.Content
    ///       | SessionIdle -> printfn "Done"
    ///       | _ -> ())
    member _.On(handler: SessionEvent -> unit) : IDisposable =
        lock handlersLock (fun () -> eventHandlers.Add(handler))
        { new IDisposable with
            member _.Dispose() =
                lock handlersLock (fun () -> eventHandlers.Remove(handler) |> ignore) }

    /// Subscribe to session events via an IObservable.
    member this.Events : IObservable<SessionEvent> =
        { new IObservable<SessionEvent> with
            member _.Subscribe(observer) =
                this.On(fun evt -> observer.OnNext(evt)) }

    /// Register a tool that the assistant can invoke during this session.
    member _.RegisterTool(registeredTool: RegisteredTool) =
        toolHandlers.[registeredTool.Tool.Name] <- registeredTool.Handler

    /// Register multiple tools at once.
    member this.RegisterTools(tools: RegisteredTool list) =
        tools |> List.iter this.RegisterTool

    /// Send a message to the session. Returns the response message ID.
    member _.SendAsync(options: MessageOptions, ?cancellationToken: CancellationToken) : Async<string> = async {
        let request =
            WireJson.toObject (
                box
                    {| sessionId = sessionId
                       prompt = options.Prompt
                       attachments = options.Attachments |> Option.defaultValue []
                       mode = options.Mode
                       agentMode = options.AgentMode
                       displayPrompt = options.DisplayPrompt
                       requestHeaders = options.RequestHeaders |})
        WireJson.setOpt request "source" options.Source
        match options.ResponseSchema with
        | Some schema -> WireJson.set request "responseFormat" (box (MessageOptions.responseFormatFor schema))
        | None -> ()
        let! response = rpc.SendRequestAsync<{| messageId: string |}>("session.send", request)
        return response.messageId
    }

    /// Send a message and wait until the session becomes idle.
    /// Returns the final assistant message, or None if no message was received.
    member this.SendAndWaitAsync(
            options: MessageOptions,
            ?timeout: TimeSpan,
            ?cancellationToken: CancellationToken) : Async<AssistantMessageData option> = async {
        let effectiveTimeout = defaultArg timeout (TimeSpan.FromSeconds(60.0))
        let tcs = TaskCompletionSource<AssistantMessageData option>()
        let mutable lastMessage : AssistantMessageData option = None

        use _ = this.On(fun evt ->
            match evt with
            | AssistantMessage data ->
                lastMessage <- Some data
            | SessionIdle ->
                tcs.TrySetResult(lastMessage) |> ignore
            | _ -> ())

        let! _messageId = this.SendAsync(options)

        use ctSource = new CancellationTokenSource(effectiveTimeout)
        use _reg = ctSource.Token.Register(fun () ->
            tcs.TrySetException(TimeoutException("SendAndWait timed out")) |> ignore)

        return! tcs.Task |> Async.AwaitTask
    }

    /// Send a message with a JSON Schema for structured output and wait until
    /// the session becomes idle. Returns the final assistant content parsed as JSON.
    /// Fails when the run produced no assistant content.
    member this.SendAndWaitStructuredAsync(
            options: MessageOptions,
            responseSchema: JsonElement,
            ?timeout: TimeSpan,
            ?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        let effectiveTimeout = defaultArg timeout (TimeSpan.FromSeconds(60.0))
        let! reply =
            this.SendAndWaitAsync(
                { options with ResponseSchema = Some responseSchema },
                timeout = effectiveTimeout)
        match reply with
        | Some data when not (String.IsNullOrWhiteSpace data.Content) ->
            use doc = JsonDocument.Parse(data.Content)
            return doc.RootElement.Clone()
        | _ ->
            return failwith "The requested run completed without a structured assistant response."
    }

    /// Retrieve the conversation history for this session.
    member _.GetHistoryAsync(?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        return! rpc.SendRequestAsync<JsonElement>("session.getHistory", {| sessionId = sessionId |})
    }

    /// Stop the session (notifies the server).
    member _.StopAsync(?cancellationToken: CancellationToken) : Async<unit> = async {
        do! rpc.SendNotificationAsync("session.stop", {| sessionId = sessionId |})
    }

    /// Change the Auto routing preference without changing the selected model
    /// (experimental). Pass None to return to the default Auto routing (sent as
    /// JSON null). Returns the runtime's acknowledgement.
    member _.SetAutoTierAsync(autoTier: string option, ?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        return!
            rpc.SendRequestAsync<JsonElement>(
                "session.model.switchAutoTier",
                {| sessionId = sessionId; autoTier = autoTier |})
    }

    /// Get the managed settings in effect for this session.
    member _.GetManagedSettingsAsync(?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        return! rpc.SendRequestAsync<JsonElement>("session.managedSettings.get", {| sessionId = sessionId |})
    }

    /// Re-run the managed-settings policy helper for this session.
    member _.RecheckPolicyHelperAsync(?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        return!
            rpc.SendRequestAsync<JsonElement>(
                "session.managedSettings.recheckPolicyHelper",
                {| sessionId = sessionId |})
    }

    /// Disconnect this session and release its in-memory resources. Session
    /// state on disk is preserved so it can be resumed later. Sends
    /// `session.detach`, falling back to `session.destroy` on older CLIs.
    member _.DisconnectAsync(?cancellationToken: CancellationToken) : Async<unit> = async {
        let request = {| sessionId = sessionId |}
        try
            do! rpc.SendRequestAsync<JsonElement>("session.detach", request) |> Async.Ignore
        with
        | :? JsonRpcException as ex when ex.Code = -32601 ->
            do! rpc.SendRequestAsync<JsonElement>("session.destroy", request) |> Async.Ignore
        lock handlersLock (fun () -> eventHandlers.Clear())
        toolHandlers.Clear()
        subagentStartHook <- None
        subagentStopHook <- None
        let callbacks = disconnectCallbacks
        disconnectCallbacks <- []
        for callback in callbacks do
            try callback ()
            with _ -> ()
    }

    /// Register the sub-agent lifecycle hooks for this session.
    member internal _.SetSubagentHooks(onStart: SubagentStartHandler option, onStop: SubagentStopHandler option) =
        subagentStartHook <- onStart
        subagentStopHook <- onStop

    /// Register a callback invoked once the session is disconnected.
    member internal _.AddDisconnectCallback(callback: unit -> unit) =
        disconnectCallbacks <- callback :: disconnectCallbacks

    /// Dispatch a `hooks.invoke` request to the matching hook. Returns the hook
    /// output wire payload, or None when no hook handled the request.
    member internal _.HandleHooksInvokeAsync(hookType: string, input: JsonElement option) : Async<IDictionary<string, obj> option> = async {
        match hookType, input with
        | "subagentStart", Some elem ->
            match subagentStartHook with
            | Some handler ->
                let! output = handler (SubagentHooks.parseStartInput elem)
                return output |> Option.map SubagentHooks.startOutputToWire
            | None -> return None
        | "subagentStop", Some elem ->
            match subagentStopHook with
            | Some handler ->
                let! output = handler (SubagentHooks.parseStopInput elem)
                return output |> Option.map SubagentHooks.stopOutputToWire
            | None -> return None
        | _ -> return None
    }

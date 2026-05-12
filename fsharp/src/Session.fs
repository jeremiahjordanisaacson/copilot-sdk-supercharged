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
    let mutable exitPlanModeHandler : ExitPlanModeHandler option = None
    let mutable traceContextProvider : TraceContextProvider option = None

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

        // Register handler for exit plan mode requests from the server.
        rpc.OnRequest("exitPlanMode.request", fun paramData ->
            async {
                match paramData with
                | Some elem ->
                    let summary =
                        match elem.TryGetProperty("summary") with
                        | true, s -> s.GetString()
                        | _ -> ""
                    let planContent =
                        match elem.TryGetProperty("planContent") with
                        | true, s -> Some (s.GetString())
                        | _ -> None
                    let actions =
                        match elem.TryGetProperty("actions") with
                        | true, a ->
                            [ for i in 0 .. a.GetArrayLength() - 1 -> a.[i].GetString() ]
                        | _ -> []
                    let recommendedAction =
                        match elem.TryGetProperty("recommendedAction") with
                        | true, s -> s.GetString()
                        | _ -> ""
                    let request =
                        { SessionId = sessionId
                          Summary = summary
                          PlanContent = planContent
                          Actions = actions
                          RecommendedAction = recommendedAction }
                    let! result =
                        match exitPlanModeHandler with
                        | None ->
                            async { return { Approved = true; SelectedAction = None; Feedback = None } }
                        | Some handler ->
                            async {
                                try
                                    return! handler request
                                with _ ->
                                    return { Approved = true; SelectedAction = None; Feedback = None }
                            }
                    return
                        {| approved = result.Approved
                           selectedAction = result.SelectedAction |> Option.toObj
                           feedback = result.Feedback |> Option.toObj |} :> obj
                | None ->
                    return {| approved = true |} :> obj
            })

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

    /// Register a handler for exit plan mode requests.
    member _.RegisterExitPlanModeHandler(handler: ExitPlanModeHandler) =
        exitPlanModeHandler <- Some handler

    /// Register a trace context provider for distributed tracing.
    member _.RegisterTraceContextProvider(provider: TraceContextProvider) =
        traceContextProvider <- Some provider

    /// Send a message to the session. Returns the response message ID.
    member _.SendAsync(options: MessageOptions, ?cancellationToken: CancellationToken) : Async<string> = async {
        let request = Dictionary<string, obj>()
        request.["sessionId"] <- box sessionId
        request.["prompt"] <- box options.Prompt
        request.["attachments"] <- box (options.Attachments |> Option.defaultValue [])
        options.Mode |> Option.iter (fun m -> request.["mode"] <- box m)
        match traceContextProvider with
        | Some provider ->
            try
                let ctx = provider ()
                ctx.Traceparent |> Option.iter (fun tp -> request.["traceparent"] <- box tp)
                ctx.Tracestate |> Option.iter (fun ts -> request.["tracestate"] <- box ts)
            with _ -> ()
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

    /// Retrieve the conversation history for this session.
    member _.GetHistoryAsync(?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        return! rpc.SendRequestAsync<JsonElement>("session.getHistory", {| sessionId = sessionId |})
    }

    /// Stop the session (notifies the server).
    member _.StopAsync(?cancellationToken: CancellationToken) : Async<unit> = async {
        do! rpc.SendNotificationAsync("session.stop", {| sessionId = sessionId |})
    }

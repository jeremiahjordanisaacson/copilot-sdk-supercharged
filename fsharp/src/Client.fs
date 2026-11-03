namespace CopilotSDK.Supercharged.FSharp

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net.Sockets
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

/// A GitHub token provider registered for one session (client-level registry).
type internal GitHubTokenRegistration(provider: GitHubTokenProvider, sessionId: string option) =
    member _.Provider = provider
    member val SessionId = sessionId with get, set

/// Main client for interacting with the GitHub Copilot CLI.
///
/// CopilotClient manages the connection to the Copilot CLI server and
/// provides methods to create and manage conversation sessions. It can
/// either spawn a CLI server process or connect to an existing server.
///
/// Example:
///   async {
///       use client = CopilotClient.create CopilotClientOptions.defaults
///       do! client.StartAsync()
///       let! session =
///           client.CreateSessionAsync(SessionConfig.defaults)
///       let! reply =
///           session.SendAndWaitAsync(MessageOptions.create "Hello!")
///       reply |> Option.iter (fun data -> printfn "%s" data.Content)
///       do! client.StopAsync()
///   } |> Async.RunSynchronously
type CopilotClient(options: CopilotClientOptions) =

    static let minProtocolVersion = 3

    let sessions = ConcurrentDictionary<string, CopilotSession>()
    let gitHubTokenProviders = ConcurrentDictionary<string, GitHubTokenRegistration>()
    let mutable transport : JsonRpcTransport option = None
    let mutable cliProcess : Process option = None
    let mutable connectionState = Disconnected
    let mutable negotiatedProtocolVersion : int option = None
    let mutable actualPort : int option = None
    let stateLock = obj()

    // ------------------------------------------------------------------
    // Connection helpers (let bindings must precede members)
    // ------------------------------------------------------------------

    let resolveCliPath () =
        match options.CliPath with
        | Some path -> path
        | None ->
            match Environment.GetEnvironmentVariable("COPILOT_CLI_PATH") with
            | null | "" ->
                failwith
                    "No CLI path specified. Set CliPath in options, or the \
                     COPILOT_CLI_PATH environment variable."
            | envPath -> envPath

    let buildCliArgs () =
        let args = ResizeArray<string>()
        args.AddRange(options.CliArgs)
        if options.UseStdio then
            args.Add("--stdio")
        else
            args.Add("--port")
            args.Add(string options.Port)
        args.Add("--log-level")
        args.Add(options.LogLevel)
        match options.SessionIdleTimeoutSeconds with
        | Some t ->
            args.Add("--session-idle-timeout")
            args.Add(string t)
        | None -> ()
        args |> Seq.toList

    let spawnCliProcess () =
        let cliPath = resolveCliPath ()
        let args = buildCliArgs ()
        let psi = ProcessStartInfo(cliPath)
        psi.Arguments <- String.Join(" ", args)
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.CreateNoWindow <- true

        match options.Cwd with
        | Some cwd -> psi.WorkingDirectory <- cwd
        | None -> ()

        match options.Environment with
        | Some env ->
            for kvp in env do
                psi.EnvironmentVariables.[kvp.Key] <- kvp.Value
        | None -> ()

        match options.GitHubToken with
        | Some token ->
            psi.EnvironmentVariables.["GITHUB_TOKEN"] <- token
        | None -> ()

        match options.UseLoggedInUser with
        | Some false ->
            psi.EnvironmentVariables.["COPILOT_USE_LOGGED_IN_USER"] <- "false"
        | _ -> ()

        let proc = Process.Start(psi)
        cliProcess <- Some proc
        proc

    let connectStdio (proc: Process) =
        let t = new JsonRpcTransport(proc.StandardOutput.BaseStream, proc.StandardInput.BaseStream)
        t.StartReadLoop()
        transport <- Some t

    let connectTcp (host: string) (port: int) = async {
        let client = new TcpClient()
        do! client.ConnectAsync(host, port) |> Async.AwaitTask
        let stream = client.GetStream()
        let t = new JsonRpcTransport(stream, stream)
        t.StartReadLoop()
        transport <- Some t
    }

    let initializeHandshake () = async {
        let t = transport |> Option.get
        let! statusResponse =
            t.SendRequestAsync<GetStatusResponse>("getStatus")
        negotiatedProtocolVersion <- statusResponse.ProtocolVersion
        match statusResponse.ProtocolVersion with
        | Some v when v < minProtocolVersion ->
            failwithf
                "Server protocol version %d is below the minimum required version %d"
                v minProtocolVersion
        | _ -> ()
    }

    let getTransport () =
        match transport with
        | Some t -> t
        | None -> failwith "Client is not started. Call StartAsync first."

    let tryGetString (elem: JsonElement) (name: string) =
        if elem.ValueKind <> JsonValueKind.Object then None
        else
            match elem.TryGetProperty(name) with
            | true, v when v.ValueKind = JsonValueKind.String -> Some (v.GetString())
            | _ -> None

    /// Send the `connect` request carrying clientInfo before the handshake.
    /// Older CLIs that do not implement `connect` are tolerated.
    let sendClientInfo () = async {
        match options.ClientInfo |> Option.bind CopilotClientInfo.toWire with
        | Some info ->
            let t = getTransport ()
            try
                do! t.SendRequestAsync<JsonElement>("connect", {| clientInfo = info |}) |> Async.Ignore
            with
            | :? JsonRpcException as ex when ex.Code = -32601 || ex.Message.Contains("Unhandled method connect") -> ()
        | None -> ()
    }

    /// Handle a `gitHubToken.getToken` request by invoking the registered provider.
    let acquireGitHubToken (parameters: JsonElement option) : Async<obj> = async {
        let field name = parameters |> Option.bind (fun p -> tryGetString p name)
        let registrationId = field "registrationId" |> Option.defaultValue ""
        match gitHubTokenProviders.TryGetValue(registrationId) with
        | true, registration ->
            let args : GitHubTokenProviderArgs =
                { Host = field "host" |> Option.defaultValue ""
                  SessionId = field "sessionId" |> Option.orElse registration.SessionId
                  Reason = field "reason" |> Option.defaultValue "initial" }
            let provider = registration.Provider
            let! result = provider args
            return box (GitHubTokenProviderResult.toWire result)
        | _ ->
            return
                raise (
                    InvalidOperationException(
                        sprintf "No GitHub token provider registered for registration ID \"%s\"" registrationId))
    }

    /// Handle a `hooks.invoke` request by dispatching to the target session.
    let handleHooksInvoke (parameters: JsonElement option) : Async<obj> = async {
        let field name = parameters |> Option.bind (fun p -> tryGetString p name)
        let input =
            parameters
            |> Option.bind (fun p ->
                if p.ValueKind <> JsonValueKind.Object then None
                else
                    match p.TryGetProperty("input") with
                    | true, v -> Some (v.Clone())
                    | _ -> None)
        let result = Dictionary<string, obj>()
        match field "sessionId" with
        | Some sid ->
            match sessions.TryGetValue(sid) with
            | true, session ->
                let! output = session.HandleHooksInvokeAsync(field "hookType" |> Option.defaultValue "", input)
                output |> Option.iter (fun o -> result.["output"] <- box o)
            | _ -> ()
        | None -> ()
        return box result
    }

    /// Register the client-level server-to-client request handlers.
    let registerServerHandlers (t: JsonRpcTransport) =
        t.OnRequest("gitHubToken.getToken", acquireGitHubToken)
        t.OnRequest("hooks.invoke", handleHooksInvoke)

    /// Store a GitHub token provider and return its registration id.
    let registerGitHubTokenProvider (provider: GitHubTokenProvider option) (sessionId: string option) =
        provider
        |> Option.map (fun p ->
            let registrationId = Guid.NewGuid().ToString()
            gitHubTokenProviders.[registrationId] <- GitHubTokenRegistration(p, sessionId)
            registrationId)

    let removeGitHubTokenProvider (registrationId: string option) =
        registrationId |> Option.iter (fun id -> gitHubTokenProviders.TryRemove(id) |> ignore)

    /// Add the options shared by session.create and session.resume.
    let addSharedSessionOptions
            (request: JsonObject)
            (autoTier: string option)
            (enableWebSocketResponses: bool option)
            (askUserVariant: string option)
            (sandboxConfig: SandboxConfig option)
            (featureFlags: IDictionary<string, bool> option)
            (allowedModels: string list option)
            (authClientIdMetadataUrl: string option)
            (agentDirectories: string list option)
            (hookDirectories: string list option)
            (interactiveUser: bool option)
            (excludedManagedSettings: string list option)
            (enforceManagedModelDefaults: bool option)
            (imageGeneration: ImageGenerationConfig option)
            (autopilot: AutopilotConfig option)
            (hasSubagentHooks: bool)
            (provider: ProviderConfig option)
            (registrationId: string option) =
        // enableWebSocketResponses lives under "capi" together with autoTier.
        let capi = Dictionary<string, obj>()
        autoTier |> Option.iter (fun v -> capi.["autoTier"] <- box v)
        enableWebSocketResponses |> Option.iter (fun v -> capi.["enableWebSocketResponses"] <- box v)
        if capi.Count > 0 then WireJson.set request "capi" (box capi)
        WireJson.setOpt request "askUserVariant" askUserVariant
        sandboxConfig |> Option.iter (fun c -> WireJson.set request "sandboxConfig" (box (SandboxConfig.toWire c)))
        featureFlags |> Option.iter (fun f -> WireJson.set request "featureFlags" (box (Dictionary<string, bool>(f))))
        WireJson.setOpt request "allowedModels" allowedModels
        WireJson.setOpt request "authClientIdMetadataUrl" authClientIdMetadataUrl
        WireJson.setOpt request "agentDirectories" agentDirectories
        WireJson.setOpt request "hookDirectories" hookDirectories
        WireJson.setOpt request "interactiveUser" interactiveUser
        WireJson.setOpt request "excludedManagedSettings" excludedManagedSettings
        WireJson.setOpt request "enforceManagedModelDefaults" enforceManagedModelDefaults
        imageGeneration |> Option.iter (fun c -> WireJson.set request "imageGeneration" (box (ImageGenerationConfig.toWire c)))
        autopilot |> Option.iter (fun c -> WireJson.set request "autopilot" (box (AutopilotConfig.toWire c)))
        if hasSubagentHooks then WireJson.set request "hooks" (box true)
        provider |> Option.iter (fun c -> WireJson.set request "provider" (box (ProviderConfig.toWire c)))
        WireJson.setOpt request "gitHubTokenProviderRegistrationId" registrationId

    /// Wire up a newly created/resumed session and track it.
    let trackSession (session: CopilotSession) (registrationId: string option) onStart onStop =
        let sid = session.SessionId
        registrationId
        |> Option.iter (fun id ->
            match gitHubTokenProviders.TryGetValue(id) with
            | true, registration -> registration.SessionId <- Some sid
            | _ -> ())
        session.SetSubagentHooks(onStart, onStop)
        session.AddDisconnectCallback(fun () ->
            removeGitHubTokenProvider registrationId
            sessions.TryRemove(sid) |> ignore)
        sessions.TryAdd(sid, session) |> ignore

    /// Create a client with default options.
    new() = new CopilotClient(CopilotClientOptions.defaults)

    /// Current connection state.
    member _.ConnectionState = connectionState

    /// Negotiated protocol version with the server.
    member _.NegotiatedProtocolVersion = negotiatedProtocolVersion

    /// Actual TCP port when not using stdio.
    member _.ActualPort = actualPort

    /// Start the client, spawning the CLI process or connecting to an
    /// existing server.
    member _.StartAsync(?cancellationToken: CancellationToken) : Async<unit> = async {
        lock stateLock (fun () -> connectionState <- Connecting)
        try
            match options.CliUrl with
            | Some url ->
                let parts = url.Split(':')
                let host = parts.[0]
                let port = Int32.Parse(parts.[1])
                actualPort <- Some port
                do! connectTcp host port
            | None ->
                let proc = spawnCliProcess ()
                if options.UseStdio then
                    connectStdio proc
                else
                    // Wait briefly for the server to print its port
                    do! Async.Sleep 500
                    let port = if options.Port = 0 then 3000 else options.Port
                    actualPort <- Some port
                    do! connectTcp "127.0.0.1" port

            transport |> Option.iter registerServerHandlers

            do! sendClientInfo ()

            do! initializeHandshake ()

            // Set up session filesystem provider if configured
            match options.SessionFs with
            | Some fs ->
                let fsParams =
                    {| initialCwd = fs.InitialCwd
                       sessionStatePath = fs.SessionStatePath
                       conventions = fs.Conventions |}
                let t = transport |> Option.get
                do! t.SendRequestAsync<obj>("sessionFs.setProvider", fsParams)
                    |> Async.Ignore
            | None -> ()

            lock stateLock (fun () -> connectionState <- Connected)
        with ex ->
            lock stateLock (fun () -> connectionState <- Error)
            raise ex
    }

    /// Stop the client, killing the CLI process if one was spawned.
    member _.StopAsync(?cancellationToken: CancellationToken) : Async<unit> = async {
        lock stateLock (fun () -> connectionState <- Disconnected)
        gitHubTokenProviders.Clear()
        transport |> Option.iter (fun t -> (t :> IDisposable).Dispose())
        transport <- None
        cliProcess |> Option.iter (fun p ->
            try
                if not p.HasExited then p.Kill()
                p.Dispose()
            with _ -> ())
        cliProcess <- None
    }

    // ------------------------------------------------------------------
    // Session management
    // ------------------------------------------------------------------

    /// Create a new conversation session.
    member _.CreateSessionAsync(config: SessionConfig, ?cancellationToken: CancellationToken) : Async<CopilotSession> = async {
        let t = getTransport ()
        if config.GitHubToken.IsSome && config.GitHubTokenProvider.IsSome then
            invalidArg "config" "GitHubToken and GitHubTokenProvider are mutually exclusive"
        let tools =
            config.SkillDirectories
            |> Option.map (fun dirs -> {| skillDirectories = dirs |})

        let request =
            WireJson.toObject (
                box
                    {| model = config.Model
                       systemMessage = config.SystemMessage
                       streaming = config.Streaming
                       agent = config.Agent
                       sdkProtocolVersion = SdkProtocolVersion.getVersion ()
                       includeSubAgentStreamingEvents = config.IncludeSubAgentStreamingEvents
                       // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
                       enableCitations = config.EnableCitations
                       excludedBuiltinAgents = config.ExcludedBuiltinAgents
                       sessionLimits = (config.SessionLimits |> Option.map SessionLimitsConfig.toWire)
                       memory = (config.Memory |> Option.map MemoryConfiguration.toWire)
                       otlpProtocol = config.OtlpProtocol
                       expAssignments = config.ExpAssignments
                       rewindEnabled = config.RewindEnabled
                       additionalDirectories = config.AdditionalDirectories
                       disabledMcpServers = config.DisabledMcpServers
                       githubMcpToolConfig = config.GithubMcpToolConfig
                       canvasProvider = config.CanvasProvider
                       customAgentsLocalOnly = config.CustomAgentsLocalOnly
                       toolSearch = config.ToolSearch
                       experimentalMode = config.ExperimentalMode
                       contentExclusion = config.ContentExclusion
                       reasoningEffort = config.ReasoningEffort
                       // Signal to the runtime that a user-prompt-transformed hook is registered.
                       userPromptTransformed = (config.OnUserPromptTransformed |> Option.map (fun _ -> true))
                       // Signal to the runtime that an MCP OAuth host-token handler is registered.
                       mcpAuthHandler = (config.OnMcpAuthRequest |> Option.map (fun _ -> true)) |})

        let registrationId = registerGitHubTokenProvider config.GitHubTokenProvider None
        addSharedSessionOptions
            request
            config.AutoTier
            config.EnableWebSocketResponses
            config.AskUserVariant
            config.SandboxConfig
            config.FeatureFlags
            config.AllowedModels
            config.AuthClientIdMetadataUrl
            config.AgentDirectories
            config.HookDirectories
            config.InteractiveUser
            config.ExcludedManagedSettings
            config.EnforceManagedModelDefaults
            config.ImageGeneration
            config.Autopilot
            (config.OnSubagentStart.IsSome || config.OnSubagentStop.IsSome)
            config.Provider
            registrationId
        // Create-only options.
        WireJson.setOpt request "name" config.Name
        WireJson.setOpt request "trustWorkingDirectory" config.TrustWorkingDirectory
        WireJson.setOpt request "refreshCustomInstructions" config.RefreshCustomInstructions

        let! response =
            async {
                try
                    return! t.SendRequestAsync<{| sessionId: string; workspacePath: string option |}>("session.create", request)
                with ex ->
                    removeGitHubTokenProvider registrationId
                    return raise ex
            }

        let session =
            CopilotSession(
                response.sessionId,
                t,
                response.workspacePath)
        trackSession session registrationId config.OnSubagentStart config.OnSubagentStop
        return session
    }

    /// Resume an existing session by ID.
    member _.ResumeSessionAsync(config: ResumeSessionConfig, ?cancellationToken: CancellationToken) : Async<CopilotSession> = async {
        let t = getTransport ()
        let request =
            WireJson.toObject (
                box
                    {| sessionId = config.SessionId
                       model = config.Model
                       sdkProtocolVersion = SdkProtocolVersion.getVersion () |})

        let registrationId = registerGitHubTokenProvider config.GitHubTokenProvider (Some config.SessionId)
        addSharedSessionOptions
            request
            config.AutoTier
            None
            config.AskUserVariant
            config.SandboxConfig
            config.FeatureFlags
            config.AllowedModels
            config.AuthClientIdMetadataUrl
            config.AgentDirectories
            config.HookDirectories
            config.InteractiveUser
            config.ExcludedManagedSettings
            config.EnforceManagedModelDefaults
            config.ImageGeneration
            config.Autopilot
            (config.OnSubagentStart.IsSome || config.OnSubagentStop.IsSome)
            config.Provider
            registrationId
        // Resume-only options.
        WireJson.setOpt request "allowTranscriptRecovery" config.AllowTranscriptRecovery

        let! response =
            async {
                try
                    return! t.SendRequestAsync<{| sessionId: string; workspacePath: string option |}>("session.resume", request)
                with ex ->
                    removeGitHubTokenProvider registrationId
                    return raise ex
            }

        let session =
            CopilotSession(
                response.sessionId,
                t,
                response.workspacePath)
        trackSession session registrationId config.OnSubagentStart config.OnSubagentStop
        return session
    }

    /// Clear the server's managed-settings cache.
    member _.ClearManagedSettingsCacheAsync(?cancellationToken: CancellationToken) : Async<JsonElement> = async {
        let t = getTransport ()
        return! t.SendRequestAsync<JsonElement>("managedSettings.clearCache", {||})
    }

    /// Get metadata for a session.
    member _.GetSessionMetadataAsync(sessionId: string, ?cancellationToken: CancellationToken) : Async<SessionMetadata option> = async {
        let t = getTransport ()
        try
            let! meta = t.SendRequestAsync<SessionMetadata>("session.getMetadata", {| sessionId = sessionId |})
            return Some meta
        with
        | :? JsonRpcException -> return None
    }

    /// List available models.
    member _.ListModelsAsync(?cancellationToken: CancellationToken) : Async<ModelInfo list> = async {
        let t = getTransport ()
        let! models = t.SendRequestAsync<ModelInfo list>("models.list")
        return models
    }

    /// Get the authentication status.
    member _.GetAuthStatusAsync(?cancellationToken: CancellationToken) : Async<GetAuthStatusResponse> = async {
        let t = getTransport ()
        return! t.SendRequestAsync<GetAuthStatusResponse>("auth.getStatus")
    }

    /// Get the server status.
    member _.GetStatusAsync(?cancellationToken: CancellationToken) : Async<GetStatusResponse> = async {
        let t = getTransport ()
        return! t.SendRequestAsync<GetStatusResponse>("status.get")
    }

    /// Get the ID of the most recently updated session.
    member _.GetLastSessionIdAsync(?cancellationToken: CancellationToken) : Async<string option> = async {
        let t = getTransport ()
        try
            let! result = t.SendRequestAsync<{| sessionId: string option |}>("session.getLastId", {||})
            return result.sessionId
        with
        | :? JsonRpcException -> return None
    }

    /// Ping the server to verify connectivity.
    member _.PingAsync(?message: string, ?cancellationToken: CancellationToken) : Async<{| message: string; timestamp: string |}> = async {
        let t = getTransport ()
        let msg = defaultArg message "ping"
        return! t.SendRequestAsync<{| message: string; timestamp: string |}>("ping", {| message = msg |})
    }

    /// Delete a session permanently.
    member _.DeleteSessionAsync(sessionId: string, ?cancellationToken: CancellationToken) : Async<unit> = async {
        let t = getTransport ()
        do! t.SendNotificationAsync("session.delete", {| sessionId = sessionId |})
        sessions.TryRemove(sessionId) |> ignore
    }

    /// Get the foreground session ID.
    member _.GetForegroundSessionIdAsync(?cancellationToken: CancellationToken) : Async<string option> = async {
        let t = getTransport ()
        try
            let! result = t.SendRequestAsync<{| sessionId: string option |}>("session.getForeground")
            return result.sessionId
        with
        | :? JsonRpcException -> return None
    }

    /// List all sessions known to the server.
    member _.ListSessionsAsync(?cancellationToken: CancellationToken) : Async<SessionMetadata list> = async {
        let t = getTransport ()
        let! sessions = t.SendRequestAsync<SessionMetadata list>("session.list")
        return sessions
    }

    /// Set the foreground session ID.
    member _.SetForegroundSessionIdAsync(sessionId: string, ?cancellationToken: CancellationToken) : Async<unit> = async {
        let t = getTransport ()
        let! result = t.SendRequestAsync<{| success: bool; error: string option |}>("session.setForeground", {| sessionId = sessionId |})
        if not result.success then
            let errMsg = result.error |> Option.defaultValue "Unknown error"
            failwithf "Failed to set foreground session: %s" errMsg
    }

    // ------------------------------------------------------------------
    // IDisposable
    // ------------------------------------------------------------------

    interface IDisposable with
        member this.Dispose() =
            this.StopAsync() |> Async.RunSynchronously

module CopilotClient =
    /// Create a CopilotClient with the given options.
    let create (options: CopilotClientOptions) = new CopilotClient(options)

    /// Create a CopilotClient with default options.
    let createDefault () = new CopilotClient()

    /// Start the client asynchronously.
    let start (client: CopilotClient) = client.StartAsync()

    /// Stop the client asynchronously.
    let stop (client: CopilotClient) = client.StopAsync()

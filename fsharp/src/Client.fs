namespace CopilotSDK.Supercharged.FSharp

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Net.Sockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks

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
///           session.SendAndWaitAsync({ Prompt = "Hello!"; Attachments = None; Mode = None })
///       reply |> Option.iter (fun data -> printfn "%s" data.Content)
///       do! client.StopAsync()
///   } |> Async.RunSynchronously
type CopilotClient(options: CopilotClientOptions) =

    static let minProtocolVersion = 2

    let sessions = ConcurrentDictionary<string, CopilotSession>()
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
                    |> Async.AwaitTask |> Async.Ignore
            | None -> ()

            lock stateLock (fun () -> connectionState <- Connected)
        with ex ->
            lock stateLock (fun () -> connectionState <- Error)
            raise ex
    }

    /// Stop the client, killing the CLI process if one was spawned.
    member _.StopAsync(?cancellationToken: CancellationToken) : Async<unit> = async {
        lock stateLock (fun () -> connectionState <- Disconnected)
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
        let tools =
            config.SkillDirectories
            |> Option.map (fun dirs -> {| skillDirectories = dirs |})

        let request =
            {| model = config.Model
               systemMessage = config.SystemMessage
               streaming = config.Streaming
               agent = config.Agent
               sdkProtocolVersion = SdkProtocolVersion.getVersion ()
               includeSubAgentStreamingEvents = config.IncludeSubAgentStreamingEvents |}

        let! response = t.SendRequestAsync<{| sessionId: string; workspacePath: string option |}>("session.create", request)

        let session =
            CopilotSession(
                response.sessionId,
                t,
                response.workspacePath)
        sessions.TryAdd(response.sessionId, session) |> ignore
        return session
    }

    /// Resume an existing session by ID.
    member _.ResumeSessionAsync(config: ResumeSessionConfig, ?cancellationToken: CancellationToken) : Async<CopilotSession> = async {
        let t = getTransport ()
        let request =
            {| sessionId = config.SessionId
               model = config.Model
               sdkProtocolVersion = SdkProtocolVersion.getVersion () |}

        let! response = t.SendRequestAsync<{| sessionId: string; workspacePath: string option |}>("session.resume", request)

        let session =
            CopilotSession(
                response.sessionId,
                t,
                response.workspacePath)
        sessions.TryAdd(response.sessionId, session) |> ignore
        return session
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
        let! models = t.SendRequestAsync<ModelInfo list>("listModels")
        return models
    }

    /// Get the authentication status.
    member _.GetAuthStatusAsync(?cancellationToken: CancellationToken) : Async<GetAuthStatusResponse> = async {
        let t = getTransport ()
        return! t.SendRequestAsync<GetAuthStatusResponse>("getAuthStatus")
    }

    /// Get the server status.
    member _.GetStatusAsync(?cancellationToken: CancellationToken) : Async<GetStatusResponse> = async {
        let t = getTransport ()
        return! t.SendRequestAsync<GetStatusResponse>("getStatus")
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

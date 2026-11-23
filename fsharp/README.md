# Copilot Supercharged SDK for F#

An idiomatic F# client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

Add a project reference (from the repo):

```bash
dotnet add reference ../fsharp/src/CopilotSDK.Supercharged.FSharp.fsproj
```

Or if published to NuGet:

```bash
dotnet add package CopilotSDK.Supercharged.FSharp
```

## Quick Start

```fsharp
open CopilotSDK.Supercharged.FSharp

async {
    // Create and start the client
    use client = CopilotClient.create CopilotClientOptions.defaults
    do! client.StartAsync()

    // Create a session with default settings (approves all permissions)
    let! session =
        client.CreateSessionAsync(SessionConfig.defaults)

    // Subscribe to events using pattern matching
    use _ = session.On(function
        | AssistantMessage data -> printfn "Assistant: %s" data.Content
        | AssistantReasoning data -> printfn "[reasoning] %s" data.Content
        | ToolExecutionStart data -> printfn "[tool] %s" data.ToolName
        | SessionIdle -> printfn "-- idle --"
        | _ -> ())

    // Send a message and wait for the response
    let! reply =
        session.SendAndWaitAsync(MessageOptions.create "What is 2 + 2?")

    reply |> Option.iter (fun data -> printfn "Final answer: %s" data.Content)

    // Clean up
    do! client.StopAsync()
} |> Async.RunSynchronously
```

## What's New in v3.0.0

This release speaks **SDK protocol 4** (the protocol of the Microsoft Copilot SDK 2.0 runtime) and
requires **Copilot CLI 1.0.113 or newer**. `StartAsync` rejects older CLIs with an
`SDK protocol version mismatch: SDK supports version 4, but server reports version N` error.

Existing options keep working. `SessionConfig`, `ResumeSessionConfig`, and `MessageOptions` keep their
flat F# fields; right before sending `session.create` and `session.resume`, the client translates the
flat payload into the grouped v4 shape (`inference`, `tools`, `permissions`, `interaction`, `events`,
`agents`, `mcp`, `github`, `state`, ...) with `SessionWire.groupSessionParams`. For example
`Streaming = true` is sent as `events.streaming`, sub-agent hooks as `hooks.hasHandlers`, and
`AutoTier` as `github.capi.autoTier`. The legacy single `Provider` is sent as a named provider called
`default` (with `inference.defaultProvider = "default"` and `github.capi.enabled = false`, as v1 BYOK
behaved) unless you also pass the new `Providers` / `DefaultProvider` / `CapiEnabled` options.
`DeleteSessionAsync` now waits for the server, so a failed delete raises `JsonRpcException`.

| Feature | F# API |
|---------|--------|
| Named BYOK providers | `Providers` (`NamedProviderConfig`), `Models` (`ProviderModelConfig`), `DefaultProvider`, `CapiEnabled` on `SessionConfig` / `ResumeSessionConfig`; select a model with `Model = Some "provider/id"`; `ClearDefaultProvider = Some true` on resume sends `defaultProvider: null` |
| Idempotent tools | `Tools = Some [ ... ]` declares tool definitions (`tools.definitions`) and registers the handlers; `DefineTool.idempotent` or `DefineTool.withIdempotent true` sets `idempotent` |
| Client turn ID | `MessageOptions.ClientTurnId = Some "turn-1"` (sent as `clientTurnId`) |
| Client mode | `CopilotClientOptions.Mode = Some ClientMode.Empty`; default is `ClientMode.CodingAgent` (`"coding-agent"`, legacy `"copilot-cli"` accepted). Empty mode requires `AvailableTools` on every create/resume and applies locked-down defaults under your values |
| Shell output events | `ToolShellOutput` event case (`tool.shell_output`): `ToolCallId`, `Text`, `Sequence`, `Stream` |
| Shell authentication | `OnShellAuthenticationRequest` answers `shellAuthentication.request` with `ShellAuthenticationAccept value` or `ShellAuthenticationCancel`; the `CancellationToken` fires on `shellAuthentication.cancel` |
| Memory store | `OnStoreMemory` receives `StoredMemory` from `memoryStore.store` (sends `state.hasMemoryStore`) |
| Network callbacks | `CopilotClientOptions.Network` (`NetworkOptions`): `ResolveProxy`, `AuthenticateNegotiate`, `AuthenticateBasic`, `Scope`; registered with `network.setProvider` after connecting |
| Policy helper failure | `JsonRpcException.PolicyHelperFailure` reads `error.data.policyHelperFailure` when `session.create` / `session.resume` fails |
| Permission mode refusal reason | `session.SetPermissionModeAsync PermissionMode.AllowAll` returns `PermissionModeSetResult` whose `Reason` can be `PermissionModeRefusalReason.ManagedPolicy` (`"managed_policy"`) |
| Resume event suppression | `ResumeSessionConfig.SuppressResumeEvent = Some true` (wire `suppressResumeEvent`) |

```fsharp
open CopilotSDK.Supercharged.FSharp

async {
    use client =
        CopilotClient.create
            { CopilotClientOptions.defaults with
                Mode = Some ClientMode.Empty // every session must list AvailableTools
                Network =
                    Some { NetworkOptions.empty with
                            Scope = Some NetworkScope.Connection
                            ResolveProxy = Some (fun url -> async { return Some "PROXY proxy.contoso.com:8080; DIRECT" })
                            AuthenticateBasic =
                                Some (fun request -> async {
                                    // request.Proxy, request.Realm, request.PreviousRejected
                                    return Some { Username = Some "svc-copilot"; Password = Some (loadSecret ()) } }) } }
    do! client.StartAsync()

    let lookupOrder =
        DefineTool.createSimple "lookup_order" "Look up an order" (fun () -> async { return "shipped" })
        |> DefineTool.idempotent

    let config =
        { SessionConfig.defaults with
            Model = Some "ollama/llama3"
            AvailableTools = Some [ "lookup_order"; "view" ]
            Tools = Some [ lookupOrder ]
            Providers =
                Some [ { NamedProviderConfig.create "ollama" "http://localhost:11434/v1" with
                            ProviderType = Some "openai"
                            ApiKeyCommand = Some "op read op://dev/ollama/key" } ]
            Models = Some [ { ProviderModelConfig.create "ollama" "llama3" with MaxOutputTokens = Some 4096 } ]
            DefaultProvider = Some "ollama"
            CapiEnabled = Some false
            OnShellAuthenticationRequest =
                Some (fun request cancellationToken -> async {
                    if request.InputKind = ShellAuthenticationInputKind.Confirm then
                        return ShellAuthenticationCancel
                    else
                        return ShellAuthenticationAccept (askUser request.Prompt) })
            OnStoreMemory = Some (fun memory -> async { saveMemory memory.Subject memory.Fact memory.Citations }) }

    let! session =
        async {
            try
                return! client.CreateSessionAsync(config)
            with :? JsonRpcException as ex when ex.PolicyHelperFailure.IsSome ->
                let failure = ex.PolicyHelperFailure.Value
                printfn "Blocked by the policy helper (%s): %s" failure.Category failure.Message
                return raise ex
        }

    use _ = session.On(function
        | ToolShellOutput data -> printf "%s" data.Text
        | _ -> ())

    let! _ = session.SendAsync({ MessageOptions.create "Where is order 42?" with ClientTurnId = Some "turn-1" })

    let! modeResult = session.SetPermissionModeAsync(PermissionMode.AllowAll)
    if not modeResult.Success && modeResult.Reason = Some PermissionModeRefusalReason.ManagedPolicy then
        printfn "Managed policy keeps the permission mode at %s" modeResult.Mode

    do! session.DisconnectAsync()
    let! resumed =
        client.ResumeSessionAsync(
            { ResumeSessionConfig.create session.SessionId with
                AvailableTools = Some [ "lookup_order"; "view" ]
                SuppressResumeEvent = Some true
                ClearDefaultProvider = Some true })
    ()
} |> Async.RunSynchronously
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not available:
  it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: this SDK keeps spawning the CLI
  (stdio or TCP) or connecting to `CliUrl`. The existing `InProcess` option is unchanged.
- Tool invocations still arrive on this SDK's `session.toolCall` path; `Tools` now declares the
  definitions to the runtime, but host-owned `external_tool.requested` cancellation is not exposed.

## API Reference

### CopilotClient

The main entry point for connecting to the Copilot CLI server.

#### Creating a Client

```fsharp
// With default options (spawns CLI server, stdio transport)
use client = CopilotClient.create CopilotClientOptions.defaults

// With a custom CLI path
use client =
    CopilotClient.create
        { CopilotClientOptions.defaults with
            CliPath = Some "/usr/local/bin/copilot-cli" }

// Connect to an existing server
use client =
    CopilotClient.create
        { CopilotClientOptions.defaults with
            CliUrl = Some "localhost:3000" }
```

#### CopilotClientOptions

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `CliPath` | `string option` | `None` | Path to CLI executable (falls back to `COPILOT_CLI_PATH` env) |
| `CliArgs` | `string list` | `[]` | Extra CLI arguments |
| `Cwd` | `string option` | `None` | Working directory for the CLI process |
| `Port` | `int` | `0` | Server port (0 = random) |
| `UseStdio` | `bool` | `true` | Use stdio transport instead of TCP |
| `CliUrl` | `string option` | `None` | URL of existing CLI server |
| `LogLevel` | `string` | `"info"` | Log level |
| `AutoStart` | `bool` | `true` | Auto-start server |
| `Environment` | `IDictionary option` | `None` | Environment variables for the CLI |
| `GitHubToken` | `string option` | `None` | GitHub token for authentication |
| `UseLoggedInUser` | `bool option` | `None` | Use stored OAuth/gh CLI tokens |
| `Telemetry` | `TelemetryConfig option` | `None` | OpenTelemetry configuration |
| `SessionIdleTimeoutSeconds` | `int option` | `None` | Idle timeout in seconds |
| `ClientInfo` | `CopilotClientInfo option` | `None` | Embedding application identity (sent with `connect`) |

#### Client Methods

```fsharp
// Start and stop
do! client.StartAsync()
do! client.StopAsync()

// Session management
let! session = client.CreateSessionAsync(config)
let! session = client.ResumeSessionAsync(resumeConfig)
let! meta = client.GetSessionMetadataAsync(sessionId)
do! client.DeleteSessionAsync(sessionId)

// Server info
let! models = client.ListModelsAsync()
let! authStatus = client.GetAuthStatusAsync()
let! status = client.GetStatusAsync()
```

### CopilotSession

Represents a single conversation session.

#### Sending Messages

```fsharp
// Fire and forget (returns message ID)
let! messageId = session.SendAsync(MessageOptions.create "Hello")

// Send and wait for the response (blocks until session.idle)
let! reply =
    session.SendAndWaitAsync(
        MessageOptions.create "Hello",
        timeout = TimeSpan.FromSeconds(120.0))
```

#### Subscribing to Events

F# discriminated unions make event handling clean and exhaustive:

```fsharp
// Pattern matching handler
use _ = session.On(function
    | AssistantMessage data ->
        printfn "Message: %s" data.Content
    | AssistantMessageDelta data ->
        printf "%s" data.Content  // streaming delta
    | AssistantReasoning data ->
        printfn "[thinking] %s" data.Content
    | ToolExecutionStart data ->
        printfn "[tool start] %s" data.ToolName
    | ToolExecutionComplete data ->
        printfn "[tool done] %s" data.ToolName
    | SessionIdle ->
        printfn "-- session idle --"
    | SessionCompactionStart _ ->
        printfn "[compaction started]"
    | SessionCompactionComplete _ ->
        printfn "[compaction complete]"
    | _ -> ())
```

Using the `IObservable` interface with `|>` pipes:

```fsharp
open System

session.Events
|> Observable.filter SessionEvent.isIdle
|> Observable.subscribe (fun _ -> printfn "Session is idle")
|> ignore
```

#### SessionEvent (Discriminated Union)

| Case | Payload | Description |
|------|---------|-------------|
| `AssistantMessage` | `AssistantMessageData` | Final assistant response |
| `AssistantMessageDelta` | `AssistantMessageDeltaData` | Streaming text chunk |
| `AssistantReasoning` | `AssistantReasoningData` | Final reasoning text |
| `AssistantReasoningDelta` | `AssistantMessageDeltaData` | Streaming reasoning chunk |
| `ToolExecutionStart` | `ToolExecutionStartData` | Tool invocation started |
| `ToolExecutionComplete` | `ToolExecutionCompleteData` | Tool invocation finished |
| `SessionIdle` | (none) | Session finished processing |
| `SessionCompactionStart` | `SessionCompactionData` | Compaction started |
| `SessionCompactionComplete` | `SessionCompactionData` | Compaction finished |
| `UnknownEvent` | `string * JsonElement option` | Unrecognized event type |

Helper functions in the `SessionEvent` module:

```fsharp
SessionEvent.tryGetMessageContent event  // string option
SessionEvent.tryGetDeltaContent event    // string option
SessionEvent.isIdle event               // bool
```

### Tools

Register tools that the assistant can call using the `DefineTool` module:

```fsharp
// Simple tool with no parameters
let greetTool =
    DefineTool.createSimple "greet" "Say hello" (fun () -> async {
        return "Hello from F#!"
    })

// Tool with typed arguments
type WeatherArgs = { City: string }

let weatherTool =
    DefineTool.createTyped<WeatherArgs, ToolResultObject>
        "get_weather"
        "Get weather for a city"
        None
        (fun args -> async {
            return ToolResultObject.success (sprintf "Sunny in %s" args.City)
        })

// Pipe-friendly builder
let calculatorTool =
    DefineTool.define "calculate" "Evaluate a math expression"
    |> DefineTool.withHandler (fun inv -> async {
        return ToolResultObject.success "42" :> obj
    })
    |> DefineTool.build

// Register tools on a session
session.RegisterTools [greetTool; weatherTool; calculatorTool]
```

### Permission Handling

```fsharp
// Built-in handlers
SessionConfig.approveAll   // Approves every permission request
SessionConfig.rejectAll    // Rejects every permission request

// Custom handler
let myHandler : PermissionRequestHandler =
    fun request -> async {
        if request.ToolName = "dangerous_tool" then
            return { Kind = Reject; UpdraftMessage = Some "Not allowed" }
        else
            return { Kind = ApproveOnce; UpdraftMessage = None }
    }
```

## What's New in v2.6.0

Parity with upstream `@github/copilot-sdk` v2.6.0. Every new option defaults to `None` and is
omitted from the JSON-RPC request unless set (`FeatureFlags` is sent whenever it is `Some`, even
when empty).

| Feature | F# API |
|---------|--------|
| Client info | `CopilotClientOptions.ClientInfo` (`CopilotClientInfo`) — sent with `connect` before the handshake; older CLIs without `connect` are tolerated |
| Auto routing tier (experimental) | `AutoTier = Some AutoTier.Intelligence` on `SessionConfig`/`ResumeSessionConfig` (sent under `capi`); `session.SetAutoTierAsync(Some AutoTier.Fast)` / `SetAutoTierAsync None` |
| WebSocket responses | `EnableWebSocketResponses` is now sent as `capi.enableWebSocketResponses` |
| Ask-user variant | `AskUserVariant = Some AskUserVariant.Elicitation` (create + resume) |
| GitHub token provider | `GitHubTokenProvider = Some (fun args -> async { ... })` returning `GitHubTokenGranted` / `GitHubTokenCancelled`; mutually exclusive with `GitHubToken` |
| Sandbox | `SandboxConfig = Some { SandboxConfig.create true with AllowBypass = Some true }` |
| Feature flags | `FeatureFlags = Some (dict [ "flag", true ])` |
| Session detach | `session.DisconnectAsync()` sends `session.detach` (falls back to `session.destroy` on older CLIs) |
| Message source | `MessageOptions.Source = Some MessageSource.User` / `MessageSource.System` / `MessageSource.agent "id"` |
| Managed settings | `client.ClearManagedSettingsCacheAsync()`, `session.GetManagedSettingsAsync()`, `session.RecheckPolicyHelperAsync()` |
| Model allowlist | `AllowedModels = Some [ ... ]` |
| OAuth client metadata | `AuthClientIdMetadataUrl = Some "https://..."` |
| Max output tokens | `ModelCapabilities = Some (ModelLimitsOverride.toCapabilities { ... MaxOutputTokens = Some 16384 ... })` (`max_output_tokens`) |
| Structured output | `MessageOptions.ResponseSchema = Some schema`; `session.SendAndWaitStructuredAsync(options, schema)` |
| Name & directories | `Name` (create only), `AgentDirectories`, `HookDirectories` |
| Runtime toggles | `InteractiveUser`, `ExcludedManagedSettings`, `EnforceManagedModelDefaults`; create-only `TrustWorkingDirectory`, `RefreshCustomInstructions`; resume-only `AllowTranscriptRecovery` |
| Image generation | `ImageGeneration = Some { Enabled = Some true; Watermark = Some false; SaveToDisk = Some true }` |
| Autopilot | `Autopilot = Some { ContinuationEnabled = Some true; MaxContinues = Some 5 }` |
| Sub-agent hooks | `OnSubagentStart` / `OnSubagentStop` (dispatched from `hooks.invoke`; setting either sends `hooks: true`) |
| BYOK provider | `Provider = Some { ProviderConfig.create url with ModelProvider = Some ModelProvider.Ollama; ApiKeyCommand = Some "..." }` |

```fsharp
open System.Text.Json
open CopilotSDK.Supercharged.FSharp

// Identify the embedding application (sent with `connect` at startup)
use client =
    CopilotClient.create
        { CopilotClientOptions.defaults with
            ClientInfo =
                Some { CopilotClientInfo.empty with
                        ApplicationName = Some "my-ide"
                        ApplicationVersion = Some "1.4.0"
                        IntegrationName = Some "copilot-fsharp" } }

let config =
    { SessionConfig.defaults with
        Name = Some "release-triage"
        AutoTier = Some AutoTier.Intelligence            // experimental; sent as capi.autoTier
        AskUserVariant = Some AskUserVariant.Elicitation
        SandboxConfig = Some { SandboxConfig.create true with AllowBypass = Some true }
        FeatureFlags = Some (dict [ "my-flag", true ])
        AllowedModels = Some [ "gpt-5"; "claude-sonnet-4.5" ]
        AuthClientIdMetadataUrl = Some "https://example.com/oauth/client.json"
        AgentDirectories = Some [ "./agents" ]
        HookDirectories = Some [ "./hooks" ]
        InteractiveUser = Some false
        ExcludedManagedSettings = Some [ "model" ]
        EnforceManagedModelDefaults = Some true
        TrustWorkingDirectory = Some TrustWorkingDirectory.Session
        RefreshCustomInstructions = Some true
        ImageGeneration = Some { Enabled = Some true; Watermark = Some false; SaveToDisk = Some true }
        Autopilot = Some { ContinuationEnabled = Some true; MaxContinues = Some 5 }
        ModelCapabilities =
            Some (ModelLimitsOverride.toCapabilities
                    { MaxPromptTokens = None; MaxOutputTokens = Some 16384; MaxContextWindowTokens = None })
        // Per-session GitHub tokens (do not combine with GitHubToken)
        GitHubTokenProvider =
            Some (fun args -> async {
                // args.Host, args.SessionId, args.Reason ("initial" | "refresh")
                return GitHubTokenGranted { AccessToken = loadToken args.Host; TokenType = None; ExpiresIn = 28800 } })
        // Sub-agent lifecycle hooks
        OnSubagentStart =
            Some (fun input -> async {
                return Some { AdditionalContext = Some (sprintf "You are %s; be concise." input.AgentName) } })
        OnSubagentStop =
            Some (fun input -> async {
                return Some { Decision = None; Reason = None; ModifiedResponse = Some (input.Response.Trim()) } })
        // BYOK provider with a key fetched by a command before each request
        Provider =
            Some { ProviderConfig.create "http://localhost:11434/v1" with
                    ProviderType = Some "openai"
                    ModelProvider = Some ModelProvider.Ollama
                    ApiKeyCommand = Some "op read op://dev/ollama/key" } }

let! session = client.CreateSessionAsync(config)

// Message source and structured output
let! _ = session.SendAsync({ MessageOptions.create "Summarize the diff" with Source = Some (MessageSource.agent "reviewer") })
let schema =
    JsonDocument.Parse("""{"type":"object","properties":{"answer":{"type":"string"}},"required":["answer"]}""").RootElement
let! result = session.SendAndWaitStructuredAsync(MessageOptions.create "What is 2 + 2?", schema)
printfn "%s" (result.GetProperty("answer").GetString())

// Auto tier and managed settings
let! ack = session.SetAutoTierAsync(Some AutoTier.Fast)   // None returns to default Auto routing
let! settings = session.GetManagedSettingsAsync()
let! policy = session.RecheckPolicyHelperAsync()
let! _ = client.ClearManagedSettingsCacheAsync()

// Detach (state on disk is kept) and resume later
do! session.DisconnectAsync()
let! resumed =
    client.ResumeSessionAsync(
        { ResumeSessionConfig.create session.SessionId with
            AllowTranscriptRecovery = Some true
            AutoTier = Some AutoTier.Balance })
```

### Parity notes

- Host-owned external tool cancellation (`external_tool.requested` / `external_tool.completed`) is
  N/A for this SDK: tool invocations arrive on the SDK's tool-call request path.
- Experimental host-integration callbacks — skill provider, installation confirmation, extension
  launch provider, and Entra/Kerberos auth callbacks — are not yet available in this SDK.
- This SDK has no `SetModel` method, so the optional `autoTier` argument of
  `session.model.switchTo` is not exposed; use `SetAutoTierAsync` instead.

## Recent Features (v2.4–v2.5)

Parity options tracking the upstream `@github/copilot` SDK, exposed as fields on the
`SessionConfig`, `CopilotClientOptions`, and `MessageOptions` records. Build them with
`{ SessionConfig.defaults with ... }` / `{ CopilotClientOptions.defaults with ... }`.

### v2.5.0

- **Reasoning effort** — `ReasoningEffort = Some "high"` (`"low"`/`"medium"`/`"high"`/`"xhigh"`).
- **Session rewind** — `RewindEnabled = Some true`; roll back to an earlier snapshot (`session.snapshot_rewind` event).
- **Additional directories** — `AdditionalDirectories = Some [ ... ]`; extra workspace roots the agent may read.
- **Content exclusion** — `ContentExclusion = Some true`; honor repository content-exclusion rules.
- **Tool search** — `ToolSearch = Some (...)`; on-demand tool discovery for large tool sets.
- **Disabled MCP servers** — `DisabledMcpServers = Some [ ... ]`.
- **GitHub MCP tool config** — `GithubMcpToolConfig = Some (...)`.
- **Canvas provider** — `CanvasProvider = Some (...)`.
- **Custom agents local-only** — `CustomAgentsLocalOnly = Some true`.
- **Experimental mode** — `ExperimentalMode = Some true`.
- **User-prompt-transformed hook** — `OnUserPromptTransformed = Some (...)`.
- **Built-in plugin directories** — `CopilotClientOptions.BuiltinPluginDirectories = Some [ ... ]`.
- **In-process (FFI) transport** — `CopilotClientOptions.InProcess = Some true`.
- **Agent factory args schema** — custom agents accept a typed `ArgsSchema`.
- **Permission decision context** — permission results carry `DecisionContext`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions.BearerTokenProvider = Some (fun args -> ...)`; receives `ProviderTokenArgs`.
- **HTTP request handler** — `CopilotClientOptions.RequestHandler = Some (...)` (`CopilotRequestHandler`).
- **MCP OAuth token handler** — `OnMcpAuthRequest = Some (...)`.
- **Session citations** — `EnableCitations = Some true`.
- **Excluded built-in agents** — `ExcludedBuiltinAgents = Some [ ... ]`.
- **Session spending limits** — `SessionLimits = Some { MaxAiCredits = Some 100 }`.
- **Session memory** — `Memory = Some (...)` (`MemoryConfiguration`).
- **OTLP protocol** — `OtlpProtocol = Some "grpc"`.
- **WebSocket responses** — `EnableWebSocketResponses = Some true`.
- **Experiment assignments** — `ExpAssignments = Some (...)`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `OnPostToolUse`, `OnPreMcpToolCall`.
- **Per-message agent mode & display prompt** — `MessageOptions.AgentMode`, `MessageOptions.DisplayPrompt`.
- **System-message sections** — `SystemMessageSection.Preamble` / `.Preserve`.
- **Tool defer policy** — `ToolDefer.Auto` / `.Never`.
- **GitHub attachment variants** — `GitHubAttachment.GitHubCommit` / `.GitHubRepository`.

### Examples

```fsharp
// Reasoning effort — trade latency for depth on models that support it
let config =
    { SessionConfig.defaults with
        ReasoningEffort = Some "high" }   // "low" | "medium" | "high" | "xhigh"
```

```fsharp
// Roll back to an earlier snapshot, expose extra roots, and honor content exclusion
let config =
    { SessionConfig.defaults with
        RewindEnabled = Some true          // emits session.snapshot_rewind events
        AdditionalDirectories = Some [ "/repo/shared"; "/repo/docs" ]
        ContentExclusion = Some true }
```

```fsharp
// On-demand tool discovery for large tool sets
let config =
    { SessionConfig.defaults with
        ToolSearch = Some (dict [ "enabled", box true ]) }
```

```fsharp
// BYOK bearer tokens + in-process FFI transport (client-level)
use client =
    CopilotClient.create
        { CopilotClientOptions.defaults with
            BearerTokenProvider = Some (fun args -> async { return fetchToken args.SessionId })
            BuiltinPluginDirectories = Some [ "/opt/copilot/plugins" ]
            InProcess = Some true }
```

## F# Idioms

This SDK is designed around F# best practices:

- **Discriminated Unions** for `SessionEvent` - enables exhaustive pattern matching
- **Immutable Records** for all configuration types
- **Async Workflows** (`async { }`) for all asynchronous operations
- **Pipe Operators** (`|>`) for composable tool building and event filtering
- **Option Types** instead of nulls
- **Module Functions** (`CopilotClient.create`, `SessionConfig.defaults`) for a functional API
- **IObservable** integration for reactive event streams
- **Result<'T,'E>** pattern available via `ToolResultObject.success` / `ToolResultObject.failure`

## Building

```bash
cd fsharp/src
dotnet build
```

## Testing

```bash
cd fsharp/tests
dotnet test
```

## Examples

See [`examples/BasicExample.fsx`](examples/BasicExample.fsx) for a runnable F# script.

See the [`cookbook/`](cookbook/) directory for recipes covering common patterns.

namespace CopilotSDK.Supercharged.FSharp

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization

// ---------------------------------------------------------------------------
// Connection state
// ---------------------------------------------------------------------------

/// Represents the connection state of the Copilot client.
type ConnectionState =
    | Disconnected
    | Connecting
    | Connected
    | Error

// ---------------------------------------------------------------------------
// Permission handling
// ---------------------------------------------------------------------------

/// Kind of a permission-request result.
type PermissionResultKind =
    | ApproveOnce
    | Reject
    | UserNotAvailable
    | NoResult

module PermissionResultKind =
    let toWireString = function
        | ApproveOnce -> "approve-once"
        | Reject -> "reject"
        | UserNotAvailable -> "user-not-available"
        | NoResult -> "no-result"

/// Result returned by a permission-request handler.
type PermissionRequestResult =
    { Kind: PermissionResultKind
      UpdraftMessage: string option
      /// Opaque decision context forwarded with the permission reply.
      DecisionContext: IDictionary<string, obj> option }

/// Permission-request payload sent by the server.
type PermissionRequest =
    { [<JsonPropertyName("toolName")>]
      ToolName: string
      [<JsonPropertyName("toolCallId")>]
      ToolCallId: string
      [<JsonPropertyName("input")>]
      Input: JsonElement option }

/// Handler that decides whether to approve a tool invocation.
type PermissionRequestHandler = PermissionRequest -> Async<PermissionRequestResult>

// ---------------------------------------------------------------------------
// Telemetry configuration
// ---------------------------------------------------------------------------

/// OpenTelemetry configuration for the CLI server.
type TelemetryConfig =
    { OtlpEndpoint: string option
      FilePath: string option
      ExporterType: string option
      SourceName: string option
      CaptureContent: bool option }

// ---------------------------------------------------------------------------
// Session filesystem configuration
// ---------------------------------------------------------------------------

/// Configuration for a custom session filesystem provider.
type SessionFsConfig =
    { /// Initial working directory for sessions.
      InitialCwd: string
      /// Path where the runtime stores session-scoped files.
      SessionStatePath: string
      /// Path conventions ("posix" or "windows").
      Conventions: string }

// ---------------------------------------------------------------------------
// Upstream-sync feature types (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Per-session AI-credit budget; set MaxAiCredits to cap spend.
type SessionLimitsConfig =
    { /// Maximum AI credits the session may consume.
      MaxAiCredits: int option }

module SessionLimitsConfig =
    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: SessionLimitsConfig) =
        {| maxAiCredits = config.MaxAiCredits |}

/// Opt-in persistent session memory.
type MemoryConfiguration =
    { /// Whether persistent memory is enabled for the session.
      Enabled: bool }

module MemoryConfiguration =
    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: MemoryConfiguration) =
        {| enabled = config.Enabled |}

/// Arguments passed to a BYOK bearer-token provider (per-session scoping).
type ProviderTokenArgs =
    { /// The session the token is being requested for.
      SessionId: string }

/// Intercepts outbound LLM inference HTTP/WebSocket requests. Assign an
/// instance to CopilotClientOptions.RequestHandler and override SendRequest
/// to mutate, replace, or forward the request. BYOK providers may also set
/// BearerTokenProvider on the client options.
type CopilotRequestHandler() =
    /// Transform an outbound request; override in a derived type.
    abstract member SendRequest: request: JsonElement * context: JsonElement option -> JsonElement
    default _.SendRequest(request, _context) = request

/// Handler for an MCP OAuth host-token request; returns an OAuth access token.
type McpAuthRequestHandler = JsonElement option -> Async<string>

/// Hook invoked after a tool completes (post-tool-use).
type PostToolUseHandler = JsonElement option -> Async<unit>

/// Hook invoked before an MCP tool call (pre-MCP-tool-call).
type PreMcpToolCallHandler = JsonElement option -> Async<unit>

/// Hook invoked after the user prompt is transformed (user-prompt-transformed).
type UserPromptTransformedHandler = JsonElement option -> Async<unit>

/// Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto").
module ToolDefer =
    [<Literal>]
    let Auto = "auto"
    [<Literal>]
    let Never = "never"

/// System-message section identifiers (used with system-message overrides).
/// The "preamble" section targets the identity preamble; the "preserve" action
/// protects an individually-addressable section from a group-level remove.
module SystemMessageSection =
    [<Literal>]
    let Preamble = "preamble"
    [<Literal>]
    let Identity = "identity"
    [<Literal>]
    let ToolInstructions = "tool_instructions"
    [<Literal>]
    let Preserve = "preserve"

/// GitHub-anchored attachment variants.
module GitHubAttachment =
    [<Literal>]
    let GitHubCommit = "GitHubCommit"
    [<Literal>]
    let GitHubRelease = "GitHubRelease"
    [<Literal>]
    let GitHubActionsJob = "GitHubActionsJob"
    [<Literal>]
    let GitHubRepository = "GitHubRepository"
    [<Literal>]
    let GitHubFileDiff = "GitHubFileDiff"
    [<Literal>]
    let GitHubTreeComparison = "GitHubTreeComparison"
    [<Literal>]
    let GitHubUrl = "GitHubUrl"
    [<Literal>]
    let GitHubFile = "GitHubFile"
    [<Literal>]
    let GitHubSnippet = "GitHubSnippet"

// ---------------------------------------------------------------------------
// v2.6.0 upstream-sync feature types (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Identity of the application embedding the SDK. When any field is set, the
/// client sends a `connect` request with `clientInfo` before the handshake.
type CopilotClientInfo =
    { /// Host application name (wire: editorName).
      ApplicationName: string option
      /// Host application version (wire: editorVersion).
      ApplicationVersion: string option
      /// Integration/extension name (wire: extensionName).
      IntegrationName: string option
      /// Integration/extension version (wire: extensionVersion).
      IntegrationVersion: string option }

module CopilotClientInfo =
    /// Client info with every field unset.
    let empty : CopilotClientInfo =
        { ApplicationName = None
          ApplicationVersion = None
          IntegrationName = None
          IntegrationVersion = None }

    /// Build the `connect` clientInfo payload, dropping empty fields.
    /// Returns None when no field carries a non-empty value.
    let toWire (info: CopilotClientInfo) : IDictionary<string, obj> option =
        let wire = Dictionary<string, obj>()
        let add (key: string) (value: string option) =
            match value with
            | Some v when not (String.IsNullOrEmpty v) -> wire.[key] <- box v
            | _ -> ()
        add "editorName" info.ApplicationName
        add "editorVersion" info.ApplicationVersion
        add "extensionName" info.IntegrationName
        add "extensionVersion" info.IntegrationVersion
        if wire.Count > 0 then Some (wire :> IDictionary<string, obj>) else None

/// Auto-routing tier preference (experimental).
module AutoTier =
    [<Literal>]
    let Efficiency = "efficiency"
    [<Literal>]
    let Balance = "balance"
    [<Literal>]
    let Intelligence = "intelligence"
    [<Literal>]
    let Fast = "fast"

/// Ask-user tool variant ("legacy" or "elicitation").
module AskUserVariant =
    [<Literal>]
    let Legacy = "legacy"
    [<Literal>]
    let Elicitation = "elicitation"

/// Working-directory trust scope used on session creation.
module TrustWorkingDirectory =
    [<Literal>]
    let Session = "session"
    [<Literal>]
    let User = "user"

/// Origin of a message sent with `session.send`.
[<RequireQualifiedAccess>]
module MessageSource =
    /// The message comes from the user.
    [<Literal>]
    let User = "user"
    /// The message is a system message.
    [<Literal>]
    let System = "system"
    /// The message comes from another agent ("agent-<id>").
    let agent (agentId: string) = "agent-" + agentId

/// Product serving a BYOK model (ProviderConfig.ModelProvider).
module ModelProvider =
    [<Literal>]
    let OpenAI = "openai"
    [<Literal>]
    let Anthropic = "anthropic"
    [<Literal>]
    let AzureOpenAI = "azure_openai"
    [<Literal>]
    let Ollama = "ollama"
    [<Literal>]
    let LmStudio = "lm_studio"
    [<Literal>]
    let FoundryLocal = "foundry_local"
    [<Literal>]
    let LlamaCpp = "llama_cpp"

/// Arguments passed to a session's GitHub token provider.
type GitHubTokenProviderArgs =
    { /// GitHub host the token is requested for.
      Host: string
      /// Session the token is requested for, when known.
      SessionId: string option
      /// "initial" or "refresh".
      Reason: string }

/// A GitHub token granted by a provider.
type GitHubTokenGrant =
    { AccessToken: string
      TokenType: string option
      /// Remaining lifetime in seconds.
      ExpiresIn: int }

/// Result returned by a GitHub token provider.
type GitHubTokenProviderResult =
    | GitHubTokenGranted of GitHubTokenGrant
    | GitHubTokenCancelled

module GitHubTokenProviderResult =
    /// Build the `gitHubToken.getToken` JSON-RPC result.
    let toWire (result: GitHubTokenProviderResult) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        match result with
        | GitHubTokenGranted grant ->
            wire.["kind"] <- box "token"
            wire.["accessToken"] <- box grant.AccessToken
            grant.TokenType |> Option.iter (fun t -> wire.["tokenType"] <- box t)
            wire.["expiresIn"] <- box grant.ExpiresIn
        | GitHubTokenCancelled ->
            wire.["kind"] <- box "cancelled"
        wire :> IDictionary<string, obj>

/// Supplies GitHub tokens for one session (mutually exclusive with GitHubToken).
type GitHubTokenProvider = GitHubTokenProviderArgs -> Async<GitHubTokenProviderResult>

/// Sandbox configuration for a session.
type SandboxConfig =
    { /// Whether sandboxing is enabled.
      Enabled: bool
      /// User-managed policy fragment merged into the base policy.
      UserPolicy: IDictionary<string, obj> option
      /// Auto-add the working directory to read/write paths.
      AddCurrentWorkingDirectory: bool option
      /// Confine MCP servers launched by the session.
      SandboxMcpServers: bool option
      /// Confine language servers launched by the session.
      SandboxLspServers: bool option
      /// Confine extension processes launched by the session.
      SandboxExtensions: bool option
      /// Opt in to letting the agent request an individual command outside the
      /// sandbox through the normal permission flow.
      AllowBypass: bool option }

module SandboxConfig =
    /// Sandbox config with only `Enabled` set.
    let create (enabled: bool) : SandboxConfig =
        { Enabled = enabled
          UserPolicy = None
          AddCurrentWorkingDirectory = None
          SandboxMcpServers = None
          SandboxLspServers = None
          SandboxExtensions = None
          AllowBypass = None }

    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: SandboxConfig) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        wire.["enabled"] <- box config.Enabled
        config.UserPolicy |> Option.iter (fun v -> wire.["userPolicy"] <- box v)
        config.AddCurrentWorkingDirectory |> Option.iter (fun v -> wire.["addCurrentWorkingDirectory"] <- box v)
        config.SandboxMcpServers |> Option.iter (fun v -> wire.["sandboxMcpServers"] <- box v)
        config.SandboxLspServers |> Option.iter (fun v -> wire.["sandboxLspServers"] <- box v)
        config.SandboxExtensions |> Option.iter (fun v -> wire.["sandboxExtensions"] <- box v)
        config.AllowBypass |> Option.iter (fun v -> wire.["allowBypass"] <- box v)
        wire :> IDictionary<string, obj>

/// Session-level image generation configuration.
type ImageGenerationConfig =
    { Enabled: bool option
      Watermark: bool option
      SaveToDisk: bool option }

module ImageGenerationConfig =
    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: ImageGenerationConfig) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        config.Enabled |> Option.iter (fun v -> wire.["enabled"] <- box v)
        config.Watermark |> Option.iter (fun v -> wire.["watermark"] <- box v)
        config.SaveToDisk |> Option.iter (fun v -> wire.["saveToDisk"] <- box v)
        wire :> IDictionary<string, obj>

/// Autopilot continuation options.
type AutopilotConfig =
    { ContinuationEnabled: bool option
      MaxContinues: int option }

module AutopilotConfig =
    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: AutopilotConfig) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        config.ContinuationEnabled |> Option.iter (fun v -> wire.["continuationEnabled"] <- box v)
        config.MaxContinues |> Option.iter (fun v -> wire.["maxContinues"] <- box v)
        wire :> IDictionary<string, obj>

/// Token-limit overrides for the model capabilities override (`limits`).
type ModelLimitsOverride =
    { MaxPromptTokens: int option
      MaxOutputTokens: int option
      MaxContextWindowTokens: int option }

module ModelLimitsOverride =
    /// Build a `ModelCapabilities` override containing these limits
    /// (snake_case wire keys such as `max_output_tokens`).
    let toCapabilities (limits: ModelLimitsOverride) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        limits.MaxPromptTokens |> Option.iter (fun v -> wire.["max_prompt_tokens"] <- box v)
        limits.MaxOutputTokens |> Option.iter (fun v -> wire.["max_output_tokens"] <- box v)
        limits.MaxContextWindowTokens |> Option.iter (fun v -> wire.["max_context_window_tokens"] <- box v)
        let caps = Dictionary<string, obj>()
        caps.["limits"] <- box wire
        caps :> IDictionary<string, obj>

/// BYOK model provider configuration.
type ProviderConfig =
    { /// Provider type ("openai", "azure", or "anthropic").
      ProviderType: string option
      /// Wire API ("completions" or "responses").
      WireApi: string option
      /// API endpoint URL.
      BaseUrl: string
      /// Static API key.
      ApiKey: string option
      /// Command whose stdout is used as the API key before each request.
      ApiKeyCommand: string option
      /// Bearer token sent in the Authorization header.
      BearerToken: string option
      /// Product serving the model (see the ModelProvider module).
      ModelProvider: string option
      /// Azure API version (Azure providers only).
      AzureApiVersion: string option }

module ProviderConfig =
    /// Provider config pointing at baseUrl with every other field unset.
    let create (baseUrl: string) : ProviderConfig =
        { ProviderType = None
          WireApi = None
          BaseUrl = baseUrl
          ApiKey = None
          ApiKeyCommand = None
          BearerToken = None
          ModelProvider = None
          AzureApiVersion = None }

    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: ProviderConfig) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        config.ProviderType |> Option.iter (fun v -> wire.["type"] <- box v)
        config.WireApi |> Option.iter (fun v -> wire.["wireApi"] <- box v)
        wire.["baseUrl"] <- box config.BaseUrl
        config.ApiKey |> Option.iter (fun v -> wire.["apiKey"] <- box v)
        config.ApiKeyCommand |> Option.iter (fun v -> wire.["apiKeyCommand"] <- box v)
        config.BearerToken |> Option.iter (fun v -> wire.["bearerToken"] <- box v)
        config.ModelProvider |> Option.iter (fun v -> wire.["modelProvider"] <- box v)
        config.AzureApiVersion
        |> Option.iter (fun v ->
            let azure = Dictionary<string, obj>()
            azure.["apiVersion"] <- box v
            wire.["azure"] <- box azure)
        wire :> IDictionary<string, obj>

/// Input for the hook fired before a sub-agent's first turn (hook type "subagentStart").
type SubagentStartHookInput =
    { /// Hook timestamp (epoch milliseconds), when provided.
      Timestamp: int64 option
      /// Working directory, when provided.
      Cwd: string option
      TranscriptPath: string
      AgentName: string
      AgentDisplayName: string option
      AgentDescription: string option
      /// Raw hook payload as sent by the runtime.
      Raw: JsonElement }

/// Output for the sub-agent start hook; context is prepended to the child's prompt.
type SubagentStartHookOutput =
    { AdditionalContext: string option }

/// Input for the hook fired after a sub-agent completes a turn (hook type "subagentStop").
type SubagentStopHookInput =
    { Timestamp: int64 option
      Cwd: string option
      TranscriptPath: string
      AgentName: string
      AgentDisplayName: string option
      AgentDescription: string option
      AgentId: string option
      AgentType: string
      /// Always "end_turn".
      StopReason: string
      /// The sub-agent's last assistant message.
      Response: string
      Raw: JsonElement }

/// Output for the sub-agent stop hook. `Decision = Some "block"` with a reason
/// continues the child; otherwise `ModifiedResponse` rewrites the reported response.
type SubagentStopHookOutput =
    { Decision: string option
      Reason: string option
      ModifiedResponse: string option }

/// Hook invoked before a sub-agent starts.
type SubagentStartHandler = SubagentStartHookInput -> Async<SubagentStartHookOutput option>

/// Hook invoked after a sub-agent stops.
type SubagentStopHandler = SubagentStopHookInput -> Async<SubagentStopHookOutput option>

module SubagentHooks =
    let private tryString (elem: JsonElement) (name: string) =
        match elem.TryGetProperty(name) with
        | true, v when v.ValueKind = JsonValueKind.String -> Some (v.GetString())
        | _ -> None

    let private tryInt64 (elem: JsonElement) (name: string) =
        match elem.TryGetProperty(name) with
        | true, v when v.ValueKind = JsonValueKind.Number ->
            match v.TryGetInt64() with
            | true, n -> Some n
            | _ -> None
        | _ -> None

    /// Parse a "subagentStart" hook payload.
    let parseStartInput (elem: JsonElement) : SubagentStartHookInput =
        { Timestamp = tryInt64 elem "timestamp"
          Cwd = tryString elem "cwd"
          TranscriptPath = tryString elem "transcriptPath" |> Option.defaultValue ""
          AgentName = tryString elem "agentName" |> Option.defaultValue ""
          AgentDisplayName = tryString elem "agentDisplayName"
          AgentDescription = tryString elem "agentDescription"
          Raw = elem }

    /// Parse a "subagentStop" hook payload.
    let parseStopInput (elem: JsonElement) : SubagentStopHookInput =
        { Timestamp = tryInt64 elem "timestamp"
          Cwd = tryString elem "cwd"
          TranscriptPath = tryString elem "transcriptPath" |> Option.defaultValue ""
          AgentName = tryString elem "agentName" |> Option.defaultValue ""
          AgentDisplayName = tryString elem "agentDisplayName"
          AgentDescription = tryString elem "agentDescription"
          AgentId = tryString elem "agentId"
          AgentType = tryString elem "agentType" |> Option.defaultValue ""
          StopReason = tryString elem "stopReason" |> Option.defaultValue "end_turn"
          Response = tryString elem "response" |> Option.defaultValue ""
          Raw = elem }

    /// Build the wire output for a sub-agent start hook.
    let startOutputToWire (output: SubagentStartHookOutput) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        output.AdditionalContext |> Option.iter (fun v -> wire.["additionalContext"] <- box v)
        wire :> IDictionary<string, obj>

    /// Build the wire output for a sub-agent stop hook.
    let stopOutputToWire (output: SubagentStopHookOutput) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        output.Decision |> Option.iter (fun v -> wire.["decision"] <- box v)
        output.Reason |> Option.iter (fun v -> wire.["reason"] <- box v)
        output.ModifiedResponse |> Option.iter (fun v -> wire.["modifiedResponse"] <- box v)
        wire :> IDictionary<string, obj>

// ---------------------------------------------------------------------------
// Tool definition
// ---------------------------------------------------------------------------

/// Schema for a tool's parameters (JSON Schema object).
type ToolParameterSchema = JsonElement

/// Definition of a tool that the assistant can invoke.
type Tool =
    { /// Unique tool name.
      Name: string
      /// Human-readable description.
      Description: string
      /// JSON Schema for parameters.
      Parameters: ToolParameterSchema option
      /// Whether repeating a call with the same arguments is safe (no extra
      /// side effects). Sent as `idempotent` in `tools.definitions`.
      Idempotent: bool option }

module Tool =
    /// Build the `tools.definitions` wire entry for a tool.
    let toWire (tool: Tool) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        wire.["name"] <- box tool.Name
        wire.["description"] <- box tool.Description
        tool.Parameters |> Option.iter (fun v -> wire.["parameters"] <- box v)
        tool.Idempotent |> Option.iter (fun v -> wire.["idempotent"] <- box v)
        wire :> IDictionary<string, obj>

/// Context passed to a tool handler during invocation.
type ToolInvocation =
    { SessionId: string
      ToolCallId: string
      ToolName: string
      Arguments: JsonElement option }

/// Handler function for tool invocations.
type ToolHandler = ToolInvocation -> Async<obj>

/// A registered tool pairing its definition with its handler.
type RegisteredTool =
    { Tool: Tool
      Handler: ToolHandler }

// ---------------------------------------------------------------------------
// v3.0.0 upstream-sync feature types (SDK protocol 4)
// ---------------------------------------------------------------------------

/// Client mode. "coding-agent" (the default) keeps today's behavior; "empty"
/// requires every session to list AvailableTools and applies locked-down
/// defaults under the caller's values. "copilot-cli" is a legacy alias of
/// "coding-agent".
module ClientMode =
    [<Literal>]
    let CodingAgent = "coding-agent"
    [<Literal>]
    let Empty = "empty"
    /// Legacy alias of CodingAgent.
    [<Literal>]
    let CopilotCli = "copilot-cli"

    /// Normalize a mode option to "coding-agent" or "empty". Fails on any other value.
    let normalize (mode: string option) : string =
        match mode with
        | None
        | Some CodingAgent
        | Some CopilotCli -> CodingAgent
        | Some Empty -> Empty
        | Some other ->
            invalidArg "mode" (sprintf "Mode must be '%s' or '%s', got '%s'" CodingAgent Empty other)

/// Named BYOK provider connection (inference.providers[]).
type NamedProviderConfig =
    { /// Connection name referenced by ProviderModelConfig.Provider and DefaultProvider.
      Name: string
      /// Provider type ("openai", "azure", or "anthropic").
      ProviderType: string option
      /// API endpoint URL.
      BaseUrl: string
      ApiKey: string option
      /// Command whose stdout is used as the API key before each request.
      ApiKeyCommand: string option
      BearerToken: string option
      /// Wire API ("completions" or "responses").
      WireApi: string option
      /// Transport ("http" or "websocket").
      Transport: string option
      /// Azure API version (Azure providers only).
      AzureApiVersion: string option
      /// Extra HTTP headers sent with each request.
      Headers: IDictionary<string, string> option
      /// Product serving the model (see the ModelProvider module).
      ModelProvider: string option }

module NamedProviderConfig =
    /// Named provider pointing at baseUrl with every other field unset.
    let create (name: string) (baseUrl: string) : NamedProviderConfig =
        { Name = name
          ProviderType = None
          BaseUrl = baseUrl
          ApiKey = None
          ApiKeyCommand = None
          BearerToken = None
          WireApi = None
          Transport = None
          AzureApiVersion = None
          Headers = None
          ModelProvider = None }

    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: NamedProviderConfig) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        wire.["name"] <- box config.Name
        config.ProviderType |> Option.iter (fun v -> wire.["type"] <- box v)
        wire.["baseUrl"] <- box config.BaseUrl
        config.ApiKey |> Option.iter (fun v -> wire.["apiKey"] <- box v)
        config.ApiKeyCommand |> Option.iter (fun v -> wire.["apiKeyCommand"] <- box v)
        config.BearerToken |> Option.iter (fun v -> wire.["bearerToken"] <- box v)
        config.WireApi |> Option.iter (fun v -> wire.["wireApi"] <- box v)
        config.Transport |> Option.iter (fun v -> wire.["transport"] <- box v)
        config.AzureApiVersion
        |> Option.iter (fun v ->
            let azure = Dictionary<string, obj>()
            azure.["apiVersion"] <- box v
            wire.["azure"] <- box azure)
        config.Headers |> Option.iter (fun v -> wire.["headers"] <- box (Dictionary<string, string>(v)))
        config.ModelProvider |> Option.iter (fun v -> wire.["modelProvider"] <- box v)
        wire :> IDictionary<string, obj>

/// A model served by a named BYOK provider (inference.models[]). Select it
/// with the session model "provider/id".
type ProviderModelConfig =
    { /// Name of the NamedProviderConfig serving this model.
      Provider: string
      /// Model id used in "provider/id" selections.
      Id: string
      /// Model id sent to the provider, when different from Id.
      ModelId: string option
      /// Model name sent on the wire, when different from ModelId.
      WireModel: string option
      /// Display name.
      Name: string option
      MaxPromptTokens: int option
      MaxContextWindowTokens: int option
      MaxOutputTokens: int option
      /// Model capabilities override.
      Capabilities: IDictionary<string, obj> option }

module ProviderModelConfig =
    /// Model entry with every optional field unset.
    let create (provider: string) (id: string) : ProviderModelConfig =
        { Provider = provider
          Id = id
          ModelId = None
          WireModel = None
          Name = None
          MaxPromptTokens = None
          MaxContextWindowTokens = None
          MaxOutputTokens = None
          Capabilities = None }

    /// Build the camelCase JSON-RPC wire payload.
    let toWire (config: ProviderModelConfig) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        wire.["provider"] <- box config.Provider
        wire.["id"] <- box config.Id
        config.ModelId |> Option.iter (fun v -> wire.["modelId"] <- box v)
        config.WireModel |> Option.iter (fun v -> wire.["wireModel"] <- box v)
        config.Name |> Option.iter (fun v -> wire.["name"] <- box v)
        config.MaxPromptTokens |> Option.iter (fun v -> wire.["maxPromptTokens"] <- box v)
        config.MaxContextWindowTokens |> Option.iter (fun v -> wire.["maxContextWindowTokens"] <- box v)
        config.MaxOutputTokens |> Option.iter (fun v -> wire.["maxOutputTokens"] <- box v)
        config.Capabilities |> Option.iter (fun v -> wire.["capabilities"] <- box v)
        wire :> IDictionary<string, obj>

/// Traffic served by the network callbacks (NetworkOptions.Scope).
module NetworkScope =
    [<Literal>]
    let Connection = "connection"
    [<Literal>]
    let Runtime = "runtime"

/// Payload of a `network.authenticateBasic` request.
type NetworkBasicAuthRequest =
    { /// Proxy that challenged, as "http://host:port" or "https://host:port".
      Proxy: string
      /// Realm named by the proxy (untrusted text), when any.
      Realm: string option
      /// True when the proxy rejected the credentials sent last.
      PreviousRejected: bool }

/// Proxy Basic credentials. Never log or persist them.
type NetworkBasicCredentials =
    { Username: string option
      Password: string option }

/// Host-provided network callbacks. When any callback is set, the client
/// registers them with `network.setProvider` after connecting.
type NetworkOptions =
    { /// "connection" (default) or "runtime" (see NetworkScope).
      Scope: string option
      /// Returns a PAC-style proxy list for a URL (None leaves it to the runtime).
      ResolveProxy: (string -> Async<string option>) option
      /// Returns a base64 SPNEGO token for a service principal name (None when unavailable).
      AuthenticateNegotiate: (string -> Async<string option>) option
      /// Returns proxy Basic credentials (None when unavailable).
      AuthenticateBasic: (NetworkBasicAuthRequest -> Async<NetworkBasicCredentials option>) option }

module NetworkOptions =
    /// Network options with no callbacks.
    let empty : NetworkOptions =
        { Scope = None
          ResolveProxy = None
          AuthenticateNegotiate = None
          AuthenticateBasic = None }

    /// True when at least one callback is registered.
    let hasCallbacks (options: NetworkOptions) =
        options.ResolveProxy.IsSome || options.AuthenticateNegotiate.IsSome || options.AuthenticateBasic.IsSome

    /// Build the `network.setProvider` params (an empty object marks each registered callback).
    let toSetProviderParams (options: NetworkOptions) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        options.Scope |> Option.iter (fun v -> wire.["scope"] <- box v)
        if options.ResolveProxy.IsSome then wire.["resolveProxy"] <- box (Dictionary<string, obj>())
        if options.AuthenticateNegotiate.IsSome then wire.["authenticateNegotiate"] <- box (Dictionary<string, obj>())
        if options.AuthenticateBasic.IsSome then wire.["authenticateBasic"] <- box (Dictionary<string, obj>())
        wire :> IDictionary<string, obj>

/// Kind of input requested by a shell authentication prompt.
module ShellAuthenticationInputKind =
    /// A password or passphrase (mask the input).
    [<Literal>]
    let Secret = "secret"
    /// A visible answer such as a user name.
    [<Literal>]
    let Text = "text"
    /// A yes/no question; answer "yes" to accept.
    [<Literal>]
    let Confirm = "confirm"

/// A `shellAuthentication.request` from a program (sudo, ssh, git) run by the agent.
type ShellAuthenticationRequest =
    { SessionId: string
      RequestId: string
      /// The program that asked: "sudo", "ssh", or "git".
      Program: string
      /// The prompt text the program supplied.
      Prompt: string
      /// "secret", "text", or "confirm" (see ShellAuthenticationInputKind).
      InputKind: string
      /// The shell command that triggered the prompt.
      Command: string
      /// The command's working directory.
      Cwd: string }

/// Answer to a shell authentication request.
type ShellAuthenticationResult =
    | ShellAuthenticationAccept of value: string
    | ShellAuthenticationCancel

module ShellAuthenticationResult =
    /// Build the `shellAuthentication.request` JSON-RPC result.
    let toWire (result: ShellAuthenticationResult) : IDictionary<string, obj> =
        let wire = Dictionary<string, obj>()
        match result with
        | ShellAuthenticationAccept value ->
            wire.["action"] <- box "accept"
            wire.["value"] <- box value
        | ShellAuthenticationCancel ->
            wire.["action"] <- box "cancel"
        wire :> IDictionary<string, obj>

/// Answers shell authentication prompts. The CancellationToken fires when the
/// answer is no longer needed (`shellAuthentication.cancel`).
type ShellAuthenticationHandler =
    ShellAuthenticationRequest -> System.Threading.CancellationToken -> Async<ShellAuthenticationResult>

/// A fact the agent asked the host to remember (`memoryStore.store`).
type StoredMemory =
    { SessionId: string
      /// Short topic the memory is about.
      Subject: string
      /// The fact to remember.
      Fact: string
      /// Sources supporting the fact, such as file paths with line numbers.
      Citations: string
      /// Why the agent considers the fact worth remembering.
      Reason: string }

/// Persists memories for a session (sends `state.hasMemoryStore`).
type StoreMemoryHandler = StoredMemory -> Async<unit>

/// One field-specific finding reported by a Policy Helper.
type PolicyHelperFailureDetail =
    { Message: string
      Path: string option }

/// Validated help link supplied by a Policy Helper.
type PolicyHelperHelpLink =
    { Url: string
      Label: string option }

/// The latest unresolved Policy Helper failure. Helper-authored text is
/// untrusted: render it as plain text and never execute it.
type PolicyHelperFailure =
    { /// Runtime classification such as "spawn", "timeout", or "schema".
      Category: string
      /// "ignore" or "failClosed".
      FailureMode: string
      Message: string
      PreviousPolicyRetained: bool
      /// Invocation reason such as "sessionStart".
      Reason: string
      Authority: string option
      Code: string option
      Details: PolicyHelperFailureDetail list option
      Diagnostic: string option
      HelpLink: PolicyHelperHelpLink option
      Remediation: string option
      Severity: string option }

module PolicyHelperFailure =
    let private tryString (elem: JsonElement) (name: string) =
        match elem.TryGetProperty(name) with
        | true, v when v.ValueKind = JsonValueKind.String -> Some (v.GetString())
        | _ -> None

    /// Parse a PolicyHelperFailure JSON object. Returns None when the value is not an object.
    let tryParse (elem: JsonElement) : PolicyHelperFailure option =
        if elem.ValueKind <> JsonValueKind.Object then None
        else
            let details =
                match elem.TryGetProperty("details") with
                | true, v when v.ValueKind = JsonValueKind.Array ->
                    v.EnumerateArray()
                    |> Seq.filter (fun d -> d.ValueKind = JsonValueKind.Object)
                    |> Seq.map (fun d ->
                        ({ Message = tryString d "message" |> Option.defaultValue ""
                           Path = tryString d "path" } : PolicyHelperFailureDetail))
                    |> Seq.toList
                    |> Some
                | _ -> None
            let helpLink =
                match elem.TryGetProperty("helpLink") with
                | true, v when v.ValueKind = JsonValueKind.Object ->
                    tryString v "url"
                    |> Option.map (fun url -> ({ Url = url; Label = tryString v "label" } : PolicyHelperHelpLink))
                | _ -> None
            Some
                ({ Category = tryString elem "category" |> Option.defaultValue ""
                   FailureMode = tryString elem "failureMode" |> Option.defaultValue ""
                   Message = tryString elem "message" |> Option.defaultValue ""
                   PreviousPolicyRetained =
                     match elem.TryGetProperty("previousPolicyRetained") with
                     | true, v when v.ValueKind = JsonValueKind.True -> true
                     | _ -> false
                   Reason = tryString elem "reason" |> Option.defaultValue ""
                   Authority = tryString elem "authority"
                   Code = tryString elem "code"
                   Details = details
                   Diagnostic = tryString elem "diagnostic"
                   HelpLink = helpLink
                   Remediation = tryString elem "remediation"
                   Severity = tryString elem "severity" } : PolicyHelperFailure)

    /// Read `policyHelperFailure` from JSON-RPC error data (session.create/resume failures).
    let tryFromErrorData (data: JsonElement option) : PolicyHelperFailure option =
        match data with
        | Some d when d.ValueKind = JsonValueKind.Object ->
            match d.TryGetProperty("policyHelperFailure") with
            | true, v -> tryParse v
            | _ -> None
        | _ -> None

/// Session permission modes (session.permissions.setMode).
module PermissionMode =
    /// Permission requests follow the normal approval flow.
    [<Literal>]
    let Manual = "manual"
    /// Requests carry an LLM safety recommendation.
    [<Literal>]
    let Assisted = "assisted"
    /// Tool, path, and URL permission requests are approved automatically.
    [<Literal>]
    let AllowAll = "allow-all"

/// Why a permission-mode change was refused (PermissionModeSetResult.Reason).
module PermissionModeRefusalReason =
    /// Managed (enterprise) policy does not allow the requested mode.
    [<Literal>]
    let ManagedPolicy = "managed_policy"

/// Result of `session.permissions.setMode`.
type PermissionModeSetResult =
    { Success: bool
      /// Authoritative permission mode after the change.
      Mode: string
      /// Why the requested mode was refused, when known (see PermissionModeRefusalReason).
      Reason: string option }

// ---------------------------------------------------------------------------
// Client options
// ---------------------------------------------------------------------------

/// Configuration options for creating a CopilotClient.
type CopilotClientOptions =
    { /// Path to the Copilot CLI executable.
      CliPath: string option
      /// Extra arguments prepended before SDK-managed flags.
      CliArgs: string list
      /// Working directory for the CLI process.
      Cwd: string option
      /// Port number for the CLI server (0 for random).
      Port: int
      /// Use stdio transport instead of TCP.
      UseStdio: bool
      /// URL of an existing CLI server to connect to.
      CliUrl: string option
      /// Log level for the CLI server.
      LogLevel: string
      /// Auto-start server on first use.
      AutoStart: bool
      /// Environment variables for the CLI process.
      Environment: IDictionary<string, string> option
      /// GitHub token for authentication.
      GitHubToken: string option
      /// Use the logged-in user for authentication.
      UseLoggedInUser: bool option
      /// OpenTelemetry configuration.
      Telemetry: TelemetryConfig option
      /// Session idle timeout in seconds (None = no timeout).
      SessionIdleTimeoutSeconds: int option
      /// Session filesystem provider configuration.
      SessionFs: SessionFsConfig option
      /// Override the Copilot home directory.
      CopilotHome: string option
      /// Token for TCP connection authentication.
      TcpConnectionToken: string option
      /// HTTP request handler for intercepting outbound LLM inference requests.
      RequestHandler: CopilotRequestHandler option
      /// BYOK bearer-token provider invoked per session (receives ProviderTokenArgs).
      BearerTokenProvider: (ProviderTokenArgs -> Async<string>) option
      /// Directories to search for built-in plugins.
      BuiltinPluginDirectories: string list option
      /// Run the CLI in-process via the FFI transport instead of spawning a process.
      InProcess: bool option
      /// Identity of the embedding application, sent with `connect` at startup.
      ClientInfo: CopilotClientInfo option
      /// Client mode: ClientMode.CodingAgent (default when None) or ClientMode.Empty.
      Mode: string option
      /// Host network callbacks (proxy resolution, Negotiate/Basic proxy auth).
      Network: NetworkOptions option }

module CopilotClientOptions =
    /// Default client options (stdio, auto-start, info log level).
    let defaults =
        { CliPath = None
          CliArgs = []
          Cwd = None
          Port = 0
          UseStdio = true
          CliUrl = None
          LogLevel = "info"
          AutoStart = true
          Environment = None
          GitHubToken = None
          UseLoggedInUser = None
          Telemetry = None
          SessionIdleTimeoutSeconds = None
          SessionFs = None
          CopilotHome = None
          TcpConnectionToken = None
          RequestHandler = None
          BearerTokenProvider = None
          BuiltinPluginDirectories = None
          InProcess = None
          ClientInfo = None
          Mode = None
          Network = None }

// ---------------------------------------------------------------------------
// MCP server configuration
// ---------------------------------------------------------------------------

/// Configuration for an MCP stdio (local) server.
type MCPStdioServerConfig =
    { Tools: string list option
      Type: string option
      Timeout: int option
      Command: string
      Args: string list option
      Env: IDictionary<string, string> option
      Cwd: string option }

/// Configuration for an MCP HTTP (remote) server.
type MCPHttpServerConfig =
    { Tools: string list option
      Type: string
      Timeout: int option
      Url: string
      Headers: IDictionary<string, string> option }

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

/// Definition of a slash command.
type CommandDefinition =
    { Name: string
      Description: string option
      /// JSON Schema describing the command's arguments.
      ArgsSchema: IDictionary<string, obj> option }

// ---------------------------------------------------------------------------
// Response format / image options
// ---------------------------------------------------------------------------

/// Image generation options.
type ImageOptions =
    { Size: string option
      Quality: string option
      Style: string option }

// ---------------------------------------------------------------------------
// Attachments
// ---------------------------------------------------------------------------

/// File attachment for a message.
type Attachment =
    { [<JsonPropertyName("type")>]
      Type: string
      [<JsonPropertyName("path")>]
      Path: string option
      [<JsonPropertyName("url")>]
      Url: string option
      [<JsonPropertyName("content")>]
      Content: string option }

// ---------------------------------------------------------------------------
// Message options
// ---------------------------------------------------------------------------

/// Options for sending a message to a session.
type MessageOptions =
    { /// The prompt text to send.
      Prompt: string
      /// Optional file attachments.
      Attachments: Attachment list option
      /// Optional mode override.
      Mode: string option
      /// Optional agent-mode override for this message.
      AgentMode: string option
      /// Alternate prompt text shown to the user (the model still sees Prompt).
      DisplayPrompt: string option
      /// Per-message request headers forwarded with the turn.
      RequestHeaders: IDictionary<string, string> option
      /// Message origin: MessageSource.User, MessageSource.System, or MessageSource.agent id.
      Source: string option
      /// JSON Schema for structured output (sent as a json_schema responseFormat).
      ResponseSchema: JsonElement option
      /// Opaque caller-chosen turn identifier echoed on the turn's events.
      ClientTurnId: string option }

module MessageOptions =
    /// Message options carrying only a prompt.
    let create (prompt: string) : MessageOptions =
        { Prompt = prompt
          Attachments = None
          Mode = None
          AgentMode = None
          DisplayPrompt = None
          RequestHeaders = None
          Source = None
          ResponseSchema = None
          ClientTurnId = None }

    /// Build the structured-output `responseFormat` payload for a JSON Schema.
    let responseFormatFor (schema: JsonElement) : IDictionary<string, obj> =
        let jsonSchema = Dictionary<string, obj>()
        jsonSchema.["name"] <- box "response"
        jsonSchema.["strict"] <- box true
        jsonSchema.["schema"] <- box schema
        let format = Dictionary<string, obj>()
        format.["type"] <- box "json_schema"
        format.["jsonSchema"] <- box jsonSchema
        format :> IDictionary<string, obj>

// ---------------------------------------------------------------------------
// Elicitation handling
// ---------------------------------------------------------------------------

/// Context for an elicitation request from the server.
type ElicitationContext =
    { SessionId: string
      Message: string
      RequestedSchema: IDictionary<string, obj> option
      Mode: string option }

/// Result returned from an elicitation handler.
type ElicitationResult =
    { Action: string
      Content: IDictionary<string, obj> option }

/// Handler for elicitation requests.
type ElicitationHandler = ElicitationContext -> Async<ElicitationResult>

// ---------------------------------------------------------------------------
// Session configuration
// ---------------------------------------------------------------------------

/// Configuration for creating a new session.
type SessionConfig =
    { /// Model to use (e.g. "gpt-5").
      Model: string option
      /// Handler for permission requests (required).
      OnPermissionRequest: PermissionRequestHandler
      /// System message override.
      SystemMessage: string option
      /// Enable streaming events.
      Streaming: bool
      /// Custom agent name.
      Agent: string option
      /// Skill directories to load.
      SkillDirectories: string list option
      /// Include sub-agent streaming events.
      IncludeSubAgentStreamingEvents: bool
      /// Tools to exclude from the session.
      ExcludedTools: string list option
      /// Slash commands registered for this session.
      Commands: CommandDefinition list option
      /// Additional request headers sent with each turn.
      RequestHeaders: IDictionary<string, string> option
      /// Override model capabilities.
      ModelCapabilities: IDictionary<string, obj> option
      /// Enable automatic config discovery.
      EnableConfigDiscovery: bool option
      /// MCP server configurations (keyed by name).
      McpServers: IDictionary<string, obj> option
      /// Response format ("text", "image", "json_object").
      ResponseFormat: string option
      /// Image generation options.
      ImageOptions: ImageOptions option
      /// Handler for elicitation requests from the server.
      OnElicitationRequest: ElicitationHandler option
      /// Per-session GitHub token.
      GitHubToken: string option
      /// Directories to search for instruction files.
      InstructionDirectories: string list option
      /// Enable inline source citations in assistant responses.
      EnableCitations: bool option
      /// Built-in agents to exclude from this session.
      ExcludedBuiltinAgents: string list option
      /// Per-session AI-credit spending limits.
      SessionLimits: SessionLimitsConfig option
      /// Persistent session memory configuration.
      Memory: MemoryConfiguration option
      /// OTLP telemetry protocol ("grpc" or "http/protobuf").
      OtlpProtocol: string option
      /// Stream assistant responses over a WebSocket transport.
      EnableWebSocketResponses: bool option
      /// Experiment assignments forwarded to the runtime.
      ExpAssignments: IDictionary<string, obj> option
      /// Handler for MCP OAuth host-token requests.
      OnMcpAuthRequest: McpAuthRequestHandler option
      /// Hook invoked after a tool completes (post-tool-use).
      OnPostToolUse: PostToolUseHandler option
      /// Hook invoked before an MCP tool call (pre-MCP-tool-call).
      OnPreMcpToolCall: PreMcpToolCallHandler option
      /// Hook invoked after the user prompt is transformed (user-prompt-transformed).
      OnUserPromptTransformed: UserPromptTransformedHandler option
      /// Enable rewinding the session to an earlier point.
      RewindEnabled: bool option
      /// Additional workspace directories the session may access.
      AdditionalDirectories: string list option
      /// MCP servers to disable for this session.
      DisabledMcpServers: string list option
      /// Configuration for the GitHub MCP tool.
      GithubMcpToolConfig: IDictionary<string, obj> option
      /// Canvas provider configuration.
      CanvasProvider: IDictionary<string, obj> option
      /// Load custom agents from local directories only.
      CustomAgentsLocalOnly: bool option
      /// Tool-search configuration.
      ToolSearch: IDictionary<string, obj> option
      /// Enable experimental runtime mode.
      ExperimentalMode: bool option
      /// Enable content-exclusion enforcement.
      ContentExclusion: bool option
      /// Reasoning effort hint for the model ("low", "medium", "high").
      ReasoningEffort: string option
      // --- v2.6.0 upstream-sync session options ---
      /// Auto-routing tier (see AutoTier; experimental). Sent under "capi".
      AutoTier: string option
      /// Ask-user tool variant (see AskUserVariant).
      AskUserVariant: string option
      /// Per-session GitHub token provider (mutually exclusive with GitHubToken).
      GitHubTokenProvider: GitHubTokenProvider option
      /// Sandbox configuration.
      SandboxConfig: SandboxConfig option
      /// Runtime feature flags (sent even when empty).
      FeatureFlags: IDictionary<string, bool> option
      /// Restrict the models this session may use.
      AllowedModels: string list option
      /// OAuth client ID metadata document URL.
      AuthClientIdMetadataUrl: string option
      /// Human-readable session name.
      Name: string option
      /// Directories to load custom agents from.
      AgentDirectories: string list option
      /// Directories to load hook definitions from.
      HookDirectories: string list option
      /// Whether an interactive user is present.
      InteractiveUser: bool option
      /// Managed settings to exclude.
      ExcludedManagedSettings: string list option
      /// Enforce managed model defaults.
      EnforceManagedModelDefaults: bool option
      /// Trust scope for the working directory (see TrustWorkingDirectory).
      TrustWorkingDirectory: string option
      /// Re-read custom instruction files.
      RefreshCustomInstructions: bool option
      /// Session-level image generation configuration.
      ImageGeneration: ImageGenerationConfig option
      /// Autopilot continuation options.
      Autopilot: AutopilotConfig option
      /// Hook invoked before a sub-agent starts ("subagentStart").
      OnSubagentStart: SubagentStartHandler option
      /// Hook invoked after a sub-agent stops ("subagentStop").
      OnSubagentStop: SubagentStopHandler option
      /// BYOK model provider configuration.
      Provider: ProviderConfig option
      // --- v3.0.0 (SDK protocol 4) session options ---
      /// Tools declared to the runtime (tools.definitions) and registered on the session.
      Tools: RegisteredTool list option
      /// Allow-list of tool names (required for every session in ClientMode.Empty).
      AvailableTools: string list option
      /// Named BYOK provider connections (inference.providers).
      Providers: NamedProviderConfig list option
      /// Models served by the named providers (inference.models).
      Models: ProviderModelConfig list option
      /// Provider used for unqualified model selections (inference.defaultProvider).
      DefaultProvider: string option
      /// Whether Copilot API models are available (github.capi.enabled); Some false for BYOK only.
      CapiEnabled: bool option
      /// Answers sudo/ssh/git prompts (sends interaction.requestShellAuthentication).
      OnShellAuthenticationRequest: ShellAuthenticationHandler option
      /// Persists agent memories (sends state.hasMemoryStore).
      OnStoreMemory: StoreMemoryHandler option }

module SessionConfig =
    /// Approve all permission requests automatically.
    let approveAll : PermissionRequestHandler =
        fun _ -> async { return { Kind = ApproveOnce; UpdraftMessage = None; DecisionContext = None } }

    /// Reject all permission requests.
    let rejectAll : PermissionRequestHandler =
        fun _ -> async { return { Kind = Reject; UpdraftMessage = None; DecisionContext = None } }

    /// Default session config with approveAll handler.
    let defaults =
        { Model = None
          OnPermissionRequest = approveAll
          SystemMessage = None
          Streaming = true
          Agent = None
          SkillDirectories = None
          IncludeSubAgentStreamingEvents = false
          ExcludedTools = None
          Commands = None
          RequestHeaders = None
          ModelCapabilities = None
          EnableConfigDiscovery = None
          McpServers = None
          ResponseFormat = None
          ImageOptions = None
          OnElicitationRequest = None
          GitHubToken = None
          InstructionDirectories = None
          EnableCitations = None
          ExcludedBuiltinAgents = None
          SessionLimits = None
          Memory = None
          OtlpProtocol = None
          EnableWebSocketResponses = None
          ExpAssignments = None
          OnMcpAuthRequest = None
          OnPostToolUse = None
          OnPreMcpToolCall = None
          OnUserPromptTransformed = None
          RewindEnabled = None
          AdditionalDirectories = None
          DisabledMcpServers = None
          GithubMcpToolConfig = None
          CanvasProvider = None
          CustomAgentsLocalOnly = None
          ToolSearch = None
          ExperimentalMode = None
          ContentExclusion = None
          ReasoningEffort = None
          AutoTier = None
          AskUserVariant = None
          GitHubTokenProvider = None
          SandboxConfig = None
          FeatureFlags = None
          AllowedModels = None
          AuthClientIdMetadataUrl = None
          Name = None
          AgentDirectories = None
          HookDirectories = None
          InteractiveUser = None
          ExcludedManagedSettings = None
          EnforceManagedModelDefaults = None
          TrustWorkingDirectory = None
          RefreshCustomInstructions = None
          ImageGeneration = None
          Autopilot = None
          OnSubagentStart = None
          OnSubagentStop = None
          Provider = None
          Tools = None
          AvailableTools = None
          Providers = None
          Models = None
          DefaultProvider = None
          CapiEnabled = None
          OnShellAuthenticationRequest = None
          OnStoreMemory = None }

// ---------------------------------------------------------------------------
// Resume session configuration
// ---------------------------------------------------------------------------

/// Configuration for resuming an existing session.
type ResumeSessionConfig =
    { /// The session ID to resume.
      SessionId: string
      /// Handler for permission requests.
      OnPermissionRequest: PermissionRequestHandler
      /// Model override for the resumed session.
      Model: string option
      /// Directories to search for instruction files.
      InstructionDirectories: string list option
      // --- v2.6.0 upstream-sync session options ---
      /// Auto-routing tier (see AutoTier; experimental). Sent under "capi".
      AutoTier: string option
      /// Ask-user tool variant (see AskUserVariant).
      AskUserVariant: string option
      /// Per-session GitHub token provider.
      GitHubTokenProvider: GitHubTokenProvider option
      /// Sandbox configuration.
      SandboxConfig: SandboxConfig option
      /// Runtime feature flags (sent even when empty).
      FeatureFlags: IDictionary<string, bool> option
      /// Restrict the models this session may use.
      AllowedModels: string list option
      /// OAuth client ID metadata document URL.
      AuthClientIdMetadataUrl: string option
      /// Directories to load custom agents from.
      AgentDirectories: string list option
      /// Directories to load hook definitions from.
      HookDirectories: string list option
      /// Whether an interactive user is present.
      InteractiveUser: bool option
      /// Managed settings to exclude.
      ExcludedManagedSettings: string list option
      /// Enforce managed model defaults.
      EnforceManagedModelDefaults: bool option
      /// Allow recovering the session from its transcript.
      AllowTranscriptRecovery: bool option
      /// Session-level image generation configuration.
      ImageGeneration: ImageGenerationConfig option
      /// Autopilot continuation options.
      Autopilot: AutopilotConfig option
      /// Hook invoked before a sub-agent starts ("subagentStart").
      OnSubagentStart: SubagentStartHandler option
      /// Hook invoked after a sub-agent stops ("subagentStop").
      OnSubagentStop: SubagentStopHandler option
      /// BYOK model provider configuration.
      Provider: ProviderConfig option
      // --- v3.0.0 (SDK protocol 4) session options ---
      /// Suppress the session.resume event emitted on resume (wire: suppressResumeEvent).
      SuppressResumeEvent: bool option
      /// Tools declared to the runtime (tools.definitions) and registered on the session.
      Tools: RegisteredTool list option
      /// Allow-list of tool names (required for every session in ClientMode.Empty).
      AvailableTools: string list option
      /// Tools to exclude from the session.
      ExcludedTools: string list option
      /// Named BYOK provider connections (inference.providers).
      Providers: NamedProviderConfig list option
      /// Models served by the named providers (inference.models).
      Models: ProviderModelConfig list option
      /// Provider used for unqualified model selections (inference.defaultProvider).
      DefaultProvider: string option
      /// Send inference.defaultProvider = null to clear the stored default provider.
      ClearDefaultProvider: bool option
      /// Whether Copilot API models are available (github.capi.enabled); Some false for BYOK only.
      CapiEnabled: bool option
      /// Answers sudo/ssh/git prompts (sends interaction.requestShellAuthentication).
      OnShellAuthenticationRequest: ShellAuthenticationHandler option
      /// Persists agent memories (sends state.hasMemoryStore).
      OnStoreMemory: StoreMemoryHandler option }

module ResumeSessionConfig =
    /// Resume configuration for sessionId with approveAll and every option unset.
    let create (sessionId: string) : ResumeSessionConfig =
        { SessionId = sessionId
          OnPermissionRequest = SessionConfig.approveAll
          Model = None
          InstructionDirectories = None
          AutoTier = None
          AskUserVariant = None
          GitHubTokenProvider = None
          SandboxConfig = None
          FeatureFlags = None
          AllowedModels = None
          AuthClientIdMetadataUrl = None
          AgentDirectories = None
          HookDirectories = None
          InteractiveUser = None
          ExcludedManagedSettings = None
          EnforceManagedModelDefaults = None
          AllowTranscriptRecovery = None
          ImageGeneration = None
          Autopilot = None
          OnSubagentStart = None
          OnSubagentStop = None
          Provider = None
          SuppressResumeEvent = None
          Tools = None
          AvailableTools = None
          ExcludedTools = None
          Providers = None
          Models = None
          DefaultProvider = None
          ClearDefaultProvider = None
          CapiEnabled = None
          OnShellAuthenticationRequest = None
          OnStoreMemory = None }

// ---------------------------------------------------------------------------
// Tool result
// ---------------------------------------------------------------------------

/// Binary result from a tool (e.g. an image).
type ToolBinaryResult =
    { [<JsonPropertyName("data")>]
      Data: string
      [<JsonPropertyName("mimeType")>]
      MimeType: string
      [<JsonPropertyName("type")>]
      Type: string }

/// Structured result of a tool execution.
type ToolResultObject =
    { [<JsonPropertyName("textResultForLlm")>]
      TextResultForLlm: string
      [<JsonPropertyName("resultType")>]
      ResultType: string
      [<JsonPropertyName("binaryResultsForLlm")>]
      BinaryResultsForLlm: ToolBinaryResult list option
      [<JsonPropertyName("error")>]
      Error: string option }

module ToolResultObject =
    /// Create a successful text result.
    let success text =
        { TextResultForLlm = text
          ResultType = "success"
          BinaryResultsForLlm = None
          Error = None }

    /// Create a failure result with an error message.
    let failure errorMsg =
        { TextResultForLlm = ""
          ResultType = "failure"
          BinaryResultsForLlm = None
          Error = Some errorMsg }

// ---------------------------------------------------------------------------
// Session events (discriminated union)
// ---------------------------------------------------------------------------

/// Data payload for an assistant message event.
type AssistantMessageData =
    { [<JsonPropertyName("content")>]
      Content: string
      [<JsonPropertyName("messageId")>]
      MessageId: string option }

/// Data payload for an assistant message delta event.
type AssistantMessageDeltaData =
    { [<JsonPropertyName("content")>]
      Content: string }

/// Data payload for an assistant reasoning event.
type AssistantReasoningData =
    { [<JsonPropertyName("content")>]
      Content: string }

/// Data payload for a tool execution start event.
type ToolExecutionStartData =
    { [<JsonPropertyName("toolName")>]
      ToolName: string
      [<JsonPropertyName("toolCallId")>]
      ToolCallId: string }

/// Data payload for a tool execution complete event.
type ToolExecutionCompleteData =
    { [<JsonPropertyName("toolName")>]
      ToolName: string
      [<JsonPropertyName("toolCallId")>]
      ToolCallId: string }

/// Data payload for a session compaction event.
type SessionCompactionData =
    { [<JsonPropertyName("reason")>]
      Reason: string option }

/// Data payload for a "tool.shell_output" event: live, append-only shell
/// output that is not persisted or replayed.
type ToolShellOutputData =
    { [<JsonPropertyName("toolCallId")>]
      ToolCallId: string
      /// New output to append.
      [<JsonPropertyName("text")>]
      Text: string
      /// Zero-based publication sequence across all streams of the tool call.
      [<JsonPropertyName("sequence")>]
      Sequence: int64
      /// "stdout" or "stderr"; None means stdout.
      [<JsonPropertyName("stream")>]
      Stream: string option }

/// All session events emitted by the Copilot CLI, represented as a discriminated union.
type SessionEvent =
    | AssistantMessage of AssistantMessageData
    | AssistantMessageDelta of AssistantMessageDeltaData
    | AssistantReasoning of AssistantReasoningData
    | AssistantReasoningDelta of AssistantMessageDeltaData
    | ToolExecutionStart of ToolExecutionStartData
    | ToolExecutionComplete of ToolExecutionCompleteData
    | ToolShellOutput of ToolShellOutputData
    | SessionIdle
    | SessionCompactionStart of SessionCompactionData
    | SessionCompactionComplete of SessionCompactionData
    | UnknownEvent of eventType: string * raw: JsonElement option

module SessionEvent =
    /// Try to extract the assistant message content from an event.
    let tryGetMessageContent = function
        | AssistantMessage data -> Some data.Content
        | _ -> None

    /// Try to extract delta content for streaming.
    let tryGetDeltaContent = function
        | AssistantMessageDelta data -> Some data.Content
        | _ -> None

    /// Check whether this event signals session idle.
    let isIdle = function
        | SessionIdle -> true
        | _ -> false

    /// Parse a wire event type string and raw JSON into a SessionEvent.
    let parse (eventType: string) (raw: JsonElement option) : SessionEvent =
        let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let deserializeAs (r: JsonElement option) : 'T =
            match r with
            | Some elem -> JsonSerializer.Deserialize<'T>(elem.GetRawText(), opts)
            | None -> Unchecked.defaultof<'T>
        match eventType with
        | "assistant.message" -> AssistantMessage (deserializeAs raw)
        | "assistant.message_delta" -> AssistantMessageDelta (deserializeAs raw)
        | "assistant.reasoning" -> AssistantReasoning (deserializeAs raw)
        | "assistant.reasoning_delta" -> AssistantReasoningDelta (deserializeAs raw)
        | "tool.execution_start" -> ToolExecutionStart (deserializeAs raw)
        | "tool.execution_complete" -> ToolExecutionComplete (deserializeAs raw)
        | "tool.shell_output" -> ToolShellOutput (deserializeAs raw)
        | "session.idle" -> SessionIdle
        | "session.compaction_start" -> SessionCompactionStart (deserializeAs raw)
        | "session.compaction_complete" -> SessionCompactionComplete (deserializeAs raw)
        | other -> UnknownEvent (other, raw)

/// Envelope wrapping a session event with metadata.
type SessionEventEnvelope =
    { Id: string
      Timestamp: string
      ParentId: string option
      AgentId: string option
      Ephemeral: bool option
      Event: SessionEvent }

// ---------------------------------------------------------------------------
// Model info
// ---------------------------------------------------------------------------

/// Information about an available model.
type ModelInfo =
    { [<JsonPropertyName("id")>]
      Id: string
      [<JsonPropertyName("name")>]
      Name: string option }

// ---------------------------------------------------------------------------
// Session metadata
// ---------------------------------------------------------------------------

/// Metadata for a persisted session.
type SessionMetadata =
    { [<JsonPropertyName("sessionId")>]
      SessionId: string
      [<JsonPropertyName("summary")>]
      Summary: string option
      [<JsonPropertyName("createdAt")>]
      CreatedAt: string option }

// ---------------------------------------------------------------------------
// Status responses
// ---------------------------------------------------------------------------

/// Response from the server status endpoint.
type GetStatusResponse =
    { [<JsonPropertyName("version")>]
      Version: string option
      [<JsonPropertyName("protocolVersion")>]
      ProtocolVersion: int option }

/// Authentication status response.
type GetAuthStatusResponse =
    { [<JsonPropertyName("authenticated")>]
      Authenticated: bool
      [<JsonPropertyName("user")>]
      User: string option }

// ---------------------------------------------------------------------------
// Wire helpers
// ---------------------------------------------------------------------------

/// Helpers for building JSON-RPC params that omit unset optional keys.
module internal WireJson =
    open System.Text.Json.Nodes

    /// Serialize a value (anonymous record, dictionary, ...) into a JsonObject.
    let toObject (value: obj) : JsonObject =
        match JsonSerializer.SerializeToNode(value) with
        | :? JsonObject as o -> o
        | _ -> JsonObject()

    /// Serialize value into target under key (overwriting any existing key).
    let set (target: JsonObject) (key: string) (value: obj) =
        target.[key] <- JsonSerializer.SerializeToNode(value)

    /// Set key only when the option carries a value.
    let setOpt (target: JsonObject) (key: string) (value: 'T option) =
        match value with
        | Some v -> set target key (box v)
        | None -> ()

// ---------------------------------------------------------------------------
// SDK protocol 4 session payload translation
// ---------------------------------------------------------------------------

/// Translates the flat `session.create` / `session.resume` payload built by
/// the client into the grouped SDK protocol 4 payload. Options keep their
/// flat names on the F# side; only the wire shape changes.
[<RequireQualifiedAccess>]
module SessionWire =
    open System.Text.Json.Nodes

    /// Keys that stay at the top level of the grouped payload.
    let private rootKeys =
        set [ "sessionId"; "model"; "name"; "clientName"; "workingDirectory"; "configDirectory"
              "enableConfigDiscovery"; "systemMessage"; "diagnostics"; "lifecycleDiagnostics"; "cloud"
              "traceparent"; "tracestate"; "suppressResumeEvent"; "continuePendingWork"
              "allowTranscriptRecovery"; "requestedEnvironmentVariables" ]

    /// Feature groups. A flat key with one of these names and an object value
    /// is an already-grouped object whose members win over flat keys.
    let private groupNames =
        set [ "inference"; "tools"; "permissions"; "interaction"; "events"; "agents"; "skills"; "mcp"
              "github"; "filesystem"; "execution"; "instructions"; "hooks"; "git"; "state"; "plugins"
              "canvases"; "extensions"; "experiments"; "policy" ]

    /// Flat key -> (group, member).
    let private groupedKeys : Map<string, string * string> =
        Map.ofList [
            "reasoningEffort", ("inference", "reasoningEffort")
            "reasoningSummary", ("inference", "reasoningSummary")
            "contextTier", ("inference", "contextTier")
            "allowMultipleToolCalls", ("inference", "allowMultipleToolCalls")
            "enableCitations", ("inference", "enableCitations")
            "modelCapabilities", ("inference", "capabilities")
            "verbosity", ("inference", "verbosity")
            "providers", ("inference", "providers")
            "models", ("inference", "models")
            "defaultProvider", ("inference", "defaultProvider")
            "tools", ("tools", "definitions")
            "availableTools", ("tools", "availableTools")
            "excludedTools", ("tools", "excludedTools")
            "toolSearch", ("tools", "search")
            "largeOutput", ("tools", "largeOutput")
            "imageGeneration", ("tools", "imageGeneration")
            "manageScheduleEnabled", ("tools", "manageScheduleEnabled")
            "webSearch", ("tools", "webSearch")
            "shell", ("tools", "shell")
            "maxInlineBinaryBytes", ("tools", "maxInlineBinaryBytes")
            "requestPermission", ("permissions", "request")
            "sandboxConfig", ("permissions", "sandbox")
            "sandbox", ("permissions", "sandbox")
            "permissionMode", ("permissions", "mode")
            "sandboxConfigSource", ("permissions", "sandboxSource")
            "commands", ("interaction", "commands")
            "askUserVariant", ("interaction", "askUserVariant")
            "requestUserInput", ("interaction", "requestUserInput")
            "requestElicitation", ("interaction", "requestElicitation")
            "requestShellAuthentication", ("interaction", "requestShellAuthentication")
            "requestExitPlanMode", ("interaction", "requestExitPlanMode")
            "requestAutoModeSwitch", ("interaction", "requestAutoModeSwitch")
            "requestAutoModeRecovery", ("interaction", "requestAutoModeRecovery")
            "streaming", ("events", "streaming")
            "includeSubAgentStreamingEvents", ("events", "includeSubAgentStreamingEvents")
            "customAgents", ("agents", "definitions")
            "defaultAgent", ("agents", "default")
            "agent", ("agents", "selected")
            "includedBuiltinAgents", ("agents", "includedBuiltinAgents")
            "excludedBuiltinAgents", ("agents", "excludedBuiltinAgents")
            "customAgentsLocalOnly", ("agents", "localDiscoveryOnly")
            "agentDirectories", ("agents", "directories")
            "skillDirectories", ("skills", "directories")
            "enableSkills", ("skills", "enabled")
            "includedBuiltinSkills", ("skills", "includedBuiltinSkills")
            "disabledSkills", ("skills", "disabledSkills")
            "hasSkillProvider", ("skills", "hasProvider")
            "ignoredSkillsLocations", ("skills", "ignoredLocations")
            "mcpServers", ("mcp", "servers")
            "disabledMcpServers", ("mcp", "disabledServers")
            "requestMcpApps", ("mcp", "enableApps")
            "enableMcpApps", ("mcp", "enableApps")
            "mcpOAuthTokenStorage", ("mcp", "oauthTokenStorage")
            "authClientIdMetadataUrl", ("mcp", "authClientIdMetadataUrl")
            "envValueMode", ("mcp", "envValueMode")
            "managedMcpServers", ("mcp", "managedServers")
            "allowAllMcpServerInstructions", ("mcp", "allowAllServerInstructions")
            "gitHubToken", ("github", "token")
            "gitHubTokenProviderRegistrationId", ("github", "tokenProviderRegistrationId")
            "githubMcpToolConfig", ("github", "mcp")
            "capi", ("github", "capi")
            "enableSessionTelemetry", ("github", "telemetryEnabled")
            "enableManagedSettings", ("github", "fetchManagedSettings")
            "interactiveUser", ("github", "interactiveUser")
            "enableGitHubTelemetryForwarding", ("github", "telemetryForwarding")
            "authInfo", ("github", "authInfo")
            "copilotUrl", ("github", "copilotUrl")
            "additionalDirectories", ("filesystem", "additionalDirectories")
            "enableFileChangeTracking", ("filesystem", "trackFileChanges")
            "trustWorkingDirectory", ("filesystem", "trustWorkingDirectory")
            "sessionLimits", ("execution", "limits")
            "autopilot", ("execution", "autopilot")
            "autopilotContinuation", ("execution", "autopilotContinuation")
            "instructionDirectories", ("instructions", "directories")
            "organizationCustomInstructions", ("instructions", "organizationText")
            "enableOnDemandInstructionDiscovery", ("instructions", "onDemandDiscovery")
            "skipCustomInstructions", ("instructions", "skipCustomInstructions")
            "refreshCustomInstructions", ("instructions", "refreshCustomInstructions")
            "hooks", ("hooks", "hasHandlers")
            "hookDirectories", ("hooks", "directories")
            "enableFileHooks", ("hooks", "enableRepositoryHooks")
            "coauthorEnabled", ("git", "coauthorEnabled")
            "enableHostGitOperations", ("git", "enableHostGitOperations")
            "infiniteSessions", ("state", "infiniteSessions")
            "memory", ("state", "memory")
            "hasMemoryStore", ("state", "hasMemoryStore")
            "enableSessionStore", ("state", "enableCrossSessionStore")
            "skipEmbeddingRetrieval", ("state", "skipEmbeddingRetrieval")
            "embeddingCacheStorage", ("state", "embeddingCacheStorage")
            "pluginDirectories", ("plugins", "directories")
            "installedPlugins", ("plugins", "installed")
            "canvases", ("canvases", "definitions")
            "canvasProvider", ("canvases", "provider")
            "requestCanvasRenderer", ("canvases", "requestRenderer")
            "openCanvases", ("canvases", "open")
            "requestExtensions", ("extensions", "enabled")
            "extensionSdkPath", ("extensions", "sdkPath")
            "extensionInfo", ("extensions", "identity")
            "enableExperimentalMode", ("experiments", "enableExperimentalMode")
            "isExperimentalMode", ("experiments", "enableExperimentalMode")
            "featureFlags", ("experiments", "flags")
            "expAssignments", ("experiments", "assignments")
            "featureFlagConfiguration", ("experiments", "configuration")
            "allowedModels", ("policy", "allowedModels")
            "managedSettings", ("policy", "managedSettings")
            "excludedManagedSettings", ("policy", "excludedManagedSettings")
            "enforceManagedModelDefaults", ("policy", "enforceManagedModelDefaults") ]

    /// Members accepted in each group: exactly the members named in the table above.
    let private allowedMembers : Map<string, Set<string>> =
        groupedKeys
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.groupBy fst
        |> Seq.map (fun (groupName, paths) -> groupName, (paths |> Seq.map snd |> Set.ofSeq))
        |> Map.ofSeq

    /// Group members accepted only by session.create (dropped on resume).
    let private createOnlyMembers =
        set [ ("filesystem", "trustWorkingDirectory"); ("instructions", "refreshCustomInstructions") ]

    /// Group members accepted only by session.resume (dropped on create).
    let private resumeOnlyMembers = set [ ("canvases", "open") ]

    /// The runtime rejects unknown group members, so only table members are sent,
    /// honoring the create-only / resume-only members.
    let private memberAllowed (isResume: bool) (groupName: string) (memberName: string) =
        (match Map.tryFind groupName allowedMembers with
         | Some members -> members.Contains memberName
         | None -> false)
        && not (isResume && createOnlyMembers.Contains((groupName, memberName)))
        && not ((not isResume) && resumeOnlyMembers.Contains((groupName, memberName)))

    /// Legacy `provider` fields that describe the model rather than the connection.
    let private providerModelFields =
        set [ "modelId"; "wireModel"; "maxPromptTokens"; "maxContextWindowTokens"; "maxOutputTokens"; "capabilities" ]

    let private cloneNode (node: JsonNode) : JsonNode =
        if isNull node then null else node.DeepClone()

    let private str (value: string) : JsonNode = JsonValue.Create(value) :> JsonNode

    let private flag (value: bool) : JsonNode = JsonValue.Create(value) :> JsonNode

    /// Translate the legacy single BYOK `provider` into a named "default" provider.
    let private translateLegacyProvider
            (provider: JsonObject)
            (result: JsonObject)
            (group: string -> JsonObject) =
        let inference = group "inference"
        let github = group "github"
        let capiEnabledSet =
            match github.["capi"] with
            | :? JsonObject as capi -> capi.ContainsKey "enabled"
            | _ -> false
        // Explicit named providers, default provider, or capi.enabled win over the legacy option.
        if not (inference.ContainsKey "providers" || inference.ContainsKey "defaultProvider" || capiEnabledSet) then
            let connection = JsonObject()
            connection.["name"] <- str "default"
            let modelFields = JsonObject()
            for KeyValue(fieldName, fieldValue) in provider do
                if not (isNull fieldValue) then
                    if providerModelFields.Contains fieldName then
                        modelFields.[fieldName] <- cloneNode fieldValue
                    else
                        connection.[fieldName] <- cloneNode fieldValue
            let providers = JsonArray()
            providers.Add(connection :> JsonNode)
            inference.["providers"] <- providers
            inference.["defaultProvider"] <- str "default"
            let capi =
                match github.["capi"] with
                | :? JsonObject as existing -> existing
                | _ ->
                    let created = JsonObject()
                    github.["capi"] <- created
                    created
            capi.["enabled"] <- flag false
            let sessionModel =
                match result.["model"] with
                | :? JsonValue as value ->
                    match value.TryGetValue<string>() with
                    | true, modelId when not (String.IsNullOrEmpty modelId) -> Some modelId
                    | _ -> None
                | _ -> None
            match sessionModel with
            | Some modelId when modelFields.Count > 0 ->
                let entry = JsonObject()
                entry.["provider"] <- str "default"
                entry.["id"] <- str modelId
                for KeyValue(fieldName, fieldValue) in modelFields do
                    entry.[fieldName] <- cloneNode fieldValue
                let models = JsonArray()
                models.Add(entry :> JsonNode)
                inference.["models"] <- models
            | _ -> ()

    /// Convert a flat `session.create` (isResume = false) or `session.resume`
    /// (isResume = true) payload into the grouped SDK protocol 4 payload.
    /// ROOT keys stay top level, known flat keys move to `group.member`,
    /// unknown keys stay at the root, null values are skipped, and empty
    /// groups are not emitted. Members of already-grouped objects win.
    let groupSessionParams (flat: JsonObject) (isResume: bool) : JsonObject =
        let result = JsonObject()
        let groups = Dictionary<string, JsonObject>()
        let order = ResizeArray<string>()
        let group (name: string) =
            match groups.TryGetValue(name) with
            | true, existing -> existing
            | _ ->
                let created = JsonObject()
                groups.[name] <- created
                order.Add(name)
                created
        let setFlat (groupName: string) (memberName: string) (value: JsonNode) =
            let target = group groupName
            if not (target.ContainsKey memberName) then
                target.[memberName] <- cloneNode value
        let mutable legacyProvider : JsonObject option = None
        for KeyValue(key, value) in flat do
            if not (isNull value) then
                if rootKeys.Contains key then
                    result.[key] <- cloneNode value
                elif key = "configDir" then
                    if not (result.ContainsKey "configDirectory") then
                        result.["configDirectory"] <- cloneNode value
                elif key = "disableResume" then
                    if isResume && not (result.ContainsKey "suppressResumeEvent") then
                        result.["suppressResumeEvent"] <- cloneNode value
                elif key = "provider" then
                    match value with
                    | :? JsonObject as provider -> legacyProvider <- Some provider
                    | _ -> ()
                else
                    match value with
                    | :? JsonObject as explicitGroup when groupNames.Contains key ->
                        let target = group key
                        for KeyValue(memberName, memberValue) in explicitGroup do
                            if memberAllowed isResume key memberName then
                                target.[memberName] <- cloneNode memberValue
                    | _ ->
                        match Map.tryFind key groupedKeys with
                        | Some (groupName, memberName) ->
                            if memberAllowed isResume groupName memberName then
                                setFlat groupName memberName value
                                if key = "mcpServers" then
                                    setFlat "mcp" "envValueMode" (str "direct")
                        | None ->
                            result.[key] <- cloneNode value
        legacyProvider |> Option.iter (fun provider -> translateLegacyProvider provider result group)
        for name in order do
            let members = groups.[name]
            if members.Count > 0 then
                result.[name] <- members
        result

    /// Defaults applied under the caller's values for ClientMode.Empty.
    let private emptyModeDefaults () : (string * string * JsonNode) list =
        let memory = JsonObject()
        memory.["enabled"] <- flag false
        [ "experiments", "enableExperimentalMode", flag false
          "github", "telemetryEnabled", flag false
          "mcp", "oauthTokenStorage", str "in-memory"
          "state", "skipEmbeddingRetrieval", flag true
          "state", "embeddingCacheStorage", str "in-memory"
          "state", "enableCrossSessionStore", flag false
          "state", "memory", (memory :> JsonNode)
          "instructions", "onDemandDiscovery", flag false
          "hooks", "enableRepositoryHooks", flag false
          "git", "enableHostGitOperations", flag false ]

    /// Apply the ClientMode.Empty defaults to a grouped payload. Values the
    /// caller already set (non-null) are kept.
    let applyEmptyModeDefaults (grouped: JsonObject) : JsonObject =
        for (groupName, memberName, value) in emptyModeDefaults () do
            let target =
                match grouped.[groupName] with
                | :? JsonObject as existing -> existing
                | _ ->
                    let created = JsonObject()
                    grouped.[groupName] <- created
                    created
            match target.TryGetPropertyValue(memberName) with
            | true, existing when not (isNull existing) -> ()
            | _ -> target.[memberName] <- value
        grouped

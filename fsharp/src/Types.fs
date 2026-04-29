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
      UpdraftMessage: string option }

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
      SessionIdleTimeoutSeconds: int option }

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
          SessionIdleTimeoutSeconds = None }

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
      Mode: string option }

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
      IncludeSubAgentStreamingEvents: bool }

module SessionConfig =
    /// Approve all permission requests automatically.
    let approveAll : PermissionRequestHandler =
        fun _ -> async { return { Kind = ApproveOnce; UpdraftMessage = None } }

    /// Reject all permission requests.
    let rejectAll : PermissionRequestHandler =
        fun _ -> async { return { Kind = Reject; UpdraftMessage = None } }

    /// Default session config with approveAll handler.
    let defaults =
        { Model = None
          OnPermissionRequest = approveAll
          SystemMessage = None
          Streaming = true
          Agent = None
          SkillDirectories = None
          IncludeSubAgentStreamingEvents = false }

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
      Model: string option }

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
      Parameters: ToolParameterSchema option }

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

/// All session events emitted by the Copilot CLI, represented as a discriminated union.
type SessionEvent =
    | AssistantMessage of AssistantMessageData
    | AssistantMessageDelta of AssistantMessageDeltaData
    | AssistantReasoning of AssistantReasoningData
    | AssistantReasoningDelta of AssistantMessageDeltaData
    | ToolExecutionStart of ToolExecutionStartData
    | ToolExecutionComplete of ToolExecutionCompleteData
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
        | "session.idle" -> SessionIdle
        | "session.compaction_start" -> SessionCompactionStart (deserializeAs raw)
        | "session.compaction_complete" -> SessionCompactionComplete (deserializeAs raw)
        | other -> UnknownEvent (other, raw)

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

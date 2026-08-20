' Copyright (c) Microsoft Corporation. All rights reserved.

Imports System.Text.Json.Serialization

Namespace GitHub.Copilot.SDK

    ' -----------------------------------------------------------------------
    '  Client options
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Configuration options for creating a <see cref="CopilotClient"/> instance.
    ''' </summary>
    Public Class CopilotClientOptions

        ''' <summary>Path to the Copilot CLI executable.</summary>
        Public Property CliPath As String

        ''' <summary>Extra command-line arguments for the CLI process.</summary>
        Public Property CliArgs As String()

        ''' <summary>Working directory for the CLI process.</summary>
        Public Property Cwd As String

        ''' <summary>Port number for the CLI server (0 = random).</summary>
        Public Property Port As Integer = 0

        ''' <summary>Use stdio transport instead of TCP (default: True).</summary>
        Public Property UseStdio As Boolean = True

        ''' <summary>URL of an existing CLI server to connect to.</summary>
        Public Property CliUrl As String

        ''' <summary>Log level for the CLI server.</summary>
        Public Property LogLevel As String = "info"

        ''' <summary>Automatically start the CLI server if not running (default: True).</summary>
        Public Property AutoStart As Boolean = True

        ''' <summary>Environment variables passed to the CLI process.</summary>
        Public Property Environment As Dictionary(Of String, String)

        ''' <summary>GitHub token for authentication.</summary>
        Public Property GitHubToken As String

        ''' <summary>Whether to use the logged-in user for authentication.</summary>
        Public Property UseLoggedInUser As Boolean? = Nothing

        ''' <summary>Enable streaming delta events.</summary>
        Public Property Streaming As Boolean = False

        ''' <summary>Custom session filesystem provider configuration.</summary>
        Public Property SessionFs As SessionFsConfig

        ''' <summary>OpenTelemetry configuration.</summary>
        Public Property Telemetry As TelemetryConfig

        ''' <summary>Session idle timeout in seconds (Nothing = no timeout).</summary>
        Public Property SessionIdleTimeoutSeconds As Integer? = Nothing

        ''' <summary>Override the Copilot home directory.</summary>
        Public Property CopilotHome As String

        ''' <summary>Token for TCP connection authentication.</summary>
        Public Property TcpConnectionToken As String

        ''' <summary>HTTP request handler for intercepting outbound LLM inference requests.</summary>
        Public Property RequestHandler As CopilotRequestHandler

        ''' <summary>BYOK bearer-token provider invoked per session (receives <see cref="ProviderTokenArgs"/>).</summary>
        Public Property BearerTokenProvider As Func(Of ProviderTokenArgs, Task(Of String))

        ''' <summary>Provider invoked per turn to supply the current W3C <see cref="TraceContext"/>.</summary>
        Public Property OnGetTraceContext As TraceContextProvider

    End Class

    ' -----------------------------------------------------------------------
    '  Session configuration
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Configuration for creating a new session.
    ''' </summary>
    Public Class SessionConfig

        ''' <summary>Model to use (e.g. "gpt-4", "gpt-5").</summary>
        Public Property Model As String

        ''' <summary>System message sent before the first user turn.</summary>
        Public Property SystemMessage As String

        ''' <summary>Handler for permission requests from the assistant.</summary>
        Public Property OnPermissionRequest As PermissionHandler

        ''' <summary>Handler for exit-plan-mode requests from the assistant.</summary>
        Public Property OnExitPlanMode As ExitPlanModeHandler

        ''' <summary>Handler for user input requests.</summary>
        Public Property OnUserInput As Func(Of UserInputRequest, Task(Of String))

        ''' <summary>Handler for elicitation requests.</summary>
        Public Property OnElicitation As Func(Of ElicitationRequest, Task(Of ElicitationResponse))

        ''' <summary>Tools available to the assistant in this session.</summary>
        Public Property Tools As List(Of DefineTool)

        ''' <summary>Skills to enable for this session.</summary>
        Public Property Skills As List(Of String)

        ''' <summary>Enable customize mode for editing files.</summary>
        Public Property CustomizeMode As Boolean = False

        ''' <summary>Enable agent mode.</summary>
        Public Property AgentMode As Boolean = False

        ''' <summary>Initial attachments for context.</summary>
        Public Property Attachments As List(Of Attachment)

        ''' <summary>Tools to exclude from the session.</summary>
        Public Property ExcludedTools As List(Of String)

        ''' <summary>Slash commands registered for this session.</summary>
        Public Property Commands As List(Of CommandDefinition)

        ''' <summary>Additional request headers sent with each turn.</summary>
        Public Property RequestHeaders As Dictionary(Of String, String)

        ''' <summary>Override model capabilities.</summary>
        Public Property ModelCapabilities As Dictionary(Of String, Object)

        ''' <summary>Enable automatic config discovery.</summary>
        Public Property EnableConfigDiscovery As Boolean? = Nothing

        ''' <summary>Include sub-agent streaming events.</summary>
        Public Property IncludeSubAgentStreamingEvents As Boolean = False

        ''' <summary>MCP server configurations keyed by server name.</summary>
        Public Property McpServers As Dictionary(Of String, Object)

        ''' <summary>Response format for the session.</summary>
        Public Property ResponseFormat As ResponseFormat? = Nothing

        ''' <summary>Image generation options.</summary>
        Public Property ImageOptions As ImageOptions

        ''' <summary>Directories to search for instruction files.</summary>
        Public Property InstructionDirectories As List(Of String)

        ''' <summary>Enable inline source citations in assistant responses.</summary>
        Public Property EnableCitations As Boolean? = Nothing

        ''' <summary>Built-in agents to exclude from this session.</summary>
        Public Property ExcludedBuiltinAgents As List(Of String)

        ''' <summary>Per-session AI-credit spending limits.</summary>
        Public Property SessionLimits As SessionLimitsConfig

        ''' <summary>Persistent session memory configuration.</summary>
        Public Property Memory As MemoryConfiguration

        ''' <summary>OTLP telemetry protocol ("grpc" or "http/protobuf").</summary>
        Public Property OtlpProtocol As String

        ''' <summary>Stream assistant responses over a WebSocket transport.</summary>
        Public Property EnableWebSocketResponses As Boolean? = Nothing

        ''' <summary>Experiment assignments forwarded to the runtime.</summary>
        Public Property ExpAssignments As Dictionary(Of String, Object)

        ''' <summary>Handler for MCP OAuth host-token requests.</summary>
        Public Property OnMcpAuthRequest As Func(Of McpAuthRequest, Task(Of String))

        ''' <summary>Hook invoked after a tool completes (post-tool-use).</summary>
        Public Property OnPostToolUse As Func(Of ToolCallData, Task)

        ''' <summary>Hook invoked before an MCP tool call (pre-MCP-tool-call).</summary>
        Public Property OnPreMcpToolCall As Func(Of ToolCallData, Task)

        ''' <summary>Emit per-session telemetry events for this session.</summary>
        Public Property EnableSessionTelemetry As Boolean? = Nothing

        ''' <summary>Enable conversational rewind (restore an earlier turn).</summary>
        Public Property RewindEnabled As Boolean? = Nothing

        ''' <summary>Extra workspace directories to expose to the session.</summary>
        Public Property AdditionalDirectories As List(Of String)

        ''' <summary>Names of MCP servers to disable for this session.</summary>
        Public Property DisabledMcpServers As List(Of String)

        ''' <summary>GitHub MCP tool configuration (opaque passthrough).</summary>
        Public Property GithubMcpToolConfig As Dictionary(Of String, Object)

        ''' <summary>Canvas provider configuration (opaque passthrough).</summary>
        Public Property CanvasProvider As Dictionary(Of String, Object)

        ''' <summary>Restrict custom agents to locally-defined ones only.</summary>
        Public Property CustomAgentsLocalOnly As Boolean? = Nothing

        ''' <summary>Permission-reply decision context (opaque passthrough).</summary>
        Public Property DecisionContext As Dictionary(Of String, Object)

        ''' <summary>Hook invoked after a user prompt is transformed (userPromptTransformed).</summary>
        Public Property OnUserPromptTransformed As Func(Of String, Task)

        ''' <summary>Built-in plugin directories to load.</summary>
        Public Property BuiltinPluginDirectories As List(Of String)

        ''' <summary>Agent-factory authoring arguments schema (opaque object).</summary>
        Public Property ArgsSchema As Dictionary(Of String, Object)

        ''' <summary>Reasoning effort control ("low", "medium", "high").</summary>
        Public Property ReasoningEffort As String

        ''' <summary>Tool-search configuration (opaque passthrough).</summary>
        Public Property ToolSearch As Dictionary(Of String, Object)

        ''' <summary>Use the in-process FFI transport instead of a spawned CLI.</summary>
        Public Property InProcess As Boolean? = Nothing

        ''' <summary>Enable experimental mode features.</summary>
        Public Property ExperimentalMode As Boolean? = Nothing

        ''' <summary>Enable content exclusion policy enforcement.</summary>
        Public Property ContentExclusion As Boolean? = Nothing

    End Class

    ''' <summary>
    ''' Configuration for resuming an existing session.
    ''' </summary>
    Public Class ResumeSessionConfig

        ''' <summary>Handler for permission requests from the assistant.</summary>
        Public Property OnPermissionRequest As PermissionHandler

        ''' <summary>Handler for user input requests.</summary>
        Public Property OnUserInput As Func(Of UserInputRequest, Task(Of String))

        ''' <summary>Handler for elicitation requests.</summary>
        Public Property OnElicitation As Func(Of ElicitationRequest, Task(Of ElicitationResponse))

        ''' <summary>Tools available to the assistant in this session.</summary>
        Public Property Tools As List(Of DefineTool)

        ''' <summary>Skills to enable for this session.</summary>
        Public Property Skills As List(Of String)

        ''' <summary>Slash commands registered for this session.</summary>
        Public Property Commands As List(Of CommandDefinition)

        ''' <summary>Tools to exclude from the session.</summary>
        Public Property ExcludedTools As List(Of String)

        ''' <summary>Include sub-agent streaming events.</summary>
        Public Property IncludeSubAgentStreamingEvents As Boolean = False

        ''' <summary>Directories to search for instruction files.</summary>
        Public Property InstructionDirectories As List(Of String)

    End Class

    ' -----------------------------------------------------------------------
    '  Message types
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Options for sending a message to a session.
    ''' </summary>
    Public Class MessageOptions

        ''' <summary>The user prompt text.</summary>
        Public Property Prompt As String

        ''' <summary>File or URI attachments to include with the message.</summary>
        Public Property Attachments As List(Of Attachment)

        ''' <summary>Interaction mode (e.g. "agent", "edit", "chat").</summary>
        Public Property Mode As String

        ''' <summary>Agent-mode override for this message.</summary>
        Public Property AgentMode As String

        ''' <summary>Alternate prompt text shown to the user (the model still sees Prompt).</summary>
        Public Property DisplayPrompt As String

        ''' <summary>Per-message request headers forwarded with the turn.</summary>
        Public Property RequestHeaders As Dictionary(Of String, String)

    End Class

    ''' <summary>
    ''' Represents a file or URI attachment.
    ''' </summary>
    Public Class Attachment

        <JsonPropertyName("type")>
        Public Property Type As String

        <JsonPropertyName("path")>
        Public Property Path As String

        <JsonPropertyName("uri")>
        Public Property Uri As String

        <JsonPropertyName("language")>
        Public Property Language As String

    End Class

    ' -----------------------------------------------------------------------
    '  Responses
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Response returned by <see cref="CopilotClient.PingAsync"/>.
    ''' </summary>
    Public Class PingResponse

        <JsonPropertyName("message")>
        Public Property Message As String

        <JsonPropertyName("serverTime")>
        Public Property ServerTime As String

    End Class

    ''' <summary>
    ''' Metadata for an existing session.
    ''' </summary>
    Public Class SessionMetadata

        <JsonPropertyName("sessionId")>
        Public Property SessionId As String

        <JsonPropertyName("createdAt")>
        Public Property CreatedAt As String

        <JsonPropertyName("lastActiveAt")>
        Public Property LastActiveAt As String

        <JsonPropertyName("model")>
        Public Property Model As String

        <JsonPropertyName("workspacePath")>
        Public Property WorkspacePath As String

    End Class

    ''' <summary>
    ''' Response returned after sending a message.
    ''' </summary>
    Public Class SendMessageResponse

        <JsonPropertyName("messageId")>
        Public Property MessageId As String

    End Class

    ''' <summary>
    ''' Internal request object for sending a message.
    ''' </summary>
    Friend Class SendMessageRequest

        <JsonPropertyName("sessionId")>
        Public Property SessionId As String

        <JsonPropertyName("prompt")>
        Public Property Prompt As String

        <JsonPropertyName("attachments")>
        Public Property Attachments As List(Of Attachment)

        <JsonPropertyName("mode")>
        Public Property Mode As String

        <JsonPropertyName("agentMode")>
        Public Property AgentMode As String

        <JsonPropertyName("displayPrompt")>
        Public Property DisplayPrompt As String

        <JsonPropertyName("requestHeaders")>
        Public Property RequestHeaders As Dictionary(Of String, String)

    End Class

    ' -----------------------------------------------------------------------
    '  Permission handling
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Delegate that handles permission requests from the assistant.
    ''' </summary>
    Public Delegate Function PermissionHandler(request As PermissionRequest) As Task(Of PermissionResponse)

    ''' <summary>
    ''' A permission request from the assistant (e.g. to run a command).
    ''' </summary>
    Public Class PermissionRequest

        <JsonPropertyName("type")>
        Public Property Type As String

        <JsonPropertyName("tool")>
        Public Property Tool As String

        <JsonPropertyName("description")>
        Public Property Description As String

        <JsonPropertyName("input")>
        Public Property Input As Object

    End Class

    ''' <summary>
    ''' Response to a permission request.
    ''' </summary>
    Public Class PermissionResponse

        ''' <summary>Whether the action is allowed.</summary>
        Public Property Allowed As Boolean

        ''' <summary>Optional reason for denial.</summary>
        Public Property Reason As String

    End Class

    ''' <summary>
    ''' Static helpers that return common permission handler delegates.
    ''' </summary>
    Public Module PermissionHandlers

        ''' <summary>Approves every permission request.</summary>
        Public Function ApproveAll() As PermissionHandler
            Return Function(req) Task.FromResult(New PermissionResponse With {.Allowed = True})
        End Function

        ''' <summary>Denies every permission request.</summary>
        Public Function DenyAll() As PermissionHandler
            Return Function(req) Task.FromResult(New PermissionResponse With {.Allowed = False, .Reason = "denied by policy"})
        End Function

    End Module

    ' -----------------------------------------------------------------------
    '  User input / elicitation
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Request from the assistant for user input.
    ''' </summary>
    Public Class UserInputRequest

        <JsonPropertyName("prompt")>
        Public Property Prompt As String

        <JsonPropertyName("placeholder")>
        Public Property Placeholder As String

    End Class

    ''' <summary>
    ''' Request from the assistant for structured elicitation.
    ''' </summary>
    Public Class ElicitationRequest

        <JsonPropertyName("type")>
        Public Property Type As String

        <JsonPropertyName("title")>
        Public Property Title As String

        <JsonPropertyName("message")>
        Public Property Message As String

        <JsonPropertyName("options")>
        Public Property Options As List(Of ElicitationOption)

    End Class

    ''' <summary>
    ''' A single option in an elicitation dialog.
    ''' </summary>
    Public Class ElicitationOption

        <JsonPropertyName("id")>
        Public Property Id As String

        <JsonPropertyName("label")>
        Public Property Label As String

    End Class

    ''' <summary>
    ''' Response to an elicitation request.
    ''' </summary>
    Public Class ElicitationResponse

        <JsonPropertyName("result")>
        Public Property Result As String

    End Class

    ' -----------------------------------------------------------------------
    '  Session filesystem
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Configuration for a custom session filesystem provider.
    ''' </summary>
    Public Class SessionFsConfig

        ''' <summary>Initial working directory for sessions.</summary>
        Public Property InitialCwd As String

        ''' <summary>Path where the runtime stores session-scoped files.</summary>
        Public Property SessionStatePath As String

        ''' <summary>Path conventions ("posix" or "windows").</summary>
        Public Property Conventions As String

    End Class

    ' -----------------------------------------------------------------------
    '  MCP server configuration
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Configuration for an MCP stdio (local) server.
    ''' </summary>
    Public Class MCPStdioServerConfig

        Public Property Tools As List(Of String)
        Public Property Type As String
        Public Property Timeout As Integer? = Nothing
        Public Property Command As String
        Public Property Args As List(Of String)
        Public Property Env As Dictionary(Of String, String)
        Public Property Cwd As String

    End Class

    ''' <summary>
    ''' Configuration for an MCP HTTP (remote) server.
    ''' </summary>
    Public Class MCPHttpServerConfig

        Public Property Tools As List(Of String)
        Public Property Type As String
        Public Property Timeout As Integer? = Nothing
        Public Property Url As String
        Public Property Headers As Dictionary(Of String, String)

    End Class

    ' -----------------------------------------------------------------------
    '  Commands
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Definition of a slash command.
    ''' </summary>
    Public Class CommandDefinition

        ''' <summary>Command name (e.g. "/help").</summary>
        Public Property Name As String

        ''' <summary>Human-readable description.</summary>
        Public Property Description As String

        ''' <summary>Handler function for the command.</summary>
        Public Property Handler As Func(Of String, Task(Of String))

    End Class

    ' -----------------------------------------------------------------------
    '  Response format / image options
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Response format for messages.
    ''' </summary>
    Public Enum ResponseFormat
        Text
        Image
        JsonObject
    End Enum

    ''' <summary>
    ''' Image generation options.
    ''' </summary>
    Public Class ImageOptions

        Public Property Size As String
        Public Property Quality As String
        Public Property Style As String

    End Class

    ' -----------------------------------------------------------------------
    '  Telemetry
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' OpenTelemetry configuration for the CLI server.
    ''' </summary>
    Public Class TelemetryConfig

        ''' <summary>OTLP exporter endpoint URL.</summary>
        Public Property OtlpEndpoint As String

        ''' <summary>File path for the file exporter.</summary>
        Public Property FilePath As String

        ''' <summary>Exporter type ("otlp-http" or "file").</summary>
        Public Property ExporterType As String

        ''' <summary>Source name for telemetry spans.</summary>
        Public Property SourceName As String

        ''' <summary>Whether to capture message content.</summary>
        Public Property CaptureContent As Boolean? = Nothing

    End Class

    ' -----------------------------------------------------------------------
    '  Event types
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Base class for all session events.
    ''' </summary>
    Public Class SessionEvent

        <JsonPropertyName("type")>
        Public Property Type As String

        <JsonPropertyName("id")>
        Public Property Id As String

        <JsonPropertyName("timestamp")>
        Public Property Timestamp As String

        <JsonPropertyName("parentId")>
        Public Property ParentId As String

        <JsonPropertyName("agentId")>
        Public Property AgentId As String

        <JsonPropertyName("ephemeral")>
        Public Property Ephemeral As Boolean?

        <JsonPropertyName("sessionId")>
        Public Property SessionId As String

    End Class

    ''' <summary>
    ''' Event emitted when the assistant sends a complete message.
    ''' </summary>
    Public Class AssistantMessageEvent
        Inherits SessionEvent

        <JsonPropertyName("data")>
        Public Property Data As AssistantMessageData

    End Class

    ''' <summary>
    ''' Payload of an <see cref="AssistantMessageEvent"/>.
    ''' </summary>
    Public Class AssistantMessageData

        <JsonPropertyName("content")>
        Public Property Content As String

        <JsonPropertyName("role")>
        Public Property Role As String

    End Class

    ''' <summary>
    ''' Streaming delta event for partial assistant messages.
    ''' </summary>
    Public Class AssistantMessageDeltaEvent
        Inherits SessionEvent

        <JsonPropertyName("data")>
        Public Property Data As AssistantMessageDeltaData

    End Class

    ''' <summary>
    ''' Payload of an <see cref="AssistantMessageDeltaEvent"/>.
    ''' </summary>
    Public Class AssistantMessageDeltaData

        <JsonPropertyName("delta")>
        Public Property Delta As String

    End Class

    ''' <summary>
    ''' Event emitted when the session becomes idle (no pending work).
    ''' </summary>
    Public Class SessionIdleEvent
        Inherits SessionEvent
    End Class

    ''' <summary>
    ''' Event emitted when a session error occurs.
    ''' </summary>
    Public Class SessionErrorEvent
        Inherits SessionEvent

        <JsonPropertyName("data")>
        Public Property Data As SessionErrorData

    End Class

    ''' <summary>
    ''' Payload of a <see cref="SessionErrorEvent"/>.
    ''' </summary>
    Public Class SessionErrorData

        <JsonPropertyName("message")>
        Public Property Message As String

        <JsonPropertyName("code")>
        Public Property Code As String

    End Class

    ''' <summary>
    ''' Event emitted when a tool is being called.
    ''' </summary>
    Public Class ToolCallEvent
        Inherits SessionEvent

        <JsonPropertyName("data")>
        Public Property Data As ToolCallData

    End Class

    ''' <summary>
    ''' Payload of a <see cref="ToolCallEvent"/>.
    ''' </summary>
    Public Class ToolCallData

        <JsonPropertyName("toolName")>
        Public Property ToolName As String

        <JsonPropertyName("toolCallId")>
        Public Property ToolCallId As String

        <JsonPropertyName("input")>
        Public Property Input As Object

    End Class

    ''' <summary>
    ''' Event emitted when reasoning content is available.
    ''' </summary>
    Public Class AssistantReasoningEvent
        Inherits SessionEvent

        <JsonPropertyName("data")>
        Public Property Data As AssistantReasoningData

    End Class

    ''' <summary>
    ''' Payload of an <see cref="AssistantReasoningEvent"/>.
    ''' </summary>
    Public Class AssistantReasoningData

        <JsonPropertyName("content")>
        Public Property Content As String

    End Class

    ''' <summary>
    ''' Event emitted when session compaction starts.
    ''' </summary>
    Public Class SessionCompactionStartEvent
        Inherits SessionEvent
    End Class

    ''' <summary>
    ''' Event emitted when session compaction completes.
    ''' </summary>
    Public Class SessionCompactionCompleteEvent
        Inherits SessionEvent
    End Class

    ''' <summary>
    ''' Model information returned by ListModels.
    ''' </summary>
    Public Class ModelInfo

        <JsonPropertyName("id")>
        Public Property Id As String

        <JsonPropertyName("name")>
        Public Property Name As String

        <JsonPropertyName("vendor")>
        Public Property Vendor As String

    End Class

    ''' <summary>
    ''' Session capabilities reported by the host.
    ''' </summary>
    Public Class SessionCapabilities

        <JsonPropertyName("streaming")>
        Public Property Streaming As Boolean

        <JsonPropertyName("tools")>
        Public Property Tools As Boolean

        <JsonPropertyName("skills")>
        Public Property Skills As Boolean

        <JsonPropertyName("agentMode")>
        Public Property AgentMode As Boolean

        <JsonPropertyName("customizeMode")>
        Public Property CustomizeMode As Boolean

    End Class

    ''' <summary>
    ''' Internal session creation response.
    ''' </summary>
    Friend Class SessionCreateResponse

        <JsonPropertyName("sessionId")>
        Public Property SessionId As String

        <JsonPropertyName("workspacePath")>
        Public Property WorkspacePath As String

        <JsonPropertyName("capabilities")>
        Public Property Capabilities As SessionCapabilities

    End Class

    ' -----------------------------------------------------------------------
    '  Upstream-sync feature types (parity with @github/copilot-sdk)
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Per-session AI-credit budget; set <see cref="MaxAiCredits"/> to cap spend.
    ''' </summary>
    Public Class SessionLimitsConfig

        ''' <summary>Maximum AI credits the session may consume.</summary>
        Public Property MaxAiCredits As Integer? = Nothing

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If MaxAiCredits.HasValue Then d("maxAiCredits") = MaxAiCredits.Value
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Opt-in persistent session memory.
    ''' </summary>
    Public Class MemoryConfiguration

        ''' <summary>Whether persistent memory is enabled for the session.</summary>
        Public Property Enabled As Boolean

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Return New Dictionary(Of String, Object) From {{"enabled", Enabled}}
        End Function

    End Class

    ''' <summary>
    ''' Arguments passed to a BYOK bearer-token provider (per-session scoping).
    ''' </summary>
    Public Class ProviderTokenArgs

        ''' <summary>The session the token is being requested for.</summary>
        Public Property SessionId As String

    End Class

    ''' <summary>
    ''' Arguments passed to an MCP OAuth host-token handler.
    ''' </summary>
    Public Class McpAuthRequest

        <JsonPropertyName("sessionId")>
        Public Property SessionId As String

        <JsonPropertyName("serverName")>
        Public Property ServerName As String

    End Class

    ''' <summary>
    ''' Intercepts outbound LLM inference HTTP/WebSocket requests. Assign an
    ''' instance to <see cref="CopilotClientOptions.RequestHandler"/> and override
    ''' <see cref="SendRequest"/> to mutate, replace, or forward the request.
    ''' BYOK providers may also set <see cref="CopilotClientOptions.BearerTokenProvider"/>.
    ''' </summary>
    Public Class CopilotRequestHandler

        ''' <summary>Transform an outbound request; override in a derived type.</summary>
        Public Overridable Function SendRequest(request As Object, context As Object) As Object
            Return request
        End Function

    End Class

    ''' <summary>
    ''' Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto").
    ''' </summary>
    Public Module ToolDefer
        Public Const Auto As String = "auto"
        Public Const Never As String = "never"
    End Module

    ''' <summary>
    ''' System-message section identifiers (used with system-message overrides).
    ''' The "preamble" section targets the identity preamble; the "preserve" action
    ''' protects an individually-addressable section from a group-level remove.
    ''' </summary>
    Public Module SystemMessageSection
        Public Const Preamble As String = "preamble"
        Public Const Identity As String = "identity"
        Public Const ToolInstructions As String = "tool_instructions"
        Public Const Preserve As String = "preserve"
    End Module

    ''' <summary>
    ''' GitHub-anchored attachment variants.
    ''' </summary>
    Public Module GitHubAttachment
        Public Const GitHubCommit As String = "GitHubCommit"
        Public Const GitHubRelease As String = "GitHubRelease"
        Public Const GitHubActionsJob As String = "GitHubActionsJob"
        Public Const GitHubRepository As String = "GitHubRepository"
        Public Const GitHubFileDiff As String = "GitHubFileDiff"
        Public Const GitHubTreeComparison As String = "GitHubTreeComparison"
        Public Const GitHubUrl As String = "GitHubUrl"
        Public Const GitHubFile As String = "GitHubFile"
        Public Const GitHubSnippet As String = "GitHubSnippet"
    End Module

    ' -----------------------------------------------------------------------
    '  Exit-plan-mode and W3C Trace Context
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Request to exit plan mode and continue with a selected action.
    ''' </summary>
    Public Class ExitPlanModeRequest

        ''' <summary>Summary of the plan or proposed next step.</summary>
        <JsonPropertyName("summary")>
        Public Property Summary As String

        ''' <summary>Full plan content, when available.</summary>
        <JsonPropertyName("planContent")>
        Public Property PlanContent As String

        ''' <summary>Available actions the user can select.</summary>
        <JsonPropertyName("actions")>
        Public Property Actions As List(Of String)

        ''' <summary>The action recommended by the runtime.</summary>
        <JsonPropertyName("recommendedAction")>
        Public Property RecommendedAction As String = "autopilot"

    End Class

    ''' <summary>
    ''' Response to an exit-plan-mode request.
    ''' </summary>
    Public Class ExitPlanModeResult

        ''' <summary>Whether the user approved exiting plan mode.</summary>
        <JsonPropertyName("approved")>
        Public Property Approved As Boolean = True

        ''' <summary>Selected action, if the user chose one.</summary>
        <JsonPropertyName("selectedAction")>
        Public Property SelectedAction As String

        ''' <summary>Optional feedback provided by the user.</summary>
        <JsonPropertyName("feedback")>
        Public Property Feedback As String

    End Class

    ''' <summary>
    ''' W3C Trace Context propagated to the runtime for distributed tracing of an agent turn.
    ''' </summary>
    Public Class TraceContext

        ''' <summary>W3C Trace Context traceparent header for this agent turn.</summary>
        <JsonPropertyName("traceparent")>
        Public Property Traceparent As String

        ''' <summary>W3C Trace Context tracestate header for distributed tracing.</summary>
        <JsonPropertyName("tracestate")>
        Public Property Tracestate As String

    End Class

    ''' <summary>
    ''' Delegate that handles exit-plan-mode requests from the assistant.
    ''' </summary>
    Public Delegate Function ExitPlanModeHandler(request As ExitPlanModeRequest) As Task(Of ExitPlanModeResult)

    ''' <summary>
    ''' Delegate that supplies the current W3C <see cref="TraceContext"/> for a session turn.
    ''' </summary>
    Public Delegate Function TraceContextProvider() As Task(Of TraceContext)

End Namespace

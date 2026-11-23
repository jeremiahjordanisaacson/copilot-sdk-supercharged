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

        ''' <summary>
        ''' Identity of the integrating application, declared to the runtime via the
        ''' <c>connect</c> handshake before the protocol check (omitted when unset or all-empty).
        ''' </summary>
        Public Property ClientInfo As CopilotClientInfo

        ''' <summary>
        ''' Client mode: <see cref="ClientMode.CodingAgent"/> (default when Nothing) or
        ''' <see cref="ClientMode.Empty"/>. "copilot-cli" is accepted as a legacy alias of "coding-agent".
        ''' </summary>
        Public Property Mode As String

        ''' <summary>Host network callbacks (proxy resolution, Negotiate and Basic proxy authentication).</summary>
        Public Property Network As NetworkOptions

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

        ' --- v2.6.0 session options (parity with @github/copilot-sdk) ---

        ''' <summary>Session name (create only).</summary>
        Public Property Name As String

        ''' <summary>
        ''' Auto routing tier ("efficiency", "balance", "intelligence", "fast"; experimental).
        ''' Sent as <c>capi.autoTier</c>.
        ''' </summary>
        Public Property AutoTier As String

        ''' <summary>Ask-user variant ("legacy" or "elicitation").</summary>
        Public Property AskUserVariant As String

        ''' <summary>
        ''' Acquires GitHub tokens on demand (answers <c>gitHubToken.getToken</c> requests).
        ''' </summary>
        Public Property GitHubTokenProvider As Func(Of GitHubTokenProviderArgs, Task(Of GitHubTokenProviderResult))

        ''' <summary>Sandbox configuration for the session.</summary>
        Public Property SandboxConfig As SandboxConfig

        ''' <summary>Runtime feature flags (sent whenever set, even when empty).</summary>
        Public Property FeatureFlags As Dictionary(Of String, Boolean)

        ''' <summary>Model allowlist for the session.</summary>
        Public Property AllowedModels As List(Of String)

        ''' <summary>URL of the OAuth client ID metadata document.</summary>
        Public Property AuthClientIdMetadataUrl As String

        ''' <summary>Directories to load custom agents from.</summary>
        Public Property AgentDirectories As List(Of String)

        ''' <summary>Directories to load hook definitions from.</summary>
        Public Property HookDirectories As List(Of String)

        ''' <summary>Whether an interactive user is present.</summary>
        Public Property InteractiveUser As Boolean? = Nothing

        ''' <summary>Managed settings to exclude for this session.</summary>
        Public Property ExcludedManagedSettings As List(Of String)

        ''' <summary>Enforce managed model defaults.</summary>
        Public Property EnforceManagedModelDefaults As Boolean? = Nothing

        ''' <summary>Working-directory trust scope ("session" or "user"; create only).</summary>
        Public Property TrustWorkingDirectory As String

        ''' <summary>Re-read custom instruction files (create only).</summary>
        Public Property RefreshCustomInstructions As Boolean? = Nothing

        ''' <summary>Session-level image generation configuration.</summary>
        Public Property ImageGeneration As ImageGenerationConfig

        ''' <summary>Autopilot continuation options.</summary>
        Public Property Autopilot As AutopilotOptions

        ''' <summary>BYOK provider configuration.</summary>
        Public Property Provider As ProviderConfig

        ''' <summary>Hook invoked before a sub-agent's first turn (hook type "subagentStart").</summary>
        Public Property OnSubagentStart As Func(Of SubagentStartHookInput, Task(Of SubagentStartHookOutput))

        ''' <summary>Hook invoked after a sub-agent completes a turn (hook type "subagentStop").</summary>
        Public Property OnSubagentStop As Func(Of SubagentStopHookInput, Task(Of SubagentStopHookOutput))

        ' --- v3.0.0 session options (SDK protocol 4) ---

        ''' <summary>Per-session GitHub token (mutually exclusive with <see cref="GitHubTokenProvider"/>).</summary>
        Public Property GitHubToken As String

        ''' <summary>Allow-list of tool names (required for every session in <see cref="ClientMode.Empty"/>).</summary>
        Public Property AvailableTools As List(Of String)

        ''' <summary>Named BYOK provider connections (inference.providers).</summary>
        Public Property Providers As List(Of NamedProviderConfig)

        ''' <summary>Models served by the named providers (inference.models).</summary>
        Public Property Models As List(Of ProviderModelConfig)

        ''' <summary>Provider used for unqualified model selections (inference.defaultProvider).</summary>
        Public Property DefaultProvider As String

        ''' <summary>Whether Copilot API models are available (github.capi.enabled); False for BYOK only.</summary>
        Public Property CapiEnabled As Boolean? = Nothing

        ''' <summary>Answers sudo/ssh/git prompts (sends interaction.requestShellAuthentication).</summary>
        Public Property OnShellAuthenticationRequest As ShellAuthenticationHandler

        ''' <summary>Persists agent memories (sends state.hasMemoryStore).</summary>
        Public Property OnStoreMemory As Func(Of StoredMemory, Task)

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

        ' --- v2.6.0 session options (parity with @github/copilot-sdk) ---

        ''' <summary>Auto routing tier (experimental). Sent as <c>capi.autoTier</c>.</summary>
        Public Property AutoTier As String

        ''' <summary>Stream responses over WebSockets. Sent as <c>capi.enableWebSocketResponses</c>.</summary>
        Public Property EnableWebSocketResponses As Boolean? = Nothing

        ''' <summary>Ask-user variant ("legacy" or "elicitation").</summary>
        Public Property AskUserVariant As String

        ''' <summary>
        ''' Acquires GitHub tokens on demand (answers <c>gitHubToken.getToken</c> requests).
        ''' </summary>
        Public Property GitHubTokenProvider As Func(Of GitHubTokenProviderArgs, Task(Of GitHubTokenProviderResult))

        ''' <summary>Sandbox configuration for the session.</summary>
        Public Property SandboxConfig As SandboxConfig

        ''' <summary>Runtime feature flags (sent whenever set, even when empty).</summary>
        Public Property FeatureFlags As Dictionary(Of String, Boolean)

        ''' <summary>Model allowlist for the session.</summary>
        Public Property AllowedModels As List(Of String)

        ''' <summary>URL of the OAuth client ID metadata document.</summary>
        Public Property AuthClientIdMetadataUrl As String

        ''' <summary>Directories to load custom agents from.</summary>
        Public Property AgentDirectories As List(Of String)

        ''' <summary>Directories to load hook definitions from.</summary>
        Public Property HookDirectories As List(Of String)

        ''' <summary>Whether an interactive user is present.</summary>
        Public Property InteractiveUser As Boolean? = Nothing

        ''' <summary>Managed settings to exclude for this session.</summary>
        Public Property ExcludedManagedSettings As List(Of String)

        ''' <summary>Enforce managed model defaults.</summary>
        Public Property EnforceManagedModelDefaults As Boolean? = Nothing

        ''' <summary>Allow recovering the session from its transcript (resume only).</summary>
        Public Property AllowTranscriptRecovery As Boolean? = Nothing

        ''' <summary>Session-level image generation configuration.</summary>
        Public Property ImageGeneration As ImageGenerationConfig

        ''' <summary>Autopilot continuation options.</summary>
        Public Property Autopilot As AutopilotOptions

        ''' <summary>BYOK provider configuration.</summary>
        Public Property Provider As ProviderConfig

        ''' <summary>Hook invoked before a sub-agent's first turn (hook type "subagentStart").</summary>
        Public Property OnSubagentStart As Func(Of SubagentStartHookInput, Task(Of SubagentStartHookOutput))

        ''' <summary>Hook invoked after a sub-agent completes a turn (hook type "subagentStop").</summary>
        Public Property OnSubagentStop As Func(Of SubagentStopHookInput, Task(Of SubagentStopHookOutput))

        ' --- v3.0.0 session options (SDK protocol 4) ---

        ''' <summary>Model override for the resumed session.</summary>
        Public Property Model As String

        ''' <summary>Suppress the resume event emitted by the runtime (wire: suppressResumeEvent).</summary>
        Public Property SuppressResumeEvent As Boolean? = Nothing

        ''' <summary>Allow-list of tool names (required for every session in <see cref="ClientMode.Empty"/>).</summary>
        Public Property AvailableTools As List(Of String)

        ''' <summary>Named BYOK provider connections (inference.providers).</summary>
        Public Property Providers As List(Of NamedProviderConfig)

        ''' <summary>Models served by the named providers (inference.models).</summary>
        Public Property Models As List(Of ProviderModelConfig)

        ''' <summary>Provider used for unqualified model selections (inference.defaultProvider).</summary>
        Public Property DefaultProvider As String

        ''' <summary>Send <c>inference.defaultProvider = null</c> to clear the provider stored with the session.</summary>
        Public Property ClearDefaultProvider As Boolean = False

        ''' <summary>Whether Copilot API models are available (github.capi.enabled); False for BYOK only.</summary>
        Public Property CapiEnabled As Boolean? = Nothing

        ''' <summary>Answers sudo/ssh/git prompts (sends interaction.requestShellAuthentication).</summary>
        Public Property OnShellAuthenticationRequest As ShellAuthenticationHandler

        ''' <summary>Persists agent memories (sends state.hasMemoryStore).</summary>
        Public Property OnStoreMemory As Func(Of StoredMemory, Task)

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

        ''' <summary>
        ''' Message provenance: "user", "system" or "agent-&lt;id&gt;" (see <see cref="MessageSource"/>).
        ''' Omitted when Nothing.
        ''' </summary>
        Public Property Source As String

        ''' <summary>
        ''' JSON Schema (e.g. a Dictionary(Of String, Object) or JsonElement) requesting structured
        ''' output. Sent as a <c>json_schema</c> <c>responseFormat</c>.
        ''' </summary>
        Public Property ResponseSchema As Object

        ''' <summary>Opaque caller-chosen turn identifier (wire: clientTurnId). Omitted when Nothing.</summary>
        Public Property ClientTurnId As String

        ''' <summary>Returns a shallow copy of these options with <see cref="ResponseSchema"/> replaced.</summary>
        Friend Function WithResponseSchema(schema As Object) As MessageOptions
            Dim copy = DirectCast(MemberwiseClone(), MessageOptions)
            copy.ResponseSchema = schema
            Return copy
        End Function

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

        <JsonPropertyName("source")>
        <JsonIgnore(Condition:=JsonIgnoreCondition.WhenWritingNull)>
        Public Property Source As String

        <JsonPropertyName("responseFormat")>
        <JsonIgnore(Condition:=JsonIgnoreCondition.WhenWritingNull)>
        Public Property ResponseFormat As Object

        <JsonPropertyName("clientTurnId")>
        <JsonIgnore(Condition:=JsonIgnoreCondition.WhenWritingNull)>
        Public Property ClientTurnId As String

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
    ''' Event "tool.shell_output": live, append-only shell output. It is not persisted or
    ''' replayed to late subscribers; chunks need not contain complete lines.
    ''' </summary>
    Public Class ToolShellOutputEvent
        Inherits SessionEvent

        <JsonPropertyName("data")>
        Public Property Data As ToolShellOutputData

    End Class

    ''' <summary>
    ''' Payload of a <see cref="ToolShellOutputEvent"/>.
    ''' </summary>
    Public Class ToolShellOutputData

        ''' <summary>Tool call that owns this output.</summary>
        <JsonPropertyName("toolCallId")>
        Public Property ToolCallId As String

        ''' <summary>New output to append.</summary>
        <JsonPropertyName("text")>
        Public Property Text As String

        ''' <summary>Zero-based publication sequence across all streams of the tool call.</summary>
        <JsonPropertyName("sequence")>
        Public Property Sequence As Long

        ''' <summary>"stdout" or "stderr"; Nothing means stdout.</summary>
        <JsonPropertyName("stream")>
        Public Property Stream As String

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

    ' -----------------------------------------------------------------------
    '  v2.6.0 feature types (parity with @github/copilot-sdk)
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Identity of the application integrating the SDK, sent on the <c>connect</c> handshake.
    ''' Empty fields are omitted; an all-empty identity skips <c>connect</c>.
    ''' </summary>
    Public Class CopilotClientInfo

        ''' <summary>Name of the application using the SDK (wire: editorName).</summary>
        Public Property ApplicationName As String

        ''' <summary>Version of the application (wire: editorVersion).</summary>
        Public Property ApplicationVersion As String

        ''' <summary>Name of an integration within the application (wire: extensionName).</summary>
        Public Property IntegrationName As String

        ''' <summary>Version of the integration (wire: extensionVersion).</summary>
        Public Property IntegrationVersion As String

        ''' <summary>Build the <c>clientInfo</c> wire payload (only non-empty fields).</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If Not String.IsNullOrEmpty(ApplicationName) Then d("editorName") = ApplicationName
            If Not String.IsNullOrEmpty(ApplicationVersion) Then d("editorVersion") = ApplicationVersion
            If Not String.IsNullOrEmpty(IntegrationName) Then d("extensionName") = IntegrationName
            If Not String.IsNullOrEmpty(IntegrationVersion) Then d("extensionVersion") = IntegrationVersion
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Auto routing tier values for <see cref="SessionConfig.AutoTier"/> (experimental).
    ''' </summary>
    Public NotInheritable Class AutoTier
        Public Const Efficiency As String = "efficiency"
        Public Const Balance As String = "balance"
        Public Const Intelligence As String = "intelligence"
        Public Const Fast As String = "fast"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Ask-user variant values for <see cref="SessionConfig.AskUserVariant"/>.
    ''' </summary>
    Public NotInheritable Class AskUserVariant
        Public Const Legacy As String = "legacy"
        Public Const Elicitation As String = "elicitation"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Working-directory trust values for <see cref="SessionConfig.TrustWorkingDirectory"/>.
    ''' </summary>
    Public NotInheritable Class WorkingDirectoryTrust
        Public Const Session As String = "session"
        Public Const User As String = "user"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Message provenance values for <see cref="MessageOptions.Source"/>.
    ''' </summary>
    Public NotInheritable Class MessageSource
        ''' <summary>A message typed by the user.</summary>
        Public Const User As String = "user"

        ''' <summary>Application-generated context.</summary>
        Public Const [System] As String = "system"

        ''' <summary>A message originating from the identified agent ("agent-&lt;id&gt;").</summary>
        Public Shared Function Agent(agentId As String) As String
            If String.IsNullOrEmpty(agentId) Then
                Throw New ArgumentException("An agent id is required.", NameOf(agentId))
            End If
            Return "agent-" & agentId
        End Function

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Arguments passed to a <see cref="SessionConfig.GitHubTokenProvider"/>.
    ''' </summary>
    Public Class GitHubTokenProviderArgs

        ''' <summary>GitHub host the token is requested for.</summary>
        Public Property Host As String

        ''' <summary>Session receiving the token (may be Nothing before a session id is assigned).</summary>
        Public Property SessionId As String

        ''' <summary>Why the token is requested: "initial" or "refresh".</summary>
        Public Property Reason As String

    End Class

    ''' <summary>
    ''' Result returned by a GitHub token provider: a token or a cancellation.
    ''' </summary>
    Public Class GitHubTokenProviderResult

        ''' <summary>"token" or "cancelled".</summary>
        Public Property Kind As String = "token"

        ''' <summary>GitHub access token.</summary>
        Public Property AccessToken As String

        ''' <summary>OAuth token type (defaults to bearer when omitted).</summary>
        Public Property TokenType As String

        ''' <summary>Remaining token lifetime in seconds.</summary>
        Public Property ExpiresIn As Integer? = Nothing

        ''' <summary>Creates a token result.</summary>
        Public Shared Function Token(accessToken As String, expiresIn As Integer, Optional tokenType As String = Nothing) As GitHubTokenProviderResult
            Return New GitHubTokenProviderResult With {
                .Kind = "token",
                .AccessToken = accessToken,
                .ExpiresIn = expiresIn,
                .TokenType = tokenType
            }
        End Function

        ''' <summary>Creates a cancellation result.</summary>
        Public Shared Function Cancelled() As GitHubTokenProviderResult
            Return New GitHubTokenProviderResult With {.Kind = "cancelled"}
        End Function

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object) From {{"kind", Kind}}
            If Kind <> "cancelled" Then
                d("accessToken") = AccessToken
                If Not String.IsNullOrEmpty(TokenType) Then d("tokenType") = TokenType
                If ExpiresIn.HasValue Then d("expiresIn") = ExpiresIn.Value
            End If
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Sandbox configuration for a session.
    ''' </summary>
    Public Class SandboxConfig

        ''' <summary>Whether sandboxing is enabled for the session.</summary>
        Public Property Enabled As Boolean

        ''' <summary>User-managed policy fragment merged into the base policy (opaque object).</summary>
        Public Property UserPolicy As Dictionary(Of String, Object)

        ''' <summary>Auto-add the current working directory to the read/write paths.</summary>
        Public Property AddCurrentWorkingDirectory As Boolean? = Nothing

        ''' <summary>Confine MCP servers launched by the session.</summary>
        Public Property SandboxMcpServers As Boolean? = Nothing

        ''' <summary>Confine language servers launched by the session.</summary>
        Public Property SandboxLspServers As Boolean? = Nothing

        ''' <summary>Confine extension processes launched by the session.</summary>
        Public Property SandboxExtensions As Boolean? = Nothing

        ''' <summary>
        ''' Opt in to letting the agent request that an individual command run outside the
        ''' sandbox, approved or denied through the normal permission flow.
        ''' </summary>
        Public Property AllowBypass As Boolean? = Nothing

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object) From {{"enabled", Enabled}}
            If UserPolicy IsNot Nothing Then d("userPolicy") = UserPolicy
            If AddCurrentWorkingDirectory.HasValue Then d("addCurrentWorkingDirectory") = AddCurrentWorkingDirectory.Value
            If SandboxMcpServers.HasValue Then d("sandboxMcpServers") = SandboxMcpServers.Value
            If SandboxLspServers.HasValue Then d("sandboxLspServers") = SandboxLspServers.Value
            If SandboxExtensions.HasValue Then d("sandboxExtensions") = SandboxExtensions.Value
            If AllowBypass.HasValue Then d("allowBypass") = AllowBypass.Value
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Session-level image generation configuration.
    ''' </summary>
    Public Class ImageGenerationConfig

        ''' <summary>Opt in to image generation.</summary>
        Public Property Enabled As Boolean? = Nothing

        ''' <summary>Request a visible "Made with AI" label on generated images.</summary>
        Public Property Watermark As Boolean? = Nothing

        ''' <summary>Save generated images under the session-state directory.</summary>
        Public Property SaveToDisk As Boolean? = Nothing

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If Enabled.HasValue Then d("enabled") = Enabled.Value
            If Watermark.HasValue Then d("watermark") = Watermark.Value
            If SaveToDisk.HasValue Then d("saveToDisk") = SaveToDisk.Value
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Autopilot continuation options.
    ''' </summary>
    Public Class AutopilotOptions

        ''' <summary>Enable runtime-owned automatic continuation in Autopilot mode.</summary>
        Public Property ContinuationEnabled As Boolean? = Nothing

        ''' <summary>Maximum automatic continuations (zero allows none).</summary>
        Public Property MaxContinues As Integer? = Nothing

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If ContinuationEnabled.HasValue Then d("continuationEnabled") = ContinuationEnabled.Value
            If MaxContinues.HasValue Then d("maxContinues") = MaxContinues.Value
            Return d
        End Function

    End Class

    ''' <summary>
    ''' BYOK (bring your own key) provider configuration.
    ''' </summary>
    Public Class ProviderConfig

        ''' <summary>Provider type ("openai", "azure" or "anthropic").</summary>
        Public Property Type As String

        ''' <summary>API format ("completions" or "responses").</summary>
        Public Property WireApi As String

        ''' <summary>API endpoint URL.</summary>
        Public Property BaseUrl As String

        ''' <summary>Static API key.</summary>
        Public Property ApiKey As String

        ''' <summary>Bearer token sent as the Authorization header.</summary>
        Public Property BearerToken As String

        ''' <summary>
        ''' Product serving the model: "openai", "anthropic", "azure_openai", "ollama",
        ''' "lm_studio", "foundry_local" or "llama_cpp".
        ''' </summary>
        Public Property ModelProvider As String

        ''' <summary>Command whose stdout is used as the API key (run before each request).</summary>
        Public Property ApiKeyCommand As String

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If Not String.IsNullOrEmpty(Type) Then d("type") = Type
            If Not String.IsNullOrEmpty(WireApi) Then d("wireApi") = WireApi
            If Not String.IsNullOrEmpty(BaseUrl) Then d("baseUrl") = BaseUrl
            If Not String.IsNullOrEmpty(ApiKey) Then d("apiKey") = ApiKey
            If Not String.IsNullOrEmpty(BearerToken) Then d("bearerToken") = BearerToken
            If Not String.IsNullOrEmpty(ModelProvider) Then d("modelProvider") = ModelProvider
            If Not String.IsNullOrEmpty(ApiKeyCommand) Then d("apiKeyCommand") = ApiKeyCommand
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Token limits for a model capabilities override. Place <see cref="ToWire"/> under the
    ''' "limits" key of <see cref="SessionConfig.ModelCapabilities"/>. Wire keys are snake_case.
    ''' </summary>
    Public Class ModelCapabilitiesLimits

        ''' <summary>Maximum prompt/input tokens (wire: max_prompt_tokens).</summary>
        Public Property MaxPromptTokens As Integer? = Nothing

        ''' <summary>Maximum output/completion tokens (wire: max_output_tokens).</summary>
        Public Property MaxOutputTokens As Integer? = Nothing

        ''' <summary>Maximum context window tokens (wire: max_context_window_tokens).</summary>
        Public Property MaxContextWindowTokens As Integer? = Nothing

        ''' <summary>Build the snake_case JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If MaxPromptTokens.HasValue Then d("max_prompt_tokens") = MaxPromptTokens.Value
            If MaxOutputTokens.HasValue Then d("max_output_tokens") = MaxOutputTokens.Value
            If MaxContextWindowTokens.HasValue Then d("max_context_window_tokens") = MaxContextWindowTokens.Value
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Input for the sub-agent start hook (fired before a sub-agent's first turn).
    ''' </summary>
    Public Class SubagentStartHookInput

        ''' <summary>Runtime session id (the parent session for sub-agent hooks).</summary>
        <JsonPropertyName("sessionId")>
        Public Property SessionId As String

        ''' <summary>Unix epoch milliseconds at which the hook fired.</summary>
        <JsonPropertyName("timestamp")>
        Public Property Timestamp As Long? = Nothing

        ''' <summary>Working directory of the hook (wire: cwd).</summary>
        <JsonPropertyName("cwd")>
        Public Property WorkingDirectory As String

        <JsonPropertyName("transcriptPath")>
        Public Property TranscriptPath As String

        <JsonPropertyName("agentName")>
        Public Property AgentName As String

        <JsonPropertyName("agentDisplayName")>
        Public Property AgentDisplayName As String

        <JsonPropertyName("agentDescription")>
        Public Property AgentDescription As String

    End Class

    ''' <summary>
    ''' Output for the sub-agent start hook.
    ''' </summary>
    Public Class SubagentStartHookOutput

        ''' <summary>Context prepended to the sub-agent's initial prompt.</summary>
        Public Property AdditionalContext As String

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If AdditionalContext IsNot Nothing Then d("additionalContext") = AdditionalContext
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Input for the sub-agent stop hook (fired after a sub-agent completes a turn).
    ''' </summary>
    Public Class SubagentStopHookInput
        Inherits SubagentStartHookInput

        <JsonPropertyName("agentId")>
        Public Property AgentId As String

        <JsonPropertyName("agentType")>
        Public Property AgentType As String

        ''' <summary>Why the sub-agent stopped ("end_turn").</summary>
        <JsonPropertyName("stopReason")>
        Public Property StopReason As String

        ''' <summary>The sub-agent's last assistant message.</summary>
        <JsonPropertyName("response")>
        Public Property Response As String

    End Class

    ''' <summary>
    ''' Output for the sub-agent stop hook. "block" with a reason continues the sub-agent;
    ''' otherwise <see cref="ModifiedResponse"/> replaces the response reported to the parent.
    ''' </summary>
    Public Class SubagentStopHookOutput

        ''' <summary>"block" or "allow".</summary>
        Public Property Decision As String

        ''' <summary>Instruction required when the stop is blocked.</summary>
        Public Property Reason As String

        ''' <summary>Replacement for the sub-agent's final response.</summary>
        Public Property ModifiedResponse As String

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If Decision IsNot Nothing Then d("decision") = Decision
            If Reason IsNot Nothing Then d("reason") = Reason
            If ModifiedResponse IsNot Nothing Then d("modifiedResponse") = ModifiedResponse
            Return d
        End Function

    End Class

    ' -----------------------------------------------------------------------
    '  v3.0.0 feature types (SDK protocol 4)
    ' -----------------------------------------------------------------------

    ''' <summary>
    ''' Client modes for <see cref="CopilotClientOptions.Mode"/>.
    ''' </summary>
    Public NotInheritable Class ClientMode
        ''' <summary>Default mode, identical to the behavior before modes existed.</summary>
        Public Const CodingAgent As String = "coding-agent"

        ''' <summary>
        ''' Every session must list AvailableTools, and locked-down defaults are applied under the
        ''' caller's values (no experimental mode, telemetry, persistent memory, cross-session store,
        ''' on-demand instructions, repository hooks, or host git operations).
        ''' </summary>
        Public Const Empty As String = "empty"

        ''' <summary>Legacy alias of <see cref="CodingAgent"/>.</summary>
        Public Const CopilotCli As String = "copilot-cli"

        ''' <summary>Normalizes a mode to "coding-agent" or "empty"; throws for any other value.</summary>
        Public Shared Function Normalize(mode As String) As String
            If String.IsNullOrEmpty(mode) OrElse mode = CodingAgent OrElse mode = CopilotCli Then Return CodingAgent
            If mode = Empty Then Return Empty
            Throw New ArgumentException($"Mode must be '{CodingAgent}' or '{Empty}', got '{mode}'.", NameOf(mode))
        End Function

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' A named BYOK provider connection (inference.providers).
    ''' </summary>
    Public Class NamedProviderConfig

        ''' <summary>Connection name referenced by <see cref="ProviderModelConfig.Provider"/> and DefaultProvider.</summary>
        Public Property Name As String

        ''' <summary>Provider type ("openai", "azure" or "anthropic").</summary>
        Public Property Type As String

        ''' <summary>API endpoint URL.</summary>
        Public Property BaseUrl As String

        ''' <summary>Static API key.</summary>
        Public Property ApiKey As String

        ''' <summary>Command whose stdout is used as the API key (run before each request).</summary>
        Public Property ApiKeyCommand As String

        ''' <summary>Bearer token sent as the Authorization header.</summary>
        Public Property BearerToken As String

        ''' <summary>API format ("completions" or "responses").</summary>
        Public Property WireApi As String

        ''' <summary>Transport ("http" or "websocket").</summary>
        Public Property Transport As String

        ''' <summary>Azure API version (Azure providers only).</summary>
        Public Property AzureApiVersion As String

        ''' <summary>Extra HTTP headers sent with each request.</summary>
        Public Property Headers As Dictionary(Of String, String)

        ''' <summary>Product serving the model (for example "ollama").</summary>
        Public Property ModelProvider As String

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object) From {{"name", Name}}
            If Not String.IsNullOrEmpty(Type) Then d("type") = Type
            If Not String.IsNullOrEmpty(BaseUrl) Then d("baseUrl") = BaseUrl
            If Not String.IsNullOrEmpty(ApiKey) Then d("apiKey") = ApiKey
            If Not String.IsNullOrEmpty(ApiKeyCommand) Then d("apiKeyCommand") = ApiKeyCommand
            If Not String.IsNullOrEmpty(BearerToken) Then d("bearerToken") = BearerToken
            If Not String.IsNullOrEmpty(WireApi) Then d("wireApi") = WireApi
            If Not String.IsNullOrEmpty(Transport) Then d("transport") = Transport
            If Not String.IsNullOrEmpty(AzureApiVersion) Then
                d("azure") = New Dictionary(Of String, Object) From {{"apiVersion", AzureApiVersion}}
            End If
            If Headers IsNot Nothing Then d("headers") = Headers
            If Not String.IsNullOrEmpty(ModelProvider) Then d("modelProvider") = ModelProvider
            Return d
        End Function

    End Class

    ''' <summary>
    ''' A model served by a named BYOK provider (inference.models). Select it with the session
    ''' model "provider/id".
    ''' </summary>
    Public Class ProviderModelConfig

        ''' <summary>Name of the <see cref="NamedProviderConfig"/> serving this model.</summary>
        Public Property Provider As String

        ''' <summary>Model id used in "provider/id" selections.</summary>
        Public Property Id As String

        ''' <summary>Model id sent to the provider, when different from <see cref="Id"/>.</summary>
        Public Property ModelId As String

        ''' <summary>Model name sent on the wire, when different from <see cref="ModelId"/>.</summary>
        Public Property WireModel As String

        ''' <summary>Display name.</summary>
        Public Property Name As String

        Public Property MaxPromptTokens As Integer? = Nothing

        Public Property MaxContextWindowTokens As Integer? = Nothing

        Public Property MaxOutputTokens As Integer? = Nothing

        ''' <summary>Model capabilities override.</summary>
        Public Property Capabilities As Dictionary(Of String, Object)

        ''' <summary>Build the camelCase JSON-RPC wire payload.</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object) From {{"provider", Provider}, {"id", Id}}
            If Not String.IsNullOrEmpty(ModelId) Then d("modelId") = ModelId
            If Not String.IsNullOrEmpty(WireModel) Then d("wireModel") = WireModel
            If Not String.IsNullOrEmpty(Name) Then d("name") = Name
            If MaxPromptTokens.HasValue Then d("maxPromptTokens") = MaxPromptTokens.Value
            If MaxContextWindowTokens.HasValue Then d("maxContextWindowTokens") = MaxContextWindowTokens.Value
            If MaxOutputTokens.HasValue Then d("maxOutputTokens") = MaxOutputTokens.Value
            If Capabilities IsNot Nothing Then d("capabilities") = Capabilities
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Traffic served by the network callbacks (<see cref="NetworkOptions.Scope"/>).
    ''' </summary>
    Public NotInheritable Class NetworkScope
        ''' <summary>Only requests made for this connection and its sessions (default).</summary>
        Public Const Connection As String = "connection"

        ''' <summary>Also the runtime's shared background traffic.</summary>
        Public Const Runtime As String = "runtime"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Payload of a <c>network.authenticateBasic</c> request.
    ''' </summary>
    Public Class NetworkBasicAuthRequest

        ''' <summary>Proxy that challenged, as "http://host:port" or "https://host:port".</summary>
        Public Property Proxy As String

        ''' <summary>Realm named by the proxy (untrusted text), when any.</summary>
        Public Property Realm As String

        ''' <summary>True when the proxy rejected the credentials sent last.</summary>
        Public Property PreviousRejected As Boolean

    End Class

    ''' <summary>
    ''' Proxy Basic credentials. Never log or persist them.
    ''' </summary>
    Public Class NetworkBasicCredentials

        Public Property Username As String

        Public Property Password As String

    End Class

    ''' <summary>
    ''' Host-provided network callbacks. When any callback is set, the client registers them with
    ''' <c>network.setProvider</c> after connecting.
    ''' </summary>
    Public Class NetworkOptions

        ''' <summary>"connection" (default) or "runtime" (see <see cref="NetworkScope"/>).</summary>
        Public Property Scope As String

        ''' <summary>Returns a PAC-style proxy list for a URL (Nothing leaves it to the runtime).</summary>
        Public Property ResolveProxy As Func(Of String, Task(Of String))

        ''' <summary>Returns a base64 SPNEGO token for a service principal name (Nothing when unavailable).</summary>
        Public Property AuthenticateNegotiate As Func(Of String, Task(Of String))

        ''' <summary>Returns proxy Basic credentials (Nothing when unavailable).</summary>
        Public Property AuthenticateBasic As Func(Of NetworkBasicAuthRequest, Task(Of NetworkBasicCredentials))

        ''' <summary>True when at least one callback is registered.</summary>
        Public ReadOnly Property HasCallbacks As Boolean
            Get
                Return ResolveProxy IsNot Nothing OrElse AuthenticateNegotiate IsNot Nothing OrElse AuthenticateBasic IsNot Nothing
            End Get
        End Property

        ''' <summary>Build the <c>network.setProvider</c> params (an empty object marks each callback).</summary>
        Public Function ToSetProviderParams() As Dictionary(Of String, Object)
            Dim d As New Dictionary(Of String, Object)
            If Not String.IsNullOrEmpty(Scope) Then d("scope") = Scope
            If ResolveProxy IsNot Nothing Then d("resolveProxy") = New Dictionary(Of String, Object)
            If AuthenticateNegotiate IsNot Nothing Then d("authenticateNegotiate") = New Dictionary(Of String, Object)
            If AuthenticateBasic IsNot Nothing Then d("authenticateBasic") = New Dictionary(Of String, Object)
            Return d
        End Function

    End Class

    ''' <summary>
    ''' Input kinds of a shell authentication prompt.
    ''' </summary>
    Public NotInheritable Class ShellAuthenticationInputKind
        ''' <summary>A password or passphrase (mask the input).</summary>
        Public Const Secret As String = "secret"

        ''' <summary>A visible answer such as a user name.</summary>
        Public Const Text As String = "text"

        ''' <summary>A yes/no question; answer "yes" to accept.</summary>
        Public Const Confirm As String = "confirm"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' A <c>shellAuthentication.request</c> from a program (sudo, ssh, git) run by the agent.
    ''' </summary>
    Public Class ShellAuthenticationRequest

        Public Property SessionId As String

        ''' <summary>Identifies the request; a cancellation carries the same id.</summary>
        Public Property RequestId As String

        ''' <summary>The program that asked: "sudo", "ssh", or "git".</summary>
        Public Property Program As String

        ''' <summary>The prompt text the program supplied.</summary>
        Public Property Prompt As String

        ''' <summary>"secret", "text", or "confirm" (see <see cref="ShellAuthenticationInputKind"/>).</summary>
        Public Property InputKind As String

        ''' <summary>The shell command that triggered the prompt.</summary>
        Public Property Command As String

        ''' <summary>The command's working directory.</summary>
        Public Property Cwd As String

    End Class

    ''' <summary>
    ''' Answer to a <see cref="ShellAuthenticationRequest"/>.
    ''' </summary>
    Public Class ShellAuthenticationResponse

        ''' <summary>"accept" or "cancel".</summary>
        Public Property Action As String = "cancel"

        ''' <summary>The answer sent to the program (accept only).</summary>
        Public Property Value As String

        ''' <summary>Accept the prompt with a value (answer "yes" to a confirm prompt).</summary>
        Public Shared Function Accept(value As String) As ShellAuthenticationResponse
            Return New ShellAuthenticationResponse With {.Action = "accept", .Value = value}
        End Function

        ''' <summary>Decline the prompt.</summary>
        Public Shared Function Cancel() As ShellAuthenticationResponse
            Return New ShellAuthenticationResponse With {.Action = "cancel"}
        End Function

        ''' <summary>Build the JSON-RPC result ({action:"accept", value} or {action:"cancel"}).</summary>
        Public Function ToWire() As Dictionary(Of String, Object)
            If Action = "accept" AndAlso Value IsNot Nothing Then
                Return New Dictionary(Of String, Object) From {{"action", "accept"}, {"value", Value}}
            End If
            Return New Dictionary(Of String, Object) From {{"action", "cancel"}}
        End Function

    End Class

    ''' <summary>
    ''' Answers shell authentication prompts. The token is cancelled when the answer is no longer
    ''' needed (<c>shellAuthentication.cancel</c>).
    ''' </summary>
    Public Delegate Function ShellAuthenticationHandler(
        request As ShellAuthenticationRequest,
        cancellationToken As System.Threading.CancellationToken) As Task(Of ShellAuthenticationResponse)

    ''' <summary>
    ''' A fact the agent asked the host to remember (<c>memoryStore.store</c>).
    ''' </summary>
    Public Class StoredMemory

        Public Property SessionId As String

        ''' <summary>Short topic the memory is about.</summary>
        Public Property Subject As String

        ''' <summary>The fact to remember.</summary>
        Public Property Fact As String

        ''' <summary>Sources supporting the fact, such as file paths with line numbers.</summary>
        Public Property Citations As String

        ''' <summary>Why the agent considers the fact worth remembering.</summary>
        Public Property Reason As String

    End Class

    ''' <summary>
    ''' One field-specific finding reported by a Policy Helper.
    ''' </summary>
    Public Class PolicyHelperFailureDetail
        Public Property Message As String
        Public Property Path As String
    End Class

    ''' <summary>
    ''' Validated help link supplied by a Policy Helper.
    ''' </summary>
    Public Class PolicyHelperHelpLink
        Public Property Url As String
        Public Property Label As String
    End Class

    ''' <summary>
    ''' The latest unresolved Policy Helper failure. Helper-authored text is untrusted: render it as
    ''' plain text and never execute it.
    ''' </summary>
    Public Class PolicyHelperFailure

        ''' <summary>Runtime classification such as "spawn", "timeout", or "schema".</summary>
        Public Property Category As String

        ''' <summary>"ignore" or "failClosed".</summary>
        Public Property FailureMode As String

        Public Property Message As String

        Public Property PreviousPolicyRetained As Boolean

        ''' <summary>Invocation reason such as "sessionStart".</summary>
        Public Property Reason As String

        Public Property Authority As String

        Public Property Code As String

        Public Property Details As List(Of PolicyHelperFailureDetail)

        Public Property Diagnostic As String

        Public Property HelpLink As PolicyHelperHelpLink

        Public Property Remediation As String

        Public Property Severity As String

        ''' <summary>Parse a PolicyHelperFailure JSON object (Nothing when the value is not an object).</summary>
        Public Shared Function FromJson(element As System.Text.Json.JsonElement) As PolicyHelperFailure
            If element.ValueKind <> System.Text.Json.JsonValueKind.Object Then Return Nothing
            Dim failure As New PolicyHelperFailure With {
                .Category = ReadString(element, "category"),
                .FailureMode = ReadString(element, "failureMode"),
                .Message = ReadString(element, "message"),
                .Reason = ReadString(element, "reason"),
                .Authority = ReadString(element, "authority"),
                .Code = ReadString(element, "code"),
                .Diagnostic = ReadString(element, "diagnostic"),
                .Remediation = ReadString(element, "remediation"),
                .Severity = ReadString(element, "severity")
            }
            Dim retained As System.Text.Json.JsonElement = Nothing
            If element.TryGetProperty("previousPolicyRetained", retained) Then
                failure.PreviousPolicyRetained = retained.ValueKind = System.Text.Json.JsonValueKind.True
            End If
            Dim details As System.Text.Json.JsonElement = Nothing
            If element.TryGetProperty("details", details) AndAlso details.ValueKind = System.Text.Json.JsonValueKind.Array Then
                failure.Details = New List(Of PolicyHelperFailureDetail)
                For Each item In details.EnumerateArray()
                    If item.ValueKind = System.Text.Json.JsonValueKind.Object Then
                        failure.Details.Add(New PolicyHelperFailureDetail With {
                            .Message = ReadString(item, "message"),
                            .Path = ReadString(item, "path")
                        })
                    End If
                Next
            End If
            Dim link As System.Text.Json.JsonElement = Nothing
            If element.TryGetProperty("helpLink", link) AndAlso link.ValueKind = System.Text.Json.JsonValueKind.Object Then
                failure.HelpLink = New PolicyHelperHelpLink With {
                    .Url = ReadString(link, "url"),
                    .Label = ReadString(link, "label")
                }
            End If
            Return failure
        End Function

        Private Shared Function ReadString(element As System.Text.Json.JsonElement, name As String) As String
            Dim value As System.Text.Json.JsonElement = Nothing
            If element.TryGetProperty(name, value) AndAlso value.ValueKind = System.Text.Json.JsonValueKind.String Then
                Return value.GetString()
            End If
            Return Nothing
        End Function

    End Class

    ''' <summary>
    ''' Session permission modes (<c>session.permissions.setMode</c>).
    ''' </summary>
    Public NotInheritable Class PermissionMode
        ''' <summary>Permission requests follow the normal approval flow.</summary>
        Public Const Manual As String = "manual"

        ''' <summary>Requests carry an LLM safety recommendation.</summary>
        Public Const Assisted As String = "assisted"

        ''' <summary>Tool, path, and URL permission requests are approved automatically.</summary>
        Public Const AllowAll As String = "allow-all"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Why a permission-mode change was refused (<see cref="PermissionModeSetResult.Reason"/>).
    ''' </summary>
    Public NotInheritable Class PermissionModeRefusalReason
        ''' <summary>Managed (enterprise) policy does not allow the requested mode.</summary>
        Public Const ManagedPolicy As String = "managed_policy"

        Private Sub New()
        End Sub
    End Class

    ''' <summary>
    ''' Result of <c>session.permissions.setMode</c>.
    ''' </summary>
    Public Class PermissionModeSetResult

        <JsonPropertyName("success")>
        Public Property Success As Boolean

        ''' <summary>Authoritative permission mode after the change.</summary>
        <JsonPropertyName("mode")>
        Public Property Mode As String

        ''' <summary>Why the requested mode was refused, when known (see <see cref="PermissionModeRefusalReason"/>).</summary>
        <JsonPropertyName("reason")>
        Public Property Reason As String

    End Class

End Namespace

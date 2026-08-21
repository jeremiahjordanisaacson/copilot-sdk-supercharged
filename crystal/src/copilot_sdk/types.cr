# Copyright (c) Microsoft Corporation. All rights reserved.

# Type definitions for the Copilot SDK.
#
# All data structures used to communicate with the Copilot CLI server
# via JSON-RPC 2.0, using Crystal's JSON::Serializable for encoding.

require "json"

module CopilotSDK
  # Represents the connection state of the client.
  enum ConnectionState
    Disconnected
    Connecting
    Connected
    Error
  end

  # Result type for tool execution.
  enum ToolResultType
    Success
    Failure
    Rejected
    Denied

    def to_json(json : JSON::Builder) : Nil
      json.string(to_s.downcase)
    end

    def self.from_json(value : String) : self
      parse(value)
    end
  end

  # Binary result from a tool execution.
  class ToolBinaryResult
    include JSON::Serializable

    property data : String
    @[JSON::Field(key: "mimeType")]
    property mime_type : String
    @[JSON::Field(key: "type")]
    property result_type : String
    property description : String?

    def initialize(@data, @mime_type, @result_type, @description = nil)
    end
  end

  # Structured tool result with metadata.
  class ToolResultObject
    include JSON::Serializable

    @[JSON::Field(key: "textResultForLlm")]
    property text_result_for_llm : String
    @[JSON::Field(key: "binaryResultsForLlm")]
    property binary_results_for_llm : Array(ToolBinaryResult)?
    @[JSON::Field(key: "resultType")]
    property result_type : String
    property error : String?
    @[JSON::Field(key: "sessionLog")]
    property session_log : String?
    @[JSON::Field(key: "toolTelemetry")]
    property tool_telemetry : Hash(String, JSON::Any)?

    def initialize(@text_result_for_llm, @result_type = "success", @error = nil,
                   @binary_results_for_llm = nil, @session_log = nil, @tool_telemetry = nil)
    end
  end

  # Information about a tool invocation from the server.
  class ToolInvocation
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    @[JSON::Field(key: "toolCallId")]
    property tool_call_id : String
    @[JSON::Field(key: "toolName")]
    property tool_name : String
    property arguments : JSON::Any

    def initialize(@session_id, @tool_call_id, @tool_name, @arguments)
    end
  end

  # Definition of a tool exposed to the Copilot CLI.
  class ToolDefinition
    include JSON::Serializable

    property name : String
    property description : String?
    property parameters : JSON::Any?

    def initialize(@name, @description = nil, @parameters = nil)
    end
  end

  # Payload sent by the server when requesting a tool call.
  class ToolCallRequestPayload
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    @[JSON::Field(key: "toolCallId")]
    property tool_call_id : String
    @[JSON::Field(key: "toolName")]
    property tool_name : String
    property arguments : JSON::Any

    def initialize(@session_id, @tool_call_id, @tool_name, @arguments)
    end
  end

  # Payload for tool call responses sent back to the server.
  class ToolCallResponsePayload
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    @[JSON::Field(key: "toolCallId")]
    property tool_call_id : String
    property result : JSON::Any

    def initialize(@session_id, @tool_call_id, @result)
    end
  end

  # Model information returned by the server.
  class ModelInfo
    include JSON::Serializable

    property id : String
    @[JSON::Field(key: "displayName")]
    property display_name : String?
    property vendor : String?
    property version : String?

    def initialize(@id, @display_name = nil, @vendor = nil, @version = nil)
    end
  end

  # Permission request from the server.
  class PermissionRequest
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    property resource : String
    property action : String
    property details : JSON::Any?

    def initialize(@session_id, @resource, @action, @details = nil)
    end
  end

  # Result of permission evaluation.
  class PermissionRequestResult
    include JSON::Serializable

    property allowed : Bool
    property reason : String?
    @[JSON::Field(key: "decisionContext")]
    property decision_context : Hash(String, JSON::Any)?

    def initialize(@allowed, @reason = nil, @decision_context = nil)
    end
  end

  # Request from the agent to exit plan mode and begin executing.
  class ExitPlanModeRequest
    include JSON::Serializable

    # Summary of the plan or proposed next step.
    property summary : String
    # Full plan content, when available.
    @[JSON::Field(key: "planContent")]
    property plan_content : String?
    # Available actions the user can select.
    property actions : Array(String)
    # The action recommended by the runtime.
    @[JSON::Field(key: "recommendedAction")]
    property recommended_action : String

    def initialize(@summary, @actions, @recommended_action, @plan_content = nil)
    end
  end

  # Result of an exit-plan-mode request.
  class ExitPlanModeResult
    include JSON::Serializable

    # Whether the user approved exiting plan mode.
    property approved : Bool
    # Selected action, if the user chose one.
    @[JSON::Field(key: "selectedAction")]
    property selected_action : String?
    # Optional feedback provided by the user.
    property feedback : String?

    def initialize(@approved, @selected_action = nil, @feedback = nil)
    end
  end

  # Context for an elicitation request from the server.
  class ElicitationRequest
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    property message : String
    property mode : String?
    @[JSON::Field(key: "requestedSchema")]
    property requested_schema : Hash(String, JSON::Any)?

    def initialize(@session_id, @message, @mode = nil, @requested_schema = nil)
    end
  end

  # Result returned from an elicitation handler.
  class ElicitationResult
    include JSON::Serializable

    property action : String
    property content : Hash(String, JSON::Any)?

    def initialize(@action = "accept", @content = nil)
    end
  end

  # Session event from the Copilot CLI.
  class SessionEvent
    include JSON::Serializable

    property type : String
    property data : JSON::Any?
    @[JSON::Field(key: "id")]
    property id : String?
    @[JSON::Field(key: "timestamp")]
    property timestamp : String?
    @[JSON::Field(key: "parentId")]
    property parent_id : String?
    @[JSON::Field(key: "agentId")]
    property agent_id : String?
    property ephemeral : Bool?

    def initialize(@type, @data = nil, @id = nil, @timestamp = nil, @parent_id = nil, @agent_id = nil, @ephemeral = nil)
    end

    # Helper to extract assistant message content from an event.
    def assistant_message_content : String?
      return nil unless type == "assistant.message"
      data.try &.["content"]?.try &.as_s
    end

    # Helper to extract assistant reasoning content from an event.
    def assistant_reasoning_content : String?
      return nil unless type == "assistant.reasoning"
      data.try &.["content"]?.try &.as_s
    end

    # Helper to extract delta text from streaming events.
    def delta_text : String?
      return nil unless type.ends_with?("_delta")
      data.try &.["delta"]?.try &.as_s
    end
  end

  # Configuration for creating a new session.
  class SessionConfig
    include JSON::Serializable

    property model : String?
    property streaming : Bool?
    @[JSON::Field(key: "systemMessage")]
    property system_message : String?
    property tools : Array(ToolDefinition)?
    property instructions : String?
    @[JSON::Field(key: "agentMode")]
    property agent_mode : String?
    @[JSON::Field(key: "excludedTools")]
    property excluded_tools : Array(String)?
    @[JSON::Field(key: "mcpServers")]
    property mcp_servers : Hash(String, McpServerConfig)?
    @[JSON::Field(key: "modelCapabilities")]
    property model_capabilities : Hash(String, JSON::Any)?
    @[JSON::Field(key: "enableConfigDiscovery")]
    property enable_config_discovery : Bool?
    @[JSON::Field(key: "includeSubAgentStreamingEvents")]
    property include_sub_agent_streaming_events : Bool?
    property commands : Array(CommandDefinition)?
    @[JSON::Field(key: "skillDirectories")]
    property skill_directories : Array(String)?
    @[JSON::Field(key: "disabledSkills")]
    property disabled_skills : Array(String)?
    @[JSON::Field(key: "workingDirectory")]
    property working_directory : String?
    @[JSON::Field(key: "gitHubToken")]
    property github_token : String?
    @[JSON::Field(key: "reasoningEffort")]
    property reasoning_effort : String?
    @[JSON::Field(key: "responseFormat")]
    property response_format : String?
    @[JSON::Field(key: "requestHeaders")]
    property request_headers : Hash(String, String)?
    @[JSON::Field(key: "elicitationHandler")]
    property elicitation_handler : Bool?
    @[JSON::Field(key: "instructionDirectories")]
    property instruction_directories : Array(String)?
    @[JSON::Field(key: "enableCitations")]
    property enable_citations : Bool?
    @[JSON::Field(key: "excludedBuiltinAgents")]
    property excluded_builtin_agents : Array(String)?
    @[JSON::Field(key: "sessionLimits")]
    property session_limits : SessionLimits?
    @[JSON::Field(key: "memory")]
    property memory_config : MemoryConfiguration?
    @[JSON::Field(key: "otlpProtocol")]
    property otlp_protocol : String?
    @[JSON::Field(key: "enableWebSocketResponses")]
    property enable_web_socket_responses : Bool?
    @[JSON::Field(key: "expAssignments")]
    property exp_assignments : Hash(String, JSON::Any)?
    @[JSON::Field(key: "mcpAuthHandler")]
    property mcp_auth_handler : Bool?
    @[JSON::Field(key: "rewindEnabled")]
    property rewind_enabled : Bool?
    @[JSON::Field(key: "additionalDirectories")]
    property additional_directories : Array(String)?
    @[JSON::Field(key: "disabledMcpServers")]
    property disabled_mcp_servers : Array(String)?
    @[JSON::Field(key: "githubMcpToolConfig")]
    property github_mcp_tool_config : Hash(String, JSON::Any)?
    @[JSON::Field(key: "canvasProvider")]
    property canvas_provider : JSON::Any?
    @[JSON::Field(key: "customAgentsLocalOnly")]
    property custom_agents_local_only : Bool?
    @[JSON::Field(key: "toolSearch")]
    property tool_search : JSON::Any?
    @[JSON::Field(key: "experimentalMode")]
    property experimental_mode : Bool?
    @[JSON::Field(key: "contentExclusion")]
    property content_exclusion : Bool?

    def initialize(@model = nil, @streaming = nil, @system_message = nil,
                   @tools = nil, @instructions = nil, @agent_mode = nil,
                   @excluded_tools = nil, @mcp_servers = nil, @model_capabilities = nil,
                   @enable_config_discovery = nil, @include_sub_agent_streaming_events = nil,
                   @commands = nil, @skill_directories = nil, @disabled_skills = nil,
                   @working_directory = nil, @github_token = nil, @reasoning_effort = nil,
                   @response_format = nil, @request_headers = nil, @elicitation_handler = nil,
                   @instruction_directories = nil, @enable_citations = nil,
                   @excluded_builtin_agents = nil, @session_limits = nil, @memory_config = nil,
                   @otlp_protocol = nil, @enable_web_socket_responses = nil,
                   @exp_assignments = nil, @mcp_auth_handler = nil,
                   @rewind_enabled = nil, @additional_directories = nil,
                   @disabled_mcp_servers = nil, @github_mcp_tool_config = nil,
                   @canvas_provider = nil, @custom_agents_local_only = nil,
                   @tool_search = nil, @experimental_mode = nil, @content_exclusion = nil)
    end
  end

  # Configuration for resuming an existing session.
  class ResumeSessionConfig
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    property model : String?
    property streaming : Bool?
    @[JSON::Field(key: "instructionDirectories")]
    property instruction_directories : Array(String)?

    def initialize(@session_id, @model = nil, @streaming = nil, @instruction_directories = nil)
    end
  end

  # Options for sending a message to a session.
  class MessageOptions
    include JSON::Serializable

    property prompt : String
    property attachments : Array(JSON::Any)?
    property mode : String?
    @[JSON::Field(key: "agentMode")]
    property agent_mode : String?
    @[JSON::Field(key: "displayPrompt")]
    property display_prompt : String?
    @[JSON::Field(key: "requestHeaders")]
    property request_headers : Hash(String, String)?

    def initialize(@prompt, @attachments = nil, @mode = nil,
                   @agent_mode = nil, @display_prompt = nil, @request_headers = nil)
    end
  end

  # Filesystem provider configuration.
  class SessionFsConfig
    include JSON::Serializable

    @[JSON::Field(key: "initialCwd")]
    property initial_cwd : String
    @[JSON::Field(key: "sessionStatePath")]
    property session_state_path : String
    property conventions : String

    def initialize(@initial_cwd, @session_state_path, @conventions = "posix")
    end
  end

  # MCP server type.
  enum McpServerType
    Stdio
    Http

    def to_json(json : JSON::Builder) : Nil
      json.string(to_s.downcase)
    end
  end

  # MCP server configuration.
  class McpServerConfig
    include JSON::Serializable

    property type : McpServerType
    property command : String?
    property args : Array(String)?
    property url : String?
    property env : Hash(String, String)?
    property headers : Hash(String, String)?

    def initialize(@type, @command = nil, @args = nil, @url = nil, @env = nil, @headers = nil)
    end
  end

  # Command definition for session commands.
  class CommandDefinition
    include JSON::Serializable

    property name : String
    property description : String?

    def initialize(@name, @description = nil)
    end
  end

  # Options provided when constructing a CopilotClient.
  class CopilotClientOptions
    property cli_path : String?
    property cli_url : String?
    property auto_start : Bool
    property request_timeout : Int32
    property github_token : String?
    property use_logged_in_user : Bool?
    property session_idle_timeout_seconds : Int32?
    property session_fs : SessionFsConfig?
    property copilot_home : String?
    property tcp_connection_token : String?
    property bearer_token_provider : BearerTokenProvider?
    property on_mcp_auth_request : McpAuthHandler?
    property on_get_trace_context : TraceContextProvider?
    property builtin_plugin_directories : Array(String)? # wire: builtinPluginDirectories
    property in_process : Bool?                           # wire: inProcess

    def initialize(@cli_path = nil, @cli_url = nil, @auto_start = true, @request_timeout = 30,
                   @github_token = nil, @use_logged_in_user = nil,
                   @session_idle_timeout_seconds = nil, @session_fs = nil,
                   @copilot_home = nil, @tcp_connection_token = nil,
                   @bearer_token_provider = nil, @on_mcp_auth_request = nil,
                   @on_get_trace_context = nil,
                   @builtin_plugin_directories = nil, @in_process = nil)
    end
  end

  # Server status response.
  class GetStatusResponse
    include JSON::Serializable

    property version : String?
    @[JSON::Field(key: "protocolVersion")]
    property protocol_version : Int32?
    property status : String?

    def initialize(@version = nil, @protocol_version = nil, @status = nil)
    end
  end

  # Authentication status response.
  class GetAuthStatusResponse
    include JSON::Serializable

    property authenticated : Bool?
    property user : String?

    def initialize(@authenticated = nil, @user = nil)
    end
  end

  # Metadata about a session.
  class SessionMetadata
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String
    property model : String?
    @[JSON::Field(key: "createdAt")]
    property created_at : String?

    def initialize(@session_id, @model = nil, @created_at = nil)
    end
  end

  # ---------------------------------------------------------------------------
  # Upstream-sync feature types (parity with @github/copilot-sdk)
  # JSON-RPC wire keys are preserved in camelCase.
  # ---------------------------------------------------------------------------

  # Per-session spending limits.
  class SessionLimits
    include JSON::Serializable

    @[JSON::Field(key: "maxAiCredits")]
    property max_ai_credits : Float64?

    def initialize(@max_ai_credits = nil)
    end
  end

  # Opt-in persistent session memory configuration.
  class MemoryConfiguration
    include JSON::Serializable

    property enabled : Bool?

    def initialize(@enabled = nil)
    end
  end

  # Authoring options for a programmatic agent factory.
  class AgentFactoryOptions
    include JSON::Serializable

    @[JSON::Field(key: "argsSchema")]
    property args_schema : Hash(String, JSON::Any)?

    def initialize(@args_schema = nil)
    end
  end

  # Session hook identifiers (includes post-tool-use and pre-MCP-tool-call).
  enum HookType
    PreToolUse
    PostToolUse
    PreMcpToolCall
    UserPromptSubmitted
    UserPromptTransformed
    SessionStart
    SessionEnd

    # The camelCase wire name for this hook.
    def to_wire : String
      case self
      when PreToolUse          then "preToolUse"
      when PostToolUse         then "postToolUse"
      when PreMcpToolCall      then "preMcpToolCall"
      when UserPromptSubmitted then "userPromptSubmitted"
      when UserPromptTransformed then "userPromptTransformed"
      when SessionStart        then "sessionStart"
      when SessionEnd          then "sessionEnd"
      else                          "preToolUse"
      end
    end
  end

  # Tool defer-loading policy (:auto loads lazily via search, :never pre-loads).
  enum ToolDeferMode
    Auto
    Never

    def to_wire : String
      case self
      when Auto  then "auto"
      when Never then "never"
      else            "auto"
      end
    end
  end

  # Addressable system-message sections.
  enum SystemMessageSection
    Preamble
    Identity
    ToolInstructions
    Preserve

    def to_wire : String
      case self
      when Preamble         then "preamble"
      when Identity         then "identity"
      when ToolInstructions then "tool_instructions"
      when Preserve         then "preserve"
      else                       "preamble"
      end
    end
  end

  # GitHub-anchored attachment variants.
  enum GitHubAttachmentType
    GitHubCommit
    GitHubRelease
    GitHubActionsJob
    GitHubRepository
    GitHubFileDiff
    GitHubTreeComparison
    GitHubUrl
    GitHubFile
    GitHubSnippet

    def to_wire : String
      to_s
    end
  end

  # Alias for a BYOK bearer-token provider (returns a token for outbound requests).
  alias BearerTokenProvider = Proc(JSON::Any, String)

  # Alias for an MCP OAuth host-token handler.
  alias McpAuthHandler = Proc(JSON::Any, String)

  # Alias for tool handler callbacks.
  alias ToolHandler = Proc(JSON::Any, ToolInvocation, JSON::Any)

  # Alias for permission handler callbacks.
  alias PermissionHandler = Proc(PermissionRequest, String, PermissionRequestResult)

  # Alias for session event handler callbacks.
  alias SessionEventHandler = Proc(SessionEvent, Nil)

  # Alias for exit-plan-mode handler callbacks.
  alias ExitPlanModeHandler = Proc(ExitPlanModeRequest, String, ExitPlanModeResult)

  # W3C Trace Context headers returned by a TraceContextProvider and injected
  # into session.create/resume/send RPCs for distributed tracing of agent turns.
  struct TraceContext
    getter traceparent : String?
    getter tracestate : String?

    def initialize(@traceparent : String? = nil, @tracestate : String? = nil)
    end
  end

  # Alias for trace-context provider callbacks. Returns the current W3C trace
  # context to inject into agent-turn RPCs.
  alias TraceContextProvider = Proc(TraceContext)

  # Default permission handler that denies all requests.
  def self.deny_all_permissions(request : PermissionRequest, session_id : String) : PermissionRequestResult
    PermissionRequestResult.new(allowed: false, reason: "denied by default policy")
  end

  # Permission handler that approves all requests.
  def self.approve_all_permissions(request : PermissionRequest, session_id : String) : PermissionRequestResult
    PermissionRequestResult.new(allowed: true)
  end
end

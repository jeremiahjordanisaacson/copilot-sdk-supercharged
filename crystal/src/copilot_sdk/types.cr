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

  # Completion type for slash command inputs.
  enum SlashCommandInputCompletion
    Directory

    def to_json(json : JSON::Builder) : Nil
      json.string(to_s.downcase)
    end

    def self.from_json(value : String) : self
      parse(value)
    end
  end

  # Kind of slash command.
  enum SlashCommandKind
    Builtin
    Client
    Skill

    def to_json(json : JSON::Builder) : Nil
      json.string(to_s.downcase)
    end

    def self.from_json(value : String) : self
      parse(value)
    end
  end

  # Price category for model picker.
  enum ModelPickerPriceCategory
    High
    Low
    Medium
    VeryHigh

    def to_json(json : JSON::Builder) : Nil
      case self
      when VeryHigh then json.string("very_high")
      else json.string(to_s.downcase)
      end
    end

    def self.from_json(value : String) : self
      case value
      when "very_high" then VeryHigh
      else parse(value)
      end
    end
  end

  # Input definition for a slash command.
  class SlashCommandInput
    include JSON::Serializable

    property hint : String
    property completion : SlashCommandInputCompletion?

    def initialize(@hint, @completion = nil)
    end
  end

  # Information about a slash command.
  class SlashCommandInfo
    include JSON::Serializable

    @[JSON::Field(key: "allowDuringAgentExecution")]
    property allow_during_agent_execution : Bool
    property description : String
    property kind : SlashCommandKind
    property name : String
    property aliases : Array(String)?
    property experimental : Bool?
    property input : SlashCommandInput?

    def initialize(@allow_during_agent_execution, @description, @kind, @name,
                   @aliases = nil, @experimental = nil, @input = nil)
    end
  end

  # Request to invoke a command.
  class CommandsInvokeRequest
    include JSON::Serializable

    property name : String
    property input : String?

    def initialize(@name, @input = nil)
    end
  end

  # Request to list available commands.
  class CommandsListRequest
    include JSON::Serializable

    @[JSON::Field(key: "includeBuiltins")]
    property include_builtins : Bool?
    @[JSON::Field(key: "includeClientCommands")]
    property include_client_commands : Bool?
    @[JSON::Field(key: "includeSkills")]
    property include_skills : Bool?

    def initialize(@include_builtins = nil, @include_client_commands = nil, @include_skills = nil)
    end
  end

  # Token pricing information for model billing.
  class ModelBillingTokenPrices
    include JSON::Serializable

    @[JSON::Field(key: "batchSize")]
    property batch_size : Int32?
    @[JSON::Field(key: "cachePrice")]
    property cache_price : Int32?
    @[JSON::Field(key: "inputPrice")]
    property input_price : Int32?
    @[JSON::Field(key: "outputPrice")]
    property output_price : Int32?

    def initialize(@batch_size = nil, @cache_price = nil, @input_price = nil, @output_price = nil)
    end
  end

  # Experimental: Diagnostics from loading skills.
  class SkillsLoadDiagnostics
    include JSON::Serializable

    property errors : Array(String)
    property warnings : Array(String)

    def initialize(@errors, @warnings)
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

    def initialize(@allowed, @reason = nil)
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
    @[JSON::Field(key: "requestExitPlanMode")]
    property request_exit_plan_mode : Bool?
    @[JSON::Field(key: "enableSessionTelemetry")]
    property enable_session_telemetry : Bool?

    def initialize(@model = nil, @streaming = nil, @system_message = nil,
                   @tools = nil, @instructions = nil, @agent_mode = nil,
                   @excluded_tools = nil, @mcp_servers = nil, @model_capabilities = nil,
                   @enable_config_discovery = nil, @include_sub_agent_streaming_events = nil,
                   @commands = nil, @skill_directories = nil, @disabled_skills = nil,
                   @working_directory = nil, @github_token = nil, @reasoning_effort = nil,
                   @response_format = nil, @request_headers = nil, @elicitation_handler = nil,
                   @instruction_directories = nil, @request_exit_plan_mode = nil,
                   @enable_session_telemetry = nil)
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
    @[JSON::Field(key: "requestExitPlanMode")]
    property request_exit_plan_mode : Bool?
    @[JSON::Field(key: "enableSessionTelemetry")]
    property enable_session_telemetry : Bool?

    def initialize(@session_id, @model = nil, @streaming = nil, @instruction_directories = nil,
                   @request_exit_plan_mode = nil, @enable_session_telemetry = nil)
    end
  end

  # Options for sending a message to a session.
  class MessageOptions
    include JSON::Serializable

    property prompt : String
    property attachments : Array(JSON::Any)?
    property mode : String?

    def initialize(@prompt, @attachments = nil, @mode = nil)
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
    property remote : Bool
    property on_get_trace_context : TraceContextProvider?

    def initialize(@cli_path = nil, @cli_url = nil, @auto_start = true, @request_timeout = 30,
                   @github_token = nil, @use_logged_in_user = nil,
                   @session_idle_timeout_seconds = nil, @session_fs = nil,
                   @copilot_home = nil, @tcp_connection_token = nil,
                   @remote = false, @on_get_trace_context = nil)
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

  # Exit plan mode request from the agent.
  class ExitPlanModeRequest
    include JSON::Serializable
    property summary : String
    @[JSON::Field(key: "planContent")]
    property plan_content : String?
    property actions : Array(String)
    @[JSON::Field(key: "recommendedAction")]
    property recommended_action : String

    def initialize(@summary, @actions, @recommended_action, @plan_content = nil)
    end
  end

  # Response to an exit-plan-mode request.
  class ExitPlanModeResult
    include JSON::Serializable
    property approved : Bool
    @[JSON::Field(key: "selectedAction")]
    property selected_action : String?
    property feedback : String?

    def initialize(@approved, @selected_action = nil, @feedback = nil)
    end
  end

  # W3C Trace Context for distributed tracing.
  class TraceContext
    include JSON::Serializable
    property traceparent : String?
    property tracestate : String?

    def initialize(@traceparent = nil, @tracestate = nil)
    end
  end

  # Callback that returns the current W3C Trace Context.
  alias TraceContextProvider = Proc(TraceContext)

  # Handler for exit-plan-mode requests.
  alias ExitPlanModeHandler = Proc(ExitPlanModeRequest, String, ExitPlanModeResult)

  # Alias for tool handler callbacks.
  alias ToolHandler = Proc(JSON::Any, ToolInvocation, JSON::Any)

  # Alias for permission handler callbacks.
  alias PermissionHandler = Proc(PermissionRequest, String, PermissionRequestResult)

  # Alias for session event handler callbacks.
  alias SessionEventHandler = Proc(SessionEvent, Nil)

  # Default permission handler that denies all requests.
  def self.deny_all_permissions(request : PermissionRequest, session_id : String) : PermissionRequestResult
    PermissionRequestResult.new(allowed: false, reason: "denied by default policy")
  end

  # Permission handler that approves all requests.
  def self.approve_all_permissions(request : PermissionRequest, session_id : String) : PermissionRequestResult
    PermissionRequestResult.new(allowed: true)
  end
end

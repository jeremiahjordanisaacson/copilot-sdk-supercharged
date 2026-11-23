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
    # When true, repeating a call with the same arguments has no additional
    # effect, so the runtime may safely retry a call whose outcome was lost.
    property idempotent : Bool?

    def initialize(@name, @description = nil, @parameters = nil, @idempotent = nil)
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
    @[JSON::Field(key: "enableExperimentalMode")]
    property experimental_mode : Bool?
    @[JSON::Field(key: "contentExclusion")]
    property content_exclusion : Bool?

    # --- v2.6.0 / v3.0.0 session options (SDK protocol 4) ---
    # Tools the session may use (`tools.availableTools`); required in empty client mode.
    @[JSON::Field(key: "availableTools")]
    property available_tools : Array(String)?
    # Legacy single BYOK provider, sent as a `"default"` named provider.
    property provider : ProviderConfig?
    # Session name (create only).
    property name : String?
    # Ask-user tool variant: `AskUserVariant::LEGACY` or `AskUserVariant::ELICITATION`.
    @[JSON::Field(key: "askUserVariant")]
    property ask_user_variant : String?
    # Sandbox configuration for tool execution (`permissions.sandbox`).
    @[JSON::Field(key: "sandboxConfig")]
    property sandbox_config : SandboxConfig?
    # Runtime feature flag overrides (`experiments.flags`).
    @[JSON::Field(key: "featureFlags")]
    property feature_flags : Hash(String, Bool)?
    # Model allowlist (`policy.allowedModels`).
    @[JSON::Field(key: "allowedModels")]
    property allowed_models : Array(String)?
    # OAuth Client ID Metadata Document URL for MCP authorization.
    @[JSON::Field(key: "authClientIdMetadataUrl")]
    property auth_client_id_metadata_url : String?
    # Custom agent definition directories (`agents.directories`).
    @[JSON::Field(key: "agentDirectories")]
    property agent_directories : Array(String)?
    # Hook definition directories (`hooks.directories`).
    @[JSON::Field(key: "hookDirectories")]
    property hook_directories : Array(String)?
    # Grants working-directory trust at creation: `WorkingDirectoryTrust::SESSION`
    # or `WorkingDirectoryTrust::USER` (create only; requires `working_directory`).
    @[JSON::Field(key: "trustWorkingDirectory")]
    property trust_working_directory : String?
    # Image generation configuration (`tools.imageGeneration`).
    @[JSON::Field(key: "imageGeneration")]
    property image_generation : ImageGenerationConfig?
    # Autopilot continuation options (`execution.autopilot`).
    property autopilot : AutopilotOptions?
    # Named BYOK provider connections (`inference.providers`).
    property providers : Array(NamedProviderConfig)?
    # BYOK model definitions (`inference.models`); select one with "<provider>/<id>".
    property models : Array(ProviderModelConfig)?
    # Named connection for unqualified BYOK model selections (`inference.defaultProvider`).
    @[JSON::Field(key: "defaultProvider")]
    property default_provider : String?
    # Copilot API options (`github.capi`); `enabled: false` makes a BYOK-only session.
    property capi : CapiSessionOptions?
    # Session hook handlers (sends `hooks.hasHandlers` when any is set).
    @[JSON::Field(ignore: true)]
    property hooks : SessionHooks? = nil
    # Supplies GitHub tokens on demand (`gitHubToken.getToken`). Mutually
    # exclusive with `github_token`.
    @[JSON::Field(ignore: true)]
    property github_token_provider : GitHubTokenProvider? = nil
    # Answers shell authentication prompts (sends `interaction.requestShellAuthentication`).
    @[JSON::Field(ignore: true)]
    property on_shell_authentication_request : ShellAuthenticationHandler? = nil

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
                   @tool_search = nil, @experimental_mode = nil, @content_exclusion = nil,
                   @available_tools = nil, @provider = nil, @name = nil,
                   @ask_user_variant = nil, @sandbox_config = nil, @feature_flags = nil,
                   @allowed_models = nil, @auth_client_id_metadata_url = nil,
                   @agent_directories = nil, @hook_directories = nil,
                   @trust_working_directory = nil, @image_generation = nil, @autopilot = nil,
                   @providers = nil, @models = nil, @default_provider = nil, @capi = nil,
                   @hooks = nil, @github_token_provider = nil,
                   @on_shell_authentication_request = nil)
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

    # --- v2.6.0 / v3.0.0 resume options (SDK protocol 4) ---
    @[JSON::Field(key: "availableTools")]
    property available_tools : Array(String)?
    @[JSON::Field(key: "excludedTools")]
    property excluded_tools : Array(String)?
    property tools : Array(ToolDefinition)?
    @[JSON::Field(key: "systemMessage")]
    property system_message : String?
    @[JSON::Field(key: "workingDirectory")]
    property working_directory : String?
    @[JSON::Field(key: "gitHubToken")]
    property github_token : String?
    @[JSON::Field(key: "reasoningEffort")]
    property reasoning_effort : String?
    @[JSON::Field(key: "modelCapabilities")]
    property model_capabilities : Hash(String, JSON::Any)?
    @[JSON::Field(key: "mcpServers")]
    property mcp_servers : Hash(String, McpServerConfig)?
    property provider : ProviderConfig?
    @[JSON::Field(key: "memory")]
    property memory_config : MemoryConfiguration?
    # Suppresses the `session.resume` event (preferred over `disable_resume`).
    @[JSON::Field(key: "suppressResumeEvent")]
    property suppress_resume_event : Bool?
    # Legacy alias of `suppress_resume_event`.
    @[JSON::Field(key: "disableResume")]
    property disable_resume : Bool?
    # Recover the conversation from the transcript when saved state cannot be loaded.
    @[JSON::Field(key: "allowTranscriptRecovery")]
    property allow_transcript_recovery : Bool?
    @[JSON::Field(key: "askUserVariant")]
    property ask_user_variant : String?
    @[JSON::Field(key: "sandboxConfig")]
    property sandbox_config : SandboxConfig?
    @[JSON::Field(key: "featureFlags")]
    property feature_flags : Hash(String, Bool)?
    @[JSON::Field(key: "allowedModels")]
    property allowed_models : Array(String)?
    @[JSON::Field(key: "authClientIdMetadataUrl")]
    property auth_client_id_metadata_url : String?
    @[JSON::Field(key: "agentDirectories")]
    property agent_directories : Array(String)?
    @[JSON::Field(key: "hookDirectories")]
    property hook_directories : Array(String)?
    @[JSON::Field(key: "imageGeneration")]
    property image_generation : ImageGenerationConfig?
    property autopilot : AutopilotOptions?
    property providers : Array(NamedProviderConfig)?
    property models : Array(ProviderModelConfig)?
    @[JSON::Field(key: "defaultProvider")]
    property default_provider : String?
    property capi : CapiSessionOptions?
    # When true (and `default_provider` is nil), clears the default provider by
    # sending `inference.defaultProvider: null`.
    @[JSON::Field(ignore: true)]
    property clear_default_provider : Bool? = nil
    @[JSON::Field(ignore: true)]
    property hooks : SessionHooks? = nil
    @[JSON::Field(ignore: true)]
    property github_token_provider : GitHubTokenProvider? = nil
    @[JSON::Field(ignore: true)]
    property on_shell_authentication_request : ShellAuthenticationHandler? = nil

    def initialize(@session_id, @model = nil, @streaming = nil, @instruction_directories = nil,
                   @available_tools = nil, @excluded_tools = nil, @tools = nil,
                   @system_message = nil, @working_directory = nil, @github_token = nil,
                   @reasoning_effort = nil, @model_capabilities = nil, @mcp_servers = nil,
                   @provider = nil, @memory_config = nil, @suppress_resume_event = nil,
                   @disable_resume = nil, @allow_transcript_recovery = nil,
                   @ask_user_variant = nil, @sandbox_config = nil, @feature_flags = nil,
                   @allowed_models = nil, @auth_client_id_metadata_url = nil,
                   @agent_directories = nil, @hook_directories = nil, @image_generation = nil,
                   @autopilot = nil, @providers = nil, @models = nil, @default_provider = nil,
                   @capi = nil, @clear_default_provider = nil, @hooks = nil,
                   @github_token_provider = nil, @on_shell_authentication_request = nil)
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
    # Who the message is from: `MessageSource::USER`, `MessageSource::SYSTEM`,
    # or `MessageSource.agent(id)`.
    property source : String?
    # JSON schema the final assistant response must match. Sent as
    # `responseFormat: {type: "json_schema", jsonSchema: {name, strict, schema}}`.
    @[JSON::Field(ignore: true)]
    property response_schema : JSON::Any? = nil
    # Opaque caller-chosen identifier echoed on the events of this turn.
    @[JSON::Field(key: "clientTurnId")]
    property client_turn_id : String?

    def initialize(@prompt, @attachments = nil, @mode = nil,
                   @agent_mode = nil, @display_prompt = nil, @request_headers = nil,
                   @source = nil, @response_schema = nil, @client_turn_id = nil)
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
    property in_process : Bool?                          # wire: inProcess
    # Identity of the integrating application, sent as `clientInfo` on the
    # `connect` handshake (empty fields dropped; omitted when all are empty).
    property client_info : ClientInfo?
    # Session defaults profile: `ClientMode::CODING_AGENT` (default) or
    # `ClientMode::EMPTY`; `ClientMode::COPILOT_CLI` is a legacy alias.
    property mode : String?
    # Network callbacks registered with `network.setProvider` after connecting.
    property network : NetworkConfig?

    def initialize(@cli_path = nil, @cli_url = nil, @auto_start = true, @request_timeout = 30,
                   @github_token = nil, @use_logged_in_user = nil,
                   @session_idle_timeout_seconds = nil, @session_fs = nil,
                   @copilot_home = nil, @tcp_connection_token = nil,
                   @bearer_token_provider = nil, @on_mcp_auth_request = nil,
                   @on_get_trace_context = nil,
                   @builtin_plugin_directories = nil, @in_process = nil,
                   @client_info = nil, @mode = nil, @network = nil)
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
    # Stores memories in the app instead of the runtime's built-in store
    # (`memoryStore.store`, sends `state.hasMemoryStore`). Requires `enabled`
    # (nil is treated as enabled). Experimental.
    @[JSON::Field(ignore: true)]
    property store_memory : MemoryStoreHandler? = nil

    def initialize(@enabled = nil, @store_memory = nil)
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
    SubagentStart
    SubagentStop

    # The camelCase wire name for this hook.
    def to_wire : String
      case self
      when PreToolUse            then "preToolUse"
      when PostToolUse           then "postToolUse"
      when PreMcpToolCall        then "preMcpToolCall"
      when UserPromptSubmitted   then "userPromptSubmitted"
      when UserPromptTransformed then "userPromptTransformed"
      when SessionStart          then "sessionStart"
      when SessionEnd            then "sessionEnd"
      when SubagentStart         then "subagentStart"
      when SubagentStop          then "subagentStop"
      else                            "preToolUse"
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

  # ---------------------------------------------------------------------------
  # SDK protocol 4 (v3.0.0) types. JSON-RPC wire keys are camelCase.
  # ---------------------------------------------------------------------------

  # Identity of the integrating application, sent as `clientInfo` on the
  # `connect` handshake.
  class ClientInfo
    include JSON::Serializable

    # Name of the application using the SDK (for example "acme-portal").
    @[JSON::Field(key: "editorName")]
    property editor_name : String?
    @[JSON::Field(key: "editorVersion")]
    property editor_version : String?
    # Name of the integration within the application (extension, plugin).
    @[JSON::Field(key: "extensionName")]
    property extension_name : String?
    @[JSON::Field(key: "extensionVersion")]
    property extension_version : String?

    def initialize(@editor_name = nil, @editor_version = nil, @extension_name = nil,
                   @extension_version = nil)
    end

    # The wire form with empty fields dropped, or nil when every field is empty.
    def to_wire : Hash(String, JSON::Any)?
      wire = {} of String => JSON::Any
      {"editorName"       => @editor_name,
       "editorVersion"    => @editor_version,
       "extensionName"    => @extension_name,
       "extensionVersion" => @extension_version}.each do |key, value|
        wire[key] = JSON::Any.new(value) if value && !value.empty?
      end
      wire.empty? ? nil : wire
    end
  end

  # Values for `CopilotClientOptions#mode`.
  module ClientMode
    # Default: identical to the Copilot CLI's ambient behavior.
    CODING_AGENT = "coding-agent"
    # Ambient runtime features off; every session must set `available_tools`.
    EMPTY = "empty"
    # Legacy alias of `CODING_AGENT`.
    COPILOT_CLI = "copilot-cli"
  end

  # Auto routing tiers for `CopilotSession#switch_auto_tier`.
  module AutoTier
    FAST         = "fast"
    EFFICIENCY   = "efficiency"
    BALANCE      = "balance"
    INTELLIGENCE = "intelligence"
  end

  # Values for the ask-user tool variant (`ask_user_variant`).
  module AskUserVariant
    LEGACY      = "legacy"
    ELICITATION = "elicitation"
  end

  # Values for `SessionConfig#trust_working_directory`.
  module WorkingDirectoryTrust
    # Trust applies only to the created session.
    SESSION = "session"
    # Trust is persisted for later sessions.
    USER = "user"
  end

  # Values for `MessageOptions#source` (`session.send` param `source`).
  module MessageSource
    USER   = "user"
    SYSTEM = "system"

    # A message from another agent.
    def self.agent(id : String) : String
      "agent-#{id}"
    end
  end

  # Request passed to a `GitHubTokenProvider` (`gitHubToken.getToken`).
  class GitHubTokenRequest
    # Effective GitHub host the token is for.
    getter host : String
    # Session receiving the token, when known.
    getter session_id : String?
    # Why the runtime is requesting a credential.
    getter reason : String

    def initialize(@host : String, @session_id : String?, @reason : String)
    end
  end

  # Result of a `GitHubTokenProvider`: a token or a cancellation.
  class GitHubTokenResult
    # `"token"` or `"cancelled"`.
    getter kind : String
    getter access_token : String?
    # Remaining token lifetime in seconds (more than one hour must remain).
    getter expires_in : Int64?
    # OAuth token type (bearer when omitted).
    getter token_type : String?

    def initialize(@kind : String, @access_token : String? = nil, @expires_in : Int64? = nil,
                   @token_type : String? = nil)
    end

    def self.token(access_token : String, expires_in : Int, token_type : String? = nil) : self
      new("token", access_token, expires_in.to_i64, token_type)
    end

    def self.cancelled : self
      new("cancelled")
    end

    def to_json_any : JSON::Any
      wire = {"kind" => JSON::Any.new(@kind)}
      if @kind == "token"
        wire["accessToken"] = JSON::Any.new(@access_token)
        wire["expiresIn"] = JSON::Any.new(@expires_in)
        if token_type = @token_type
          wire["tokenType"] = JSON::Any.new(token_type)
        end
      end
      JSON::Any.new(wire)
    end
  end

  # Acquires a GitHub token for one session on demand.
  alias GitHubTokenProvider = Proc(GitHubTokenRequest, GitHubTokenResult)

  # Sandbox configuration for tool execution (`permissions.sandbox`).
  class SandboxConfig
    include JSON::Serializable

    property enabled : Bool
    # User sandbox policy (filesystem and network rules), passed through as-is.
    @[JSON::Field(key: "userPolicy")]
    property user_policy : JSON::Any?
    @[JSON::Field(key: "addCurrentWorkingDirectory")]
    property add_current_working_directory : Bool?
    @[JSON::Field(key: "sandboxMcpServers")]
    property sandbox_mcp_servers : Bool?
    @[JSON::Field(key: "sandboxLspServers")]
    property sandbox_lsp_servers : Bool?
    @[JSON::Field(key: "sandboxExtensions")]
    property sandbox_extensions : Bool?
    # Lets the agent request an individual command outside the sandbox through
    # the normal permission flow.
    @[JSON::Field(key: "allowBypass")]
    property allow_bypass : Bool?

    def initialize(@enabled = true, @user_policy = nil, @add_current_working_directory = nil,
                   @sandbox_mcp_servers = nil, @sandbox_lsp_servers = nil,
                   @sandbox_extensions = nil, @allow_bypass = nil)
    end
  end

  # Session image generation configuration (`tools.imageGeneration`).
  class ImageGenerationConfig
    include JSON::Serializable

    property enabled : Bool?
    # Request a visible "Made with AI" label on generated images.
    property watermark : Bool?
    # Save generated images under the session-state `images` folder.
    @[JSON::Field(key: "saveToDisk")]
    property save_to_disk : Bool?

    def initialize(@enabled = nil, @watermark = nil, @save_to_disk = nil)
    end
  end

  # Autopilot continuation options (`execution.autopilot`).
  class AutopilotOptions
    include JSON::Serializable

    @[JSON::Field(key: "continuationEnabled")]
    property continuation_enabled : Bool?
    @[JSON::Field(key: "maxContinues")]
    property max_continues : Int32?

    def initialize(@continuation_enabled = nil, @max_continues = nil)
    end
  end

  # Model capability limit overrides. Wire keys are snake_case; for example
  # `max_output_tokens` is sent as `max_output_tokens`.
  class ModelLimitsOverride
    include JSON::Serializable

    @[JSON::Field(key: "max_prompt_tokens")]
    property max_prompt_tokens : Int64?
    @[JSON::Field(key: "max_output_tokens")]
    property max_output_tokens : Int64?
    @[JSON::Field(key: "max_context_window_tokens")]
    property max_context_window_tokens : Int64?

    def initialize(@max_prompt_tokens = nil, @max_output_tokens = nil,
                   @max_context_window_tokens = nil)
    end
  end

  # Typed builder for `SessionConfig#model_capabilities` (`inference.capabilities`).
  class ModelCapabilitiesOverride
    include JSON::Serializable

    # Feature support overrides, for example `{"vision" => true}`.
    property supports : Hash(String, Bool)?
    property limits : ModelLimitsOverride?

    def initialize(@supports = nil, @limits = nil)
    end

    # The override as a `model_capabilities` hash.
    def to_h : Hash(String, JSON::Any)
      JSON.parse(to_json).as_h
    end
  end

  # Legacy single BYOK provider (`SessionConfig#provider`). Protocol 4 sends it
  # as a `"default"` named provider with Copilot API models disabled; model
  # fields become an `inference.models` entry for the session model.
  class ProviderConfig
    include JSON::Serializable

    property type : String?
    @[JSON::Field(key: "baseUrl")]
    property base_url : String
    @[JSON::Field(key: "apiKey")]
    property api_key : String?
    # Command run before each request to obtain a fresh API key.
    @[JSON::Field(key: "apiKeyCommand")]
    property api_key_command : String?
    @[JSON::Field(key: "bearerToken")]
    property bearer_token : String?
    @[JSON::Field(key: "wireApi")]
    property wire_api : String?
    # `"http"` (default) or `"websockets"`.
    property transport : String?
    property azure : JSON::Any?
    property headers : Hash(String, String)?
    @[JSON::Field(key: "modelProvider")]
    property model_provider : String?
    @[JSON::Field(key: "modelId")]
    property model_id : String?
    @[JSON::Field(key: "wireModel")]
    property wire_model : String?
    @[JSON::Field(key: "maxPromptTokens")]
    property max_prompt_tokens : Int64?
    @[JSON::Field(key: "maxContextWindowTokens")]
    property max_context_window_tokens : Int64?
    @[JSON::Field(key: "maxOutputTokens")]
    property max_output_tokens : Int64?

    def initialize(@base_url, @type = nil, @api_key = nil, @api_key_command = nil,
                   @bearer_token = nil, @wire_api = nil, @transport = nil, @azure = nil,
                   @headers = nil, @model_provider = nil, @model_id = nil, @wire_model = nil,
                   @max_prompt_tokens = nil, @max_context_window_tokens = nil,
                   @max_output_tokens = nil)
    end
  end

  # A named BYOK provider connection (`inference.providers`).
  class NamedProviderConfig
    include JSON::Serializable

    # Stable identifier referenced by `ProviderModelConfig#provider`.
    property name : String
    property type : String?
    @[JSON::Field(key: "baseUrl")]
    property base_url : String
    @[JSON::Field(key: "apiKey")]
    property api_key : String?
    @[JSON::Field(key: "apiKeyCommand")]
    property api_key_command : String?
    @[JSON::Field(key: "bearerToken")]
    property bearer_token : String?
    @[JSON::Field(key: "wireApi")]
    property wire_api : String?
    property transport : String?
    property azure : JSON::Any?
    property headers : Hash(String, String)?
    @[JSON::Field(key: "modelProvider")]
    property model_provider : String?

    def initialize(@name, @base_url, @type = nil, @api_key = nil, @api_key_command = nil,
                   @bearer_token = nil, @wire_api = nil, @transport = nil, @azure = nil,
                   @headers = nil, @model_provider = nil)
    end
  end

  # A BYOK model definition (`inference.models`) served by a named provider.
  class ProviderModelConfig
    include JSON::Serializable

    # Name of the `NamedProviderConfig` serving this model.
    property provider : String
    # Provider-local model id; select it with "<provider>/<id>".
    property id : String
    @[JSON::Field(key: "modelId")]
    property model_id : String?
    @[JSON::Field(key: "wireModel")]
    property wire_model : String?
    property name : String?
    @[JSON::Field(key: "maxPromptTokens")]
    property max_prompt_tokens : Int64?
    @[JSON::Field(key: "maxContextWindowTokens")]
    property max_context_window_tokens : Int64?
    @[JSON::Field(key: "maxOutputTokens")]
    property max_output_tokens : Int64?
    property capabilities : ModelCapabilitiesOverride?

    def initialize(@provider, @id, @model_id = nil, @wire_model = nil, @name = nil,
                   @max_prompt_tokens = nil, @max_context_window_tokens = nil,
                   @max_output_tokens = nil, @capabilities = nil)
    end
  end

  # Copilot API (CAPI) options (`github.capi`).
  class CapiSessionOptions
    include JSON::Serializable

    # Enables Copilot API models; `false` for BYOK-only sessions.
    property enabled : Bool?
    # Auto routing preference (see `AutoTier`) used when the session model is `auto`.
    @[JSON::Field(key: "autoTier")]
    property auto_tier : String?
    # Use the WebSocket transport for the Responses API.
    @[JSON::Field(key: "enableWebSocketResponses")]
    property enable_web_socket_responses : Bool?

    def initialize(@enabled = nil, @auto_tier = nil, @enable_web_socket_responses = nil)
    end
  end

  # A password or other answer a shell command run by the agent asks for
  # (`shellAuthentication.request`). The answer goes only to that program.
  class ShellAuthenticationRequest
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String = ""
    # Identifies the request; a `shellAuthentication.cancel` carries it too.
    @[JSON::Field(key: "requestId")]
    property request_id : String = ""
    # The program that asked: `sudo`, `ssh`, or `git`.
    property program : String = ""
    property prompt : String = ""
    # `"secret"` (mask the input), `"text"`, or `"confirm"` (yes/no).
    @[JSON::Field(key: "inputKind")]
    property input_kind : String = "secret"
    property command : String = ""
    property cwd : String = ""

    def initialize(@session_id = "", @request_id = "", @program = "", @prompt = "",
                   @input_kind = "secret", @command = "", @cwd = "")
    end
  end

  # Answer to a `ShellAuthenticationRequest`.
  class ShellAuthenticationResponse
    # `"accept"` or `"cancel"`.
    getter action : String
    getter value : String?

    def initialize(@action : String, @value : String? = nil)
    end

    # Answers the prompt with `value` (use `"yes"` for a confirm prompt).
    def self.accept(value : String) : self
      new("accept", value)
    end

    # Declines the prompt.
    def self.cancel : self
      new("cancel")
    end

    def to_json_any : JSON::Any
      wire = {"action" => JSON::Any.new(@action)}
      if @action == "accept"
        wire["value"] = JSON::Any.new(@value || "")
      end
      JSON::Any.new(wire)
    end
  end

  # Answers shell authentication prompts. The answer is ignored when the
  # runtime cancels the request first (`shellAuthentication.cancel`).
  alias ShellAuthenticationHandler = Proc(ShellAuthenticationRequest, ShellAuthenticationResponse)

  # A memory the agent asked to store (`memoryStore.store`), after validation,
  # secret filtering and approval.
  class StoredMemory
    include JSON::Serializable

    @[JSON::Field(key: "sessionId")]
    property session_id : String = ""
    property subject : String = ""
    property fact : String = ""
    property citations : String = ""
    property reason : String = ""

    def initialize(@session_id = "", @subject = "", @fact = "", @citations = "", @reason = "")
    end
  end

  # Stores a memory in the app. The agent's `store_memory` succeeds once this returns.
  alias MemoryStoreHandler = Proc(StoredMemory, Nil)

  # Request for `network.authenticateBasic`.
  class NetworkAuthenticateBasicRequest
    # Proxy URL that issued the Basic challenge.
    getter proxy : String
    # Realm from the proxy (untrusted text).
    getter realm : String?
    # True when the proxy rejected the previously supplied credentials.
    getter previous_rejected : Bool

    def initialize(@proxy : String, @realm : String?, @previous_rejected : Bool)
    end
  end

  # Credentials returned for `network.authenticateBasic`.
  class NetworkBasicCredentials
    getter username : String?
    getter password : String?

    def initialize(@username : String? = nil, @password : String? = nil)
    end
  end

  # Network callbacks through which the app decides the runtime's proxies and
  # supplies proxy credentials. The SDK never logs returned tokens or passwords.
  class NetworkConfig
    # `"connection"` (default) or `"runtime"`.
    property scope : String?
    # Returns a PAC-style proxy list for a URL (`DIRECT` for none), or nil
    # (`network.resolveProxy`).
    property resolve_proxy : Proc(String, String?)?
    # Returns a base64 SPNEGO token for a service principal name, or nil
    # (`network.authenticateNegotiate`).
    property authenticate_negotiate : Proc(String, String?)?
    # Returns Basic credentials for a proxy, or nil (`network.authenticateBasic`).
    property authenticate_basic : Proc(NetworkAuthenticateBasicRequest, NetworkBasicCredentials?)?

    def initialize(@scope = nil, @resolve_proxy = nil, @authenticate_negotiate = nil,
                   @authenticate_basic = nil)
    end

    # True when at least one callback is set.
    def has_callbacks? : Bool
      !@resolve_proxy.nil? || !@authenticate_negotiate.nil? || !@authenticate_basic.nil?
    end
  end

  # Session event type names.
  module SessionEventTypes
    # Live, append-only shell output of a running tool; not persisted.
    TOOL_SHELL_OUTPUT = "tool.shell_output"
    # Removed upstream; still tolerated when received.
    TOOL_EXECUTION_PARTIAL_RESULT = "tool.execution_partial_result"
  end

  # Data of a `tool.shell_output` event.
  class ToolShellOutputData
    getter tool_call_id : String
    getter text : String
    # Zero-based publication sequence across the call's output streams.
    getter sequence : Int64
    # `"stdout"`, `"stderr"`, or nil (stdout).
    getter stream : String?

    def initialize(@tool_call_id : String, @text : String, @sequence : Int64,
                   @stream : String? = nil)
    end

    # Parses the data of a `tool.shell_output` event; nil for other event types.
    def self.from_event(event : SessionEvent) : ToolShellOutputData?
      return nil unless event.type == SessionEventTypes::TOOL_SHELL_OUTPUT
      data = event.data.try(&.as_h?) || {} of String => JSON::Any
      new(
        tool_call_id: data["toolCallId"]?.try(&.as_s?) || "",
        text: data["text"]?.try(&.as_s?) || "",
        sequence: data["sequence"]?.try(&.as_i64?) || 0_i64,
        stream: data["stream"]?.try(&.as_s?)
      )
    end
  end

  # A field-specific finding reported by a Policy Helper.
  class PolicyHelperFailureDetail
    include JSON::Serializable

    property message : String = ""
    property path : String?

    def initialize(@message = "", @path = nil)
    end
  end

  # The latest unresolved Policy Helper failure. Exposed from the JSON-RPC
  # error data (`error.data.policyHelperFailure`) when `session.create` /
  # `session.resume` fails (see `JsonRpcError#policy_helper_failure`). Helper
  # text is untrusted: render it as plain text.
  class PolicyHelperFailure
    include JSON::Serializable

    property category : String = ""
    @[JSON::Field(key: "failureMode")]
    property failure_mode : String = ""
    property message : String = ""
    @[JSON::Field(key: "previousPolicyRetained")]
    property previous_policy_retained : Bool = false
    property reason : String = ""
    property authority : String?
    property code : String?
    property details : Array(PolicyHelperFailureDetail)?
    property diagnostic : String?
    # Validated additional-information link (`{url, label?}`), if any.
    @[JSON::Field(key: "helpLink")]
    property help_link : JSON::Any?

    def initialize(@category = "", @failure_mode = "", @message = "",
                   @previous_policy_retained = false, @reason = "", @authority = nil,
                   @code = nil, @details = nil, @diagnostic = nil, @help_link = nil)
    end
  end

  # Known refusal reasons on a `PermissionModeSetResult`.
  module PermissionModeRefusalReason
    # Managed (enterprise) policy refused the requested permission mode.
    MANAGED_POLICY = "managed_policy"
  end

  # Result of `CopilotSession#set_permission_mode` (`session.permissions.setMode`).
  class PermissionModeSetResult
    include JSON::Serializable

    # Whether the requested mode was applied.
    property success : Bool = false
    # Authoritative permission mode after the call.
    property mode : String = ""
    # Why the mode was refused, when known (see `PermissionModeRefusalReason`).
    property reason : String?

    def initialize(@success = false, @mode = "", @reason = nil)
    end
  end

  # Input for the `subagentStart` hook, fired before a sub-agent's first turn.
  class SubagentStartHookInput
    include JSON::Serializable

    property timestamp : Int64 = 0_i64
    property cwd : String = ""
    @[JSON::Field(key: "transcriptPath")]
    property transcript_path : String = ""
    @[JSON::Field(key: "agentName")]
    property agent_name : String = ""
    @[JSON::Field(key: "agentDisplayName")]
    property agent_display_name : String?
    @[JSON::Field(key: "agentDescription")]
    property agent_description : String?
  end

  # Output for the `subagentStart` hook.
  class SubagentStartHookOutput
    include JSON::Serializable

    # Context prepended to the sub-agent's initial prompt.
    @[JSON::Field(key: "additionalContext")]
    property additional_context : String?

    def initialize(@additional_context = nil)
    end
  end

  # Input for the `subagentStop` hook, fired after a sub-agent completes a turn.
  class SubagentStopHookInput
    include JSON::Serializable

    property timestamp : Int64 = 0_i64
    property cwd : String = ""
    @[JSON::Field(key: "transcriptPath")]
    property transcript_path : String = ""
    @[JSON::Field(key: "agentName")]
    property agent_name : String = ""
    @[JSON::Field(key: "agentDisplayName")]
    property agent_display_name : String?
    @[JSON::Field(key: "agentDescription")]
    property agent_description : String?
    @[JSON::Field(key: "agentId")]
    property agent_id : String?
    @[JSON::Field(key: "agentType")]
    property agent_type : String = ""
    # Why the sub-agent stopped (currently always `"end_turn"`).
    @[JSON::Field(key: "stopReason")]
    property stop_reason : String = "end_turn"
    # The sub-agent's last assistant message before any hook rewrite.
    property response : String = ""
  end

  # Output for the `subagentStop` hook. `decision: "block"` with a non-empty
  # `reason` continues the sub-agent; otherwise `modified_response` replaces
  # the response reported to the parent agent.
  class SubagentStopHookOutput
    include JSON::Serializable

    # `"block"` or `"allow"`.
    property decision : String?
    property reason : String?
    @[JSON::Field(key: "modifiedResponse")]
    property modified_response : String?

    def initialize(@decision = nil, @reason = nil, @modified_response = nil)
    end
  end

  # Handler for a hook delivered through `hooks.invoke`: receives the hook input
  # and the session ID and returns the hook output (or nil).
  alias HookHandler = Proc(JSON::Any, String, JSON::Any?)

  # Handler for the `subagentStart` hook.
  alias SubagentStartHandler = Proc(SubagentStartHookInput, String, SubagentStartHookOutput?)

  # Handler for the `subagentStop` hook.
  alias SubagentStopHandler = Proc(SubagentStopHookInput, String, SubagentStopHookOutput?)

  # Session hook handlers, answered through the `hooks.invoke` server request.
  class SessionHooks
    # Raw handlers keyed by hook wire name (see `HookType#to_wire`).
    getter handlers : Hash(String, HookHandler)
    # Invoked before a sub-agent's first turn (`subagentStart`).
    property on_subagent_start : SubagentStartHandler?
    # Invoked after a sub-agent completes a turn (`subagentStop`).
    property on_subagent_stop : SubagentStopHandler?

    def initialize(@on_subagent_start = nil, @on_subagent_stop = nil)
      @handlers = {} of String => HookHandler
    end

    # Registers a raw handler for a hook type.
    def on(hook_type : HookType, &handler : JSON::Any, String -> JSON::Any?) : Nil
      @handlers[hook_type.to_wire] = handler
    end

    # True when any handler is registered.
    def any? : Bool
      !@handlers.empty? || !@on_subagent_start.nil? || !@on_subagent_stop.nil?
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

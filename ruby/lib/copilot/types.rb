# frozen_string_literal: true

# Copyright (c) Microsoft Corporation. All rights reserved.

require "json"

module Copilot
  # Connection state of the client.
  # Valid values: "disconnected", "connecting", "connected", "error"
  module ConnectionState
    DISCONNECTED = "disconnected"
    CONNECTING   = "connecting"
    CONNECTED    = "connected"
    ERROR        = "error"
  end

  # Tool result type constants.
  module ToolResultType
    SUCCESS  = "success"
    FAILURE  = "failure"
    REJECTED = "rejected"
    DENIED   = "denied"
  end

  # A binary payload returned by a tool.
  ToolBinaryResult = Struct.new(
    :data, :mime_type, :type, :description,
    keyword_init: true
  ) do
    def to_h
      h = { data: data, mimeType: mime_type, type: type }
      h[:description] = description if description
      h
    end
  end

  # The result of a tool invocation.
  # Can be constructed directly or returned as a plain Hash / String from a handler.
  ToolResult = Struct.new(
    :text_result_for_llm, :result_type,
    :binary_results_for_llm, :error, :session_log, :tool_telemetry,
    keyword_init: true
  ) do
    def to_h
      h = {
        textResultForLlm: text_result_for_llm,
        resultType: result_type || ToolResultType::SUCCESS,
      }
      h[:binaryResultsForLlm] = binary_results_for_llm.map(&:to_h) if binary_results_for_llm
      h[:error] = error if error
      h[:sessionLog] = session_log if session_log
      h[:toolTelemetry] = tool_telemetry || {}
      h
    end
  end

  # Context provided to a tool handler when invoked.
  ToolInvocation = Struct.new(
    :session_id, :tool_call_id, :tool_name, :arguments,
    keyword_init: true
  )

  # A tool definition. +handler+ is a callable (Proc/lambda/method) that receives
  # (arguments, invocation) and returns a result (String, Hash, or ToolResult).
  # Set +idempotent+ to true when repeating a call with the same arguments has no
  # additional effect, so the runtime may safely retry it.
  Tool = Struct.new(
    :name, :description, :parameters, :handler, :idempotent,
    keyword_init: true
  ) do
    # @return [Hash] wire-format representation sent to the server
    def to_wire
      h = { name: name }
      h[:description] = description if description
      h[:parameters] = parameters if parameters
      h[:idempotent] = idempotent unless idempotent.nil?
      h
    end
  end

  # System message in append mode (default).
  SystemMessageAppendConfig = Struct.new(:mode, :content, keyword_init: true) do
    def to_h
      h = {}
      h[:mode] = mode || "append"
      h[:content] = content if content
      h
    end
  end

  # System message in replace mode - caller provides entire system message.
  SystemMessageReplaceConfig = Struct.new(:content, keyword_init: true) do
    def to_h
      { mode: "replace", content: content }
    end
  end

  # Known system prompt section identifiers for "customize" mode.
  module SystemPromptSection
    IDENTITY             = "identity"
    TONE                 = "tone"
    TOOL_EFFICIENCY      = "tool_efficiency"
    ENVIRONMENT_CONTEXT  = "environment_context"
    CODE_CHANGE_RULES    = "code_change_rules"
    GUIDELINES           = "guidelines"
    SAFETY               = "safety"
    TOOL_INSTRUCTIONS    = "tool_instructions"
    CUSTOM_INSTRUCTIONS  = "custom_instructions"
    LAST_INSTRUCTIONS    = "last_instructions"
  end

  # Override action for a system prompt section.
  module SectionOverrideAction
    REPLACE = "replace"
    REMOVE  = "remove"
    APPEND  = "append"
    PREPEND = "prepend"
  end

  # Override operation for a single system prompt section.
  SectionOverride = Struct.new(:action, :content, keyword_init: true) do
    def to_h
      h = { action: action }
      h[:content] = content if content
      h
    end
  end

  # System message in customize mode - section-level overrides.
  SystemMessageCustomizeConfig = Struct.new(:sections, :content, keyword_init: true) do
    def to_h
      h = { mode: "customize" }
      if sections
        h[:sections] = sections.transform_values(&:to_h)
      end
      h[:content] = content if content
      h
    end
  end

  # Permission request from the server.
  PermissionRequest = Struct.new(:kind, :tool_call_id, :extra, keyword_init: true) do
    def self.from_hash(h)
      new(
        kind: h["kind"],
        tool_call_id: h["toolCallId"],
        extra: h.reject { |k, _| %w[kind toolCallId].include?(k) }
      )
    end
  end

  # Result of a permission request.
  PermissionRequestResult = Struct.new(:kind, :rules, keyword_init: true) do
    def to_h
      h = { kind: kind }
      h[:rules] = rules if rules
      h
    end
  end

  # Permission result kind constants.
  module PermissionKind
    APPROVED = "approved"
    DENIED_BY_RULES = "denied-by-rules"
    DENIED_NO_APPROVAL = "denied-no-approval-rule-and-could-not-request-from-user"
    DENIED_BY_USER = "denied-interactively-by-user"
  end

  # Request for user input from the agent (enables ask_user tool).
  UserInputRequest = Struct.new(:question, :choices, :allow_freeform, keyword_init: true) do
    def self.from_hash(h)
      new(
        question: h["question"],
        choices: h["choices"],
        allow_freeform: h.fetch("allowFreeform", true)
      )
    end
  end

  # Response to a user input request.
  UserInputResponse = Struct.new(:answer, :was_freeform, keyword_init: true) do
    def to_h
      { answer: answer, wasFreeform: was_freeform }
    end
  end

  # Custom API provider configuration (BYOK).
  #
  # +model_provider+ selects the provider family (see {ModelProvider}) and
  # +api_key_command+ is a command whose stdout is used as the API key.
  ProviderConfig = Struct.new(
    :type, :wire_api, :base_url, :api_key, :bearer_token, :azure,
    :model_provider, :api_key_command,
    keyword_init: true
  ) do
    def to_wire
      h = {}
      h[:type] = type if type
      h[:wireApi] = wire_api if wire_api
      h[:baseUrl] = base_url if base_url
      h[:apiKey] = api_key if api_key
      h[:bearerToken] = bearer_token if bearer_token
      h[:modelProvider] = model_provider if model_provider
      h[:apiKeyCommand] = api_key_command if api_key_command
      if azure
        az = {}
        az[:apiVersion] = azure[:api_version] if azure[:api_version]
        h[:azure] = az unless az.empty?
      end
      h
    end
  end

  # MCP local/stdio server configuration.
  MCPLocalServerConfig = Struct.new(
    :tools, :type, :timeout, :command, :args, :env, :cwd,
    keyword_init: true
  ) do
    def to_h
      h = { tools: tools || [], command: command, args: args || [] }
      h[:type] = type if type
      h[:timeout] = timeout if timeout
      h[:env] = env if env
      h[:cwd] = cwd if cwd
      h
    end
  end

  # MCP remote server configuration (HTTP or SSE).
  MCPRemoteServerConfig = Struct.new(
    :tools, :type, :timeout, :url, :headers,
    keyword_init: true
  ) do
    def to_h
      h = { tools: tools || [], type: type, url: url }
      h[:timeout] = timeout if timeout
      h[:headers] = headers if headers
      h
    end
  end

  # Custom agent configuration.
  CustomAgentConfig = Struct.new(
    :name, :display_name, :description, :tools, :prompt, :mcp_servers, :infer,
    :skills,
    keyword_init: true
  ) do
    def to_wire
      h = { name: name, prompt: prompt }
      h[:displayName] = display_name if display_name
      h[:description] = description if description
      h[:tools] = tools if tools
      h[:mcpServers] = mcp_servers if mcp_servers
      h[:infer] = infer unless infer.nil?
      h[:skills] = skills if skills
      h
    end
  end

  # Infinite session configuration for automatic context compaction and workspace persistence.
  InfiniteSessionConfig = Struct.new(
    :enabled, :background_compaction_threshold, :buffer_exhaustion_threshold,
    keyword_init: true
  ) do
    def to_wire
      h = {}
      h[:enabled] = enabled unless enabled.nil?
      h[:backgroundCompactionThreshold] = background_compaction_threshold if background_compaction_threshold
      h[:bufferExhaustionThreshold] = buffer_exhaustion_threshold if buffer_exhaustion_threshold
      h
    end
  end

  # Context for a slash-command invocation.
  CommandContext = Struct.new(:session_id, :command, :command_name, :args, keyword_init: true)

  # Definition of a slash command registered with the session.
  # handler is a callable (Proc/lambda) that receives a CommandContext.
  CommandDefinition = Struct.new(:name, :description, :handler, keyword_init: true)

  # Context for an elicitation request from the server.
  ElicitationContext = Struct.new(
    :session_id, :message, :requested_schema, :mode, :elicitation_source, :url,
    keyword_init: true
  )

  # Result returned from an elicitation handler.
  ElicitationResult = Struct.new(:action, :content, keyword_init: true)

  # Configuration for creating a session.
  SessionConfig = Struct.new(
    :session_id, :model, :reasoning_effort, :config_dir,
    :tools, :system_message, :available_tools, :excluded_tools,
    :provider, :on_permission_request, :on_user_input_request, :hooks,
    :working_directory, :streaming, :include_sub_agent_streaming_events,
    :mcp_servers, :custom_agents,
    :skill_directories, :disabled_skills, :infinite_sessions,
    :model_capabilities, :enable_config_discovery,
    :github_token,
    :commands, :on_elicitation_request,
    :instruction_directories,
    # v2.6.0 upstream-sync options
    :name, :auto_tier, :ask_user_variant, :github_token_provider,
    :sandbox_config, :feature_flags, :allowed_models, :auth_client_id_metadata_url,
    :agent_directories, :hook_directories,
    :interactive_user, :excluded_managed_settings, :enforce_managed_model_defaults,
    :trust_working_directory, :refresh_custom_instructions,
    :image_generation, :autopilot,
    # v3.0.0 (SDK protocol 4) options
    :providers, :models, :default_provider, :capi,
    :on_shell_authentication_request, :store_memory, :permission_mode,
    keyword_init: true
  )

  # Configuration for resuming a session. Same fields as SessionConfig plus disable_resume
  # (create-only fields such as +name+ are omitted; +allow_transcript_recovery+ is resume-only).
  ResumeSessionConfig = Struct.new(
    :model, :reasoning_effort, :config_dir,
    :tools, :system_message, :available_tools, :excluded_tools,
    :provider, :on_permission_request, :on_user_input_request, :hooks,
    :working_directory, :streaming, :include_sub_agent_streaming_events,
    :mcp_servers, :custom_agents,
    :skill_directories, :disabled_skills, :infinite_sessions,
    :model_capabilities, :enable_config_discovery,
    :github_token,
    :commands, :on_elicitation_request,
    :instruction_directories,
    :disable_resume,
    # v2.6.0 upstream-sync options
    :auto_tier, :ask_user_variant, :github_token_provider,
    :sandbox_config, :feature_flags, :allowed_models, :auth_client_id_metadata_url,
    :agent_directories, :hook_directories,
    :interactive_user, :excluded_managed_settings, :enforce_managed_model_defaults,
    :allow_transcript_recovery,
    :image_generation, :autopilot,
    # v3.0.0 (SDK protocol 4) options; an explicit +default_provider: nil+ clears the
    # session's default provider
    :providers, :models, :default_provider, :capi,
    :on_shell_authentication_request, :store_memory, :permission_mode,
    keyword_init: true
  )

  # Response format for message responses.
  RESPONSE_FORMATS = %w[text image json_object].freeze

  # Options for image generation.
  ImageOptions = Struct.new(:size, :quality, :style, keyword_init: true) do
    def to_h
      super.compact
    end
  end

  # Image data from an assistant image response.
  AssistantImageData = Struct.new(:format, :base64, :url, :revised_prompt, :width, :height, keyword_init: true)

  # A content block in a mixed text+image response.
  ContentBlock = Struct.new(:type, :text, :image, keyword_init: true)

  # Options for sending a message.
  #
  # +source+ is the message provenance (see {MessageSource}); +response_schema+ is a
  # JSON Schema Hash requesting structured output for this run; +client_turn_id+ is an
  # opaque caller-chosen identifier echoed on the turn's events.
  MessageOptions = Struct.new(
    :prompt, :attachments, :mode, :response_format, :image_options, :request_headers,
    :agent_mode, :display_prompt, :source, :response_schema, :client_turn_id,
    keyword_init: true
  )

  # Ping response from the server.
  PingResponse = Struct.new(:message, :timestamp, :protocol_version, keyword_init: true) do
    def self.from_hash(h)
      new(
        message: h["message"],
        timestamp: h["timestamp"],
        protocol_version: h["protocolVersion"]
      )
    end
  end

  # Response from status.get.
  GetStatusResponse = Struct.new(:version, :protocol_version, keyword_init: true) do
    def self.from_hash(h)
      new(version: h["version"], protocol_version: h["protocolVersion"])
    end
  end

  # Response from auth.getStatus.
  GetAuthStatusResponse = Struct.new(
    :is_authenticated, :auth_type, :host, :login, :status_message,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        is_authenticated: h["isAuthenticated"],
        auth_type: h["authType"],
        host: h["host"],
        login: h["login"],
        status_message: h["statusMessage"]
      )
    end
  end

  # Model support flags.
  ModelSupports = Struct.new(:vision, :reasoning_effort, keyword_init: true) do
    def self.from_hash(h)
      new(vision: h["vision"], reasoning_effort: h["reasoningEffort"])
    end
  end

  # Model vision limits.
  ModelVisionLimits = Struct.new(
    :supported_media_types, :max_prompt_images, :max_prompt_image_size,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        supported_media_types: h["supported_media_types"],
        max_prompt_images: h["max_prompt_images"],
        max_prompt_image_size: h["max_prompt_image_size"]
      )
    end
  end

  # Model limits.
  ModelLimits = Struct.new(
    :max_prompt_tokens, :max_context_window_tokens, :vision, :max_output_tokens,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        max_prompt_tokens: h["max_prompt_tokens"],
        max_context_window_tokens: h["max_context_window_tokens"],
        vision: h["vision"] ? ModelVisionLimits.from_hash(h["vision"]) : nil,
        max_output_tokens: h["max_output_tokens"]
      )
    end
  end

  # Model capabilities and limits.
  ModelCapabilities = Struct.new(:supports, :limits, keyword_init: true) do
    def self.from_hash(h)
      new(
        supports: ModelSupports.from_hash(h["supports"] || {}),
        limits: ModelLimits.from_hash(h["limits"] || {})
      )
    end
  end

  # Model policy state.
  ModelPolicy = Struct.new(:state, :terms, keyword_init: true) do
    def self.from_hash(h)
      new(state: h["state"], terms: h["terms"])
    end
  end

  # Model billing information.
  ModelBilling = Struct.new(:multiplier, keyword_init: true) do
    def self.from_hash(h)
      new(multiplier: h["multiplier"])
    end
  end

  # Information about an available model.
  ModelInfo = Struct.new(
    :id, :name, :capabilities, :policy, :billing,
    :supported_reasoning_efforts, :default_reasoning_effort,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        id: h["id"],
        name: h["name"],
        capabilities: ModelCapabilities.from_hash(h["capabilities"] || {}),
        policy: h["policy"] ? ModelPolicy.from_hash(h["policy"]) : nil,
        billing: h["billing"] ? ModelBilling.from_hash(h["billing"]) : nil,
        supported_reasoning_efforts: h["supportedReasoningEfforts"],
        default_reasoning_effort: h["defaultReasoningEffort"]
      )
    end
  end

  # Metadata about a session.
  SessionMetadata = Struct.new(
    :session_id, :start_time, :modified_time, :summary, :is_remote,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        session_id: h["sessionId"],
        start_time: h["startTime"],
        modified_time: h["modifiedTime"],
        summary: h["summary"],
        is_remote: h["isRemote"]
      )
    end
  end

  # Session event from the server.
  # Uses an open struct-style approach so new event types are forward-compatible.
  SessionEvent = Struct.new(:id, :timestamp, :parent_id, :agent_id, :ephemeral, :type, :data, keyword_init: true) do
    def self.from_hash(h)
      new(
        id: h["id"],
        timestamp: h["timestamp"],
        parent_id: h["parentId"],
        agent_id: h["agentId"],
        ephemeral: h["ephemeral"],
        type: h["type"],
        data: h["data"] || {}
      )
    end
  end

  # Session event type constants.
  module SessionEventType
    SESSION_START               = "session.start"
    SESSION_RESUME              = "session.resume"
    SESSION_ERROR               = "session.error"
    SESSION_IDLE                = "session.idle"
    SESSION_INFO                = "session.info"
    SESSION_MODEL_CHANGE        = "session.model_change"
    SESSION_HANDOFF             = "session.handoff"
    SESSION_TRUNCATION          = "session.truncation"
    SESSION_SNAPSHOT_REWIND     = "session.snapshot_rewind"
    SESSION_SHUTDOWN            = "session.shutdown"
    SESSION_USAGE_INFO          = "session.usage_info"
    SESSION_COMPACTION_START    = "session.compaction_start"
    SESSION_COMPACTION_COMPLETE = "session.compaction_complete"
    USER_MESSAGE                = "user.message"
    PENDING_MESSAGES_MODIFIED   = "pending_messages.modified"
    ASSISTANT_TURN_START        = "assistant.turn_start"
    ASSISTANT_INTENT            = "assistant.intent"
    ASSISTANT_REASONING         = "assistant.reasoning"
    ASSISTANT_REASONING_DELTA   = "assistant.reasoning_delta"
    ASSISTANT_MESSAGE           = "assistant.message"
    ASSISTANT_MESSAGE_DELTA     = "assistant.message_delta"
    ASSISTANT_TURN_END          = "assistant.turn_end"
    ASSISTANT_USAGE             = "assistant.usage"
    ABORT                       = "abort"
    TOOL_USER_REQUESTED         = "tool.user_requested"
    TOOL_EXECUTION_START        = "tool.execution_start"
    TOOL_EXECUTION_PARTIAL      = "tool.execution_partial_result"
    TOOL_EXECUTION_PROGRESS     = "tool.execution_progress"
    TOOL_EXECUTION_COMPLETE     = "tool.execution_complete"
    TOOL_SHELL_OUTPUT           = "tool.shell_output"
    SKILL_INVOKED               = "skill.invoked"
    SUBAGENT_STARTED            = "subagent.started"
    SUBAGENT_COMPLETED          = "subagent.completed"
    SUBAGENT_FAILED             = "subagent.failed"
    SUBAGENT_SELECTED           = "subagent.selected"
    HOOK_START                  = "hook.start"
    HOOK_END                    = "hook.end"
    SYSTEM_MESSAGE              = "system.message"
  end

  # Session lifecycle event types.
  module SessionLifecycleEventType
    SESSION_CREATED    = "session.created"
    SESSION_DELETED    = "session.deleted"
    SESSION_UPDATED    = "session.updated"
    SESSION_FOREGROUND = "session.foreground"
    SESSION_BACKGROUND = "session.background"
  end

  # Session lifecycle event notification.
  SessionLifecycleEvent = Struct.new(:type, :session_id, :metadata, keyword_init: true) do
    def self.from_hash(h)
      new(
        type: h["type"],
        session_id: h["sessionId"],
        metadata: h["metadata"]
      )
    end
  end

  # Session lifecycle event metadata.
  SessionLifecycleMetadata = Struct.new(:start_time, :modified_time, :summary, keyword_init: true) do
    def self.from_hash(h)
      new(
        start_time: h["startTime"],
        modified_time: h["modifiedTime"],
        summary: h["summary"]
      )
    end
  end

  # Foreground session info (TUI+server mode).
  ForegroundSessionInfo = Struct.new(:session_id, :workspace_path, keyword_init: true)

  # Error during client stop.
  StopError = Struct.new(:message, keyword_init: true)

  # Hook configuration for a session. Each value is a callable or nil.
  #
  # +on_subagent_start+ (hook type "subagentStart") receives the base hook fields plus
  # transcriptPath, agentName, agentDisplayName, agentDescription and may return
  # { additionalContext: }. +on_subagent_stop+ (hook type "subagentStop") also receives
  # agentId, agentType, stopReason ("end_turn") and response, and may return
  # { decision: "block" | "allow", reason:, modifiedResponse: }.
  SessionHooks = Struct.new(
    :on_pre_tool_use, :on_post_tool_use, :on_user_prompt_submitted,
    :on_user_prompt_transformed,
    :on_session_start, :on_session_end, :on_error_occurred,
    :on_subagent_start, :on_subagent_stop,
    keyword_init: true
  ) do
    def any_handler?
      [on_pre_tool_use, on_post_tool_use, on_user_prompt_submitted,
       on_user_prompt_transformed,
       on_session_start, on_session_end, on_error_occurred,
       on_subagent_start, on_subagent_stop].any?
    end
  end

  # Configuration for a custom session filesystem provider.
  SessionFsConfig = Struct.new(
    :initial_cwd, :session_state_path, :conventions,
    keyword_init: true
  )

  # File metadata returned by session filesystem operations.
  SessionFsFileInfo = Struct.new(
    :name, :size, :is_directory, :is_file, :created_at, :modified_at,
    keyword_init: true
  )

  # Interface for session filesystem providers.
  # Implementors should include this module and define all methods.
  module SessionFsProvider
    def read_file(session_id, path) raise NotImplementedError end
    def write_file(session_id, path, content) raise NotImplementedError end
    def append_file(session_id, path, content) raise NotImplementedError end
    def exists?(session_id, path) raise NotImplementedError end
    def stat(session_id, path) raise NotImplementedError end
    def mkdir(session_id, path, recursive: false) raise NotImplementedError end
    def readdir(session_id, path) raise NotImplementedError end
    def readdir_with_types(session_id, path) raise NotImplementedError end
    def rm(session_id, path, recursive: false) raise NotImplementedError end
    def rename(session_id, old_path, new_path) raise NotImplementedError end
  end

  # Client options.
  ClientOptions = Struct.new(
    :cli_path, :cli_args, :cwd, :port, :use_stdio, :cli_url,
    :log_level, :auto_start, :auto_restart, :env,
    :github_token, :use_logged_in_user, :session_idle_timeout_seconds,
    :session_fs,
    :copilot_home, :tcp_connection_token,
    :request_handler,
    :on_get_trace_context,
    :client_info,
    :mode, :network,
    keyword_init: true
  )

  # --- Upstream-sync feature types (parity with @github/copilot-sdk) ---

  # Per-session AI-credit budget; set +max_ai_credits+ to cap spend.
  SessionLimitsConfig = Struct.new(:max_ai_credits, keyword_init: true) do
    def to_wire
      { maxAiCredits: max_ai_credits }.compact
    end
  end

  # Opt-in persistent session memory.
  MemoryConfiguration = Struct.new(:enabled, keyword_init: true) do
    def to_wire
      { enabled: enabled }.compact
    end
  end

  # Arguments passed to a BYOK +bearer_token_provider+ callable (per-session scoping).
  ProviderTokenArgs = Struct.new(:session_id, keyword_init: true)

  # Intercept outbound LLM inference HTTP/WebSocket requests. Assign an instance to
  # +ClientOptions#request_handler+ and override +send_request+ to mutate, replace,
  # or forward the request. BYOK providers may also set +bearer_token_provider+.
  class CopilotRequestHandler
    def send_request(request, _context)
      request
    end
  end

  # Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto").
  module ToolDefer
    AUTO  = "auto"
    NEVER = "never"
  end

  # System-message section identifiers (used with system_message section overrides).
  # The "preamble" section targets only the identity preamble; the "preserve" action
  # protects an individually-addressable section from a group-level remove.
  module SystemMessageSection
    PREAMBLE          = "preamble"
    IDENTITY          = "identity"
    TOOL_INSTRUCTIONS = "tool_instructions"
    PRESERVE          = "preserve"
  end

  # Hook names include :on_pre_tool_use, :on_post_tool_use, :on_pre_mcp_tool_call,
  # and :on_mcp_auth_request (MCP OAuth host token handler).

  # GitHub-anchored attachment variants.
  module GitHubAttachment
    GITHUB_COMMIT          = "GitHubCommit"
    GITHUB_RELEASE         = "GitHubRelease"
    GITHUB_ACTIONS_JOB     = "GitHubActionsJob"
    GITHUB_REPOSITORY      = "GitHubRepository"
    GITHUB_FILE_DIFF       = "GitHubFileDiff"
    GITHUB_TREE_COMPARISON = "GitHubTreeComparison"
    GITHUB_URL             = "GitHubUrl"
    GITHUB_FILE            = "GitHubFile"
    GITHUB_SNIPPET         = "GitHubSnippet"
  end

  # --- v2.6.0 upstream-sync feature types (parity with @github/copilot-sdk) ---

  # Identity of the integrating application. When any field is set, the client sends it
  # on the +connect+ handshake (as editorName/editorVersion/extensionName/extensionVersion)
  # so the runtime attributes its telemetry to your application.
  ClientInfo = Struct.new(
    :application_name, :application_version, :integration_name, :integration_version,
    keyword_init: true
  ) do
    # @return [Hash, nil] wire shape with only non-empty fields, or nil when all are empty
    def to_wire
      h = {}
      h[:editorName] = application_name unless application_name.to_s.empty?
      h[:editorVersion] = application_version unless application_version.to_s.empty?
      h[:extensionName] = integration_name unless integration_name.to_s.empty?
      h[:extensionVersion] = integration_version unless integration_version.to_s.empty?
      h.empty? ? nil : h
    end
  end

  # Auto routing preference used when the session model is "auto" (experimental).
  module AutoTier
    EFFICIENCY   = "efficiency"
    BALANCE      = "balance"
    INTELLIGENCE = "intelligence"
    FAST         = "fast"
  end

  # Wire shape used for ask-user prompts.
  module AskUserVariant
    LEGACY      = "legacy"
    ELICITATION = "elicitation"
  end

  # Scope of the working-directory trust granted at session creation.
  module WorkingDirectoryTrust
    SESSION = "session"
    USER    = "user"
  end

  # BYOK provider families accepted by +ProviderConfig#model_provider+.
  module ModelProvider
    OPENAI        = "openai"
    ANTHROPIC     = "anthropic"
    AZURE_OPENAI  = "azure_openai"
    OLLAMA        = "ollama"
    LM_STUDIO     = "lm_studio"
    FOUNDRY_LOCAL = "foundry_local"
    LLAMA_CPP     = "llama_cpp"
  end

  # Message provenance for +CopilotSession#send+ (+source:+).
  module MessageSource
    USER   = "user"
    SYSTEM = "system"

    # @param agent_id [String] opaque agent identifier
    # @return [String] the "agent-<id>" source value
    def self.agent(agent_id)
      "agent-#{agent_id}"
    end
  end

  # Arguments passed to a session's +github_token_provider+ callable.
  # +reason+ is "initial" or "refresh"; +session_id+ may be nil before a cloud
  # session has been assigned its id.
  GitHubTokenProviderArgs = Struct.new(:host, :session_id, :reason, keyword_init: true)

  # Result returned by a +github_token_provider+: either a token
  # (+kind+ "token" with +access_token+, optional +token_type+ and +expires_in+ seconds)
  # or a cancellation (+kind+ "cancelled"). Providers may also return a plain Hash.
  GitHubTokenResult = Struct.new(:kind, :access_token, :token_type, :expires_in, keyword_init: true) do
    def self.token(access_token:, expires_in:, token_type: nil)
      new(kind: "token", access_token: access_token, token_type: token_type, expires_in: expires_in)
    end

    def self.cancelled
      new(kind: "cancelled")
    end

    def to_wire
      return { kind: "cancelled" } if kind.to_s == "cancelled"

      h = { kind: "token", accessToken: access_token, expiresIn: expires_in }
      h[:tokenType] = token_type if token_type
      h
    end
  end

  # Session sandbox configuration. +allow_bypass+ opts in to letting the agent request
  # that an individual command run outside the sandbox through the normal permission flow.
  SandboxConfig = Struct.new(
    :enabled, :user_policy, :add_current_working_directory,
    :sandbox_mcp_servers, :sandbox_lsp_servers, :sandbox_extensions, :allow_bypass,
    keyword_init: true
  ) do
    def to_wire
      h = { enabled: enabled ? true : false }
      h[:userPolicy] = user_policy if user_policy
      h[:addCurrentWorkingDirectory] = add_current_working_directory unless add_current_working_directory.nil?
      h[:sandboxMcpServers] = sandbox_mcp_servers unless sandbox_mcp_servers.nil?
      h[:sandboxLspServers] = sandbox_lsp_servers unless sandbox_lsp_servers.nil?
      h[:sandboxExtensions] = sandbox_extensions unless sandbox_extensions.nil?
      h[:allowBypass] = allow_bypass unless allow_bypass.nil?
      h
    end
  end

  # Session-level image generation configuration.
  ImageGenerationConfig = Struct.new(:enabled, :watermark, :save_to_disk, keyword_init: true) do
    def to_wire
      h = {}
      h[:enabled] = enabled unless enabled.nil?
      h[:watermark] = watermark unless watermark.nil?
      h[:saveToDisk] = save_to_disk unless save_to_disk.nil?
      h
    end
  end

  # Runtime-owned Autopilot continuation options.
  AutopilotOptions = Struct.new(:continuation_enabled, :max_continues, keyword_init: true) do
    def to_wire
      h = {}
      h[:continuationEnabled] = continuation_enabled unless continuation_enabled.nil?
      h[:maxContinues] = max_continues unless max_continues.nil?
      h
    end
  end

  # Per-property model capability overrides (+model_capabilities:+ session option).
  # +limits+ uses snake_case wire keys, e.g.
  # { max_prompt_tokens:, max_output_tokens:, max_context_window_tokens: }.
  ModelCapabilitiesOverride = Struct.new(:supports, :limits, keyword_init: true) do
    def to_wire
      h = {}
      h[:supports] = supports if supports
      h[:limits] = (limits.respond_to?(:to_wire) ? limits.to_wire : limits) if limits
      h
    end
  end

  # Limit overrides for {ModelCapabilitiesOverride}.
  ModelLimitsOverride = Struct.new(
    :max_prompt_tokens, :max_output_tokens, :max_context_window_tokens,
    keyword_init: true
  ) do
    def to_wire
      to_h.compact
    end
  end

  # --- v3.0.0 feature types (SDK protocol 4, parity with the Microsoft Copilot SDK 2.0) ---

  # Client mode (+mode:+ client option). "coding-agent" (the default) keeps the CLI-like
  # coding tools and defaults. "empty" requires every session to set +available_tools+
  # and applies conservative session defaults (no telemetry, in-memory OAuth and
  # embedding storage, no cross-session store or memory, no on-demand instruction
  # discovery, repository hooks or host git operations). "copilot-cli" is accepted as a
  # legacy alias of "coding-agent".
  module ClientMode
    CODING_AGENT = "coding-agent"
    EMPTY        = "empty"
    COPILOT_CLI  = "copilot-cli"
  end

  # A named BYOK provider connection (+providers:+ session option). Select a registered
  # model with "provider/id" (see {ProviderModelConfig}) or set +default_provider:+ to
  # route unqualified model names through this connection.
  NamedProviderConfig = Struct.new(
    :name, :type, :base_url, :api_key, :api_key_command, :bearer_token,
    :wire_api, :transport, :azure, :headers, :model_provider,
    keyword_init: true
  ) do
    def to_wire
      h = { name: name }
      h[:type] = type if type
      h[:baseUrl] = base_url if base_url
      h[:apiKey] = api_key if api_key
      h[:apiKeyCommand] = api_key_command if api_key_command
      h[:bearerToken] = bearer_token if bearer_token
      h[:wireApi] = wire_api if wire_api
      h[:transport] = transport if transport
      if azure
        az = azure.transform_keys(&:to_sym)
        api_version = az[:api_version] || az[:apiVersion]
        h[:azure] = { apiVersion: api_version } if api_version
      end
      h[:headers] = headers if headers
      h[:modelProvider] = model_provider if model_provider
      h
    end
  end

  # A model registered under a named provider (+models:+ session option). The canonical
  # selection id is "provider/id"; +model_id+ is the known base model and +wire_model+
  # the name sent to the provider API (both default to +id+).
  ProviderModelConfig = Struct.new(
    :provider, :id, :model_id, :wire_model, :name,
    :max_prompt_tokens, :max_context_window_tokens, :max_output_tokens, :capabilities,
    keyword_init: true
  ) do
    def to_wire
      h = { provider: provider, id: id }
      h[:modelId] = model_id if model_id
      h[:wireModel] = wire_model if wire_model
      h[:name] = name if name
      h[:maxPromptTokens] = max_prompt_tokens if max_prompt_tokens
      h[:maxContextWindowTokens] = max_context_window_tokens if max_context_window_tokens
      h[:maxOutputTokens] = max_output_tokens if max_output_tokens
      h[:capabilities] = (capabilities.respond_to?(:to_wire) ? capabilities.to_wire : capabilities) if capabilities
      h
    end
  end

  # Copilot API (CAPI) routing policy (+capi:+ session option). Set +enabled+ to false
  # for a BYOK-only model catalog.
  CapiConfig = Struct.new(:enabled, keyword_init: true) do
    def to_wire
      { enabled: enabled }.compact
    end
  end

  # Stream of a {ShellOutputData} chunk; omitted on the wire means stdout.
  module ShellOutputStream
    STDOUT = "stdout"
    STDERR = "stderr"
  end

  # Data of a live "tool.shell_output" event ({SessionEventType::TOOL_SHELL_OUTPUT}).
  # +sequence+ orders chunks across both streams of one tool call.
  ShellOutputData = Struct.new(:tool_call_id, :text, :sequence, :stream, keyword_init: true) do
    def self.from_hash(h)
      new(
        tool_call_id: h["toolCallId"],
        text: h["text"],
        sequence: h["sequence"],
        stream: h["stream"]
      )
    end
  end

  # Input kinds of a {ShellAuthenticationRequest}.
  module ShellAuthenticationInputKind
    SECRET  = "secret"
    TEXT    = "text"
    CONFIRM = "confirm"
  end

  # A shell program asking for credentials (+on_shell_authentication_request:+ handler).
  ShellAuthenticationRequest = Struct.new(
    :session_id, :request_id, :program, :prompt, :input_kind, :command, :cwd,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        session_id: h["sessionId"],
        request_id: h["requestId"],
        program: h["program"],
        prompt: h["prompt"],
        input_kind: h["inputKind"],
        command: h["command"].is_a?(String) ? h["command"] : "",
        cwd: h["cwd"].is_a?(String) ? h["cwd"] : ""
      )
    end
  end

  # Answer to a {ShellAuthenticationRequest}: +action+ "accept" with a String +value+, or
  # "cancel". Handlers may also return a plain Hash or nil (cancel).
  ShellAuthenticationResponse = Struct.new(:action, :value, keyword_init: true) do
    def self.accept(value)
      new(action: "accept", value: value)
    end

    def self.cancel
      new(action: "cancel")
    end
  end

  # A memory the agent asked to store (+store_memory:+ session callback).
  StoredMemory = Struct.new(:session_id, :subject, :fact, :citations, :reason, keyword_init: true) do
    def self.from_hash(h)
      new(
        session_id: h["sessionId"],
        subject: h["subject"],
        fact: h["fact"],
        citations: h["citations"],
        reason: h["reason"]
      )
    end
  end

  # Traffic served by the {NetworkConfig} callbacks.
  module NetworkScope
    CONNECTION = "connection"
    RUNTIME    = "runtime"
  end

  # Host network callbacks (+network:+ client option). Each callback is optional:
  # +resolve_proxy+ receives a {NetworkResolveProxyRequest} and returns a PAC-style proxy
  # list String (or nil to keep the runtime's own resolution); +authenticate_negotiate+
  # receives a {NetworkAuthenticateNegotiateRequest} and returns a base64 SPNEGO token
  # String or nil; +authenticate_basic+ receives a {NetworkAuthenticateBasicRequest} and
  # returns +{ username:, password: }+ or nil. Tokens and passwords are never logged.
  NetworkConfig = Struct.new(
    :scope, :resolve_proxy, :authenticate_negotiate, :authenticate_basic,
    keyword_init: true
  ) do
    # @return [Boolean] true when at least one callback is set
    def any_callback?
      !!(resolve_proxy || authenticate_negotiate || authenticate_basic)
    end
  end

  # Request target passed to {NetworkConfig#resolve_proxy}.
  NetworkResolveProxyRequest = Struct.new(:url, keyword_init: true)

  # Kerberos service principal passed to {NetworkConfig#authenticate_negotiate}.
  NetworkAuthenticateNegotiateRequest = Struct.new(:spn, keyword_init: true)

  # Proxy Basic challenge passed to {NetworkConfig#authenticate_basic}.
  NetworkAuthenticateBasicRequest = Struct.new(:proxy, :realm, :previous_rejected, keyword_init: true)

  # Structured Policy Helper failure. Attached to a failed session.create / session.resume
  # as +error.data.policyHelperFailure+ (see {JsonRpcError#policy_helper_failure}).
  # Helper-authored text is untrusted: render it as plain text and never execute it.
  PolicyHelperFailure = Struct.new(
    :category, :failure_mode, :message, :previous_policy_retained, :reason,
    :authority, :code, :details, :diagnostic, :help_link, :remediation, :severity,
    keyword_init: true
  ) do
    def self.from_hash(h)
      new(
        category: h["category"],
        failure_mode: h["failureMode"],
        message: h["message"],
        previous_policy_retained: h["previousPolicyRetained"] ? true : false,
        reason: h["reason"],
        authority: h["authority"],
        code: h["code"],
        details: h["details"],
        diagnostic: h["diagnostic"],
        help_link: h["helpLink"],
        remediation: h["remediation"],
        severity: h["severity"]
      )
    end
  end

  # Session permission modes (+permission_mode:+ option, {CopilotSession#set_permission_mode}).
  module PermissionMode
    MANUAL    = "manual"
    ASSISTED  = "assisted"
    ALLOW_ALL = "allow-all"
  end

  # Known reasons a permission-mode change was refused.
  module PermissionModeRefusalReason
    MANAGED_POLICY = "managed_policy"
  end

  # Result of {CopilotSession#set_permission_mode}. +reason+ explains a refusal when known
  # (see {PermissionModeRefusalReason}, currently "managed_policy").
  PermissionModeSetResult = Struct.new(:success, :mode, :reason, keyword_init: true) do
    def self.from_hash(h)
      new(success: h["success"] ? true : false, mode: h["mode"], reason: h["reason"])
    end

    # @return [Boolean] true when managed (enterprise) policy refused the mode
    def refused_by_managed_policy?
      reason == PermissionModeRefusalReason::MANAGED_POLICY
    end
  end
end

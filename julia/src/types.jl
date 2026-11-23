# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# Type definitions for the Copilot SDK

"""Connection state of the client."""
@enum ConnectionState begin
    DISCONNECTED
    CONNECTING
    CONNECTED
    CONNECTION_ERROR
end

"""Log verbosity levels for the CLI server."""
@enum LogLevel begin
    LOG_NONE
    LOG_ERROR
    LOG_WARNING
    LOG_INFO
    LOG_DEBUG
    LOG_ALL
end

const LOG_LEVEL_STRINGS = Dict{LogLevel, String}(
    LOG_NONE    => "none",
    LOG_ERROR   => "error",
    LOG_WARNING => "warning",
    LOG_INFO    => "info",
    LOG_DEBUG   => "debug",
    LOG_ALL     => "all",
)

"""Result type for tool invocations."""
@enum ToolResultType begin
    TOOL_SUCCESS
    TOOL_FAILURE
    TOOL_REJECTED
    TOOL_DENIED
    TOOL_TIMEOUT
end

const TOOL_RESULT_STRINGS = Dict{ToolResultType, String}(
    TOOL_SUCCESS  => "success",
    TOOL_FAILURE  => "failure",
    TOOL_REJECTED => "rejected",
    TOOL_DENIED   => "denied",
    TOOL_TIMEOUT  => "timeout",
)

"""Configuration for the session filesystem provider."""
Base.@kwdef struct SessionFsConfig
    initial_cwd::String = ""
    session_state_path::String = ""
    conventions::String = "posix"  # "windows" or "posix"
end

"""MCP server connection type."""
@enum McpServerType MCP_STDIO MCP_HTTP

const MCP_SERVER_TYPE_STRINGS = Dict{McpServerType, String}(
    MCP_STDIO => "stdio",
    MCP_HTTP  => "http",
)

"""MCP server configuration."""
Base.@kwdef struct McpServerConfig
    type::McpServerType = MCP_STDIO
    command::Union{String, Nothing} = nothing
    args::Vector{String} = String[]
    url::Union{String, Nothing} = nothing
    env::Union{Dict{String, String}, Nothing} = nothing
    headers::Union{Dict{String, String}, Nothing} = nothing
    tools::Vector{String} = ["*"]  # tools to expose from the server ("*" = all)
end

"""Command definition for session commands."""
Base.@kwdef struct CommandDefinition
    name::String = ""
    description::String = ""
end

"""Image/response format options."""
@enum ImageResponseFormat FORMAT_TEXT FORMAT_IMAGE FORMAT_JSON_OBJECT

const IMAGE_RESPONSE_FORMAT_STRINGS = Dict{ImageResponseFormat, String}(
    FORMAT_TEXT        => "text",
    FORMAT_IMAGE       => "image",
    FORMAT_JSON_OBJECT => "json_object",
)

# ------------------------------------------------------------------------------------
#  v2.6.0 / v3.0.0 option types (SDK protocol 4)
# ------------------------------------------------------------------------------------

# Client modes: "coding-agent" (default, CLI-like coding tools and defaults) or "empty"
# (every create/resume must pass available_tools, and restrictive defaults are applied
# under explicit options). "copilot-cli" is accepted as an alias of "coding-agent".
const CLIENT_MODE_CODING_AGENT = "coding-agent"
const CLIENT_MODE_EMPTY = "empty"

"""Identity of the integrating application, sent on the `connect` handshake."""
Base.@kwdef struct ClientInfo
    editor_name::Union{String, Nothing} = nothing
    editor_version::Union{String, Nothing} = nothing
    extension_name::Union{String, Nothing} = nothing
    extension_version::Union{String, Nothing} = nothing
end

"""
Network callbacks registered with `network.setProvider` after connecting.

- `scope`: `"connection"` (default) or `"runtime"`.
- `resolve_proxy(request::Dict)`: return a PAC-style proxy list such as
  `"PROXY proxy.example:8080; DIRECT"`, or `nothing`.
- `authenticate_negotiate(request::Dict)`: return a base64 SPNEGO token (without the
  `Negotiate ` prefix), or `nothing`.
- `authenticate_basic(request::Dict)`: return `(username=..., password=...)` (or a Dict
  with those keys), or `nothing`.

Never log tokens or passwords.
"""
Base.@kwdef struct NetworkConfig
    scope::Union{String, Nothing} = nothing
    resolve_proxy::Union{Function, Nothing} = nothing
    authenticate_negotiate::Union{Function, Nothing} = nothing
    authenticate_basic::Union{Function, Nothing} = nothing
end

"""Sandbox configuration (`allow_bypass` opts in to per-command sandbox bypass requests)."""
Base.@kwdef struct SandboxConfig
    enabled::Bool = true
    allow_bypass::Union{Bool, Nothing} = nothing
    user_policy::Union{Dict{String, Any}, Nothing} = nothing
    add_current_working_directory::Union{Bool, Nothing} = nothing
    sandbox_mcp_servers::Union{Bool, Nothing} = nothing
    sandbox_lsp_servers::Union{Bool, Nothing} = nothing
    sandbox_extensions::Union{Bool, Nothing} = nothing
end

"""Image generation opt-in (wire `imageGeneration`)."""
Base.@kwdef struct ImageGenerationConfig
    enabled::Union{Bool, Nothing} = nothing
    watermark::Union{Bool, Nothing} = nothing
    save_to_disk::Union{Bool, Nothing} = nothing
end

"""Autopilot continuation options (wire `autopilot`)."""
Base.@kwdef struct AutopilotOptions
    continuation_enabled::Union{Bool, Nothing} = nothing
    max_continues::Union{Int, Nothing} = nothing
end

"""
Legacy single BYOK provider. `api_key_command` runs before each request to obtain a fresh
API key. On SDK protocol 4 it is sent as the named `"default"` connection with Copilot API
models disabled; `model_id`, `wire_model`, and `max_*_tokens` become a model definition for
the session model.
"""
Base.@kwdef struct ProviderConfig
    type::Union{String, Nothing} = nothing
    base_url::Union{String, Nothing} = nothing
    api_key::Union{String, Nothing} = nothing
    api_key_command::Union{String, Nothing} = nothing
    bearer_token::Union{String, Nothing} = nothing
    wire_api::Union{String, Nothing} = nothing
    transport::Union{String, Nothing} = nothing
    headers::Union{Dict{String, String}, Nothing} = nothing
    azure_api_version::Union{String, Nothing} = nothing
    model_provider::Union{String, Nothing} = nothing
    model_id::Union{String, Nothing} = nothing
    wire_model::Union{String, Nothing} = nothing
    max_prompt_tokens::Union{Int, Nothing} = nothing
    max_context_window_tokens::Union{Int, Nothing} = nothing
    max_output_tokens::Union{Int, Nothing} = nothing
end

"""Named BYOK provider connection (wire `inference.providers[]`); select models as `"name/id"`."""
Base.@kwdef struct NamedProviderConfig
    name::String
    base_url::String
    type::Union{String, Nothing} = nothing
    api_key::Union{String, Nothing} = nothing
    api_key_command::Union{String, Nothing} = nothing
    bearer_token::Union{String, Nothing} = nothing
    wire_api::Union{String, Nothing} = nothing
    transport::Union{String, Nothing} = nothing
    headers::Union{Dict{String, String}, Nothing} = nothing
    azure_api_version::Union{String, Nothing} = nothing
    model_provider::Union{String, Nothing} = nothing
end

"""BYOK model definition (wire `inference.models[]`) served by a named provider."""
Base.@kwdef struct ProviderModelConfig
    provider::String
    id::String
    model_id::Union{String, Nothing} = nothing
    wire_model::Union{String, Nothing} = nothing
    name::Union{String, Nothing} = nothing
    max_prompt_tokens::Union{Int, Nothing} = nothing
    max_context_window_tokens::Union{Int, Nothing} = nothing
    max_output_tokens::Union{Int, Nothing} = nothing
    capabilities::Union{Dict{String, Any}, Nothing} = nothing
end

"""Arguments passed to a `github_token_provider` callback (`gitHubToken.getToken`)."""
Base.@kwdef struct GitHubTokenProviderArgs
    host::String = ""
    session_id::Union{String, Nothing} = nothing
    reason::String = ""
end

"""
A password or other answer that a shell command run by the agent asks for (for example
`sudo -A`, `ssh`, or `git` over HTTPS). Answer with `(action="accept", value="...")` or
`(action="cancel")`; the answer goes only to the program and is never logged.
"""
Base.@kwdef struct ShellAuthenticationRequest
    request_id::String = ""
    program::String = ""
    prompt::String = ""
    input_kind::String = ""  # "secret", "text", or "confirm"
    command::String = ""
    cwd::String = ""
end

"""A memory the agent asked to store, passed to a session's `store_memory` callback."""
Base.@kwdef struct StoredMemory
    session_id::String = ""
    subject::String = ""
    fact::String = ""
    citations::String = ""
    reason::String = ""
end

# Session event types (unknown types are still delivered to listeners unchanged).
const EVENT_TOOL_SHELL_OUTPUT = "tool.shell_output"
# No longer emitted by protocol 4 runtimes; still tolerated when received.
const EVENT_TOOL_EXECUTION_PARTIAL_RESULT = "tool.execution_partial_result"
const EVENT_EXTERNAL_TOOL_REQUESTED = "external_tool.requested"

"""Data of a `tool.shell_output` event (`stream` is "stdout", "stderr", or `nothing`)."""
Base.@kwdef struct ShellOutputEventData
    tool_call_id::String = ""
    text::String = ""
    sequence::Int = 0
    stream::Union{String, Nothing} = nothing
end

# Known value of `PermissionModeSetResult.reason`.
const PERMISSION_MODE_REFUSAL_MANAGED_POLICY = "managed_policy"

"""Result of `set_permission_mode` (`reason` explains a refusal when known)."""
Base.@kwdef struct PermissionModeSetResult
    success::Bool = false
    mode::String = ""
    reason::Union{String, Nothing} = nothing
end

"""
Structured Policy Helper failure, read from a failed create/resume `JsonRpcError`
(`policy_helper_failure(err)`) or a managed settings snapshot. Helper-authored text is
untrusted: render it as plain text.
"""
Base.@kwdef struct PolicyHelperFailure
    category::String = ""
    failure_mode::String = ""
    message::String = ""
    previous_policy_retained::Bool = false
    reason::String = ""
    severity::Union{String, Nothing} = nothing
    authority::Union{String, Nothing} = nothing
    code::Union{String, Nothing} = nothing
    details::Any = nothing
    diagnostic::Union{String, Nothing} = nothing
    help_link::Any = nothing
    remediation::Union{String, Nothing} = nothing
end

# Message sources for `MessageOptions.source`.
const MESSAGE_SOURCE_USER = "user"
const MESSAGE_SOURCE_SYSTEM = "system"
"""Message source for an agent: `"agent-<id>"`."""
message_source_agent(agent_id::AbstractString) = "agent-" * agent_id

"""Options for creating a CopilotClient."""
Base.@kwdef mutable struct CopilotClientOptions
    cli_path::Union{String, Nothing} = nothing
    cli_args::Vector{String} = String[]
    cwd::Union{String, Nothing} = nothing
    port::Int = 0
    use_stdio::Bool = true
    cli_url::Union{String, Nothing} = nothing
    log_level::LogLevel = LOG_ERROR
    auto_start::Bool = true
    env::Union{Dict{String, String}, Nothing} = nothing
    github_token::Union{String, Nothing} = nothing
    use_logged_in_user::Bool = true
    session_idle_timeout_seconds::Union{Int, Nothing} = nothing
    session_fs::Union{SessionFsConfig, Nothing} = nothing
    copilot_home::Union{String, Nothing} = nothing
    tcp_connection_token::Union{String, Nothing} = nothing
    builtin_plugin_directories::Vector{String} = String[]  # wire: builtinPluginDirectories
    in_process::Bool = false                               # wire: inProcess
    remote::Bool = false                                   # spawns the CLI with --remote
    # --- v2.6.0 / v3.0.0 (SDK protocol 4) ---
    client_info::Union{ClientInfo, Nothing} = nothing      # sent on the connect handshake
    mode::String = CLIENT_MODE_CODING_AGENT                # "coding-agent" or "empty"
    network::Union{NetworkConfig, Nothing} = nothing       # network.setProvider callbacks
    on_get_trace_context::Union{Function, Nothing} = nothing
end

"""Per-session spending / credit limits."""
Base.@kwdef struct SessionLimitsConfig
    max_ai_credits::Union{Float64, Nothing} = nothing
    max_requests::Union{Int, Nothing} = nothing
    max_tokens::Union{Int, Nothing} = nothing
end

"""Persistent cross-turn session memory configuration."""
Base.@kwdef struct MemoryConfiguration
    enabled::Bool = false
    max_entries::Union{Int, Nothing} = nothing
    directory::Union{String, Nothing} = nothing
end

"""Configuration for creating a session."""
Base.@kwdef mutable struct SessionConfig
    model::String = "gpt-4"
    system_message::Union{String, Nothing} = nothing
    instructions::Union{String, Nothing} = nothing
    tools::Vector{Any} = Any[]
    on_event::Union{Function, Nothing} = nothing
    on_permission_request::Union{Function, Nothing} = nothing
    reasoning_effort::Union{String, Nothing} = nothing
    streaming::Bool = true
    agent::Union{String, Nothing} = nothing
    excluded_tools::Vector{String} = String[]
    mcp_servers::Union{Dict{String, McpServerConfig}, Nothing} = nothing
    model_capabilities::Union{Dict{String, Any}, Nothing} = nothing
    enable_config_discovery::Bool = false
    include_sub_agent_streaming_events::Bool = false
    commands::Vector{CommandDefinition} = CommandDefinition[]
    skill_directories::Vector{String} = String[]
    disabled_skills::Vector{String} = String[]
    working_directory::Union{String, Nothing} = nothing
    github_token::Union{String, Nothing} = nothing
    response_format::Union{ImageResponseFormat, Nothing} = nothing
    request_headers::Union{Dict{String, String}, Nothing} = nothing
    on_elicitation_request::Union{Function, Nothing} = nothing
    instruction_directories::Vector{String} = String[]
    # --- Upstream-sync session configuration (parity with @github/copilot-sdk) ---
    enable_citations::Bool = false
    excluded_builtin_agents::Vector{String} = String[]
    session_limits::Union{SessionLimitsConfig, Nothing} = nothing
    memory::Union{MemoryConfiguration, Nothing} = nothing
    otlp_protocol::Union{String, Nothing} = nothing
    enable_web_socket_responses::Bool = false
    exp_assignments::Union{Dict{String, String}, Nothing} = nothing
    on_mcp_auth_request::Union{Function, Nothing} = nothing
    bearer_token_provider::Union{Function, Nothing} = nothing
    on_post_tool_use::Union{Function, Nothing} = nothing
    on_pre_mcp_tool_call::Union{Function, Nothing} = nothing
    request_handler::Union{Function, Nothing} = nothing
    # --- Additional upstream-sync session configuration (2026-08) ---
    rewind_enabled::Bool = false                                         # wire: rewindEnabled
    additional_directories::Vector{String} = String[]                   # wire: additionalDirectories
    disabled_mcp_servers::Vector{String} = String[]                     # wire: disabledMcpServers
    github_mcp_tool_config::Union{Dict{String, Any}, Nothing} = nothing # wire: githubMcpToolConfig
    canvas_provider::Any = nothing                                      # wire: canvasProvider
    custom_agents_local_only::Bool = false                              # wire: customAgentsLocalOnly
    tool_search::Any = nothing                                          # wire: toolSearch
    experimental_mode::Bool = false                                     # wire: experimentalMode
    content_exclusion::Bool = false                                     # wire: contentExclusion
    on_user_prompt_transformed::Union{Function, Nothing} = nothing      # wire: userPromptTransformed
    # --- Options read by create/resume ---
    cloud::Any = nothing                                                 # wire: cloud (create only)
    on_exit_plan_mode::Union{Function, Nothing} = nothing               # wire: requestExitPlanMode
    enable_session_telemetry::Union{Bool, Nothing} = nothing            # wire: enableSessionTelemetry
    # --- v2.6.0 upstream-sync session configuration ---
    auto_tier::Union{String, Nothing} = nothing                         # wire: capi.autoTier
    ask_user_variant::Union{String, Nothing} = nothing                  # wire: askUserVariant
    github_token_provider::Union{Function, Nothing} = nothing           # answers gitHubToken.getToken
    sandbox_config::Union{SandboxConfig, Nothing} = nothing             # wire: sandboxConfig
    feature_flags::Union{Dict{String, Any}, Nothing} = nothing          # wire: featureFlags
    allowed_models::Union{Vector{String}, Nothing} = nothing            # wire: allowedModels
    auth_client_id_metadata_url::Union{String, Nothing} = nothing       # wire: authClientIdMetadataUrl
    name::Union{String, Nothing} = nothing                              # wire: name (create only)
    agent_directories::Union{Vector{String}, Nothing} = nothing         # wire: agentDirectories
    hook_directories::Union{Vector{String}, Nothing} = nothing          # wire: hookDirectories
    interactive_user::Union{Bool, Nothing} = nothing                    # wire: interactiveUser
    excluded_managed_settings::Union{Vector{String}, Nothing} = nothing # wire: excludedManagedSettings
    enforce_managed_model_defaults::Union{Bool, Nothing} = nothing      # wire: enforceManagedModelDefaults
    trust_working_directory::Union{String, Nothing} = nothing           # "session"/"user" (create only)
    refresh_custom_instructions::Union{Bool, Nothing} = nothing         # create only
    allow_transcript_recovery::Union{Bool, Nothing} = nothing           # resume only
    disable_resume::Bool = false                                        # resume only: suppressResumeEvent
    image_generation::Union{ImageGenerationConfig, Nothing} = nothing   # wire: imageGeneration
    autopilot::Union{AutopilotOptions, Nothing} = nothing               # wire: autopilot
    on_subagent_start::Union{Function, Nothing} = nothing               # hooks.invoke subagentStart
    on_subagent_stop::Union{Function, Nothing} = nothing                # hooks.invoke subagentStop
    provider::Union{ProviderConfig, Nothing} = nothing                  # legacy single BYOK provider
    available_tools::Union{Vector{String}, Nothing} = nothing           # wire: availableTools
    # --- v3.0.0 (SDK protocol 4) ---
    providers::Union{Vector{NamedProviderConfig}, Nothing} = nothing    # wire: inference.providers
    models::Union{Vector{ProviderModelConfig}, Nothing} = nothing       # wire: inference.models
    default_provider::Union{String, Nothing, Missing} = nothing         # `missing` clears the saved default
    capi::Union{Dict{String, Any}, Nothing} = nothing                   # wire: github.capi
    on_shell_authentication_request::Union{Function, Nothing} = nothing # answers shellAuthentication.request
    store_memory::Union{Function, Nothing} = nothing                    # answers memoryStore.store
end

"""Payload for sending a message to a session."""
Base.@kwdef struct MessageOptions
    prompt::String
    attachments::Union{Vector{Dict{String, Any}}, Nothing} = nothing
    mode::Union{String, Nothing} = nothing
    agent_mode::Union{String, Nothing} = nothing
    display_prompt::Union{String, Nothing} = nothing
    request_headers::Union{Dict{String, String}, Nothing} = nothing
    source::Union{String, Nothing} = nothing                  # "user", "system", or "agent-<id>"
    client_turn_id::Union{String, Nothing} = nothing          # wire: clientTurnId
    response_schema::Union{Dict{String, Any}, Nothing} = nothing  # structured output JSON Schema
end

"""Result returned from a tool handler."""
Base.@kwdef mutable struct ToolResult
    text_result_for_llm::String = ""
    result_type::ToolResultType = TOOL_SUCCESS
    error::Union{String, Nothing} = nothing
    session_log::Union{String, Nothing} = nothing
end

"""Context provided to a tool handler on invocation."""
Base.@kwdef struct ToolInvocation
    session_id::String = ""
    tool_call_id::String = ""
    tool_name::String = ""
    arguments::Any = nothing
end

"""A session event received from the CLI server."""
struct SessionEvent
    type::String
    data::Dict{String, Any}
    id::String
    timestamp::String
    parent_id::Union{String, Nothing}
    agent_id::Union{String, Nothing}
    ephemeral::Union{Bool, Nothing}
end

SessionEvent(type::String) = SessionEvent(type, Dict{String, Any}(), "", "", nothing, nothing, nothing)
SessionEvent(type::String, data::Dict{String, Any}) = SessionEvent(type, data, "", "", nothing, nothing, nothing)

"""Status information returned by the CLI server."""
struct ServerStatus
    version::String
    protocol_version::Int
    authenticated::Bool
end

ServerStatus() = ServerStatus("", 0, false)

"""Authentication status."""
struct AuthStatus
    authenticated::Bool
    user::Union{String, Nothing}
end

AuthStatus() = AuthStatus(false, nothing)

"""Model information."""
Base.@kwdef struct ModelInfo
    id::String = ""
    name::String = ""
    vendor::String = ""
    family::String = ""
    version::String = ""
    is_default::Bool = false
end

"""Session metadata."""
Base.@kwdef struct SessionMetadata
    session_id::String = ""
    title::Union{String, Nothing} = nothing
    created_at::Union{String, Nothing} = nothing
    updated_at::Union{String, Nothing} = nothing
    model::Union{String, Nothing} = nothing
end

"""
Permission request sent by the server. On SDK protocol 4 it comes from a
`permission.requested` event: `kind` is the permission kind (such as "shell", "write",
"read", "mcp", "url", or "custom-tool") and `details` holds the full request.
"""
Base.@kwdef struct PermissionRequest
    permission_id::String = ""
    tool_name::String = ""
    description::String = ""
    arguments::Dict{String, Any} = Dict{String, Any}()
    kind::String = ""
    tool_call_id::String = ""
    details::Dict{String, Any} = Dict{String, Any}()
end

# Convenience constructors from Dict
function SessionEvent(d::Dict{String, Any})
    SessionEvent(
        get(d, "type", ""),
        get(d, "data", Dict{String, Any}()),
    )
end

function PermissionRequest(d::Dict)
    PermissionRequest(;
        permission_id = get(d, "permissionId", get(d, "permission_id", "")),
        tool_name     = get(d, "toolName", get(d, "tool_name", "")),
        description   = get(d, "description", ""),
        arguments     = get(d, "arguments", Dict{String, Any}()),
    )
end

# ------------------------------------------------------------------------------------
#  Upstream-sync feature types & constants (parity with @github/copilot-sdk, 2026-07)
# ------------------------------------------------------------------------------------

"""Arguments passed to a BYOK bearer-token provider callback."""
Base.@kwdef struct ProviderTokenArgs
    provider::String = ""
    endpoint::Union{String, Nothing} = nothing
    scope::Union{String, Nothing} = nothing
end

"""A named, individually-configurable section of the system message."""
Base.@kwdef struct SystemMessageSection
    name::String = ""       # e.g. "preamble"
    content::String = ""
    preserve::Bool = false  # preserve this section across compaction
end

"""GitHub content (commit, repository, PR, issue) attached to a message."""
Base.@kwdef struct GitHubAttachment
    kind::String = ""  # one of the GITHUB_*_ATTACHMENT variants
    ref::String = ""
end

"""Tool defer-loading strategy: load eagerly or lazily on first use."""
@enum ToolDefer TOOL_DEFER_EAGER TOOL_DEFER_LAZY

# System message section names.
const SYSTEM_MESSAGE_PREAMBLE = "preamble"
const SYSTEM_MESSAGE_PRESERVE = "preserve"

# GitHub attachment variant identifiers.
const GITHUB_COMMIT_ATTACHMENT = "GitHubCommit"
const GITHUB_REPOSITORY_ATTACHMENT = "GitHubRepository"
const GITHUB_PULL_REQUEST_ATTACHMENT = "GitHubPullRequest"
const GITHUB_ISSUE_ATTACHMENT = "GitHubIssue"

# OTLP telemetry protocols.
const OTLP_PROTOCOL_GRPC = "grpc"
const OTLP_PROTOCOL_HTTP = "http/protobuf"

"""Signature alias for a custom Copilot HTTP request handler."""
const CopilotRequestHandler = Function

"""Authoring options for a programmatic agent factory."""
Base.@kwdef struct AgentFactoryOptions
    args_schema::Union{Dict{String, Any}, Nothing} = nothing  # wire: argsSchema
end

"""Reply to a permission request, including optional decision context."""
Base.@kwdef struct PermissionResponse
    allowed::Bool = false
    reason::Union{String, Nothing} = nothing
    decision_context::Union{Dict{String, Any}, Nothing} = nothing  # wire: decisionContext
end

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
end

"""Payload for sending a message to a session."""
Base.@kwdef struct MessageOptions
    prompt::String
    attachments::Union{Vector{Dict{String, Any}}, Nothing} = nothing
    mode::Union{String, Nothing} = nothing
    agent_mode::Union{String, Nothing} = nothing
    display_prompt::Union{String, Nothing} = nothing
    request_headers::Union{Dict{String, String}, Nothing} = nothing
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

"""Permission request sent by the server."""
Base.@kwdef struct PermissionRequest
    permission_id::String = ""
    tool_name::String = ""
    description::String = ""
    arguments::Dict{String, Any} = Dict{String, Any}()
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

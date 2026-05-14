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
    remote::Bool = false
    on_get_trace_context::Union{Function, Nothing} = nothing
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
    enable_session_telemetry::Union{Bool, Nothing} = nothing
    on_exit_plan_mode::Union{Function, Nothing} = nothing
end

"""Payload for sending a message to a session."""
Base.@kwdef struct MessageOptions
    prompt::String
    attachments::Union{Vector{Dict{String, Any}}, Nothing} = nothing
    mode::Union{String, Nothing} = nothing
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

"""Completion type for slash command input."""
@enum SlashCommandInputCompletion SCIC_DIRECTORY

const SLASH_COMMAND_INPUT_COMPLETION_STRINGS = Dict{SlashCommandInputCompletion, String}(
    SCIC_DIRECTORY => "directory",
)

"""Kind of a slash command."""
@enum SlashCommandKind SCK_BUILTIN SCK_CLIENT SCK_SKILL

const SLASH_COMMAND_KIND_STRINGS = Dict{SlashCommandKind, String}(
    SCK_BUILTIN => "builtin",
    SCK_CLIENT  => "client",
    SCK_SKILL   => "skill",
)

"""Price category for the model picker."""
@enum ModelPickerPriceCategory MPPC_HIGH MPPC_LOW MPPC_MEDIUM MPPC_VERY_HIGH

const MODEL_PICKER_PRICE_CATEGORY_STRINGS = Dict{ModelPickerPriceCategory, String}(
    MPPC_HIGH      => "high",
    MPPC_LOW       => "low",
    MPPC_MEDIUM    => "medium",
    MPPC_VERY_HIGH => "very_high",
)

"""Input specification for a slash command."""
Base.@kwdef struct SlashCommandInput
    hint::String = ""
    completion::Union{SlashCommandInputCompletion, Nothing} = nothing
end

"""Information about a slash command."""
Base.@kwdef struct SlashCommandInfo
    allow_during_agent_execution::Bool = false
    description::String = ""
    kind::SlashCommandKind = SCK_BUILTIN
    name::String = ""
    aliases::Union{Vector{String}, Nothing} = nothing
    experimental::Union{Bool, Nothing} = nothing
    input::Union{SlashCommandInput, Nothing} = nothing
end

"""Request to invoke a command."""
Base.@kwdef struct CommandsInvokeRequest
    name::String = ""
    input::Union{String, Nothing} = nothing
end

"""Request to list available commands."""
Base.@kwdef struct CommandsListRequest
    include_builtins::Union{Bool, Nothing} = nothing
    include_client_commands::Union{Bool, Nothing} = nothing
    include_skills::Union{Bool, Nothing} = nothing
end

"""Token prices for model billing."""
Base.@kwdef struct ModelBillingTokenPrices
    batch_size::Union{Int, Nothing} = nothing
    cache_price::Union{Int, Nothing} = nothing
    input_price::Union{Int, Nothing} = nothing
    output_price::Union{Int, Nothing} = nothing
end

"""Model billing information."""
Base.@kwdef struct ModelBilling
    multiplier::Float64 = 0.0
    token_prices::Union{ModelBillingTokenPrices, Nothing} = nothing
    picker_price_category::Union{ModelPickerPriceCategory, Nothing} = nothing
end

"""Experimental
Diagnostics from loading skills."""
Base.@kwdef struct SkillsLoadDiagnostics
    errors::Vector{String} = String[]
    warnings::Vector{String} = String[]
end

"""Exit plan mode request from the agent."""
Base.@kwdef struct ExitPlanModeRequest
    summary::String = ""
    plan_content::Union{String, Nothing} = nothing
    actions::Vector{String} = String[]
    recommended_action::String = ""
end

"""Response to an exit-plan-mode request."""
Base.@kwdef struct ExitPlanModeResult
    approved::Bool = true
    selected_action::Union{String, Nothing} = nothing
    feedback::Union{String, Nothing} = nothing
end

"""W3C Trace Context for distributed tracing."""
Base.@kwdef struct TraceContext
    traceparent::Union{String, Nothing} = nothing
    tracestate::Union{String, Nothing} = nothing
end

function ExitPlanModeRequest(d::Dict)
    ExitPlanModeRequest(;
        summary = get(d, "summary", ""),
        plan_content = get(d, "planContent", nothing),
        actions = get(d, "actions", String[]),
        recommended_action = get(d, "recommendedAction", ""),
    )
end

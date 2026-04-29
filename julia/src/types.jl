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
end

SessionEvent(type::String) = SessionEvent(type, Dict{String, Any}())

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

# types.tcl - Type definitions and helper procs for the Copilot SDK for Tcl
#
# Types are represented as Tcl dicts. This module provides constructors and
# validators for the core data structures used throughout the SDK.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6

namespace eval ::copilot::types {
    namespace export make_client_options make_session_config make_tool \
                     make_send_options make_session_event validate_dict \
                     make_session_fs_config make_mcp_server_config \
                     make_command_definition \
                     make_session_limits_config make_memory_configuration \
                     make_provider_token_args make_copilot_request_handler \
                     tool_defer system_message_section github_attachment
}

# -- Client options -----------------------------------------------------------

proc ::copilot::types::make_client_options {args} {
    set defaults [dict create \
        cli_path                        "" \
        cli_url                         "" \
        log_level                       "warn" \
        timeout                         30000 \
        github_token                    "" \
        use_logged_in_user              1 \
        session_idle_timeout_seconds    0 \
        session_fs                      {} \
        copilot_home                    "" \
        tcp_connection_token            "" \
        request_handler                 "" \
        bearer_token_provider           "" \
    ]
    set opts $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown client option: $key"
        }
        dict set opts $key $value
    }
    return $opts
}

# -- Session config -----------------------------------------------------------

proc ::copilot::types::make_session_config {args} {
    set defaults [dict create \
        system_prompt                       "" \
        github_token                        "" \
        tools                               {} \
        streaming                           0 \
        model                               "" \
        session_id                          "" \
        state                               {} \
        session_idle_timeout_seconds        0 \
        excluded_tools                      {} \
        mcp_servers                         {} \
        model_capabilities                  {} \
        enable_config_discovery             0 \
        include_sub_agent_streaming_events  0 \
        commands                            {} \
        skill_directories                   {} \
        disabled_skills                     {} \
        working_directory                   "" \
        reasoning_effort                    "" \
        response_format                     "" \
        request_headers                     {} \
        elicitation_handler                 "" \
        instruction_directories             {} \
        enable_citations                    0 \
        excluded_builtin_agents             {} \
        session_limits                      {} \
        memory                              {} \
        otlp_protocol                       "" \
        enable_web_socket_responses         0 \
        exp_assignments                     {} \
        on_mcp_auth_request                 "" \
        on_post_tool_use                    "" \
        on_pre_mcp_tool_call                "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown session config key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# -- Tool definition ----------------------------------------------------------

proc ::copilot::types::make_tool {name description handler {parameters {}}} {
    if {$name eq ""} {
        error "Tool name must not be empty"
    }
    if {$handler eq ""} {
        error "Tool handler must not be empty"
    }
    return [dict create \
        name        $name \
        description $description \
        handler     $handler \
        parameters  $parameters \
    ]
}

# -- Send options -------------------------------------------------------------

proc ::copilot::types::make_send_options {args} {
    set defaults [dict create \
        message         "" \
        prompt          "" \
        timeout         0 \
        agent_mode      "" \
        display_prompt  "" \
        request_headers {} \
    ]
    set opts $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown send option: $key"
        }
        dict set opts $key $value
    }
    # Allow "prompt" as an alias for "message"
    if {[dict get $opts message] eq "" && [dict get $opts prompt] ne ""} {
        dict set opts message [dict get $opts prompt]
    }
    return $opts
}

# -- Session event ------------------------------------------------------------

proc ::copilot::types::make_session_event {type {data {}}} {
    return [dict create \
        type $type \
        data $data \
        id "" \
        timestamp [clock milliseconds] \
        parentId "" \
        agentId "" \
        ephemeral 0 \
    ]
}

# -- Connection state ---------------------------------------------------------

proc ::copilot::types::connection_states {} {
    return {disconnected connecting connected error}
}

# -- Validation helper --------------------------------------------------------

proc ::copilot::types::validate_dict {d required_keys} {
    foreach key $required_keys {
        if {![dict exists $d $key]} {
            error "Missing required key: $key"
        }
    }
    return 1
}

# -- JSON schema helpers for tool parameters ----------------------------------

proc ::copilot::types::make_json_schema {type properties {required {}}} {
    set schema [dict create type $type properties $properties]
    if {[llength $required] > 0} {
        dict set schema required $required
    }
    return $schema
}

proc ::copilot::types::make_property {type description} {
    return [dict create type $type description $description]
}

# -- Result types -------------------------------------------------------------

proc ::copilot::types::make_ok {value} {
    return [dict create ok 1 value $value error ""]
}

proc ::copilot::types::make_error {message} {
    return [dict create ok 0 value "" error $message]
}

proc ::copilot::types::is_ok {result} {
    return [dict get $result ok]
}

proc ::copilot::types::unwrap {result} {
    if {![dict get $result ok]} {
        error [dict get $result error]
    }
    return [dict get $result value]
}

# -- Session filesystem configuration ----------------------------------------

proc ::copilot::types::make_session_fs_config {args} {
    set defaults [dict create \
        initial_cwd         "" \
        session_state_path  "" \
        conventions         "posix" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown session_fs config key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# -- MCP server configuration ------------------------------------------------

proc ::copilot::types::make_mcp_server_config {args} {
    set defaults [dict create \
        type     "stdio" \
        command  "" \
        args     {} \
        url      "" \
        env      {} \
        headers  {} \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown mcp_server config key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# -- Command definition -------------------------------------------------------

proc ::copilot::types::make_command_definition {name description} {
    return [dict create name $name description $description]
}

# -- Upstream-sync feature types (parity with @github/copilot-sdk) ------------

# Per-session AI-credit budget; set max_ai_credits to cap spend.
proc ::copilot::types::make_session_limits_config {args} {
    set defaults [dict create \
        max_ai_credits "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown session_limits config key: $key"
        }
        dict set cfg $key $value
    }
    # Build the camelCase JSON-RPC wire dict.
    set wire [dict create]
    if {[dict get $cfg max_ai_credits] ne ""} {
        dict set wire maxAiCredits [dict get $cfg max_ai_credits]
    }
    return $wire
}

# Opt-in persistent session memory.
proc ::copilot::types::make_memory_configuration {args} {
    set defaults [dict create \
        enabled 0 \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown memory config key: $key"
        }
        dict set cfg $key $value
    }
    return [dict create enabled [dict get $cfg enabled]]
}

# Arguments passed to a BYOK bearer_token_provider (per-session scoping).
proc ::copilot::types::make_provider_token_args {session_id} {
    return [dict create sessionId $session_id]
}

# CopilotRequestHandler: intercepts outbound LLM inference HTTP/WebSocket
# requests. Provide a send_request proc/lambda; assign to the client option
# request_handler. BYOK providers may also set bearer_token_provider.
proc ::copilot::types::make_copilot_request_handler {{send_request ""}} {
    return [dict create send_request $send_request]
}

# Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto").
proc ::copilot::types::tool_defer {} {
    return [dict create AUTO "auto" NEVER "never"]
}

# System-message section identifiers (used with system-message overrides).
# The "preamble" section targets the identity preamble; the "preserve" action
# protects an individually-addressable section from a group-level remove.
proc ::copilot::types::system_message_section {} {
    return [dict create \
        PREAMBLE          "preamble" \
        IDENTITY          "identity" \
        TOOL_INSTRUCTIONS "tool_instructions" \
        PRESERVE          "preserve" \
    ]
}

# GitHub-anchored attachment variants.
proc ::copilot::types::github_attachment {} {
    return [dict create \
        GITHUB_COMMIT          "GitHubCommit" \
        GITHUB_RELEASE         "GitHubRelease" \
        GITHUB_ACTIONS_JOB     "GitHubActionsJob" \
        GITHUB_REPOSITORY      "GitHubRepository" \
        GITHUB_FILE_DIFF       "GitHubFileDiff" \
        GITHUB_TREE_COMPARISON "GitHubTreeComparison" \
        GITHUB_URL             "GitHubUrl" \
        GITHUB_FILE            "GitHubFile" \
        GITHUB_SNIPPET         "GitHubSnippet" \
    ]
}

package provide copilot::types 2.0.0

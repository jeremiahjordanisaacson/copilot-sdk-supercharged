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
                     make_exit_plan_mode_request make_exit_plan_mode_response \
                     make_trace_context \
                     make_slash_command_input make_slash_command_info \
                     make_commands_invoke_request make_commands_list_request \
                     make_model_billing_token_prices make_skills_load_diagnostics \
                     slash_command_input_completions slash_command_kinds \
                     model_picker_price_categories
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
        remote                          0 \
        on_get_trace_context            "" \
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
        enable_session_telemetry            0 \
        on_exit_plan_mode                   "" \
        on_get_trace_context                "" \
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
        message  "" \
        prompt   "" \
        timeout  0 \
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

# -- Slash command input completion constants ---------------------------------

proc ::copilot::types::slash_command_input_completions {} {
    return {directory}
}

# -- Slash command kind constants ---------------------------------------------

proc ::copilot::types::slash_command_kinds {} {
    return {builtin client skill}
}

# -- Model picker price category constants ------------------------------------

proc ::copilot::types::model_picker_price_categories {} {
    return {high low medium very_high}
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

# -- Slash command input ------------------------------------------------------

proc ::copilot::types::make_slash_command_input {args} {
    set defaults [dict create \
        hint       "" \
        completion "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown slash_command_input key: $key"
        }
        dict set cfg $key $value
    }
    if {[dict get $cfg hint] eq ""} {
        error "hint is required for slash_command_input"
    }
    return $cfg
}

# -- Slash command info -------------------------------------------------------

proc ::copilot::types::make_slash_command_info {args} {
    set defaults [dict create \
        allowDuringAgentExecution 0 \
        description               "" \
        kind                      "builtin" \
        name                      "" \
        aliases                   {} \
        experimental              "" \
        input                     {} \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown slash_command_info key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# -- Commands invoke request --------------------------------------------------

proc ::copilot::types::make_commands_invoke_request {args} {
    set defaults [dict create \
        name  "" \
        input "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown commands_invoke_request key: $key"
        }
        dict set cfg $key $value
    }
    if {[dict get $cfg name] eq ""} {
        error "name is required for commands_invoke_request"
    }
    return $cfg
}

# -- Commands list request ----------------------------------------------------

proc ::copilot::types::make_commands_list_request {args} {
    set defaults [dict create \
        includeBuiltins       "" \
        includeClientCommands "" \
        includeSkills         "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown commands_list_request key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# -- Model billing token prices -----------------------------------------------

proc ::copilot::types::make_model_billing_token_prices {args} {
    set defaults [dict create \
        batchSize   "" \
        cachePrice  "" \
        inputPrice  "" \
        outputPrice "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown model_billing_token_prices key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# -- Exit plan mode types ---------------------------------------------------

proc ::copilot::types::make_exit_plan_mode_request {session_id} {
    return [dict create sessionId $session_id]
}

proc ::copilot::types::make_exit_plan_mode_response {approved} {
    return [dict create approved $approved]
}

# -- Trace context -----------------------------------------------------------

proc ::copilot::types::make_trace_context {args} {
    set defaults [dict create \
        traceparent "" \
        tracestate  "" \
    ]
    set ctx $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown trace context key: $key"
        }
        dict set ctx $key $value
    }
    return $ctx
}

# -- Experimental: Skills load diagnostics ------------------------------------

proc ::copilot::types::make_skills_load_diagnostics {args} {
    set defaults [dict create \
        errors   {} \
        warnings {} \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown skills_load_diagnostics key: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

package provide copilot::types 2.0.0

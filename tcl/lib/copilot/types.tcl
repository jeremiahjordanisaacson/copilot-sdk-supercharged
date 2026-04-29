# types.tcl - Type definitions and helper procs for the Copilot SDK for Tcl
#
# Types are represented as Tcl dicts. This module provides constructors and
# validators for the core data structures used throughout the SDK.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6

namespace eval ::copilot::types {
    namespace export make_client_options make_session_config make_tool \
                     make_send_options make_session_event validate_dict
}

# -- Client options -----------------------------------------------------------

proc ::copilot::types::make_client_options {args} {
    set defaults [dict create \
        cli_path    "" \
        cli_url     "" \
        log_level   "warn" \
        timeout     30000 \
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
        system_prompt  "" \
        github_token   "" \
        tools          {} \
        streaming      0 \
        model          "" \
        session_id     "" \
        state          {} \
        session_idle_timeout_seconds 0 \
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
        timestamp [clock milliseconds] \
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

package provide copilot::types 2.0.0

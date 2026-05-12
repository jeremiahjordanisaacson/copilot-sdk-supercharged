# session.tcl - Session management for the Copilot SDK for Tcl
#
# A CopilotSession wraps a single conversation. It sends messages through the
# client's JSON-RPC connection and dispatches incoming events to subscribers.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require copilot::types
package require copilot::jsonrpc
package require copilot::tools

namespace eval ::copilot::session {
    variable sessions [dict create]
    variable next_session_idx 0

    namespace export create_session destroy_session send_message \
                     send_and_wait on_event get_state session_info
}

# -- Internal: generate a local session handle --------------------------------

proc ::copilot::session::_next_handle {} {
    variable next_session_idx
    incr next_session_idx
    return "session_$next_session_idx"
}

# -- Create a session via JSON-RPC --------------------------------------------

proc ::copilot::session::create_session {write_ch read_ch config} {
    variable sessions

    set handle [_next_handle]

    # Build wire params
    set params [dict create]
    set sys_prompt [dict get $config system_prompt]
    if {$sys_prompt ne ""} {
        dict set params systemPrompt $sys_prompt
    }

    set token [dict get $config github_token]
    if {$token ne ""} {
        dict set params githubToken $token
    }

    set model [dict get $config model]
    if {$model ne ""} {
        dict set params model $model
    }

    set idle_timeout [dict get $config session_idle_timeout_seconds]
    if {$idle_timeout > 0} {
        dict set params sessionIdleTimeoutSeconds $idle_timeout
    }

    set streaming [dict get $config streaming]
    if {$streaming} {
        dict set params streaming true
    }

    set enable_telemetry [dict get $config enable_session_telemetry]
    if {$enable_telemetry} {
        dict set params enableSessionTelemetry true
    }

    set epm_handler [dict get $config on_exit_plan_mode]
    if {$epm_handler ne ""} {
        dict set params requestExitPlanMode true
    }

    # Include tools in session creation
    set wire_tools [::copilot::tools::tools_to_wire]
    if {[llength $wire_tools] > 0} {
        dict set params tools $wire_tools
    }

    # Send the createSession request
    set req_id [::copilot::jsonrpc::send_request $write_ch "session.create" $params]
    ::copilot::jsonrpc::register_pending $req_id

    # Read messages until we get our response
    set result [_wait_for_response $read_ch $write_ch $req_id]

    # Store session state
    set session_data [dict create \
        handle     $handle \
        session_id "" \
        write_ch   $write_ch \
        read_ch    $read_ch \
        config     $config \
        listeners  {} \
        events     {} \
        state      {} \
        exit_plan_mode_handler "" \
        trace_context_provider "" \
    ]

    if {[dict exists $result sessionId]} {
        dict set session_data session_id [dict get $result sessionId]
    }

    dict set sessions $handle $session_data
    return $handle
}

# -- Destroy a session --------------------------------------------------------

proc ::copilot::session::destroy_session {handle} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        return
    }

    set sdata [dict get $sessions $handle]
    set write_ch [dict get $sdata write_ch]
    set read_ch  [dict get $sdata read_ch]
    set sid [dict get $sdata session_id]

    if {$sid ne ""} {
        catch {
            set params [dict create sessionId $sid]
            ::copilot::jsonrpc::send_notification $write_ch "session/destroy" $params
        }
    }

    dict unset sessions $handle
}

# -- Send a message and wait for the final response ---------------------------

proc ::copilot::session::send_and_wait {handle opts} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }

    set sdata [dict get $sessions $handle]
    set write_ch [dict get $sdata write_ch]
    set read_ch  [dict get $sdata read_ch]
    set sid      [dict get $sdata session_id]

    set message [dict get $opts message]
    if {$message eq ""} {
        error "Message must not be empty"
    }

    set params [dict create sessionId $sid message $message]

    # Inject trace context if provider is available
    set sdata_tc [dict get $sessions $handle]
    set tc_provider [dict get $sdata_tc trace_context_provider]
    if {$tc_provider ne ""} {
        if {![catch {set tc [uplevel #0 $tc_provider]} err]} {
            if {[dict exists $tc traceparent] && [dict get $tc traceparent] ne ""} {
                dict set params traceparent [dict get $tc traceparent]
            }
            if {[dict exists $tc tracestate] && [dict get $tc tracestate] ne ""} {
                dict set params tracestate [dict get $tc tracestate]
            }
        }
    }

    set req_id [::copilot::jsonrpc::send_request $write_ch "session/sendMessage" $params]
    ::copilot::jsonrpc::register_pending $req_id

    set result [_wait_for_response $read_ch $write_ch $req_id]

    # Fire listeners
    set event [::copilot::types::make_session_event "response" $result]
    _fire_listeners $handle $event

    return $result
}

# -- Send a message (fire-and-forget) -----------------------------------------

proc ::copilot::session::send_message {handle message} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }

    set sdata [dict get $sessions $handle]
    set write_ch [dict get $sdata write_ch]
    set sid [dict get $sdata session_id]

    set params [dict create sessionId $sid message $message]
    ::copilot::jsonrpc::send_notification $write_ch "session/sendMessage" $params
}

# -- Register an event listener -----------------------------------------------

proc ::copilot::session::on_event {handle callback} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }

    set sdata [dict get $sessions $handle]
    set listeners [dict get $sdata listeners]
    lappend listeners $callback
    dict set sdata listeners $listeners
    dict set sessions $handle $sdata

    # Return an unsubscribe proc
    set idx [expr {[llength $listeners] - 1}]
    return [list apply {{handle idx} {
        variable sessions
        if {[dict exists $sessions $handle]} {
            set sdata [dict get $sessions $handle]
            set listeners [dict get $sdata listeners]
            set listeners [lreplace $listeners $idx $idx {}]
            dict set sdata listeners $listeners
            dict set sessions $handle $sdata
        }
    } ::copilot::session} $handle $idx]
}

# -- Get session state --------------------------------------------------------

proc ::copilot::session::get_state {handle} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }
    set sdata [dict get $sessions $handle]
    return [dict get $sdata state]
}

# -- Session info accessor ----------------------------------------------------

proc ::copilot::session::session_info {handle} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }
    set sdata [dict get $sessions $handle]
    return [dict create \
        handle     $handle \
        session_id [dict get $sdata session_id] \
        config     [dict get $sdata config] \
    ]
}

# -- Internal: fire all listeners for a session event -------------------------

proc ::copilot::session::_fire_listeners {handle event} {
    variable sessions
    if {![dict exists $sessions $handle]} { return }

    set sdata [dict get $sessions $handle]
    foreach cb [dict get $sdata listeners] {
        if {$cb eq ""} { continue }
        catch {uplevel #0 [list {*}$cb $event]}
    }
}

# -- Internal: wait for a JSON-RPC response, handling requests in between -----

proc ::copilot::session::_wait_for_response {read_ch write_ch req_id} {
    while {![::copilot::jsonrpc::is_resolved $req_id]} {
        set msg [::copilot::jsonrpc::read_message $read_ch]
        if {$msg eq ""} {
            ::copilot::jsonrpc::reject_pending $req_id "Connection closed"
            break
        }

        # Check if it is a response to our request
        if {[dict exists $msg id]} {
            set msg_id [dict get $msg id]
            if {[dict exists $msg result]} {
                ::copilot::jsonrpc::resolve_pending $msg_id [dict get $msg result]
            } elseif {[dict exists $msg error]} {
                set err_info [dict get $msg error]
                set err_msg "JSON-RPC error"
                if {[dict exists $err_info message]} {
                    set err_msg [dict get $err_info message]
                }
                ::copilot::jsonrpc::reject_pending $msg_id $err_msg
            }
            continue
        }

        # It is a notification or server-initiated request
        if {[dict exists $msg method]} {
            set method [dict get $msg method]
            set params {}
            if {[dict exists $msg params]} {
                set params [dict get $msg params]
            }

            # Handle tool call requests from the server
            if {$method eq "toolCall"} {
                _handle_tool_call $write_ch $msg
                continue
            }

            if {$method eq "exitPlanMode.request"} {
                _handle_exit_plan_mode $write_ch $msg
                continue
            }

            # Dispatch other notifications
            ::copilot::jsonrpc::dispatch_notification $method $params
        }
    }

    set result [::copilot::jsonrpc::get_result $req_id]
    ::copilot::jsonrpc::remove_pending $req_id
    return $result
}

# -- Internal: handle a tool call from the server -----------------------------

proc ::copilot::session::_handle_tool_call {write_ch msg} {
    set call_id ""
    if {[dict exists $msg id]} {
        set call_id [dict get $msg id]
    }

    set params [dict get $msg params]
    set tool_name ""
    if {[dict exists $params name]} {
        set tool_name [dict get $params name]
    }
    set tool_params {}
    if {[dict exists $params arguments]} {
        set tool_params [dict get $params arguments]
    }

    set invoke_result [::copilot::tools::invoke_tool $tool_name $tool_params]

    if {[::copilot::types::is_ok $invoke_result]} {
        set tool_result [::copilot::types::unwrap $invoke_result]
        set response_payload [dict create content $tool_result]
        ::copilot::jsonrpc::send_response $write_ch $call_id $response_payload
    } else {
        set err_msg [dict get $invoke_result error]
        ::copilot::jsonrpc::send_error_response $write_ch $call_id -32000 $err_msg
    }
}

# -- Register exit plan mode handler -----------------------------------------

proc ::copilot::session::register_exit_plan_mode_handler {handle handler} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }
    set sdata [dict get $sessions $handle]
    dict set sdata exit_plan_mode_handler $handler
    dict set sessions $handle $sdata
}

# -- Register trace context provider ------------------------------------------

proc ::copilot::session::register_trace_context_provider {handle provider} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }
    set sdata [dict get $sessions $handle]
    dict set sdata trace_context_provider $provider
    dict set sessions $handle $sdata
}

# -- Internal: handle exit plan mode request from server ----------------------

proc ::copilot::session::_handle_exit_plan_mode {write_ch msg} {
    variable sessions

    set call_id ""
    if {[dict exists $msg id]} {
        set call_id [dict get $msg id]
    }

    set params [dict get $msg params]
    set session_id ""
    if {[dict exists $params sessionId]} {
        set session_id [dict get $params sessionId]
    }

    # Find session by server session_id
    set handler ""
    dict for {handle sdata} $sessions {
        if {[dict get $sdata session_id] eq $session_id} {
            set handler [dict get $sdata exit_plan_mode_handler]
            break
        }
    }

    set result [dict create approved 1]
    if {$handler ne ""} {
        if {[catch {set result [uplevel #0 [list {*}$handler $params]]} err]} {
            set result [dict create approved 1]
        }
    }

    if {$call_id ne ""} {
        ::copilot::jsonrpc::send_response $write_ch $call_id $result
    }
}

package provide copilot::session 2.0.0

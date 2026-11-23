# client.tcl - Main client for the Copilot SDK for Tcl
#
# CopilotClient manages the connection to the Copilot CLI server. It spawns the
# CLI as a child process (via "open |...") or connects to an existing server URL.
# Sessions are created through this client.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require copilot::version
package require copilot::types
package require copilot::jsonrpc
package require copilot::session
package require copilot::tools

namespace eval ::copilot::client {
    variable clients [dict create]
    variable next_client_idx 0

    namespace export new start stop create_session resume_session \
                     delete_session list_sessions destroy_session \
                     define_tool get_status get_auth_status ping \
                     list_models get_last_session_id get_session_metadata \
                     set_session_fs_provider clear_managed_settings_cache
}

# ---- Resolve the CLI path ---------------------------------------------------

proc ::copilot::client::_resolve_cli_path {opts} {
    set cli_path [dict get $opts cli_path]
    if {$cli_path ne ""} {
        return $cli_path
    }

    # Check environment variable (used when the file exists)
    set env_path ""
    catch { set env_path $::env(COPILOT_CLI_PATH) }
    if {$env_path ne "" && [file exists $env_path]} {
        return $env_path
    }

    # Try default "copilot" on PATH
    if {$::tcl_platform(platform) eq "windows"} {
        set candidates [list "copilot.exe" "copilot"]
    } else {
        set candidates [list "copilot"]
    }

    foreach cmd $candidates {
        if {![catch {exec which $cmd} result]} {
            return [string trim $result]
        }
        if {![catch {exec where.exe $cmd} result]} {
            return [lindex [split [string trim $result] "\n"] 0]
        }
    }

    error "Cannot find copilot CLI. Set cli_path, COPILOT_CLI_PATH, or add copilot to PATH."
}

# ---- Create a new client instance -------------------------------------------

proc ::copilot::client::new {args} {
    variable clients
    variable next_client_idx

    set opts [::copilot::types::make_client_options {*}$args]
    incr next_client_idx
    set handle "client_$next_client_idx"

    dict set clients $handle [dict create \
        opts         $opts \
        state        "disconnected" \
        pid          "" \
        write_ch     "" \
        read_ch      "" \
        process_ch   "" \
        sessions     {} \
    ]

    return $handle
}

# ---- Start the client (spawn CLI or connect to URL) -------------------------

proc ::copilot::client::start {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set opts  [dict get $cdata opts]

    dict set cdata state "connecting"
    dict set clients $handle $cdata

    set cli_url [dict get $opts cli_url]
    if {$cli_url ne ""} {
        # Connect to external server via socket
        _connect_url $handle $cli_url
    } else {
        # Spawn CLI process
        _spawn_process $handle
    }

    # Perform initialization handshake
    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    _do_initialize $handle $write_ch $read_ch

    dict set cdata state "connected"
    dict set clients $handle $cdata

    # Set up session filesystem if configured
    set session_fs [dict get $opts session_fs]
    if {$session_fs ne "" && [dict size $session_fs] > 0} {
        set_session_fs_provider $handle $session_fs
    }

    return $handle
}

# ---- Spawn the CLI process --------------------------------------------------

proc ::copilot::client::_spawn_process {handle} {
    variable clients
    set cdata [dict get $clients $handle]
    set opts  [dict get $cdata opts]

    set cli_path [_resolve_cli_path $opts]
    set log_level [dict get $opts log_level]

    # A .js entry point (for example npm-loader.js) is run with node; other paths
    # (such as node_modules/.bin/copilot) are executed directly.
    if {[string match -nocase *.js $cli_path]} {
        set cmd [list node $cli_path --headless --no-auto-update --stdio]
    } else {
        set cmd [list $cli_path --headless --no-auto-update --stdio]
    }
    if {$log_level ne ""} {
        lappend cmd "--log-level" $log_level
    }

    set remote [dict get $opts remote]
    if {$remote} {
        lappend cmd "--remote"
    }

    # Open bidirectional pipe to the CLI process
    set pipe [open "|$cmd" r+]
    fconfigure $pipe -translation binary -buffering full -blocking 1

    dict set cdata process_ch $pipe
    dict set cdata write_ch   $pipe
    dict set cdata read_ch    $pipe
    dict set cdata pid        [pid $pipe]
    dict set clients $handle $cdata
}

# ---- Connect to an existing server via URL (TCP) ----------------------------

proc ::copilot::client::_connect_url {handle url} {
    variable clients
    set cdata [dict get $clients $handle]

    # Parse host:port from url (expects http://host:port or just host:port)
    set url [string trimright $url "/"]
    regsub {^https?://} $url {} stripped
    set parts [split $stripped ":"]
    set host [lindex $parts 0]
    set port [lindex $parts 1]
    if {$port eq ""} {
        set port 80
    }

    set sock [socket $host $port]
    fconfigure $sock -translation binary -buffering full -blocking 1

    dict set cdata write_ch $sock
    dict set cdata read_ch  $sock
    dict set clients $handle $cdata
}

# ---- Initialization handshake -----------------------------------------------

# Declares the integrating application with connect (when client_info is set;
# CLIs without connect are tolerated), then verifies with ping that the server
# speaks exactly this SDK's protocol version, and registers network callbacks.
proc ::copilot::client::_do_initialize {handle write_ch read_ch} {
    variable clients
    set opts [dict get [dict get $clients $handle] opts]
    set expected [::copilot::version::get_protocol]

    set client_info [::copilot::types::client_info_json [dict get $opts client_info]]
    if {$client_info ne ""} {
        set params [::copilot::jsonrpc::json_raw "{\"clientInfo\":$client_info}"]
        if {[catch {_rpc $write_ch $read_ch "connect" $params} err err_opts]} {
            set last $::copilot::session::last_rpc_error
            set method_not_found [expr {[dict exists $last code] && [dict get $last code] == -32601}]
            if {!$method_not_found && ![string match "*Unhandled method connect*" $err]} {
                return -options $err_opts $err
            }
        }
    }

    set result [_rpc $write_ch $read_ch "ping" [dict create message "ping"]]
    if {![dict exists $result protocolVersion] || [dict get $result protocolVersion] eq "null"} {
        error "SDK protocol version mismatch: SDK supports version $expected, but server does not report a protocol version. Please update your server to ensure compatibility."
    }
    set server_version [dict get $result protocolVersion]
    if {$server_version != $expected} {
        error "SDK protocol version mismatch: SDK supports version $expected, but server reports version $server_version. Please update your SDK or server to ensure compatibility."
    }

    # Register network callbacks (network.setProvider) when any is configured
    set network [dict get $opts network]
    ::copilot::session::set_network_callbacks $network
    set provider [dict create]
    foreach {name wire} {resolve_proxy resolveProxy authenticate_negotiate authenticateNegotiate
            authenticate_basic authenticateBasic} {
        if {$network ne "" && [dict exists $network $name] && [dict get $network $name] ne ""} {
            dict set provider $wire "\{\}"
        }
    }
    if {[dict size $provider] > 0} {
        if {[dict exists $network scope] && [dict get $network scope] ne ""} {
            dict set provider scope [::copilot::jsonrpc::json_string [dict get $network scope]]
        }
        _rpc $write_ch $read_ch "network.setProvider" \
            [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $provider]]
    }

    return $result
}

# Send a request and wait for its result (raises on a JSON-RPC error).
proc ::copilot::client::_rpc {write_ch read_ch method params} {
    set req_id [::copilot::jsonrpc::send_request $write_ch $method $params]
    ::copilot::jsonrpc::register_pending $req_id
    return [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
}

# Send a request on a client's connection and wait for its result.
proc ::copilot::client::_client_rpc {handle method params} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }
    set cdata [dict get $clients $handle]
    return [_rpc [dict get $cdata write_ch] [dict get $cdata read_ch] $method $params]
}

# Clear the runtime's cached managed (enterprise) settings so they are re-read.
proc ::copilot::client::clear_managed_settings_cache {handle} {
    return [_client_rpc $handle "managedSettings.clearCache" [::copilot::jsonrpc::json_raw "\{\}"]]
}

# The PolicyHelperFailure dict carried by a failed create_session/resume_session
# error (catch it with an options variable), or "" when there is none.
proc ::copilot::client::policy_helper_failure {error_options} {
    if {![dict exists $error_options -errorcode]} {
        return ""
    }
    set code [dict get $error_options -errorcode]
    if {[lindex $code 0] eq "COPILOT" && [lindex $code 1] eq "POLICY_HELPER_FAILURE"} {
        return [lindex $code 2]
    }
    return ""
}

# ---- Stop the client --------------------------------------------------------

proc ::copilot::client::stop {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        return
    }

    set cdata [dict get $clients $handle]

    # Destroy all sessions
    foreach sess_handle [dict get $cdata sessions] {
        catch { ::copilot::session::destroy_session $sess_handle }
    }

    # Send shutdown request
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]
    if {$write_ch ne ""} {
        catch {
            ::copilot::jsonrpc::send_request $write_ch "shutdown" [dict create]
            ::copilot::jsonrpc::send_notification $write_ch "exit" [dict create]
        }
    }

    # Close channels
    set process_ch [dict get $cdata process_ch]
    if {$process_ch ne ""} {
        catch { close $process_ch }
    } else {
        catch { close $write_ch }
        if {$read_ch ne $write_ch} {
            catch { close $read_ch }
        }
    }

    dict set cdata state "disconnected"
    dict set cdata write_ch ""
    dict set cdata read_ch  ""
    dict set cdata process_ch ""
    dict set cdata pid ""
    dict set clients $handle $cdata
}

# ---- Create a session -------------------------------------------------------

proc ::copilot::client::create_session {handle args} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    if {[dict get $cdata state] ne "connected"} {
        error "Client is not connected. Call start first."
    }

    set config [::copilot::types::make_session_config {*}$args]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]
    set mode     [dict get [dict get $cdata opts] mode]

    # Delegates to session module which sends "session.create" via JSON-RPC
    set sess_handle [::copilot::session::create_session $write_ch $read_ch $config $mode]

    # Register exit plan mode handler if configured
    set epm_handler [dict get $config on_exit_plan_mode]
    if {$epm_handler ne ""} {
        ::copilot::session::register_exit_plan_mode_handler $sess_handle $epm_handler
    }

    # Register trace context provider if configured
    set tc_provider [dict get [dict get $cdata opts] on_get_trace_context]
    if {$tc_provider ne ""} {
        ::copilot::session::register_trace_context_provider $sess_handle $tc_provider
    }

    # Track the session in the client
    set sessions [dict get $cdata sessions]
    lappend sessions $sess_handle
    dict set cdata sessions $sessions
    dict set clients $handle $cdata

    return $sess_handle
}

# ---- Destroy a specific session ---------------------------------------------

proc ::copilot::client::destroy_session {handle sess_handle} {
    variable clients
    if {![dict exists $clients $handle]} { return }

    ::copilot::session::destroy_session $sess_handle

    set cdata [dict get $clients $handle]
    set sessions [dict get $cdata sessions]
    set idx [lsearch -exact $sessions $sess_handle]
    if {$idx >= 0} {
        set sessions [lreplace $sessions $idx $idx]
        dict set cdata sessions $sessions
        dict set clients $handle $cdata
    }
}

# ---- Define a tool (convenience wrapper) ------------------------------------

proc ::copilot::client::define_tool {name description handler args} {
    return [::copilot::tools::define_tool $name $description $handler {*}$args]
}

# ---- Get server status ------------------------------------------------------

proc ::copilot::client::get_status {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set req_id [::copilot::jsonrpc::send_request $write_ch "status.get" [dict create]]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- Get auth status --------------------------------------------------------

proc ::copilot::client::get_auth_status {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set req_id [::copilot::jsonrpc::send_request $write_ch "auth.getStatus" [dict create]]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- Get foreground session ID ----------------------------------------------

proc ::copilot::client::get_foreground_session_id {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set req_id [::copilot::jsonrpc::send_request $write_ch "session.getForeground" [dict create]]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]

    if {[dict exists $result sessionId]} {
        return [dict get $result sessionId]
    }
    error "No sessionId in response"
}

# ---- Set foreground session ID ----------------------------------------------

proc ::copilot::client::set_foreground_session_id {handle session_id} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set params [dict create sessionId $session_id]
    set req_id [::copilot::jsonrpc::send_request $write_ch "session.setForeground" $params]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]

    if {![dict exists $result success] || ![dict get $result success]} {
        set err_msg "Unknown"
        if {[dict exists $result error]} {
            set err_msg [dict get $result error]
        }
        error "Failed to set foreground session: $err_msg"
    }
}

# ---- Ping -------------------------------------------------------------------

proc ::copilot::client::ping {handle {message "ping"}} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set params [dict create message $message]
    set req_id [::copilot::jsonrpc::send_request $write_ch "ping" $params]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- List models ------------------------------------------------------------

proc ::copilot::client::list_models {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set req_id [::copilot::jsonrpc::send_request $write_ch "models.list" [dict create]]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- Resume a session -------------------------------------------------------

proc ::copilot::client::resume_session {handle session_id args} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    if {[dict get $cdata state] ne "connected"} {
        error "Client is not connected. Call start first."
    }

    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]
    set config   [::copilot::types::make_session_config {*}$args]
    set mode     [dict get [dict get $cdata opts] mode]

    # Sends the grouped SDK protocol 4 session.resume payload; the result also
    # carries "handle", the local session handle for the resumed session.
    set result [::copilot::session::resume_session $write_ch $read_ch $session_id $config $mode]

    set sessions [dict get $cdata sessions]
    lappend sessions [dict get $result handle]
    dict set cdata sessions $sessions
    dict set clients $handle $cdata
    return $result
}

# ---- Delete a session -------------------------------------------------------

proc ::copilot::client::delete_session {handle session_id} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    if {[dict get $cdata state] ne "connected"} {
        error "Client is not connected. Call start first."
    }

    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set params [dict create sessionId $session_id]
    set req_id [::copilot::jsonrpc::send_request $write_ch "session.delete" $params]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- List sessions ----------------------------------------------------------

proc ::copilot::client::list_sessions {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    if {[dict get $cdata state] ne "connected"} {
        error "Client is not connected. Call start first."
    }

    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set req_id [::copilot::jsonrpc::send_request $write_ch "session.list" [dict create]]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- Get last session ID ----------------------------------------------------

proc ::copilot::client::get_last_session_id {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set req_id [::copilot::jsonrpc::send_request $write_ch "session.getLastId" [dict create]]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]

    if {[dict exists $result sessionId]} {
        return [dict get $result sessionId]
    }
    return ""
}

# ---- Get session metadata ---------------------------------------------------

proc ::copilot::client::get_session_metadata {handle session_id} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set params [dict create sessionId $session_id]
    set req_id [::copilot::jsonrpc::send_request $write_ch "session.getMetadata" $params]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- Set session filesystem provider ----------------------------------------

proc ::copilot::client::set_session_fs_provider {handle config} {
    variable clients
    if {![dict exists $clients $handle]} {
        error "Client not found: $handle"
    }

    set cdata [dict get $clients $handle]
    set write_ch [dict get $cdata write_ch]
    set read_ch  [dict get $cdata read_ch]

    set params [dict create \
        initialCwd        [dict get $config initial_cwd] \
        sessionStatePath  [dict get $config session_state_path] \
        conventions       [dict get $config conventions] \
    ]
    set req_id [::copilot::jsonrpc::send_request $write_ch "sessionFs.setProvider" $params]
    ::copilot::jsonrpc::register_pending $req_id

    set result [::copilot::session::_wait_for_response $read_ch $write_ch $req_id]
    return $result
}

# ---- Get connection state ---------------------------------------------------

proc ::copilot::client::get_state {handle} {
    variable clients
    if {![dict exists $clients $handle]} {
        return "disconnected"
    }
    return [dict get [dict get $clients $handle] state]
}

# ---- Convenience: send_and_wait on a session --------------------------------

proc ::copilot::client::send_and_wait {sess_handle opts} {
    return [::copilot::session::send_and_wait $sess_handle $opts]
}

# ---- Convenience: register event listener on a session ----------------------

proc ::copilot::client::on_event {sess_handle callback} {
    return [::copilot::session::on_event $sess_handle $callback]
}

package provide copilot::client 2.0.0

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

    namespace export new start stop create_session destroy_session \
                     define_tool get_status get_auth_status ping \
                     list_models get_last_session_id get_session_metadata \
                     set_session_fs_provider
}

# ---- Resolve the CLI path ---------------------------------------------------

proc ::copilot::client::_resolve_cli_path {opts} {
    set cli_path [dict get $opts cli_path]
    if {$cli_path ne ""} {
        return $cli_path
    }

    # Check environment variable
    set env_path ""
    catch { set env_path $::env(COPILOT_CLI_PATH) }
    if {$env_path ne ""} {
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

    set cmd [list $cli_path --stdio]
    if {$log_level ne ""} {
        lappend cmd "--log-level" $log_level
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

proc ::copilot::client::_do_initialize {handle write_ch read_ch} {
    set sdk_version [::copilot::version::get]
    set sdk_name    [::copilot::version::get_name]
    set min_proto   [::copilot::version::get_min_protocol]

    set params [dict create \
        clientInfo [dict create \
            name    $sdk_name \
            version $sdk_version \
        ] \
    ]

    set req_id [::copilot::jsonrpc::send_request $write_ch "initialize" $params]
    ::copilot::jsonrpc::register_pending $req_id

    # Wait for the initialize response
    set timeout_ms 30000
    set start_time [clock milliseconds]

    while {![::copilot::jsonrpc::is_resolved $req_id]} {
        if {[clock milliseconds] - $start_time > $timeout_ms} {
            ::copilot::jsonrpc::reject_pending $req_id "Initialize timed out"
            break
        }

        set msg [::copilot::jsonrpc::read_message $read_ch]
        if {$msg eq ""} {
            ::copilot::jsonrpc::reject_pending $req_id "Connection closed during initialize"
            break
        }

        if {[dict exists $msg id]} {
            set msg_id [dict get $msg id]
            if {[dict exists $msg result]} {
                ::copilot::jsonrpc::resolve_pending $msg_id [dict get $msg result]
            } elseif {[dict exists $msg error]} {
                set err_info [dict get $msg error]
                set err_msg "Initialize error"
                if {[dict exists $err_info message]} {
                    set err_msg [dict get $err_info message]
                }
                ::copilot::jsonrpc::reject_pending $msg_id $err_msg
            }
        }
    }

    set result [::copilot::jsonrpc::get_result $req_id]
    ::copilot::jsonrpc::remove_pending $req_id

    # Verify protocol version
    if {[dict exists $result protocolVersion]} {
        set proto_ver [dict get $result protocolVersion]
        if {$proto_ver < $min_proto} {
            error "Server protocol version $proto_ver is below minimum $min_proto"
        }
    }

    # Send initialized notification
    ::copilot::jsonrpc::send_notification $write_ch "initialized" [dict create]

    return $result
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

    set sess_handle [::copilot::session::create_session $write_ch $read_ch $config]

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

    set req_id [::copilot::jsonrpc::send_request $write_ch "getStatus" [dict create]]
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

    set req_id [::copilot::jsonrpc::send_request $write_ch "getAuthStatus" [dict create]]
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

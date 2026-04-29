#!/usr/bin/env tclsh
# client_test.tcl - Test suite for the Copilot SDK for Tcl
#
# Uses the built-in tcltest framework. Run with: tclsh client_test.tcl
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require tcltest
namespace import ::tcltest::*

# Add SDK library to the package path
set script_dir [file dirname [file normalize [info script]]]
lappend auto_path [file join $script_dir .. lib]

# ---- Load packages ----------------------------------------------------------

test load-version "Load copilot::version" -body {
    package require copilot::version
    ::copilot::version::get
} -result "2.0.0"

test load-types "Load copilot::types" -body {
    package require copilot::types
    expr {[info commands ::copilot::types::make_client_options] ne ""}
} -result 1

test load-jsonrpc "Load copilot::jsonrpc" -body {
    package require copilot::jsonrpc
    expr {[info commands ::copilot::jsonrpc::json_encode] ne ""}
} -result 1

test load-tools "Load copilot::tools" -body {
    package require copilot::tools
    expr {[info commands ::copilot::tools::define_tool] ne ""}
} -result 1

test load-session "Load copilot::session" -body {
    package require copilot::session
    expr {[info commands ::copilot::session::create_session] ne ""}
} -result 1

test load-client "Load copilot::client" -body {
    package require copilot::client
    expr {[info commands ::copilot::client::new] ne ""}
} -result 1

# ---- Version tests ----------------------------------------------------------

test version-get "version::get returns SDK version" -body {
    ::copilot::version::get
} -result "2.0.0"

test version-name "version::get_name returns SDK name" -body {
    ::copilot::version::get_name
} -result "copilot-sdk-tcl"

test version-min-proto "version::get_min_protocol returns 2" -body {
    ::copilot::version::get_min_protocol
} -result 2

# ---- Types tests ------------------------------------------------------------

test types-client-opts-defaults "make_client_options with defaults" -body {
    set opts [::copilot::types::make_client_options]
    list [dict get $opts cli_path] [dict get $opts log_level] [dict get $opts timeout]
} -result [list "" "warn" 30000]

test types-client-opts-custom "make_client_options with custom values" -body {
    set opts [::copilot::types::make_client_options cli_path "/usr/bin/copilot" log_level "debug"]
    list [dict get $opts cli_path] [dict get $opts log_level]
} -result [list "/usr/bin/copilot" "debug"]

test types-client-opts-invalid "make_client_options rejects unknown keys" -body {
    catch {::copilot::types::make_client_options bogus_key "val"} err
    set err
} -match glob -result "Unknown client option: bogus_key"

test types-session-config-defaults "make_session_config with defaults" -body {
    set cfg [::copilot::types::make_session_config]
    list [dict get $cfg system_prompt] [dict get $cfg streaming]
} -result [list "" 0]

test types-session-config-custom "make_session_config with custom values" -body {
    set cfg [::copilot::types::make_session_config \
        system_prompt "Hello" streaming 1]
    list [dict get $cfg system_prompt] [dict get $cfg streaming]
} -result [list "Hello" 1]

test types-make-tool "make_tool creates a tool dict" -body {
    set t [::copilot::types::make_tool "calc" "calculator" myhandler]
    list [dict get $t name] [dict get $t description] [dict get $t handler]
} -result [list "calc" "calculator" "myhandler"]

test types-make-tool-empty-name "make_tool rejects empty name" -body {
    catch {::copilot::types::make_tool "" "desc" handler} err
    set err
} -match glob -result "Tool name must not be empty"

test types-send-opts "make_send_options with message" -body {
    set o [::copilot::types::make_send_options message "Hello"]
    dict get $o message
} -result "Hello"

test types-send-opts-prompt-alias "make_send_options prompt aliased to message" -body {
    set o [::copilot::types::make_send_options prompt "World"]
    dict get $o message
} -result "World"

test types-session-event "make_session_event" -body {
    set e [::copilot::types::make_session_event "response" [dict create content "Hi"]]
    list [dict get $e type] [dict exists $e timestamp]
} -result [list "response" 1]

test types-validate-dict "validate_dict passes with required keys" -body {
    ::copilot::types::validate_dict [dict create a 1 b 2] {a b}
} -result 1

test types-validate-dict-missing "validate_dict fails with missing key" -body {
    catch {::copilot::types::validate_dict [dict create a 1] {a b}} err
    set err
} -match glob -result "Missing required key: b"

test types-make-ok "make_ok wraps a value" -body {
    set r [::copilot::types::make_ok "hello"]
    list [::copilot::types::is_ok $r] [::copilot::types::unwrap $r]
} -result [list 1 "hello"]

test types-make-error "make_error wraps an error" -body {
    set r [::copilot::types::make_error "oops"]
    list [::copilot::types::is_ok $r] [dict get $r error]
} -result [list 0 "oops"]

test types-unwrap-error "unwrap on error result raises" -body {
    set r [::copilot::types::make_error "bad"]
    catch {::copilot::types::unwrap $r} err
    set err
} -result "bad"

test types-json-schema "make_json_schema builds a schema dict" -body {
    set props [dict create \
        name [::copilot::types::make_property "string" "The name"] \
    ]
    set schema [::copilot::types::make_json_schema "object" $props [list "name"]]
    list [dict get $schema type] [dict exists $schema required]
} -result [list "object" 1]

# ---- JSON-RPC tests ---------------------------------------------------------

test jsonrpc-encode-string "json_encode string" -body {
    ::copilot::jsonrpc::json_encode "hello world"
} -result {"hello world"}

test jsonrpc-encode-number "json_encode integer" -body {
    ::copilot::jsonrpc::json_encode 42
} -result 42

test jsonrpc-encode-bool "json_encode boolean" -body {
    list [::copilot::jsonrpc::json_encode true] [::copilot::jsonrpc::json_encode false]
} -result [list true false]

test jsonrpc-encode-null "json_encode null" -body {
    ::copilot::jsonrpc::json_encode null
} -result null

test jsonrpc-encode-dict "json_encode dict" -body {
    set d [dict create name "Alice" age 30]
    set json [::copilot::jsonrpc::json_encode $d]
    # Verify it is valid by decoding
    set decoded [::copilot::jsonrpc::json_decode $json]
    list [dict get $decoded name] [dict get $decoded age]
} -result [list "Alice" 30]

test jsonrpc-decode-object "json_decode object" -body {
    set d [::copilot::jsonrpc::json_decode {{"foo":"bar","num":42}}]
    list [dict get $d foo] [dict get $d num]
} -result [list "bar" 42]

test jsonrpc-decode-array "json_decode array" -body {
    set a [::copilot::jsonrpc::json_decode {[1,2,3]}]
    set a
} -result [list 1 2 3]

test jsonrpc-decode-nested "json_decode nested object" -body {
    set d [::copilot::jsonrpc::json_decode {{"a":{"b":"c"}}}]
    dict get [dict get $d a] b
} -result "c"

test jsonrpc-decode-escape "json_decode escaped strings" -body {
    set d [::copilot::jsonrpc::json_decode {{"msg":"hello\nworld"}}]
    string match "*\n*" [dict get $d msg]
} -result 1

test jsonrpc-decode-empty-object "json_decode empty object" -body {
    set d [::copilot::jsonrpc::json_decode {{}}]
    dict size $d
} -result 0

test jsonrpc-decode-empty-array "json_decode empty array" -body {
    set a [::copilot::jsonrpc::json_decode {[]}]
    llength $a
} -result 0

test jsonrpc-roundtrip "json_encode then json_decode roundtrip" -body {
    set orig [dict create name "Bob" count 7 active true]
    set json [::copilot::jsonrpc::json_encode $orig]
    set decoded [::copilot::jsonrpc::json_decode $json]
    list [dict get $decoded name] [dict get $decoded count] [dict get $decoded active]
} -result [list "Bob" 7 "true"]

test jsonrpc-encode-message "encode_message adds Content-Length header" -body {
    set body [dict create jsonrpc "2.0" method "test" params [dict create]]
    set encoded [::copilot::jsonrpc::encode_message $body]
    string match "Content-Length: *\r\n\r\n*" $encoded
} -result 1

test jsonrpc-next-id "next_request_id increments" -body {
    set id1 [::copilot::jsonrpc::next_request_id]
    set id2 [::copilot::jsonrpc::next_request_id]
    expr {$id2 == $id1 + 1}
} -result 1

# ---- Pending request tracking tests -----------------------------------------

test jsonrpc-pending-lifecycle "register, resolve, get, remove pending" -body {
    ::copilot::jsonrpc::register_pending 9999
    expr {![::copilot::jsonrpc::is_resolved 9999]}
} -result 1

test jsonrpc-pending-resolve "resolve pending request" -body {
    ::copilot::jsonrpc::register_pending 9998
    ::copilot::jsonrpc::resolve_pending 9998 [dict create value "ok"]
    set resolved [::copilot::jsonrpc::is_resolved 9998]
    set result [::copilot::jsonrpc::get_result 9998]
    ::copilot::jsonrpc::remove_pending 9998
    list $resolved [dict get $result value]
} -result [list 1 "ok"]

test jsonrpc-pending-reject "reject pending request" -body {
    ::copilot::jsonrpc::register_pending 9997
    ::copilot::jsonrpc::reject_pending 9997 "something failed"
    catch {::copilot::jsonrpc::get_result 9997} err
    ::copilot::jsonrpc::remove_pending 9997
    set err
} -result "something failed"

# ---- Notification handler tests ---------------------------------------------

test jsonrpc-notification-handler "register and dispatch notification" -body {
    set ::test_notification_value ""
    ::copilot::jsonrpc::register_handler "test/notify" [list apply {{params} {
        set ::test_notification_value [dict get $params msg]
    }}]
    ::copilot::jsonrpc::dispatch_notification "test/notify" [dict create msg "hello"]
    set result $::test_notification_value
    ::copilot::jsonrpc::unregister_handler "test/notify"
    set result
} -result "hello"

# ---- Tools tests ------------------------------------------------------------

test tools-define "define_tool registers a tool" -body {
    ::copilot::tools::clear_tools
    ::copilot::tools::define_tool "greet" "Says hello" [list apply {{params} {
        return "Hello, [dict get $params name]!"
    }}]
    expr {"greet" in [::copilot::tools::list_tools]}
} -result 1

test tools-invoke "invoke_tool calls the handler" -body {
    ::copilot::tools::clear_tools
    ::copilot::tools::define_tool "echo" "Echoes input" [list apply {{params} {
        return [dict get $params text]
    }}]
    set result [::copilot::tools::invoke_tool "echo" [dict create text "hi"]]
    ::copilot::types::unwrap $result
} -result "hi"

test tools-invoke-unknown "invoke_tool returns error for unknown tool" -body {
    set result [::copilot::tools::invoke_tool "nonexistent" [dict create]]
    ::copilot::types::is_ok $result
} -result 0

test tools-remove "remove_tool removes a registered tool" -body {
    ::copilot::tools::clear_tools
    ::copilot::tools::define_tool "temp" "Temp tool" [list apply {{params} { return "ok" }}]
    ::copilot::tools::remove_tool "temp"
    expr {"temp" in [::copilot::tools::list_tools]}
} -result 0

test tools-clear "clear_tools removes all tools" -body {
    ::copilot::tools::define_tool "a" "A" [list apply {{params} { return "a" }}]
    ::copilot::tools::define_tool "b" "B" [list apply {{params} { return "b" }}]
    ::copilot::tools::clear_tools
    llength [::copilot::tools::list_tools]
} -result 0

test tools-to-wire "tools_to_wire produces wire format" -body {
    ::copilot::tools::clear_tools
    ::copilot::tools::define_tool "calc" "Calculator" [list apply {{p} { return "ok" }}]
    set wire [::copilot::tools::tools_to_wire]
    set first [lindex $wire 0]
    list [dict get $first name] [dict get $first description]
} -result [list "calc" "Calculator"]

# ---- Client tests (unit-level, no CLI required) -----------------------------

test client-new "client::new creates a client handle" -body {
    set h [::copilot::client::new]
    string match "client_*" $h
} -result 1

test client-state-initial "new client is disconnected" -body {
    set h [::copilot::client::new]
    ::copilot::client::get_state $h
} -result "disconnected"

test client-state-unknown "unknown client returns disconnected" -body {
    ::copilot::client::get_state "bogus_handle"
} -result "disconnected"

# ---- Connection state type test ---------------------------------------------

test types-connection-states "connection_states returns valid states" -body {
    set states [::copilot::types::connection_states]
    expr {[llength $states] == 4 && "connected" in $states}
} -result 1

# ---- Summary ----------------------------------------------------------------

puts ""
puts "=== Test Summary ==="
cleanupTests

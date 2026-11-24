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
} -result "3.1.0"

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
} -result "3.1.0"

test version-name "version::get_name returns SDK name" -body {
    ::copilot::version::get_name
} -result "copilot-sdk-tcl"

test version-min-proto "version::get_min_protocol returns 4" -body {
    ::copilot::version::get_min_protocol
} -result 4

test version-proto "version::get_protocol returns SDK protocol 4" -body {
    ::copilot::version::get_protocol
} -result 4

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

# ---- SDK protocol 4 payload translation --------------------------------------

proc ::test_decode {json} {
    return [::copilot::jsonrpc::json_decode $json]
}

test group-spec-example "flat resume payload becomes grouped v4 payload" -body {
    set flat [dict create \
        sessionId     {"s1"} \
        model         {"m"} \
        streaming     true \
        requestPermission true \
        tools         {[{"name":"t","description":"d"}]} \
        hooks         true \
        disableResume true \
    ]
    set d [::test_decode [::copilot::session::group_session_params $flat 1]]
    list [lsort [dict keys $d]] [dict get $d events streaming] [dict get $d permissions request] \
        [dict get [lindex [dict get $d tools definitions] 0] name] [dict get $d hooks hasHandlers] \
        [dict get $d suppressResumeEvent]
} -result [list {events hooks model permissions sessionId suppressResumeEvent tools} true true t true true]

test group-create-only "create drops resume-only members and renames configDir" -body {
    set flat [dict create model {"m"} disableResume true openCanvases {[]} \
        trustWorkingDirectory {"session"} configDir {"/cfg"} otlpProtocol {"grpc"}]
    set d [::test_decode [::copilot::session::group_session_params $flat 0]]
    list [lsort [dict keys $d]] [dict get $d filesystem trustWorkingDirectory] \
        [dict get $d configDirectory] [dict get $d otlpProtocol]
} -result [list {configDirectory filesystem model otlpProtocol} session /cfg grpc]

test group-resume-only "resume drops create-only members and keeps canvases.open" -body {
    set flat [dict create sessionId {"s"} trustWorkingDirectory {"session"} \
        refreshCustomInstructions true openCanvases {["c"]} allowTranscriptRecovery true]
    set d [::test_decode [::copilot::session::group_session_params $flat 1]]
    list [lsort [dict keys $d]] [dict get $d canvases open] [dict get $d allowTranscriptRecovery]
} -result [list {allowTranscriptRecovery canvases sessionId} c true]

test group-legacy-provider "legacy provider becomes the default named connection" -body {
    set flat [dict create model {"gpt"} \
        provider {{"type":"openai","baseUrl":"http://x/v1","apiKey":"k","maxOutputTokens":10}} \
        capi {{"autoTier":"fast"}}]
    set d [::test_decode [::copilot::session::group_session_params $flat 0]]
    set provider [lindex [dict get $d inference providers] 0]
    set model [lindex [dict get $d inference models] 0]
    list [dict get $provider name] [dict get $provider baseUrl] [dict exists $provider maxOutputTokens] \
        [dict get $d inference defaultProvider] [dict get $model provider] [dict get $model id] \
        [dict get $model maxOutputTokens] [dict get $d github capi enabled] [dict get $d github capi autoTier] \
        [dict exists $d provider]
} -result [list default http://x/v1 0 default default gpt 10 false fast 0]

test group-explicit-providers "explicit providers win over the legacy provider" -body {
    set flat [dict create provider {{"type":"openai","baseUrl":"http://x"}} \
        providers {[{"name":"gw","baseUrl":"http://y"}]}]
    set d [::test_decode [::copilot::session::group_session_params $flat 0]]
    list [dict get [lindex [dict get $d inference providers] 0] name] [dict exists $d github]
} -result [list gw 0]

test group-null-default-provider "null defaultProvider is kept to clear the saved default" -body {
    set json [::copilot::session::group_session_params [dict create sessionId {"s"} defaultProvider null] 1]
    string match {*"inference":{"defaultProvider":null}*} $json
} -result 1

test group-mcp-env-value-mode "mcp servers get envValueMode direct" -body {
    set flat [dict create mcpServers {{"a":{"command":"x","tools":["*"]}}}]
    set d [::test_decode [::copilot::session::group_session_params $flat 0]]
    dict get $d mcp envValueMode
} -result direct

test group-empty-mode "empty mode defaults apply under explicit options" -body {
    set flat [dict create availableTools {["view"]} enableExperimentalMode true]
    set d [::test_decode [::copilot::session::group_session_params $flat 0 1]]
    list [dict get $d experiments enableExperimentalMode] [dict get $d github telemetryEnabled] \
        [dict get $d mcp oauthTokenStorage] [dict get $d state memory enabled] \
        [dict get $d git enableHostGitOperations] [dict get $d tools availableTools]
} -result [list true false in-memory false false view]

test group-flat-builder "session config options map to v4 groups" -body {
    ::copilot::tools::clear_tools
    ::copilot::tools::define_tool "lookup" "Look up" [list apply {{p} { return "ok" }}] \
        [::copilot::jsonrpc::json_raw {{"type":"object"}}] 1
    set cfg [::copilot::types::make_session_config \
        model "m" streaming 1 available_tools {view grep} ask_user_variant "elicitation" \
        sandbox_config [::copilot::types::make_sandbox_config allow_bypass 1] \
        feature_flags {my_flag 1} allowed_models {m} hook_directories {./hooks} \
        image_generation [::copilot::types::make_image_generation_config save_to_disk 0] \
        autopilot [::copilot::types::make_autopilot_options max_continues 3] \
        model_capabilities [::copilot::types::make_model_capabilities_limits max_output_tokens 4096] \
        auth_client_id_metadata_url "https://example.com/m.json" \
        hooks [dict create on_subagent_start ::noop] \
        store_memory ::noop on_shell_authentication_request ::noop \
        providers [list [::copilot::types::make_named_provider_config name gw base_url http://y api_key_command "echo k"]] \
        default_provider gw capi [dict create enabled false]]
    set flat [::copilot::session::_session_flat_params $cfg 0]
    set d [::test_decode [::copilot::session::group_session_params $flat 0]]
    ::copilot::tools::clear_tools
    list [dict get $d events streaming] [dict get $d tools availableTools] \
        [dict get $d interaction askUserVariant] [dict get $d permissions sandbox allowBypass] \
        [dict get $d experiments flags my_flag] [dict get $d policy allowedModels] \
        [dict get $d hooks directories] [dict get $d hooks hasHandlers] \
        [dict get $d tools imageGeneration saveToDisk] [dict get $d execution autopilot maxContinues] \
        [dict get $d inference capabilities limits max_output_tokens] \
        [dict get $d mcp authClientIdMetadataUrl] [dict get $d state hasMemoryStore] \
        [dict get $d interaction requestShellAuthentication] \
        [dict get [lindex [dict get $d inference providers] 0] apiKeyCommand] \
        [dict get $d inference defaultProvider] [dict get $d github capi enabled] \
        [dict get [lindex [dict get $d tools definitions] 0] idempotent]
} -result [list true {view grep} elicitation true true m ./hooks true false 3 4096 \
    https://example.com/m.json true true {echo k} gw false true]

test group-legacy-provider-builder "make_provider_config feeds the legacy translation" -body {
    set cfg [::copilot::types::make_session_config model "dep" \
        provider [::copilot::types::make_provider_config type openai base_url http://x/v1 \
            api_key_command "echo k" wire_model dep]]
    set d [::test_decode [::copilot::session::group_session_params \
        [::copilot::session::_session_flat_params $cfg 0] 0]]
    list [dict get [lindex [dict get $d inference providers] 0] apiKeyCommand] \
        [dict get [lindex [dict get $d inference models] 0] wireModel] [dict get $d github capi enabled]
} -result [list {echo k} dep false]

test empty-mode-requires-tools "empty mode requires available_tools" -body {
    catch {::copilot::session::_require_available_tools \
        [::copilot::types::make_session_config] empty} err
    set err
} -match glob -result "*requires available_tools*"

# ---- Typed JSON helpers -------------------------------------------------------

test json-object-members "json_object_members keeps member JSON text" -body {
    set m [::copilot::jsonrpc::json_object_members {{"a": "x,y", "b":{"c":[1,2]}, "d":true}}]
    list [dict get $m a] [dict get $m b] [dict get $m d]
} -result [list {"x,y"} {{"c":[1,2]}} true]

test json-params "params_json encodes a flat dict as an object" -body {
    ::copilot::jsonrpc::params_json [dict create sessionId abc message "hello world"]
} -result {{"sessionId":"abc","message":"hello world"}}

test send-params "send params carry source, clientTurnId, and the response schema" -body {
    set ::copilot::session::sessions [dict create h1 [dict create session_id s1]]
    set opts [::copilot::types::make_send_options message "Hi" \
        source [::copilot::types::message_source_agent bot] client_turn_id t-1 \
        response_schema [::copilot::jsonrpc::json_raw {{"type":"object"}}]]
    set d [::test_decode [lindex [::copilot::session::_send_params h1 $opts] 1]]
    set ::copilot::session::sessions [dict create]
    list [dict get $d prompt] [dict get $d source] [dict get $d clientTurnId] \
        [dict get $d responseFormat type] [dict get $d responseFormat jsonSchema strict] \
        [dict get $d responseFormat jsonSchema schema type]
} -result [list Hi agent-bot t-1 json_schema true object]

test shell-auth-reply "shell authentication replies accept only string answers" -body {
    proc ::test_auth {request context} {
        return [dict create action accept value "s3cret" extra 1]
    }
    set ::copilot::session::sessions [dict create h1 [dict create session_id s1 \
        config [::copilot::types::make_session_config on_shell_authentication_request ::test_auth]]]
    set accepted [::copilot::session::_shell_authentication_reply \
        [dict create sessionId s1 requestId r1 program sudo prompt pw inputKind secret]]
    set bad_kind [::copilot::session::_shell_authentication_reply \
        [dict create sessionId s1 requestId r1 program sudo prompt pw inputKind bogus]]
    set ::copilot::session::sessions [dict create]
    list $accepted $bad_kind
} -result [list {{"action":"accept","value":"s3cret"}} {{"action":"cancel"}}]

test permission-decisions "permission handler results map to protocol 4 decisions" -body {
    list [::copilot::session::permission_decision_json approve] \
        [::copilot::session::permission_decision_json approved] \
        [::copilot::session::permission_decision_json 0] \
        [::copilot::session::permission_decision_json [dict create kind reject feedback "Not now"]] \
        [::copilot::session::permission_decision_json denied-by-rules] \
        [::copilot::session::permission_decision_json no-result]
} -result [list {{"kind":"approve-once"}} {{"kind":"approve-once"}} {{"kind":"reject"}} \
    {{"kind":"reject","feedback":"Not now"}} {{"kind":"user-not-available"}} {}]

test permission-wire-flags "permission handler and skip_permission reach the wire" -body {
    ::copilot::tools::clear_tools
    ::copilot::tools::define_tool "lookup" "Look up" [list apply {{p} { return "ok" }}] {} "" 1
    set cfg [::copilot::types::make_session_config model "m" \
        on_permission_request ::copilot::session::approve_all]
    set d [::test_decode [::copilot::session::group_session_params \
        [::copilot::session::_session_flat_params $cfg 0] 0]]
    ::copilot::tools::clear_tools
    list [dict get $d permissions request] \
        [dict get [lindex [dict get $d tools definitions] 0] skipPermission]
} -result [list true true]

# ---- v3.1.0: MCP secrets, extension approvals, ask-user images -----------------

proc ::test_stub_client_rpc {results} {
    rename ::copilot::client::_client_rpc ::copilot::client::_test_client_rpc
    proc ::copilot::client::_client_rpc {handle method params} {
        lappend ::test_rpc_calls $method [::copilot::jsonrpc::params_json $params]
        return [dict get $::test_rpc_results $method]
    }
    set ::test_rpc_calls {}
    set ::test_rpc_results $results
}

proc ::test_restore_client_rpc {} {
    rename ::copilot::client::_client_rpc {}
    rename ::copilot::client::_test_client_rpc ::copilot::client::_client_rpc
}

test server-rpc-wire "MCP secret and extension approval procs send camelCase params" -setup {
    ::test_stub_client_rpc [dict create \
        mcp.secrets.save [dict create usedKeychain true] \
        mcp.secrets.delete null \
        permissions.extensionApprovals.revoke [dict create revoked false]]
} -body {
    set saved [::copilot::client::mcp_secrets_save c1 my-token "s3cret value" /cfg]
    set deleted [::copilot::client::mcp_secrets_delete c1 my-token]
    set revoked [::copilot::client::extension_approvals_revoke c1 plugin:p:e key-1 s1]
    list $::test_rpc_calls $saved $deleted $revoked
} -cleanup {
    ::test_restore_client_rpc
} -result [list [list \
    mcp.secrets.save {{"secretId":"my-token","value":"s3cret value","configDir":"/cfg"}} \
    mcp.secrets.delete {{"secretId":"my-token"}} \
    permissions.extensionApprovals.revoke \
        {{"extensionName":"plugin:p:e","installationKey":"key-1","sessionId":"s1"}}] \
    {used_keychain 1} {} {revoked 0}]

test extension-approvals-results "grant and list results become snake_case approval dicts" -setup {
    set approval [::test_decode {{"extensionName":"plugin:p:e","pluginName":"p",
        "capabilities":["network","fs"],"grantedAt":"2026-11-24T00:00:00Z",
        "installationKey":"key-1"}}]
    ::test_stub_client_rpc [dict create \
        permissions.extensionApprovals.grant [dict create granted true approval $approval] \
        permissions.extensionApprovals.list [dict create approvals [list $approval]]]
} -body {
    set grant [::copilot::client::extension_approvals_grant c1 s1 req-1]
    set first [lindex [dict get [::copilot::client::extension_approvals_list c1] approvals] 0]
    list $::test_rpc_calls [dict get $grant granted] [dict get $grant approval extension_name] \
        [dict get $first capabilities] [dict get $first installation_key] \
        [dict get $first installation_label]
} -cleanup {
    ::test_restore_client_rpc
} -result [list [list \
    permissions.extensionApprovals.grant {{"sessionId":"s1","requestId":"req-1"}} \
    permissions.extensionApprovals.list {{}}] \
    1 plugin:p:e {network fs} key-1 {}]

test user-input-response-json "user input responses send attachments only when present" -body {
    set image [::copilot::types::make_user_input_image_attachment data aGk= mime_type image/png \
        display_name shot.png]
    list [::copilot::types::user_input_response_json \
            [::copilot::types::make_user_input_response answer yes]] \
        [::copilot::types::user_input_response_json [::copilot::types::make_user_input_response \
            answer "" was_freeform 1 attachments [list $image]]] \
        [::copilot::types::user_input_response_json "plain text"]
} -result [list {{"answer":"yes","wasFreeform":false}} \
    {{"answer":"","wasFreeform":true,"attachments":[{"type":"image","data":"aGk=","mimeType":"image/png","displayName":"shot.png"}]}} \
    {{"answer":"plain text","wasFreeform":true}}]

test user-input-reply "userInput.request passes image constraints, requestId, and _meta to the handler" -body {
    proc ::test_user_input {request context} {
        set ::test_user_input_request $request
        return [::copilot::types::make_user_input_response answer "" was_freeform 1 \
            attachments [list [::copilot::types::make_user_input_image_attachment \
                data aGk= mime_type image/png description "A screenshot"]]]
    }
    set cfg [::copilot::types::make_session_config on_user_input_request ::test_user_input]
    set ::copilot::session::sessions [dict create h1 [dict create session_id s1 config $cfg]]
    set reply [::copilot::session::_user_input_reply [::test_decode {{"sessionId":"s1",
        "question":"Screenshot?","requestId":"ui-1","_meta":{"k":"v"},
        "imageAttachmentConstraints":{"allowedMimeTypes":["image/png"],
        "maxBytesPerImage":1048576,"maxCount":2}}}]]
    set ::copilot::session::sessions [dict create]
    set req $::test_user_input_request
    set d [::test_decode [::copilot::session::group_session_params \
        [::copilot::session::_session_flat_params $cfg 0] 0]]
    list [dict get $req question] [dict get $req allow_freeform] [dict get $req request_id] \
        [dict get $req image_attachment_constraints] [dict get $req meta] $reply \
        [dict get $d interaction requestUserInput]
} -result [list Screenshot? 1 ui-1 \
    {allowed_mime_types image/png max_bytes_per_image 1048576 max_count 2} {k v} \
    {{"answer":"","wasFreeform":true,"attachments":[{"type":"image","data":"aGk=","mimeType":"image/png","description":"A screenshot"}]}} \
    true]

# ---- Summary ----------------------------------------------------------------

puts ""
puts "=== Test Summary ==="
cleanupTests

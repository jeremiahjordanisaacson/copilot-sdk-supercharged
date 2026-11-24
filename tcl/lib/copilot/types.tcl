# types.tcl - Type definitions and helper procs for the Copilot SDK for Tcl
#
# Types are represented as Tcl dicts. This module provides constructors and
# validators for the core data structures used throughout the SDK.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require copilot::jsonrpc

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
        remote                          0 \
        on_get_trace_context            "" \
        client_info                     {} \
        mode                            "coding-agent" \
        network                         {} \
    ]
    set opts $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown client option: $key"
        }
        dict set opts $key $value
    }
    dict set opts mode [normalize_client_mode [dict get $opts mode]]
    return $opts
}

# -- Client mode --------------------------------------------------------------

# Client modes: "coding-agent" (default, CLI-like coding tools and defaults) or
# "empty" (every create/resume must pass available_tools, and restrictive
# defaults are applied under explicit options). "copilot-cli" is an alias of
# "coding-agent".
proc ::copilot::types::client_mode {} {
    return [dict create CODING_AGENT "coding-agent" EMPTY "empty"]
}

proc ::copilot::types::normalize_client_mode {mode} {
    if {$mode eq "" || $mode eq "copilot-cli"} {
        return "coding-agent"
    }
    if {$mode ne "coding-agent" && $mode ne "empty"} {
        error "Invalid client mode \"$mode\": expected \"coding-agent\" or \"empty\""
    }
    return $mode
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
        rewind_enabled                      0 \
        additional_directories              {} \
        disabled_mcp_servers                {} \
        github_mcp_tool_config              {} \
        canvas_provider                     "" \
        custom_agents_local_only            0 \
        builtin_plugin_directories          {} \
        args_schema                         {} \
        decision_context                    {} \
        tool_search                         "" \
        in_process                          0 \
        experimental_mode                   0 \
        content_exclusion                   0 \
        on_user_prompt_transformed          "" \
        on_mcp_auth_request                 "" \
        on_post_tool_use                    "" \
        on_pre_mcp_tool_call                "" \
        on_exit_plan_mode                   "" \
        available_tools                     {} \
        hooks                               {} \
        provider                            {} \
        providers                           {} \
        models                              {} \
        default_provider                    "" \
        capi                                {} \
        auto_tier                           "" \
        ask_user_variant                    "" \
        github_token_provider               "" \
        sandbox_config                      {} \
        feature_flags                       {} \
        allowed_models                      {} \
        auth_client_id_metadata_url         "" \
        name                                "" \
        agent_directories                   {} \
        hook_directories                    {} \
        interactive_user                    "" \
        excluded_managed_settings           {} \
        enforce_managed_model_defaults      "" \
        trust_working_directory             "" \
        refresh_custom_instructions         "" \
        allow_transcript_recovery           "" \
        disable_resume                      0 \
        image_generation                    {} \
        autopilot                           {} \
        on_shell_authentication_request     "" \
        store_memory                        "" \
        on_permission_request               "" \
        on_user_input_request               "" \
    ]
    set cfg $defaults
    foreach {key value} $args {
        if {![dict exists $defaults $key]} {
            error "Unknown session config key: $key"
        }
        dict set cfg $key $value
    }
    if {[dict get $cfg github_token] ne "" && [dict get $cfg github_token_provider] ne ""} {
        error "github_token and github_token_provider are mutually exclusive"
    }
    return $cfg
}

# -- Tool definition ----------------------------------------------------------

proc ::copilot::types::make_tool {name description handler {parameters {}} {idempotent ""} {skip_permission ""}} {
    if {$name eq ""} {
        error "Tool name must not be empty"
    }
    if {$handler eq ""} {
        error "Tool handler must not be empty"
    }
    return [dict create \
        name            $name \
        description     $description \
        handler         $handler \
        parameters      $parameters \
        idempotent      $idempotent \
        skip_permission $skip_permission \
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
        source          "" \
        response_schema "" \
        client_turn_id  "" \
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

# -- v2.6.0 / v3.0.0 builders (raw JSON with exact wire types) -----------------

# Internal: parse key/value args against defaults (raises on unknown keys).
proc ::copilot::types::_options {defaults arglist what} {
    set cfg $defaults
    foreach {key value} $arglist {
        if {![dict exists $defaults $key]} {
            error "Unknown $what option: $key"
        }
        dict set cfg $key $value
    }
    return $cfg
}

# Internal: dict of wire key -> JSON text for the options that are set ("" means
# unset). spec is a flat list of option, wire key, and kind (string, bool, number,
# strings, or json); raw JSON values are used verbatim.
proc ::copilot::types::_wire_object {cfg spec} {
    set members [dict create]
    foreach {option key kind} $spec {
        set value [dict get $cfg $option]
        if {[::copilot::jsonrpc::is_raw $value]} {
            dict set members $key [::copilot::jsonrpc::to_json $value]
            continue
        }
        if {$value eq ""} {
            continue
        }
        switch -- $kind {
            string  { dict set members $key [::copilot::jsonrpc::json_string $value] }
            bool    { dict set members $key [::copilot::jsonrpc::json_bool $value] }
            number  { dict set members $key [::copilot::jsonrpc::json_number $value] }
            strings { dict set members $key [::copilot::jsonrpc::json_string_list $value] }
            json    { dict set members $key [::copilot::jsonrpc::params_json $value] }
            default { error "Unknown option kind: $kind" }
        }
    }
    return $members
}

# Client info sent on the connect handshake (client option client_info).
proc ::copilot::types::make_client_info {args} {
    return [_options [dict create editor_name "" editor_version "" \
        extension_name "" extension_version ""] $args "client_info"]
}

# JSON text of the connect clientInfo parameter, or "" when no field is set.
proc ::copilot::types::client_info_json {info} {
    if {$info eq ""} {
        return ""
    }
    set members [dict create]
    foreach {snake camel} {editor_name editorName editor_version editorVersion \
            extension_name extensionName extension_version extensionVersion} {
        foreach key [list $snake $camel] {
            if {[dict exists $info $key] && [dict get $info $key] ne ""} {
                dict set members $camel [::copilot::jsonrpc::json_string [dict get $info $key]]
            }
        }
    }
    if {[dict size $members] == 0} {
        return ""
    }
    return [::copilot::jsonrpc::json_object $members]
}

# Sandbox configuration (session option sandbox_config). allow_bypass opts in to
# letting the agent request that an individual command run outside the sandbox,
# approved through the normal permission flow.
proc ::copilot::types::make_sandbox_config {args} {
    set cfg [_options [dict create enabled 1 allow_bypass "" user_policy "" \
        add_current_working_directory "" sandbox_mcp_servers "" \
        sandbox_lsp_servers "" sandbox_extensions ""] $args "sandbox_config"]
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [_wire_object $cfg {
        enabled                       enabled                    bool
        user_policy                   userPolicy                 json
        add_current_working_directory addCurrentWorkingDirectory bool
        sandbox_mcp_servers           sandboxMcpServers          bool
        sandbox_lsp_servers           sandboxLspServers          bool
        sandbox_extensions            sandboxExtensions          bool
        allow_bypass                  allowBypass                bool
    }]]]
}

# Image generation opt-in (session option image_generation).
proc ::copilot::types::make_image_generation_config {args} {
    set cfg [_options [dict create enabled "" watermark "" save_to_disk ""] $args "image_generation"]
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [_wire_object $cfg {
        enabled      enabled    bool
        watermark    watermark  bool
        save_to_disk saveToDisk bool
    }]]]
}

# Autopilot continuation options (session option autopilot).
proc ::copilot::types::make_autopilot_options {args} {
    set cfg [_options [dict create continuation_enabled "" max_continues ""] $args "autopilot"]
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [_wire_object $cfg {
        continuation_enabled continuationEnabled bool
        max_continues        maxContinues        number
    }]]]
}

# Model capabilities override with limits (session option model_capabilities). The
# limits use snake_case wire keys, for example max_output_tokens.
proc ::copilot::types::make_model_capabilities_limits {args} {
    set cfg [_options [dict create max_prompt_tokens "" max_context_window_tokens "" \
        max_output_tokens ""] $args "model_capabilities"]
    set limits [::copilot::jsonrpc::json_object [_wire_object $cfg {
        max_prompt_tokens         max_prompt_tokens         number
        max_context_window_tokens max_context_window_tokens number
        max_output_tokens         max_output_tokens         number
    }]]
    return [::copilot::jsonrpc::json_raw "{\"limits\":$limits}"]
}

# Internal: provider connection members shared by the legacy and named providers.
proc ::copilot::types::_connection_members {cfg} {
    set members [_wire_object $cfg {
        type            type          string
        base_url        baseUrl       string
        api_key         apiKey        string
        api_key_command apiKeyCommand string
        bearer_token    bearerToken   string
        wire_api        wireApi       string
        transport       transport     string
        headers         headers       json
        model_provider  modelProvider string
    }]
    if {[dict get $cfg azure_api_version] ne ""} {
        dict set members azure [::copilot::jsonrpc::json_object [dict create \
            apiVersion [::copilot::jsonrpc::json_string [dict get $cfg azure_api_version]]]]
    }
    return $members
}

# Legacy single BYOK provider (session option provider). api_key_command runs
# before each request to obtain a fresh API key. On SDK protocol 4 it is sent as
# the named "default" connection with Copilot API models disabled; model_id,
# wire_model, and max_*_tokens become a model definition for the session model.
proc ::copilot::types::make_provider_config {args} {
    set cfg [_options [dict create type "" base_url "" api_key "" api_key_command "" \
        bearer_token "" wire_api "" transport "" headers "" model_provider "" \
        azure_api_version "" model_id "" wire_model "" max_prompt_tokens "" \
        max_context_window_tokens "" max_output_tokens ""] $args "provider"]
    set members [dict merge [_connection_members $cfg] [_wire_object $cfg {
        model_id                  modelId                string
        wire_model                wireModel              string
        max_prompt_tokens         maxPromptTokens        number
        max_context_window_tokens maxContextWindowTokens number
        max_output_tokens         maxOutputTokens        number
    }]]
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
}

# Named BYOK provider connection (session option providers, a list of these).
# Select a registered model with "<name>/<id>".
proc ::copilot::types::make_named_provider_config {args} {
    set cfg [_options [dict create name "" base_url "" type "" api_key "" \
        api_key_command "" bearer_token "" wire_api "" transport "" headers "" \
        model_provider "" azure_api_version ""] $args "named provider"]
    if {[dict get $cfg name] eq "" || [dict get $cfg base_url] eq ""} {
        error "A named provider requires name and base_url"
    }
    set members [dict merge \
        [dict create name [::copilot::jsonrpc::json_string [dict get $cfg name]]] \
        [_connection_members $cfg]]
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
}

# BYOK model definition served by a named provider (session option models, a list
# of these).
proc ::copilot::types::make_provider_model_config {args} {
    set cfg [_options [dict create provider "" id "" model_id "" wire_model "" name "" \
        max_prompt_tokens "" max_context_window_tokens "" max_output_tokens "" \
        capabilities ""] $args "provider model"]
    if {[dict get $cfg provider] eq "" || [dict get $cfg id] eq ""} {
        error "A provider model requires provider and id"
    }
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [_wire_object $cfg {
        provider                  provider               string
        id                        id                     string
        model_id                  modelId                string
        wire_model                wireModel              string
        name                      name                   string
        max_prompt_tokens         maxPromptTokens        number
        max_context_window_tokens maxContextWindowTokens number
        max_output_tokens         maxOutputTokens        number
        capabilities              capabilities           json
    }]]]
}

# Message source values for the send option source: USER, SYSTEM, or
# message_source_agent <id> ("agent-<id>").
proc ::copilot::types::message_source {} {
    return [dict create USER "user" SYSTEM "system"]
}

proc ::copilot::types::message_source_agent {agent_id} {
    return "agent-$agent_id"
}

# Common session event types. Other (including future) event types are still
# delivered to listeners unchanged; tool.execution_partial_result is no longer
# emitted by protocol 4 runtimes but is tolerated.
proc ::copilot::types::session_event_types {} {
    return [dict create \
        ASSISTANT_MESSAGE             "assistant.message" \
        ASSISTANT_MESSAGE_DELTA       "assistant.message_delta" \
        SESSION_IDLE                  "session.idle" \
        SESSION_ERROR                 "session.error" \
        EXTERNAL_TOOL_REQUESTED       "external_tool.requested" \
        TOOL_SHELL_OUTPUT             "tool.shell_output" \
        TOOL_EXECUTION_PARTIAL_RESULT "tool.execution_partial_result" \
    ]
}

# Data of a tool.shell_output event as a dict (tool_call_id, text, sequence, and
# stream: "stdout", "stderr", or "" when not reported); "" for other events.
proc ::copilot::types::make_shell_output_event_data {event} {
    if {![dict exists $event type] || [dict get $event type] ne "tool.shell_output"} {
        return ""
    }
    set result [dict create tool_call_id "" text "" sequence "" stream ""]
    if {[dict exists $event data]} {
        set data [dict get $event data]
        foreach {key wire} {tool_call_id toolCallId text text sequence sequence stream stream} {
            if {[dict exists $data $wire] && [dict get $data $wire] ne "null"} {
                dict set result $key [dict get $data $wire]
            }
        }
    }
    return $result
}

# Known values of the reason in a set_permission_mode result.
proc ::copilot::types::permission_mode_refusal_reason {} {
    return [dict create MANAGED_POLICY "managed_policy"]
}

# PolicyHelperFailure (from error data policyHelperFailure on a failed create or
# resume, or a managed settings snapshot) as a dict with snake_case keys.
# Helper-authored text is untrusted: render it as plain text.
proc ::copilot::types::make_policy_helper_failure {failure} {
    set result [dict create category "" failure_mode "" message "" \
        previous_policy_retained 0 reason "" severity "" authority "" code "" \
        details {} diagnostic "" help_link {} remediation ""]
    foreach {key wire} {
        category category failure_mode failureMode message message reason reason
        severity severity authority authority code code details details
        diagnostic diagnostic help_link helpLink remediation remediation
    } {
        if {[dict exists $failure $wire] && [dict get $failure $wire] ne "null"} {
            dict set result $key [dict get $failure $wire]
        }
    }
    if {[dict exists $failure previousPolicyRetained]} {
        dict set result previous_policy_retained \
            [string is true -strict [dict get $failure previousPolicyRetained]]
    }
    return $result
}

# -- v3.1.0 types (parity with the Microsoft Copilot SDK 2.0.3-preview.0) -------

# A user-scoped approval for one installed plugin extension (from an
# ::copilot::client::extension_approvals_grant or extension_approvals_list result)
# as a dict with snake_case keys: extension_name (stable identity, for example
# "plugin:plugin-name:extension-name"), plugin_name, capabilities (list of the exact
# privileged capabilities the user approved), granted_at (RFC 3339 timestamp),
# installation_key (opaque: pass it back unchanged to extension_approvals_revoke),
# and installation_label ("" when absent).
proc ::copilot::types::make_permissions_extension_approval {approval} {
    set result [dict create extension_name "" plugin_name "" capabilities {} \
        granted_at "" installation_key "" installation_label ""]
    foreach {key wire} {
        extension_name extensionName plugin_name pluginName capabilities capabilities
        granted_at grantedAt installation_key installationKey
        installation_label installationLabel
    } {
        if {[dict exists $approval $wire] && [dict get $approval $wire] ne "null"} {
            dict set result $key [dict get $approval $wire]
        }
    }
    return $result
}

# Image support advertised by a userInput.request (wire imageAttachmentConstraints)
# as a dict: allowed_mime_types (list), max_bytes_per_image, and max_count. Returns
# "" when the request carries no constraints (the runtime does not accept images).
proc ::copilot::types::make_user_input_image_attachment_constraints {constraints} {
    if {$constraints eq "" || $constraints eq "null" || [catch {dict size $constraints}]} {
        return ""
    }
    set result [dict create allowed_mime_types {} max_bytes_per_image "" max_count ""]
    foreach {key wire} {
        allowed_mime_types allowedMimeTypes max_bytes_per_image maxBytesPerImage
        max_count maxCount
    } {
        if {[dict exists $constraints $wire] && [dict get $constraints $wire] ne "null"} {
            dict set result $key [dict get $constraints $wire]
        }
    }
    return $result
}

# An image attached to an ask_user answer (UserInputImageAttachment): data is the
# base64-encoded image and mime_type its MIME type; description and display_name are
# optional. The wire type is always "image". Only valid when the request advertised
# image_attachment_constraints; the runtime enforces the MIME types, size, and count.
proc ::copilot::types::make_user_input_image_attachment {args} {
    set cfg [_options [dict create data "" mime_type "" description "" display_name ""] \
        $args "user input image attachment"]
    if {[dict get $cfg data] eq "" || [dict get $cfg mime_type] eq ""} {
        error "An image attachment requires data and mime_type"
    }
    return [dict merge [dict create type image] $cfg]
}

# Response to a user input request, returned by an on_user_input_request handler:
# answer, was_freeform, and attachments (a list of make_user_input_image_attachment
# dicts, sent only when non-empty). Image-only answers use an empty answer with
# was_freeform 1.
proc ::copilot::types::make_user_input_response {args} {
    return [_options [dict create answer "" was_freeform 0 attachments {}] $args \
        "user input response"]
}

# JSON text of a user input response: raw JSON verbatim, a dict with answer (see
# make_user_input_response; wasFreeform is also accepted), or any other value as a
# freeform text answer. attachments are sent only when non-empty; each is a
# make_user_input_image_attachment dict (camelCase keys are also accepted) or raw JSON.
proc ::copilot::types::user_input_response_json {response} {
    if {[::copilot::jsonrpc::is_raw $response]} {
        return [::copilot::jsonrpc::to_json $response]
    }
    if {[catch {dict exists $response answer} has_answer] || !$has_answer} {
        return [::copilot::jsonrpc::json_object [dict create \
            answer      [::copilot::jsonrpc::json_string $response] \
            wasFreeform true \
        ]]
    }
    set was_freeform 0
    foreach key {was_freeform wasFreeform} {
        if {[dict exists $response $key]} {
            set was_freeform [dict get $response $key]
        }
    }
    set members [dict create \
        answer      [::copilot::jsonrpc::json_string [dict get $response answer]] \
        wasFreeform [::copilot::jsonrpc::json_bool $was_freeform] \
    ]
    set texts [list]
    if {[dict exists $response attachments]} {
        foreach attachment [dict get $response attachments] {
            lappend texts [_user_input_image_attachment_json $attachment]
        }
    }
    if {[llength $texts] > 0} {
        dict set members attachments [::copilot::jsonrpc::json_array $texts]
    }
    return [::copilot::jsonrpc::json_object $members]
}

# Internal: wire JSON of one user input image attachment.
proc ::copilot::types::_user_input_image_attachment_json {attachment} {
    if {[::copilot::jsonrpc::is_raw $attachment]} {
        return [::copilot::jsonrpc::to_json $attachment]
    }
    set members [dict create type [::copilot::jsonrpc::json_string image]]
    foreach {wire keys} {
        data data mimeType {mime_type mimeType} description description
        displayName {display_name displayName}
    } {
        foreach key $keys {
            if {[dict exists $attachment $key] && [dict get $attachment $key] ne ""} {
                dict set members $wire [::copilot::jsonrpc::json_string [dict get $attachment $key]]
            }
        }
    }
    return [::copilot::jsonrpc::json_object $members]
}

package provide copilot::types 2.0.0

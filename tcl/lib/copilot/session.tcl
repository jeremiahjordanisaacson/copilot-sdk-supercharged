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
    # registration_id -> {provider <command prefix> session_id <id>} for gitHubToken.getToken
    variable github_token_providers [dict create]
    # Client network callbacks (network.resolveProxy / authenticateNegotiate / authenticateBasic)
    variable network_callbacks [dict create]
    # Last JSON-RPC error object received for a request (code, message, data)
    variable last_rpc_error [dict create]
    # Session whose send_and_wait is collecting events, and its collected state
    variable _active_wait_sid ""
    variable _wait_state [dict create done 0 content "" error "" events {}]

    # SDK protocol 4 option groups of session.create / session.resume.
    variable option_groups {
        inference tools permissions interaction events agents skills mcp github
        filesystem execution instructions hooks git state plugins canvases
        extensions experiments policy
    }

    # Flat session option -> {group member} in the SDK protocol 4 payload. Root
    # options (sessionId, model, systemMessage, ...) and unknown options are not
    # listed and stay at the top level.
    variable flat_paths [dict create \
        reasoningEffort {inference reasoningEffort} \
        reasoningSummary {inference reasoningSummary} \
        contextTier {inference contextTier} \
        allowMultipleToolCalls {inference allowMultipleToolCalls} \
        enableCitations {inference enableCitations} \
        modelCapabilities {inference capabilities} \
        verbosity {inference verbosity} \
        providers {inference providers} \
        models {inference models} \
        defaultProvider {inference defaultProvider} \
        tools {tools definitions} \
        availableTools {tools availableTools} \
        excludedTools {tools excludedTools} \
        toolSearch {tools search} \
        largeOutput {tools largeOutput} \
        imageGeneration {tools imageGeneration} \
        manageScheduleEnabled {tools manageScheduleEnabled} \
        webSearch {tools webSearch} \
        shell {tools shell} \
        maxInlineBinaryBytes {tools maxInlineBinaryBytes} \
        requestPermission {permissions request} \
        sandboxConfig {permissions sandbox} \
        sandbox {permissions sandbox} \
        permissionMode {permissions mode} \
        sandboxConfigSource {permissions sandboxSource} \
        commands {interaction commands} \
        askUserVariant {interaction askUserVariant} \
        requestUserInput {interaction requestUserInput} \
        requestElicitation {interaction requestElicitation} \
        requestShellAuthentication {interaction requestShellAuthentication} \
        requestExitPlanMode {interaction requestExitPlanMode} \
        requestAutoModeSwitch {interaction requestAutoModeSwitch} \
        requestAutoModeRecovery {interaction requestAutoModeRecovery} \
        streaming {events streaming} \
        includeSubAgentStreamingEvents {events includeSubAgentStreamingEvents} \
        customAgents {agents definitions} \
        defaultAgent {agents default} \
        agent {agents selected} \
        includedBuiltinAgents {agents includedBuiltinAgents} \
        excludedBuiltinAgents {agents excludedBuiltinAgents} \
        customAgentsLocalOnly {agents localDiscoveryOnly} \
        agentDirectories {agents directories} \
        skillDirectories {skills directories} \
        enableSkills {skills enabled} \
        includedBuiltinSkills {skills includedBuiltinSkills} \
        disabledSkills {skills disabledSkills} \
        hasSkillProvider {skills hasProvider} \
        ignoredSkillsLocations {skills ignoredLocations} \
        mcpServers {mcp servers} \
        disabledMcpServers {mcp disabledServers} \
        requestMcpApps {mcp enableApps} \
        enableMcpApps {mcp enableApps} \
        mcpOAuthTokenStorage {mcp oauthTokenStorage} \
        authClientIdMetadataUrl {mcp authClientIdMetadataUrl} \
        envValueMode {mcp envValueMode} \
        managedMcpServers {mcp managedServers} \
        allowAllMcpServerInstructions {mcp allowAllServerInstructions} \
        gitHubToken {github token} \
        gitHubTokenProviderRegistrationId {github tokenProviderRegistrationId} \
        githubMcpToolConfig {github mcp} \
        capi {github capi} \
        enableSessionTelemetry {github telemetryEnabled} \
        enableManagedSettings {github fetchManagedSettings} \
        interactiveUser {github interactiveUser} \
        enableGitHubTelemetryForwarding {github telemetryForwarding} \
        authInfo {github authInfo} \
        copilotUrl {github copilotUrl} \
        additionalDirectories {filesystem additionalDirectories} \
        enableFileChangeTracking {filesystem trackFileChanges} \
        trustWorkingDirectory {filesystem trustWorkingDirectory} \
        sessionLimits {execution limits} \
        autopilot {execution autopilot} \
        autopilotContinuation {execution autopilotContinuation} \
        instructionDirectories {instructions directories} \
        organizationCustomInstructions {instructions organizationText} \
        enableOnDemandInstructionDiscovery {instructions onDemandDiscovery} \
        skipCustomInstructions {instructions skipCustomInstructions} \
        refreshCustomInstructions {instructions refreshCustomInstructions} \
        hooks {hooks hasHandlers} \
        hookDirectories {hooks directories} \
        enableFileHooks {hooks enableRepositoryHooks} \
        coauthorEnabled {git coauthorEnabled} \
        enableHostGitOperations {git enableHostGitOperations} \
        infiniteSessions {state infiniteSessions} \
        memory {state memory} \
        hasMemoryStore {state hasMemoryStore} \
        enableSessionStore {state enableCrossSessionStore} \
        skipEmbeddingRetrieval {state skipEmbeddingRetrieval} \
        embeddingCacheStorage {state embeddingCacheStorage} \
        pluginDirectories {plugins directories} \
        installedPlugins {plugins installed} \
        canvases {canvases definitions} \
        canvasProvider {canvases provider} \
        requestCanvasRenderer {canvases requestRenderer} \
        openCanvases {canvases open} \
        requestExtensions {extensions enabled} \
        extensionSdkPath {extensions sdkPath} \
        extensionInfo {extensions identity} \
        enableExperimentalMode {experiments enableExperimentalMode} \
        isExperimentalMode {experiments enableExperimentalMode} \
        featureFlags {experiments flags} \
        expAssignments {experiments assignments} \
        featureFlagConfiguration {experiments configuration} \
        allowedModels {policy allowedModels} \
        managedSettings {policy managedSettings} \
        excludedManagedSettings {policy excludedManagedSettings} \
        enforceManagedModelDefaults {policy enforceManagedModelDefaults} \
    ]

    # Members of a legacy single provider that belong to a model definition.
    variable legacy_model_keys {
        modelId wireModel maxPromptTokens maxContextWindowTokens maxOutputTokens capabilities
    }

    namespace export create_session resume_session destroy_session detach_session \
                     send_message send_and_wait on_event get_state session_info \
                     group_session_params set_auto_tier get_managed_settings \
                     recheck_policy_helper set_permission_mode \
                     permission_decision_json approve_all deny_all
}

# -- Internal: generate a local session handle --------------------------------

proc ::copilot::session::_next_handle {} {
    variable next_session_idx
    incr next_session_idx
    return "session_$next_session_idx"
}

# -- Create a session via JSON-RPC --------------------------------------------

proc ::copilot::session::create_session {write_ch read_ch config {mode "coding-agent"}} {
    variable sessions

    set handle [_next_handle]
    _require_available_tools $config $mode

    # Build the flat options and translate them to the SDK protocol 4 payload
    set flat [_session_flat_params $config 0]
    set registration_id [_register_github_token_provider $config ""]
    if {$registration_id ne ""} {
        dict set flat gitHubTokenProviderRegistrationId [::copilot::jsonrpc::json_string $registration_id]
    }
    set params [group_session_params $flat 0 [expr {$mode eq "empty"}]]

    set req_id [::copilot::jsonrpc::send_request $write_ch "session.create" \
        [::copilot::jsonrpc::json_raw $params]]
    ::copilot::jsonrpc::register_pending $req_id

    # Read messages until we get our response
    if {[catch {_wait_for_response $read_ch $write_ch $req_id} result opts]} {
        _unregister_github_token_provider $registration_id
        return -options [_with_policy_helper_failure $opts] $result
    }

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
        registration_id $registration_id \
    ]

    if {[dict exists $result sessionId]} {
        dict set session_data session_id [dict get $result sessionId]
    }
    _commit_github_token_provider $registration_id [dict get $session_data session_id]

    dict set sessions $handle $session_data
    return $handle
}

# -- Resume a session via JSON-RPC --------------------------------------------

# Returns the session.resume result dict plus "handle", the local session handle
# that routes events and server requests (tools, hooks, shell authentication,
# memory store, GitHub token provider) for the resumed session.
proc ::copilot::session::resume_session {write_ch read_ch session_id config {mode "coding-agent"}} {
    variable sessions

    _require_available_tools $config $mode

    set flat [dict create sessionId [::copilot::jsonrpc::json_string $session_id]]
    set flat [dict merge $flat [_session_flat_params $config 1]]
    set registration_id [_register_github_token_provider $config $session_id]
    if {$registration_id ne ""} {
        dict set flat gitHubTokenProviderRegistrationId [::copilot::jsonrpc::json_string $registration_id]
    }
    set params [group_session_params $flat 1 [expr {$mode eq "empty"}]]

    set req_id [::copilot::jsonrpc::send_request $write_ch "session.resume" \
        [::copilot::jsonrpc::json_raw $params]]
    ::copilot::jsonrpc::register_pending $req_id

    if {[catch {_wait_for_response $read_ch $write_ch $req_id} result opts]} {
        _unregister_github_token_provider $registration_id
        return -options [_with_policy_helper_failure $opts] $result
    }

    set sid $session_id
    if {[dict exists $result sessionId]} {
        set sid [dict get $result sessionId]
    }
    _commit_github_token_provider $registration_id $sid

    set handle [_next_handle]
    dict set sessions $handle [dict create \
        handle     $handle \
        session_id $sid \
        write_ch   $write_ch \
        read_ch    $read_ch \
        config     $config \
        listeners  {} \
        events     {} \
        state      {} \
        registration_id $registration_id \
    ]
    dict set result handle $handle
    return $result
}

# -- Internal: empty client mode requires an explicit tool allowlist ----------

proc ::copilot::session::_require_available_tools {config mode} {
    if {$mode eq "empty" && [dict get $config available_tools] eq ""} {
        error "Client mode \"empty\" requires available_tools on every create_session and resume_session call"
    }
}

# -- Internal: flat session options as a dict of wire key -> JSON text --------

# Adds key -> JSON text to the dict in varName when value is set. Raw JSON values
# (::copilot::jsonrpc::json_raw) are always used verbatim. Kinds: string, bool
# (true or false), true (only sent when true), number, strings (array of
# strings), object (Tcl dict), json (best-effort encoding), objects (list of
# objects), flags (dict of booleans).
proc ::copilot::session::_put {varName key value kind} {
    upvar 1 $varName members
    if {[::copilot::jsonrpc::is_raw $value]} {
        dict set members $key [::copilot::jsonrpc::to_json $value]
        return
    }
    if {$value eq ""} {
        return
    }
    switch -- $kind {
        string {
            set text [::copilot::jsonrpc::json_string $value]
        }
        bool {
            set text [::copilot::jsonrpc::json_bool $value]
        }
        true {
            if {![string is true -strict $value]} {
                return
            }
            set text "true"
        }
        number {
            set text [::copilot::jsonrpc::json_number $value]
        }
        strings {
            set text [::copilot::jsonrpc::json_string_list $value]
        }
        object {
            set text [::copilot::jsonrpc::params_json $value]
        }
        json {
            if {$value eq "true" || $value eq "false" || $value eq "null"} {
                set text $value
            } else {
                set text [::copilot::jsonrpc::params_json $value]
            }
        }
        objects {
            set texts [list]
            foreach item $value {
                lappend texts [::copilot::jsonrpc::params_json $item]
            }
            set text [::copilot::jsonrpc::json_array $texts]
        }
        flags {
            set flag_members [dict create]
            dict for {flag enabled} $value {
                dict set flag_members $flag [::copilot::jsonrpc::json_bool $enabled]
            }
            set text [::copilot::jsonrpc::json_object $flag_members]
        }
        default {
            error "Unknown option kind: $kind"
        }
    }
    dict set members $key $text
}

proc ::copilot::session::_session_flat_params {config is_resume} {
    set p [dict create]
    _put p model [dict get $config model] string
    set sys_prompt [dict get $config system_prompt]
    if {$sys_prompt ne ""} {
        dict set p systemMessage [::copilot::jsonrpc::json_object [dict create \
            mode    [::copilot::jsonrpc::json_string "append"] \
            content [::copilot::jsonrpc::json_string $sys_prompt] \
        ]]
    }
    _put p workingDirectory [dict get $config working_directory] string
    _put p gitHubToken [dict get $config github_token] string
    set idle_timeout [dict get $config session_idle_timeout_seconds]
    if {$idle_timeout ne "" && $idle_timeout > 0} {
        dict set p sessionIdleTimeoutSeconds [::copilot::jsonrpc::json_number $idle_timeout]
    }
    _put p streaming [dict get $config streaming] true
    _put p availableTools [dict get $config available_tools] strings
    _put p excludedTools [dict get $config excluded_tools] strings
    _put p mcpServers [_mcp_servers_json [dict get $config mcp_servers]] string
    _put p modelCapabilities [dict get $config model_capabilities] object
    _put p enableConfigDiscovery [dict get $config enable_config_discovery] true
    _put p includeSubAgentStreamingEvents [dict get $config include_sub_agent_streaming_events] true
    _put p commands [_commands_json [dict get $config commands]] string
    _put p skillDirectories [dict get $config skill_directories] strings
    _put p disabledSkills [dict get $config disabled_skills] strings
    _put p instructionDirectories [dict get $config instruction_directories] strings
    if {[dict get $config elicitation_handler] ne ""} {
        dict set p requestElicitation true
    }
    if {[dict get $config on_exit_plan_mode] ne ""} {
        dict set p requestExitPlanMode true
    }
    # Permission prompts arrive as permission.requested events answered by the handler
    if {[dict get $config on_permission_request] ne ""} {
        dict set p requestPermission true
    }

    # Upstream-sync session options (parity with @github/copilot-sdk)
    _put p enableCitations [dict get $config enable_citations] true
    _put p excludedBuiltinAgents [dict get $config excluded_builtin_agents] strings
    _put p sessionLimits [dict get $config session_limits] object
    set memory [dict get $config memory]
    if {[::copilot::jsonrpc::is_raw $memory]} {
        dict set p memory [::copilot::jsonrpc::to_json $memory]
    } elseif {$memory ne "" && [dict exists $memory enabled]} {
        dict set p memory [::copilot::jsonrpc::json_object [dict create \
            enabled [::copilot::jsonrpc::json_bool [dict get $memory enabled]] \
        ]]
    }
    _put p otlpProtocol [dict get $config otlp_protocol] string
    _put p expAssignments [dict get $config exp_assignments] object
    # --- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk) ---
    _put p reasoningEffort [dict get $config reasoning_effort] string
    _put p rewindEnabled [dict get $config rewind_enabled] true
    _put p additionalDirectories [dict get $config additional_directories] strings
    _put p disabledMcpServers [dict get $config disabled_mcp_servers] strings
    _put p githubMcpToolConfig [dict get $config github_mcp_tool_config] object
    _put p canvasProvider [dict get $config canvas_provider] string
    _put p customAgentsLocalOnly [dict get $config custom_agents_local_only] true
    _put p builtinPluginDirectories [dict get $config builtin_plugin_directories] strings
    _put p argsSchema [dict get $config args_schema] object
    _put p decisionContext [dict get $config decision_context] object
    _put p toolSearch [dict get $config tool_search] json
    _put p inProcess [dict get $config in_process] true
    _put p experimentalMode [dict get $config experimental_mode] true
    _put p contentExclusion [dict get $config content_exclusion] true
    # User-prompt-transformed hook: signal to the runtime that a handler is registered.
    if {[dict get $config on_user_prompt_transformed] ne ""} {
        dict set p userPromptTransformed true
    }
    # Signal to the runtime that an MCP OAuth host-token handler is registered.
    if {[dict get $config on_mcp_auth_request] ne ""} {
        dict set p mcpAuthHandler true
    }
    # Hook handlers are answered through hooks.invoke (see _handle_server_request).
    if {[_hook_handlers $config] ne ""} {
        dict set p hooks true
    }

    # Include tools in session creation
    set wire_tools [::copilot::tools::tools_to_wire_json]
    if {$wire_tools ne "\[\]"} {
        dict set p tools $wire_tools
    }

    # --- v2.6.0 upstream-sync session options ---
    set capi_members [dict create]
    set capi [dict get $config capi]
    if {[::copilot::jsonrpc::is_raw $capi]} {
        set capi_members [::copilot::jsonrpc::json_object_members [::copilot::jsonrpc::to_json $capi]]
    } elseif {$capi ne ""} {
        dict for {key value} $capi {
            dict set capi_members $key [::copilot::jsonrpc::to_json $value]
        }
    }
    _put capi_members autoTier [dict get $config auto_tier] string
    if {[dict get $config enable_web_socket_responses]} {
        dict set capi_members enableWebSocketResponses true
    }
    if {[dict size $capi_members] > 0} {
        dict set p capi [::copilot::jsonrpc::json_object $capi_members]
    }
    _put p askUserVariant [dict get $config ask_user_variant] string
    _put p sandboxConfig [dict get $config sandbox_config] object
    _put p featureFlags [dict get $config feature_flags] flags
    _put p allowedModels [dict get $config allowed_models] strings
    _put p authClientIdMetadataUrl [dict get $config auth_client_id_metadata_url] string
    _put p agentDirectories [dict get $config agent_directories] strings
    _put p hookDirectories [dict get $config hook_directories] strings
    _put p interactiveUser [dict get $config interactive_user] bool
    _put p excludedManagedSettings [dict get $config excluded_managed_settings] strings
    _put p enforceManagedModelDefaults [dict get $config enforce_managed_model_defaults] bool
    _put p imageGeneration [dict get $config image_generation] object
    _put p autopilot [dict get $config autopilot] object
    if {$is_resume} {
        _put p allowTranscriptRecovery [dict get $config allow_transcript_recovery] bool
        _put p disableResume [dict get $config disable_resume] true
    } else {
        _put p name [dict get $config name] string
        _put p trustWorkingDirectory [dict get $config trust_working_directory] string
        _put p refreshCustomInstructions [dict get $config refresh_custom_instructions] bool
    }

    # --- v3.0.0 (SDK protocol 4) session options ---
    _put p provider [_provider_json [dict get $config provider]] string
    _put p providers [dict get $config providers] objects
    _put p models [dict get $config models] objects
    set default_provider [dict get $config default_provider]
    if {$default_provider eq "null"} {
        # JSON null clears a saved default provider on resume.
        dict set p defaultProvider null
    } else {
        _put p defaultProvider $default_provider string
    }
    if {[dict get $config on_shell_authentication_request] ne ""} {
        dict set p requestShellAuthentication true
    }
    if {[dict get $config store_memory] ne ""} {
        dict set p hasMemoryStore true
    }
    return $p
}

# Raw JSON for mcp_servers (name -> make_mcp_server_config dict or raw JSON).
proc ::copilot::session::_mcp_servers_json {servers} {
    if {[::copilot::jsonrpc::is_raw $servers]} {
        return $servers
    }
    if {$servers eq ""} {
        return ""
    }
    set members [dict create]
    dict for {name server} $servers {
        if {[::copilot::jsonrpc::is_raw $server]} {
            dict set members $name [::copilot::jsonrpc::to_json $server]
            continue
        }
        set m [dict create]
        dict for {key value} $server {
            switch -- $key {
                args - tools {
                    dict set m $key [::copilot::jsonrpc::json_string_list $value]
                }
                env - headers {
                    if {$value ne ""} {
                        set inner [dict create]
                        dict for {k v} $value {
                            dict set inner $k [::copilot::jsonrpc::json_string $v]
                        }
                        dict set m $key [::copilot::jsonrpc::json_object $inner]
                    }
                }
                timeout {
                    dict set m $key [::copilot::jsonrpc::json_number $value]
                }
                default {
                    if {$value ne ""} {
                        dict set m $key [::copilot::jsonrpc::to_json $value]
                    }
                }
            }
        }
        if {![dict exists $m tools]} {
            dict set m tools {["*"]}
        }
        dict set members $name [::copilot::jsonrpc::json_object $m]
    }
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
}

# Raw JSON for commands (list of make_command_definition dicts).
proc ::copilot::session::_commands_json {commands} {
    if {[::copilot::jsonrpc::is_raw $commands]} {
        return $commands
    }
    if {$commands eq ""} {
        return ""
    }
    set texts [list]
    foreach cmd $commands {
        set description ""
        if {[dict exists $cmd description]} {
            set description [dict get $cmd description]
        }
        lappend texts [::copilot::jsonrpc::json_object [dict create \
            name        [::copilot::jsonrpc::json_string [dict get $cmd name]] \
            description [::copilot::jsonrpc::json_string $description] \
        ]]
    }
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_array $texts]]
}

# Raw JSON for a legacy single BYOK provider: raw JSON (make_provider_config) or a
# dict with snake_case or camelCase keys.
proc ::copilot::session::_provider_json {provider} {
    if {[::copilot::jsonrpc::is_raw $provider] || $provider eq ""} {
        return $provider
    }
    set wire_keys [dict create \
        base_url baseUrl api_key apiKey api_key_command apiKeyCommand \
        bearer_token bearerToken wire_api wireApi model_provider modelProvider \
        local_model localModel model_id modelId wire_model wireModel \
        max_prompt_tokens maxPromptTokens max_context_window_tokens maxContextWindowTokens \
        max_output_tokens maxOutputTokens \
    ]
    set members [dict create]
    dict for {key value} $provider {
        if {[dict exists $wire_keys $key]} {
            set key [dict get $wire_keys $key]
        }
        if {$key eq "azure_api_version"} {
            dict set members azure [::copilot::jsonrpc::json_object [dict create \
                apiVersion [::copilot::jsonrpc::json_string $value]]]
        } elseif {[string match max*Tokens $key]} {
            dict set members $key [::copilot::jsonrpc::json_number $value]
        } elseif {$key in {azure headers capabilities}} {
            dict set members $key [::copilot::jsonrpc::params_json $value]
        } else {
            dict set members $key [::copilot::jsonrpc::json_string $value]
        }
    }
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
}

# -- SDK protocol 4 payload translation ----------------------------------------

# Convert a flat session.create / session.resume payload (dict of wire key -> JSON
# text) to the SDK protocol 4 grouped payload and return it as JSON text.
#
# Root keys (sessionId, model, systemMessage, workingDirectory, ...) and unknown
# keys stay at the top level; every other known flat key moves into its option
# group (for example streaming becomes events.streaming and "hooks": true becomes
# hooks.hasHandlers). Objects already keyed by a group name are merged with their
# explicit members winning, nulls are skipped (except a null defaultProvider,
# which clears the saved default), empty groups are not emitted, configDir becomes
# configDirectory, and disableResume becomes suppressResumeEvent on resume. On
# resume, create-only filesystem.trustWorkingDirectory and
# instructions.refreshCustomInstructions are dropped; canvases.open is only sent
# on resume. A legacy single "provider" becomes the named "default" connection
# with Copilot API models disabled, unless named providers, a default provider,
# or capi.enabled are set explicitly. With empty_mode, the empty client mode
# defaults are applied under explicit options.
proc ::copilot::session::group_session_params {flat {is_resume 0} {empty_mode 0}} {
    variable option_groups
    variable flat_paths
    variable legacy_model_keys

    set root [dict create]
    set groups [dict create]
    set explicit [dict create]

    dict for {key text} $flat {
        if {$key eq "provider"} {
            continue
        }
        if {$text eq "null" && $key ne "defaultProvider"} {
            continue
        }
        if {$key in $option_groups && [string index [string trim $text] 0] eq "\{"} {
            dict set explicit $key $text
            continue
        }
        if {$key eq "configDir"} {
            if {![dict exists $flat configDirectory]} {
                dict set root configDirectory $text
            }
            continue
        }
        if {$key eq "disableResume"} {
            if {$is_resume && ![dict exists $flat suppressResumeEvent]} {
                dict set root suppressResumeEvent $text
            }
            continue
        }
        if {$is_resume && ($key eq "trustWorkingDirectory" || $key eq "refreshCustomInstructions")} {
            continue
        }
        if {!$is_resume && $key eq "openCanvases"} {
            continue
        }
        if {[dict exists $flat_paths $key]} {
            lassign [dict get $flat_paths $key] group member
            dict set groups $group $member $text
        } else {
            dict set root $key $text
        }
    }

    dict for {group text} $explicit {
        dict for {member mtext} [::copilot::jsonrpc::json_object_members $text] {
            if {$mtext eq "null" && !($group eq "inference" && $member eq "defaultProvider")} {
                continue
            }
            dict set groups $group $member $mtext
        }
    }

    if {[dict exists $flat provider]} {
        set ptext [string trim [dict get $flat provider]]
        set capi_members [dict create]
        if {[dict exists $groups github capi]} {
            set capi_members [::copilot::jsonrpc::json_object_members [dict get $groups github capi]]
        }
        set explicit_routing [expr {[dict exists $groups inference providers] \
            || [dict exists $groups inference defaultProvider] \
            || [dict exists $capi_members enabled]}]
        if {[string index $ptext 0] eq "\{" && !$explicit_routing} {
            set connection [dict create name [::copilot::jsonrpc::json_string "default"]]
            set model_fields [dict create]
            dict for {pkey pvalue} [::copilot::jsonrpc::json_object_members $ptext] {
                if {$pvalue eq "null" || $pkey eq "name"} {
                    continue
                }
                if {$pkey in $legacy_model_keys} {
                    dict set model_fields $pkey $pvalue
                } else {
                    dict set connection $pkey $pvalue
                }
            }
            if {[dict size $connection] > 1 || [dict size $model_fields] > 0} {
                dict set groups inference providers \
                    [::copilot::jsonrpc::json_array [list [::copilot::jsonrpc::json_object $connection]]]
                dict set groups inference defaultProvider [::copilot::jsonrpc::json_string "default"]
                if {[dict size $model_fields] > 0 && [dict exists $flat model] \
                        && ![dict exists $groups inference models]} {
                    set model_def [dict merge \
                        [dict create provider [::copilot::jsonrpc::json_string "default"] \
                            id [dict get $flat model]] \
                        $model_fields]
                    dict set groups inference models \
                        [::copilot::jsonrpc::json_array [list [::copilot::jsonrpc::json_object $model_def]]]
                }
                dict set capi_members enabled false
                dict set groups github capi [::copilot::jsonrpc::json_object $capi_members]
            }
        }
    }

    if {[dict exists $groups mcp servers] && ![dict exists $groups mcp envValueMode]} {
        dict set groups mcp envValueMode [::copilot::jsonrpc::json_string "direct"]
    }

    if {$empty_mode} {
        set empty_defaults [list \
            experiments  enableExperimentalMode  false \
            github       telemetryEnabled        false \
            mcp          oauthTokenStorage       [::copilot::jsonrpc::json_string "in-memory"] \
            state        skipEmbeddingRetrieval  true \
            state        embeddingCacheStorage   [::copilot::jsonrpc::json_string "in-memory"] \
            state        enableCrossSessionStore false \
            state        memory                  {{"enabled":false}} \
            instructions onDemandDiscovery       false \
            hooks        enableRepositoryHooks   false \
            git          enableHostGitOperations false \
        ]
        foreach {group member value} $empty_defaults {
            if {![dict exists $groups $group $member]} {
                dict set groups $group $member $value
            }
        }
    }

    set out $root
    dict for {group members} $groups {
        if {[dict size $members] > 0} {
            dict set out $group [::copilot::jsonrpc::json_object $members]
        }
    }
    return [::copilot::jsonrpc::json_object $out]
}

# -- Internal: GitHub token provider registry (gitHubToken.getToken) ----------

proc ::copilot::session::_new_registration_id {} {
    set hex ""
    for {set i 0} {$i < 32} {incr i} {
        append hex [format %x [expr {int(rand() * 16)}]]
    }
    return "[string range $hex 0 7]-[string range $hex 8 11]-4[string range $hex 13 15]-[string range $hex 16 19]-[string range $hex 20 31]"
}

proc ::copilot::session::_register_github_token_provider {config session_id} {
    variable github_token_providers
    set provider [dict get $config github_token_provider]
    if {$provider eq ""} {
        return ""
    }
    set registration_id [_new_registration_id]
    dict set github_token_providers $registration_id \
        [dict create provider $provider session_id $session_id]
    return $registration_id
}

proc ::copilot::session::_unregister_github_token_provider {registration_id} {
    variable github_token_providers
    if {$registration_id ne "" && [dict exists $github_token_providers $registration_id]} {
        dict unset github_token_providers $registration_id
    }
}

# Bind a registration to its session, dropping earlier registrations of the session.
proc ::copilot::session::_commit_github_token_provider {registration_id session_id} {
    variable github_token_providers
    if {$registration_id eq ""} {
        return
    }
    dict for {rid entry} $github_token_providers {
        if {$rid ne $registration_id && [dict get $entry session_id] eq $session_id} {
            dict unset github_token_providers $rid
        }
    }
    if {[dict exists $github_token_providers $registration_id]} {
        dict set github_token_providers $registration_id session_id $session_id
    }
}

# Attach the PolicyHelperFailure carried in the last JSON-RPC error's data to the
# error code ({COPILOT POLICY_HELPER_FAILURE <dict>}) of a failed create/resume.
proc ::copilot::session::_with_policy_helper_failure {opts} {
    variable last_rpc_error
    if {[dict exists $last_rpc_error data policyHelperFailure]} {
        set failure [::copilot::types::make_policy_helper_failure \
            [dict get $last_rpc_error data policyHelperFailure]]
        dict set opts -errorcode [list COPILOT POLICY_HELPER_FAILURE $failure]
    }
    return $opts
}

# -- Destroy a session --------------------------------------------------------

# Detaches the session (session.detach keeps its state on disk so it can be
# resumed) and forgets the local handle. Falls back to session.destroy on CLIs
# without session.detach.
proc ::copilot::session::destroy_session {handle} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        return
    }

    catch { detach_session $handle }
    _unregister_github_token_provider [dict get $sessions $handle registration_id]
    dict unset sessions $handle
}

# Send session.detach for a session; returns the result dict.
proc ::copilot::session::detach_session {handle} {
    set sid [_session_id $handle]
    if {$sid eq ""} {
        return [dict create]
    }
    set params [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [dict create \
        sessionId [::copilot::jsonrpc::json_string $sid]]]]
    if {[catch {_session_request $handle "session.detach" $params} result opts]} {
        variable last_rpc_error
        if {[dict exists $last_rpc_error code] && [dict get $last_rpc_error code] == -32601} {
            return [_session_request $handle "session.destroy" $params]
        }
        return -options $opts $result
    }
    return $result
}

# -- Send a message and wait for the final response ---------------------------

# Sends session.send and processes events until the session is idle. Returns a
# dict with messageId, content (the last assistant message), and events (every
# session event received for the session while waiting).
proc ::copilot::session::send_and_wait {handle opts} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }

    set sdata [dict get $sessions $handle]
    set read_ch  [dict get $sdata read_ch]
    set sid      [dict get $sdata session_id]

    set timeout_ms 60000
    if {[dict exists $opts timeout] && [dict get $opts timeout] > 0} {
        set timeout_ms [dict get $opts timeout]
    }

    # Process events until session.idle (or session.error) for this session
    variable _active_wait_sid
    set ::copilot::session::_wait_state [dict create done 0 content "" error "" events {}]
    set _active_wait_sid $sid
    try {
        set send_result [_session_request $handle "session.send" [_send_params $handle $opts]]
        set message_id ""
        if {[dict exists $send_result messageId]} {
            set message_id [dict get $send_result messageId]
        }

        set deadline [expr {[clock milliseconds] + $timeout_ms}]
        while {![dict get $::copilot::session::_wait_state done]} {
            if {[clock milliseconds] > $deadline} {
                error "Timeout after ${timeout_ms}ms waiting for session.idle"
            }
            set msg [::copilot::jsonrpc::read_message $read_ch]
            if {$msg eq ""} {
                error "Connection closed while waiting for session.idle"
            }
            _process_incoming [dict get $sdata write_ch] $msg $sid
        }
    } finally {
        set _active_wait_sid ""
    }

    set state $::copilot::session::_wait_state
    if {[dict get $state error] ne ""} {
        error "Session error: [dict get $state error]"
    }
    return [dict create \
        messageId $message_id \
        content   [dict get $state content] \
        events    [dict get $state events] \
    ]
}

# -- Send a message (fire-and-forget) -----------------------------------------

# Sends session.send without waiting; message is a prompt string or a send
# options dict (see make_send_options).
proc ::copilot::session::send_message {handle message} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }

    set sdata [dict get $sessions $handle]
    set write_ch [dict get $sdata write_ch]
    if {![catch {expr {[dict exists $message message] && [dict exists $message prompt]}} is_opts] && $is_opts} {
        set opts $message
    } else {
        set opts [::copilot::types::make_send_options message $message]
    }
    ::copilot::jsonrpc::send_request $write_ch "session.send" [_send_params $handle $opts]
}

# Build the session.send params (raw JSON) from send options.
proc ::copilot::session::_send_params {handle opts} {
    set prompt ""
    if {[dict exists $opts message]} {
        set prompt [dict get $opts message]
    }
    if {$prompt eq "" && [dict exists $opts prompt]} {
        set prompt [dict get $opts prompt]
    }
    if {$prompt eq ""} {
        error "Message must not be empty"
    }

    set p [dict create \
        sessionId [::copilot::jsonrpc::json_string [_session_id $handle]] \
        prompt    [::copilot::jsonrpc::json_string $prompt] \
    ]
    foreach {opt key} {source source client_turn_id clientTurnId agent_mode agentMode display_prompt displayPrompt} {
        if {[dict exists $opts $opt] && [dict get $opts $opt] ne ""} {
            dict set p $key [::copilot::jsonrpc::json_string [dict get $opts $opt]]
        }
    }
    if {[dict exists $opts request_headers] && [dict get $opts request_headers] ne ""} {
        dict set p requestHeaders [::copilot::jsonrpc::params_json [dict get $opts request_headers]]
    }
    if {[dict exists $opts response_schema] && [dict get $opts response_schema] ne ""} {
        # Structured output: the reply is a JSON document matching the schema.
        dict set p responseFormat [::copilot::jsonrpc::json_object [dict create \
            type       [::copilot::jsonrpc::json_string "json_schema"] \
            jsonSchema [::copilot::jsonrpc::json_object [dict create \
                name   [::copilot::jsonrpc::json_string "response"] \
                strict true \
                schema [::copilot::jsonrpc::params_json [dict get $opts response_schema]] \
            ]] \
        ]]
    }
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $p]]
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
        _process_incoming $write_ch $msg $::copilot::session::_active_wait_sid
    }

    set result [::copilot::jsonrpc::get_result $req_id]
    ::copilot::jsonrpc::remove_pending $req_id
    return $result
}

# -- Internal: route one incoming message --------------------------------------

# Server requests (method and id) are answered, notifications are dispatched
# (session events go to the owning session), and responses resolve pending
# requests. wait_sid is the session whose send_and_wait is collecting events.
proc ::copilot::session::_process_incoming {write_ch msg wait_sid} {
    variable last_rpc_error
    if {[dict exists $msg method]} {
        set method [dict get $msg method]
        set params {}
        if {[dict exists $msg params]} {
            set params [dict get $msg params]
        }
        if {[dict exists $msg id]} {
            _handle_server_request $write_ch [dict get $msg id] $method $params
        } elseif {$method eq "session.event"} {
            _handle_session_event $write_ch $params $wait_sid
        } else {
            ::copilot::jsonrpc::dispatch_notification $method $params
        }
        return
    }

    if {[dict exists $msg id]} {
        set msg_id [dict get $msg id]
        if {[dict exists $msg result]} {
            ::copilot::jsonrpc::resolve_pending $msg_id [dict get $msg result]
        } elseif {[dict exists $msg error]} {
            set err_info [dict get $msg error]
            set last_rpc_error $err_info
            set err_msg "JSON-RPC error"
            if {[dict exists $err_info message]} {
                set err_msg [dict get $err_info message]
            }
            ::copilot::jsonrpc::reject_pending $msg_id $err_msg
        }
    }
}

# -- Internal: session events --------------------------------------------------

proc ::copilot::session::_handle_session_event {write_ch params wait_sid} {
    if {![dict exists $params event]} {
        return
    }
    set event [dict get $params event]
    set sid ""
    if {[dict exists $params sessionId]} {
        set sid [dict get $params sessionId]
    }
    set type ""
    if {[dict exists $event type]} {
        set type [dict get $event type]
    }

    set handle [_handle_for_session $sid]
    if {$handle ne ""} {
        _fire_listeners $handle $event
        if {$type eq "external_tool.requested"} {
            _handle_external_tool $write_ch $sid $event
        } elseif {$type eq "permission.requested"} {
            _handle_permission_requested $write_ch $sid $handle $event
        }
    }

    # Unknown event types (including the removed tool.execution_partial_result) are
    # delivered to listeners unchanged.
    if {$wait_sid ne "" && $sid eq $wait_sid} {
        set state $::copilot::session::_wait_state
        dict lappend state events $event
        if {$type eq "assistant.message" && [dict exists $event data content]} {
            dict set state content [dict get $event data content]
        } elseif {$type eq "session.idle"} {
            dict set state done 1
        } elseif {$type eq "session.error"} {
            set message "Unknown error"
            if {[dict exists $event data message]} {
                set message [dict get $event data message]
            }
            dict set state error $message
            dict set state done 1
        }
        set ::copilot::session::_wait_state $state
    }
}

# Run a registered tool for an external_tool.requested event and answer it with
# session.tools.handlePendingToolCall (the reply is not awaited).
proc ::copilot::session::_handle_external_tool {write_ch sid event} {
    if {![dict exists $event data]} {
        return
    }
    set data [dict get $event data]
    if {[dict exists $data hasHandler] && [dict get $data hasHandler] eq "false"} {
        return
    }
    if {![dict exists $data requestId] || ![dict exists $data toolName]} {
        return
    }
    set tool_name [dict get $data toolName]
    if {$tool_name ni [::copilot::tools::list_tools]} {
        return
    }
    set arguments {}
    if {[dict exists $data arguments]} {
        set arguments [dict get $data arguments]
    }
    set p [dict create \
        sessionId [::copilot::jsonrpc::json_string $sid] \
        requestId [::copilot::jsonrpc::json_string [dict get $data requestId]] \
    ]
    set invoke_result [::copilot::tools::invoke_tool $tool_name $arguments]
    if {[::copilot::types::is_ok $invoke_result]} {
        dict set p result [::copilot::jsonrpc::json_string [::copilot::types::unwrap $invoke_result]]
    } else {
        dict set p error [::copilot::jsonrpc::json_string [dict get $invoke_result error]]
    }
    ::copilot::jsonrpc::send_request $write_ch "session.tools.handlePendingToolCall" \
        [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $p]]
}

# Answer a permission.requested event with the session's on_permission_request
# handler via session.permissions.handlePendingPermissionRequest (the reply is not
# awaited). The handler is called with a dict (kind, tool_call_id, request_id, and
# details: the full permission request) and returns a decision (see
# permission_decision_json). A failing handler reports that no user is available.
proc ::copilot::session::_handle_permission_requested {write_ch sid handle event} {
    if {![dict exists $event data]} {
        return
    }
    set data [dict get $event data]
    if {[dict exists $data resolvedByHook] && [dict get $data resolvedByHook] eq "true"} {
        return
    }
    if {![dict exists $data requestId]} {
        return
    }
    set config [dict get [_session_data $handle] config]
    if {![dict exists $config on_permission_request]} {
        return
    }
    set handler [dict get $config on_permission_request]
    if {$handler eq ""} {
        return
    }
    set details {}
    if {[dict exists $data permissionRequest]} {
        set details [dict get $data permissionRequest]
    }
    set request [dict create kind "" tool_call_id "" \
        request_id [dict get $data requestId] details $details]
    if {[dict exists $details kind]} {
        dict set request kind [dict get $details kind]
    }
    if {[dict exists $details toolCallId]} {
        dict set request tool_call_id [dict get $details toolCallId]
    }
    if {[catch {uplevel #0 [list {*}$handler $request]} decision]} {
        set decision "user-not-available"
    }
    set result [permission_decision_json $decision]
    if {$result eq ""} {
        return
    }
    ::copilot::jsonrpc::send_request $write_ch \
        "session.permissions.handlePendingPermissionRequest" \
        [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [dict create \
            sessionId [::copilot::jsonrpc::json_string $sid] \
            requestId [::copilot::jsonrpc::json_string [dict get $data requestId]] \
            result    $result \
        ]]]
}

# JSON permission decision for a handler result: "approve" / "allow" / a true
# boolean approve once, "approve-for-session" approves for the session, "deny" /
# "reject" / a false boolean reject, "user-not-available" (or an empty result)
# reports that no user is available, and a dict with "kind" (plus an optional
# "feedback" for a rejection) is sent as given. Earlier result kinds ("approved",
# "denied-*") are mapped. "no-result" returns "" (no reply: the request stays
# pending for another client).
proc ::copilot::session::permission_decision_json {decision} {
    set feedback ""
    if {![catch {llength $decision} n] && $n > 1 && $n % 2 == 0 && [dict exists $decision kind]} {
        if {[dict exists $decision feedback]} {
            set feedback [dict get $decision feedback]
        }
        set decision [dict get $decision kind]
    }
    switch -- $decision {
        no-result {
            return ""
        }
        approve - approved - allow - approve-once - 1 - true - yes {
            set kind approve-once
        }
        approve-for-session - approve-permanently {
            set kind $decision
        }
        "" - user-not-available - denied-by-rules -
        denied-no-approval-rule-and-could-not-request-from-user -
        denied-by-content-exclusion-policy {
            set kind user-not-available
        }
        default {
            set kind reject
        }
    }
    set members [dict create kind [::copilot::jsonrpc::json_string $kind]]
    if {$kind eq "reject" && $feedback ne ""} {
        dict set members feedback [::copilot::jsonrpc::json_string $feedback]
    }
    return [::copilot::jsonrpc::json_object $members]
}

# Permission handlers for on_permission_request.
proc ::copilot::session::approve_all {request} {
    return approve-once
}

proc ::copilot::session::deny_all {request} {
    return reject
}

# -- Internal: server -> client requests ---------------------------------------

proc ::copilot::session::_handle_server_request {write_ch id method params} {
    if {[catch {_server_request_result $method $params $write_ch $id} result opts]} {
        ::copilot::jsonrpc::send_error_response $write_ch $id -32603 $result
        return
    }
    if {$result eq "::copilot::session::NO_REPLY"} {
        return
    }
    ::copilot::jsonrpc::send_response $write_ch $id $result
}

# Returns the reply (raw JSON) for a server request; raises to reply with an error.
proc ::copilot::session::_server_request_result {method params write_ch id} {
    variable github_token_providers
    variable network_callbacks

    switch -- $method {
        toolCall {
            # Legacy tool call request
            _handle_tool_call $write_ch [dict create id $id params $params]
            return "::copilot::session::NO_REPLY"
        }
        gitHubToken.getToken {
            set registration_id ""
            if {[dict exists $params registrationId]} {
                set registration_id [dict get $params registrationId]
            }
            if {![dict exists $github_token_providers $registration_id]} {
                error "No GitHub token provider registered for registration ID \"$registration_id\""
            }
            set entry [dict get $github_token_providers $registration_id]
            set args [dict create host "" session_id [dict get $entry session_id] reason ""]
            foreach {key wire} {host host session_id sessionId reason reason} {
                if {[dict exists $params $wire] && [dict get $params $wire] ne "null"} {
                    dict set args $key [dict get $params $wire]
                }
            }
            set token [uplevel #0 [list {*}[dict get $entry provider] $args]]
            return [::copilot::jsonrpc::json_raw [_github_token_result_json $token]]
        }
        hooks.invoke {
            set sid [_param $params sessionId]
            set hook_type [_param $params hookType]
            set input {}
            if {[dict exists $params input]} {
                set input [dict get $params input]
            }
            set output "null"
            set handle [_handle_for_session $sid]
            if {$handle ne ""} {
                set handlers [_hook_handlers [dict get [_session_data $handle] config]]
                if {[dict exists $handlers $hook_type]} {
                    set hook_result ""
                    catch {
                        set hook_result [uplevel #0 [list {*}[dict get $handlers $hook_type] \
                            $input [dict create session_id $sid]]]
                    }
                    if {$hook_result ne ""} {
                        set output [::copilot::jsonrpc::params_json $hook_result]
                    }
                }
            }
            return [::copilot::jsonrpc::json_raw "{\"output\":$output}"]
        }
        shellAuthentication.request {
            return [::copilot::jsonrpc::json_raw [_shell_authentication_reply $params]]
        }
        shellAuthentication.cancel {
            # Handlers run synchronously, so a cancellation only arrives after the
            # handler for that request has returned; acknowledge it.
            return [::copilot::jsonrpc::json_raw "\{\}"]
        }
        memoryStore.store {
            set sid [_param $params sessionId]
            set handle [_handle_for_session $sid]
            if {$handle eq ""} {
                error "Memory store owner is unavailable"
            }
            set handler [dict get [_session_data $handle] config store_memory]
            if {$handler eq ""} {
                error "No memory store is registered"
            }
            set memory [dict create \
                session_id $sid \
                subject    [_param $params subject] \
                fact       [_param $params fact] \
                citations  [_param $params citations] \
                reason     [_param $params reason] \
            ]
            if {[catch {uplevel #0 [list {*}$handler $memory [dict create session_id $sid]]} err]} {
                puts stderr "Memory store callback failed: $err"
                error "Memory store callback failed"
            }
            return [::copilot::jsonrpc::json_raw null]
        }
        network.resolveProxy {
            set proxy [_network_callback resolve_proxy [dict create url [_param $params url]]]
            if {$proxy eq ""} {
                return [::copilot::jsonrpc::json_raw "\{\}"]
            }
            return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [dict create \
                proxy [::copilot::jsonrpc::json_string $proxy]]]]
        }
        network.authenticateNegotiate {
            set token [_network_callback authenticate_negotiate [dict create spn [_param $params spn]]]
            if {$token eq ""} {
                return [::copilot::jsonrpc::json_raw "\{\}"]
            }
            return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object [dict create \
                token [::copilot::jsonrpc::json_string $token]]]]
        }
        network.authenticateBasic {
            set request [dict create \
                proxy [_param $params proxy] \
                realm [_param $params realm] \
                previous_rejected [expr {[_param $params previousRejected] eq "true"}] \
            ]
            set credentials [_network_callback authenticate_basic $request]
            # Send only the credential fields, never other members of the result.
            set members [dict create]
            foreach key {username password} {
                if {$credentials ne "" && [dict exists $credentials $key]} {
                    dict set members $key [::copilot::jsonrpc::json_string [dict get $credentials $key]]
                }
            }
            return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
        }
        exitPlanMode.request {
            set handle [_handle_for_session [_param $params sessionId]]
            set handler ""
            if {$handle ne ""} {
                set sdata [_session_data $handle]
                if {[dict exists $sdata exit_plan_mode_handler]} {
                    set handler [dict get $sdata exit_plan_mode_handler]
                }
            }
            if {$handler eq ""} {
                return [::copilot::jsonrpc::json_raw {{"approved":true}}]
            }
            set decision [uplevel #0 [list {*}$handler $params]]
            set members [dict create approved true]
            if {[dict exists $decision approved]} {
                dict set members approved [::copilot::jsonrpc::json_bool [dict get $decision approved]]
            }
            foreach {key wire} {selected_action selectedAction selectedAction selectedAction feedback feedback} {
                if {[dict exists $decision $key]} {
                    dict set members $wire [::copilot::jsonrpc::json_string [dict get $decision $key]]
                }
            }
            return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
        }
        default {
            ::copilot::jsonrpc::send_error_response $write_ch $id -32601 "Method not found: $method"
            return "::copilot::session::NO_REPLY"
        }
    }
}

# A request parameter, or "" when absent or null.
proc ::copilot::session::_param {params key} {
    if {[dict exists $params $key] && [dict get $params $key] ne "null"} {
        return [dict get $params $key]
    }
    return ""
}

# Call a network callback (resolve_proxy, authenticate_negotiate, authenticate_basic).
proc ::copilot::session::_network_callback {name request} {
    variable network_callbacks
    if {![dict exists $network_callbacks $name] || [dict get $network_callbacks $name] eq ""} {
        error "No network $name callback is registered"
    }
    return [uplevel #0 [list {*}[dict get $network_callbacks $name] $request]]
}

# Register the client's network callbacks (dict: resolve_proxy, authenticate_negotiate,
# authenticate_basic).
proc ::copilot::session::set_network_callbacks {callbacks} {
    variable network_callbacks
    set network_callbacks $callbacks
}

# Shell authentication reply JSON for a shellAuthentication.request. Handler
# failures and anything but an accepted string answer cancel the prompt; the
# answer is never logged.
proc ::copilot::session::_shell_authentication_reply {params} {
    set cancel {{"action":"cancel"}}
    set input_kind [_param $params inputKind]
    if {$input_kind ni {secret text confirm}} {
        return $cancel
    }
    set sid [_param $params sessionId]
    set handle [_handle_for_session $sid]
    if {$handle eq ""} {
        return $cancel
    }
    set handler [dict get [_session_data $handle] config on_shell_authentication_request]
    if {$handler eq ""} {
        return $cancel
    }
    set request [dict create \
        request_id [_param $params requestId] \
        program    [_param $params program] \
        prompt     [_param $params prompt] \
        input_kind $input_kind \
        command    [_param $params command] \
        cwd        [_param $params cwd] \
    ]
    if {[catch {uplevel #0 [list {*}$handler $request [dict create session_id $sid]]} answer]} {
        return $cancel
    }
    if {[catch {dict get $answer action} action] || $action ne "accept" \
            || ![dict exists $answer value]} {
        return $cancel
    }
    return [::copilot::jsonrpc::json_object [dict create \
        action [::copilot::jsonrpc::json_string "accept"] \
        value  [::copilot::jsonrpc::json_string [dict get $answer value]] \
    ]]
}

# Normalize a GitHub token provider result (dict with kind/access_token/token_type/
# expires_in, or raw JSON) to its wire JSON.
proc ::copilot::session::_github_token_result_json {result} {
    if {[::copilot::jsonrpc::is_raw $result]} {
        return [::copilot::jsonrpc::to_json $result]
    }
    set kind "token"
    if {[dict exists $result kind]} {
        set kind [dict get $result kind]
    }
    if {$kind eq "cancelled"} {
        return {{"kind":"cancelled"}}
    }
    set members [dict create kind [::copilot::jsonrpc::json_string $kind]]
    foreach {key wire} {access_token accessToken accessToken accessToken token_type tokenType tokenType tokenType} {
        if {[dict exists $result $key]} {
            dict set members $wire [::copilot::jsonrpc::json_string [dict get $result $key]]
        }
    }
    foreach key {expires_in expiresIn} {
        if {[dict exists $result $key]} {
            dict set members expiresIn [::copilot::jsonrpc::json_number [dict get $result $key]]
        }
    }
    return [::copilot::jsonrpc::json_object $members]
}

# Hook handlers configured for a session: hookType -> command prefix.
proc ::copilot::session::_hook_handlers {config} {
    set handlers [dict create]
    set hooks [dict get $config hooks]
    foreach {name hook_type} {
        on_pre_tool_use preToolUse on_post_tool_use postToolUse
        on_user_prompt_submitted userPromptSubmitted
        on_user_prompt_transformed userPromptTransformed
        on_session_start sessionStart on_session_end sessionEnd
        on_error_occurred errorOccurred on_pre_mcp_tool_call preMcpToolCall
        on_subagent_start subagentStart on_subagent_stop subagentStop
    } {
        if {$hooks ne "" && [dict exists $hooks $name] && [dict get $hooks $name] ne ""} {
            dict set handlers $hook_type [dict get $hooks $name]
        }
    }
    foreach {key hook_type} {on_post_tool_use postToolUse on_pre_mcp_tool_call preMcpToolCall} {
        if {[dict get $config $key] ne "" && ![dict exists $handlers $hook_type]} {
            dict set handlers $hook_type [dict get $config $key]
        }
    }
    return $handlers
}

# -- Internal: session lookups and requests -------------------------------------

proc ::copilot::session::_session_data {handle} {
    variable sessions
    if {![dict exists $sessions $handle]} {
        error "Session not found: $handle"
    }
    return [dict get $sessions $handle]
}

proc ::copilot::session::_session_id {handle} {
    return [dict get [_session_data $handle] session_id]
}

proc ::copilot::session::_handle_for_session {sid} {
    variable sessions
    if {$sid eq ""} {
        return ""
    }
    dict for {handle sdata} $sessions {
        if {[dict get $sdata session_id] eq $sid} {
            return $handle
        }
    }
    return ""
}

# Send a request on a session's connection and wait for its result.
proc ::copilot::session::_session_request {handle method params} {
    set sdata [_session_data $handle]
    set write_ch [dict get $sdata write_ch]
    set req_id [::copilot::jsonrpc::send_request $write_ch $method $params]
    ::copilot::jsonrpc::register_pending $req_id
    return [_wait_for_response [dict get $sdata read_ch] $write_ch $req_id]
}

proc ::copilot::session::_session_params {handle {extra {}}} {
    set members [dict create sessionId [::copilot::jsonrpc::json_string [_session_id $handle]]]
    set members [dict merge $members $extra]
    return [::copilot::jsonrpc::json_raw [::copilot::jsonrpc::json_object $members]]
}

# -- Session methods ------------------------------------------------------------

# Change the experimental Auto routing preference (efficiency, balance,
# intelligence, or fast) without changing the model; "" or "null" restores the
# provider's default routing.
proc ::copilot::session::set_auto_tier {handle tier} {
    if {$tier eq "" || $tier eq "null"} {
        set tier_json null
    } else {
        set tier_json [::copilot::jsonrpc::json_string $tier]
    }
    return [_session_request $handle "session.model.switchAutoTier" \
        [_session_params $handle [dict create autoTier $tier_json]]]
}

# Managed (enterprise) settings resolved for the session. A policyHelperFailure
# member can be read with ::copilot::types::make_policy_helper_failure.
proc ::copilot::session::get_managed_settings {handle} {
    return [_session_request $handle "session.managedSettings.get" [_session_params $handle]]
}

# Re-run the managed-settings policy helper for the session.
proc ::copilot::session::recheck_policy_helper {handle} {
    return [_session_request $handle "session.managedSettings.recheckPolicyHelper" \
        [_session_params $handle]]
}

# Change the session's permission mode (manual, assisted, or allow-all). Returns a
# dict with success, mode, and (when refused for a known cause) reason, such as
# managed_policy (see ::copilot::types::permission_mode_refusal_reason).
proc ::copilot::session::set_permission_mode {handle mode} {
    set result [_session_request $handle "session.permissions.setMode" \
        [_session_params $handle [dict create mode [::copilot::jsonrpc::json_string $mode]]]]
    set reason ""
    if {[dict exists $result reason]} {
        set reason [dict get $result reason]
    }
    set success 0
    if {[dict exists $result success]} {
        set success [string is true -strict [dict get $result success]]
    }
    set applied_mode ""
    if {[dict exists $result mode]} {
        set applied_mode [dict get $result mode]
    }
    return [dict create success $success mode $applied_mode reason $reason]
}

# Register the exit-plan-mode handler of a session (called with the request params
# dict; returns a dict with approved and optional selected_action / feedback).
proc ::copilot::session::register_exit_plan_mode_handler {handle handler} {
    variable sessions
    _session_data $handle
    dict set sessions $handle exit_plan_mode_handler $handler
}

# Register a trace context provider for a session.
proc ::copilot::session::register_trace_context_provider {handle provider} {
    variable sessions
    _session_data $handle
    dict set sessions $handle trace_context_provider $provider
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

package provide copilot::session 2.0.0

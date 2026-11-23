# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

# CopilotClient -- main entry point for the Copilot SDK.
#
# Manages the connection to the Copilot CLI server (spawned or external) and
# provides session creation, status queries, and lifecycle management.

const MIN_PROTOCOL_VERSION = 4
const SDK_PROTOCOL_VERSION = 4

"""
    CopilotClient

Main client for interacting with the Copilot CLI.

# Example

```julia
client = CopilotClient(CopilotClientOptions(cli_path="/path/to/copilot-cli"))
start!(client)
session = create_session(client, SessionConfig(model="gpt-4", on_permission_request=_ -> true))
on(session) do event
    if event.type == "assistant.message"
        println(get(event.data, "content", ""))
    end
end
send(session, "Hello!")
disconnect(session)
stop!(client)
```
"""
mutable struct CopilotClient
    options::CopilotClientOptions
    rpc::Union{JsonRpcClient, Nothing}
    process::Union{Base.Process, Nothing}
    state::ConnectionState
    sessions::Dict{String, CopilotSession}
    # gitHubToken.getToken registrations: registration ID => (provider, session ID)
    github_token_providers::Dict{String, Any}

    CopilotClient(opts::CopilotClientOptions) =
        new(_validate_client_options(opts), nothing, nothing, DISCONNECTED,
            Dict{String, CopilotSession}(), Dict{String, Any}())
end

# Allow keyword construction: CopilotClient(cli_path="...", ...)
# Also serves as the zero-arg constructor: CopilotClient()
CopilotClient(; kwargs...) = CopilotClient(CopilotClientOptions(; kwargs...))

# -- Lifecycle --------------------------------------------------------------------

"""
    start!(client) -> CopilotClient

Start the CLI server process (or connect to an external one). Must be called
before `create_session`.
"""
function start!(client::CopilotClient)
    client.state == CONNECTED && return client
    client.state = CONNECTING

    try
        if client.options.cli_url !== nothing
            _connect_tcp!(client)
        else
            _spawn_stdio!(client)
        end
    catch
        client.state = CONNECTION_ERROR
        if client.rpc !== nothing
            try stop!(client.rpc) catch; end
            client.rpc = nothing
        end
        rethrow()
    end

    client.state = CONNECTED

    # Set up session filesystem if configured
    if client.options.session_fs !== nothing
        try
            set_session_fs_provider(client, client.options.session_fs)
        catch; end
    end

    return client
end

"""
    stop!(client)

Stop the CLI server and close all sessions.
"""
function stop!(client::CopilotClient)
    for (_, session) in client.sessions
        try disconnect(session) catch; end
    end
    empty!(client.sessions)
    empty!(client.github_token_providers)

    if client.rpc !== nothing
        stop!(client.rpc)
        client.rpc = nothing
    end

    client.process = nothing
    client.state = DISCONNECTED
    return nothing
end

# -- Session management -----------------------------------------------------------

"""
    create_session(client, config::SessionConfig) -> CopilotSession
    create_session(client; kwargs...) -> CopilotSession

Create a new conversation session.

# Required keyword
- `on_permission_request` -- callback `(PermissionRequest) -> Bool/"allow"/"deny"`.
"""
function create_session(client::CopilotClient, config::SessionConfig)
    _ensure_started!(client)
    _require_available_tools(client, config)

    flat = _session_flat_params(client, config; resume=false)
    registration_id = _register_github_token_provider!(client, config, nothing)
    if registration_id !== nothing
        flat["gitHubTokenProviderRegistrationId"] = registration_id
    end
    # Translate the flat options to the SDK protocol 4 grouped payload
    params = _session_wire_params(client, flat, false)

    # A failed create can carry a PolicyHelperFailure: see `policy_helper_failure(err)`.
    result = try
        send_request(client.rpc, "session.create", params; timeout=30)
    catch
        registration_id !== nothing && delete!(client.github_token_providers, registration_id)
        rethrow()
    end

    session_id = if result isa AbstractDict
        string(get(result, "sessionId", get(result, "session_id", string(UUIDs.uuid4()))))
    else
        string(UUIDs.uuid4())
    end

    session = CopilotSession(session_id, client.rpc, config)
    client.sessions[session_id] = session
    _commit_github_token_provider!(client, session, registration_id)

    # Register session-level RPC handlers
    _register_session_handlers!(client, session)

    return session
end

create_session(client::CopilotClient; kwargs...) =
    create_session(client, SessionConfig(; kwargs...))

# -- SDK protocol 4 session payloads ----------------------------------------------

# Option groups of the SDK protocol 4 session.create / session.resume payload.
const _SESSION_OPTION_GROUPS = Set([
    "inference", "tools", "permissions", "interaction", "events", "agents", "skills", "mcp",
    "github", "filesystem", "execution", "instructions", "hooks", "git", "state", "plugins",
    "canvases", "extensions", "experiments", "policy",
])

# Flat session option => (group, member) in the SDK protocol 4 payload. Root options
# (sessionId, model, systemMessage, ...) and unknown options are not listed and stay at
# the top level.
const _SESSION_PARAM_GROUPS = Dict{String, Tuple{String, String}}(
    "reasoningEffort" => ("inference", "reasoningEffort"),
    "reasoningSummary" => ("inference", "reasoningSummary"),
    "contextTier" => ("inference", "contextTier"),
    "allowMultipleToolCalls" => ("inference", "allowMultipleToolCalls"),
    "enableCitations" => ("inference", "enableCitations"),
    "modelCapabilities" => ("inference", "capabilities"),
    "verbosity" => ("inference", "verbosity"),
    "providers" => ("inference", "providers"),
    "models" => ("inference", "models"),
    "defaultProvider" => ("inference", "defaultProvider"),
    "tools" => ("tools", "definitions"),
    "availableTools" => ("tools", "availableTools"),
    "excludedTools" => ("tools", "excludedTools"),
    "toolSearch" => ("tools", "search"),
    "largeOutput" => ("tools", "largeOutput"),
    "imageGeneration" => ("tools", "imageGeneration"),
    "manageScheduleEnabled" => ("tools", "manageScheduleEnabled"),
    "webSearch" => ("tools", "webSearch"),
    "shell" => ("tools", "shell"),
    "maxInlineBinaryBytes" => ("tools", "maxInlineBinaryBytes"),
    "requestPermission" => ("permissions", "request"),
    "sandboxConfig" => ("permissions", "sandbox"),
    "sandbox" => ("permissions", "sandbox"),
    "permissionMode" => ("permissions", "mode"),
    "sandboxConfigSource" => ("permissions", "sandboxSource"),
    "commands" => ("interaction", "commands"),
    "askUserVariant" => ("interaction", "askUserVariant"),
    "requestUserInput" => ("interaction", "requestUserInput"),
    "requestElicitation" => ("interaction", "requestElicitation"),
    "requestShellAuthentication" => ("interaction", "requestShellAuthentication"),
    "requestExitPlanMode" => ("interaction", "requestExitPlanMode"),
    "requestAutoModeSwitch" => ("interaction", "requestAutoModeSwitch"),
    "requestAutoModeRecovery" => ("interaction", "requestAutoModeRecovery"),
    "streaming" => ("events", "streaming"),
    "includeSubAgentStreamingEvents" => ("events", "includeSubAgentStreamingEvents"),
    "customAgents" => ("agents", "definitions"),
    "defaultAgent" => ("agents", "default"),
    "agent" => ("agents", "selected"),
    "includedBuiltinAgents" => ("agents", "includedBuiltinAgents"),
    "excludedBuiltinAgents" => ("agents", "excludedBuiltinAgents"),
    "customAgentsLocalOnly" => ("agents", "localDiscoveryOnly"),
    "agentDirectories" => ("agents", "directories"),
    "skillDirectories" => ("skills", "directories"),
    "enableSkills" => ("skills", "enabled"),
    "includedBuiltinSkills" => ("skills", "includedBuiltinSkills"),
    "disabledSkills" => ("skills", "disabledSkills"),
    "hasSkillProvider" => ("skills", "hasProvider"),
    "ignoredSkillsLocations" => ("skills", "ignoredLocations"),
    "mcpServers" => ("mcp", "servers"),
    "disabledMcpServers" => ("mcp", "disabledServers"),
    "requestMcpApps" => ("mcp", "enableApps"),
    "enableMcpApps" => ("mcp", "enableApps"),
    "mcpOAuthTokenStorage" => ("mcp", "oauthTokenStorage"),
    "authClientIdMetadataUrl" => ("mcp", "authClientIdMetadataUrl"),
    "envValueMode" => ("mcp", "envValueMode"),
    "managedMcpServers" => ("mcp", "managedServers"),
    "allowAllMcpServerInstructions" => ("mcp", "allowAllServerInstructions"),
    "gitHubToken" => ("github", "token"),
    "gitHubTokenProviderRegistrationId" => ("github", "tokenProviderRegistrationId"),
    "githubMcpToolConfig" => ("github", "mcp"),
    "capi" => ("github", "capi"),
    "enableSessionTelemetry" => ("github", "telemetryEnabled"),
    "enableManagedSettings" => ("github", "fetchManagedSettings"),
    "interactiveUser" => ("github", "interactiveUser"),
    "enableGitHubTelemetryForwarding" => ("github", "telemetryForwarding"),
    "authInfo" => ("github", "authInfo"),
    "copilotUrl" => ("github", "copilotUrl"),
    "additionalDirectories" => ("filesystem", "additionalDirectories"),
    "enableFileChangeTracking" => ("filesystem", "trackFileChanges"),
    "trustWorkingDirectory" => ("filesystem", "trustWorkingDirectory"),
    "sessionLimits" => ("execution", "limits"),
    "autopilot" => ("execution", "autopilot"),
    "autopilotContinuation" => ("execution", "autopilotContinuation"),
    "instructionDirectories" => ("instructions", "directories"),
    "organizationCustomInstructions" => ("instructions", "organizationText"),
    "enableOnDemandInstructionDiscovery" => ("instructions", "onDemandDiscovery"),
    "skipCustomInstructions" => ("instructions", "skipCustomInstructions"),
    "refreshCustomInstructions" => ("instructions", "refreshCustomInstructions"),
    "hooks" => ("hooks", "hasHandlers"),
    "hookDirectories" => ("hooks", "directories"),
    "enableFileHooks" => ("hooks", "enableRepositoryHooks"),
    "coauthorEnabled" => ("git", "coauthorEnabled"),
    "enableHostGitOperations" => ("git", "enableHostGitOperations"),
    "infiniteSessions" => ("state", "infiniteSessions"),
    "memory" => ("state", "memory"),
    "hasMemoryStore" => ("state", "hasMemoryStore"),
    "enableSessionStore" => ("state", "enableCrossSessionStore"),
    "skipEmbeddingRetrieval" => ("state", "skipEmbeddingRetrieval"),
    "embeddingCacheStorage" => ("state", "embeddingCacheStorage"),
    "pluginDirectories" => ("plugins", "directories"),
    "installedPlugins" => ("plugins", "installed"),
    "canvases" => ("canvases", "definitions"),
    "canvasProvider" => ("canvases", "provider"),
    "requestCanvasRenderer" => ("canvases", "requestRenderer"),
    "openCanvases" => ("canvases", "open"),
    "requestExtensions" => ("extensions", "enabled"),
    "extensionSdkPath" => ("extensions", "sdkPath"),
    "extensionInfo" => ("extensions", "identity"),
    "enableExperimentalMode" => ("experiments", "enableExperimentalMode"),
    "isExperimentalMode" => ("experiments", "enableExperimentalMode"),
    "featureFlags" => ("experiments", "flags"),
    "expAssignments" => ("experiments", "assignments"),
    "featureFlagConfiguration" => ("experiments", "configuration"),
    "allowedModels" => ("policy", "allowedModels"),
    "managedSettings" => ("policy", "managedSettings"),
    "excludedManagedSettings" => ("policy", "excludedManagedSettings"),
    "enforceManagedModelDefaults" => ("policy", "enforceManagedModelDefaults"),
)

# Members of a legacy single provider that belong to a model definition.
const _LEGACY_PROVIDER_MODEL_KEYS = (
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens",
    "capabilities",
)

"""
    group_session_params(flat, resume=false) -> Dict{String,Any}

Convert a flat `session.create` / `session.resume` payload to the SDK protocol 4 grouped
payload. Root keys (`sessionId`, `model`, `systemMessage`, ...) and unknown keys stay at the
top level; every other known flat key moves into its option group (for example `streaming`
becomes `events.streaming` and `hooks => true` becomes `hooks.hasHandlers`). Dicts already
keyed by a group name are merged with their explicit members winning, `nothing` values are
skipped (a `missing` `defaultProvider` is sent as JSON null to clear the saved default),
empty groups are not emitted, `configDir` becomes `configDirectory`, and `disableResume`
becomes `suppressResumeEvent` on resume. On resume, the create-only
`filesystem.trustWorkingDirectory` and `instructions.refreshCustomInstructions` are dropped;
`canvases.open` is only sent on resume. A legacy single `provider` becomes the named
`"default"` connection with Copilot API models disabled, unless named providers, a default
provider, or `capi.enabled` are set explicitly.
"""
function group_session_params(flat::AbstractDict, resume::Bool=false)
    root = Dict{String, Any}()
    groups = Dict{String, Dict{String, Any}}()
    explicit = Dict{String, AbstractDict}()
    setmember!(group, member, value) = (get!(groups, group, Dict{String, Any}())[member] = value)

    for (k, value) in flat
        key = String(k)
        key == "provider" && continue
        value === nothing && continue
        value === missing && key != "defaultProvider" && continue
        if key in _SESSION_OPTION_GROUPS && value isa AbstractDict
            explicit[key] = value
            continue
        end
        if key == "configDir"
            haskey(flat, "configDirectory") || (root["configDirectory"] = value)
            continue
        end
        if key == "disableResume"
            if resume && !haskey(flat, "suppressResumeEvent")
                root["suppressResumeEvent"] = value
            end
            continue
        end
        resume && key in ("trustWorkingDirectory", "refreshCustomInstructions") && continue
        !resume && key == "openCanvases" && continue
        path = get(_SESSION_PARAM_GROUPS, key, nothing)
        if path === nothing
            root[key] = value
        else
            setmember!(path[1], path[2], value === missing ? nothing : value)
        end
    end

    for (group, members) in explicit
        for (m, value) in members
            member = String(m)
            if value === nothing && !(group == "inference" && member == "defaultProvider")
                continue
            end
            setmember!(group, member, value)
        end
    end

    provider = get(flat, "provider", nothing)
    if provider isa AbstractDict && !isempty(provider)
        inference = get(groups, "inference", Dict{String, Any}())
        capi = get(get(groups, "github", Dict{String, Any}()), "capi", nothing)
        explicit_routing = haskey(inference, "providers") || haskey(inference, "defaultProvider") ||
            (capi isa AbstractDict && haskey(capi, "enabled"))
        if !explicit_routing
            connection = Dict{String, Any}("name" => "default")
            model_fields = Dict{String, Any}()
            for (k, value) in provider
                key = String(k)
                (value === nothing || key == "name") && continue
                if key in _LEGACY_PROVIDER_MODEL_KEYS
                    model_fields[key] = value
                else
                    connection[key] = value
                end
            end
            setmember!("inference", "providers", Any[connection])
            setmember!("inference", "defaultProvider", "default")
            model = get(flat, "model", nothing)
            if !isempty(model_fields) && model !== nothing && !haskey(groups["inference"], "models")
                setmember!("inference", "models",
                    Any[merge(Dict{String, Any}("provider" => "default", "id" => model), model_fields)])
            end
            new_capi = Dict{String, Any}()
            if capi isa AbstractDict
                for (k, value) in capi
                    new_capi[String(k)] = value
                end
            end
            new_capi["enabled"] = false
            setmember!("github", "capi", new_capi)
        end
    end

    mcp = get(groups, "mcp", nothing)
    if mcp !== nothing && haskey(mcp, "servers") && !haskey(mcp, "envValueMode")
        mcp["envValueMode"] = "direct"
    end

    for (group, members) in groups
        isempty(members) || (root[group] = members)
    end
    return root
end

# Defaults applied (under explicit options) to every session of an "empty" mode client.
const _EMPTY_MODE_SESSION_DEFAULTS = (
    ("experiments", "enableExperimentalMode", false),
    ("github", "telemetryEnabled", false),
    ("mcp", "oauthTokenStorage", "in-memory"),
    ("state", "skipEmbeddingRetrieval", true),
    ("state", "embeddingCacheStorage", "in-memory"),
    ("state", "enableCrossSessionStore", false),
    ("state", "memory", nothing),  # {"enabled": false}, built per call
    ("instructions", "onDemandDiscovery", false),
    ("hooks", "enableRepositoryHooks", false),
    ("git", "enableHostGitOperations", false),
)

function _apply_empty_mode_defaults!(params::Dict{String, Any})
    for (group, member, value) in _EMPTY_MODE_SESSION_DEFAULTS
        members = get!(params, group, Dict{String, Any}())
        if !haskey(members, member)
            members[member] = member == "memory" ? Dict{String, Any}("enabled" => false) : value
        end
    end
    return params
end

function _session_wire_params(client::CopilotClient, flat::Dict{String, Any}, resume::Bool)
    params = group_session_params(flat, resume)
    client.options.mode == CLIENT_MODE_EMPTY && _apply_empty_mode_defaults!(params)
    return params
end

function _require_available_tools(client::CopilotClient, config::SessionConfig)
    if client.options.mode == CLIENT_MODE_EMPTY && config.available_tools === nothing
        throw(ArgumentError(
            "Client mode \"empty\" requires available_tools on every create_session and " *
            "resume_session call"))
    end
    return nothing
end

function _validate_client_options(opts::CopilotClientOptions)
    if isempty(opts.mode) || opts.mode == "copilot-cli"
        opts.mode = CLIENT_MODE_CODING_AGENT
    elseif opts.mode != CLIENT_MODE_CODING_AGENT && opts.mode != CLIENT_MODE_EMPTY
        throw(ArgumentError("Invalid client mode \"$(opts.mode)\": expected \"coding-agent\" or \"empty\""))
    end
    return opts
end

# Flat session options (wire keys) shared by session.create and session.resume.
function _session_flat_params(client::CopilotClient, config::SessionConfig; resume::Bool)
    p = Dict{String, Any}(
        "model"     => config.model,
        "streaming" => config.streaming,
    )

    if config.system_message !== nothing
        p["systemMessage"] = Dict{String, Any}("mode" => "append", "content" => config.system_message)
    end
    if config.instructions !== nothing
        # Free-form instructions are applied as organization custom instructions.
        p["organizationCustomInstructions"] = config.instructions
    end
    if config.reasoning_effort !== nothing
        p["reasoningEffort"] = config.reasoning_effort
    end
    # Permission requests arrive as `permission.requested` events answered by the handler
    config.on_permission_request !== nothing && (p["requestPermission"] = true)
    if config.agent !== nothing
        p["agent"] = config.agent
    end
    if !isempty(config.excluded_tools)
        p["excludedTools"] = config.excluded_tools
    end
    if config.available_tools !== nothing
        p["availableTools"] = config.available_tools
    end
    if !resume && config.cloud !== nothing
        p["cloud"] = config.cloud
    end
    if config.mcp_servers !== nothing && !isempty(config.mcp_servers)
        p["mcpServers"] = Dict{String, Any}(name => _mcp_server_wire(cfg) for (name, cfg) in config.mcp_servers)
    end
    if config.model_capabilities !== nothing
        p["modelCapabilities"] = config.model_capabilities
    end
    if config.enable_config_discovery
        p["enableConfigDiscovery"] = true
    end
    if config.include_sub_agent_streaming_events
        p["includeSubAgentStreamingEvents"] = true
    end
    if !isempty(config.commands)
        p["commands"] = [Dict{String, Any}("name" => c.name, "description" => c.description) for c in config.commands]
    end
    if !isempty(config.skill_directories)
        p["skillDirectories"] = config.skill_directories
    end
    if !isempty(config.disabled_skills)
        p["disabledSkills"] = config.disabled_skills
    end
    if !isempty(config.instruction_directories)
        p["instructionDirectories"] = config.instruction_directories
    end
    if config.working_directory !== nothing
        p["workingDirectory"] = config.working_directory
    end
    if config.github_token !== nothing
        p["gitHubToken"] = config.github_token
    end
    if config.response_format !== nothing
        p["responseFormat"] = get(IMAGE_RESPONSE_FORMAT_STRINGS, config.response_format, "text")
    end
    # --- Upstream-sync session configuration passthroughs ---
    if config.enable_citations
        p["enableCitations"] = true
    end
    if !isempty(config.excluded_builtin_agents)
        p["excludedBuiltinAgents"] = config.excluded_builtin_agents
    end
    if config.session_limits !== nothing
        sl = config.session_limits
        sl_dict = Dict{String, Any}()
        sl.max_ai_credits !== nothing && (sl_dict["maxAiCredits"] = sl.max_ai_credits)
        sl.max_requests !== nothing && (sl_dict["maxRequests"] = sl.max_requests)
        sl.max_tokens !== nothing && (sl_dict["maxTokens"] = sl.max_tokens)
        p["sessionLimits"] = sl_dict
    end
    if config.memory !== nothing
        mem = config.memory
        mem_dict = Dict{String, Any}("enabled" => mem.enabled)
        mem.max_entries !== nothing && (mem_dict["maxEntries"] = mem.max_entries)
        mem.directory !== nothing && (mem_dict["directory"] = mem.directory)
        p["memory"] = mem_dict
    end
    if config.otlp_protocol !== nothing
        p["otlpProtocol"] = config.otlp_protocol
    end
    if config.exp_assignments !== nothing
        p["expAssignments"] = config.exp_assignments
    end
    if config.on_mcp_auth_request !== nothing
        p["mcpAuthHandler"] = true
    end
    if config.rewind_enabled
        p["rewindEnabled"] = true
    end
    if !isempty(config.additional_directories)
        p["additionalDirectories"] = config.additional_directories
    end
    if !isempty(config.disabled_mcp_servers)
        p["disabledMcpServers"] = config.disabled_mcp_servers
    end
    if config.github_mcp_tool_config !== nothing
        p["githubMcpToolConfig"] = config.github_mcp_tool_config
    end
    if config.canvas_provider !== nothing
        p["canvasProvider"] = config.canvas_provider
    end
    if config.custom_agents_local_only
        p["customAgentsLocalOnly"] = true
    end
    if config.tool_search !== nothing
        p["toolSearch"] = config.tool_search
    end
    if config.experimental_mode
        p["experimentalMode"] = true
    end
    if config.content_exclusion
        p["contentExclusion"] = true
    end
    if config.on_user_prompt_transformed !== nothing
        p["userPromptTransformed"] = true
    end

    # Convert tools
    tool_list = Tool[t for t in config.tools if t isa Tool]
    if !isempty(tool_list)
        p["tools"] = [tool_to_wire(t) for t in tool_list]
    end

    if config.on_exit_plan_mode !== nothing
        p["requestExitPlanMode"] = true
    end
    if config.enable_session_telemetry !== nothing
        p["enableSessionTelemetry"] = config.enable_session_telemetry
    end

    # --- v2.6.0 upstream-sync session options ---
    # CAPI options (github.capi on the wire): explicit capi members plus the auto routing
    # tier and the WebSocket transport toggle.
    capi = Dict{String, Any}()
    if config.capi !== nothing
        for (k, v) in config.capi
            capi[String(k)] = v
        end
    end
    config.auto_tier !== nothing && (capi["autoTier"] = config.auto_tier)
    config.enable_web_socket_responses && (capi["enableWebSocketResponses"] = true)
    isempty(capi) || (p["capi"] = capi)
    config.ask_user_variant !== nothing && (p["askUserVariant"] = config.ask_user_variant)
    config.sandbox_config !== nothing && (p["sandboxConfig"] = _sandbox_wire(config.sandbox_config))
    config.feature_flags !== nothing && (p["featureFlags"] = config.feature_flags)
    config.allowed_models !== nothing && (p["allowedModels"] = config.allowed_models)
    if config.auth_client_id_metadata_url !== nothing
        p["authClientIdMetadataUrl"] = config.auth_client_id_metadata_url
    end
    config.agent_directories !== nothing && (p["agentDirectories"] = config.agent_directories)
    config.hook_directories !== nothing && (p["hookDirectories"] = config.hook_directories)
    config.interactive_user !== nothing && (p["interactiveUser"] = config.interactive_user)
    if config.excluded_managed_settings !== nothing
        p["excludedManagedSettings"] = config.excluded_managed_settings
    end
    if config.enforce_managed_model_defaults !== nothing
        p["enforceManagedModelDefaults"] = config.enforce_managed_model_defaults
    end
    if config.image_generation !== nothing
        p["imageGeneration"] = _image_generation_wire(config.image_generation)
    end
    config.autopilot !== nothing && (p["autopilot"] = _autopilot_wire(config.autopilot))
    # Hook handlers are answered through hooks.invoke
    _has_hook_handlers(config) && (p["hooks"] = true)
    if resume
        if config.allow_transcript_recovery !== nothing
            p["allowTranscriptRecovery"] = config.allow_transcript_recovery
        end
        config.disable_resume && (p["disableResume"] = true)
    else
        config.name !== nothing && (p["name"] = config.name)
        if config.trust_working_directory !== nothing
            p["trustWorkingDirectory"] = config.trust_working_directory
        end
        if config.refresh_custom_instructions !== nothing
            p["refreshCustomInstructions"] = config.refresh_custom_instructions
        end
    end

    # --- v3.0.0 (SDK protocol 4) session options ---
    config.provider !== nothing && (p["provider"] = _provider_wire(config.provider))
    if config.providers !== nothing
        p["providers"] = Any[_named_provider_wire(x) for x in config.providers]
    end
    if config.models !== nothing
        p["models"] = Any[_provider_model_wire(x) for x in config.models]
    end
    # `missing` is sent as JSON null, which clears a saved default provider on resume.
    config.default_provider !== nothing && (p["defaultProvider"] = config.default_provider)
    config.on_shell_authentication_request !== nothing && (p["requestShellAuthentication"] = true)
    config.store_memory !== nothing && (p["hasMemoryStore"] = true)
    return p
end

# Session hook handlers answered through `hooks.invoke`: hookType => SessionConfig field.
const _HOOK_HANDLER_FIELDS = Dict{String, Symbol}(
    "postToolUse"           => :on_post_tool_use,
    "preMcpToolCall"        => :on_pre_mcp_tool_call,
    "userPromptTransformed" => :on_user_prompt_transformed,
    "subagentStart"         => :on_subagent_start,
    "subagentStop"          => :on_subagent_stop,
)

_has_hook_handlers(config::SessionConfig) =
    any(field -> getproperty(config, field) !== nothing, values(_HOOK_HANDLER_FIELDS))

function _mcp_server_wire(cfg::McpServerConfig)
    w = Dict{String, Any}("type" => get(MCP_SERVER_TYPE_STRINGS, cfg.type, "stdio"), "tools" => cfg.tools)
    cfg.command !== nothing && (w["command"] = cfg.command)
    isempty(cfg.args) || (w["args"] = cfg.args)
    cfg.url !== nothing && (w["url"] = cfg.url)
    cfg.env !== nothing && (w["env"] = cfg.env)
    cfg.headers !== nothing && (w["headers"] = cfg.headers)
    return w
end

function _sandbox_wire(c::SandboxConfig)
    w = Dict{String, Any}("enabled" => c.enabled)
    c.user_policy !== nothing && (w["userPolicy"] = c.user_policy)
    c.add_current_working_directory !== nothing && (w["addCurrentWorkingDirectory"] = c.add_current_working_directory)
    c.sandbox_mcp_servers !== nothing && (w["sandboxMcpServers"] = c.sandbox_mcp_servers)
    c.sandbox_lsp_servers !== nothing && (w["sandboxLspServers"] = c.sandbox_lsp_servers)
    c.sandbox_extensions !== nothing && (w["sandboxExtensions"] = c.sandbox_extensions)
    c.allow_bypass !== nothing && (w["allowBypass"] = c.allow_bypass)
    return w
end

function _image_generation_wire(c::ImageGenerationConfig)
    w = Dict{String, Any}()
    c.enabled !== nothing && (w["enabled"] = c.enabled)
    c.watermark !== nothing && (w["watermark"] = c.watermark)
    c.save_to_disk !== nothing && (w["saveToDisk"] = c.save_to_disk)
    return w
end

function _autopilot_wire(c::AutopilotOptions)
    w = Dict{String, Any}()
    c.continuation_enabled !== nothing && (w["continuationEnabled"] = c.continuation_enabled)
    c.max_continues !== nothing && (w["maxContinues"] = c.max_continues)
    return w
end

# Connection members shared by ProviderConfig and NamedProviderConfig.
function _connection_wire!(w::Dict{String, Any}, c)
    c.type !== nothing && (w["type"] = c.type)
    c.base_url !== nothing && (w["baseUrl"] = c.base_url)
    c.api_key !== nothing && (w["apiKey"] = c.api_key)
    c.api_key_command !== nothing && (w["apiKeyCommand"] = c.api_key_command)
    c.bearer_token !== nothing && (w["bearerToken"] = c.bearer_token)
    c.wire_api !== nothing && (w["wireApi"] = c.wire_api)
    c.transport !== nothing && (w["transport"] = c.transport)
    c.headers !== nothing && (w["headers"] = c.headers)
    c.azure_api_version !== nothing && (w["azure"] = Dict{String, Any}("apiVersion" => c.azure_api_version))
    c.model_provider !== nothing && (w["modelProvider"] = c.model_provider)
    return w
end

function _provider_wire(c::ProviderConfig)
    w = _connection_wire!(Dict{String, Any}(), c)
    c.model_id !== nothing && (w["modelId"] = c.model_id)
    c.wire_model !== nothing && (w["wireModel"] = c.wire_model)
    c.max_prompt_tokens !== nothing && (w["maxPromptTokens"] = c.max_prompt_tokens)
    c.max_context_window_tokens !== nothing && (w["maxContextWindowTokens"] = c.max_context_window_tokens)
    c.max_output_tokens !== nothing && (w["maxOutputTokens"] = c.max_output_tokens)
    return w
end

_named_provider_wire(c::NamedProviderConfig) =
    _connection_wire!(Dict{String, Any}("name" => c.name), c)

function _provider_model_wire(c::ProviderModelConfig)
    w = Dict{String, Any}("provider" => c.provider, "id" => c.id)
    c.model_id !== nothing && (w["modelId"] = c.model_id)
    c.wire_model !== nothing && (w["wireModel"] = c.wire_model)
    c.name !== nothing && (w["name"] = c.name)
    c.max_prompt_tokens !== nothing && (w["maxPromptTokens"] = c.max_prompt_tokens)
    c.max_context_window_tokens !== nothing && (w["maxContextWindowTokens"] = c.max_context_window_tokens)
    c.max_output_tokens !== nothing && (w["maxOutputTokens"] = c.max_output_tokens)
    c.capabilities !== nothing && (w["capabilities"] = c.capabilities)
    return w
end

# -- GitHub token provider registry (gitHubToken.getToken) -------------------------

function _register_github_token_provider!(client::CopilotClient, config::SessionConfig,
                                          session_id::Union{String, Nothing})
    config.github_token_provider === nothing && return nothing
    if config.github_token !== nothing
        throw(ArgumentError("github_token and github_token_provider are mutually exclusive"))
    end
    registration_id = string(UUIDs.uuid4())
    client.github_token_providers[registration_id] = (config.github_token_provider, session_id)
    return registration_id
end

# Bind a registration to its session (dropping earlier ones for the session) and remove it
# when the session is disconnected.
function _commit_github_token_provider!(client::CopilotClient, session::CopilotSession,
                                        registration_id::Union{String, Nothing})
    registration_id === nothing && return nothing
    sid = session.session_id
    for (other, entry) in collect(client.github_token_providers)
        other != registration_id && entry[2] == sid && delete!(client.github_token_providers, other)
    end
    provider = client.github_token_providers[registration_id][1]
    client.github_token_providers[registration_id] = (provider, sid)
    push!(session.close_callbacks, () -> delete!(client.github_token_providers, registration_id))
    return nothing
end

# -- Server queries ---------------------------------------------------------------

"""
    get_status(client) -> ServerStatus

Query the server for version and authentication status.
"""
function get_status(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "status.get", nothing; timeout=10)
    if result isa Dict
        ServerStatus(
            get(result, "version", ""),
            get(result, "protocolVersion", 0),
            get(result, "authenticated", false),
        )
    else
        ServerStatus()
    end
end

"""
    get_models(client) -> Vector{ModelInfo}

List available models from the server.
"""
function get_models(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "models.list", nothing; timeout=10)
    models = ModelInfo[]
    if result isa AbstractVector
        for m in result
            if m isa Dict
                push!(models, ModelInfo(;
                    id         = get(m, "id", ""),
                    name       = get(m, "name", ""),
                    vendor     = get(m, "vendor", ""),
                    family     = get(m, "family", ""),
                    version    = get(m, "version", ""),
                    is_default = get(m, "isDefault", false),
                ))
            end
        end
    end
    return models
end

"""
    list_sessions(client) -> Vector{SessionMetadata}

List sessions known to the server.
"""
function list_sessions(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.list", nothing; timeout=10)
    sessions = SessionMetadata[]
    if result isa AbstractVector
        for s in result
            if s isa Dict
                push!(sessions, SessionMetadata(;
                    session_id = get(s, "sessionId", ""),
                    title      = get(s, "title", nothing),
                    created_at = get(s, "createdAt", nothing),
                    updated_at = get(s, "updatedAt", nothing),
                    model      = get(s, "model", nothing),
                ))
            end
        end
    end
    return sessions
end

"""
    resume_session(client, session_id; kwargs...) -> CopilotSession

Resume an existing session by ID.
"""
function resume_session(client::CopilotClient, session_id::AbstractString, config::SessionConfig)
    _ensure_started!(client)
    _require_available_tools(client, config)

    flat = _session_flat_params(client, config; resume=true)
    flat["sessionId"] = String(session_id)
    registration_id = _register_github_token_provider!(client, config, String(session_id))
    if registration_id !== nothing
        flat["gitHubTokenProviderRegistrationId"] = registration_id
    end
    # Translate the flat options to the SDK protocol 4 grouped payload
    params = _session_wire_params(client, flat, true)

    # A failed resume can carry a PolicyHelperFailure: see `policy_helper_failure(err)`.
    result = try
        send_request(client.rpc, "session.resume", params; timeout=30)
    catch
        registration_id !== nothing && delete!(client.github_token_providers, registration_id)
        rethrow()
    end

    sid = if result isa AbstractDict
        string(get(result, "sessionId", session_id))
    else
        String(session_id)
    end

    session = CopilotSession(sid, client.rpc, config)
    client.sessions[sid] = session
    _commit_github_token_provider!(client, session, registration_id)
    _register_session_handlers!(client, session)
    return session
end

resume_session(client::CopilotClient, session_id::AbstractString; kwargs...) =
    resume_session(client, session_id, SessionConfig(; kwargs...))

"""
    delete_session(client, session_id)

Delete a session permanently.
"""
function delete_session(client::CopilotClient, session_id::AbstractString)
    _ensure_started!(client)
    send_request(client.rpc, "session.delete",
        Dict{String,Any}("sessionId" => session_id); timeout=10)
    delete!(client.sessions, session_id)
    return nothing
end

"""
    get_foreground_session_id(client) -> String

Return the session ID of the current foreground session.
"""
function get_foreground_session_id(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.getForeground", Dict{String,Any}(); timeout=10)
    if result isa Dict
        return get(result, "sessionId", "")
    end
    return ""
end

"""
    set_foreground_session_id(client, session_id)

Set the foreground session to the given session ID.
"""
function set_foreground_session_id(client::CopilotClient, session_id::AbstractString)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.setForeground",
        Dict{String,Any}("sessionId" => session_id); timeout=10)
    if result isa Dict && get(result, "success", false) != true
        error_msg = get(result, "error", "Unknown")
        throw(ErrorException("Failed to set foreground session: $error_msg"))
    end
end

"""
    get_last_session_id(client) -> Union{String, Nothing}

Return the last-used session ID, or `nothing` if none exists.
"""
function get_last_session_id(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.getLastId", Dict{String,Any}(); timeout=10)
    if result isa Dict
        sid = get(result, "sessionId", nothing)
        return sid isa AbstractString && !isempty(sid) ? sid : nothing
    end
    return nothing
end

"""
    get_session_metadata(client, session_id) -> Dict

Retrieve metadata for a specific session.
"""
function get_session_metadata(client::CopilotClient, session_id::AbstractString)
    _ensure_started!(client)
    result = send_request(client.rpc, "session.getMetadata",
        Dict{String,Any}("sessionId" => session_id); timeout=15)
    return result isa Dict ? result : Dict{String,Any}()
end

"""
    ping(client; message="ping") -> Dict

Ping the server and return the response (message + timestamp).
"""
function ping(client::CopilotClient; message::AbstractString="ping")
    _ensure_started!(client)
    result = send_request(client.rpc, "ping",
        Dict{String,Any}("message" => message); timeout=10)
    return result isa Dict ? result : Dict{String,Any}()
end

"""
    get_auth_status(client) -> Dict

Query the server for authentication status.
"""
function get_auth_status(client::CopilotClient)
    _ensure_started!(client)
    result = send_request(client.rpc, "auth.getStatus", Dict{String,Any}(); timeout=10)
    return result isa Dict ? result : Dict{String,Any}()
end

"""
    set_session_fs_provider(client, config::SessionFsConfig) -> Dict

Register a session filesystem provider with the CLI server.
"""
function set_session_fs_provider(client::CopilotClient, config::SessionFsConfig)
    _ensure_started!(client)
    params = Dict{String, Any}(
        "initialCwd"       => config.initial_cwd,
        "sessionStatePath" => config.session_state_path,
        "conventions"      => config.conventions,
    )
    result = send_request(client.rpc, "sessionFs.setProvider", params; timeout=10)
    return result isa Dict ? result : Dict{String, Any}()
end

# -- Internal helpers -------------------------------------------------------------

function _ensure_started!(client::CopilotClient)
    if client.state != CONNECTED
        if client.options.auto_start
            start!(client)
        else
            throw(ErrorException("Client is not connected. Call start!(client) first."))
        end
    end
end

function _spawn_stdio!(client::CopilotClient)
    cli_path = _resolve_cli_path(client.options)

    args = copy(client.options.cli_args)
    append!(args, ["--headless", "--no-auto-update", "--stdio"])
    log_str = get(LOG_LEVEL_STRINGS, client.options.log_level, "error")
    push!(args, "--log-level=$log_str")
    if client.options.remote
        push!(args, "--remote")
    end

    env_pairs = something(client.options.env, copy(ENV))

    # A .js entry point (for example npm-loader.js) is run with node; any other path
    # (such as node_modules/.bin/copilot) is executed directly.
    base = endswith(lowercase(cli_path), ".js") ? `node $cli_path $args` : `$cli_path $args`
    cmd = Cmd(base; dir=something(client.options.cwd, pwd()), env=env_pairs)
    proc = open(cmd, "r+")

    client.process = proc
    client.rpc = JsonRpcClient(proc, proc.in, proc.out)
    start!(client.rpc)

    _register_client_handlers!(client)
    _verify_protocol!(client)
    _register_network_provider!(client)
end

function _connect_tcp!(client::CopilotClient)
    url = client.options.cli_url
    host, port = _parse_url(url)

    sock = Sockets.connect(host, port)
    # Wrap socket in a pseudo-process-like pipeline
    proc = _NullProcess()
    client.rpc = JsonRpcClient(proc, sock, sock)
    start!(client.rpc)

    _register_client_handlers!(client)
    _verify_protocol!(client)
    _register_network_provider!(client)
end

"""Thin wrapper so the JsonRpcClient can treat a TCP socket like a process."""
mutable struct _NullProcess
    running::Bool
    _NullProcess() = new(true)
end

function _parse_url(url::AbstractString)
    url = strip(url)
    url = replace(url, r"^https?://" => "")
    parts = split(url, ":")
    if length(parts) == 1
        return ("127.0.0.1", parse(Int, parts[1]))
    end
    host = String(parts[1])
    port = parse(Int, parts[2])
    return (host, port)
end

function _resolve_cli_path(opts::CopilotClientOptions)
    if opts.cli_path !== nothing
        return opts.cli_path
    end
    env_path = get(ENV, "COPILOT_CLI_PATH", "")
    if !isempty(env_path) && isfile(env_path)
        return env_path
    end
    # Try to find the CLI on PATH
    for name in ("copilot", "copilot-cli")
        found = Sys.which(name)
        found !== nothing && return found
    end
    throw(ErrorException(
        "Could not find Copilot CLI. Set cli_path, COPILOT_CLI_PATH, or " *
        "ensure copilot is on your PATH."
    ))
end

# `connect` clientInfo parameter (empty fields dropped), or `nothing` when no field is set.
function _client_info_wire(info::Union{ClientInfo, Nothing})
    info === nothing && return nothing
    wire = Dict{String, Any}()
    for (field, key) in ((:editor_name, "editorName"), (:editor_version, "editorVersion"),
                         (:extension_name, "extensionName"), (:extension_version, "extensionVersion"))
        value = getproperty(info, field)
        (value === nothing || isempty(value)) || (wire[key] = value)
    end
    return isempty(wire) ? nothing : wire
end

# Declare the integrating application with `connect` (when client_info is set; CLIs without
# `connect` are tolerated), then verify with `ping` that the server speaks exactly this SDK's
# protocol version.
function _verify_protocol!(client::CopilotClient)
    info = _client_info_wire(client.options.client_info)
    if info !== nothing
        try
            send_request(client.rpc, "connect", Dict{String, Any}("clientInfo" => info); timeout=30)
        catch e
            method_not_found = e isa JsonRpcError &&
                (e.code == -32601 || occursin("Unhandled method connect", e.message))
            method_not_found || rethrow()
        end
    end

    result = send_request(client.rpc, "ping", Dict{String, Any}("message" => "ping"); timeout=30)
    server_version = result isa AbstractDict ? get(result, "protocolVersion", nothing) : nothing
    if server_version === nothing
        throw(ErrorException(
            "SDK protocol version mismatch: SDK supports version $SDK_PROTOCOL_VERSION, but " *
            "server does not report a protocol version. Please update your server to ensure " *
            "compatibility."))
    end
    if server_version != SDK_PROTOCOL_VERSION
        throw(ErrorException(
            "SDK protocol version mismatch: SDK supports version $SDK_PROTOCOL_VERSION, but " *
            "server reports version $server_version. Please update your SDK or server to " *
            "ensure compatibility."))
    end
    return nothing
end

# Register the configured network callbacks with `network.setProvider`.
function _register_network_provider!(client::CopilotClient)
    net = client.options.network
    net === nothing && return nothing
    params = Dict{String, Any}()
    net.resolve_proxy !== nothing && (params["resolveProxy"] = Dict{String, Any}())
    net.authenticate_negotiate !== nothing && (params["authenticateNegotiate"] = Dict{String, Any}())
    net.authenticate_basic !== nothing && (params["authenticateBasic"] = Dict{String, Any}())
    isempty(params) && return nothing
    net.scope !== nothing && (params["scope"] = net.scope)
    send_request(client.rpc, "network.setProvider", params; timeout=30)
    return nothing
end

"""
    clear_managed_settings_cache(client)

Clear the runtime's cached managed (enterprise) settings so they are re-read.
"""
function clear_managed_settings_cache(client::CopilotClient)
    _ensure_started!(client)
    return send_request(client.rpc, "managedSettings.clearCache", Dict{String, Any}(); timeout=30)
end

"""
    policy_helper_failure(x) -> Union{PolicyHelperFailure, Nothing}

Read the structured Policy Helper failure from a `JsonRpcError` raised by a failed
`create_session` / `resume_session` (its `data.policyHelperFailure`), from a managed
settings snapshot, or from a `policyHelperFailure` Dict. Returns `nothing` when absent.
"""
policy_helper_failure(e::JsonRpcError) = policy_helper_failure(e.data)

function policy_helper_failure(d::AbstractDict)
    if !haskey(d, "category") && haskey(d, "policyHelperFailure")
        return policy_helper_failure(d["policyHelperFailure"])
    end
    haskey(d, "category") || return nothing
    return PolicyHelperFailure(
        category = _str(get(d, "category", "")),
        failure_mode = _str(get(d, "failureMode", "")),
        message = _str(get(d, "message", "")),
        previous_policy_retained = get(d, "previousPolicyRetained", false) === true,
        reason = _str(get(d, "reason", "")),
        severity = _opt_str(get(d, "severity", nothing)),
        authority = _opt_str(get(d, "authority", nothing)),
        code = _opt_str(get(d, "code", nothing)),
        details = get(d, "details", nothing),
        diagnostic = _opt_str(get(d, "diagnostic", nothing)),
        help_link = get(d, "helpLink", nothing),
        remediation = _opt_str(get(d, "remediation", nothing)),
    )
end

policy_helper_failure(::Any) = nothing

"""
    shell_output_event_data(event) -> Union{ShellOutputEventData, Nothing}

Read the data of a `tool.shell_output` event; `nothing` for other events.
"""
function shell_output_event_data(event::SessionEvent)
    event.type == EVENT_TOOL_SHELL_OUTPUT || return nothing
    stream = get(event.data, "stream", nothing)
    sequence = get(event.data, "sequence", 0)
    return ShellOutputEventData(
        tool_call_id = _str(get(event.data, "toolCallId", "")),
        text = _str(get(event.data, "text", "")),
        sequence = sequence isa Integer ? Int(sequence) : 0,
        stream = stream === nothing ? nothing : string(stream),
    )
end

_str(x) = x === nothing ? "" : string(x)
_opt_str(x) = x === nothing ? nothing : string(x)

# Build a SessionEvent from a `session.event` notification's event object. Unknown event
# types are kept as-is.
function _session_event_from_wire(e)
    e isa AbstractDict || return SessionEvent("")
    data = get(e, "data", nothing)
    ephemeral = get(e, "ephemeral", nothing)
    return SessionEvent(
        _str(get(e, "type", "")),
        data isa AbstractDict ? Dict{String, Any}(data) : Dict{String, Any}(),
        _str(get(e, "id", "")),
        _str(get(e, "timestamp", "")),
        _opt_str(get(e, "parentId", nothing)),
        _opt_str(get(e, "agentId", nothing)),
        ephemeral isa Bool ? ephemeral : nothing,
    )
end

# A field of a callback result given as a Dict (string or symbol keys), NamedTuple, or struct.
function _field(x, key::Symbol)
    if x isa AbstractDict
        return haskey(x, String(key)) ? x[String(key)] : get(x, key, nothing)
    elseif x !== nothing && hasproperty(x, key)
        return getproperty(x, key)
    end
    return nothing
end

# -- Server -> client requests --------------------------------------------------------

# Pending shell authentication requests: request ID => cancelled.
const _SHELL_AUTH_LOCK = ReentrantLock()
const _SHELL_AUTH_PENDING = Dict{String, Bool}()

function _register_client_handlers!(client::CopilotClient)
    rpc = client.rpc
    rpc === nothing && return nothing

    # Session events
    on_notification!(rpc, "session.event") do params
        sid = get(params, "sessionId", "")
        s = get(client.sessions, sid, nothing)
        s === nothing && return
        event = _session_event_from_wire(get(params, "event", params))
        _emit(s, event)
        event.type == EVENT_EXTERNAL_TOOL_REQUESTED && _handle_external_tool(s, event)
        event.type == "permission.requested" && _handle_permission_requested(s, event)
    end

    on_request!(rpc, "gitHubToken.getToken") do params
        _handle_github_token_request(client, params)
    end
    on_request!(rpc, "hooks.invoke") do params
        _handle_hooks_invoke(client, params)
    end
    on_request!(rpc, "shellAuthentication.request") do params
        _handle_shell_authentication(client, params)
    end
    on_request!(rpc, "shellAuthentication.cancel") do params
        request_id = get(params, "requestId", nothing)
        if request_id isa AbstractString
            lock(_SHELL_AUTH_LOCK) do
                haskey(_SHELL_AUTH_PENDING, request_id) && (_SHELL_AUTH_PENDING[request_id] = true)
            end
        end
        Dict{String, Any}()
    end
    on_request!(rpc, "memoryStore.store") do params
        _handle_memory_store(client, params)
    end
    on_request!(rpc, "network.resolveProxy") do params
        _handle_network_request(client, :resolve_proxy, params)
    end
    on_request!(rpc, "network.authenticateNegotiate") do params
        _handle_network_request(client, :authenticate_negotiate, params)
    end
    on_request!(rpc, "network.authenticateBasic") do params
        _handle_network_request(client, :authenticate_basic, params)
    end
    on_request!(rpc, "exitPlanMode.request") do params
        s = get(client.sessions, get(params, "sessionId", ""), nothing)
        if s === nothing || s.config.on_exit_plan_mode === nothing
            return Dict{String, Any}("approved" => true)
        end
        decision = s.config.on_exit_plan_mode(params)
        decision isa Bool ? Dict{String, Any}("approved" => decision) : decision
    end
    return nothing
end

function _session_for(client::CopilotClient, params)
    sid = get(params, "sessionId", nothing)
    return sid isa AbstractString ? get(client.sessions, sid, nothing) : nothing
end

function _handle_github_token_request(client::CopilotClient, params)
    registration_id = get(params, "registrationId", nothing)
    entry = registration_id isa AbstractString ?
        get(client.github_token_providers, registration_id, nothing) : nothing
    if entry === nothing
        throw(ErrorException("No GitHub token provider registered for registration ID \"$(registration_id)\""))
    end
    provider, bound_session_id = entry
    sid = get(params, "sessionId", nothing)
    args = GitHubTokenProviderArgs(
        host = _str(get(params, "host", "")),
        session_id = sid isa AbstractString ? sid : bound_session_id,
        reason = _str(get(params, "reason", "")),
    )
    result = provider(args)
    result === nothing && throw(ErrorException("GitHub token provider returned no result"))
    result isa AbstractString && return Dict{String, Any}("kind" => "token", "accessToken" => result)
    kind = something(_field(result, :kind), "token")
    kind == "cancelled" && return Dict{String, Any}("kind" => "cancelled")
    wire = Dict{String, Any}("kind" => kind)
    token = something(_field(result, :access_token), _field(result, :accessToken), Some(nothing))
    token !== nothing && (wire["accessToken"] = token)
    token_type = something(_field(result, :token_type), _field(result, :tokenType), Some(nothing))
    token_type !== nothing && (wire["tokenType"] = token_type)
    expires_in = something(_field(result, :expires_in), _field(result, :expiresIn), Some(nothing))
    expires_in !== nothing && (wire["expiresIn"] = expires_in)
    return wire
end

function _handle_hooks_invoke(client::CopilotClient, params)
    s = _session_for(client, params)
    field = get(_HOOK_HANDLER_FIELDS, _str(get(params, "hookType", "")), nothing)
    output = nothing
    if s !== nothing && field !== nothing
        handler = getproperty(s.config, field)
        if handler !== nothing
            output = try
                handler(get(params, "input", nothing))
            catch e
                @error "Hook handler error" hook_type=field exception=(e, catch_backtrace())
                nothing
            end
        end
    end
    return Dict{String, Any}("output" => output)
end

# Answer a shell authentication request. Handler failures, cancellations, and anything but
# an accepted string answer cancel the prompt; the answer is never logged.
function _handle_shell_authentication(client::CopilotClient, params)
    cancel = Dict{String, Any}("action" => "cancel")
    request_id = get(params, "requestId", nothing)
    input_kind = get(params, "inputKind", nothing)
    valid = request_id isa AbstractString && get(params, "program", nothing) isa AbstractString &&
        get(params, "prompt", nothing) isa AbstractString && input_kind in ("secret", "text", "confirm")
    valid || return cancel
    s = _session_for(client, params)
    (s === nothing || s.config.on_shell_authentication_request === nothing) && return cancel

    lock(_SHELL_AUTH_LOCK) do
        _SHELL_AUTH_PENDING[request_id] = false
    end
    request = ShellAuthenticationRequest(
        request_id = request_id,
        program = params["program"],
        prompt = params["prompt"],
        input_kind = input_kind,
        command = _str(get(params, "command", "")),
        cwd = _str(get(params, "cwd", "")),
    )
    answer = try
        s.config.on_shell_authentication_request(request)
    catch e
        # The error message could contain the answer, so only its type is logged.
        @error "Shell authentication handler failed" session_id=s.session_id error_type=typeof(e)
        nothing
    end
    cancelled = lock(_SHELL_AUTH_LOCK) do
        pop!(_SHELL_AUTH_PENDING, request_id, false)
    end
    (cancelled || answer === nothing) && return cancel
    value = _field(answer, :value)
    if _field(answer, :action) == "accept" && value isa AbstractString
        return Dict{String, Any}("action" => "accept", "value" => String(value))
    end
    return cancel
end

function _handle_memory_store(client::CopilotClient, params)
    s = _session_for(client, params)
    s === nothing && throw(ErrorException("Memory store owner is unavailable"))
    s.config.store_memory === nothing && throw(ErrorException("No memory store is registered"))
    memory = StoredMemory(
        session_id = s.session_id,
        subject = _str(get(params, "subject", "")),
        fact = _str(get(params, "fact", "")),
        citations = _str(get(params, "citations", "")),
        reason = _str(get(params, "reason", "")),
    )
    try
        s.config.store_memory(memory)
    catch e
        @warn "Memory store callback failed" exception=(e, catch_backtrace())
        throw(ErrorException("Memory store callback failed"))
    end
    return nothing
end

# Answer a network callback request. Tokens and passwords are never logged.
function _handle_network_request(client::CopilotClient, kind::Symbol, params)
    net = client.options.network
    callback = net === nothing ? nothing : getproperty(net, kind)
    callback === nothing && throw(ErrorException("No network $(kind) callback is registered"))
    if kind === :resolve_proxy
        proxy = callback(Dict{String, Any}("url" => get(params, "url", nothing)))
        return proxy === nothing ? Dict{String, Any}() : Dict{String, Any}("proxy" => string(proxy))
    elseif kind === :authenticate_negotiate
        token = callback(Dict{String, Any}("spn" => get(params, "spn", nothing)))
        return token === nothing ? Dict{String, Any}() : Dict{String, Any}("token" => string(token))
    end
    credentials = callback(Dict{String, Any}(
        "proxy" => get(params, "proxy", nothing),
        "realm" => get(params, "realm", nothing),
        "previousRejected" => get(params, "previousRejected", false) === true,
    ))
    # Send only the credential fields, never other members of the callback result.
    result = Dict{String, Any}()
    if credentials !== nothing
        username = _field(credentials, :username)
        password = _field(credentials, :password)
        username !== nothing && (result["username"] = string(username))
        password !== nothing && (result["password"] = string(password))
    end
    return result
end

function _register_session_handlers!(client::CopilotClient, session::CopilotSession)
    rpc = client.rpc
    rpc === nothing && return

    # Legacy tool call requests
    on_request!(rpc, "session/toolCall") do params
        sid = get(params, "sessionId", "")
        s = get(client.sessions, sid, nothing)
        if s === nothing
            return Dict{String, Any}("resultType" => "failure", "textResultForLlm" => "Unknown session")
        end
        _handle_tool_call(s, params)
    end

    # Permission requests
    on_request!(rpc, "session/permissionRequest") do params
        sid = get(params, "sessionId", "")
        s = get(client.sessions, sid, nothing)
        if s === nothing
            return Dict{String, Any}("decision" => "deny")
        end
        _handle_permission(s, params)
    end
end

# Convenience permission handlers

"""Approve all permission requests. Pass as `on_permission_request=approve_all`."""
approve_all(::PermissionRequest) = "allow"

"""Deny all permission requests. Pass as `on_permission_request=deny_all`."""
deny_all(::PermissionRequest) = "deny"

--- SDK protocol 4 grouping of session.create / session.resume payloads.
--
-- Converts the flat payload built by CopilotClient into the grouped protocol 4
-- shape (for example `streaming` becomes `events.streaming`). Internal module.

local cjson = require("cjson")

local M = {}

-- Option groups of the SDK protocol 4 session payloads.
local GROUP_NAMES = {
    inference = true, tools = true, permissions = true, interaction = true,
    events = true, agents = true, skills = true, mcp = true, github = true,
    filesystem = true, execution = true, instructions = true, hooks = true,
    git = true, state = true, plugins = true, canvases = true,
    extensions = true, experiments = true, policy = true,
}

-- Flat wire keys only valid on session.create (dropped on resume).
local CREATE_ONLY = {
    name = true, cloud = true, trustWorkingDirectory = true, refreshCustomInstructions = true,
}

-- Flat wire keys only valid on session.resume (dropped on create).
local RESUME_ONLY = {
    disableResume = true, suppressResumeEvent = true, continuePendingWork = true,
    allowTranscriptRecovery = true, requestedEnvironmentVariables = true, openCanvases = true,
}

-- Flat (SDK protocol 3) wire keys and the SDK protocol 4 { group, member } they move to.
-- Root keys (sessionId, model, name, clientName, workingDirectory, configDirectory,
-- enableConfigDiscovery, systemMessage, diagnostics, lifecycleDiagnostics, cloud,
-- traceparent, tracestate and the resume controls) and unknown keys stay top level.
local GROUPS = {
    reasoningEffort                    = { "inference", "reasoningEffort" },
    reasoningSummary                   = { "inference", "reasoningSummary" },
    contextTier                        = { "inference", "contextTier" },
    allowMultipleToolCalls             = { "inference", "allowMultipleToolCalls" },
    enableCitations                    = { "inference", "enableCitations" },
    modelCapabilities                  = { "inference", "capabilities" },
    verbosity                          = { "inference", "verbosity" },
    providers                          = { "inference", "providers" },
    models                             = { "inference", "models" },
    defaultProvider                    = { "inference", "defaultProvider" },
    tools                              = { "tools", "definitions" },
    availableTools                     = { "tools", "availableTools" },
    excludedTools                      = { "tools", "excludedTools" },
    toolSearch                         = { "tools", "search" },
    largeOutput                        = { "tools", "largeOutput" },
    imageGeneration                    = { "tools", "imageGeneration" },
    manageScheduleEnabled              = { "tools", "manageScheduleEnabled" },
    webSearch                          = { "tools", "webSearch" },
    shell                              = { "tools", "shell" },
    maxInlineBinaryBytes               = { "tools", "maxInlineBinaryBytes" },
    requestPermission                  = { "permissions", "request" },
    sandboxConfig                      = { "permissions", "sandbox" },
    sandbox                            = { "permissions", "sandbox" },
    permissionMode                     = { "permissions", "mode" },
    sandboxConfigSource                = { "permissions", "sandboxSource" },
    commands                           = { "interaction", "commands" },
    askUserVariant                     = { "interaction", "askUserVariant" },
    requestUserInput                   = { "interaction", "requestUserInput" },
    requestElicitation                 = { "interaction", "requestElicitation" },
    requestShellAuthentication         = { "interaction", "requestShellAuthentication" },
    requestExitPlanMode                = { "interaction", "requestExitPlanMode" },
    requestAutoModeSwitch              = { "interaction", "requestAutoModeSwitch" },
    requestAutoModeRecovery            = { "interaction", "requestAutoModeRecovery" },
    streaming                          = { "events", "streaming" },
    includeSubAgentStreamingEvents     = { "events", "includeSubAgentStreamingEvents" },
    customAgents                       = { "agents", "definitions" },
    defaultAgent                       = { "agents", "default" },
    agent                              = { "agents", "selected" },
    includedBuiltinAgents              = { "agents", "includedBuiltinAgents" },
    excludedBuiltinAgents              = { "agents", "excludedBuiltinAgents" },
    customAgentsLocalOnly              = { "agents", "localDiscoveryOnly" },
    agentDirectories                   = { "agents", "directories" },
    skillDirectories                   = { "skills", "directories" },
    enableSkills                       = { "skills", "enabled" },
    includedBuiltinSkills              = { "skills", "includedBuiltinSkills" },
    disabledSkills                     = { "skills", "disabledSkills" },
    hasSkillProvider                   = { "skills", "hasProvider" },
    ignoredSkillsLocations             = { "skills", "ignoredLocations" },
    mcpServers                         = { "mcp", "servers" },
    disabledMcpServers                 = { "mcp", "disabledServers" },
    requestMcpApps                     = { "mcp", "enableApps" },
    enableMcpApps                      = { "mcp", "enableApps" },
    mcpOAuthTokenStorage               = { "mcp", "oauthTokenStorage" },
    authClientIdMetadataUrl            = { "mcp", "authClientIdMetadataUrl" },
    envValueMode                       = { "mcp", "envValueMode" },
    managedMcpServers                  = { "mcp", "managedServers" },
    allowAllMcpServerInstructions      = { "mcp", "allowAllServerInstructions" },
    gitHubToken                        = { "github", "token" },
    gitHubTokenProviderRegistrationId  = { "github", "tokenProviderRegistrationId" },
    githubMcpToolConfig                = { "github", "mcp" },
    capi                               = { "github", "capi" },
    enableSessionTelemetry             = { "github", "telemetryEnabled" },
    enableManagedSettings              = { "github", "fetchManagedSettings" },
    interactiveUser                    = { "github", "interactiveUser" },
    enableGitHubTelemetryForwarding    = { "github", "telemetryForwarding" },
    authInfo                           = { "github", "authInfo" },
    copilotUrl                         = { "github", "copilotUrl" },
    additionalDirectories              = { "filesystem", "additionalDirectories" },
    enableFileChangeTracking           = { "filesystem", "trackFileChanges" },
    trustWorkingDirectory              = { "filesystem", "trustWorkingDirectory" },
    sessionLimits                      = { "execution", "limits" },
    autopilot                          = { "execution", "autopilot" },
    autopilotContinuation              = { "execution", "autopilotContinuation" },
    instructionDirectories             = { "instructions", "directories" },
    organizationCustomInstructions     = { "instructions", "organizationText" },
    enableOnDemandInstructionDiscovery = { "instructions", "onDemandDiscovery" },
    skipCustomInstructions             = { "instructions", "skipCustomInstructions" },
    refreshCustomInstructions          = { "instructions", "refreshCustomInstructions" },
    hooks                              = { "hooks", "hasHandlers" },
    hookDirectories                    = { "hooks", "directories" },
    enableFileHooks                    = { "hooks", "enableRepositoryHooks" },
    coauthorEnabled                    = { "git", "coauthorEnabled" },
    enableHostGitOperations            = { "git", "enableHostGitOperations" },
    infiniteSessions                   = { "state", "infiniteSessions" },
    memory                             = { "state", "memory" },
    hasMemoryStore                     = { "state", "hasMemoryStore" },
    enableSessionStore                 = { "state", "enableCrossSessionStore" },
    skipEmbeddingRetrieval             = { "state", "skipEmbeddingRetrieval" },
    embeddingCacheStorage              = { "state", "embeddingCacheStorage" },
    pluginDirectories                  = { "plugins", "directories" },
    installedPlugins                   = { "plugins", "installed" },
    canvases                           = { "canvases", "definitions" },
    canvasProvider                     = { "canvases", "provider" },
    requestCanvasRenderer              = { "canvases", "requestRenderer" },
    openCanvases                       = { "canvases", "open" },
    requestExtensions                  = { "extensions", "enabled" },
    extensionSdkPath                   = { "extensions", "sdkPath" },
    extensionInfo                      = { "extensions", "identity" },
    enableExperimentalMode             = { "experiments", "enableExperimentalMode" },
    isExperimentalMode                 = { "experiments", "enableExperimentalMode" },
    featureFlags                       = { "experiments", "flags" },
    expAssignments                     = { "experiments", "assignments" },
    featureFlagConfiguration           = { "experiments", "configuration" },
    allowedModels                      = { "policy", "allowedModels" },
    managedSettings                    = { "policy", "managedSettings" },
    excludedManagedSettings            = { "policy", "excludedManagedSettings" },
    enforceManagedModelDefaults        = { "policy", "enforceManagedModelDefaults" },
}

-- Legacy single-provider fields that describe the model rather than the connection.
local LEGACY_PROVIDER_MODEL_FIELDS = {
    modelId = true, wireModel = true, maxPromptTokens = true,
    maxContextWindowTokens = true, maxOutputTokens = true, capabilities = true,
}

-- Session defaults applied under the caller's values when the client mode is "empty".
-- The memory default is built per call because it is a table.
local EMPTY_MODE_DEFAULTS = {
    { "experiments",  "enableExperimentalMode",  false },
    { "github",       "telemetryEnabled",        false },
    { "mcp",          "oauthTokenStorage",       "in-memory" },
    { "state",        "skipEmbeddingRetrieval",  true },
    { "state",        "embeddingCacheStorage",   "in-memory" },
    { "state",        "enableCrossSessionStore", false },
    { "state",        "memory",                  "memory" },
    { "instructions", "onDemandDiscovery",       false },
    { "hooks",        "enableRepositoryHooks",   false },
    { "git",          "enableHostGitOperations", false },
}

local function is_null(value)
    return value == nil or value == cjson.null
end

-- True for a non-empty table with string keys (a JSON object).
local function is_map(value)
    if type(value) ~= "table" then return false end
    if cjson.array_mt and getmetatable(value) == cjson.array_mt then return false end
    if value[1] ~= nil then return false end
    return next(value) ~= nil
end

-- Translate a legacy single provider into inference.providers / inference.models /
-- inference.defaultProvider and github.capi.enabled = false.
local function group_legacy_provider(provider, model, groups)
    local inference = groups.inference or {}
    local github = groups.github or {}
    local capi = github.capi
    local explicit_capi = type(capi) == "table" and not is_null(capi.enabled)
    if inference.providers ~= nil or inference.defaultProvider ~= nil or explicit_capi then
        return
    end

    local connection = { name = "default" }
    local model_fields = {}
    local has_model_fields = false
    for key, value in pairs(provider) do
        if not is_null(value) and key ~= "name" then
            if LEGACY_PROVIDER_MODEL_FIELDS[key] then
                model_fields[key] = value
                has_model_fields = true
            else
                connection[key] = value
            end
        end
    end

    inference.providers = { connection }
    inference.defaultProvider = "default"
    local merged_capi = {}
    if type(capi) == "table" then
        for k, v in pairs(capi) do merged_capi[k] = v end
    end
    merged_capi.enabled = false
    github.capi = merged_capi
    if has_model_fields and not is_null(model) then
        local entry = { provider = "default", id = model }
        for k, v in pairs(model_fields) do entry[k] = v end
        inference.models = { entry }
    end
    groups.inference = inference
    groups.github = github
end

--- Convert a flat payload into the grouped SDK protocol 4 shape.
-- Root keys stay top level, known flat keys move into their option group, and
-- unknown keys pass through unchanged. nil / cjson.null values and empty groups
-- are dropped, except an explicit defaultProvider = cjson.null on resume, which
-- clears the default provider. Group tables already present are merged and their
-- members win. A legacy single `provider` becomes a "default" named provider with
-- Copilot API models disabled, unless explicit providers, defaultProvider or
-- capi.enabled are present. A non-object value under a group name that has no
-- flat mapping (such as a legacy `instructions` string, which the runtime never
-- honored) cannot be represented in protocol 4 and is dropped.
-- @param flat table    Flat payload
-- @param resume boolean True for session.resume, false for session.create
-- @return table Grouped payload
function M.group(flat, resume)
    local payload, groups, explicit = {}, {}, {}
    local legacy_provider = nil

    for key, value in pairs(flat) do
        local skip = false
        if is_null(value) and not (resume and key == "defaultProvider" and value == cjson.null) then
            skip = true
        elseif resume and CREATE_ONLY[key] then
            skip = true
        elseif not resume and RESUME_ONLY[key] then
            skip = true
        end

        if not skip then
            local path = GROUPS[key]
            if GROUP_NAMES[key] and is_map(value) then
                explicit[key] = value
            elseif GROUP_NAMES[key] and not path then
                -- Not representable in protocol 4 (see above); dropped.
                skip = true
            elseif key == "configDir" then
                payload.configDirectory = value
            elseif key == "disableResume" then
                payload.suppressResumeEvent = value
            elseif key == "provider" then
                legacy_provider = value
            elseif path then
                groups[path[1]] = groups[path[1]] or {}
                groups[path[1]][path[2]] = value
            else
                payload[key] = value
            end
        end
    end

    for name, members in pairs(explicit) do
        for member, value in pairs(members) do
            if not is_null(value) then
                groups[name] = groups[name] or {}
                groups[name][member] = value
            end
        end
    end

    if type(legacy_provider) == "table" then
        group_legacy_provider(legacy_provider, payload.model, groups)
    end

    if groups.mcp and groups.mcp.servers ~= nil and is_null(groups.mcp.envValueMode) then
        groups.mcp.envValueMode = "direct"
    end

    for name, members in pairs(groups) do
        if next(members) ~= nil then
            payload[name] = members
        end
    end
    return payload
end

--- Apply the "empty" client mode session defaults to a grouped payload in place.
-- Values the caller already set win.
-- @param payload table Grouped payload from group()
-- @return table The same payload
function M.apply_empty_mode_defaults(payload)
    for _, default in ipairs(EMPTY_MODE_DEFAULTS) do
        local group, member, value = default[1], default[2], default[3]
        if type(payload[group]) ~= "table" then
            payload[group] = {}
        end
        local target = payload[group]
        if is_null(target[member]) then
            if member == "memory" then
                target[member] = { enabled = false }
            else
                target[member] = value
            end
        end
    end
    return payload
end

return M

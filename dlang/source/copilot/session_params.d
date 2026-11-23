/// SDK protocol 4 session payload translation for the Copilot D SDK.
///
/// `SessionConfig.toJson` / `ResumeSessionConfig.toJson` build a flat
/// payload. Protocol 4 servers expect most options inside named groups
/// (`inference`, `tools`, `permissions`, ...) and reject the flat keys.
/// `groupSessionParams` converts the flat payload right before
/// `session.create` / `session.resume` is sent, so callers keep using the
/// same options.
module copilot.session_params;

import std.json;
import std.algorithm.searching : canFind;
import std.string : indexOf;

/// Keys that stay at the top level of the payload.
private immutable string[] sessionRootKeys = [
    "sessionId", "model", "name", "clientName", "workingDirectory",
    "configDirectory", "enableConfigDiscovery", "systemMessage", "diagnostics",
    "lifecycleDiagnostics", "cloud", "traceparent", "tracestate",
    "suppressResumeEvent", "continuePendingWork", "allowTranscriptRecovery",
    "requestedEnvironmentVariables",
];

/// Protocol 4 option groups.
private immutable string[] sessionGroupNames = [
    "inference", "tools", "permissions", "interaction", "events", "agents",
    "skills", "mcp", "github", "filesystem", "execution", "instructions",
    "hooks", "git", "state", "plugins", "canvases", "extensions",
    "experiments", "policy",
];

private immutable string[] createOnlyGroupedKeys = ["trustWorkingDirectory", "refreshCustomInstructions"];
private immutable string[] resumeOnlyGroupedKeys = ["openCanvases"];

/// Legacy `provider` members that describe the model, not the connection.
private immutable string[] legacyProviderModelFields = [
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens",
    "maxOutputTokens", "capabilities",
];

/// Flat wire key -> "group.member".
private immutable string[string] sessionGroupedKeys;

shared static this()
{
    sessionGroupedKeys = [
        "reasoningEffort": "inference.reasoningEffort",
        "reasoningSummary": "inference.reasoningSummary",
        "contextTier": "inference.contextTier",
        "allowMultipleToolCalls": "inference.allowMultipleToolCalls",
        "enableCitations": "inference.enableCitations",
        "modelCapabilities": "inference.capabilities",
        "verbosity": "inference.verbosity",
        "providers": "inference.providers",
        "models": "inference.models",
        "defaultProvider": "inference.defaultProvider",
        "tools": "tools.definitions",
        "availableTools": "tools.availableTools",
        "excludedTools": "tools.excludedTools",
        "toolSearch": "tools.search",
        "largeOutput": "tools.largeOutput",
        "imageGeneration": "tools.imageGeneration",
        "manageScheduleEnabled": "tools.manageScheduleEnabled",
        "webSearch": "tools.webSearch",
        "shell": "tools.shell",
        "maxInlineBinaryBytes": "tools.maxInlineBinaryBytes",
        "requestPermission": "permissions.request",
        "sandboxConfig": "permissions.sandbox",
        "sandbox": "permissions.sandbox",
        "permissionMode": "permissions.mode",
        "sandboxConfigSource": "permissions.sandboxSource",
        "commands": "interaction.commands",
        "askUserVariant": "interaction.askUserVariant",
        "requestUserInput": "interaction.requestUserInput",
        "requestElicitation": "interaction.requestElicitation",
        "requestShellAuthentication": "interaction.requestShellAuthentication",
        "requestExitPlanMode": "interaction.requestExitPlanMode",
        "requestAutoModeSwitch": "interaction.requestAutoModeSwitch",
        "requestAutoModeRecovery": "interaction.requestAutoModeRecovery",
        "streaming": "events.streaming",
        "includeSubAgentStreamingEvents": "events.includeSubAgentStreamingEvents",
        "customAgents": "agents.definitions",
        "defaultAgent": "agents.default",
        "agent": "agents.selected",
        "includedBuiltinAgents": "agents.includedBuiltinAgents",
        "excludedBuiltinAgents": "agents.excludedBuiltinAgents",
        "customAgentsLocalOnly": "agents.localDiscoveryOnly",
        "agentDirectories": "agents.directories",
        "skillDirectories": "skills.directories",
        "enableSkills": "skills.enabled",
        "includedBuiltinSkills": "skills.includedBuiltinSkills",
        "disabledSkills": "skills.disabledSkills",
        "hasSkillProvider": "skills.hasProvider",
        "ignoredSkillsLocations": "skills.ignoredLocations",
        "mcpServers": "mcp.servers",
        "disabledMcpServers": "mcp.disabledServers",
        "requestMcpApps": "mcp.enableApps",
        "enableMcpApps": "mcp.enableApps",
        "mcpOAuthTokenStorage": "mcp.oauthTokenStorage",
        "authClientIdMetadataUrl": "mcp.authClientIdMetadataUrl",
        "envValueMode": "mcp.envValueMode",
        "managedMcpServers": "mcp.managedServers",
        "allowAllMcpServerInstructions": "mcp.allowAllServerInstructions",
        "gitHubToken": "github.token",
        "gitHubTokenProviderRegistrationId": "github.tokenProviderRegistrationId",
        "githubMcpToolConfig": "github.mcp",
        "capi": "github.capi",
        "enableSessionTelemetry": "github.telemetryEnabled",
        "enableManagedSettings": "github.fetchManagedSettings",
        "interactiveUser": "github.interactiveUser",
        "enableGitHubTelemetryForwarding": "github.telemetryForwarding",
        "authInfo": "github.authInfo",
        "copilotUrl": "github.copilotUrl",
        "additionalDirectories": "filesystem.additionalDirectories",
        "enableFileChangeTracking": "filesystem.trackFileChanges",
        "trustWorkingDirectory": "filesystem.trustWorkingDirectory",
        "sessionLimits": "execution.limits",
        "autopilot": "execution.autopilot",
        "autopilotContinuation": "execution.autopilotContinuation",
        "instructionDirectories": "instructions.directories",
        "organizationCustomInstructions": "instructions.organizationText",
        "enableOnDemandInstructionDiscovery": "instructions.onDemandDiscovery",
        "skipCustomInstructions": "instructions.skipCustomInstructions",
        "refreshCustomInstructions": "instructions.refreshCustomInstructions",
        "hooks": "hooks.hasHandlers",
        "hookDirectories": "hooks.directories",
        "enableFileHooks": "hooks.enableRepositoryHooks",
        "coauthorEnabled": "git.coauthorEnabled",
        "enableHostGitOperations": "git.enableHostGitOperations",
        "infiniteSessions": "state.infiniteSessions",
        "memory": "state.memory",
        "hasMemoryStore": "state.hasMemoryStore",
        "enableSessionStore": "state.enableCrossSessionStore",
        "skipEmbeddingRetrieval": "state.skipEmbeddingRetrieval",
        "embeddingCacheStorage": "state.embeddingCacheStorage",
        "pluginDirectories": "plugins.directories",
        "installedPlugins": "plugins.installed",
        "canvases": "canvases.definitions",
        "canvasProvider": "canvases.provider",
        "requestCanvasRenderer": "canvases.requestRenderer",
        "openCanvases": "canvases.open",
        "requestExtensions": "extensions.enabled",
        "extensionSdkPath": "extensions.sdkPath",
        "extensionInfo": "extensions.identity",
        "enableExperimentalMode": "experiments.enableExperimentalMode",
        "isExperimentalMode": "experiments.enableExperimentalMode",
        "featureFlags": "experiments.flags",
        "expAssignments": "experiments.assignments",
        "featureFlagConfiguration": "experiments.configuration",
        "allowedModels": "policy.allowedModels",
        "managedSettings": "policy.managedSettings",
        "excludedManagedSettings": "policy.excludedManagedSettings",
        "enforceManagedModelDefaults": "policy.enforceManagedModelDefaults",
    ];
}

private void putMember(ref JSONValue groups, string group, string member, JSONValue value) @trusted
{
    if ((group in groups) is null)
        groups[group] = JSONValue(string[string].init);
    groups[group][member] = value;
}

private bool hasMember(ref JSONValue groups, string group, string member) @trusted
{
    auto g = group in groups;
    return g !is null && (*g).type == JSONType.object && (member in *g) !is null;
}

/// Convert a flat `session.create` / `session.resume` payload into the
/// grouped SDK protocol 4 payload.
///
/// Root keys stay at the top level, known flat keys move to their group
/// member, and unknown keys stay at the root unchanged. Null values are
/// skipped (except an explicit null `defaultProvider` on resume, which clears
/// it) and empty groups are not emitted. Members of an already-grouped
/// object win over the equivalent flat keys. Create-only members are dropped
/// on resume and resume-only members on create.
JSONValue groupSessionParams(JSONValue flat, bool isResume) @trusted
{
    auto result = JSONValue(string[string].init);
    if (flat.type != JSONType.object)
        return result;

    auto groups = JSONValue(string[string].init);
    JSONValue[string] explicitGroups;
    JSONValue legacyProvider;
    bool hasLegacyProvider = false;

    foreach (string key, value; flat.object)
    {
        if (value.type == JSONType.null_ && !(isResume && key == "defaultProvider"))
            continue;
        if (sessionGroupNames.canFind(key) && value.type == JSONType.object)
        {
            explicitGroups[key] = value;
            continue;
        }
        if (key == "configDir")
        {
            if (("configDirectory" in flat.object) is null)
                result["configDirectory"] = value;
        }
        else if (key == "disableResume")
        {
            if (("suppressResumeEvent" in flat.object) is null)
                result["suppressResumeEvent"] = value;
        }
        else if (key == "provider")
        {
            if (value.type == JSONType.object)
            {
                legacyProvider = value;
                hasLegacyProvider = true;
            }
        }
        else if (sessionRootKeys.canFind(key))
        {
            result[key] = value;
        }
        else if (isResume && createOnlyGroupedKeys.canFind(key))
        {
            continue;
        }
        else if (!isResume && resumeOnlyGroupedKeys.canFind(key))
        {
            continue;
        }
        else if (auto path = key in sessionGroupedKeys)
        {
            auto dot = (*path).indexOf('.');
            putMember(groups, (*path)[0 .. dot], (*path)[dot + 1 .. $], value);
        }
        else
        {
            result[key] = value;
        }
    }

    foreach (group, members; explicitGroups)
    {
        foreach (string member, value; members.object)
        {
            if (value.type == JSONType.null_
                && !(isResume && group == "inference" && member == "defaultProvider"))
                continue;
            putMember(groups, group, member, value);
        }
    }

    if (hasLegacyProvider
        && !hasMember(groups, "inference", "providers")
        && !hasMember(groups, "inference", "defaultProvider")
        && !capiEnablementChosen(groups))
    {
        auto pModel = "model" in flat.object;
        translateLegacyProvider(groups, legacyProvider,
            (pModel !is null && (*pModel).type == JSONType.string) ? (*pModel).str : null);
    }

    if (hasMember(groups, "mcp", "servers") && !hasMember(groups, "mcp", "envValueMode"))
        putMember(groups, "mcp", "envValueMode", JSONValue("direct"));

    foreach (string group, members; groups.object)
    {
        if (members.type == JSONType.object && members.object.length > 0)
            result[group] = members;
    }
    return result;
}

/// Whether the caller chose `github.capi.enabled` explicitly. The auto tier
/// and WebSocket options also live under `capi` but do not replace a legacy
/// provider.
private bool capiEnablementChosen(ref JSONValue groups) @trusted
{
    if (!hasMember(groups, "github", "capi"))
        return false;
    auto capi = groups["github"]["capi"];
    return capi.type == JSONType.object && ("enabled" in capi.object) !is null;
}

/// v1 BYOK: a single provider becomes the named "default" connection with
/// Copilot models disabled, plus a model definition for model-specific fields
/// when the session model is known.
private void translateLegacyProvider(ref JSONValue groups, JSONValue provider, string model) @trusted
{
    auto connection = JSONValue(string[string].init);
    connection["name"] = "default";
    auto modelFields = JSONValue(string[string].init);
    foreach (string key, value; provider.object)
    {
        if (value.type == JSONType.null_ || key == "name")
            continue;
        if (legacyProviderModelFields.canFind(key))
            modelFields[key] = value;
        else
            connection[key] = value;
    }
    putMember(groups, "inference", "providers", JSONValue([connection]));
    putMember(groups, "inference", "defaultProvider", JSONValue("default"));
    // Keep other capi members (auto tier, WebSocket responses).
    auto capi = JSONValue(string[string].init);
    if (hasMember(groups, "github", "capi") && groups["github"]["capi"].type == JSONType.object)
    {
        foreach (string key, value; groups["github"]["capi"].object)
            capi[key] = value;
    }
    capi["enabled"] = false;
    putMember(groups, "github", "capi", capi);

    if (modelFields.object.length > 0 && model.length > 0
        && !hasMember(groups, "inference", "models"))
    {
        auto entry = JSONValue(string[string].init);
        entry["provider"] = "default";
        entry["id"] = model;
        foreach (string key, value; modelFields.object)
            entry[key] = value;
        putMember(groups, "inference", "models", JSONValue([entry]));
    }
}

private void setDefaultMember(ref JSONValue payload, string group, string member, JSONValue value) @trusted
{
    auto g = group in payload;
    if (g is null || (*g).type != JSONType.object)
        payload[group] = JSONValue(string[string].init);
    if ((member in payload[group]) is null)
        payload[group][member] = value;
}

/// Fill the conservative "empty" client mode defaults into a grouped payload
/// without overriding values the caller supplied.
void applyEmptyModeDefaults(ref JSONValue payload) @trusted
{
    auto memory = JSONValue(string[string].init);
    memory["enabled"] = false;
    setDefaultMember(payload, "experiments", "enableExperimentalMode", JSONValue(false));
    setDefaultMember(payload, "github", "telemetryEnabled", JSONValue(false));
    setDefaultMember(payload, "mcp", "oauthTokenStorage", JSONValue("in-memory"));
    setDefaultMember(payload, "state", "skipEmbeddingRetrieval", JSONValue(true));
    setDefaultMember(payload, "state", "embeddingCacheStorage", JSONValue("in-memory"));
    setDefaultMember(payload, "state", "enableCrossSessionStore", JSONValue(false));
    setDefaultMember(payload, "state", "memory", memory);
    setDefaultMember(payload, "instructions", "onDemandDiscovery", JSONValue(false));
    setDefaultMember(payload, "hooks", "enableRepositoryHooks", JSONValue(false));
    setDefaultMember(payload, "git", "enableHostGitOperations", JSONValue(false));
}

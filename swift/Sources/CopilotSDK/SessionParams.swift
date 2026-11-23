// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import Foundation

// MARK: - SDK protocol 4 session payload grouping
//
// The client keeps building the flat (protocol 3) `session.create` /
// `session.resume` payload from the session configuration. `groupSessionParams`
// converts it into the grouped protocol 4 shape right before the request is sent.

/// Option groups of the protocol 4 session payload.
let sessionParamGroupNames: Set<String> = [
    "inference", "tools", "permissions", "interaction", "events", "agents", "skills", "mcp",
    "github", "filesystem", "execution", "instructions", "hooks", "git", "state", "plugins",
    "canvases", "extensions", "experiments", "policy",
]

/// Flat keys only valid on `session.create` (dropped on resume).
private let createOnlySessionParams: Set<String> = [
    "name", "cloud", "trustWorkingDirectory", "refreshCustomInstructions",
]

/// Flat keys only valid on `session.resume` (dropped on create).
private let resumeOnlySessionParams: Set<String> = [
    "disableResume", "suppressResumeEvent", "continuePendingWork", "allowTranscriptRecovery",
    "requestedEnvironmentVariables", "openCanvases",
]

/// Flat (protocol 3) key -> protocol 4 `(group, member)`.
///
/// Root keys (`sessionId`, `model`, `name`, `clientName`, `workingDirectory`,
/// `configDirectory`, `enableConfigDiscovery`, `systemMessage`, `diagnostics`,
/// `lifecycleDiagnostics`, `cloud`, `traceparent`, `tracestate` and the resume controls)
/// and unknown keys stay top level.
private let sessionParamGroups: [String: (group: String, member: String)] = [
    "reasoningEffort": ("inference", "reasoningEffort"),
    "reasoningSummary": ("inference", "reasoningSummary"),
    "contextTier": ("inference", "contextTier"),
    "allowMultipleToolCalls": ("inference", "allowMultipleToolCalls"),
    "enableCitations": ("inference", "enableCitations"),
    "modelCapabilities": ("inference", "capabilities"),
    "verbosity": ("inference", "verbosity"),
    "providers": ("inference", "providers"),
    "models": ("inference", "models"),
    "defaultProvider": ("inference", "defaultProvider"),
    "tools": ("tools", "definitions"),
    "availableTools": ("tools", "availableTools"),
    "excludedTools": ("tools", "excludedTools"),
    "toolSearch": ("tools", "search"),
    "largeOutput": ("tools", "largeOutput"),
    "imageGeneration": ("tools", "imageGeneration"),
    "manageScheduleEnabled": ("tools", "manageScheduleEnabled"),
    "webSearch": ("tools", "webSearch"),
    "shell": ("tools", "shell"),
    "maxInlineBinaryBytes": ("tools", "maxInlineBinaryBytes"),
    "requestPermission": ("permissions", "request"),
    "sandboxConfig": ("permissions", "sandbox"),
    "sandbox": ("permissions", "sandbox"),
    "permissionMode": ("permissions", "mode"),
    "sandboxConfigSource": ("permissions", "sandboxSource"),
    "commands": ("interaction", "commands"),
    "askUserVariant": ("interaction", "askUserVariant"),
    "requestUserInput": ("interaction", "requestUserInput"),
    "requestElicitation": ("interaction", "requestElicitation"),
    "requestShellAuthentication": ("interaction", "requestShellAuthentication"),
    "requestExitPlanMode": ("interaction", "requestExitPlanMode"),
    "requestAutoModeSwitch": ("interaction", "requestAutoModeSwitch"),
    "requestAutoModeRecovery": ("interaction", "requestAutoModeRecovery"),
    "streaming": ("events", "streaming"),
    "includeSubAgentStreamingEvents": ("events", "includeSubAgentStreamingEvents"),
    "customAgents": ("agents", "definitions"),
    "defaultAgent": ("agents", "default"),
    "agent": ("agents", "selected"),
    "includedBuiltinAgents": ("agents", "includedBuiltinAgents"),
    "excludedBuiltinAgents": ("agents", "excludedBuiltinAgents"),
    "customAgentsLocalOnly": ("agents", "localDiscoveryOnly"),
    "agentDirectories": ("agents", "directories"),
    "skillDirectories": ("skills", "directories"),
    "enableSkills": ("skills", "enabled"),
    "includedBuiltinSkills": ("skills", "includedBuiltinSkills"),
    "disabledSkills": ("skills", "disabledSkills"),
    "hasSkillProvider": ("skills", "hasProvider"),
    "ignoredSkillsLocations": ("skills", "ignoredLocations"),
    "mcpServers": ("mcp", "servers"),
    "disabledMcpServers": ("mcp", "disabledServers"),
    "requestMcpApps": ("mcp", "enableApps"),
    "enableMcpApps": ("mcp", "enableApps"),
    "mcpOAuthTokenStorage": ("mcp", "oauthTokenStorage"),
    "authClientIdMetadataUrl": ("mcp", "authClientIdMetadataUrl"),
    "envValueMode": ("mcp", "envValueMode"),
    "managedMcpServers": ("mcp", "managedServers"),
    "allowAllMcpServerInstructions": ("mcp", "allowAllServerInstructions"),
    "gitHubToken": ("github", "token"),
    "gitHubTokenProviderRegistrationId": ("github", "tokenProviderRegistrationId"),
    "githubMcpToolConfig": ("github", "mcp"),
    "capi": ("github", "capi"),
    "enableSessionTelemetry": ("github", "telemetryEnabled"),
    "enableManagedSettings": ("github", "fetchManagedSettings"),
    "interactiveUser": ("github", "interactiveUser"),
    "enableGitHubTelemetryForwarding": ("github", "telemetryForwarding"),
    "authInfo": ("github", "authInfo"),
    "copilotUrl": ("github", "copilotUrl"),
    "additionalDirectories": ("filesystem", "additionalDirectories"),
    "enableFileChangeTracking": ("filesystem", "trackFileChanges"),
    "trustWorkingDirectory": ("filesystem", "trustWorkingDirectory"),
    "sessionLimits": ("execution", "limits"),
    "autopilot": ("execution", "autopilot"),
    "autopilotContinuation": ("execution", "autopilotContinuation"),
    "instructionDirectories": ("instructions", "directories"),
    "organizationCustomInstructions": ("instructions", "organizationText"),
    "enableOnDemandInstructionDiscovery": ("instructions", "onDemandDiscovery"),
    "skipCustomInstructions": ("instructions", "skipCustomInstructions"),
    "refreshCustomInstructions": ("instructions", "refreshCustomInstructions"),
    "hooks": ("hooks", "hasHandlers"),
    "hookDirectories": ("hooks", "directories"),
    "enableFileHooks": ("hooks", "enableRepositoryHooks"),
    "coauthorEnabled": ("git", "coauthorEnabled"),
    "enableHostGitOperations": ("git", "enableHostGitOperations"),
    "infiniteSessions": ("state", "infiniteSessions"),
    "memory": ("state", "memory"),
    "hasMemoryStore": ("state", "hasMemoryStore"),
    "enableSessionStore": ("state", "enableCrossSessionStore"),
    "skipEmbeddingRetrieval": ("state", "skipEmbeddingRetrieval"),
    "embeddingCacheStorage": ("state", "embeddingCacheStorage"),
    "pluginDirectories": ("plugins", "directories"),
    "installedPlugins": ("plugins", "installed"),
    "canvases": ("canvases", "definitions"),
    "canvasProvider": ("canvases", "provider"),
    "requestCanvasRenderer": ("canvases", "requestRenderer"),
    "openCanvases": ("canvases", "open"),
    "requestExtensions": ("extensions", "enabled"),
    "extensionSdkPath": ("extensions", "sdkPath"),
    "extensionInfo": ("extensions", "identity"),
    "enableExperimentalMode": ("experiments", "enableExperimentalMode"),
    "isExperimentalMode": ("experiments", "enableExperimentalMode"),
    "featureFlags": ("experiments", "flags"),
    "expAssignments": ("experiments", "assignments"),
    "featureFlagConfiguration": ("experiments", "configuration"),
    "allowedModels": ("policy", "allowedModels"),
    "managedSettings": ("policy", "managedSettings"),
    "excludedManagedSettings": ("policy", "excludedManagedSettings"),
    "enforceManagedModelDefaults": ("policy", "enforceManagedModelDefaults"),
]

/// Legacy single-provider fields that describe the model, not the connection.
private let legacyProviderModelFields: Set<String> = [
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens",
    "capabilities",
]

/// Converts the flat `session.create` / `session.resume` payload into the grouped SDK
/// protocol 4 shape.
///
/// Known flat keys move into their option group (for example `streaming` becomes
/// `events.streaming` and a boolean `hooks` becomes `hooks.hasHandlers`), root keys and
/// unknown keys stay top level, `NSNull` values and empty groups are dropped (except an
/// explicit `defaultProvider: NSNull()` on resume, which clears the default provider), and
/// grouped dictionaries already present in `flat` are merged with their members winning.
/// `configDir` becomes `configDirectory` and `disableResume` becomes `suppressResumeEvent`.
/// A legacy single `provider` becomes a `"default"` named provider with Copilot API models
/// disabled, unless explicit `providers`, `defaultProvider` or `capi.enabled` are present.
func groupSessionParams(_ flat: [String: Any], isResume: Bool) -> [String: Any] {
    var payload: [String: Any] = [:]
    var groups: [String: [String: Any]] = [:]
    var explicitGroups: [String: [String: Any]] = [:]
    var legacyProvider: [String: Any]?

    for (key, value) in flat {
        if value is NSNull && !(isResume && key == "defaultProvider") { continue }
        if isResume && createOnlySessionParams.contains(key) { continue }
        if !isResume && resumeOnlySessionParams.contains(key) { continue }

        if sessionParamGroupNames.contains(key), let members = value as? [String: Any] {
            explicitGroups[key] = members
        } else if key == "configDir" {
            payload["configDirectory"] = value
        } else if key == "disableResume" {
            payload["suppressResumeEvent"] = value
        } else if key == "provider", let provider = value as? [String: Any] {
            legacyProvider = provider
        } else if let path = sessionParamGroups[key] {
            groups[path.group, default: [:]][path.member] = value
        } else {
            payload[key] = value
        }
    }

    for (name, members) in explicitGroups {
        for (member, value) in members {
            let clearsDefaultProvider = isResume && name == "inference" && member == "defaultProvider"
            if value is NSNull && !clearsDefaultProvider { continue }
            groups[name, default: [:]][member] = value
        }
    }

    // Grouped members that only exist on the other operation are rejected by the runtime.
    if isResume {
        groups["filesystem"]?.removeValue(forKey: "trustWorkingDirectory")
        groups["instructions"]?.removeValue(forKey: "refreshCustomInstructions")
    } else {
        groups["canvases"]?.removeValue(forKey: "open")
    }

    if let provider = legacyProvider {
        groupLegacyProvider(provider, model: payload["model"] as? String, groups: &groups)
    }

    if groups["mcp"]?["servers"] != nil && groups["mcp"]?["envValueMode"] == nil {
        groups["mcp"]?["envValueMode"] = "direct"
    }

    for (name, members) in groups where !members.isEmpty {
        payload[name] = members
    }
    return payload
}

/// Translates a legacy single `provider` into `inference.providers`,
/// `inference.defaultProvider`, `github.capi.enabled = false` and, when the session model
/// is set and the provider carries model fields, `inference.models`.
private func groupLegacyProvider(
    _ provider: [String: Any],
    model: String?,
    groups: inout [String: [String: Any]]
) {
    var inference = groups["inference"] ?? [:]
    var github = groups["github"] ?? [:]
    let capi = github["capi"] as? [String: Any]
    let explicitCapi = capi.map { $0["enabled"] != nil && !($0["enabled"] is NSNull) } ?? false
    if inference["providers"] != nil || inference["defaultProvider"] != nil || explicitCapi {
        return
    }

    var connection: [String: Any] = ["name": "default"]
    var modelFields: [String: Any] = [:]
    for (key, value) in provider where key != "name" && !(value is NSNull) {
        if legacyProviderModelFields.contains(key) {
            modelFields[key] = value
        } else {
            connection[key] = value
        }
    }

    inference["providers"] = [connection]
    inference["defaultProvider"] = "default"
    var wireCapi = capi ?? [:]
    wireCapi["enabled"] = false
    github["capi"] = wireCapi
    if !modelFields.isEmpty, let model = model {
        var entry: [String: Any] = ["provider": "default", "id": model]
        entry.merge(modelFields) { current, _ in current }
        inference["models"] = [entry]
    }
    groups["inference"] = inference
    groups["github"] = github
}

/// Applies the `"empty"` client mode session defaults to a grouped payload; values the
/// caller already set win.
func applyEmptyModeSessionDefaults(_ payload: inout [String: Any]) {
    let defaults: [(group: String, member: String, value: Any)] = [
        ("experiments", "enableExperimentalMode", false),
        ("github", "telemetryEnabled", false),
        ("mcp", "oauthTokenStorage", "in-memory"),
        ("state", "skipEmbeddingRetrieval", true),
        ("state", "embeddingCacheStorage", "in-memory"),
        ("state", "enableCrossSessionStore", false),
        ("state", "memory", ["enabled": false]),
        ("instructions", "onDemandDiscovery", false),
        ("hooks", "enableRepositoryHooks", false),
        ("git", "enableHostGitOperations", false),
    ]
    for entry in defaults {
        var target = payload[entry.group] as? [String: Any] ?? [:]
        if target[entry.member] == nil || target[entry.member] is NSNull {
            target[entry.member] = entry.value
        }
        payload[entry.group] = target
    }
}

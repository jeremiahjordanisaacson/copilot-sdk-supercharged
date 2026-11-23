/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#import "CPSessionParams.h"

#include <string.h>

/// Lifecycle methods that accept a key. Keys sent to the wrong method are dropped.
typedef NS_ENUM(NSInteger, CPParamScope) {
    CPParamScopeBoth = 0,
    CPParamScopeCreateOnly,
    CPParamScopeResumeOnly,
};

/// Maps a flat key to its protocol 4 location (`group == NULL` keeps it at the root).
typedef struct {
    const char *flatKey;
    const char *group;
    const char *member;
    CPParamScope scope;
} CPSessionParamRule;

static const CPSessionParamRule kCPSessionParamRules[] = {
    // Root fields (configDir and disableResume are renamed)
    {"sessionId", NULL, "sessionId", CPParamScopeBoth},
    {"model", NULL, "model", CPParamScopeBoth},
    {"name", NULL, "name", CPParamScopeCreateOnly},
    {"clientName", NULL, "clientName", CPParamScopeBoth},
    {"workingDirectory", NULL, "workingDirectory", CPParamScopeBoth},
    {"configDirectory", NULL, "configDirectory", CPParamScopeBoth},
    {"configDir", NULL, "configDirectory", CPParamScopeBoth},
    {"enableConfigDiscovery", NULL, "enableConfigDiscovery", CPParamScopeBoth},
    {"systemMessage", NULL, "systemMessage", CPParamScopeBoth},
    {"diagnostics", NULL, "diagnostics", CPParamScopeBoth},
    {"lifecycleDiagnostics", NULL, "lifecycleDiagnostics", CPParamScopeBoth},
    {"cloud", NULL, "cloud", CPParamScopeCreateOnly},
    {"traceparent", NULL, "traceparent", CPParamScopeBoth},
    {"tracestate", NULL, "tracestate", CPParamScopeBoth},
    {"suppressResumeEvent", NULL, "suppressResumeEvent", CPParamScopeResumeOnly},
    {"disableResume", NULL, "suppressResumeEvent", CPParamScopeResumeOnly},
    {"continuePendingWork", NULL, "continuePendingWork", CPParamScopeResumeOnly},
    {"allowTranscriptRecovery", NULL, "allowTranscriptRecovery", CPParamScopeResumeOnly},
    {"requestedEnvironmentVariables", NULL, "requestedEnvironmentVariables", CPParamScopeResumeOnly},
    // inference
    {"reasoningEffort", "inference", "reasoningEffort", CPParamScopeBoth},
    {"reasoningSummary", "inference", "reasoningSummary", CPParamScopeBoth},
    {"contextTier", "inference", "contextTier", CPParamScopeBoth},
    {"allowMultipleToolCalls", "inference", "allowMultipleToolCalls", CPParamScopeBoth},
    {"enableCitations", "inference", "enableCitations", CPParamScopeBoth},
    {"modelCapabilities", "inference", "capabilities", CPParamScopeBoth},
    {"verbosity", "inference", "verbosity", CPParamScopeBoth},
    {"providers", "inference", "providers", CPParamScopeBoth},
    {"models", "inference", "models", CPParamScopeBoth},
    {"defaultProvider", "inference", "defaultProvider", CPParamScopeBoth},
    // tools
    {"tools", "tools", "definitions", CPParamScopeBoth},
    {"availableTools", "tools", "availableTools", CPParamScopeBoth},
    {"excludedTools", "tools", "excludedTools", CPParamScopeBoth},
    {"toolSearch", "tools", "search", CPParamScopeBoth},
    {"largeOutput", "tools", "largeOutput", CPParamScopeBoth},
    {"imageGeneration", "tools", "imageGeneration", CPParamScopeBoth},
    {"manageScheduleEnabled", "tools", "manageScheduleEnabled", CPParamScopeBoth},
    {"webSearch", "tools", "webSearch", CPParamScopeBoth},
    {"shell", "tools", "shell", CPParamScopeBoth},
    {"maxInlineBinaryBytes", "tools", "maxInlineBinaryBytes", CPParamScopeBoth},
    // permissions
    {"requestPermission", "permissions", "request", CPParamScopeBoth},
    {"sandboxConfig", "permissions", "sandbox", CPParamScopeBoth},
    {"sandbox", "permissions", "sandbox", CPParamScopeBoth},
    {"permissionMode", "permissions", "mode", CPParamScopeBoth},
    {"sandboxConfigSource", "permissions", "sandboxSource", CPParamScopeBoth},
    // interaction
    {"commands", "interaction", "commands", CPParamScopeBoth},
    {"askUserVariant", "interaction", "askUserVariant", CPParamScopeBoth},
    {"requestUserInput", "interaction", "requestUserInput", CPParamScopeBoth},
    {"requestElicitation", "interaction", "requestElicitation", CPParamScopeBoth},
    {"requestShellAuthentication", "interaction", "requestShellAuthentication", CPParamScopeBoth},
    {"requestExitPlanMode", "interaction", "requestExitPlanMode", CPParamScopeBoth},
    {"requestAutoModeSwitch", "interaction", "requestAutoModeSwitch", CPParamScopeBoth},
    {"requestAutoModeRecovery", "interaction", "requestAutoModeRecovery", CPParamScopeBoth},
    // events
    {"streaming", "events", "streaming", CPParamScopeBoth},
    {"includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents", CPParamScopeBoth},
    // agents
    {"customAgents", "agents", "definitions", CPParamScopeBoth},
    {"defaultAgent", "agents", "default", CPParamScopeBoth},
    {"agent", "agents", "selected", CPParamScopeBoth},
    {"includedBuiltinAgents", "agents", "includedBuiltinAgents", CPParamScopeBoth},
    {"excludedBuiltinAgents", "agents", "excludedBuiltinAgents", CPParamScopeBoth},
    {"customAgentsLocalOnly", "agents", "localDiscoveryOnly", CPParamScopeBoth},
    {"agentDirectories", "agents", "directories", CPParamScopeBoth},
    // skills
    {"skillDirectories", "skills", "directories", CPParamScopeBoth},
    {"enableSkills", "skills", "enabled", CPParamScopeBoth},
    {"includedBuiltinSkills", "skills", "includedBuiltinSkills", CPParamScopeBoth},
    {"disabledSkills", "skills", "disabledSkills", CPParamScopeBoth},
    {"hasSkillProvider", "skills", "hasProvider", CPParamScopeBoth},
    {"ignoredSkillsLocations", "skills", "ignoredLocations", CPParamScopeBoth},
    // mcp
    {"mcpServers", "mcp", "servers", CPParamScopeBoth},
    {"disabledMcpServers", "mcp", "disabledServers", CPParamScopeBoth},
    {"requestMcpApps", "mcp", "enableApps", CPParamScopeBoth},
    {"enableMcpApps", "mcp", "enableApps", CPParamScopeBoth},
    {"mcpOAuthTokenStorage", "mcp", "oauthTokenStorage", CPParamScopeBoth},
    {"authClientIdMetadataUrl", "mcp", "authClientIdMetadataUrl", CPParamScopeBoth},
    {"envValueMode", "mcp", "envValueMode", CPParamScopeBoth},
    {"managedMcpServers", "mcp", "managedServers", CPParamScopeBoth},
    {"allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions", CPParamScopeBoth},
    // github
    {"gitHubToken", "github", "token", CPParamScopeBoth},
    {"gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId", CPParamScopeBoth},
    {"githubMcpToolConfig", "github", "mcp", CPParamScopeBoth},
    {"capi", "github", "capi", CPParamScopeBoth},
    {"enableSessionTelemetry", "github", "telemetryEnabled", CPParamScopeBoth},
    {"enableManagedSettings", "github", "fetchManagedSettings", CPParamScopeBoth},
    {"interactiveUser", "github", "interactiveUser", CPParamScopeBoth},
    {"enableGitHubTelemetryForwarding", "github", "telemetryForwarding", CPParamScopeBoth},
    {"authInfo", "github", "authInfo", CPParamScopeBoth},
    {"copilotUrl", "github", "copilotUrl", CPParamScopeBoth},
    // filesystem
    {"additionalDirectories", "filesystem", "additionalDirectories", CPParamScopeBoth},
    {"enableFileChangeTracking", "filesystem", "trackFileChanges", CPParamScopeBoth},
    {"trustWorkingDirectory", "filesystem", "trustWorkingDirectory", CPParamScopeCreateOnly},
    // execution
    {"sessionLimits", "execution", "limits", CPParamScopeBoth},
    {"autopilot", "execution", "autopilot", CPParamScopeBoth},
    {"autopilotContinuation", "execution", "autopilotContinuation", CPParamScopeBoth},
    // instructions
    {"instructionDirectories", "instructions", "directories", CPParamScopeBoth},
    {"organizationCustomInstructions", "instructions", "organizationText", CPParamScopeBoth},
    {"enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery", CPParamScopeBoth},
    {"skipCustomInstructions", "instructions", "skipCustomInstructions", CPParamScopeBoth},
    {"refreshCustomInstructions", "instructions", "refreshCustomInstructions", CPParamScopeCreateOnly},
    // hooks (boolean "hooks" becomes hooks.hasHandlers)
    {"hooks", "hooks", "hasHandlers", CPParamScopeBoth},
    {"hookDirectories", "hooks", "directories", CPParamScopeBoth},
    {"enableFileHooks", "hooks", "enableRepositoryHooks", CPParamScopeBoth},
    // git
    {"coauthorEnabled", "git", "coauthorEnabled", CPParamScopeBoth},
    {"enableHostGitOperations", "git", "enableHostGitOperations", CPParamScopeBoth},
    // state
    {"infiniteSessions", "state", "infiniteSessions", CPParamScopeBoth},
    {"memory", "state", "memory", CPParamScopeBoth},
    {"hasMemoryStore", "state", "hasMemoryStore", CPParamScopeBoth},
    {"enableSessionStore", "state", "enableCrossSessionStore", CPParamScopeBoth},
    {"skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval", CPParamScopeBoth},
    {"embeddingCacheStorage", "state", "embeddingCacheStorage", CPParamScopeBoth},
    // plugins
    {"pluginDirectories", "plugins", "directories", CPParamScopeBoth},
    {"installedPlugins", "plugins", "installed", CPParamScopeBoth},
    // canvases
    {"canvases", "canvases", "definitions", CPParamScopeBoth},
    {"canvasProvider", "canvases", "provider", CPParamScopeBoth},
    {"requestCanvasRenderer", "canvases", "requestRenderer", CPParamScopeBoth},
    {"openCanvases", "canvases", "open", CPParamScopeResumeOnly},
    // extensions
    {"requestExtensions", "extensions", "enabled", CPParamScopeBoth},
    {"extensionSdkPath", "extensions", "sdkPath", CPParamScopeBoth},
    {"extensionInfo", "extensions", "identity", CPParamScopeBoth},
    // experiments
    {"enableExperimentalMode", "experiments", "enableExperimentalMode", CPParamScopeBoth},
    {"isExperimentalMode", "experiments", "enableExperimentalMode", CPParamScopeBoth},
    {"featureFlags", "experiments", "flags", CPParamScopeBoth},
    {"expAssignments", "experiments", "assignments", CPParamScopeBoth},
    {"featureFlagConfiguration", "experiments", "configuration", CPParamScopeBoth},
    // policy
    {"allowedModels", "policy", "allowedModels", CPParamScopeBoth},
    {"managedSettings", "policy", "managedSettings", CPParamScopeBoth},
    {"excludedManagedSettings", "policy", "excludedManagedSettings", CPParamScopeBoth},
    {"enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults", CPParamScopeBoth},
};

/// Protocol 4 option groups. A key naming a group that holds an object is already grouped.
static const char *const kCPSessionParamGroups[] = {
    "inference", "tools", "permissions", "interaction", "events",
    "agents", "skills", "mcp", "github", "filesystem",
    "execution", "instructions", "hooks", "git", "state",
    "plugins", "canvases", "extensions", "experiments", "policy",
};

static const CPSessionParamRule *CPFindSessionParamRule(NSString *key) {
    const char *name = key.UTF8String;
    if (!name) return NULL;
    size_t count = sizeof(kCPSessionParamRules) / sizeof(kCPSessionParamRules[0]);
    for (size_t i = 0; i < count; i++) {
        if (strcmp(kCPSessionParamRules[i].flatKey, name) == 0) return &kCPSessionParamRules[i];
    }
    return NULL;
}

static BOOL CPIsSessionParamGroup(NSString *key) {
    const char *name = key.UTF8String;
    if (!name) return NO;
    size_t count = sizeof(kCPSessionParamGroups) / sizeof(kCPSessionParamGroups[0]);
    for (size_t i = 0; i < count; i++) {
        if (strcmp(kCPSessionParamGroups[i], name) == 0) return YES;
    }
    return NO;
}

/// Returns `parent[name]` as a mutable copy stored back into `parent` (a new dictionary when
/// absent or not a dictionary), so caller-supplied dictionaries are never mutated.
static NSMutableDictionary *CPChildGroup(NSMutableDictionary *parent, NSString *name) {
    id existing = parent[name];
    NSMutableDictionary *group = [existing isKindOfClass:[NSDictionary class]]
        ? [(NSDictionary *)existing mutableCopy] : [NSMutableDictionary dictionary];
    parent[name] = group;
    return group;
}

static BOOL CPHasGroupMember(NSDictionary *params, NSString *group, NSString *member) {
    id value = params[group];
    return [value isKindOfClass:[NSDictionary class]] && ((NSDictionary *)value)[member] != nil;
}

/// Stores `value` at `params[group][member]` unless that member is already set.
static void CPPutGroupMemberIfAbsent(NSMutableDictionary *params, NSString *group, NSString *member, id value) {
    if (CPHasGroupMember(params, group, member)) return;
    NSMutableDictionary *target = CPChildGroup(params, group);
    target[member] = value;
}

/// Translates the legacy single `provider` (no longer accepted by protocol 4). Explicit
/// `providers`, `defaultProvider`, or `capi.enabled` win: the legacy provider is then dropped.
static void CPTranslateLegacyProvider(NSMutableDictionary *params, NSDictionary *provider) {
    if (CPHasGroupMember(params, @"inference", @"providers") ||
        CPHasGroupMember(params, @"inference", @"defaultProvider")) {
        return;
    }
    id github = params[@"github"];
    if ([github isKindOfClass:[NSDictionary class]]) {
        id capi = ((NSDictionary *)github)[@"capi"];
        if ([capi isKindOfClass:[NSDictionary class]] && ((NSDictionary *)capi)[@"enabled"] != nil) return;
    }

    NSArray<NSString *> *modelFields = @[@"modelId", @"wireModel", @"maxPromptTokens",
                                         @"maxContextWindowTokens", @"maxOutputTokens", @"capabilities"];
    NSMutableDictionary *connection = [NSMutableDictionary dictionaryWithObject:@"default" forKey:@"name"];
    NSMutableDictionary *modelDefinition = [NSMutableDictionary dictionary];
    for (id key in provider) {
        if (![key isKindOfClass:[NSString class]] || [key isEqualToString:@"name"]) continue;
        id value = provider[key];
        if (value == [NSNull null]) continue;
        if ([modelFields containsObject:key]) {
            modelDefinition[key] = value;
        } else {
            connection[key] = value;
        }
    }

    NSMutableDictionary *inference = CPChildGroup(params, @"inference");
    inference[@"providers"] = @[connection];
    inference[@"defaultProvider"] = @"default";
    NSMutableDictionary *githubGroup = CPChildGroup(params, @"github");
    NSMutableDictionary *capiGroup = CPChildGroup(githubGroup, @"capi");
    capiGroup[@"enabled"] = @NO;

    id model = params[@"model"];
    if (modelDefinition.count == 0 || ![model isKindOfClass:[NSString class]]) return;
    modelDefinition[@"provider"] = @"default";
    modelDefinition[@"id"] = model;
    NSMutableArray *models = [NSMutableArray array];
    id existingModels = inference[@"models"];
    if ([existingModels isKindOfClass:[NSArray class]]) [models addObjectsFromArray:existingModels];
    [models addObject:modelDefinition];
    inference[@"models"] = models;
}

NSDictionary<NSString *, id> *CPGroupSessionParams(NSDictionary<NSString *, id> *flat, BOOL isResume) {
    NSMutableDictionary<NSString *, id> *params = [NSMutableDictionary dictionary];
    NSDictionary *legacyProvider = nil;
    NSMutableArray<NSString *> *groupedKeys = [NSMutableArray array];

    for (NSString *key in flat) {
        id value = flat[key];
        if ([key isEqualToString:@"provider"]) {
            if ([value isKindOfClass:[NSDictionary class]]) legacyProvider = value;
            continue;
        }
        if ([value isKindOfClass:[NSDictionary class]] && CPIsSessionParamGroup(key)) {
            [groupedKeys addObject:key];
            continue;
        }
        if (value == [NSNull null] && !(isResume && [key isEqualToString:@"defaultProvider"])) continue;

        const CPSessionParamRule *rule = CPFindSessionParamRule(key);
        if (!rule) {
            params[key] = value;
            continue;
        }
        if ((rule->scope == CPParamScopeCreateOnly && isResume) ||
            (rule->scope == CPParamScopeResumeOnly && !isResume)) {
            continue;
        }
        NSString *member = @(rule->member);
        if (rule->group) {
            CPPutGroupMemberIfAbsent(params, @(rule->group), member, value);
        } else {
            params[member] = value;
        }
    }

    // Already-grouped objects are merged last so their members win.
    for (NSString *key in groupedKeys) {
        NSMutableDictionary *group = CPChildGroup(params, key);
        [group addEntriesFromDictionary:flat[key]];
    }

    // MCP server definitions use direct environment values.
    if (CPHasGroupMember(params, @"mcp", @"servers")) {
        CPPutGroupMemberIfAbsent(params, @"mcp", @"envValueMode", @"direct");
    }

    if (legacyProvider) CPTranslateLegacyProvider(params, legacyProvider);

    size_t groupCount = sizeof(kCPSessionParamGroups) / sizeof(kCPSessionParamGroups[0]);
    for (size_t i = 0; i < groupCount; i++) {
        NSString *group = @(kCPSessionParamGroups[i]);
        id value = params[group];
        if ([value isKindOfClass:[NSDictionary class]] && ((NSDictionary *)value).count == 0) {
            [params removeObjectForKey:group];
        }
    }
    return [params copy];
}

NSDictionary<NSString *, id> *CPApplyEmptyModeDefaults(NSDictionary<NSString *, id> *params) {
    NSMutableDictionary *result = [params mutableCopy];
    CPPutGroupMemberIfAbsent(result, @"experiments", @"enableExperimentalMode", @NO);
    CPPutGroupMemberIfAbsent(result, @"github", @"telemetryEnabled", @NO);
    CPPutGroupMemberIfAbsent(result, @"mcp", @"oauthTokenStorage", @"in-memory");
    CPPutGroupMemberIfAbsent(result, @"state", @"skipEmbeddingRetrieval", @YES);
    CPPutGroupMemberIfAbsent(result, @"state", @"embeddingCacheStorage", @"in-memory");
    CPPutGroupMemberIfAbsent(result, @"state", @"enableCrossSessionStore", @NO);
    CPPutGroupMemberIfAbsent(result, @"state", @"memory", @{@"enabled": @NO});
    CPPutGroupMemberIfAbsent(result, @"instructions", @"onDemandDiscovery", @NO);
    CPPutGroupMemberIfAbsent(result, @"hooks", @"enableRepositoryHooks", @NO);
    CPPutGroupMemberIfAbsent(result, @"git", @"enableHostGitOperations", @NO);
    return [result copy];
}

// session_params.zig -- SDK protocol 4 session payloads.
//
// The SDK builds the same flat `session.create` / `session.resume` options it
// always has; `groupSessionParams` translates them into the grouped protocol 4
// payload right before sending (for example `streaming` becomes
// `events.streaming`), and translates the legacy single `provider`.

const std = @import("std");

const Allocator = std.mem.Allocator;
const JsonValue = std.json.Value;
const ObjectMap = std.json.ObjectMap;

/// Lifecycle methods that accept a key. Keys sent to the wrong method are dropped.
const ParamScope = enum { both, create_only, resume_only };

/// Maps a flat key to its protocol 4 location (`group == null` keeps it at the root).
const SessionParamRule = struct {
    flat_key: []const u8,
    group: ?[]const u8,
    member: []const u8,
    scope: ParamScope = .both,
};

fn root(flat_key: []const u8, member: []const u8, scope: ParamScope) SessionParamRule {
    return .{ .flat_key = flat_key, .group = null, .member = member, .scope = scope };
}

fn grouped(flat_key: []const u8, group: []const u8, member: []const u8) SessionParamRule {
    return .{ .flat_key = flat_key, .group = group, .member = member };
}

fn groupedScoped(flat_key: []const u8, group: []const u8, member: []const u8, scope: ParamScope) SessionParamRule {
    return .{ .flat_key = flat_key, .group = group, .member = member, .scope = scope };
}

const session_param_rules = [_]SessionParamRule{
    // Root fields (configDir and disableResume are renamed)
    root("sessionId", "sessionId", .both),
    root("model", "model", .both),
    root("name", "name", .create_only),
    root("clientName", "clientName", .both),
    root("workingDirectory", "workingDirectory", .both),
    root("configDirectory", "configDirectory", .both),
    root("configDir", "configDirectory", .both),
    root("enableConfigDiscovery", "enableConfigDiscovery", .both),
    root("systemMessage", "systemMessage", .both),
    root("diagnostics", "diagnostics", .both),
    root("lifecycleDiagnostics", "lifecycleDiagnostics", .both),
    root("cloud", "cloud", .create_only),
    root("traceparent", "traceparent", .both),
    root("tracestate", "tracestate", .both),
    root("suppressResumeEvent", "suppressResumeEvent", .resume_only),
    root("disableResume", "suppressResumeEvent", .resume_only),
    root("continuePendingWork", "continuePendingWork", .resume_only),
    root("allowTranscriptRecovery", "allowTranscriptRecovery", .resume_only),
    root("requestedEnvironmentVariables", "requestedEnvironmentVariables", .resume_only),
    // inference
    grouped("reasoningEffort", "inference", "reasoningEffort"),
    grouped("reasoningSummary", "inference", "reasoningSummary"),
    grouped("contextTier", "inference", "contextTier"),
    grouped("allowMultipleToolCalls", "inference", "allowMultipleToolCalls"),
    grouped("enableCitations", "inference", "enableCitations"),
    grouped("modelCapabilities", "inference", "capabilities"),
    grouped("verbosity", "inference", "verbosity"),
    grouped("providers", "inference", "providers"),
    grouped("models", "inference", "models"),
    grouped("defaultProvider", "inference", "defaultProvider"),
    // tools
    grouped("tools", "tools", "definitions"),
    grouped("availableTools", "tools", "availableTools"),
    grouped("excludedTools", "tools", "excludedTools"),
    grouped("toolSearch", "tools", "search"),
    grouped("largeOutput", "tools", "largeOutput"),
    grouped("imageGeneration", "tools", "imageGeneration"),
    grouped("manageScheduleEnabled", "tools", "manageScheduleEnabled"),
    grouped("webSearch", "tools", "webSearch"),
    grouped("shell", "tools", "shell"),
    grouped("maxInlineBinaryBytes", "tools", "maxInlineBinaryBytes"),
    // permissions
    grouped("requestPermission", "permissions", "request"),
    grouped("sandboxConfig", "permissions", "sandbox"),
    grouped("sandbox", "permissions", "sandbox"),
    grouped("permissionMode", "permissions", "mode"),
    grouped("sandboxConfigSource", "permissions", "sandboxSource"),
    // interaction
    grouped("commands", "interaction", "commands"),
    grouped("askUserVariant", "interaction", "askUserVariant"),
    grouped("requestUserInput", "interaction", "requestUserInput"),
    grouped("requestElicitation", "interaction", "requestElicitation"),
    grouped("requestShellAuthentication", "interaction", "requestShellAuthentication"),
    grouped("requestExitPlanMode", "interaction", "requestExitPlanMode"),
    grouped("requestAutoModeSwitch", "interaction", "requestAutoModeSwitch"),
    grouped("requestAutoModeRecovery", "interaction", "requestAutoModeRecovery"),
    // events
    grouped("streaming", "events", "streaming"),
    grouped("includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents"),
    // agents
    grouped("customAgents", "agents", "definitions"),
    grouped("defaultAgent", "agents", "default"),
    grouped("agent", "agents", "selected"),
    grouped("includedBuiltinAgents", "agents", "includedBuiltinAgents"),
    grouped("excludedBuiltinAgents", "agents", "excludedBuiltinAgents"),
    grouped("customAgentsLocalOnly", "agents", "localDiscoveryOnly"),
    grouped("agentDirectories", "agents", "directories"),
    // skills
    grouped("skillDirectories", "skills", "directories"),
    grouped("enableSkills", "skills", "enabled"),
    grouped("includedBuiltinSkills", "skills", "includedBuiltinSkills"),
    grouped("disabledSkills", "skills", "disabledSkills"),
    grouped("hasSkillProvider", "skills", "hasProvider"),
    grouped("ignoredSkillsLocations", "skills", "ignoredLocations"),
    // mcp
    grouped("mcpServers", "mcp", "servers"),
    grouped("disabledMcpServers", "mcp", "disabledServers"),
    grouped("requestMcpApps", "mcp", "enableApps"),
    grouped("enableMcpApps", "mcp", "enableApps"),
    grouped("mcpOAuthTokenStorage", "mcp", "oauthTokenStorage"),
    grouped("authClientIdMetadataUrl", "mcp", "authClientIdMetadataUrl"),
    grouped("envValueMode", "mcp", "envValueMode"),
    grouped("managedMcpServers", "mcp", "managedServers"),
    grouped("allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions"),
    // github
    grouped("gitHubToken", "github", "token"),
    grouped("gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId"),
    grouped("githubMcpToolConfig", "github", "mcp"),
    grouped("capi", "github", "capi"),
    grouped("enableSessionTelemetry", "github", "telemetryEnabled"),
    grouped("enableManagedSettings", "github", "fetchManagedSettings"),
    grouped("interactiveUser", "github", "interactiveUser"),
    grouped("enableGitHubTelemetryForwarding", "github", "telemetryForwarding"),
    grouped("authInfo", "github", "authInfo"),
    grouped("copilotUrl", "github", "copilotUrl"),
    // filesystem
    grouped("additionalDirectories", "filesystem", "additionalDirectories"),
    grouped("enableFileChangeTracking", "filesystem", "trackFileChanges"),
    groupedScoped("trustWorkingDirectory", "filesystem", "trustWorkingDirectory", .create_only),
    // execution
    grouped("sessionLimits", "execution", "limits"),
    grouped("autopilot", "execution", "autopilot"),
    grouped("autopilotContinuation", "execution", "autopilotContinuation"),
    // instructions
    grouped("instructionDirectories", "instructions", "directories"),
    grouped("organizationCustomInstructions", "instructions", "organizationText"),
    grouped("enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery"),
    grouped("skipCustomInstructions", "instructions", "skipCustomInstructions"),
    groupedScoped("refreshCustomInstructions", "instructions", "refreshCustomInstructions", .create_only),
    // hooks (boolean "hooks" becomes hooks.hasHandlers)
    grouped("hooks", "hooks", "hasHandlers"),
    grouped("hookDirectories", "hooks", "directories"),
    grouped("enableFileHooks", "hooks", "enableRepositoryHooks"),
    // git
    grouped("coauthorEnabled", "git", "coauthorEnabled"),
    grouped("enableHostGitOperations", "git", "enableHostGitOperations"),
    // state
    grouped("infiniteSessions", "state", "infiniteSessions"),
    grouped("memory", "state", "memory"),
    grouped("hasMemoryStore", "state", "hasMemoryStore"),
    grouped("enableSessionStore", "state", "enableCrossSessionStore"),
    grouped("skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval"),
    grouped("embeddingCacheStorage", "state", "embeddingCacheStorage"),
    // plugins
    grouped("pluginDirectories", "plugins", "directories"),
    grouped("installedPlugins", "plugins", "installed"),
    // canvases
    grouped("canvases", "canvases", "definitions"),
    grouped("canvasProvider", "canvases", "provider"),
    grouped("requestCanvasRenderer", "canvases", "requestRenderer"),
    groupedScoped("openCanvases", "canvases", "open", .resume_only),
    // extensions
    grouped("requestExtensions", "extensions", "enabled"),
    grouped("extensionSdkPath", "extensions", "sdkPath"),
    grouped("extensionInfo", "extensions", "identity"),
    // experiments
    grouped("enableExperimentalMode", "experiments", "enableExperimentalMode"),
    grouped("isExperimentalMode", "experiments", "enableExperimentalMode"),
    grouped("featureFlags", "experiments", "flags"),
    grouped("expAssignments", "experiments", "assignments"),
    grouped("featureFlagConfiguration", "experiments", "configuration"),
    // policy
    grouped("allowedModels", "policy", "allowedModels"),
    grouped("managedSettings", "policy", "managedSettings"),
    grouped("excludedManagedSettings", "policy", "excludedManagedSettings"),
    grouped("enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults"),
};

/// Protocol 4 option groups. A key naming a group that holds an object is already grouped.
const session_param_groups = [_][]const u8{
    "inference", "tools",        "permissions", "interaction", "events",
    "agents",    "skills",       "mcp",         "github",      "filesystem",
    "execution", "instructions", "hooks",       "git",         "state",
    "plugins",   "canvases",     "extensions",  "experiments", "policy",
};

/// Model-specific fields of the legacy single provider (moved to inference.models[]).
const legacy_provider_model_fields = [_][]const u8{
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens", "capabilities",
};

fn findRule(key: []const u8) ?SessionParamRule {
    for (session_param_rules) |rule| {
        if (std.mem.eql(u8, rule.flat_key, key)) return rule;
    }
    return null;
}

fn contains(names: []const []const u8, key: []const u8) bool {
    for (names) |name| {
        if (std.mem.eql(u8, name, key)) return true;
    }
    return false;
}

/// Returns `parent[name]` as an object, creating it (or replacing a non-object).
/// The pointer is invalidated when `parent` grows.
pub fn childObject(a: Allocator, parent: *ObjectMap, name: []const u8) Allocator.Error!*ObjectMap {
    const gop = try parent.getOrPut(name);
    if (!gop.found_existing or gop.value_ptr.* != .object) {
        gop.value_ptr.* = .{ .object = ObjectMap.init(a) };
    }
    return &gop.value_ptr.object;
}

/// Stores `value` at `out[group][member]` unless an explicit grouped member already exists.
fn putGroupMember(a: Allocator, out: *ObjectMap, group: []const u8, member: []const u8, value: JsonValue) Allocator.Error!void {
    const obj = try childObject(a, out, group);
    if (!obj.contains(member)) try obj.put(member, value);
}

fn hasMember(out: ObjectMap, group: []const u8, member: []const u8) bool {
    const value = out.get(group) orelse return false;
    return value == .object and value.object.contains(member);
}

/// Translates the legacy single `provider` (no longer accepted by protocol 4) into
/// `inference.providers [{name:"default", ...connection}]`, `inference.defaultProvider`
/// "default" and `github.capi.enabled` false, plus `inference.models` when model-specific
/// fields are present and the session model is set. Explicit `providers`,
/// `defaultProvider`, or `capi.enabled` win: the legacy provider is then dropped.
fn translateLegacyProvider(a: Allocator, out: *ObjectMap, provider: ObjectMap) Allocator.Error!void {
    if (hasMember(out.*, "inference", "providers") or hasMember(out.*, "inference", "defaultProvider")) return;
    if (out.get("github")) |github| {
        if (github == .object) {
            if (github.object.get("capi")) |capi| {
                if (capi == .object and capi.object.contains("enabled")) return;
            }
        }
    }

    var connection = ObjectMap.init(a);
    try connection.put("name", .{ .string = "default" });
    var model_fields = ObjectMap.init(a);
    var it = provider.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        if (entry.value_ptr.* == .null or std.mem.eql(u8, key, "name")) continue;
        if (contains(&legacy_provider_model_fields, key)) {
            try model_fields.put(key, entry.value_ptr.*);
        } else {
            try connection.put(key, entry.value_ptr.*);
        }
    }

    {
        const inference = try childObject(a, out, "inference");
        var providers = std.json.Array.init(a);
        try providers.append(.{ .object = connection });
        try inference.put("providers", .{ .array = providers });
        try inference.put("defaultProvider", .{ .string = "default" });
    }
    {
        const github = try childObject(a, out, "github");
        const capi = try childObject(a, github, "capi");
        try capi.put("enabled", .{ .bool = false });
    }

    if (model_fields.count() == 0) return;
    const model = out.get("model") orelse return;
    if (model != .string) return;

    var definition = ObjectMap.init(a);
    try definition.put("provider", .{ .string = "default" });
    try definition.put("id", model);
    var fields = model_fields.iterator();
    while (fields.next()) |entry| {
        try definition.put(entry.key_ptr.*, entry.value_ptr.*);
    }
    const inference = try childObject(a, out, "inference");
    const models = try inference.getOrPut("models");
    if (!models.found_existing or models.value_ptr.* != .array) {
        models.value_ptr.* = .{ .array = std.json.Array.init(a) };
    }
    try models.value_ptr.array.append(.{ .object = definition });
}

/// Converts the flat session.create/session.resume payload into the SDK protocol 4
/// grouped payload: root keys stay at the top level, every known flat key moves to
/// its option group, unknown keys stay at the root (forward compatibility), null
/// values are skipped (except `defaultProvider: null`, which clears it on resume),
/// explicit grouped members win over flat ones, and empty groups are not emitted.
/// All memory comes from `a` (use an arena).
pub fn groupSessionParams(a: Allocator, flat: ObjectMap, is_resume: bool) Allocator.Error!JsonValue {
    var out = ObjectMap.init(a);
    var legacy_provider: ?ObjectMap = null;
    var grouped_objects = std.ArrayList(GroupedObject).init(a);

    var it = flat.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        const value = entry.value_ptr.*;
        if (std.mem.eql(u8, key, "provider")) {
            if (value == .object) legacy_provider = value.object;
            continue;
        }
        if (value == .object and contains(&session_param_groups, key)) {
            try grouped_objects.append(.{ .name = key, .members = value.object });
            continue;
        }
        if (value == .null and !(is_resume and std.mem.eql(u8, key, "defaultProvider"))) continue;

        const rule = findRule(key) orelse {
            try out.put(key, value);
            continue;
        };
        switch (rule.scope) {
            .create_only => {
                if (is_resume) continue;
            },
            .resume_only => {
                if (!is_resume) continue;
            },
            .both => {},
        }
        if (rule.group) |group| {
            try putGroupMember(a, &out, group, rule.member, value);
        } else {
            try out.put(rule.member, value);
        }
    }

    // Already-grouped objects are merged last so their members win.
    for (grouped_objects.items) |g| {
        const obj = try childObject(a, &out, g.name);
        var members = g.members.iterator();
        while (members.next()) |m| {
            try obj.put(m.key_ptr.*, m.value_ptr.*);
        }
    }

    // MCP server definitions use direct environment values.
    if (hasMember(out, "mcp", "servers")) {
        try putGroupMember(a, &out, "mcp", "envValueMode", .{ .string = "direct" });
    }

    if (legacy_provider) |provider| try translateLegacyProvider(a, &out, provider);

    for (session_param_groups) |group| {
        if (out.get(group)) |value| {
            if (value == .object and value.object.count() == 0) _ = out.orderedRemove(group);
        }
    }
    return .{ .object = out };
}

const GroupedObject = struct {
    name: []const u8,
    members: ObjectMap,
};

/// Client mode "empty": minimal defaults applied underneath the caller's own values.
pub fn applyEmptyModeDefaults(a: Allocator, params: *ObjectMap) Allocator.Error!void {
    try putGroupMember(a, params, "experiments", "enableExperimentalMode", .{ .bool = false });
    try putGroupMember(a, params, "github", "telemetryEnabled", .{ .bool = false });
    try putGroupMember(a, params, "mcp", "oauthTokenStorage", .{ .string = "in-memory" });
    try putGroupMember(a, params, "state", "skipEmbeddingRetrieval", .{ .bool = true });
    try putGroupMember(a, params, "state", "embeddingCacheStorage", .{ .string = "in-memory" });
    try putGroupMember(a, params, "state", "enableCrossSessionStore", .{ .bool = false });
    var memory = ObjectMap.init(a);
    try memory.put("enabled", .{ .bool = false });
    try putGroupMember(a, params, "state", "memory", .{ .object = memory });
    try putGroupMember(a, params, "instructions", "onDemandDiscovery", .{ .bool = false });
    try putGroupMember(a, params, "hooks", "enableRepositoryHooks", .{ .bool = false });
    try putGroupMember(a, params, "git", "enableHostGitOperations", .{ .bool = false });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn expectGrouped(flat_json: []const u8, is_resume: bool, empty_mode: bool, expected_json: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const flat = try std.json.parseFromSliceLeaky(JsonValue, a, flat_json, .{});
    var result = try groupSessionParams(a, flat.object, is_resume);
    if (empty_mode) try applyEmptyModeDefaults(a, &result.object);

    // Compare structurally by re-parsing both sides into canonical (sorted) strings.
    const actual = try canonical(a, result);
    const expected = try canonical(a, try std.json.parseFromSliceLeaky(JsonValue, a, expected_json, .{}));
    try std.testing.expectEqualStrings(expected, actual);
}

fn canonical(a: Allocator, value: JsonValue) ![]const u8 {
    var buf = std.ArrayList(u8).init(a);
    try writeCanonical(buf.writer(), value);
    return buf.items;
}

fn writeCanonical(writer: anytype, value: JsonValue) !void {
    switch (value) {
        .object => |obj| {
            var keys = std.ArrayList([]const u8).init(obj.allocator);
            defer keys.deinit();
            var it = obj.iterator();
            while (it.next()) |entry| try keys.append(entry.key_ptr.*);
            std.mem.sort([]const u8, keys.items, {}, struct {
                fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                    return std.mem.lessThan(u8, lhs, rhs);
                }
            }.lessThan);
            try writer.writeAll("{");
            for (keys.items, 0..) |key, i| {
                if (i > 0) try writer.writeAll(",");
                try std.json.stringify(key, .{}, writer);
                try writer.writeAll(":");
                try writeCanonical(writer, obj.get(key).?);
            }
            try writer.writeAll("}");
        },
        .array => |arr| {
            try writer.writeAll("[");
            for (arr.items, 0..) |item, i| {
                if (i > 0) try writer.writeAll(",");
                try writeCanonical(writer, item);
            }
            try writer.writeAll("]");
        },
        else => try std.json.stringify(value, .{}, writer),
    }
}

test "groupSessionParams: spec example on resume" {
    try expectGrouped(
        \\{"model":"m","streaming":true,"requestPermission":true,"tools":[{"name":"t"}],"hooks":true,"disableResume":true}
    , true, false,
        \\{"model":"m","events":{"streaming":true},"permissions":{"request":true},"tools":{"definitions":[{"name":"t"}]},"hooks":{"hasHandlers":true},"suppressResumeEvent":true}
    );
}

test "groupSessionParams: legacy provider becomes the default named provider" {
    try expectGrouped(
        \\{"model":"gpt","capi":{"autoTier":"fast"},"provider":{"type":"openai","baseUrl":"http://x","apiKeyCommand":"get-key","maxOutputTokens":100}}
    , false, false,
        \\{"model":"gpt","github":{"capi":{"autoTier":"fast","enabled":false}},"inference":{"providers":[{"name":"default","type":"openai","baseUrl":"http://x","apiKeyCommand":"get-key"}],"defaultProvider":"default","models":[{"provider":"default","id":"gpt","maxOutputTokens":100}]}}
    );
}

test "groupSessionParams: explicit providers win over the legacy provider" {
    try expectGrouped(
        \\{"provider":{"baseUrl":"http://legacy"},"providers":[{"name":"p","baseUrl":"http://p"}],"defaultProvider":"p"}
    , false, false,
        \\{"inference":{"providers":[{"name":"p","baseUrl":"http://p"}],"defaultProvider":"p"}}
    );
}

test "groupSessionParams: scope rules, nulls, unknown keys, and grouped members" {
    try expectGrouped(
        \\{"sessionId":"s","name":"n","trustWorkingDirectory":"session","refreshCustomInstructions":true,"openCanvases":["c"],"defaultProvider":null,"model":null,"otlpProtocol":"grpc","toolSearch":{"y":2},"tools":{"search":{"x":1}},"configDir":"/c"}
    , true, false,
        \\{"sessionId":"s","canvases":{"open":["c"]},"inference":{"defaultProvider":null},"otlpProtocol":"grpc","tools":{"search":{"x":1}},"configDirectory":"/c"}
    );
    try expectGrouped(
        \\{"openCanvases":["c"],"allowTranscriptRecovery":true,"trustWorkingDirectory":"user","defaultProvider":null,"mcpServers":{"s":{"command":"x"}}}
    , false, false,
        \\{"filesystem":{"trustWorkingDirectory":"user"},"mcp":{"servers":{"s":{"command":"x"}},"envValueMode":"direct"}}
    );
}

test "applyEmptyModeDefaults keeps caller values" {
    try expectGrouped(
        \\{"availableTools":[],"enableExperimentalMode":true}
    , false, true,
        \\{"tools":{"availableTools":[]},"experiments":{"enableExperimentalMode":true},"github":{"telemetryEnabled":false},"mcp":{"oauthTokenStorage":"in-memory"},"state":{"skipEmbeddingRetrieval":true,"embeddingCacheStorage":"in-memory","enableCrossSessionStore":false,"memory":{"enabled":false}},"instructions":{"onDemandDiscovery":false},"hooks":{"enableRepositoryHooks":false},"git":{"enableHostGitOperations":false}}
    );
}

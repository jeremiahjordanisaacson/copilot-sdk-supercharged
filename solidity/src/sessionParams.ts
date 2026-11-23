/**
 * SDK protocol 4 session payload grouping.
 *
 * Callers keep passing the flat (v2.x) session options. Protocol 4 groups
 * them (`streaming` becomes `events.streaming`, `tools` becomes
 * `tools.definitions`, a boolean `hooks` becomes `hooks.hasHandlers`, and so
 * on). {@link groupSessionParams} performs that conversion right before
 * `session.create` / `session.resume`: for the base `CopilotClient` (which
 * serializes the grouped options onto the wire) and for the raw RPC helpers.
 */

/** Option groups of the protocol 4 `session.create` / `session.resume` payloads. */
export const SESSION_OPTION_GROUPS = [
    "inference",
    "tools",
    "permissions",
    "interaction",
    "events",
    "agents",
    "skills",
    "mcp",
    "github",
    "filesystem",
    "execution",
    "instructions",
    "hooks",
    "git",
    "state",
    "plugins",
    "canvases",
    "extensions",
    "experiments",
    "policy",
] as const;

/** A protocol 4 session option group name. */
export type SessionOptionGroup = (typeof SESSION_OPTION_GROUPS)[number];

const GROUP_NAMES: ReadonlySet<string> = new Set(SESSION_OPTION_GROUPS);

/** Flat keys only valid on `session.create` (dropped on resume). */
const CREATE_ONLY_KEYS: ReadonlySet<string> = new Set([
    "name",
    "cloud",
    "trustWorkingDirectory",
    "refreshCustomInstructions",
]);

/** Flat keys only valid on `session.resume` (dropped on create). */
const RESUME_ONLY_KEYS: ReadonlySet<string> = new Set([
    "disableResume",
    "suppressResumeEvent",
    "continuePendingWork",
    "allowTranscriptRecovery",
    "requestedEnvironmentVariables",
    "openCanvases",
]);

/** Keys of a grouped `hooks` object (anything else is a legacy handler object). */
const HOOK_GROUP_MEMBERS = ["handlers", "hasHandlers", "directories", "enableRepositoryHooks"];

/**
 * Flat session option -> protocol 4 `[group, member]`. Root keys (`sessionId`,
 * `model`, `name`, `clientName`, `workingDirectory`, `configDirectory`,
 * `enableConfigDiscovery`, `systemMessage`, `diagnostics`,
 * `lifecycleDiagnostics`, `cloud`, `traceparent`, `tracestate` and the resume
 * controls) and unknown keys stay top level.
 */
const FLAT_SESSION_KEYS: Readonly<Record<string, readonly [SessionOptionGroup, string]>> = {
    reasoningEffort: ["inference", "reasoningEffort"],
    reasoningSummary: ["inference", "reasoningSummary"],
    contextTier: ["inference", "contextTier"],
    allowMultipleToolCalls: ["inference", "allowMultipleToolCalls"],
    enableCitations: ["inference", "enableCitations"],
    modelCapabilities: ["inference", "capabilities"],
    verbosity: ["inference", "verbosity"],
    providers: ["inference", "providers"],
    models: ["inference", "models"],
    defaultProvider: ["inference", "defaultProvider"],
    availableTools: ["tools", "availableTools"],
    excludedTools: ["tools", "excludedTools"],
    toolSearch: ["tools", "search"],
    largeOutput: ["tools", "largeOutput"],
    imageGeneration: ["tools", "imageGeneration"],
    manageScheduleEnabled: ["tools", "manageScheduleEnabled"],
    webSearch: ["tools", "webSearch"],
    shell: ["tools", "shell"],
    maxInlineBinaryBytes: ["tools", "maxInlineBinaryBytes"],
    requestPermission: ["permissions", "request"],
    sandboxConfig: ["permissions", "sandbox"],
    sandbox: ["permissions", "sandbox"],
    permissionMode: ["permissions", "mode"],
    sandboxConfigSource: ["permissions", "sandboxSource"],
    commands: ["interaction", "commands"],
    askUserVariant: ["interaction", "askUserVariant"],
    requestUserInput: ["interaction", "requestUserInput"],
    requestElicitation: ["interaction", "requestElicitation"],
    requestShellAuthentication: ["interaction", "requestShellAuthentication"],
    requestExitPlanMode: ["interaction", "requestExitPlanMode"],
    requestAutoModeSwitch: ["interaction", "requestAutoModeSwitch"],
    requestAutoModeRecovery: ["interaction", "requestAutoModeRecovery"],
    streaming: ["events", "streaming"],
    includeSubAgentStreamingEvents: ["events", "includeSubAgentStreamingEvents"],
    customAgents: ["agents", "definitions"],
    defaultAgent: ["agents", "default"],
    agent: ["agents", "selected"],
    includedBuiltinAgents: ["agents", "includedBuiltinAgents"],
    excludedBuiltinAgents: ["agents", "excludedBuiltinAgents"],
    customAgentsLocalOnly: ["agents", "localDiscoveryOnly"],
    agentDirectories: ["agents", "directories"],
    skillDirectories: ["skills", "directories"],
    enableSkills: ["skills", "enabled"],
    includedBuiltinSkills: ["skills", "includedBuiltinSkills"],
    disabledSkills: ["skills", "disabledSkills"],
    hasSkillProvider: ["skills", "hasProvider"],
    ignoredSkillsLocations: ["skills", "ignoredLocations"],
    mcpServers: ["mcp", "servers"],
    disabledMcpServers: ["mcp", "disabledServers"],
    requestMcpApps: ["mcp", "enableApps"],
    enableMcpApps: ["mcp", "enableApps"],
    mcpOAuthTokenStorage: ["mcp", "oauthTokenStorage"],
    authClientIdMetadataUrl: ["mcp", "authClientIdMetadataUrl"],
    envValueMode: ["mcp", "envValueMode"],
    managedMcpServers: ["mcp", "managedServers"],
    allowAllMcpServerInstructions: ["mcp", "allowAllServerInstructions"],
    gitHubToken: ["github", "token"],
    gitHubTokenProviderRegistrationId: ["github", "tokenProviderRegistrationId"],
    githubMcpToolConfig: ["github", "mcp"],
    capi: ["github", "capi"],
    enableSessionTelemetry: ["github", "telemetryEnabled"],
    enableManagedSettings: ["github", "fetchManagedSettings"],
    interactiveUser: ["github", "interactiveUser"],
    enableGitHubTelemetryForwarding: ["github", "telemetryForwarding"],
    authInfo: ["github", "authInfo"],
    copilotUrl: ["github", "copilotUrl"],
    additionalDirectories: ["filesystem", "additionalDirectories"],
    enableFileChangeTracking: ["filesystem", "trackFileChanges"],
    trustWorkingDirectory: ["filesystem", "trustWorkingDirectory"],
    sessionLimits: ["execution", "limits"],
    autopilot: ["execution", "autopilot"],
    autopilotContinuation: ["execution", "autopilotContinuation"],
    instructionDirectories: ["instructions", "directories"],
    organizationCustomInstructions: ["instructions", "organizationText"],
    enableOnDemandInstructionDiscovery: ["instructions", "onDemandDiscovery"],
    skipCustomInstructions: ["instructions", "skipCustomInstructions"],
    refreshCustomInstructions: ["instructions", "refreshCustomInstructions"],
    hookDirectories: ["hooks", "directories"],
    enableFileHooks: ["hooks", "enableRepositoryHooks"],
    coauthorEnabled: ["git", "coauthorEnabled"],
    enableHostGitOperations: ["git", "enableHostGitOperations"],
    infiniteSessions: ["state", "infiniteSessions"],
    memory: ["state", "memory"],
    hasMemoryStore: ["state", "hasMemoryStore"],
    enableSessionStore: ["state", "enableCrossSessionStore"],
    skipEmbeddingRetrieval: ["state", "skipEmbeddingRetrieval"],
    embeddingCacheStorage: ["state", "embeddingCacheStorage"],
    pluginDirectories: ["plugins", "directories"],
    installedPlugins: ["plugins", "installed"],
    canvasProvider: ["canvases", "provider"],
    requestCanvasRenderer: ["canvases", "requestRenderer"],
    openCanvases: ["canvases", "open"],
    requestExtensions: ["extensions", "enabled"],
    extensionSdkPath: ["extensions", "sdkPath"],
    extensionInfo: ["extensions", "identity"],
    enableExperimentalMode: ["experiments", "enableExperimentalMode"],
    isExperimentalMode: ["experiments", "enableExperimentalMode"],
    featureFlags: ["experiments", "flags"],
    expAssignments: ["experiments", "assignments"],
    featureFlagConfiguration: ["experiments", "configuration"],
    allowedModels: ["policy", "allowedModels"],
    managedSettings: ["policy", "managedSettings"],
    excludedManagedSettings: ["policy", "excludedManagedSettings"],
    enforceManagedModelDefaults: ["policy", "enforceManagedModelDefaults"],
    // Handler-backed options of the base CopilotClient. The base client keeps
    // the callbacks client-side and emits the matching wire flags
    // (permissions.request, interaction.requestUserInput, github.tokenProviderRegistrationId, ...).
    onPermissionRequest: ["permissions", "onRequest"],
    onUserInputRequest: ["interaction", "onUserInputRequest"],
    onElicitationRequest: ["interaction", "onElicitationRequest"],
    onExitPlanModeRequest: ["interaction", "onExitPlanModeRequest"],
    onExitPlanMode: ["interaction", "onExitPlanModeRequest"],
    onAutoModeSwitchRequest: ["interaction", "onAutoModeSwitchRequest"],
    autoModeRecovery: ["interaction", "autoModeRecovery"],
    onShellAuthenticationRequest: ["interaction", "onShellAuthenticationRequest"],
    onEvent: ["events", "onEvent"],
    onMcpAuthRequest: ["mcp", "onAuthRequest"],
    gitHubTokenProvider: ["github", "tokenProvider"],
    skillProvider: ["skills", "provider"],
    createSessionFsProvider: ["state", "createSessionFsProvider"],
};

/** Legacy single-provider fields that describe the model rather than the connection. */
const LEGACY_PROVIDER_MODEL_FIELDS: ReadonlySet<string> = new Set([
    "modelId",
    "wireModel",
    "maxPromptTokens",
    "maxContextWindowTokens",
    "maxOutputTokens",
    "capabilities",
]);

/** Session defaults applied under the caller's values when the client mode is `"empty"`. */
const EMPTY_MODE_SESSION_DEFAULTS: ReadonlyArray<readonly [SessionOptionGroup, string, () => unknown]> = [
    ["experiments", "enableExperimentalMode", () => false],
    ["github", "telemetryEnabled", () => false],
    ["mcp", "oauthTokenStorage", () => "in-memory"],
    ["state", "skipEmbeddingRetrieval", () => true],
    ["state", "embeddingCacheStorage", () => "in-memory"],
    ["state", "enableCrossSessionStore", () => false],
    ["state", "memory", () => ({ enabled: false })],
    ["instructions", "onDemandDiscovery", () => false],
    ["hooks", "enableRepositoryHooks", () => false],
    ["git", "enableHostGitOperations", () => false],
];

type Payload = Record<string, unknown>;

function isPlainObject(value: unknown): value is Payload {
    return typeof value === "object" && value !== null && !Array.isArray(value);
}

function groupOf(groups: Map<string, Payload>, name: string): Payload {
    let group = groups.get(name);
    if (!group) {
        group = {};
        groups.set(name, group);
    }
    return group;
}

/**
 * Convert a flat (v2.x) session payload into the grouped protocol 4 shape.
 *
 * Known flat keys move into their option group, root keys and unknown keys
 * stay top level, `undefined`/`null` values and empty groups are dropped
 * (except an explicit `defaultProvider: null` on resume, which clears the
 * default provider), and grouped objects already present are merged with
 * their members winning. `configDir` becomes `configDirectory` and
 * `disableResume` becomes `suppressResumeEvent`. A legacy single `provider`
 * becomes a `"default"` named provider with Copilot API models disabled,
 * unless explicit `providers`, `defaultProvider` or `capi.enabled` are set.
 *
 * @param flat - Flat session payload.
 * @param isResume - True for `session.resume`, false for `session.create`.
 * @returns The grouped payload.
 */
export function groupSessionParams(flat: Readonly<Payload>, isResume: boolean): Payload {
    const payload: Payload = {};
    const groups = new Map<string, Payload>();
    const explicitGroups: Array<[string, Payload]> = [];
    let legacyProvider: Payload | undefined;

    for (const [key, value] of Object.entries(flat)) {
        if (value === undefined) continue;
        if (value === null && !(isResume && key === "defaultProvider")) continue;
        if (isResume && CREATE_ONLY_KEYS.has(key)) continue;
        if (!isResume && RESUME_ONLY_KEYS.has(key)) continue;

        if (key === "tools" && Array.isArray(value)) {
            groupOf(groups, "tools").definitions = value;
        } else if (key === "canvases" && Array.isArray(value)) {
            groupOf(groups, "canvases").definitions = value;
        } else if (key === "hooks" && typeof value === "boolean") {
            groupOf(groups, "hooks").hasHandlers = value;
        } else if (
            key === "hooks" &&
            isPlainObject(value) &&
            !HOOK_GROUP_MEMBERS.some((member) => member in value)
        ) {
            // Legacy handler object ({ onPreToolUse, onSubagentStart, ... }).
            groupOf(groups, "hooks").handlers = value;
        } else if (GROUP_NAMES.has(key) && isPlainObject(value)) {
            explicitGroups.push([key, value]);
        } else if (key === "configDir") {
            payload.configDirectory = value;
        } else if (key === "disableResume") {
            payload.suppressResumeEvent = value;
        } else if (key === "provider" && isPlainObject(value)) {
            legacyProvider = value;
        } else if (Object.hasOwn(FLAT_SESSION_KEYS, key)) {
            const [group, member] = FLAT_SESSION_KEYS[key]!;
            groupOf(groups, group)[member] = value;
        } else {
            payload[key] = value;
        }
    }

    for (const [name, members] of explicitGroups) {
        const target = groupOf(groups, name);
        for (const [member, value] of Object.entries(members)) {
            if (value !== undefined && (value !== null || member === "defaultProvider")) {
                target[member] = value;
            }
        }
    }

    // Grouped members that only exist on the other operation are rejected by the runtime.
    if (isResume) {
        delete groups.get("filesystem")?.trustWorkingDirectory;
        delete groups.get("instructions")?.refreshCustomInstructions;
    } else {
        delete groups.get("canvases")?.open;
    }

    if (legacyProvider) {
        groupLegacyProvider(legacyProvider, payload, groups);
    }

    const mcp = groups.get("mcp");
    if (mcp && mcp.servers !== undefined && mcp.envValueMode === undefined) {
        mcp.envValueMode = "direct";
    }

    for (const [name, members] of groups) {
        if (Object.keys(members).length > 0) {
            payload[name] = members;
        }
    }
    return payload;
}

/**
 * Translate a legacy single `provider` into `inference.providers` (a
 * `"default"` connection), `inference.defaultProvider = "default"`,
 * `github.capi.enabled = false` and, when the session model is set and the
 * provider carries model fields, `inference.models`. A root-level
 * `bearerTokenProvider` callback joins the default connection.
 */
function groupLegacyProvider(provider: Payload, payload: Payload, groups: Map<string, Payload>): void {
    const inference = groupOf(groups, "inference");
    const github = groupOf(groups, "github");
    const capi = github.capi;
    const explicitCapi = isPlainObject(capi) && capi.enabled !== undefined;
    if (inference.providers !== undefined || inference.defaultProvider !== undefined || explicitCapi) {
        return;
    }

    const connection: Payload = { name: "default" };
    const modelFields: Payload = {};
    for (const [key, value] of Object.entries(provider)) {
        if (value === undefined || value === null || key === "name") continue;
        if (LEGACY_PROVIDER_MODEL_FIELDS.has(key)) {
            modelFields[key] = value;
        } else {
            connection[key] = value;
        }
    }
    if (typeof payload.bearerTokenProvider === "function") {
        connection.bearerTokenProvider ??= payload.bearerTokenProvider;
        delete payload.bearerTokenProvider;
    }

    inference.providers = [connection];
    inference.defaultProvider = "default";
    github.capi = { ...(isPlainObject(capi) ? capi : {}), enabled: false };
    if (Object.keys(modelFields).length > 0 && typeof payload.model === "string") {
        inference.models = [{ provider: "default", id: payload.model, ...modelFields }];
    }
}

/**
 * Enforce the `"empty"` client mode on a grouped payload: the session must
 * name its `tools.availableTools`, and the empty-mode defaults are applied
 * under the caller's values.
 *
 * @throws Error when `tools.availableTools` is missing
 */
export function applyEmptyModeSessionDefaults(payload: Payload): Payload {
    const tools = payload.tools;
    if (!isPlainObject(tools) || tools.availableTools === undefined || tools.availableTools === null) {
        throw new Error(
            "CopilotSolidityClient is in mode: 'empty' but the session config did not specify " +
                "'availableTools'. Empty mode requires every session to opt into the tools it wants.",
        );
    }
    for (const [group, member, value] of EMPTY_MODE_SESSION_DEFAULTS) {
        const target = isPlainObject(payload[group]) ? (payload[group] as Payload) : {};
        if (target[member] === undefined || target[member] === null) {
            target[member] = value();
        }
        payload[group] = target;
    }
    return payload;
}

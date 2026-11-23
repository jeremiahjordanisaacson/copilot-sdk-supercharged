/**
 * Type definitions for the Copilot Solidity SDK
 *
 * Provides types for smart contract analysis, auditing, gas optimization,
 * and integration with Hardhat/Foundry workflows.
 */

import type {
    CopilotClientMode,
    CopilotClientOptions,
    CreateSessionOptions,
    ResumeSessionOptions,
    SessionOptionsBase,
    Tool,
    AskUserVariant,
    AutoTier,
    CapiSessionOptions,
    CopilotClientInfo,
    GitHubTokenProvider,
    GitHubTokenProviderArgs,
    GitHubTokenProviderResult,
    ManagedSettingsResolvedData,
    MemoryStoreCallOptions,
    ModelSwitchAutoTierResult,
    NamedProviderConfig,
    NetworkConfig,
    PolicyHelperFailure,
    ProviderConfigModelProvider,
    ProviderModelConfig,
    SessionEventHandler,
    ShellAuthenticationHandler,
    ShellAuthenticationRequest,
    ShellAuthenticationResponse,
    StoredMemory,
    SubagentStartHandler,
    SubagentStartHookInput,
    SubagentStartHookOutput,
    SubagentStopHandler,
    SubagentStopHookInput,
    SubagentStopHookOutput,
    SystemMessageConfig,
    ToolShellOutputData,
    ToolShellOutputEvent,
    WorkingDirectoryTrust,
    CanvasAction,
    CanvasDeclaration,
    CanvasHostContext,
    CloudSessionOptions,
    CloudSessionRepository,
    ExitPlanModeRequest,
    ExitPlanModeResult,
    ExitPlanModeHandler,
    ModelCapabilitiesOverride,
    PermissionHandler,
    SectionOverride,
    SystemMessageAppendConfig,
    SystemMessageCustomizeConfig,
    SystemMessageReplaceConfig,
    TraceContext,
    TraceContextProvider,
} from "copilot-sdk-supercharged";

export type {
    AskUserVariant,
    AutoTier,
    CapiSessionOptions,
    CopilotClientInfo,
    GitHubTokenProvider,
    GitHubTokenProviderArgs,
    GitHubTokenProviderResult,
    ManagedSettingsResolvedData,
    MemoryStoreCallOptions,
    ModelSwitchAutoTierResult,
    NamedProviderConfig,
    NetworkConfig,
    PolicyHelperFailure,
    ProviderConfigModelProvider,
    ProviderModelConfig,
    ShellAuthenticationHandler,
    ShellAuthenticationRequest,
    ShellAuthenticationResponse,
    StoredMemory,
    SubagentStartHandler,
    SubagentStartHookInput,
    SubagentStartHookOutput,
    SubagentStopHandler,
    SubagentStopHookInput,
    SubagentStopHookOutput,
    ToolShellOutputData,
    ToolShellOutputEvent,
    WorkingDirectoryTrust,
    CanvasAction,
    CanvasDeclaration,
    CanvasHostContext,
    CloudSessionOptions,
    CloudSessionRepository,
    ExitPlanModeRequest,
    ExitPlanModeResult,
    ExitPlanModeHandler,
    ModelCapabilitiesOverride,
    SectionOverride,
    SystemMessageAppendConfig,
    SystemMessageCustomizeConfig,
    SystemMessageReplaceConfig,
    TraceContext,
    TraceContextProvider,
};

/** Type of `member` in the base SDK's grouped session option `group`. */
type SessionGroupMember<
    G extends keyof SessionOptionsBase,
    M extends keyof NonNullable<SessionOptionsBase[G]>,
> = NonNullable<SessionOptionsBase[G]>[M];

// ---------------------------------------------------------------------------
// Severity & finding types
// ---------------------------------------------------------------------------

/** Severity levels for audit findings, matching common standards (Slither, MythX). */
export type Severity = "critical" | "high" | "medium" | "low" | "informational";

/** A single finding from a security audit analysis. */
export interface AuditFinding {
    /** Unique identifier for the finding. */
    id: string;
    /** Short title describing the vulnerability. */
    title: string;
    /** Detailed description including root cause and impact. */
    description: string;
    /** Severity rating. */
    severity: Severity;
    /** Affected line range in the source file (1-based, inclusive). */
    lineRange?: { start: number; end: number };
    /** Suggested remediation code or prose. */
    recommendation?: string;
    /** SWC registry ID if applicable (e.g. "SWC-107"). */
    swcId?: string;
}

/** Complete audit report returned by the audit tool. */
export interface AuditReport {
    /** Name of the contract that was audited. */
    contractName: string;
    /** Source file path relative to the project root. */
    filePath: string;
    /** Ordered list of findings, most severe first. */
    findings: AuditFinding[];
    /** High-level summary produced by Copilot. */
    summary: string;
    /** ISO-8601 timestamp of when the audit was performed. */
    timestamp: string;
}

// ---------------------------------------------------------------------------
// Gas optimization types
// ---------------------------------------------------------------------------

/** A single gas optimization suggestion. */
export interface GasOptimization {
    /** Short title for the optimization. */
    title: string;
    /** Explanation of the optimization and why it saves gas. */
    description: string;
    /** Estimated gas savings (human-readable, e.g. "~2,100 gas per call"). */
    estimatedSavings?: string;
    /** Original code snippet. */
    before?: string;
    /** Optimized code snippet. */
    after?: string;
    /** Category of optimization. */
    category: GasCategory;
}

/** Categories for gas optimization suggestions. */
export type GasCategory =
    | "storage"
    | "memory"
    | "calldata"
    | "loop"
    | "packing"
    | "short-circuit"
    | "unchecked-math"
    | "general";

/** Complete gas optimization report. */
export interface GasReport {
    contractName: string;
    filePath: string;
    optimizations: GasOptimization[];
    summary: string;
    timestamp: string;
}

// ---------------------------------------------------------------------------
// Test generation types
// ---------------------------------------------------------------------------

/** Framework target for generated tests. */
export type TestFramework = "hardhat" | "foundry";

/** Generated test suite. */
export interface GeneratedTestSuite {
    /** The contract under test. */
    contractName: string;
    /** Target test framework. */
    framework: TestFramework;
    /** Generated test source code. */
    code: string;
    /** Number of test cases generated. */
    testCount: number;
    /** Human-readable summary. */
    summary: string;
}

// ---------------------------------------------------------------------------
// Client option extensions
// ---------------------------------------------------------------------------

/**
 * Client defaulting strategy. `"coding-agent"` (the default) keeps today's
 * Copilot CLI behavior; `"empty"` disables ambient features and requires
 * every session to name its `availableTools`. `"copilot-cli"` is accepted as
 * a legacy alias of `"coding-agent"`.
 */
export type SolidityClientMode = CopilotClientMode | "copilot-cli";

/** Options specific to the Solidity SDK client, extending the base options. */
export interface CopilotSolidityClientOptions extends Omit<CopilotClientOptions, "mode"> {
    /** Default Solidity compiler version (e.g. "0.8.24"). */
    solidityVersion?: string;
    /** Default test framework when generating tests. */
    defaultTestFramework?: TestFramework;
    /** Whether to include gas optimization hints in every audit. */
    includeGasHints?: boolean;
    /**
     * Client mode (default `"coding-agent"`). In `"empty"` mode the one-shot
     * helpers restrict their sessions to the four Solidity tools.
     */
    mode?: SolidityClientMode;
    /** Path to the Copilot CLI executable (spawned connections). Defaults to `COPILOT_CLI_PATH` or the bundled CLI. */
    cliPath?: string;
    /** Extra arguments for the spawned Copilot CLI. */
    cliArgs?: string[];
    /** Connect to an already-running CLI server (`"host:port"`, `"http://host:port"`, or a port). */
    cliUrl?: string;
    /** Working directory for the spawned CLI process. */
    cwd?: string;
    /** Environment for the spawned CLI process. */
    env?: Record<string, string | undefined>;
    /** Spawn the CLI over TCP on this port instead of stdio (`0` picks a free port). */
    port?: number;
    /** Set to `false` to spawn the CLI over TCP instead of stdio. */
    useStdio?: boolean;
    /** Path to the Copilot home directory (the runtime's base directory). */
    copilotHome?: string;
    /** TCP connection token for authenticated server connections. */
    tcpConnectionToken?: string;
    /** Enable remote session support (Mission Control). */
    remote?: boolean;
    /** W3C Trace Context provider for distributed tracing. */
    onGetTraceContext?: TraceContextProvider;
    /**
     * Identity of the integrating application. When any field is non-empty the
     * base client sends it on the `connect` handshake (as editorName /
     * editorVersion / extensionName / extensionVersion) before `ping`; older
     * runtimes without `connect` are tolerated.
     */
    clientInfo?: CopilotClientInfo;
    /**
     * Network callbacks (experimental). When `resolveProxy`,
     * `authenticateNegotiate` or `authenticateBasic` is set, the base client
     * calls `network.setProvider` after connecting (`scope` defaults to
     * `"connection"`) and answers the runtime's `network.resolveProxy`,
     * `network.authenticateNegotiate` and `network.authenticateBasic`
     * requests with these callbacks. Tokens and passwords are never logged.
     */
    network?: NetworkConfig;
}

/**
 * Legacy single BYOK provider (`provider`). Protocol 4 no longer accepts it:
 * the client sends it as a `"default"` named provider (`inference.providers`)
 * with `inference.defaultProvider = "default"` and Copilot API models disabled
 * (`github.capi.enabled = false`). Model fields become an `inference.models`
 * entry for the session model.
 */
export interface SolidityProviderConfig extends Omit<NamedProviderConfig, "name">, ByokProviderOptions {
    /** Well-known base model id used for capability lookup. */
    modelId?: string;
    /** Model name sent to the provider API. */
    wireModel?: string;
    /** Maximum prompt tokens for the session model. */
    maxPromptTokens?: number;
    /** Maximum context window tokens for the session model. */
    maxContextWindowTokens?: number;
    /** Maximum output tokens for the session model. */
    maxOutputTokens?: number;
    /** Capability overrides for the session model. */
    capabilities?: ModelCapabilitiesOverride;
}

/**
 * Configuration for a Solidity-focused session.
 *
 * These are the flat (v2.x) session options. The client groups them into the
 * SDK protocol 4 payload right before `session.create` / `session.resume`
 * (`streaming` becomes `events.streaming`, `onPermissionRequest` becomes
 * `permissions.onRequest`, and so on). The grouped forms of the base SDK
 * (`inference`, `permissions`, `github`, ...) are accepted too; explicit
 * grouped members win over flat keys.
 */
export interface SoliditySessionConfig
    extends Pick<
        CreateSessionOptions,
        | "inference"
        | "policy"
        | "experiments"
        | "permissions"
        | "interaction"
        | "events"
        | "agents"
        | "skills"
        | "mcp"
        | "github"
        | "filesystem"
        | "execution"
        | "instructions"
        | "git"
        | "state"
        | "plugins"
        | "extensions"
        | "cloud"
        | "diagnostics"
    > {
    /** Extra Solidity-specific tools to register alongside the built-in ones. */
    solidityTools?: Tool[];
    /** Project root for Hardhat/Foundry detection. Defaults to cwd. */
    projectRoot?: string;
    /** Optional custom session ID (create only). */
    sessionId?: string;
    /** Model to use for this session. */
    model?: string;
    /** Client name included in the User-Agent header. */
    clientName?: string;
    /** Working directory for the session's tool operations. */
    workingDirectory?: string;
    /** Configuration directory override. */
    configDirectory?: string;
    /** Legacy alias of {@link configDirectory}. */
    configDir?: string;
    /** Enable runtime discovery of supported configuration. */
    enableConfigDiscovery?: boolean;
    /** System message configuration. Defaults to the Solidity expert prompt (append mode). */
    systemMessage?: SystemMessageConfig;
    /** Additional tools (sent as `tools.definitions`). */
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    tools?: Tool<any>[];
    /** Tool allowlist (`tools.availableTools`). Required for every session in `"empty"` mode. */
    availableTools?: SessionGroupMember<"tools", "availableTools">;
    /** Tool denylist (`tools.excludedTools`). */
    excludedTools?: SessionGroupMember<"tools", "excludedTools">;
    /** Large tool output handling (`tools.largeOutput`). */
    largeOutput?: SessionGroupMember<"tools", "largeOutput">;
    /** Expose the `manage_schedule` tool (`tools.manageScheduleEnabled`). */
    manageScheduleEnabled?: boolean;
    /** Permission handler (`permissions.onRequest`). Defaults to `approveAll`. */
    onPermissionRequest?: PermissionHandler;
    /** Permission mode applied before the first turn (`permissions.mode`). */
    permissionMode?: SessionGroupMember<"permissions", "mode">;
    /** Slash commands (`interaction.commands`). */
    commands?: SessionGroupMember<"interaction", "commands">;
    /** User input handler (`interaction.onUserInputRequest`). */
    onUserInputRequest?: SessionGroupMember<"interaction", "onUserInputRequest">;
    /** Elicitation handler (`interaction.onElicitationRequest`). */
    onElicitationRequest?: SessionGroupMember<"interaction", "onElicitationRequest">;
    /** Exit-plan-mode handler (`interaction.onExitPlanModeRequest`). */
    onExitPlanModeRequest?: ExitPlanModeHandler;
    /** Alias of {@link onExitPlanModeRequest}. */
    onExitPlanMode?: ExitPlanModeHandler;
    /** Auto-mode-switch handler (`interaction.onAutoModeSwitchRequest`). */
    onAutoModeSwitchRequest?: SessionGroupMember<"interaction", "onAutoModeSwitchRequest">;
    /**
     * Shell authentication handler (`interaction.onShellAuthenticationRequest`).
     * When set, the base client sends `interaction.requestShellAuthentication`
     * and answers the runtime's `shellAuthentication.request`
     * (`{ requestId, program, prompt, inputKind, command, cwd }`) with
     * `{ action: "accept", value }` or `{ action: "cancel" }`; a
     * `shellAuthentication.cancel` aborts the pending handler.
     */
    onShellAuthenticationRequest?: ShellAuthenticationHandler;
    /** Stream assistant deltas (`events.streaming`). */
    streaming?: boolean;
    /** Forward sub-agent streaming deltas (`events.includeSubAgentStreamingEvents`). */
    includeSubAgentStreamingEvents?: boolean;
    /** Event handler registered before `session.create` (`events.onEvent`). */
    onEvent?: SessionEventHandler;
    /** Custom agents (`agents.definitions`). */
    customAgents?: SessionGroupMember<"agents", "definitions">;
    /** Default agent configuration (`agents.default`). */
    defaultAgent?: SessionGroupMember<"agents", "default">;
    /** Custom agent to activate at start (`agents.selected`). */
    agent?: string;
    /** Built-in sub-agents to allow (`agents.includedBuiltinAgents`). */
    includedBuiltinAgents?: readonly string[];
    /** Skill directories (`skills.directories`). */
    skillDirectories?: readonly string[];
    /** Enable skill loading (`skills.enabled`). */
    enableSkills?: boolean;
    /** Built-in skills to include (`skills.includedBuiltinSkills`). */
    includedBuiltinSkills?: readonly string[];
    /** Skills to disable (`skills.disabledSkills`). */
    disabledSkills?: readonly string[];
    /** MCP servers (`mcp.servers`, sent with `mcp.envValueMode = "direct"`). */
    mcpServers?: SessionGroupMember<"mcp", "servers">;
    /** Enable MCP Apps UI passthrough (`mcp.enableApps`). */
    enableMcpApps?: boolean;
    /** MCP OAuth token storage (`mcp.oauthTokenStorage`). */
    mcpOAuthTokenStorage?: "persistent" | "in-memory";
    /** Directories containing instruction files. */
    instructionDirectories?: string[];
    /** Organization custom instructions (`instructions.organizationText`). */
    organizationCustomInstructions?: string;
    /** On-demand instruction discovery (`instructions.onDemandDiscovery`). */
    enableOnDemandInstructionDiscovery?: boolean;
    /** Skip custom instruction sources (`instructions.skipCustomInstructions`). */
    skipCustomInstructions?: boolean;
    /** Enable/disable session telemetry. */
    enableSessionTelemetry?: boolean;
    /** Self-fetch enterprise managed settings (`github.fetchManagedSettings`). */
    enableManagedSettings?: boolean;
    /** Track file changes for rewind (`filesystem.trackFileChanges`). */
    enableFileChangeTracking?: boolean;
    /** Enable repository file hooks (`hooks.enableRepositoryHooks`). */
    enableFileHooks?: boolean;
    /** Ask the agent for `Co-authored-by` trailers (`git.coauthorEnabled`). */
    coauthorEnabled?: boolean;
    /** Enable host git operations (`git.enableHostGitOperations`). */
    enableHostGitOperations?: boolean;
    /** Infinite session configuration (`state.infiniteSessions`). */
    infiniteSessions?: SessionGroupMember<"state", "infiniteSessions">;
    /** Skip embedding retrieval (`state.skipEmbeddingRetrieval`). */
    skipEmbeddingRetrieval?: boolean;
    /** Embedding cache storage (`state.embeddingCacheStorage`). */
    embeddingCacheStorage?: "persistent" | "in-memory";
    /** Cross-session store (`state.enableCrossSessionStore`). */
    enableSessionStore?: boolean;
    /** Plugin directories (`plugins.directories`). */
    pluginDirectories?: readonly string[];
    /** Request the canvas renderer tools (`canvases.requestRenderer`). */
    requestCanvasRenderer?: boolean;
    /** Enable extension management (`extensions.enabled`). */
    requestExtensions?: boolean;
    /** Enable experimental features (`experiments.enableExperimentalMode`). */
    enableExperimentalMode?: boolean;
    /** Host-injected managed settings (`policy.managedSettings`). */
    managedSettings?: SessionGroupMember<"policy", "managedSettings">;
    /** Legacy single BYOK provider (see {@link SolidityProviderConfig}). */
    provider?: SolidityProviderConfig;
    /** Resume only: continue pending tool calls and permission prompts. */
    continuePendingWork?: boolean;
    /** Model capabilities override (`limits` accepts {@link ModelLimitsOverride}). */
    modelCapabilities?: ModelCapabilitiesOverride & { limits?: ModelLimitsOverride };
}

// ---------------------------------------------------------------------------
// Tool parameter shapes (used by pre-built tools)
// ---------------------------------------------------------------------------

/** Parameters for the contract audit tool. */
export interface AuditToolParams {
    /** Solidity source code to audit. */
    sourceCode: string;
    /** Contract name (optional, inferred from source if omitted). */
    contractName?: string;
    /** Compiler version override. */
    solidityVersion?: string;
}

/** Parameters for the gas optimization tool. */
export interface GasToolParams {
    /** Solidity source code to analyze. */
    sourceCode: string;
    /** Contract name (optional). */
    contractName?: string;
}

/** Parameters for the test generation tool. */
export interface TestGenToolParams {
    /** Solidity source code for which to generate tests. */
    sourceCode: string;
    /** Contract name (optional). */
    contractName?: string;
    /** Target framework. Defaults to the client-level setting. */
    framework?: TestFramework;
}

/** Parameters for the security pattern check tool. */
export interface SecurityCheckToolParams {
    /** Solidity source code to check. */
    sourceCode: string;
    /** Specific patterns to check (omit for all). */
    patterns?: SecurityPattern[];
}

/** Well-known security patterns the SDK can check. */
export type SecurityPattern =
    | "reentrancy"
    | "overflow"
    | "access-control"
    | "unchecked-return"
    | "front-running"
    | "oracle-manipulation"
    | "denial-of-service"
    | "timestamp-dependency";

// ---------------------------------------------------------------------------
// Session filesystem configuration
// ---------------------------------------------------------------------------

/** Configuration for the session filesystem provider. */
export interface SessionFsConfig {
    /** Initial working directory. */
    initialCwd: string;
    /** Path for session state persistence. */
    sessionStatePath: string;
    /** Path conventions: "windows" or "posix". */
    conventions: "windows" | "posix";
}

// ---------------------------------------------------------------------------
// MCP server configuration
// ---------------------------------------------------------------------------

/** MCP server connection type. */
export type McpServerType = "stdio" | "http";

/** Configuration for an MCP server. */
export interface McpServerConfig {
    type: McpServerType;
    command?: string;
    args?: string[];
    url?: string;
    env?: Record<string, string>;
    headers?: Record<string, string>;
}

// ---------------------------------------------------------------------------
// Command definition
// ---------------------------------------------------------------------------

/** A command that can be registered with a session. */
export interface SolidityCommandDefinition {
    name: string;
    description: string;
}

// ---------------------------------------------------------------------------
// Extended session options (for explicit access beyond base SDK)
// ---------------------------------------------------------------------------

/** Extended options for Solidity sessions with all feature flags. */
export interface SolidityExtendedSessionConfig
    extends Omit<SoliditySessionConfig, "commands" | "mcpServers" | "modelCapabilities"> {
    /** Idle timeout in seconds — session auto-closes after this inactivity period. */
    idleTimeout?: number;
    /** Excluded tools for this session. */
    excludedTools?: string[];
    /** MCP server configurations. */
    mcpServers?: Record<string, McpServerConfig>;
    /** Model capabilities override. */
    modelCapabilities?: Record<string, unknown>;
    /** Enable automatic config discovery. */
    enableConfigDiscovery?: boolean;
    /** Include sub-agent streaming events. */
    includeSubAgentStreamingEvents?: boolean;
    /** Command definitions. */
    commands?: SolidityCommandDefinition[];
    /** Skill directories. */
    skillDirectories?: string[];
    /** Disabled skills. */
    disabledSkills?: string[];
    /** Per-session auth token. */
    gitHubToken?: string;
    /** Session filesystem config. */
    sessionFs?: SessionFsConfig;
    /** Response format. */
    responseFormat?: "text" | "image" | "json_object";
    /** Handler for elicitation requests. */
    elicitationHandler?: (request: Record<string, unknown>) => Promise<Record<string, unknown>>;
    /** Additional request headers sent with each model request. */
    requestHeaders?: Record<string, string>;
    /** Per-session auth token override. */
    authToken?: string;
    /** Directories containing instruction files. */
    instructionDirectories?: string[];
}

// ---------------------------------------------------------------------------
// Slash command types
// ---------------------------------------------------------------------------

/** Completion type for slash command input. */
export type SlashCommandInputCompletion = "directory";

/** Kind of a slash command. */
export type SlashCommandKind = "builtin" | "client" | "skill";

/** Price category for the model picker. */
export type ModelPickerPriceCategory = "high" | "low" | "medium" | "very_high";

/** Input specification for a slash command. */
export interface SlashCommandInput {
    /** Hint string for the command input. */
    hint: string;
    /** Completion type. */
    completion?: SlashCommandInputCompletion;
}

/** Information about a slash command. */
export interface SlashCommandInfo {
    /** Whether the command can run during agent execution. */
    allowDuringAgentExecution: boolean;
    /** Description of the command. */
    description: string;
    /** Kind of command. */
    kind: SlashCommandKind;
    /** Name of the command. */
    name: string;
    /** Aliases for the command. */
    aliases?: string[];
    /** Whether the command is experimental. */
    experimental?: boolean;
    /** Input specification. */
    input?: SlashCommandInput;
}

// ---------------------------------------------------------------------------
// Command request types
// ---------------------------------------------------------------------------

/** Request to invoke a command. */
export interface CommandsInvokeRequest {
    /** Command name. */
    name: string;
    /** Input string for the command. */
    input?: string;
}

/** Request to list available commands. */
export interface CommandsListRequest {
    /** Include built-in commands. */
    includeBuiltins?: boolean;
    /** Include client commands. */
    includeClientCommands?: boolean;
    /** Include skill commands. */
    includeSkills?: boolean;
}

// ---------------------------------------------------------------------------
// Model billing types
// ---------------------------------------------------------------------------

/** Token prices for model billing. */
export interface ModelBillingTokenPrices {
    /** Batch size for pricing. */
    batchSize?: number;
    /** Cache price. */
    cachePrice?: number;
    /** Input token price. */
    inputPrice?: number;
    /** Output token price. */
    outputPrice?: number;
}

/** Model billing information. */
export interface ModelBilling {
    /** Billing multiplier. */
    multiplier: number;
    /** Token prices. */
    tokenPrices?: ModelBillingTokenPrices;
    /** Picker price category. */
    pickerPriceCategory?: ModelPickerPriceCategory;
}

/** Experimental
 *  Diagnostics from loading skills. */
export interface SkillsLoadDiagnostics {
    /** Error messages. */
    errors: string[];
    /** Warning messages. */
    warnings: string[];
}

/** Experimental
 *  Mode for remote session control. */
export type RemoteSessionMode = "export" | "off" | "on";

/** Experimental
 *  Request to enable or configure remote session mode. */
export interface RemoteEnableRequest {
    /** Remote session mode. */
    mode?: RemoteSessionMode;
}

/** Experimental
 *  Result of enabling remote session mode. */
export interface RemoteEnableResult {
    /** Whether the session is remotely steerable. */
    remoteSteerable: boolean;
    /** URL for the remote session. */
    url?: string;
}

// ---------------------------------------------------------------------------
// Session Event Envelope
// ---------------------------------------------------------------------------

/** Session event with envelope metadata, re-exported from the base SDK. */
export interface SessionEventEnvelope {
    /** Unique event identifier (UUID v4). */
    id: string;
    /** ISO 8601 timestamp when the event was created. */
    timestamp: string;
    /** ID of the preceding event. Null for the first event. */
    parentId: string | null;
    /** Sub-agent instance identifier. Absent for root agent events. */
    agentId?: string;
    /** When true, the event is transient and not persisted. */
    ephemeral?: boolean;
    /** The event type discriminator. */
    type: string;
    /** The event data payload. */
    data: Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// Upstream-sync feature types & constants (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/** Per-session AI-credit budget. Set {@link maxAiCredits} to cap spend. */
export interface SessionLimitsConfig {
    /** Maximum AI credits the session may consume. */
    maxAiCredits?: number;
}

/** Opt-in persistent session memory configuration. */
export interface MemoryConfiguration {
    /** Whether persistent memory is enabled for this session. */
    enabled?: boolean;
    /**
     * Store memories in the app instead of the built-in backends
     * (experimental). The base client sends `state.hasMemoryStore` and answers
     * the runtime's `memoryStore.store` requests
     * (`{ sessionId, subject, fact, citations, reason }`) by awaiting this
     * callback, then replies `null`. Setting it enables memory unless
     * {@link enabled} is set explicitly.
     */
    storeMemory?: (memory: StoredMemory, options: MemoryStoreCallOptions) => Promise<void>;
}

/** Arguments passed to a {@link BearerTokenProvider} callback (per-session scoping). */
export interface ProviderTokenArgs {
    /** The session requesting a token. */
    sessionId?: string;
}

/** BYOK callback resolving a bearer token for outbound provider requests. */
export type BearerTokenProvider = (
    args: ProviderTokenArgs,
) => Promise<string | undefined> | string | undefined;

/** Handler for MCP OAuth host token requests. */
export type McpAuthHandler = (
    request: Record<string, unknown>,
    context: { sessionId?: string },
) => Promise<string | undefined> | string | undefined;

/** Handler fired after a tool execution. */
export type PostToolUseHandler = (
    input: Record<string, unknown>,
    context: { sessionId?: string },
) => Promise<unknown> | unknown;

/** Handler fired before an MCP tool call is dispatched. */
export type PreMcpToolCallHandler = (
    input: Record<string, unknown>,
    context: { sessionId?: string },
) => Promise<unknown> | unknown;

/** Session lifecycle hooks surfaced by the Solidity SDK. */
export interface SolidityHooks {
    /** Invoked after a tool executes. */
    onPostToolUse?: PostToolUseHandler;
    /** Invoked before an MCP tool call is dispatched. */
    onPreMcpToolCall?: PreMcpToolCallHandler;
    /**
     * Invoked before a sub-agent's first turn (hook type `subagentStart`).
     * Return `{ additionalContext }` to prepend context to the child's prompt.
     */
    onSubagentStart?: SubagentStartHandler;
    /**
     * Invoked after a sub-agent completes a turn (hook type `subagentStop`).
     * Return `{ decision: "block", reason }` to continue the child, or
     * `{ modifiedResponse }` to rewrite what the parent sees.
     */
    onSubagentStop?: SubagentStopHandler;
}

/**
 * Intercept outbound LLM inference HTTP/WebSocket requests. Implement
 * {@link sendRequest} to mutate, replace, or forward the request. BYOK
 * providers may also supply a {@link BearerTokenProvider}.
 */
export interface CopilotRequestHandler {
    sendRequest(
        request: unknown,
        context: { sessionId?: string },
    ): Promise<unknown> | unknown;
}

/** Options for sending a message to a Solidity session. */
export interface SolidityMessageOptions {
    /** The prompt text to send. */
    prompt: string;
    /** Optional file/directory attachments. */
    attachments?: unknown[];
    /** Processing mode ("enqueue" or "immediate"). */
    mode?: string;
    /** Agent mode to use for this message. */
    agentMode?: "interactive" | "plan" | "autopilot" | "shell";
    /** Alternate prompt text to display in place of {@link prompt}. */
    displayPrompt?: string;
    /** Extra HTTP headers for this message's model requests. */
    requestHeaders?: Record<string, string>;
    /** Desired response format (legacy; ignored when {@link responseSchema} is set). */
    responseFormat?: "text" | "image" | "json_object";
    /** Message provenance: `"user"`, `"system"`, or `"agent-<id>"` (see {@link MessageSource}). */
    source?: MessageSource;
    /**
     * JSON Schema for a structured response. Sent as
     * `responseFormat: { type: "json_schema", jsonSchema: { name: "response", strict: true, schema } }`.
     */
    responseSchema?: Record<string, unknown>;
    /**
     * Opaque caller-chosen ID for the turn this message starts, sent as the
     * `session.send` param `clientTurnId`. The runtime echoes it on the
     * `user.message` event and stamps it on MCP `tools/call` requests.
     */
    clientTurnId?: string;
}

/** Upstream-sync session options forwarded to the base SDK on session creation. */
export interface SolidityUpstreamSessionOptions {
    /** Enable response citations. */
    enableCitations?: boolean;
    /** Names of built-in agents to exclude. */
    excludedBuiltinAgents?: string[];
    /** Per-session AI-credit spending limits. */
    sessionLimits?: SessionLimitsConfig;
    /** Persistent session memory configuration. */
    memory?: MemoryConfiguration;
    /** OTLP telemetry protocol. */
    otlpProtocol?: "http/json" | "http/protobuf";
    /** Use the WebSocket response transport instead of stdio streaming. */
    enableWebSocketResponses?: boolean;
    /** Experiment assignment overrides. */
    expAssignments?: Record<string, string>;
    /** Handler for MCP OAuth host token requests. */
    onMcpAuthRequest?: McpAuthHandler;
    /** BYOK bearer token provider. */
    bearerTokenProvider?: BearerTokenProvider;
    /** Session lifecycle hooks (incl. onPostToolUse / onPreMcpToolCall). */
    hooks?: SolidityHooks;
    /** Enable conversational rewind (restore an earlier turn). */
    rewindEnabled?: boolean;
    /** Extra workspace directories to expose to the session. */
    additionalDirectories?: string[];
    /** Names of MCP servers to disable for this session. */
    disabledMcpServers?: string[];
    /** GitHub MCP tool configuration (opaque passthrough). */
    githubMcpToolConfig?: Record<string, unknown>;
    /** Canvas provider configuration (opaque passthrough). */
    canvasProvider?: Record<string, unknown>;
    /** Restrict custom agents to locally-defined ones only. */
    customAgentsLocalOnly?: boolean;
    /** Permission-reply decision context (opaque object) forwarded with replies. */
    decisionContext?: Record<string, unknown>;
    /** Hook invoked after a user prompt is transformed (userPromptTransformed). */
    onUserPromptTransformed?: (context: Record<string, unknown>) => unknown | Promise<unknown>;
    /** Built-in plugin directories to load. */
    builtinPluginDirectories?: string[];
    /** Agent-factory authoring arguments schema (opaque object). */
    argsSchema?: Record<string, unknown>;
    /** Reasoning effort control ("low", "medium", "high"). */
    reasoningEffort?: string;
    /** Tool-search configuration (opaque object or boolean toggle). */
    toolSearch?: Record<string, unknown> | boolean;
    /** Use the in-process FFI transport instead of a spawned CLI. */
    inProcess?: boolean;
    /** Enable experimental mode features. */
    experimentalMode?: boolean;
    /** Enable content exclusion policy enforcement. */
    contentExclusion?: boolean;

    // --- v2.6.0 upstream-sync options ---

    /**
     * Auto model routing preference, sent as `capi.autoTier` (experimental).
     * Change it on a live session with `CopilotSolidityClient.setAutoTier`.
     */
    autoTier?: AutoTier;
    /** Ask-user tool variant (`"legacy"` or `"elicitation"`). */
    askUserVariant?: AskUserVariant;
    /**
     * Per-session GitHub token callback answering `gitHubToken.getToken`
     * requests. Mutually exclusive with `gitHubToken`.
     */
    gitHubTokenProvider?: GitHubTokenProvider;
    /** Per-session GitHub token (mutually exclusive with {@link gitHubTokenProvider}). */
    gitHubToken?: string;
    /** Sandbox configuration for tool execution. */
    sandboxConfig?: SandboxConfig;
    /** Runtime feature flags; sent whenever provided, even when empty. */
    featureFlags?: Record<string, boolean>;
    /** Model allowlist for the session. */
    allowedModels?: string[];
    /** OAuth client ID metadata document URL. */
    authClientIdMetadataUrl?: string;
    /** Session name (create only). */
    name?: string;
    /** Custom agent definition directories. */
    agentDirectories?: string[];
    /** Hook definition directories. */
    hookDirectories?: string[];
    /** Whether a human user is interacting with the session. */
    interactiveUser?: boolean;
    /** Managed-settings keys to exclude for this session. */
    excludedManagedSettings?: string[];
    /** Enforce managed model defaults. */
    enforceManagedModelDefaults?: boolean;
    /** Grant working-directory trust at creation: `"session"` or `"user"` (create only). */
    trustWorkingDirectory?: WorkingDirectoryTrust;
    /** Invalidate the custom-instruction discovery cache first (create only). */
    refreshCustomInstructions?: boolean;
    /** Allow the runtime to recover a damaged transcript (resume only). */
    allowTranscriptRecovery?: boolean;
    /** Session-level image generation configuration. */
    imageGeneration?: ImageGenerationOptions;
    /** Autopilot continuation options. */
    autopilot?: AutopilotOptions;

    // --- v3.0.0 (SDK protocol 4) options ---

    /**
     * Named BYOK provider connections (`inference.providers`), referenced by
     * {@link models} entries and by {@link defaultProvider}. Select a
     * registered model with `"<provider>/<id>"`.
     */
    providers?: NamedProviderConfig[];
    /** BYOK model definitions (`inference.models`), each naming a provider. */
    models?: ProviderModelConfig[];
    /**
     * Named connection used for unqualified BYOK model selections
     * (`inference.defaultProvider`). Requires `capi: { enabled: false }`.
     * Pass `null` on resume to clear it.
     */
    defaultProvider?: string | null;
    /**
     * Copilot API (CAPI) options (`github.capi`); `enabled: false` disables
     * Copilot models for BYOK-only sessions. `autoTier` and
     * `enableWebSocketResponses` are merged in.
     */
    capi?: CapiSessionOptions;
    /** Resume only: skip emitting the `session.resume` event (root `suppressResumeEvent`). */
    suppressResumeEvent?: boolean;
    /** Resume only: legacy alias of {@link suppressResumeEvent}. */
    disableResume?: boolean;
}

/** Event type of live shell output chunks (`tool.shell_output`). */
export const TOOL_SHELL_OUTPUT_EVENT = "tool.shell_output" as const;

/** Stream of a {@link ToolShellOutputData} chunk; omitted means stdout. */
export type ToolShellOutputStream = NonNullable<ToolShellOutputData["stream"]>;

/** Known refusal reasons on a {@link PermissionModeSetResult}. */
export const PermissionModeRefusalReason = {
    /** Managed (enterprise) policy refused the requested permission mode. */
    managedPolicy: "managed_policy",
} as const;
export type PermissionModeRefusalReason =
    (typeof PermissionModeRefusalReason)[keyof typeof PermissionModeRefusalReason];

/** Result of `session.permissions.setMode`. */
export interface PermissionModeSetResult {
    /** Whether the requested mode was applied. */
    success: boolean;
    /** Authoritative permission mode after the call. */
    mode: string;
    /**
     * Why the requested mode was refused, when known: currently
     * `"managed_policy"`. Absent on success or when the cause is unknown.
     */
    reason?: PermissionModeRefusalReason | (string & {});
}

/**
 * Error thrown when `session.create` / `session.resume` fails. When the
 * runtime attached a Policy Helper failure to the JSON-RPC error data
 * (`error.data.policyHelperFailure`), it is exposed as
 * {@link policyHelperFailure}.
 */
export interface SessionStartError extends Error {
    /** JSON-RPC error code, when the failure was a JSON-RPC error. */
    code?: number;
    /** JSON-RPC error data, when present. */
    data?: unknown;
    /** Structured Policy Helper failure from `error.data.policyHelperFailure`. */
    policyHelperFailure?: PolicyHelperFailure;
}

/** Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto"). */
export const ToolDefer = {
    auto: "auto",
    never: "never",
} as const;
export type ToolDeferMode = (typeof ToolDefer)[keyof typeof ToolDefer];

/**
 * System-message section identifiers. The `preamble` section targets only the
 * identity preamble; the `preserve` action protects an individually-addressable
 * section from a group-level remove.
 */
export const SystemMessageSection = {
    preamble: "preamble",
    identity: "identity",
    toolInstructions: "tool_instructions",
    preserve: "preserve",
} as const;
export type SystemMessageSectionName =
    (typeof SystemMessageSection)[keyof typeof SystemMessageSection];

/** GitHub-anchored attachment variants. */
export const GitHubAttachment = {
    gitHubCommit: "GitHubCommit",
    gitHubRelease: "GitHubRelease",
    gitHubActionsJob: "GitHubActionsJob",
    gitHubRepository: "GitHubRepository",
    gitHubFileDiff: "GitHubFileDiff",
    gitHubTreeComparison: "GitHubTreeComparison",
    gitHubUrl: "GitHubUrl",
    gitHubFile: "GitHubFile",
    gitHubSnippet: "GitHubSnippet",
} as const;
export type GitHubAttachmentType =
    (typeof GitHubAttachment)[keyof typeof GitHubAttachment];

// ---------------------------------------------------------------------------
// v2.6.0 upstream-sync types & constants (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/** Message provenance for {@link SolidityMessageOptions.source}. */
export type MessageSource = "user" | "system" | `agent-${string}`;

/** Well-known {@link MessageSource} values and an agent-source builder. */
export const MessageSource = {
    user: "user",
    system: "system",
    /** Build the `"agent-<id>"` source for messages from an identified agent. */
    agent: (agentId: string): MessageSource => `agent-${agentId}`,
} as const;

/** Sandbox configuration (`sandboxConfig`) for tool execution. */
export interface SandboxConfig {
    /** Whether the sandbox is enabled. */
    enabled: boolean;
    /** User sandbox policy (filesystem / network rules), passed through as-is. */
    userPolicy?: Record<string, unknown>;
    /** Grant the current working directory to the sandbox. */
    addCurrentWorkingDirectory?: boolean;
    /** Run MCP servers inside the sandbox. */
    sandboxMcpServers?: boolean;
    /** Run LSP servers inside the sandbox. */
    sandboxLspServers?: boolean;
    /** Run extensions inside the sandbox. */
    sandboxExtensions?: boolean;
    /**
     * Opt in to letting the agent request an individual command outside the
     * sandbox through the normal permission flow.
     */
    allowBypass?: boolean;
}

/** Session-level image generation configuration (`imageGeneration`). */
export interface ImageGenerationOptions {
    /** Opt in to image generation. */
    enabled?: boolean;
    /** Request a visible "Made with AI" label on generated images. */
    watermark?: boolean;
    /** Save generated images under the session-state `images` folder. */
    saveToDisk?: boolean;
}

/** Autopilot continuation options (`autopilot`). */
export interface AutopilotOptions {
    /** Whether autopilot may continue automatically after a turn ends. */
    continuationEnabled?: boolean;
    /** Maximum number of automatic continuations. */
    maxContinues?: number;
}

/**
 * Model capability limits override (`modelCapabilities.limits`). Wire keys
 * are snake_case; `max_output_tokens` caps the tokens generated per response.
 */
export interface ModelLimitsOverride {
    max_prompt_tokens?: number;
    max_context_window_tokens?: number;
    max_output_tokens?: number;
}

/** BYOK provider fields passed through on the session `provider` config. */
export interface ByokProviderOptions {
    /** Product serving the model (`"openai"`, `"ollama"`, `"lm_studio"`, ...). */
    modelProvider?: ProviderConfigModelProvider;
    /** Command whose stdout is used as the API key for each provider request. */
    apiKeyCommand?: string;
}

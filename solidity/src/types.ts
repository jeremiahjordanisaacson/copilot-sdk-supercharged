/**
 * Type definitions for the Copilot Solidity SDK
 *
 * Provides types for smart contract analysis, auditing, gas optimization,
 * and integration with Hardhat/Foundry workflows.
 */

import type {
    CopilotClientOptions,
    SessionConfig,
    Tool,
} from "copilot-sdk-supercharged";

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

/** Options specific to the Solidity SDK client, extending the base options. */
export interface CopilotSolidityClientOptions extends CopilotClientOptions {
    /** Default Solidity compiler version (e.g. "0.8.24"). */
    solidityVersion?: string;
    /** Default test framework when generating tests. */
    defaultTestFramework?: TestFramework;
    /** Whether to include gas optimization hints in every audit. */
    includeGasHints?: boolean;
    /** Path to the Copilot home directory. */
    copilotHome?: string;
    /** TCP connection token for authenticated server connections. */
    tcpConnectionToken?: string;
}

/** Configuration for a Solidity-focused session. */
export interface SoliditySessionConfig extends SessionConfig {
    /** Extra Solidity-specific tools to register alongside the built-in ones. */
    solidityTools?: Tool[];
    /** Project root for Hardhat/Foundry detection. Defaults to cwd. */
    projectRoot?: string;
    /** Directories containing instruction files. */
    instructionDirectories?: string[];
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
export interface SolidityExtendedSessionConfig extends SoliditySessionConfig {
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
    /** Session idle timeout in seconds. */
    idleTimeout?: number;
    /** Per-session auth token override. */
    authToken?: string;
    /** Directories containing instruction files. */
    instructionDirectories?: string[];
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
    /** Desired response format. */
    responseFormat?: "text" | "image" | "json_object";
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

/**
 * CopilotSolidityClient - A wrapper around the base CopilotClient tailored
 * for Solidity / smart-contract development workflows.
 *
 * Provides high-level helpers for:
 * - Contract security auditing
 * - Gas optimization analysis
 * - Test generation (Hardhat & Foundry)
 * - Security pattern checking
 *
 * Under the hood, the client registers Solidity-specific tools and crafts
 * system prompts that leverage Copilot's knowledge of the EVM.
 */

import {
    CopilotClient,
    RuntimeConnection,
    approveAll,
    type CopilotClientMode,
    type CopilotClientOptions,
    type CopilotSession,
    type CreateSessionOptions,
    type ManagedSettingsResolvedData,
    type ResumeSessionOptions,
    type Tool,
    type ToolShellOutputData,
} from "copilot-sdk-supercharged";

import {
    createAuditTool,
    createGasOptimizationTool,
    createTestGenerationTool,
    createSecurityCheckTool,
    createSolidityTools,
} from "./tools.js";

import { assertSdkProtocolVersion } from "./sdkProtocolVersion.js";
import { applyEmptyModeSessionDefaults, groupSessionParams } from "./sessionParams.js";

import {
    TOOL_SHELL_OUTPUT_EVENT,
    type AuditReport,
    type AutoTier,
    type CopilotSolidityClientOptions,
    type GasReport,
    type GeneratedTestSuite,
    type GitHubTokenProvider,
    type ModelSwitchAutoTierResult,
    type PermissionModeSetResult,
    type PolicyHelperFailure,
    type SessionStartError,
    type SolidityClientMode,
    type SoliditySessionConfig,
    type SolidityMessageOptions,
    type SolidityUpstreamSessionOptions,
    type SessionEventEnvelope,
    type TestFramework,
} from "./types.js";

/** Session options accepted by {@link CopilotSolidityClient.createSession} and `resumeSession`. */
export type SoliditySessionOptions = Partial<SoliditySessionConfig> & SolidityUpstreamSessionOptions;

/** Minimal JSON-RPC connection surface used for methods without a typed wrapper. */
interface RawRpcConnection {
    sendRequest(method: string, params?: unknown): Promise<unknown>;
}

// ---------------------------------------------------------------------------
// Solidity system prompt
// ---------------------------------------------------------------------------

const SOLIDITY_SYSTEM_PROMPT = [
    "You are an expert Solidity and EVM developer.",
    "You have deep knowledge of smart-contract security (SWC registry, DASP Top 10),",
    "gas optimization techniques, and the Hardhat / Foundry toolchains.",
    "When analyzing contracts, always consider reentrancy, access control, integer issues,",
    "and other common vulnerability classes.",
    "Provide concrete, actionable advice with code examples when possible.",
].join(" ");

// ---------------------------------------------------------------------------
// Client
// ---------------------------------------------------------------------------

export class CopilotSolidityClient {
    /** The underlying base SDK client. */
    public readonly client: CopilotClient;

    /** Effective client mode (`"coding-agent"` unless `"empty"` was requested). */
    public readonly mode: CopilotClientMode;

    private readonly solidityVersion?: string;
    private readonly defaultTestFramework: TestFramework;
    private readonly includeGasHints: boolean;

    constructor(options: CopilotSolidityClientOptions = {}) {
        const {
            solidityVersion,
            defaultTestFramework = "hardhat",
            includeGasHints = false,
            ...baseOptions
        } = options;

        this.mode = normalizeClientMode(baseOptions.mode);
        this.client = new CopilotClient(toBaseClientOptions(baseOptions, this.mode));
        this.solidityVersion = solidityVersion;
        this.defaultTestFramework = defaultTestFramework;
        this.includeGasHints = includeGasHints;
    }

    // -----------------------------------------------------------------------
    // Lifecycle
    // -----------------------------------------------------------------------

    /**
     * Start the underlying Copilot CLI server and verify that it speaks SDK
     * protocol 4 (Copilot CLI 1.0.113 or newer). The base client sends
     * `connect` (falling back to `ping` on older runtimes); this client then
     * pings and rejects any other protocol version.
     */
    async start(): Promise<void> {
        await this.client.start();
        try {
            const { protocolVersion } = await this.client.ping();
            assertSdkProtocolVersion(protocolVersion);
        } catch (error) {
            await this.client.stop().catch(() => undefined);
            throw error;
        }
    }

    /** Stop the underlying Copilot CLI server. */
    async stop(): Promise<void> {
        await this.client.stop();
    }

    /**
     * Force-refresh enterprise managed settings for every account
     * (`managedSettings.clearCache`). The next session resolution re-fetches
     * the account's org policy instead of serving a cached response.
     */
    async clearManagedSettingsCache(): Promise<void> {
        await this.client.rpc.managedSettings.clearCache();
    }

    // -----------------------------------------------------------------------
    // Foreground session management
    // -----------------------------------------------------------------------

    /** Get the foreground session ID from the CLI server (`session.getForeground`). */
    async getForegroundSessionId(): Promise<string> {
        return (await this.client.getForegroundSessionId()) ?? "";
    }

    /**
     * Set the foreground session ID on the CLI server (`session.setForeground`).
     * JSON-RPC errors and `success: false` replies both reject.
     */
    async setForegroundSessionId(sessionId: string): Promise<void> {
        await this.client.setForegroundSessionId(sessionId);
    }

    /** Get the last session ID (`session.getLastId`), or null if none. */
    async getLastSessionId(): Promise<string | null> {
        return (await this.client.getLastSessionId()) ?? null;
    }

    /** Get metadata for a session by ID (`session.getMetadata`). */
    async getSessionMetadata(sessionId: string): Promise<unknown> {
        return this.client.getSessionMetadata(sessionId);
    }

    /** List available models (`models.list`). */
    async listModels(): Promise<unknown[]> {
        return this.client.listModels();
    }

    /** Ping the server (`ping`). `timestamp` is an ISO 8601 string. */
    async ping(
        message?: string,
    ): Promise<{ message: string; timestamp: string; protocolVersion?: number }> {
        return this.client.ping(message ?? "ping");
    }

    /** Get server status (`status.get`). */
    async getStatus(): Promise<unknown> {
        return this.client.getStatus();
    }

    /** Get authentication status (`auth.getStatus`). */
    async getAuthStatus(): Promise<unknown> {
        return this.client.getAuthStatus();
    }

    /**
     * Resume a previously created session by ID.
     *
     * Without `config`, issues the raw "session.resume" RPC. With `config`, the
     * session is resumed through the base SDK with the Solidity tools and the
     * resume options are forwarded (autoTier, askUserVariant, sandboxConfig,
     * featureFlags, allowedModels, allowTranscriptRecovery, gitHubTokenProvider,
     * suppressResumeEvent, ...), grouped for SDK protocol 4.
     */
    async resumeSession(sessionId: string): Promise<unknown>;
    async resumeSession(sessionId: string, config: SoliditySessionOptions): Promise<CopilotSession>;
    async resumeSession(sessionId: string, config?: SoliditySessionOptions): Promise<unknown> {
        if (config === undefined) {
            return this.sendRawSessionRequest("session.resume", { sessionId }, true);
        }
        const resumeConfig = groupSessionParams(
            this.buildFlatSessionConfig(config, "resume"),
            true,
        ) as ResumeSessionOptions;
        return withPolicyHelperFailure(() => this.client.resumeSession(sessionId, resumeConfig));
    }

    /** Delete a session by ID (`session.delete`). */
    async deleteSession(sessionId: string): Promise<void> {
        await this.client.deleteSession(sessionId);
    }

    /**
     * Release a session by ID without deleting its on-disk state, so it can be
     * resumed later. Sends "session.detach" and falls back to the legacy
     * "session.destroy" when the runtime predates it (JSON-RPC -32601).
     */
    async detachSession(sessionId: string): Promise<void> {
        const connection = this.rawConnection();
        try {
            await connection.sendRequest("session.detach", { sessionId });
        } catch (error) {
            if (!isMethodNotFound(error)) {
                throw error;
            }
            await connection.sendRequest("session.destroy", { sessionId });
        }
    }

    /** List all known sessions (`session.list`). */
    async listSessions(): Promise<unknown[]> {
        return this.client.listSessions();
    }

    // -----------------------------------------------------------------------
    // Session filesystem
    // -----------------------------------------------------------------------

    /** Register a session filesystem provider with the CLI server. */
    async setSessionFsProvider(config: {
        initialCwd: string;
        sessionStatePath: string;
        conventions: "windows" | "posix";
    }): Promise<unknown> {
        return this.rawConnection().sendRequest("sessionFs.setProvider", config);
    }

    // -----------------------------------------------------------------------
    // Session helpers
    // -----------------------------------------------------------------------

    /**
     * Create a raw session via the "session.create" RPC (without Solidity tools).
     * Flat wire params are grouped for SDK protocol 4 first. For a session
     * pre-loaded with Solidity tools use {@link createSession}.
     */
    async createRawSession(params: Record<string, unknown> = {}): Promise<unknown> {
        return this.sendRawSessionRequest("session.create", params, false);
    }

    /**
     * Create a session pre-configured with Solidity tools and system prompt.
     *
     * Any extra tools or config from `config` are merged on top, then grouped
     * for SDK protocol 4.
     *
     * @throws {SessionStartError} when `session.create` fails; a Policy Helper
     *     failure from the runtime is exposed as `error.policyHelperFailure`
     */
    async createSession(
        config: SoliditySessionOptions = {},
    ): Promise<CopilotSession> {
        const sessionConfig = groupSessionParams(
            this.buildFlatSessionConfig(config, "create"),
            false,
        ) as CreateSessionOptions;
        return withPolicyHelperFailure(() => this.client.createSession(sessionConfig));
    }

    /**
     * The flat session payload for {@link createSession} / {@link resumeSession}:
     * the caller's options plus the Solidity tools, the default permission
     * handler and system prompt, and the derived upstream options.
     */
    private buildFlatSessionConfig(
        config: SoliditySessionOptions,
        mode: "create" | "resume",
    ): Record<string, unknown> {
        assertGitHubTokenOptions(config);
        const { solidityTools: _solidityTools, ...options } = config;
        const flat: Record<string, unknown> = {
            ...options,
            tools: this.buildSessionTools(config),
            onPermissionRequest: config.onPermissionRequest ?? approveAll,
            systemMessage: config.systemMessage ?? {
                mode: "append" as const,
                content: SOLIDITY_SYSTEM_PROMPT,
            },
            ...this.buildUpstreamSessionOptions(config, mode),
        };
        return flat;
    }

    /** Send a raw `session.create` / `session.resume` with a grouped payload. */
    private async sendRawSessionRequest(
        method: "session.create" | "session.resume",
        params: Record<string, unknown>,
        isResume: boolean,
    ): Promise<unknown> {
        const grouped = groupSessionParams(params, isResume);
        if (this.mode === "empty") {
            applyEmptyModeSessionDefaults(grouped);
        }
        return withPolicyHelperFailure(() => this.rawConnection().sendRequest(method, grouped));
    }

    /** Built-in Solidity tools followed by any caller-supplied tools. */
    private buildSessionTools(config: SoliditySessionOptions): Tool[] {
        const builtInTools = createSolidityTools({
            solidityVersion: this.solidityVersion,
            defaultTestFramework: this.defaultTestFramework,
        });

        return [
            ...builtInTools,
            ...(config.solidityTools ?? []),
            ...(config.tools ?? []),
        ];
    }

    /**
     * Build the upstream-sync session options forwarded to the base SDK:
     * citations, excluded built-in agents, spending limits, session memory,
     * OTLP telemetry protocol, CAPI options (autoTier / WebSocket transport),
     * experiment assignments, and the v2.6.0 / v3.0.0 create/resume options.
     * The result is still flat; {@link groupSessionParams} groups it.
     */
    private buildUpstreamSessionOptions(
        options: SolidityUpstreamSessionOptions & { capi?: object },
        mode: "create" | "resume" = "create",
    ): Record<string, unknown> {
        const params: Record<string, unknown> = {};
        if (options.enableCitations !== undefined)
            params.enableCitations = options.enableCitations;
        if (options.excludedBuiltinAgents !== undefined)
            params.excludedBuiltinAgents = options.excludedBuiltinAgents;
        if (options.sessionLimits !== undefined)
            params.sessionLimits = options.sessionLimits;
        if (options.memory !== undefined) {
            // An app-side memory store needs memory enabled (memoryStore.store).
            params.memory =
                options.memory.storeMemory !== undefined && options.memory.enabled === undefined
                    ? { ...options.memory, enabled: true }
                    : options.memory;
        }
        if (options.otlpProtocol !== undefined)
            params.otlpProtocol = options.otlpProtocol;
        // autoTier and enableWebSocketResponses travel under `capi`; the runtime
        // schema only accepts enableWebSocketResponses there.
        const capi: Record<string, unknown> = { ...(options.capi ?? {}) };
        if (options.autoTier !== undefined) capi.autoTier = options.autoTier;
        if (options.enableWebSocketResponses !== undefined)
            capi.enableWebSocketResponses = options.enableWebSocketResponses;
        if (Object.keys(capi).length > 0) params.capi = capi;
        if (options.expAssignments !== undefined)
            params.expAssignments = options.expAssignments;
        if (options.bearerTokenProvider !== undefined)
            params.bearerTokenProvider = options.bearerTokenProvider;
        if (options.rewindEnabled !== undefined)
            params.rewindEnabled = options.rewindEnabled;
        if (options.additionalDirectories !== undefined)
            params.additionalDirectories = options.additionalDirectories;
        if (options.disabledMcpServers !== undefined)
            params.disabledMcpServers = options.disabledMcpServers;
        if (options.githubMcpToolConfig !== undefined)
            params.githubMcpToolConfig = options.githubMcpToolConfig;
        if (options.canvasProvider !== undefined)
            params.canvasProvider = options.canvasProvider;
        if (options.customAgentsLocalOnly !== undefined)
            params.customAgentsLocalOnly = options.customAgentsLocalOnly;
        if (options.decisionContext !== undefined)
            params.decisionContext = options.decisionContext;
        // The user-prompt-transformed callback joins the hook handlers.
        if (options.onUserPromptTransformed !== undefined)
            params.hooks = { onUserPromptTransformed: options.onUserPromptTransformed, ...options.hooks };
        if (options.builtinPluginDirectories !== undefined)
            params.builtinPluginDirectories = options.builtinPluginDirectories;
        if (options.argsSchema !== undefined)
            params.argsSchema = options.argsSchema;
        if (options.reasoningEffort !== undefined)
            params.reasoningEffort = options.reasoningEffort;
        if (options.toolSearch !== undefined)
            params.toolSearch = options.toolSearch;
        if (options.inProcess !== undefined)
            params.inProcess = options.inProcess;
        if (options.experimentalMode !== undefined)
            params.enableExperimentalMode = options.experimentalMode;
        if (options.contentExclusion !== undefined)
            params.contentExclusion = options.contentExclusion;
        // --- v2.6.0 create/resume options ---
        if (options.askUserVariant !== undefined)
            params.askUserVariant = options.askUserVariant;
        if (options.gitHubTokenProvider !== undefined)
            params.gitHubTokenProvider = wrapGitHubTokenProvider(options.gitHubTokenProvider);
        if (options.sandboxConfig !== undefined)
            params.sandboxConfig = options.sandboxConfig;
        if (options.featureFlags !== undefined)
            params.featureFlags = options.featureFlags;
        if (options.allowedModels !== undefined)
            params.allowedModels = options.allowedModels;
        if (options.authClientIdMetadataUrl !== undefined)
            params.authClientIdMetadataUrl = options.authClientIdMetadataUrl;
        if (options.agentDirectories !== undefined)
            params.agentDirectories = options.agentDirectories;
        if (options.hookDirectories !== undefined)
            params.hookDirectories = options.hookDirectories;
        if (options.interactiveUser !== undefined)
            params.interactiveUser = options.interactiveUser;
        if (options.excludedManagedSettings !== undefined)
            params.excludedManagedSettings = options.excludedManagedSettings;
        if (options.enforceManagedModelDefaults !== undefined)
            params.enforceManagedModelDefaults = options.enforceManagedModelDefaults;
        if (options.imageGeneration !== undefined)
            params.imageGeneration = options.imageGeneration;
        if (options.autopilot !== undefined) params.autopilot = options.autopilot;
        // --- v3.0.0 named BYOK providers ---
        if (options.providers !== undefined) params.providers = options.providers;
        if (options.models !== undefined) params.models = options.models;
        if (options.defaultProvider !== undefined)
            params.defaultProvider = options.defaultProvider;
        if (mode === "create") {
            if (options.name !== undefined) params.name = options.name;
            if (options.trustWorkingDirectory !== undefined)
                params.trustWorkingDirectory = options.trustWorkingDirectory;
            if (options.refreshCustomInstructions !== undefined)
                params.refreshCustomInstructions = options.refreshCustomInstructions;
        } else {
            if (options.allowTranscriptRecovery !== undefined)
                params.allowTranscriptRecovery = options.allowTranscriptRecovery;
            const suppressResumeEvent = options.suppressResumeEvent ?? options.disableResume;
            if (suppressResumeEvent !== undefined) params.suppressResumeEvent = suppressResumeEvent;
        }
        return params;
    }

    /**
     * Send a message to a session, forwarding upstream message options
     * (agent mode, display prompt, per-message request headers, message
     * source, structured-output schema, and client turn ID).
     */
    async sendMessage(
        session: CopilotSession,
        options: SolidityMessageOptions,
        timeout?: number,
    ): Promise<SessionEventEnvelope | undefined> {
        const sendOptions: Record<string, unknown> = { prompt: options.prompt };
        if (options.attachments !== undefined)
            sendOptions.attachments = options.attachments;
        if (options.mode !== undefined) sendOptions.mode = options.mode;
        if (options.agentMode !== undefined) sendOptions.agentMode = options.agentMode;
        if (options.displayPrompt !== undefined)
            sendOptions.displayPrompt = options.displayPrompt;
        if (options.requestHeaders !== undefined)
            sendOptions.requestHeaders = options.requestHeaders;
        if (options.source !== undefined) sendOptions.source = options.source;
        if (options.clientTurnId !== undefined) sendOptions.clientTurnId = options.clientTurnId;
        // A JSON Schema takes over `responseFormat` (json_schema) from the legacy string format.
        if (options.responseSchema !== undefined)
            sendOptions.responseSchema = options.responseSchema;
        else if (options.responseFormat !== undefined)
            sendOptions.responseFormat = options.responseFormat;
        const response = await session.sendAndWait(
            sendOptions as unknown as Parameters<CopilotSession["sendAndWait"]>[0],
            timeout,
        );
        return response as SessionEventEnvelope | undefined;
    }

    /**
     * Send a message with a JSON Schema for a structured response, wait for the
     * turn to finish, and return the final assistant content parsed as JSON.
     *
     * @throws Error if the run completes without assistant content
     */
    async sendAndWaitStructured<T = unknown>(
        session: CopilotSession,
        promptOrOptions: string | SolidityMessageOptions,
        responseSchema: Record<string, unknown>,
        timeout?: number,
    ): Promise<T> {
        const options: SolidityMessageOptions =
            typeof promptOrOptions === "string" ? { prompt: promptOrOptions } : promptOrOptions;
        const response = await this.sendMessage(session, { ...options, responseSchema }, timeout);
        const content = response?.data?.content;
        if (typeof content !== "string" || content.trim() === "") {
            throw new Error("The requested run completed without a structured assistant response.");
        }
        return JSON.parse(content) as T;
    }

    /**
     * Change a session's Auto routing preference without changing its model
     * (`session.model.switchAutoTier`). Pass `null` to return to the provider's
     * default Auto routing. Experimental.
     */
    async setAutoTier(
        session: CopilotSession,
        autoTier: AutoTier | null,
    ): Promise<ModelSwitchAutoTierResult> {
        return session.rpc.model.switchAutoTier({ autoTier });
    }

    /**
     * Return a session's effective managed settings (`session.managedSettings.get`).
     * An unresolved Policy Helper failure is reported as `policyHelperFailure`.
     */
    async getManagedSettings(session: CopilotSession): Promise<ManagedSettingsResolvedData> {
        return session.rpc.managedSettings.get();
    }

    /**
     * Re-run the managed-settings policy helper for a session
     * (`session.managedSettings.recheckPolicyHelper`) and return the snapshot,
     * including `policyHelperFailure` when the helper still fails.
     */
    async recheckPolicyHelper(session: CopilotSession): Promise<ManagedSettingsResolvedData> {
        return session.rpc.managedSettings.recheckPolicyHelper();
    }

    /**
     * Set a session's permission mode (`session.permissions.setMode`). When the
     * mode is refused, `success` is false and `reason` explains why when known
     * (`"managed_policy"`: see `PermissionModeRefusalReason.managedPolicy`).
     */
    async setPermissionMode(
        session: CopilotSession,
        mode: Parameters<CopilotSession["rpc"]["permissions"]["setMode"]>[0]["mode"],
    ): Promise<PermissionModeSetResult> {
        return session.rpc.permissions.setMode({ mode });
    }

    /**
     * Subscribe to live shell output (`tool.shell_output` events: `toolCallId`,
     * `text`, `sequence`, and `stream` of `"stdout"` / `"stderr"`, omitted for
     * stdout). Chunks are ephemeral and never replayed. Returns an unsubscribe
     * function.
     */
    onShellOutput(
        session: CopilotSession,
        handler: (output: ToolShellOutputData) => void,
    ): () => void {
        return session.on(TOOL_SHELL_OUTPUT_EVENT, (event) => handler(event.data));
    }

    /** Session options for the one-shot helpers (only the Solidity tools in `"empty"` mode). */
    private helperSessionOptions(): SoliditySessionOptions {
        if (this.mode !== "empty") {
            return {};
        }
        return { availableTools: this.buildSessionTools({}).map((tool) => tool.name) };
    }

    // -----------------------------------------------------------------------
    // High-level convenience methods
    // -----------------------------------------------------------------------

    /**
     * Audit a Solidity contract and return a structured report.
     *
     * Creates a one-shot session, sends the source code with the audit tool
     * prompt, and parses the response.
     */
    async auditContract(
        sourceCode: string,
        contractName?: string,
    ): Promise<AuditReport> {
        const session = await this.createSession(this.helperSessionOptions());
        try {
            const prompt = buildAuditPrompt(sourceCode, contractName, this.includeGasHints);
            const response = await session.sendAndWait({ prompt });
            const raw = response?.data?.content ?? "";
            return parseJsonResponse<AuditReport>(raw, {
                contractName: contractName ?? "Unknown",
                filePath: "",
                findings: [],
                summary: raw,
                timestamp: new Date().toISOString(),
            });
        } finally {
            await this.releaseSession(session);
        }
    }

    /**
     * Analyze a contract for gas optimizations.
     */
    async optimizeGas(
        sourceCode: string,
        contractName?: string,
    ): Promise<GasReport> {
        const session = await this.createSession(this.helperSessionOptions());
        try {
            const prompt = buildGasPrompt(sourceCode, contractName);
            const response = await session.sendAndWait({ prompt });
            const raw = response?.data?.content ?? "";
            return parseJsonResponse<GasReport>(raw, {
                contractName: contractName ?? "Unknown",
                filePath: "",
                optimizations: [],
                summary: raw,
                timestamp: new Date().toISOString(),
            });
        } finally {
            await this.releaseSession(session);
        }
    }

    /**
     * Generate a test suite for a Solidity contract.
     */
    async generateTests(
        sourceCode: string,
        options: { contractName?: string; framework?: TestFramework } = {},
    ): Promise<GeneratedTestSuite> {
        const fw = options.framework ?? this.defaultTestFramework;
        const session = await this.createSession(this.helperSessionOptions());
        try {
            const prompt = buildTestGenPrompt(sourceCode, fw, options.contractName);
            const response = await session.sendAndWait({ prompt });
            const raw = response?.data?.content ?? "";
            return parseJsonResponse<GeneratedTestSuite>(raw, {
                contractName: options.contractName ?? "Unknown",
                framework: fw,
                code: raw,
                testCount: 0,
                summary: raw,
            });
        } finally {
            await this.releaseSession(session);
        }
    }

    /**
     * Perform a targeted security-pattern check.
     */
    async checkSecurityPatterns(
        sourceCode: string,
        patterns?: string[],
    ): Promise<unknown[]> {
        const session = await this.createSession(this.helperSessionOptions());
        try {
            const patternsNote = patterns?.length
                ? `Focus on: ${patterns.join(", ")}.`
                : "Check all common vulnerability patterns.";
            const prompt = [
                "Analyze the following Solidity code for security anti-patterns.",
                patternsNote,
                "Return a JSON array of findings.",
                "```solidity",
                sourceCode,
                "```",
            ].join("\n");

            const response = await session.sendAndWait({ prompt });
            const raw = response?.data?.content ?? "";
            return parseJsonResponse<unknown[]>(raw, []);
        } finally {
            await this.releaseSession(session);
        }
    }

    // -----------------------------------------------------------------------
    // Session release (session.detach with legacy fallback)
    // -----------------------------------------------------------------------

    /**
     * Release a helper session: the base SDK's `disconnect()` sends
     * "session.detach"; runtimes without it (-32601) get "session.destroy".
     */
    private async releaseSession(session: CopilotSession): Promise<void> {
        try {
            await session.disconnect();
        } catch (error) {
            if (!isMethodNotFound(error)) {
                throw error;
            }
            await this.rawConnection().sendRequest("session.destroy", {
                sessionId: session.sessionId,
            });
        }
    }

    /** The base client's JSON-RPC connection (for methods without a typed wrapper). */
    private rawConnection(): RawRpcConnection {
        const connection = (this.client as unknown as { connection?: RawRpcConnection })
            .connection;
        if (!connection) {
            throw new Error("Client is not connected. Call start() first.");
        }
        return connection;
    }

    // -----------------------------------------------------------------------
    // Tool accessors (for manual session composition)
    // -----------------------------------------------------------------------

    /** Get the audit tool configured with the client-level Solidity version. */
    getAuditTool(): Tool<unknown> {
        return createAuditTool(this.solidityVersion) as Tool<unknown>;
    }

    /** Get the gas optimization tool. */
    getGasOptimizationTool(): Tool<unknown> {
        return createGasOptimizationTool() as Tool<unknown>;
    }

    /** Get the test generation tool configured with the default framework. */
    getTestGenerationTool(): Tool<unknown> {
        return createTestGenerationTool(this.defaultTestFramework) as Tool<unknown>;
    }

    /** Get the security check tool. */
    getSecurityCheckTool(): Tool<unknown> {
        return createSecurityCheckTool() as Tool<unknown>;
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/** Resolve the client mode; `"copilot-cli"` is a legacy alias of `"coding-agent"`. */
function normalizeClientMode(mode: SolidityClientMode | undefined): CopilotClientMode {
    if (mode === undefined || mode === "coding-agent" || mode === "copilot-cli") {
        return "coding-agent";
    }
    if (mode === "empty") {
        return "empty";
    }
    throw new Error(`Unsupported client mode '${String(mode)}'; use "coding-agent" or "empty"`);
}

/** Solidity client options handed to the base SDK (the Solidity-only fields removed). */
type SolidityBaseOptions = Omit<
    CopilotSolidityClientOptions,
    "solidityVersion" | "defaultTestFramework" | "includeGasHints"
>;

/**
 * Map the Solidity client options onto the base `CopilotClient` options:
 * the resolved `mode`, and (unless `runtime` is given) a runtime connection
 * built from the legacy `cliUrl` / `cliPath` / `cliArgs` / `cwd` / `env` /
 * `port` / `useStdio` / `copilotHome` options. Like v2.x, the default spawns
 * the Copilot CLI over stdio.
 */
function toBaseClientOptions(
    options: SolidityBaseOptions,
    mode: CopilotClientMode,
): CopilotClientOptions {
    const {
        mode: _mode,
        cliPath,
        cliArgs,
        cliUrl,
        cwd,
        env,
        port,
        useStdio,
        copilotHome,
        tcpConnectionToken,
        remote: _remote,
        workingDirectory,
        ...base
    } = options as SolidityBaseOptions & { workingDirectory?: string };

    let runtime = base.runtime;
    if (runtime === undefined && !("connection" in base)) {
        runtime = defaultRuntimeConnection({
            cliPath,
            cliArgs,
            cliUrl,
            cwd: cwd ?? workingDirectory,
            env,
            port,
            useStdio,
            copilotHome,
            tcpConnectionToken,
        });
    }
    return { ...base, mode, ...(runtime ? { runtime } : {}) };
}

/** Runtime connection for the legacy client options (spawned stdio by default). */
function defaultRuntimeConnection(options: {
    cliPath?: string;
    cliArgs?: string[];
    cliUrl?: string;
    cwd?: string;
    env?: Record<string, string | undefined>;
    port?: number;
    useStdio?: boolean;
    copilotHome?: string;
    tcpConnectionToken?: string;
}): RuntimeConnection {
    if (options.cliUrl !== undefined) {
        const { host, port } = parseCliUrl(options.cliUrl);
        return RuntimeConnection.forTcp(host, port, { connectionToken: options.tcpConnectionToken });
    }
    const spawn = {
        path: options.cliPath,
        args: options.cliArgs,
        env: options.env,
        workingDirectory: options.cwd,
        baseDirectory: options.copilotHome,
    };
    if (options.useStdio === false || options.port !== undefined) {
        return RuntimeConnection.forSpawnedTcp({
            ...spawn,
            port: options.port,
            connectionToken: options.tcpConnectionToken,
        });
    }
    const spawnOptionsGiven =
        options.cliPath !== undefined ||
        options.cliArgs !== undefined ||
        options.env !== undefined ||
        options.cwd !== undefined;
    if (!spawnOptionsGiven && readEnv("COPILOT_SDK_DEFAULT_CONNECTION")) {
        // An explicit deployment default (stdio or inprocess) wins over the spawned default.
        return RuntimeConnection.forDefault({ baseDirectory: options.copilotHome });
    }
    return RuntimeConnection.forSpawnedStdio(spawn);
}

/** Parse a `cliUrl` (`"host:port"`, `"http://host:port"`, or a bare port). */
function parseCliUrl(cliUrl: string): { host: string; port: number } {
    const trimmed = cliUrl.trim();
    const match = /^(?:[a-z][a-z0-9+.-]*:\/\/)?(?:\[([^\]]+)\]|([^:/?#]+))?(?::(\d+))?\/?$/i.exec(trimmed);
    const bare = /^\d+$/.test(trimmed);
    const host = bare ? "localhost" : (match?.[1] ?? match?.[2]);
    const port = Number(bare ? trimmed : match?.[3]);
    if (!host || !Number.isInteger(port) || port < 1 || port > 65535) {
        throw new Error(`Invalid cliUrl '${cliUrl}'; expected "host:port", "http://host:port", or a port`);
    }
    return { host, port };
}

/** Read an environment variable without requiring Node type definitions. */
function readEnv(name: string): string | undefined {
    const env = (globalThis as { process?: { env?: Record<string, string | undefined> } }).process?.env;
    return env?.[name] || undefined;
}

/**
 * The Policy Helper failure attached to a JSON-RPC error
 * (`error.data.policyHelperFailure`), if any.
 */
export function getPolicyHelperFailure(error: unknown): PolicyHelperFailure | undefined {
    const data = (error as { data?: unknown } | null | undefined)?.data;
    const failure = (data as { policyHelperFailure?: unknown } | null | undefined)?.policyHelperFailure;
    return failure !== null && typeof failure === "object" ? (failure as PolicyHelperFailure) : undefined;
}

/**
 * Run a `session.create` / `session.resume`; on failure, expose the runtime's
 * Policy Helper failure as `error.policyHelperFailure` before rethrowing.
 */
async function withPolicyHelperFailure<T>(operation: () => Promise<T>): Promise<T> {
    try {
        return await operation();
    } catch (error) {
        const failure = getPolicyHelperFailure(error);
        if (failure !== undefined && error !== null && typeof error === "object") {
            (error as SessionStartError).policyHelperFailure = failure;
        }
        throw error;
    }
}

/** True for a JSON-RPC "method not found" error (-32601) from an older runtime. */
function isMethodNotFound(error: unknown): boolean {
    const e = error as { code?: unknown; message?: unknown } | null | undefined;
    if (e?.code === -32601) return true;
    return typeof e?.message === "string" && /method not found|unhandled method/i.test(e.message);
}

/** `gitHubToken` and `gitHubTokenProvider` are mutually exclusive. */
function assertGitHubTokenOptions(config: {
    gitHubToken?: string;
    gitHubTokenProvider?: unknown;
}): void {
    if (config.gitHubToken !== undefined && config.gitHubTokenProvider !== undefined) {
        throw new Error("gitHubToken and gitHubTokenProvider are mutually exclusive");
    }
}

/**
 * Wrap a GitHub token provider so every answer to the runtime's
 * `gitHubToken.getToken` request is a well-formed result. The base client
 * registers the provider under a unique registration ID, sends
 * `gitHubTokenProviderRegistrationId` on create/resume, routes
 * `gitHubToken.getToken` requests to it, and drops the registration when
 * the session is disconnected or the client stops.
 */
function wrapGitHubTokenProvider(provider: GitHubTokenProvider): GitHubTokenProvider {
    return async (args) => {
        const result = await provider(args);
        if (result?.kind === "token" || result?.kind === "cancelled") {
            return result;
        }
        throw new Error(
            'gitHubToken.getToken: provider must return { kind: "token", accessToken, expiresIn } or { kind: "cancelled" }',
        );
    };
}

function buildAuditPrompt(
    sourceCode: string,
    contractName?: string,
    includeGas?: boolean,
): string {
    const parts = [
        "Perform a comprehensive security audit of the following Solidity contract.",
        contractName ? `Contract name: ${contractName}.` : "",
        "Identify vulnerabilities, rank by severity, and provide remediation advice.",
        includeGas ? "Also include any gas optimization suggestions." : "",
        "Return the result as a JSON AuditReport.",
        "```solidity",
        sourceCode,
        "```",
    ];
    return parts.filter(Boolean).join("\n");
}

function buildGasPrompt(sourceCode: string, contractName?: string): string {
    return [
        "Analyze the following Solidity contract for gas optimization opportunities.",
        contractName ? `Contract name: ${contractName}.` : "",
        "Categorize each optimization and provide before/after code snippets.",
        "Return the result as a JSON GasReport.",
        "```solidity",
        sourceCode,
        "```",
    ]
        .filter(Boolean)
        .join("\n");
}

function buildTestGenPrompt(
    sourceCode: string,
    framework: TestFramework,
    contractName?: string,
): string {
    const frameworkLabel = framework === "foundry" ? "Foundry (Solidity)" : "Hardhat (TypeScript)";
    return [
        `Generate a comprehensive ${frameworkLabel} test suite for the following contract.`,
        contractName ? `Contract name: ${contractName}.` : "",
        "Cover deployment, access control, edge cases, reverts, events, and state transitions.",
        "Return the result as a JSON GeneratedTestSuite.",
        "```solidity",
        sourceCode,
        "```",
    ]
        .filter(Boolean)
        .join("\n");
}

/**
 * Try to parse a JSON response from Copilot. If parsing fails, return the
 * provided fallback value so callers always get a typed result.
 */
function parseJsonResponse<T>(raw: string, fallback: T): T {
    // Copilot may wrap JSON in markdown code fences
    const cleaned = raw
        .replace(/^```(?:json)?\s*/i, "")
        .replace(/\s*```$/i, "")
        .trim();

    try {
        return JSON.parse(cleaned) as T;
    } catch {
        return fallback;
    }
}

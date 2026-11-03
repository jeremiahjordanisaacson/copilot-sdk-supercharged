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
    approveAll,
    type CopilotSession,
    type ResumeSessionConfig,
    type SessionConfig,
    type Tool,
} from "copilot-sdk-supercharged";

import {
    createAuditTool,
    createGasOptimizationTool,
    createTestGenerationTool,
    createSecurityCheckTool,
    createSolidityTools,
} from "./tools.js";

import type {
    AuditReport,
    AutoTier,
    CopilotSolidityClientOptions,
    GasReport,
    GeneratedTestSuite,
    GitHubTokenProvider,
    ModelSwitchAutoTierResult,
    SoliditySessionConfig,
    SolidityMessageOptions,
    SolidityUpstreamSessionOptions,
    SessionEventEnvelope,
    TestFramework,
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

        this.client = new CopilotClient(baseOptions);
        this.solidityVersion = solidityVersion;
        this.defaultTestFramework = defaultTestFramework;
        this.includeGasHints = includeGasHints;
    }

    // -----------------------------------------------------------------------
    // Lifecycle
    // -----------------------------------------------------------------------

    /** Start the underlying Copilot CLI server. */
    async start(): Promise<void> {
        await this.client.start();
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

    /** Get the foreground session ID from the CLI server. */
    async getForegroundSessionId(): Promise<string> {
        const result = await (this.client as any).request("session.getForeground", {});
        return result?.sessionId ?? "";
    }

    /** Set the foreground session ID on the CLI server. */
    async setForegroundSessionId(sessionId: string): Promise<void> {
        const result = await (this.client as any).request("session.setForeground", { sessionId });
        if (!result?.success) {
            throw new Error(`Failed to set foreground session: ${result?.error ?? "Unknown error"}`);
        }
    }

    /** Get the last session ID, or null if none. */
    async getLastSessionId(): Promise<string | null> {
        const result = await (this.client as any).request("session.getLastId", {});
        return result?.sessionId ?? null;
    }

    /** Get metadata for a session by ID. */
    async getSessionMetadata(sessionId: string): Promise<unknown> {
        return (this.client as any).request("session.getMetadata", { sessionId });
    }

    /** List available models. */
    async listModels(): Promise<unknown[]> {
        return (this.client as any).request("models.list", {});
    }

    /** Ping the server. */
    async ping(message?: string): Promise<{ message: string; timestamp: string }> {
        return (this.client as any).request("ping", { message: message ?? "ping" });
    }

    /** Get server status. */
    async getStatus(): Promise<unknown> {
        return (this.client as any).request("status.get", {});
    }

    /** Get authentication status. */
    async getAuthStatus(): Promise<unknown> {
        return (this.client as any).request("auth.getStatus", {});
    }

    /**
     * Resume a previously created session by ID.
     *
     * Without `config`, issues the raw "session.resume" RPC. With `config`, the
     * session is resumed through the base SDK with the Solidity tools and the
     * resume options are forwarded (autoTier, askUserVariant, sandboxConfig,
     * featureFlags, allowedModels, allowTranscriptRecovery, gitHubTokenProvider, ...).
     */
    async resumeSession(sessionId: string): Promise<unknown>;
    async resumeSession(sessionId: string, config: SoliditySessionOptions): Promise<CopilotSession>;
    async resumeSession(sessionId: string, config?: SoliditySessionOptions): Promise<unknown> {
        if (config === undefined) {
            return (this.client as any).request("session.resume", { sessionId });
        }
        assertGitHubTokenOptions(config);

        const resumeConfig: ResumeSessionConfig = {
            ...config,
            tools: this.buildSessionTools(config),
            onPermissionRequest: config.onPermissionRequest ?? approveAll,
            systemMessage: config.systemMessage ?? {
                mode: "append" as const,
                content: SOLIDITY_SYSTEM_PROMPT,
            },
            ...this.buildUpstreamSessionOptions(config, "resume"),
        };

        return this.client.resumeSession(sessionId, resumeConfig);
    }

    /** Delete a session by ID. */
    async deleteSession(sessionId: string): Promise<void> {
        await (this.client as any).request("session.delete", { sessionId });
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

    /** List all known sessions. */
    async listSessions(): Promise<unknown[]> {
        return (this.client as any).request("session.list", {});
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
        return (this.client as any).request("sessionFs.setProvider", config);
    }

    // -----------------------------------------------------------------------
    // Session helpers
    // -----------------------------------------------------------------------

    /**
     * Create a raw session via the "session.create" RPC (without Solidity tools).
     * For a session pre-loaded with Solidity tools use {@link createSession}.
     */
    async createRawSession(params: Record<string, unknown> = {}): Promise<unknown> {
        return (this.client as any).request("session.create", params);
    }

    /**
     * Create a session pre-configured with Solidity tools and system prompt.
     *
     * Any extra tools or config from `config` are merged on top.
     */
    async createSession(
        config: SoliditySessionOptions = {},
    ): Promise<CopilotSession> {
        assertGitHubTokenOptions(config);

        const sessionConfig: SessionConfig = {
            ...config,
            tools: this.buildSessionTools(config),
            onPermissionRequest: config.onPermissionRequest ?? approveAll,
            systemMessage: config.systemMessage ?? {
                mode: "append" as const,
                content: SOLIDITY_SYSTEM_PROMPT,
            },
            ...this.buildUpstreamSessionOptions(config, "create"),
        };

        return this.client.createSession(sessionConfig);
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
     * experiment assignments, the MCP OAuth host token handler signal, and
     * the v2.6.0 create/resume options.
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
        if (options.memory !== undefined) params.memory = options.memory;
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
        if (options.onUserPromptTransformed !== undefined)
            params.userPromptTransformed = true;
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
            params.experimentalMode = options.experimentalMode;
        if (options.contentExclusion !== undefined)
            params.contentExclusion = options.contentExclusion;
        if (options.onMcpAuthRequest !== undefined) params.mcpAuthHandler = true;
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
        if (mode === "create") {
            if (options.name !== undefined) params.name = options.name;
            if (options.trustWorkingDirectory !== undefined)
                params.trustWorkingDirectory = options.trustWorkingDirectory;
            if (options.refreshCustomInstructions !== undefined)
                params.refreshCustomInstructions = options.refreshCustomInstructions;
        } else if (options.allowTranscriptRecovery !== undefined) {
            params.allowTranscriptRecovery = options.allowTranscriptRecovery;
        }
        return params;
    }

    /**
     * Send a message to a session, forwarding upstream message options
     * (agent mode, display prompt, per-message request headers, message
     * source, and structured-output schema).
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

    /** Return a session's effective managed settings (`session.managedSettings.get`). */
    async getManagedSettings(session: CopilotSession): Promise<unknown> {
        return session.rpc.managedSettings.get();
    }

    /**
     * Re-run the managed-settings policy helper for a session
     * (`session.managedSettings.recheckPolicyHelper`) and return the snapshot.
     */
    async recheckPolicyHelper(session: CopilotSession): Promise<unknown> {
        return session.rpc.managedSettings.recheckPolicyHelper();
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
        const session = await this.createSession();
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
        const session = await this.createSession();
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
        const session = await this.createSession();
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
        const session = await this.createSession();
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

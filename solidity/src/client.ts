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
    CopilotSolidityClientOptions,
    GasReport,
    GeneratedTestSuite,
    SoliditySessionConfig,
    TestFramework,
} from "./types.js";

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

    // -----------------------------------------------------------------------
    // Session helpers
    // -----------------------------------------------------------------------

    /**
     * Create a session pre-configured with Solidity tools and system prompt.
     *
     * Any extra tools or config from `config` are merged on top.
     */
    async createSession(
        config: Partial<SoliditySessionConfig> = {},
    ): Promise<CopilotSession> {
        const builtInTools = createSolidityTools({
            solidityVersion: this.solidityVersion,
            defaultTestFramework: this.defaultTestFramework,
        });

        const allTools: Tool[] = [
            ...builtInTools,
            ...(config.solidityTools ?? []),
            ...(config.tools ?? []),
        ];

        const sessionConfig: SessionConfig = {
            ...config,
            tools: allTools,
            onPermissionRequest: config.onPermissionRequest ?? approveAll,
            systemMessage: config.systemMessage ?? {
                mode: "append" as const,
                content: SOLIDITY_SYSTEM_PROMPT,
            },
        };

        return this.client.createSession(sessionConfig);
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
            await session.destroy();
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
            await session.destroy();
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
            await session.destroy();
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
            await session.destroy();
        }
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

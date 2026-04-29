/**
 * Pre-built Copilot tools for Solidity / smart-contract workflows.
 *
 * Each tool is a standard {@link Tool} that can be passed directly into a
 * session config. The tools craft prompts that leverage Copilot's knowledge
 * of Solidity, the EVM, and common vulnerability databases (SWC, DASP).
 */

import { defineTool, type Tool } from "copilot-sdk-supercharged";
import type {
    AuditToolParams,
    GasToolParams,
    TestGenToolParams,
    SecurityCheckToolParams,
    TestFramework,
} from "./types.js";

// ---------------------------------------------------------------------------
// Prompt helpers (keep prompts co-located with tool definitions)
// ---------------------------------------------------------------------------

function auditSystemPrompt(version?: string): string {
    const ver = version ? ` compiled with Solidity ${version}` : "";
    return [
        `You are a senior smart-contract security auditor.`,
        `Analyze the provided Solidity source code${ver} for vulnerabilities.`,
        `For each finding, include: title, severity (critical/high/medium/low/informational),`,
        `description with root cause, affected line range, remediation, and SWC ID if applicable.`,
        `Return a JSON object matching the AuditReport schema with fields:`,
        `contractName, filePath, findings[], summary, timestamp.`,
        `Order findings from most to least severe.`,
    ].join(" ");
}

function gasSystemPrompt(): string {
    return [
        `You are an EVM gas optimization expert.`,
        `Analyze the provided Solidity source code and suggest concrete gas optimizations.`,
        `Categories: storage, memory, calldata, loop, packing, short-circuit, unchecked-math, general.`,
        `For each optimization include: title, description, estimatedSavings, before/after code snippets, category.`,
        `Return a JSON object matching the GasReport schema with fields:`,
        `contractName, filePath, optimizations[], summary, timestamp.`,
    ].join(" ");
}

function testGenSystemPrompt(framework: TestFramework): string {
    const frameworkGuide =
        framework === "foundry"
            ? "Write Foundry tests in Solidity using forge-std/Test.sol. Use vm.prank, vm.expectRevert, etc."
            : "Write Hardhat tests in TypeScript using ethers.js and chai matchers (expect, revertedWith, etc.).";
    return [
        `You are an expert smart-contract test engineer.`,
        `Generate a comprehensive test suite for the provided Solidity contract.`,
        `${frameworkGuide}`,
        `Cover: deployment, access control, edge cases, reverts, events, and state transitions.`,
        `Return a JSON object matching the GeneratedTestSuite schema with fields:`,
        `contractName, framework, code, testCount, summary.`,
    ].join(" ");
}

function securityCheckSystemPrompt(patterns?: string[]): string {
    const scope = patterns?.length
        ? `Focus specifically on: ${patterns.join(", ")}.`
        : "Check for all common vulnerability patterns.";
    return [
        `You are a Solidity security specialist.`,
        `Perform a targeted security pattern analysis on the provided source code.`,
        `${scope}`,
        `For each issue found, return the pattern name, description, affected code, and severity.`,
        `Return a JSON array of findings.`,
    ].join(" ");
}

// ---------------------------------------------------------------------------
// Tool definitions
// ---------------------------------------------------------------------------

/**
 * Audit a Solidity smart contract for security vulnerabilities.
 *
 * The tool sends the source code to Copilot with an auditing system prompt
 * and returns structured findings.
 */
export function createAuditTool(defaultVersion?: string): Tool<AuditToolParams> {
    return defineTool<AuditToolParams>("solidity_audit", {
        description:
            "Audit a Solidity smart contract for security vulnerabilities. " +
            "Accepts source code and returns an AuditReport with findings ordered by severity.",
        parameters: {
            type: "object",
            properties: {
                sourceCode: {
                    type: "string",
                    description: "The Solidity source code to audit.",
                },
                contractName: {
                    type: "string",
                    description: "Contract name (inferred from source if omitted).",
                },
                solidityVersion: {
                    type: "string",
                    description: "Compiler version override (e.g. 0.8.24).",
                },
            },
            required: ["sourceCode"],
        },
        handler: async (args) => {
            const params = args as unknown as AuditToolParams;
            const version = params.solidityVersion ?? defaultVersion;
            const prompt = auditSystemPrompt(version);
            return {
                systemPrompt: prompt,
                userMessage: params.sourceCode,
                contractName: params.contractName,
            };
        },
    });
}

/**
 * Analyze a Solidity contract for gas optimization opportunities.
 */
export function createGasOptimizationTool(): Tool<GasToolParams> {
    return defineTool<GasToolParams>("solidity_gas_optimize", {
        description:
            "Analyze a Solidity smart contract for gas optimization opportunities. " +
            "Returns a GasReport with categorized suggestions and before/after snippets.",
        parameters: {
            type: "object",
            properties: {
                sourceCode: {
                    type: "string",
                    description: "The Solidity source code to analyze.",
                },
                contractName: {
                    type: "string",
                    description: "Contract name (optional).",
                },
            },
            required: ["sourceCode"],
        },
        handler: async (args) => {
            const params = args as unknown as GasToolParams;
            return {
                systemPrompt: gasSystemPrompt(),
                userMessage: params.sourceCode,
                contractName: params.contractName,
            };
        },
    });
}

/**
 * Generate a test suite for a Solidity contract.
 */
export function createTestGenerationTool(
    defaultFramework: TestFramework = "hardhat",
): Tool<TestGenToolParams> {
    return defineTool<TestGenToolParams>("solidity_generate_tests", {
        description:
            "Generate a comprehensive test suite for a Solidity smart contract. " +
            "Supports Hardhat (TypeScript) and Foundry (Solidity) output.",
        parameters: {
            type: "object",
            properties: {
                sourceCode: {
                    type: "string",
                    description: "The Solidity source code to generate tests for.",
                },
                contractName: {
                    type: "string",
                    description: "Contract name (optional).",
                },
                framework: {
                    type: "string",
                    enum: ["hardhat", "foundry"],
                    description: "Target test framework. Defaults to hardhat.",
                },
            },
            required: ["sourceCode"],
        },
        handler: async (args) => {
            const params = args as unknown as TestGenToolParams;
            const fw = params.framework ?? defaultFramework;
            return {
                systemPrompt: testGenSystemPrompt(fw),
                userMessage: params.sourceCode,
                contractName: params.contractName,
                framework: fw,
            };
        },
    });
}

/**
 * Check a contract for specific security anti-patterns.
 */
export function createSecurityCheckTool(): Tool<SecurityCheckToolParams> {
    return defineTool<SecurityCheckToolParams>("solidity_security_check", {
        description:
            "Check a Solidity smart contract for specific security anti-patterns " +
            "(reentrancy, overflow, access-control, etc.).",
        parameters: {
            type: "object",
            properties: {
                sourceCode: {
                    type: "string",
                    description: "The Solidity source code to check.",
                },
                patterns: {
                    type: "array",
                    items: {
                        type: "string",
                        enum: [
                            "reentrancy",
                            "overflow",
                            "access-control",
                            "unchecked-return",
                            "front-running",
                            "oracle-manipulation",
                            "denial-of-service",
                            "timestamp-dependency",
                        ],
                    },
                    description: "Patterns to check. Omit for all.",
                },
            },
            required: ["sourceCode"],
        },
        handler: async (args) => {
            const params = args as unknown as SecurityCheckToolParams;
            return {
                systemPrompt: securityCheckSystemPrompt(params.patterns),
                userMessage: params.sourceCode,
                patterns: params.patterns,
            };
        },
    });
}

// ---------------------------------------------------------------------------
// Convenience: get all Solidity tools at once
// ---------------------------------------------------------------------------

export interface SolidityToolsOptions {
    /** Default compiler version for the audit tool. */
    solidityVersion?: string;
    /** Default test framework for the test generation tool. */
    defaultTestFramework?: TestFramework;
}

/**
 * Create the full set of pre-built Solidity tools.
 *
 * ```ts
 * const tools = createSolidityTools({ solidityVersion: "0.8.24" });
 * const session = await client.createSession({ tools });
 * ```
 */
export function createSolidityTools(options: SolidityToolsOptions = {}): Tool<unknown>[] {
    return [
        createAuditTool(options.solidityVersion) as Tool<unknown>,
        createGasOptimizationTool() as Tool<unknown>,
        createTestGenerationTool(options.defaultTestFramework) as Tool<unknown>,
        createSecurityCheckTool() as Tool<unknown>,
    ];
}

/**
 * Tests for the Copilot Solidity SDK.
 *
 * These are unit tests that validate tool creation, client construction,
 * and type-level correctness. They do NOT require a running Copilot CLI.
 */

import { describe, it, expect, vi, beforeEach } from "vitest";
import {
    CopilotSolidityClient,
    createAuditTool,
    createGasOptimizationTool,
    createTestGenerationTool,
    createSecurityCheckTool,
    createSolidityTools,
} from "../src/index.js";

// ---------------------------------------------------------------------------
// Tool creation tests
// ---------------------------------------------------------------------------

describe("createAuditTool", () => {
    it("creates a tool with the correct name", () => {
        const tool = createAuditTool();
        expect(tool.name).toBe("solidity_audit");
    });

    it("includes a description", () => {
        const tool = createAuditTool();
        expect(tool.description).toBeTruthy();
        expect(tool.description).toContain("Audit");
    });

    it("defines sourceCode as a required parameter", () => {
        const tool = createAuditTool();
        const params = tool.parameters as Record<string, unknown>;
        expect(params).toBeDefined();
        expect((params as { required: string[] }).required).toContain("sourceCode");
    });

    it("has a callable handler", async () => {
        const tool = createAuditTool("0.8.24");
        expect(typeof tool.handler).toBe("function");

        const result = await tool.handler(
            { sourceCode: "pragma solidity ^0.8.0;", contractName: "Test" } as never,
            { toolCallId: "test-id" } as never,
        );

        expect(result).toBeDefined();
        expect((result as Record<string, unknown>).systemPrompt).toBeTruthy();
    });

    it("includes compiler version in the prompt when provided", async () => {
        const tool = createAuditTool("0.8.24");
        const result = (await tool.handler(
            { sourceCode: "code" } as never,
            { toolCallId: "test-id" } as never,
        )) as Record<string, string>;

        expect(result.systemPrompt).toContain("0.8.24");
    });
});

describe("createGasOptimizationTool", () => {
    it("creates a tool with the correct name", () => {
        const tool = createGasOptimizationTool();
        expect(tool.name).toBe("solidity_gas_optimize");
    });

    it("has a callable handler", async () => {
        const tool = createGasOptimizationTool();
        const result = await tool.handler(
            { sourceCode: "pragma solidity ^0.8.0;" } as never,
            { toolCallId: "test-id" } as never,
        );
        expect(result).toBeDefined();
    });
});

describe("createTestGenerationTool", () => {
    it("creates a tool with the correct name", () => {
        const tool = createTestGenerationTool();
        expect(tool.name).toBe("solidity_generate_tests");
    });

    it("defaults to hardhat framework", async () => {
        const tool = createTestGenerationTool();
        const result = (await tool.handler(
            { sourceCode: "code" } as never,
            { toolCallId: "test-id" } as never,
        )) as Record<string, string>;

        expect(result.framework).toBe("hardhat");
    });

    it("respects a custom default framework", async () => {
        const tool = createTestGenerationTool("foundry");
        const result = (await tool.handler(
            { sourceCode: "code" } as never,
            { toolCallId: "test-id" } as never,
        )) as Record<string, string>;

        expect(result.framework).toBe("foundry");
    });

    it("allows per-call framework override", async () => {
        const tool = createTestGenerationTool("hardhat");
        const result = (await tool.handler(
            { sourceCode: "code", framework: "foundry" } as never,
            { toolCallId: "test-id" } as never,
        )) as Record<string, string>;

        expect(result.framework).toBe("foundry");
    });
});

describe("createSecurityCheckTool", () => {
    it("creates a tool with the correct name", () => {
        const tool = createSecurityCheckTool();
        expect(tool.name).toBe("solidity_security_check");
    });

    it("accepts specific patterns", async () => {
        const tool = createSecurityCheckTool();
        const result = (await tool.handler(
            { sourceCode: "code", patterns: ["reentrancy"] } as never,
            { toolCallId: "test-id" } as never,
        )) as Record<string, unknown>;

        expect(result.systemPrompt).toBeTruthy();
        expect((result.systemPrompt as string)).toContain("reentrancy");
    });
});

describe("createSolidityTools", () => {
    it("returns all four tools", () => {
        const tools = createSolidityTools();
        expect(tools).toHaveLength(4);

        const names = tools.map((t) => t.name);
        expect(names).toContain("solidity_audit");
        expect(names).toContain("solidity_gas_optimize");
        expect(names).toContain("solidity_generate_tests");
        expect(names).toContain("solidity_security_check");
    });

    it("passes options to individual tools", () => {
        const tools = createSolidityTools({ solidityVersion: "0.8.20" });
        expect(tools).toHaveLength(4);
    });
});

// ---------------------------------------------------------------------------
// Client construction tests
// ---------------------------------------------------------------------------

describe("CopilotSolidityClient", () => {
    it("can be constructed with default options", () => {
        const client = new CopilotSolidityClient();
        expect(client).toBeDefined();
        expect(client.client).toBeDefined();
    });

    it("can be constructed with custom options", () => {
        const client = new CopilotSolidityClient({
            solidityVersion: "0.8.24",
            defaultTestFramework: "foundry",
            includeGasHints: true,
        });
        expect(client).toBeDefined();
    });

    it("exposes individual tool accessors", () => {
        const client = new CopilotSolidityClient({
            solidityVersion: "0.8.24",
            defaultTestFramework: "foundry",
        });

        expect(client.getAuditTool().name).toBe("solidity_audit");
        expect(client.getGasOptimizationTool().name).toBe("solidity_gas_optimize");
        expect(client.getTestGenerationTool().name).toBe("solidity_generate_tests");
        expect(client.getSecurityCheckTool().name).toBe("solidity_security_check");
    });
});

// ---------------------------------------------------------------------------
// Export verification
// ---------------------------------------------------------------------------

describe("module exports", () => {
    it("exports all expected symbols", async () => {
        const mod = await import("../src/index.js");

        // Client
        expect(mod.CopilotSolidityClient).toBeDefined();

        // Tool factories
        expect(mod.createAuditTool).toBeDefined();
        expect(mod.createGasOptimizationTool).toBeDefined();
        expect(mod.createTestGenerationTool).toBeDefined();
        expect(mod.createSecurityCheckTool).toBeDefined();
        expect(mod.createSolidityTools).toBeDefined();
    });
});

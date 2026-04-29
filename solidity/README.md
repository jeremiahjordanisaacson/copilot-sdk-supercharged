# GitHub Copilot Supercharged SDK for Solidity / Web3

The Solidity SDK provides TypeScript helpers for integrating GitHub Copilot into smart-contract development workflows. It wraps the base `copilot-sdk-supercharged` package with tools and prompts purpose-built for Ethereum/EVM development.

> **Note:** This is not a Solidity library deployed on-chain. It is a TypeScript/Node.js package that helps Web3 developers use Copilot for contract auditing, gas optimization, test generation, and security analysis.

## Requirements

- Node.js >= 20
- The Copilot CLI installed and available on PATH (or set `COPILOT_CLI_PATH`)
- Active GitHub Copilot authentication
- A Hardhat or Foundry project (optional, but recommended)

## Installation

```bash
npm install copilot-sdk-supercharged copilot-sdk-supercharged-solidity
```

### Building from source

```bash
cd solidity
npm install
npm run build
```

## Quick Start

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

const client = new CopilotSolidityClient({
    solidityVersion: "0.8.24",
    defaultTestFramework: "hardhat",
});

await client.start();

// Audit a contract
const report = await client.auditContract(`
    // SPDX-License-Identifier: MIT
    pragma solidity ^0.8.0;

    contract Vault {
        mapping(address => uint256) public balances;

        function deposit() external payable {
            balances[msg.sender] += msg.value;
        }

        function withdraw(uint256 amount) external {
            require(balances[msg.sender] >= amount);
            (bool ok, ) = msg.sender.call{value: amount}("");
            require(ok);
            balances[msg.sender] -= amount;
        }
    }
`);

console.log("Findings:", report.findings.length);
for (const f of report.findings) {
    console.log(`  [${f.severity}] ${f.title}`);
}

await client.stop();
```

## Architecture

```
Your App
  |
  v
CopilotSolidityClient          (this SDK)
  |-- createSession()           pre-loads Solidity tools + system prompt
  |-- auditContract()           one-shot audit helper
  |-- optimizeGas()             gas optimization analysis
  |-- generateTests()           Hardhat / Foundry test generation
  |-- checkSecurityPatterns()   targeted pattern checks
  |
  v
CopilotClient                  (copilot-sdk-supercharged)
  |
  v
Copilot CLI (JSON-RPC)
```

## Features

### Contract Auditing

The `auditContract()` method sends your Solidity source to Copilot with a security-auditor system prompt. It returns a structured `AuditReport` with findings ordered by severity.

```typescript
const report = await client.auditContract(sourceCode, "MyToken");
```

### Gas Optimization

Analyze contracts for gas savings across categories: storage packing, calldata usage, unchecked math, loop optimizations, and more.

```typescript
const gasReport = await client.optimizeGas(sourceCode);
for (const opt of gasReport.optimizations) {
    console.log(`[${opt.category}] ${opt.title} - saves ${opt.estimatedSavings}`);
}
```

### Test Generation

Generate comprehensive test suites targeting either Hardhat (TypeScript + ethers.js) or Foundry (Solidity + forge-std).

```typescript
const tests = await client.generateTests(sourceCode, {
    framework: "foundry",
    contractName: "Vault",
});

// tests.code contains the complete test file
```

### Security Pattern Checks

Run targeted checks for specific vulnerability patterns:

```typescript
const findings = await client.checkSecurityPatterns(sourceCode, [
    "reentrancy",
    "access-control",
]);
```

## Pre-built Tools

You can also use the individual tools directly in a session for more control:

```typescript
import {
    createAuditTool,
    createGasOptimizationTool,
    createSolidityTools,
} from "copilot-sdk-supercharged-solidity";

// Get all four tools at once
const tools = createSolidityTools({ solidityVersion: "0.8.24" });

// Or pick specific ones
const auditTool = createAuditTool("0.8.24");
```

Available tools:
- `solidity_audit` - Security vulnerability auditing
- `solidity_gas_optimize` - Gas optimization analysis
- `solidity_generate_tests` - Test suite generation
- `solidity_security_check` - Targeted pattern checking

## Configuration

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `solidityVersion` | `string` | `undefined` | Default Solidity compiler version |
| `defaultTestFramework` | `"hardhat" \| "foundry"` | `"hardhat"` | Target framework for test generation |
| `includeGasHints` | `boolean` | `false` | Add gas hints to every audit |
| `cliUrl` | `string` | `undefined` | Connect to an existing Copilot CLI server |
| `cwd` | `string` | `process.cwd()` | Working directory for the CLI process |

All base `CopilotClientOptions` from the core SDK are also accepted.

## Cookbook

See the `cookbook/` directory for detailed recipes:

- [Error Handling](cookbook/error-handling.md)
- [Contract Auditing](cookbook/contract-audit.md)
- [Gas Optimization](cookbook/gas-optimization.md)
- [Test Generation](cookbook/test-generation.md)
- [Tools and Skills](cookbook/tools-and-skills.md)
- [Advanced Features](cookbook/advanced-features.md)

## Running Tests

```bash
cd solidity
npm install
npm test
```

## License

MIT

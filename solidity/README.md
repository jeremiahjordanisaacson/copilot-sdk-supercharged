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

## What's New in v2.6.0

Parity with upstream `@github/copilot-sdk` (Copilot CLI 1.0.106). Session
options go to `client.createSession(...)` and, unless noted, also to
`client.resumeSession(id, config)` (`SoliditySessionOptions`). New options are
only sent when set.

```typescript
import { CopilotSolidityClient, MessageSource } from "copilot-sdk-supercharged-solidity";

// Client info: sent on the `connect` handshake before `ping`
const client = new CopilotSolidityClient({
    clientInfo: {
        applicationName: "vault-auditor",
        applicationVersion: "1.2.0",
        integrationName: "hardhat-plugin",
        integrationVersion: "0.4.0",
    },
});
await client.start();

const session = await client.createSession({
    name: "vault-audit",                               // create only
    autoTier: "balance",                               // sent as capi.autoTier (experimental)
    askUserVariant: "elicitation",
    allowedModels: ["gpt-5", "claude-sonnet-4.6"],
    authClientIdMetadataUrl: "https://example.com/oauth/client.json",
    agentDirectories: ["./.github/agents"],
    hookDirectories: ["./.github/hooks"],
    featureFlags: { someRuntimeFlag: true },           // sent even when {}
    sandboxConfig: { enabled: true, addCurrentWorkingDirectory: true, allowBypass: true },
    imageGeneration: { enabled: true, watermark: true, saveToDisk: false },
    autopilot: { continuationEnabled: true, maxContinues: 5 },
    interactiveUser: false,
    excludedManagedSettings: ["model"],
    enforceManagedModelDefaults: true,
    trustWorkingDirectory: "session",                  // create only
    refreshCustomInstructions: true,                   // create only
    modelCapabilities: { limits: { max_output_tokens: 8192 } },
    provider: {
        type: "openai",
        baseUrl: "http://localhost:11434/v1",
        modelProvider: "ollama",
        apiKeyCommand: "op read op://dev/llm/api-key",
    },
    hooks: {
        onSubagentStart: (input) => ({ additionalContext: `Audit focus for ${input.agentName}` }),
        onSubagentStop: (input) => ({ modifiedResponse: input.response.trim() }),
    },
});
```

- **Client info** — `clientInfo` (`applicationName`, `applicationVersion`, `integrationName`,
  `integrationVersion`) is declared on `connect`; older CLIs without `connect` are tolerated.
- **Auto routing tier** — `autoTier` (`"efficiency" | "balance" | "intelligence" | "fast"`) is sent under
  `capi`, together with `enableWebSocketResponses` (which now also travels under `capi`). Change it on a live
  session with `client.setAutoTier(session, "intelligence")`, or pass `null` to return to default routing.
- **GitHub token provider** — `gitHubTokenProvider` answers the runtime's `gitHubToken.getToken` requests
  (mutually exclusive with `gitHubToken`):

  ```typescript
  const session = await client.createSession({
      gitHubTokenProvider: async ({ host, sessionId, reason }) => ({
          kind: "token",
          accessToken: await mintToken(host, sessionId, reason),
          expiresIn: 7200,
      }),
  });
  ```

- **Resume options** — `client.resumeSession(id, { autoTier: "fast", allowTranscriptRecovery: false })`
  resumes through the base SDK with the Solidity tools (`allowTranscriptRecovery` is resume only).
  `client.resumeSession(id)` without options keeps the raw `session.resume` call.
- **Session detach** — the one-shot helpers (`auditContract`, `optimizeGas`, ...) release sessions with
  `session.disconnect()` (`session.detach`). `client.detachSession(id)` sends `session.detach` and falls
  back to `session.destroy` on runtimes that report `-32601`.
- **Message source** — `source` on `client.sendMessage(...)`: `MessageSource.user`, `MessageSource.system`,
  or `MessageSource.agent("auditor")` (`"agent-auditor"`).
- **Structured output** — `responseSchema` on `client.sendMessage(...)` is sent as a strict
  `json_schema` `responseFormat` (it replaces the legacy string `responseFormat`), and
  `client.sendAndWaitStructured(...)` returns the parsed JSON:

  ```typescript
  const summary = await client.sendAndWaitStructured<{ critical: number; high: number }>(
      session,
      { prompt: "Count findings by severity", source: MessageSource.system },
      {
          type: "object",
          properties: { critical: { type: "integer" }, high: { type: "integer" } },
          required: ["critical", "high"],
      },
  );
  ```

- **Managed settings** — `client.clearManagedSettingsCache()`, `client.getManagedSettings(session)`,
  `client.recheckPolicyHelper(session)`.
- **Sub-agent hooks** — `hooks.onSubagentStart` (`{ additionalContext }`) and `hooks.onSubagentStop`
  (`{ decision: "block", reason }` or `{ modifiedResponse }`).
- **BYOK provider** — `provider.modelProvider` and `provider.apiKeyCommand`.
- **Model limits** — `modelCapabilities.limits.max_output_tokens` (`ModelLimitsOverride`).

### Parity notes

- Host-owned external tool cancellation is N/A: this SDK runs tools on the `tool.call` request path.
- Experimental host-integration callbacks (skill provider, installation confirmation, extension launch
  provider, Entra/Kerberos auth callbacks) are not yet available in this SDK.

## Recent Features (v2.4–v2.5)

The Solidity client forwards the upstream `@github/copilot-sdk` session options
to the base SDK. Pass them to `client.createSession(...)`
(typed as `SolidityUpstreamSessionOptions`), or per message via
`client.sendMessage(session, ...)` (`SolidityMessageOptions`).

### v2.5

- **Reasoning effort** — `reasoningEffort` (`"low"`, `"medium"`, `"high"`).
- **Session rewind** — `rewindEnabled` rolls the conversation back to an earlier turn.
- **Additional directories** — `additionalDirectories`.
- **Content exclusion** — `contentExclusion`.
- **Tool search** — `toolSearch` (object or `true`) discovers tools on demand.
- **Disabled MCP servers** — `disabledMcpServers`.
- **GitHub MCP tool config** — `githubMcpToolConfig`.
- **Canvas provider** — `canvasProvider`.
- **Custom agents local-only** — `customAgentsLocalOnly`.
- **Experimental mode / in-process FFI** — `experimentalMode`, `inProcess`.
- **User-prompt-transformed hook** — `onUserPromptTransformed`.
- **Permission decision context** — `decisionContext`.
- **Agent-factory args schema** — `argsSchema`.
- **Built-in plugin directories** — `builtinPluginDirectories`.

### v2.4

- **BYOK bearer token provider** — `bearerTokenProvider` (`BearerTokenProvider`).
- **MCP OAuth token handler** — `onMcpAuthRequest` (`McpAuthHandler`).
- **Session citations** — `enableCitations`.
- **Excluded built-in agents** — `excludedBuiltinAgents`.
- **Spending limits** — `sessionLimits` (`SessionLimitsConfig` with `maxAiCredits`).
- **Session memory** — `memory` (`MemoryConfiguration`).
- **OTLP protocol / WebSocket transport** — `otlpProtocol`, `enableWebSocketResponses`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `hooks.onPostToolUse`, `hooks.onPreMcpToolCall`.
- **Message agent mode / display prompt** — `agentMode`, `displayPrompt`.
- **Constants** — `ToolDefer`, `SystemMessageSection.preamble` / `.preserve`, `GitHubAttachment.gitHubCommit` / `.gitHubRepository`.
- **Experiment assignments** — `expAssignments`.

```typescript
// Reasoning effort + session rewind + extra workspace roots
const session = await client.createSession({
    reasoningEffort: "high",
    rewindEnabled: true,
    additionalDirectories: ["/repo/contracts", "/repo/test"],
});
```

```typescript
// Honor content exclusion and discover tools on demand
const session = await client.createSession({
    contentExclusion: true,
    toolSearch: true,
});
```

```typescript
// Cap AI-credit spend and emit citations
const session = await client.createSession({
    enableCitations: true,
    sessionLimits: { maxAiCredits: 5 },
});
```

```typescript
// Bring-your-own-key: supply bearer tokens dynamically
const session = await client.createSession({
    bearerTokenProvider: async () => process.env.LLM_TOKEN,
});
```

```typescript
// Per-message agent mode and display prompt
await client.sendMessage(session, {
    prompt: "Refactor this contract",
    agentMode: "plan",
    displayPrompt: "Refactor (planning)",
});
```

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

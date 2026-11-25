# GitHub Copilot Supercharged SDK for Solidity / Web3

The Solidity SDK provides TypeScript helpers for integrating GitHub Copilot into smart-contract development workflows. It wraps the base `copilot-sdk-supercharged` package with tools and prompts purpose-built for Ethereum/EVM development.

> **Note:** This is not a Solidity library deployed on-chain. It is a TypeScript/Node.js package that helps Web3 developers use Copilot for contract auditing, gas optimization, test generation, and security analysis.

## Requirements

- Node.js >= 20
- Copilot CLI 1.0.113 or newer (SDK protocol 4) installed and available on PATH (or set `COPILOT_CLI_PATH`)
- Active GitHub Copilot authentication
- A Hardhat or Foundry project (optional, but recommended)

## Installation

The Solidity helpers (`copilot-sdk-supercharged-solidity`) are not published to npm yet, so build them from source. They run on top of the Node.js SDK, which is on npm:

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
cd copilot-sdk-supercharged/solidity
npm install
npm run build
```

Then add the built package to your project (for example `npm install ../copilot-sdk-supercharged/solidity`).

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

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods
below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**;
older CLIs answer with a JSON-RPC method-not-found error, which rejects like any
other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned
secret store. The secret value is never logged. Each method takes an optional
trailing `configDir` (the runtime's configuration root when omitted).

```typescript
const { usedKeychain } = await client.mcpSecretsSave("my-server-token", process.env.MY_SERVER_TOKEN!);
console.log(usedKeychain ? "Stored in the OS keychain" : "Stored in the file backend");

await client.mcpSecretsDelete("my-server-token");   // mcp.secrets.delete
await client.mcpSecretsDeleteServer("my-server");   // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions.
`extensionApprovalsList` and `extensionApprovalsRevoke` take an optional
trailing `sessionId` to use an attached session's configuration profile.

```typescript
// Persist a pending extension permission request before answering it normally
const grant = await client.extensionApprovalsGrant(session.sessionId, requestId);
if (grant.granted) console.log(`Approved ${grant.approval?.extensionName}`);

const { approvals } = await client.extensionApprovalsList();
for (const approval of approvals) {
    // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
    await client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey);
}
```

### Ask-user image attachments

`onUserInputRequest` receives `request.imageAttachmentConstraints`
(`allowedMimeTypes`, `maxBytesPerImage`, `maxCount`) when the runtime accepts
images in the answer; it is `undefined` otherwise. Answer with `attachments`, an
array of `UserInputImageAttachment` (`type: "image"`, base64 `data`, `mimeType`,
optional `description` and `displayName`), sent only when set. Image-only
answers use `answer: ""` and `wasFreeform: true`; the runtime enforces the
limits. Elicitation handlers (`onElicitationRequest`) receive the request's
`requestId` and `_meta` from the base SDK, and a `_meta` in their result is
forwarded unchanged.

```typescript
import { readFile } from "node:fs/promises";

const session = await client.createSession({
    onUserInputRequest: async (request) => {
        if (request.imageAttachmentConstraints?.allowedMimeTypes.includes("image/png")) {
            const data = (await readFile("screenshot.png")).toString("base64");
            return {
                answer: "",
                wasFreeform: true,
                attachments: [{ type: "image", data, mimeType: "image/png", displayName: "screenshot.png" }],
            };
        }
        return { answer: "No screenshot available", wasFreeform: true };
    },
});
```

## What's New in v3.0.0

SDK protocol 4 support, matching the upstream SDK 2.0 (Microsoft Copilot SDK)
wire format. **Requires Copilot CLI 1.0.113 or newer**: `client.start()` checks
the server's protocol version and rejects older CLIs with
`SDK protocol version mismatch: SDK supports version 4, but server reports version N.`

Existing options keep working. You still pass the flat v2.x session options;
right before `session.create` / `session.resume` the client groups them into the
protocol 4 payload (`streaming` becomes `events.streaming`, `onPermissionRequest`
becomes `permissions.onRequest`, `sandboxConfig` becomes `permissions.sandbox`,
and so on). A legacy single `provider` is sent as a `"default"` named provider
with `inference.defaultProvider = "default"` and Copilot API models disabled
(`github.capi.enabled = false`), so v2.x BYOK sessions behave as before. The
grouped forms (`inference`, `permissions`, `github`, ...) are accepted too, and
explicit grouped members win.

The client is built on `copilot-sdk-supercharged` 3.x. It supplies the base
client's required `mode` and, like v2.x, spawns the Copilot CLI over stdio by
default: `cliPath`, `cliArgs`, `cwd`, `env`, `copilotHome`, `port` /
`useStdio: false` and `cliUrl` select the runtime connection, or pass `runtime`
(for example `RuntimeConnection.forTcp("localhost", 3000)`) for full control.

```typescript
import {
    CopilotSolidityClient,
    PermissionModeRefusalReason,
    getPolicyHelperFailure,
} from "copilot-sdk-supercharged-solidity";
import { defineTool } from "copilot-sdk-supercharged";

// Client mode: "coding-agent" (default; "copilot-cli" is accepted as an alias) or "empty".
// Network callbacks: the base client calls `network.setProvider` after connecting and
// answers `network.resolveProxy` / `network.authenticateNegotiate` / `network.authenticateBasic`.
const client = new CopilotSolidityClient({
    mode: "coding-agent",
    network: {
        resolveProxy: async ({ url }) =>
            url.includes("corp.example") ? "PROXY proxy.corp.example:8080" : undefined,
    },
});
await client.start();

// Named BYOK providers: select a registered model with "<provider>/<id>".
const session = await client.createSession({
    model: "local/qwen3",
    providers: [{ name: "local", type: "openai", baseUrl: "http://localhost:11434/v1" }],
    models: [{ provider: "local", id: "qwen3", maxOutputTokens: 8192 }],
    defaultProvider: "local",
    capi: { enabled: false },
    // Idempotent tools: `idempotent` is sent in tools.definitions[] (the four
    // built-in Solidity tools are idempotent prompt builders).
    tools: [
        defineTool<{ address: string }>("erc20_metadata", {
            description: "Look up ERC-20 token metadata",
            parameters: { type: "object", properties: { address: { type: "string" } } },
            idempotent: true,
            handler: async ({ address }) => lookupToken(address),
        }),
    ],
    // Shell authentication: answers `shellAuthentication.request` for sudo / ssh / git prompts.
    onShellAuthenticationRequest: async (request) =>
        request.inputKind === "confirm" ? { action: "accept", value: "yes" } : { action: "cancel" },
    // Memory store: answers `memoryStore.store` (memory is enabled automatically).
    memory: {
        storeMemory: async ({ subject, fact, citations }) => {
            await saveMemory(subject, fact, citations);
        },
    },
});

// Shell output events (`tool.shell_output`): toolCallId, text, sequence, stream.
const unsubscribe = client.onShellOutput(session, ({ toolCallId, text, stream }) => {
    (stream === "stderr" ? process.stderr : process.stdout).write(`[${toolCallId}] ${text}`);
});

// Client turn ID: sent as `clientTurnId` on session.send.
await client.sendMessage(session, { prompt: "Audit Vault.sol", clientTurnId: "turn-42" });

// Permission mode refusal reason.
const result = await client.setPermissionMode(session, "allow-all");
if (!result.success && result.reason === PermissionModeRefusalReason.managedPolicy) {
    console.warn("Managed policy keeps the current permission mode");
}

unsubscribe();
```

- **Grouped v4 session payload**: `client.createRawSession(params)` and
  `client.resumeSession(id)` send raw RPCs and group flat wire params too, so a
  boolean `hooks: true` becomes `hooks: { hasHandlers: true }`.
- **Resume event suppression**: `client.resumeSession(id, { suppressResumeEvent: true })`
  (the legacy `disableResume` is accepted as an alias).
- **Client mode**: in `mode: "empty"` the base client applies the empty-mode
  defaults and every session must set `availableTools`; the one-shot helpers
  (`auditContract`, `optimizeGas`, ...) restrict their sessions to the four
  Solidity tools. Unless you connect to an external server (`cliUrl`), empty
  mode also needs `copilotHome` (or `sessionFs`).
- **Policy helper failure**: when `session.create` / `session.resume` fails with
  a Policy Helper failure in the JSON-RPC error data, it is exposed as
  `error.policyHelperFailure` (also `getPolicyHelperFailure(error)`), and
  `client.getManagedSettings(session)` / `client.recheckPolicyHelper(session)`
  report the latest failure as `policyHelperFailure`:

  ```typescript
  try {
      await client.createSession({ model: "gpt-5" });
  } catch (error) {
      const failure = getPolicyHelperFailure(error);
      if (failure) console.error(`${failure.category}: ${failure.message}`);
      throw error;
  }
  ```

- **Protocol constant**: `SDK_PROTOCOL_VERSION` (4) is exported.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC
  families) is an experimental upstream surface that depends on the runtime's
  in-process hosting library; this SDK does not wrap it.
- In-process native runtime hosting is not the default connection: like the
  other additional SDKs, this client spawns the CLI (or connects over TCP). Pass
  `runtime: RuntimeConnection.forInProcess()` (or set
  `COPILOT_SDK_DEFAULT_CONNECTION=inprocess`) to opt in through the base SDK.
- The flat v2.x session options `otlpProtocol`, `inProcess`, `rewindEnabled`,
  `contentExclusion`, `decisionContext`, `argsSchema` and
  `builtinPluginDirectories` have no protocol 4 session field and are not sent
  (configure telemetry and built-in plugin directories on the client instead).

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
| `mode` | `"coding-agent" \| "empty"` | `"coding-agent"` | Client defaulting strategy (`"copilot-cli"` is an alias of `"coding-agent"`) |
| `cliPath` | `string` | `COPILOT_CLI_PATH` or bundled CLI | Copilot CLI executable to spawn |
| `cliUrl` | `string` | `undefined` | Connect to an existing Copilot CLI server (`"host:port"`) |
| `cwd` | `string` | `process.cwd()` | Working directory for the CLI process |
| `env` | `Record<string, string>` | `process.env` | Environment for the CLI process |
| `port` / `useStdio` | `number` / `boolean` | stdio | Spawn the CLI over TCP instead of stdio |
| `copilotHome` | `string` | `~/.copilot` | Runtime base directory |
| `network` | `NetworkConfig` | `undefined` | Proxy and Kerberos/Basic callbacks (`network.setProvider`) |

All base `CopilotClientOptions` from the core SDK are also accepted (for example `runtime`, `clientInfo`, `sessionFs`, `observability`).

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

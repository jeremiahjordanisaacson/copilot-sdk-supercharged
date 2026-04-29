# Tools and Skills

Define custom tools and register them alongside the built-in Solidity tools.

## Using Built-in Tools

The SDK provides four pre-built tools. Register them all at once:

```typescript
import { createSolidityTools } from "copilot-sdk-supercharged-solidity";
import { CopilotClient } from "copilot-sdk-supercharged";

const client = new CopilotClient();
await client.start();

const tools = createSolidityTools({ solidityVersion: "0.8.24" });
const session = await client.createSession({ tools });
```

Or pick individual tools:

```typescript
import {
    createAuditTool,
    createGasOptimizationTool,
} from "copilot-sdk-supercharged-solidity";

const session = await client.createSession({
    tools: [
        createAuditTool("0.8.24"),
        createGasOptimizationTool(),
    ],
});
```

## Adding Custom Solidity Tools

Define your own tools and mix them with the built-in set:

```typescript
import { defineTool } from "copilot-sdk-supercharged";
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

const contractSizeTool = defineTool("check_contract_size", {
    description: "Check if a contract's bytecode is under the 24KB deployment limit.",
    parameters: {
        type: "object",
        properties: {
            bytecodeHex: {
                type: "string",
                description: "Hex-encoded compiled bytecode (0x-prefixed).",
            },
        },
        required: ["bytecodeHex"],
    },
    handler: async (args) => {
        const hex = (args as { bytecodeHex: string }).bytecodeHex;
        const bytes = (hex.startsWith("0x") ? hex.slice(2) : hex).length / 2;
        const limit = 24_576;
        return {
            sizeBytes: bytes,
            limitBytes: limit,
            withinLimit: bytes <= limit,
            usagePercent: ((bytes / limit) * 100).toFixed(1) + "%",
        };
    },
});

const client = new CopilotSolidityClient();
await client.start();

const session = await client.createSession({
    solidityTools: [contractSizeTool],
});
```

## Tool for Reading Hardhat Artifacts

Expose Hardhat compilation artifacts to Copilot:

```typescript
import { defineTool } from "copilot-sdk-supercharged";
import { readFileSync, existsSync } from "node:fs";
import { join } from "node:path";

const readArtifactTool = defineTool("read_hardhat_artifact", {
    description: "Read a compiled Hardhat artifact JSON for a contract.",
    parameters: {
        type: "object",
        properties: {
            contractName: {
                type: "string",
                description: "Name of the contract (e.g. 'Token').",
            },
        },
        required: ["contractName"],
    },
    handler: async (args) => {
        const name = (args as { contractName: string }).contractName;
        const path = join("artifacts", "contracts", `${name}.sol`, `${name}.json`);
        if (!existsSync(path)) {
            return { error: `Artifact not found: ${path}` };
        }
        return JSON.parse(readFileSync(path, "utf-8"));
    },
});
```

## Registering Skills

Skills are groups of tools with shared context. Register Solidity skills alongside custom ones:

```typescript
import { CopilotSolidityClient, createSolidityTools } from "copilot-sdk-supercharged-solidity";

const client = new CopilotSolidityClient();
await client.start();

const session = await client.createSession({
    solidityTools: [readArtifactTool, contractSizeTool],
    systemMessage: {
        append: "You also have access to Hardhat artifacts and bytecode size checking.",
    },
});
```

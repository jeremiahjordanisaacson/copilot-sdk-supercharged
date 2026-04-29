# Advanced Features

Advanced patterns for the GitHub Copilot Solidity SDK.

## Connecting to an External CLI Server

If you already have a Copilot CLI server running (e.g. in a CI environment):

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

const client = new CopilotSolidityClient({
    cliUrl: "localhost:8080",
    solidityVersion: "0.8.24",
});

// No need to call start() when connecting to an existing server
const report = await client.auditContract(source);
await client.stop();
```

## Streaming Responses

Subscribe to events for real-time streaming of audit results:

```typescript
const session = await client.createSession();

session.on((event) => {
    switch (event.type) {
        case "assistant.message_delta":
            process.stdout.write(event.data ?? "");
            break;
        case "assistant.message":
            console.log("\n\nFinal:", event.data);
            break;
    }
});

await session.send({
    prompt: `Audit this contract:\n\`\`\`solidity\n${source}\n\`\`\``,
});
```

## CI/CD Integration

Run audits in your CI pipeline and fail on critical findings:

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";
import { readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";

async function ciAudit() {
    const client = new CopilotSolidityClient({ solidityVersion: "0.8.24" });
    await client.start();

    const files = readdirSync("contracts").filter((f) => f.endsWith(".sol"));
    let hasBlockers = false;

    for (const file of files) {
        const source = readFileSync(join("contracts", file), "utf-8");
        const report = await client.auditContract(source);

        const blockers = report.findings.filter(
            (f) => f.severity === "critical" || f.severity === "high",
        );

        if (blockers.length > 0) {
            hasBlockers = true;
            console.error(`FAIL: ${file} has ${blockers.length} critical/high issues`);
            for (const b of blockers) {
                console.error(`  - [${b.severity}] ${b.title}`);
            }
        } else {
            console.log(`PASS: ${file}`);
        }
    }

    await client.stop();
    process.exitCode = hasBlockers ? 1 : 0;
}

ciAudit();
```

## Custom System Prompts

Override the default Solidity system prompt for specialized use cases:

```typescript
const session = await client.createSession({
    systemMessage: {
        append: [
            "You are a DeFi protocol auditor specializing in lending protocols.",
            "Pay special attention to oracle manipulation, flash loan attacks,",
            "and liquidation edge cases.",
        ].join(" "),
    },
});
```

## Combining with the Base SDK

The Solidity client exposes the underlying `CopilotClient` for advanced usage:

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

const solClient = new CopilotSolidityClient();
await solClient.start();

// Use Solidity-specific features
const audit = await solClient.auditContract(source);

// Use the base client for non-Solidity tasks
const baseSession = await solClient.client.createSession();
const response = await baseSession.sendAndWait({
    prompt: "Write a deployment script for this contract using Hardhat Ignition.",
});

await baseSession.destroy();
await solClient.stop();
```

## Parallel Analysis

Run multiple analyses concurrently for faster results:

```typescript
async function fullPipeline(source: string) {
    const client = new CopilotSolidityClient({ solidityVersion: "0.8.24" });
    await client.start();

    const [audit, gas, tests, patterns] = await Promise.all([
        client.auditContract(source),
        client.optimizeGas(source),
        client.generateTests(source, { framework: "foundry" }),
        client.checkSecurityPatterns(source, ["reentrancy", "access-control"]),
    ]);

    console.log(`Audit: ${audit.findings.length} findings`);
    console.log(`Gas: ${gas.optimizations.length} optimizations`);
    console.log(`Tests: ${tests.testCount} test cases`);
    console.log(`Patterns: ${(patterns as unknown[]).length} issues`);

    await client.stop();
}
```

# Contract Auditing

Recipes for auditing smart contracts with the GitHub Copilot Solidity SDK.

## One-Shot Audit

The simplest way to audit a contract:

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";
import { readFileSync } from "node:fs";

async function main() {
    const client = new CopilotSolidityClient({ solidityVersion: "0.8.24" });

    try {
        await client.start();

        const source = readFileSync("contracts/Vault.sol", "utf-8");
        const report = await client.auditContract(source, "Vault");

        console.log(`Findings: ${report.findings.length}`);
        for (const f of report.findings) {
            console.log(`  [${f.severity}] ${f.title}`);
        }
    } finally {
        await client.stop();
    }
}

main();
```

## Audit with Gas Hints

Enable `includeGasHints` to get gas optimization suggestions alongside security findings:

```typescript
const client = new CopilotSolidityClient({
    solidityVersion: "0.8.24",
    includeGasHints: true,
});

await client.start();
const report = await client.auditContract(source, "MyToken");
await client.stop();
```

## Filtering Findings by Severity

Process only critical and high-severity findings:

```typescript
const report = await client.auditContract(source);

const critical = report.findings.filter(
    (f) => f.severity === "critical" || f.severity === "high",
);

if (critical.length > 0) {
    console.error(`Found ${critical.length} critical/high issues!`);
    for (const f of critical) {
        console.error(`  ${f.title} (${f.swcId ?? "no SWC"})`);
    }
    process.exitCode = 1;
}
```

## Audit Multiple Contracts

Iterate over all `.sol` files in a directory:

```typescript
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

async function auditAll(dir: string) {
    const client = new CopilotSolidityClient({ solidityVersion: "0.8.24" });
    await client.start();

    const files = readdirSync(dir).filter((f) => f.endsWith(".sol"));

    for (const file of files) {
        const source = readFileSync(join(dir, file), "utf-8");
        const report = await client.auditContract(source, file.replace(".sol", ""));
        console.log(`${file}: ${report.findings.length} findings`);
    }

    await client.stop();
}

auditAll("contracts");
```

## Interactive Session Audit

For a more conversational audit where you can ask follow-up questions:

```typescript
const session = await client.createSession();

await session.send({
    prompt: `Audit this contract for reentrancy:\n\`\`\`solidity\n${source}\n\`\`\``,
});

// Listen for the response
session.on((event) => {
    if (event.type === "assistant.message") {
        console.log("Audit result:", event.data);
    }
});

// Ask a follow-up
await session.send({ prompt: "Can you show the fixed version?" });
```

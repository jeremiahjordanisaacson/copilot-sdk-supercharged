# Gas Optimization

Recipes for finding gas optimization opportunities in Solidity contracts.

## Basic Gas Analysis

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";
import { readFileSync } from "node:fs";

async function main() {
    const client = new CopilotSolidityClient();

    try {
        await client.start();

        const source = readFileSync("contracts/Token.sol", "utf-8");
        const report = await client.optimizeGas(source, "Token");

        console.log(`Found ${report.optimizations.length} optimizations:\n`);
        for (const opt of report.optimizations) {
            console.log(`[${opt.category}] ${opt.title}`);
            console.log(`  Saves: ${opt.estimatedSavings ?? "unknown"}`);
            console.log(`  ${opt.description}\n`);
        }
    } finally {
        await client.stop();
    }
}

main();
```

## Filter by Category

Focus on storage-related optimizations (often the biggest wins):

```typescript
const report = await client.optimizeGas(source);

const storageOpts = report.optimizations.filter(
    (o) => o.category === "storage" || o.category === "packing",
);

console.log(`Storage optimizations: ${storageOpts.length}`);
for (const opt of storageOpts) {
    if (opt.before && opt.after) {
        console.log(`\nBefore:\n${opt.before}\nAfter:\n${opt.after}`);
    }
}
```

## Combine Audit and Gas Analysis

Run both analyses and merge results:

```typescript
async function fullAnalysis(source: string) {
    const client = new CopilotSolidityClient({
        solidityVersion: "0.8.24",
        includeGasHints: true,
    });
    await client.start();

    const [audit, gas] = await Promise.all([
        client.auditContract(source),
        client.optimizeGas(source),
    ]);

    console.log("=== Security ===");
    console.log(`${audit.findings.length} findings`);

    console.log("\n=== Gas ===");
    console.log(`${gas.optimizations.length} optimizations`);

    await client.stop();
    return { audit, gas };
}
```

## Common Gas Patterns

The SDK checks for these common patterns:

| Category | Examples |
|----------|----------|
| `storage` | Cache storage reads in memory, avoid redundant SLOADs |
| `packing` | Pack struct fields, use smaller uint types together |
| `calldata` | Use `calldata` instead of `memory` for read-only params |
| `loop` | Cache array length, avoid storage reads inside loops |
| `unchecked-math` | Use `unchecked {}` for arithmetic that cannot overflow |
| `short-circuit` | Order conditions cheapest-first in require/if |
| `memory` | Avoid unnecessary memory copies |
| `general` | Use custom errors, immutable/constant, payable admin functions |

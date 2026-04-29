# Test Generation

Recipes for generating smart-contract test suites with the Copilot Solidity SDK.

## Generate Hardhat Tests

Hardhat tests are generated in TypeScript using ethers.js and chai:

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";
import { readFileSync, writeFileSync } from "node:fs";

async function main() {
    const client = new CopilotSolidityClient({
        defaultTestFramework: "hardhat",
    });

    try {
        await client.start();

        const source = readFileSync("contracts/Token.sol", "utf-8");
        const suite = await client.generateTests(source, {
            contractName: "Token",
        });

        console.log(`Generated ${suite.testCount} tests for ${suite.contractName}`);
        writeFileSync("test/Token.test.ts", suite.code);
        console.log("Wrote test/Token.test.ts");
    } finally {
        await client.stop();
    }
}

main();
```

## Generate Foundry Tests

Switch to Foundry output (Solidity tests using forge-std):

```typescript
const suite = await client.generateTests(source, {
    contractName: "Vault",
    framework: "foundry",
});

writeFileSync("test/Vault.t.sol", suite.code);
```

## Generate Tests for All Contracts

Batch generate tests for every `.sol` file:

```typescript
import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join, basename } from "node:path";
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

async function generateAllTests(contractsDir: string, testsDir: string) {
    const client = new CopilotSolidityClient({
        defaultTestFramework: "hardhat",
    });
    await client.start();

    const files = readdirSync(contractsDir).filter((f) => f.endsWith(".sol"));

    for (const file of files) {
        const source = readFileSync(join(contractsDir, file), "utf-8");
        const name = basename(file, ".sol");
        const suite = await client.generateTests(source, { contractName: name });
        writeFileSync(join(testsDir, `${name}.test.ts`), suite.code);
        console.log(`${name}: ${suite.testCount} tests`);
    }

    await client.stop();
}

generateAllTests("contracts", "test");
```

## Using the Tool Directly in a Session

For more control, use the test generation tool inside a custom session:

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

async function main() {
    const client = new CopilotSolidityClient();
    await client.start();

    const session = await client.createSession();

    const response = await session.sendAndWait({
        prompt: [
            "Generate Foundry tests for the following contract.",
            "Include fuzz tests for the transfer function.",
            "```solidity",
            source,
            "```",
        ].join("\n"),
    });

    console.log(response?.data);
    await session.destroy();
    await client.stop();
}
```

## Supported Frameworks

| Framework | Output Language | Test Runner | Assertions |
|-----------|----------------|-------------|------------|
| `hardhat` | TypeScript | Mocha | Chai + ethers.js matchers |
| `foundry` | Solidity | forge test | forge-std assertions |

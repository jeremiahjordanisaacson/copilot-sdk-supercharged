# Error Handling

Patterns for handling errors when using the GitHub Copilot Solidity SDK.

## Basic Error Handling

Wrap calls in try/catch and always call `stop()` in a finally block.

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

async function main() {
    const client = new CopilotSolidityClient();

    try {
        await client.start();
        const report = await client.auditContract(sourceCode);
        console.log(report.summary);
    } catch (error) {
        console.error("SDK error:", error);
    } finally {
        await client.stop();
    }
}
```

## Handling Specific Error Types

Check error properties to distinguish between connection issues and audit failures.

```typescript
import { CopilotSolidityClient } from "copilot-sdk-supercharged-solidity";

async function main() {
    const client = new CopilotSolidityClient();

    try {
        await client.start();
        const report = await client.auditContract(sourceCode);
        console.log(report.summary);
    } catch (error: unknown) {
        if (error instanceof Error) {
            if (error.message.includes("ECONNREFUSED")) {
                console.error("Could not connect to Copilot CLI. Is it installed?");
            } else if (error.message.includes("timeout")) {
                console.error("Request timed out. Try again or simplify the contract.");
            } else {
                console.error("Unexpected error:", error.message);
            }
        }
    } finally {
        await client.stop();
    }
}
```

## Graceful Fallback for JSON Parsing

The high-level helpers (`auditContract`, `optimizeGas`, etc.) gracefully handle
cases where Copilot returns non-JSON or malformed output. Instead of throwing,
they return a fallback object with the raw response in the `summary` field.

```typescript
const report = await client.auditContract(sourceCode);

if (report.findings.length === 0 && report.summary) {
    // Copilot returned prose instead of structured JSON
    console.log("Raw analysis:", report.summary);
} else {
    for (const finding of report.findings) {
        console.log(`[${finding.severity}] ${finding.title}`);
    }
}
```

## Retry Logic

For non-deterministic failures, implement a simple retry wrapper.

```typescript
async function withRetry<T>(
    fn: () => Promise<T>,
    maxRetries = 3,
    delayMs = 1000,
): Promise<T> {
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
        try {
            return await fn();
        } catch (error) {
            if (attempt === maxRetries) throw error;
            console.warn(`Attempt ${attempt} failed, retrying in ${delayMs}ms...`);
            await new Promise((r) => setTimeout(r, delayMs));
        }
    }
    throw new Error("Unreachable");
}

const report = await withRetry(() => client.auditContract(sourceCode));
```

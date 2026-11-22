/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import type { CopilotSession } from "../src/index.js";

/** Import one explicit memory without inference or a local-store fallback. */
export async function importExampleMemory(session: CopilotSession) {
    const result = await session.rpc.memory.importRecord({
        scope: "user",
        subject: "Example test command",
        fact: "The example project runs tests with pnpm test.",
        reason: "Use the project's test command to validate future changes. This keeps local checks consistent with the project configuration.",
        citations: "example/package.json:scripts.test",
    });
    console.log("Memory import:", result.status);
    return result;
}

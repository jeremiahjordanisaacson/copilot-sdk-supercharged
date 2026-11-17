/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import assert from "node:assert/strict";
import { approveAll, CopilotClient } from "../dist/index.js";

const cliPath = process.argv[2];
if (!cliPath) {
    throw new Error("Usage: node examples/agent-list.mjs <copilot-runtime-path>");
}

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: { kind: "spawned-stdio", path: cliPath },
    github: {
        useLoggedInUser: false,
    },
});
try {
    const session = await client.createSession({
        model: "example-model",
        github: { capi: { enabled: false } },
        inference: {
            defaultProvider: "example",
            providers: [
                {
                    name: "example",
                    type: "openai",
                    baseUrl: "http://127.0.0.1:1",
                    apiKey: "example",
                },
            ],
        },
        permissions: {
            onRequest: approveAll,
        },
        agents: {
            definitions: [
                {
                    name: "reviewer",
                    description: "Reviews a proposed change",
                    prompt: "Review the change without modifying files.",
                },
            ],
        },
    });
    try {
        const request = Object.freeze({ includePrompt: true });
        const metadata = await session.rpc.agent.list();
        const authored = await session.rpc.agent.list(request);
        const builtinAgentFlagOverrides = await session.rpc.tools.getBuiltinAgentFlagOverrides();
        assert.equal(typeof builtinAgentFlagOverrides.removeTaskSubagent, "boolean");
        assert.equal(metadata.agents[0].prompt, undefined);
        assert.equal(authored.agents[0].prompt, "Review the change without modifying files.");
        assert.deepEqual(request, { includePrompt: true });
        console.log(
            JSON.stringify({
                name: authored.agents[0].name,
                metadataOmitsPrompt: metadata.agents[0].prompt === undefined,
                prompt: authored.agents[0].prompt,
                builtinAgentFlagOverrides,
                request,
            })
        );
    } finally {
        await session.disconnect();
    }
} finally {
    await client.stop();
}

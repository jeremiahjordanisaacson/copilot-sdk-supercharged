/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import assert from "node:assert/strict";
import { CopilotClient } from "@microsoft/copilot-sdk";

const runtimePath = process.argv[2];
if (!runtimePath) {
    throw new Error("Usage: node examples/agent-file-authoring.mjs <runtime-path> [project-path]");
}
const client = new CopilotClient({
    mode: "coding-agent",
    runtime: { kind: "spawned-stdio", path: runtimePath },
    github: { useLoggedInUser: false },
});
try {
    await client.start();
    const agents = client.rpc.agents;
    const request = Object.freeze({
        name: "Code reviewer",
        description: "Reviews code without edits",
        tools: ["read", "search"],
        instructions: "Check the proposed change.",
    });
    assert.equal((await agents.validateName({ name: request.name })).valid, true);
    const { categories } = await agents.listToolCategories();
    const { content } = await agents.generateFileContent(request);
    assert.ok(content.includes("tools: ['read', 'search']"));
    let created;
    if (process.argv[3]) {
        created = await agents.createFile({
            name: request.name,
            description: request.description,
            instructions: request.instructions,
            enabledCategories: ["read"],
            location: "project",
            projectPath: process.argv[3],
            userConfigDir: process.argv[3],
        });
        assert.equal(created.kind, "success", JSON.stringify(created));
    }
    console.log(JSON.stringify({ categories: categories.map(({ id }) => id), content, created }));
} finally {
    await client.stop();
}

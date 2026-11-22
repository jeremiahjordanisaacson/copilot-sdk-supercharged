/* Copyright (c) Microsoft Corporation. All rights reserved. */

// Run with the matching checkout's runtime: node src/sdk/nodejs/examples/session-folder-trust.mjs /path/to/copilot-runtime
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdir, mkdtemp, realpath, rm } from "node:fs/promises";
import { join, resolve } from "node:path";
import { approveAll, CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const runtimePath = process.argv[2];
if (!runtimePath) {
    throw new Error("Usage: node session-folder-trust.mjs <same-checkout copilot-runtime path>");
}
const home = await mkdtemp(resolve(".copilot-session-trust-example-"));
const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forSpawnedStdio({
        path: resolve(runtimePath),
        env: { ...process.env, COPILOT_HOME: join(home, "profile") },
    }),
    github: { useLoggedInUser: false },
});
try {
    const directories = [];
    for (const name of ["accepted-workspace", "other-workspace"]) {
        const path = join(home, name);
        await mkdir(path);
        execFileSync("git", ["init", "--quiet"], { cwd: path, windowsHide: true });
        directories.push(await realpath(path));
    }
    const [path, otherPath] = directories;
    /** @type {import("@microsoft/copilot-sdk").CreateSessionOptions} */
    const config = {
        workingDirectory: path,
        enableConfigDiscovery: true,
        model: "example/example-model",
        inference: {
            providers: [
                {
                    name: "example",
                    type: "openai",
                    baseUrl: "http://127.0.0.1:1",
                    apiKey: "example",
                },
            ],
            models: [{ provider: "example", id: "example-model" }],
        },
        github: { capi: { enabled: false } },
        permissions: { onRequest: approveAll },
    };
    const session = await client.createSession(config);
    const sibling = await client.createSession(config);
    assert.equal((await session.rpc.permissions.folderTrust.isTrusted({ path })).trusted, false);

    await session.rpc.permissions.folderTrust.addSessionTrusted({ path });
    const accepted = await session.rpc.permissions.folderTrust.isTrusted({ path });
    const siblingTrust = await sibling.rpc.permissions.folderTrust.isTrusted({ path });
    assert.equal(accepted.trusted, true);
    assert.equal(siblingTrust.trusted, false);

    await session.rpc.metadata.setWorkingDirectory({ workingDirectory: otherPath });
    await assert.rejects(
        session.rpc.permissions.folderTrust.addSessionTrusted({ path }),
        /workspace changed before accepting folder trust/
    );
    const afterNavigation = await session.rpc.permissions.folderTrust.isTrusted({ path });
    assert.equal(afterNavigation.trusted, false);
    console.log(
        JSON.stringify({
            acceptedForThisSession: accepted.trusted,
            siblingSessionTrusted: siblingTrust.trusted,
            oldDirectoryTrustedAfterNavigation: afterNavigation.trusted,
            staleDirectoryRejected: true,
        })
    );
} finally {
    try {
        await client.stop();
    } finally {
        await rm(home, { recursive: true, force: true });
    }
}

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import type { CopilotClient } from "../src/index.js";

/** Inspect a checkout without merge-base probes or network requests. */
export async function inspectGitRepository(client: CopilotClient, cwd: string) {
    const root = await client["internalRpc"].git.findRoot({ cwd });
    if (!root.found) {
        return null;
    }
    const [context, status] = await Promise.all([
        client["internalRpc"].git.workingDirectoryContext({ cwd, includeBaseCommit: false }),
        client["internalRpc"].git.workingTreeStatus({ cwd }),
    ]);
    return { root: root.gitRoot, context, status };
}

/** Create a task checkout and try to inherit the main repository's persisted trust. */
export async function createTaskWorktree(
    client: CopilotClient,
    cwd: string,
    branch: string,
    configDir: string | null = null
) {
    const worktree = await client["internalRpc"].git.createWorktreeFromBase({
        gitRoot: cwd,
        branch,
        baseRef: "HEAD",
        allowSuffix: true,
    });
    try {
        await client["internalRpc"].git.inheritWorktreeTrust({
            source: cwd,
            destination: worktree.path,
            configDir,
        });
    } catch (error) {
        console.warn("Could not inherit trust for the created worktree:", worktree.path, error);
    }
    return worktree;
}

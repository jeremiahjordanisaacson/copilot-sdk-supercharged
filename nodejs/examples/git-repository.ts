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

/** Create a checkout for an editor task without changing the source checkout. */
export async function createTaskWorktree(client: CopilotClient, cwd: string, branch: string) {
    return client["internalRpc"].git.createWorktreeFromBase({
        gitRoot: cwd,
        branch,
        baseRef: "HEAD",
        allowSuffix: true,
    });
}

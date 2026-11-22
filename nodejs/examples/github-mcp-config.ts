/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import type { CopilotClient } from "../src/index.js";

/** Preview the exposure an IDE host will supply before it creates a session. */
export async function previewGitHubMcpConfig(client: CopilotClient) {
    return client["internalRpc"].mcp.resolveGitHubConfig({
        settings: { githubMcpToolsets: ["projects"] },
        flags: { additionalTools: ["get_file_contents"] },
        session: { disableFormDeferral: true },
    });
}

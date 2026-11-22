/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import type { CopilotClient, CopilotSession } from "../src/index.js";

/** Restore retained marketplace installs before a launcher opens a session. */
export async function migrateRetiredMarketplace(client: CopilotClient) {
    const result = await client.rpc.plugins.marketplaces.migrateRetiredDefault();
    console.log("Marketplace migration:", JSON.stringify(result));
    return result;
}

/** Inspect one invocation without a session or settings changes. */
export async function inspectInvocation(
    client: CopilotClient,
    workingDirectory: string,
    pluginDirectories: string[] = []
) {
    const invocation = { workingDirectory, pluginDirectories };
    const plugins = await client.rpc.plugins.list(invocation);
    const mcp = await client.rpc.mcp.discover({
        ...invocation,
        includeConfiguration: true,
    });
    const result = { plugins, mcp };
    console.log(
        "Invocation discovery:",
        JSON.stringify({
            pluginCount: plugins.plugins.length,
            pluginWarningCount: plugins.warnings?.length ?? 0,
            mcpServerCount: mcp.servers.length,
            mcpDiagnosticCount: mcp.diagnostics?.length ?? 0,
        })
    );
    return result;
}

/** Optionally change ambient discovery, then inspect catalogs without updating installed versions. */
export async function inspectPluginCatalog(
    session: CopilotSession,
    query: string,
    installedPlugin?: { name: string; marketplace: string },
    enableConfigDiscovery?: boolean
) {
    if (enableConfigDiscovery !== undefined)
        await session.rpc.options.update({ enableConfigDiscovery });
    const catalog = await session.rpc.plugins.marketplaces.search({ query });
    const pluginUpdate = installedPlugin
        ? await session.rpc.plugins.checkUpdate(installedPlugin)
        : undefined;
    const marketplaceUpdate = installedPlugin
        ? await session.rpc.plugins.marketplaces.checkUpdate({ name: installedPlugin.marketplace })
        : undefined;
    const result = { catalog, pluginUpdate, marketplaceUpdate };
    console.log("Plugin discovery:", JSON.stringify(result));
    return result;
}

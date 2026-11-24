/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { pathToFileURL } from "node:url";
import {
    CopilotClient,
    RuntimeConnection,
    type CopilotSession,
    type McpSecretsDeleteRequest,
    type McpSecretsSaveRequest,
    type McpSecretsSaveResult,
    type SessionsHasEventsRequest,
    type SessionsHasEventsResult,
    type ShellBuildInteractiveEnvironmentRequest,
    type ShellBuildInteractiveEnvironmentResult,
} from "../src/index.js";
import type { createInternalServerRpc, createInternalSessionRpc } from "../src/generated/rpc.js";

/** Follow a cloud task with the session's credential owner, without a setup bearer. */
export async function followCloudTask(
    session: CopilotSession,
    request: Omit<
        Parameters<
            ReturnType<typeof createInternalSessionRpc>["remote"]["attachMissionControl"]
        >[0],
        "publicSessionId"
    >
) {
    return session["internalRpc"].remote.attachMissionControl({
        ...request,
        publicSessionId: session.sessionId,
    });
}

/** Read the connection target before a host attaches to a remote session. */
export async function readCloudSessionTarget(
    tasks: ReturnType<typeof createInternalServerRpc>["cloudTasks"],
    request: Parameters<ReturnType<typeof createInternalServerRpc>["cloudTasks"]["getSession"]>[0],
    credentialOwner?: CopilotSession
) {
    const result = await tasks.getSession({
        ...request,
        ...(credentialOwner ? { credentialSessionId: credentialOwner.sessionId } : {}),
    });
    if (!result.ok) throw new Error(result.errorMessage ?? "Cannot read cloud session");
    return JSON.parse(result.payloadJson ?? "null") as unknown;
}

/** Require a current credential before a host deletes a cloud task. */
export async function deleteAuthenticatedCloudTask(
    tasks: ReturnType<typeof createInternalServerRpc>["cloudTasks"],
    request: Parameters<ReturnType<typeof createInternalServerRpc>["cloudTasks"]["delete"]>[0]
) {
    const result = await tasks.delete({ ...request, requireCredential: true });
    return {
        credentialResolved: result.credentialResolved,
        deleted: result.ok,
        error: result.errorMessage,
    };
}

/** Check a saved session and prepare a shell environment without exposing host credentials. */
export async function inspectHostHelperServices(
    client: CopilotClient,
    sessionId: string
): Promise<string> {
    const { exists }: SessionsHasEventsResult = await client.rpc.sessions.hasEvents({
        sessionId,
    } satisfies SessionsHasEventsRequest);
    const { environment }: ShellBuildInteractiveEnvironmentResult =
        await client.rpc.shell.buildInteractiveEnvironment({
            hostEnvironment: {
                PATH: process.env.PATH ?? "",
                GITHUB_TOKEN: "example-secret-that-must-not-reach-the-shell",
                COPILOT_SUPERVISED: "1",
            },
            additionalBlockedNames: ["COPILOT_SUPERVISED"],
            caseInsensitive: process.platform === "win32",
        } satisfies ShellBuildInteractiveEnvironmentRequest);
    return JSON.stringify(
        {
            hasEvents: exists,
            shellVariableNames: Object.keys(environment).sort(),
            gitConfig: environment.GIT_CONFIG_VALUE_0,
        },
        null,
        2
    );
}

/** Stage a credential for an MCP configuration UI, then remove the unused credential. */
export async function stageMcpCredential(client: CopilotClient) {
    const secretId = "example-mcp/api-key";
    const result: McpSecretsSaveResult = await client.rpc.mcp.secrets.save({
        secretId,
        value: "example-credential",
    } satisfies McpSecretsSaveRequest);
    await client.rpc.mcp.secrets.delete({ secretId } satisfies McpSecretsDeleteRequest);
    return {
        placeholder: "${secret:example-mcp/api-key}",
        usedKeychain: result.usedKeychain,
        removed: true,
    };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
    const directory = process.argv[2];
    if (!directory) {
        throw new Error("Usage: tsx examples/host-helper-services.ts <isolated-config-directory>");
    }
    const client = new CopilotClient({
        mode: "coding-agent",
        runtime: RuntimeConnection.forSpawnedStdio({
            path: process.env.COPILOT_CLI_PATH,
            baseDirectory: directory,
        }),
    });
    try {
        await client.start();
        console.log(JSON.stringify(await stageMcpCredential(client)));
    } finally {
        await client.stop();
    }
}

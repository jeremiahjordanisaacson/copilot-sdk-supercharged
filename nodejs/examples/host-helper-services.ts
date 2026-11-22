/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import type {
    CopilotClient,
    CopilotSession,
    SessionsHasEventsRequest,
    SessionsHasEventsResult,
    ShellBuildInteractiveEnvironmentRequest,
    ShellBuildInteractiveEnvironmentResult,
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

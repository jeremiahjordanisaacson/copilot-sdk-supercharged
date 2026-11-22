/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { randomUUID } from "node:crypto";
import type { CopilotSession } from "../src/index.js";

/** Import an external conversation without model requests or tool execution. */
export async function importExampleConversation(session: CopilotSession) {
    const toolCallId = randomUUID();
    await session.rpc.history.importConversation({
        events: [
            { type: "user.message", data: { content: "What is 2 + 2?" } },
            {
                type: "assistant.message",
                data: {
                    messageId: randomUUID(),
                    content: "I will use a calculator.",
                    toolRequests: [
                        { toolCallId, name: "calculator", arguments: { expression: "2+2" } },
                    ],
                },
            },
            {
                type: "tool.execution_start",
                data: { toolCallId, toolName: "calculator", arguments: { expression: "2+2" } },
            },
            {
                type: "tool.execution_complete",
                data: { toolCallId, success: true, result: { content: "4" } },
            },
            {
                type: "assistant.message",
                data: { messageId: randomUUID(), content: "The answer is 4." },
            },
        ],
    });
    const events = await session.getEvents();
    console.log("Imported conversation:", session.sessionId);
    return events;
}

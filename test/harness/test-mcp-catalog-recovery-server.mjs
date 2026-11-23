#!/usr/bin/env node
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * Raw stdio MCP server fixture that reproduces transient tool-discovery failures:
 * a stalled `tools/list`, a failing retry, and a later recovery, plus a stalled
 * `tools/call`. Every inbound method is appended to a traffic log so the test can
 * assert what the runtime actually sent.
 *
 * Usage: node test-mcp-catalog-recovery-server.mjs <trafficPath> <startupReleasePath> <recoveryReleasePath>
 *
 * When `startupReleasePath` is absent at startup the server holds its first
 * `tools/list` (the cached-startup scenario) until the file appears.
 */

import { appendFileSync, existsSync } from "node:fs";
import { createInterface } from "node:readline";

const trafficPath = process.argv[2];
appendFileSync(trafficPath, "started\n");
const startupReleasePath = process.argv[3];
const recoveryReleasePath = process.argv[4];
const cachedStartup = startupReleasePath !== undefined && !existsSync(startupReleasePath);
let listingState = cachedStartup ? "timeout" : "healthy";
let failStartupRetry = cachedStartup;
let stalledTool;
let stalledList;

if (cachedStartup) {
    appendFileSync(trafficPath, "startup-held\n");
    while (!existsSync(startupReleasePath)) {
        await new Promise((resolve) => setTimeout(resolve, 50));
    }
}

function send(message) {
    process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", ...message })}\n`);
}

function result(id, value) {
    send({ id, result: value });
}

for await (const line of createInterface({ input: process.stdin })) {
    const message = JSON.parse(line);
    const { id, method, params } = message;
    appendFileSync(trafficPath, `${method}${method === "tools/call" ? `:${params?.name}` : ""}\n`);
    if (method === "notifications/cancelled") {
        if (stalledTool !== undefined && params?.requestId === stalledTool) {
            stalledTool = undefined;
            appendFileSync(trafficPath, "tool-cancelled\n");
        }
        if (stalledList !== undefined && params?.requestId === stalledList) {
            stalledList = undefined;
            // Recover without another list-changed notification: the client
            // must retry discovery rather than forget the failed refresh.
            listingState = failStartupRetry ? "retry-failure" : "recovered";
            failStartupRetry = false;
            appendFileSync(trafficPath, "list-cancelled\n");
        }
    } else if (id === undefined) {
        continue;
    } else if (method === "initialize") {
        result(id, {
            protocolVersion: params?.protocolVersion,
            capabilities: { tools: { listChanged: true } },
            serverInfo: { name: "catalog-recovery", version: "1.0.0" },
        });
    } else if (method === "tools/list") {
        if (listingState === "retry-failure") {
            if (!existsSync(recoveryReleasePath)) {
                appendFileSync(trafficPath, "list-failed\n");
                send({ id, error: { code: -32603, message: "Snapshot retry still unavailable" } });
                continue;
            }
            listingState = "recovered";
        }
        if (listingState === "timeout") {
            stalledList = id;
            continue;
        }
        result(id, {
            tools: ["ping", "stall", "refresh", listingState === "recovered" ? "recovered" : "obsolete"].map(
                (name) => ({
                    name,
                    description: `Catalog recovery fixture: ${name}`,
                    inputSchema: { type: "object", properties: {} },
                    annotations: { readOnlyHint: true },
                })
            ),
        });
    } else if (method === "tools/call") {
        if (params?.name === "stall") {
            stalledTool = id;
            continue;
        }
        if (params?.name === "refresh") {
            listingState = "timeout";
            send({ method: "notifications/tools/list_changed" });
        }
        result(id, {
            content: [
                { type: "text", text: params?.name === "recovered" ? "RECOVERED_TOOL_REPLY" : "PROBE_REPLY" },
            ],
        });
    } else {
        send({ id, error: { code: -32601, message: "Method not found" } });
    }
}

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { Agent, request } from "node:http";
import { test } from "vitest";
import { startSessionExpiryMcpServer } from "./test-mcp-session-expiry-server.mjs";

test("legacy session expiry is independent of HTTP connection lifetime even for modern clients", async ({
  expect,
  onTestFinished,
}) => {
  const server = await startSessionExpiryMcpServer();
  onTestFinished(() => server.close());
  const agent = new Agent({ keepAlive: true });
  onTestFinished(() => agent.destroy());
  const probeSockets = new Set();

  const initialize = () =>
    fetch(`${server.url}/mcp`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "initialize",
        params: {
          protocolVersion: "2026-07-28",
          capabilities: {},
          clientInfo: { name: "session-expiry-test", version: "1" },
        },
      }),
    });
  const initialized = await initialize();
  expect(initialized.status).toBe(200);
  expect(initialized.headers.get("connection")).toBe("close");
  expect((await initialized.json()).result.protocolVersion).toBe("2025-03-26");
  const sessionId = initialized.headers.get("mcp-session-id");
  expect(sessionId).toBeTruthy();

  const ping = async () => {
    const response = await new Promise((resolve, reject) => {
      const req = request(
        `${server.url}/mcp`,
        {
          agent,
          method: "POST",
          headers: {
            "content-type": "application/json",
            "mcp-session-id": sessionId,
          },
        },
        (res) => {
          const socket = res.socket;
          let body = "";
          res.setEncoding("utf8");
          res.on("data", (chunk) => (body += chunk));
          res.on("error", reject);
          res.on("end", () =>
            resolve({ status: res.statusCode, headers: res.headers, socket, body })
          );
        }
      );
      req.on("error", reject);
      req.end(JSON.stringify({ jsonrpc: "2.0", id: 2, method: "ping" }));
    });
    expect(response.headers.connection).toBe("close");
    expect(probeSockets.has(response.socket)).toBe(false);
    probeSockets.add(response.socket);
    return { status: response.status, body: JSON.parse(response.body) };
  };
  const alive = await ping();
  expect(alive.status).toBe(200);
  expect(alive.body).toEqual({ jsonrpc: "2.0", id: 2, result: {} });

  const expired = await fetch(`${server.url}/__expire`, { method: "POST" });
  expect(expired.status).toBe(200);
  expect(await expired.json()).toEqual({ expired: 1 });
  for (let probe = 0; probe < 3; probe++) {
    const afterExpiry = await ping();
    expect(afterExpiry.status).toBe(404);
    expect(afterExpiry.body).toEqual({ error: "session_expired" });
  }
  const observed = await fetch(`${server.url}/__stats`).then((response) => response.json());
  expect(observed.pings).toBe(1);
  expect(observed.expiredRequests).toBe(3);
  expect(observed.expiredRequestLog).toEqual(
    Array.from({ length: 3 }, () => ({ method: "POST", elapsedMs: expect.any(Number) })),
  );
  expect(observed.protocolVersions).toEqual(["2026-07-28"]);
  expect(observed.recentRequests).toContainEqual(
    expect.objectContaining({
      httpMethod: "POST",
      sessionAttached: true,
      expiredSession: true,
      status: 404,
    }),
  );
  expect(JSON.stringify(observed)).not.toContain(sessionId);

  const reinitialized = await initialize();
  expect(reinitialized.status).toBe(200);
  expect((await reinitialized.json()).result.protocolVersion).toBe("2025-03-26");
  expect(reinitialized.headers.get("mcp-session-id")).not.toBe(sessionId);
  expect(server.stats.initializations).toBe(2);
});

test("distinguishes answered expiry probes from unrelated traffic", async ({
  expect,
  onTestFinished,
}) => {
  const startingServer = startSessionExpiryMcpServer();
  onTestFinished(async () => (await startingServer).close());
  const server = await startingServer;

  const initialized = await fetch(`${server.url}/mcp`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "initialize" }),
  });
  const sessionId = initialized.headers.get("mcp-session-id");
  expect(initialized.status).toBe(200);
  expect(sessionId).toBeTruthy();
  await initialized.json();

  const request = (method) =>
    fetch(`${server.url}/mcp`, {
      method: "POST",
      headers: { "content-type": "application/json", "mcp-session-id": sessionId },
      body: JSON.stringify({ jsonrpc: "2.0", id: 2, method }),
    });
  const alive = await request("ping");
  expect(alive.status).toBe(200);
  expect(await alive.json()).toEqual({ jsonrpc: "2.0", id: 2, result: {} });
  expect(server.stats.expiredSessionProbes).toBe(0);

  const expired = await fetch(`${server.url}/__expire`, { method: "POST" });
  expect(expired.status).toBe(200);
  expect(await expired.json()).toEqual({ expired: 1 });
  expect(server.stats.expiredSessionProbes).toBe(0);

  const ordinary = await request("tools/list");
  expect(ordinary.status).toBe(404);
  expect(await ordinary.json()).toEqual({ error: "session_expired" });
  expect(server.stats.expiredSessionProbes).toBe(0);

  const probe = await request("ping");
  expect(probe.status).toBe(404);
  expect(await probe.json()).toEqual({ error: "session_expired" });
  expect(server.stats.expiredSessionProbes).toBe(1);
  const observed = await fetch(`${server.url}/__stats`);
  expect(observed.status).toBe(200);
  expect(await observed.json()).toMatchObject({ activeSessions: 0, expiredSessionProbes: 1 });
});

test("bounds session-expiry diagnostics without retaining request payloads", async ({ expect, onTestFinished }) => {
  const startingServer = startSessionExpiryMcpServer();
  onTestFinished(async () => (await startingServer).close());
  const server = await startingServer;
  for (let i = 0; i < 40; i++) {
    const response = await fetch(`${server.url}/mcp`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: `private-request-${i}`,
        method: "initialize",
        params: {
          protocolVersion: "2025-03-26",
          clientInfo: { name: "private-client", version: "1" },
        },
      }),
    });
    expect(response.status).toBe(200);
    await response.json();
  }
  const observed = await fetch(`${server.url}/__stats`).then((response) => response.json());
  expect(observed.initializations).toBe(40);
  expect(observed.recentRequests).toHaveLength(32);
  for (const request of observed.recentRequests) {
    expect(request).toEqual({
      elapsedMs: expect.any(Number),
      httpMethod: "POST",
      sessionAttached: false,
      expiredSession: false,
      rpcMethods: ["initialize"],
      status: 200,
    });
  }
  expect(JSON.stringify(observed)).not.toContain("private-");
});

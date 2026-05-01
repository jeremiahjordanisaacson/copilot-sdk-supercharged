/**
 * E2E test harness for the Solidity SDK.
 *
 * Spawns the shared replay proxy (npx tsx test/harness/server.ts),
 * parses the `Listening: http://...` URL from stdout, and provides
 * helpers for E2E tests.
 */

import { spawn, type ChildProcess } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export interface ProxyHandle {
    url: string;
    process: ChildProcess;
}

/**
 * Start the replay proxy and return its URL + process handle.
 */
export async function startProxy(): Promise<ProxyHandle> {
    const serverPath = path.resolve(
        __dirname,
        "..",
        "..",
        "test",
        "harness",
        "server.ts",
    );
    const cwd = path.dirname(serverPath);

    const child = spawn("npx", ["tsx", serverPath], {
        cwd,
        stdio: ["pipe", "pipe", "inherit"],
        shell: process.platform === "win32",
    });

    const url = await new Promise<string>((resolve, reject) => {
        const timeout = setTimeout(() => {
            child.kill();
            reject(new Error("Proxy start timeout"));
        }, 30_000);

        child.stdout!.on("data", (chunk: Buffer) => {
            const line = chunk.toString();
            const match = line.match(/Listening: (http:\/\/[^\s]+)/);
            if (match) {
                clearTimeout(timeout);
                resolve(match[1]);
            }
        });

        child.on("error", (err) => {
            clearTimeout(timeout);
            reject(err);
        });

        child.on("exit", (code) => {
            clearTimeout(timeout);
            reject(new Error(`Proxy exited with code ${code}`));
        });
    });

    process.env.COPILOT_API_URL = url;
    return { url, process: child };
}

/**
 * Stop the replay proxy.
 */
export function stopProxy(handle: ProxyHandle): void {
    try {
        handle.process.kill();
    } catch {
        // best effort
    }
}

/**
 * Configure the proxy for a specific test snapshot.
 */
export async function configureProxy(
    proxyUrl: string,
    filePath: string,
    workDir: string,
): Promise<void> {
    await fetch(`${proxyUrl}/config`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ filePath, workDir }),
    });
}

/**
 * HTTP POST helper against the proxy.
 */
export async function proxyPost(
    proxyUrl: string,
    path: string,
    body: unknown,
): Promise<Response> {
    return fetch(`${proxyUrl}${path}`, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            Authorization: "Bearer fake-token-for-e2e-tests",
        },
        body: JSON.stringify(body),
    });
}

/**
 * HTTP GET helper against the proxy.
 */
export async function proxyGet(
    proxyUrl: string,
    path: string,
): Promise<Response> {
    return fetch(`${proxyUrl}${path}`);
}

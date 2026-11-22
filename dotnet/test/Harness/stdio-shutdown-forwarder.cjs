/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

const fs = require("node:fs");
const { spawn } = require("node:child_process");
const { dirname } = require("node:path");

const [role, fixture, marker, mode, pidFile, generation] = process.argv.slice(2);
if (role === "runtime") {
    const restarted = fs.existsSync(pidFile);
    fs.writeFileSync(pidFile, String(process.pid));
    const relay = spawn(
        process.execPath,
        [__filename, "relay", fixture, marker, mode, pidFile, restarted ? "restarted" : "initial"],
        {
            // Windows libuv kills non-detached children when the parent's job handle closes.
            // The relay must retain its inherited stdout until the test releases it.
            detached: true,
            stdio: ["inherit", "inherit", "inherit", "ipc"],
        },
    );
    relay.on("error", (error) => {
        throw error;
    });
    relay.on("message", (message) => {
        if (message === "fixture-exited") process.exit(0);
    });
    relay.on("exit", (code) => process.exit(code || 1));
} else if (role === "relay") {
    fs.writeFileSync(`${pidFile}.relay`, String(process.pid));
    const child = spawn(process.execPath, [fixture, marker, mode, `${pidFile}.inner`], {
        stdio: ["pipe", "pipe", "inherit"],
    });
    child.on("error", (error) => {
        throw error;
    });
    process.stdin.pipe(child.stdin);
    child.stdin.on("error", (error) => {
        if (error.code !== "EPIPE") throw error;
    });
    let buffered = Buffer.alloc(0);
    const held = [];
    let holding = false;
    let startupLifecycle = generation === "restarted";
    child.stdout.on("data", (chunk) => {
        buffered = Buffer.concat([buffered, chunk]);
        while (true) {
            const headerEnd = buffered.indexOf("\r\n\r\n");
            if (headerEnd < 0) return;
            const length = Number(/Content-Length:\s*(\d+)/i.exec(buffered.subarray(0, headerEnd).toString())[1]);
            const end = headerEnd + 4 + length;
            if (buffered.length < end) return;
            const frame = buffered.subarray(0, end);
            const message = JSON.parse(buffered.subarray(headerEnd + 4, end));
            buffered = buffered.subarray(end);
            if (message.method === "session.lifecycle") {
                holding = true;
                const next = JSON.stringify({
                    ...message,
                    params: { ...message.params, sessionId: "shutdown-drain-next" },
                });
                process.stdout.write(
                    Buffer.concat([frame, Buffer.from(`Content-Length: ${Buffer.byteLength(next)}\r\n\r\n${next}`)]),
                );
            } else if (holding) {
                held.push(frame);
            } else {
                if (startupLifecycle) {
                    startupLifecycle = false;
                    const event = JSON.stringify({
                        jsonrpc: "2.0",
                        method: "session.lifecycle",
                        params: { type: "session.updated", sessionId: "restart-drain" },
                    });
                    process.stdout.write(`Content-Length: ${Buffer.byteLength(event)}\r\n\r\n${event}`);
                }
                process.stdout.write(frame);
            }
        }
    });
    child.on("close", (code) => {
        if (code !== 0 || !holding || buffered.length !== 0) {
            throw new Error(`Unexpected fixture close: code=${code}; holding=${holding}; remaining=${buffered.length}`);
        }
        process.stdin.unpipe(child.stdin);
        const release = `${marker}.release`;
        const forward = () => {
            if (!fs.existsSync(release)) return;
            watcher.close();
            process.stdout.write(Buffer.concat(held), () => process.exit(0));
        };
        const watcher = fs.watch(dirname(marker), forward);
        watcher.on("error", (error) => {
            throw error;
        });
        fs.writeFileSync(`${marker}.held.tmp`, Buffer.concat(held));
        fs.renameSync(`${marker}.held.tmp`, `${marker}.held`);
        process.send("fixture-exited");
        forward();
    });
} else {
    throw new Error(`Unexpected forwarder role: ${role}`);
}

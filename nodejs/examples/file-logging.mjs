/* Copyright (c) Microsoft Corporation. All rights reserved. */

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { CopilotClient, RuntimeConnection } from "../dist/index.js";

const [runtimePath, outputPath] = process.argv.slice(2);
if (!runtimePath || !outputPath) {
    throw new Error("Usage: node examples/file-logging.mjs <runtime-path> <output-path>");
}
const path = resolve(outputPath);
const client = new CopilotClient({
    runtime: RuntimeConnection.forSpawnedStdio({
        path: runtimePath,
        // The native wrapper binds this SDK host setting before it reads RPC requests.
        env: {
            ...process.env,
            COPILOT_SDK_FILE_LOG_PATH: path,
            COPILOT_ENABLE_SECRET_FILTERING: "true",
        },
    }),
    mode: "coding-agent",
    github: { useLoggedInUser: false },
});
try {
    await client.start();
    const resource = await client.rpc.logging.file.open({
        secretValues: ["example-secret"],
    });
    const filtered = await client.rpc.secrets.filter({
        text: JSON.stringify({ message: "SDK host output: example-secret" }),
        format: "json",
    });
    assert(filtered.changed);
    assert(!filtered.value.includes("example-secret"));
    console.log(filtered.value);
    await client.rpc.logging.file.write({
        ...resource,
        operation: "info",
        message: "SDK file logging example: example-secret",
    });
    await client.rpc.logging.file.flush(resource);
    await client.rpc.logging.file.close(resource);
    const output = await readFile(path, "utf8");
    assert(output.includes("SDK file logging example:"));
    assert(!output.includes("example-secret"));
    console.log(JSON.stringify({ path, redacted: true, closed: true }));
} finally {
    const errors = await client.stop();
    if (errors.length) throw new AggregateError(errors, "SDK client shutdown failed");
}

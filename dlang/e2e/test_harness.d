/// E2E test harness for the D Copilot SDK.
///
/// Spawns the shared replay proxy (npx tsx test/harness/server.ts),
/// parses the `Listening: http://...` URL from stdout, and provides
/// helpers for E2E tests.
module e2e.test_harness;

import std.process : spawnProcess, kill, Redirect, Pipe, pipe, wait,
                     pipeProcess, Config;
import std.stdio : File, stderr, writefln;
import std.string : strip, indexOf;
import std.regex : regex, matchFirst;
import std.conv : to;
import std.net.curl : HTTP, get, post;
import std.exception : enforce;

/// Handle to the running replay proxy.
struct ProxyHandle
{
    string url;
    int pid;

    /// POST JSON to a proxy endpoint.
    string httpPost(string endpoint, string jsonBody)
    {
        try
        {
            auto http = HTTP();
            http.addRequestHeader("Content-Type", "application/json");
            http.addRequestHeader("Authorization", "Bearer fake-token-for-e2e-tests");
            auto response = cast(string) .post(url ~ endpoint, jsonBody, http);
            return response;
        }
        catch (Exception e)
        {
            return "";
        }
    }

    /// GET from a proxy endpoint.
    string httpGet(string endpoint)
    {
        try
        {
            return cast(string) .get(url ~ endpoint);
        }
        catch (Exception e)
        {
            return "";
        }
    }

    /// Configure the proxy for a specific test snapshot.
    void configure(string filePath, string workDir)
    {
        import std.format : format;
        auto body = format!`{"filePath":"%s","workDir":"%s"}`(filePath, workDir);
        httpPost("/config", body);
    }
}

/// Locate the Copilot CLI for E2E runs, in order:
/// 1. `COPILOT_CLI_PATH` when it points to an existing file
/// 2. nodejs/node_modules/@github/copilot/npm-loader.js (run with node)
/// 3. legacy nodejs/node_modules/@github/copilot-*/index.js, then
///    nodejs/node_modules/@github/copilot/index.js
/// Returns null when nothing is found. The SDK runs `.js` paths with node and
/// executes any other path (for example the `.bin/copilot` shim) directly.
string findCopilotCli()
{
    import std.file : exists, isFile, dirEntries, SpanMode;
    import std.path : buildPath, buildNormalizedPath, baseName, dirName;
    import std.process : environment;
    import std.algorithm.searching : startsWith;

    auto envPath = environment.get("COPILOT_CLI_PATH", "");
    if (envPath.length > 0 && exists(envPath) && isFile(envPath))
        return envPath;

    // This file lives in dlang/e2e; the repository root is two levels up.
    auto repoRoot = buildNormalizedPath(dirName(__FILE_FULL_PATH__), "..", "..");
    auto githubDir = buildPath(repoRoot, "nodejs", "node_modules", "@github");
    auto loader = buildPath(githubDir, "copilot", "npm-loader.js");
    if (exists(loader))
        return loader;
    if (exists(githubDir))
    {
        foreach (entry; dirEntries(githubDir, SpanMode.shallow))
        {
            if (!entry.isDir || !baseName(entry.name).startsWith("copilot-"))
                continue;
            auto candidate = buildPath(entry.name, "index.js");
            if (exists(candidate))
                return candidate;
        }
    }
    auto legacy = buildPath(githubDir, "copilot", "index.js");
    if (exists(legacy))
        return legacy;
    return null;
}

/// Start the replay proxy and return a handle.
ProxyHandle startProxy()
{
    import std.path : buildPath, buildNormalizedPath, dirName;
    import std.process : environment;
    import std.stdio : stdin;

    // Let the SDK find the CLI the same way the other SDK harnesses do.
    auto cliPath = findCopilotCli();
    if (cliPath !is null)
        environment["COPILOT_CLI_PATH"] = cliPath;

    // This file lives in dlang/e2e; the replay proxy is repo-root/test/harness/server.ts.
    auto cwdPath = buildNormalizedPath(dirName(__FILE_FULL_PATH__), "..", "..", "test", "harness");
    auto serverPath = buildPath(cwdPath, "server.ts");

    auto pipes = pipe();
    auto pid = spawnProcess(
        ["npx", "tsx", serverPath],
        stdin, pipes.writeEnd, stderr,
        null, /* env inherit */
        Config.none,
        cwdPath
    );

    // Read the first line
    auto lineStr = strip(pipes.readEnd.readln());

    // Parse "Listening: http://..."
    auto re = regex(`Listening: (http://[^\s]+)`);
    auto m = matchFirst(lineStr, re);
    enforce(!m.empty, "Could not parse proxy URL from: " ~ lineStr);

    auto proxyUrl = m[1];
    writefln("[dlang-e2e] Proxy listening at %s", proxyUrl);

    return ProxyHandle(proxyUrl, 0);
}

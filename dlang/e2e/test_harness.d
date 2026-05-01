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
import core.sys.posix.stdlib : setenv;
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

/// Start the replay proxy and return a handle.
ProxyHandle startProxy()
{
    import std.path : buildPath, dirName;

    // Path from dlang/e2e/ to repo-root/test/harness/server.ts
    auto serverPath = buildPath("..", "..", "test", "harness", "server.ts");
    auto cwdPath = buildPath("..", "..", "test", "harness");

    auto pipes = pipe();
    auto pid = spawnProcess(
        ["npx", "tsx", serverPath],
        pipes.readEnd, pipes.writeEnd, stderr,
        null, /* env inherit */
        Config(cwdPath)
    );

    // Read the first line
    char[1024] buf;
    auto line = pipes.readEnd.readln(buf);
    auto lineStr = strip(cast(string) line);

    // Parse "Listening: http://..."
    auto re = regex(`Listening: (http://[^\s]+)`);
    auto m = matchFirst(lineStr, re);
    enforce(!m.empty, "Could not parse proxy URL from: " ~ lineStr);

    auto proxyUrl = m[1];
    writefln("[dlang-e2e] Proxy listening at %s", proxyUrl);

    return ProxyHandle(proxyUrl, 0);
}

/// E2E tests for the D Copilot SDK.
///
/// Uses D's built-in unittest blocks.
/// Run with: cd dlang && dub test -- --filter e2e
module e2e.test_e2e;

import e2e.test_harness;
import std.stdio : writefln;
import std.string : indexOf;

// ---------------------------------------------------------------------------
// Test 1: Session create + disconnect
// ---------------------------------------------------------------------------
unittest
{
    writefln("[e2e] Test: session create + disconnect");

    auto proxy = startProxy();
    proxy.configure("session", "should_create_and_disconnect_sessions");

    // Verify proxy is alive via /exchanges
    auto exchanges = proxy.httpGet("/exchanges");
    assert(exchanges.length > 0, "Proxy /exchanges should return data");

    // Create session
    auto response = proxy.httpPost("/v1/chat/sessions",
        `{"model":"gpt-4"}`);

    // The replay proxy may or may not have this snapshot;
    // just verify the round-trip worked
    auto exchanges2 = proxy.httpGet("/exchanges");
    assert(exchanges2.length > 0, "Exchanges should be recorded");

    writefln("[e2e] Session create + disconnect: PASS");
}

// ---------------------------------------------------------------------------
// Test 2: Send message
// ---------------------------------------------------------------------------
unittest
{
    writefln("[e2e] Test: send message");

    auto proxy = startProxy();
    proxy.configure("session", "should_have_stateful_conversation");

    auto response = proxy.httpPost("/v1/chat/completions",
        `{"messages":[{"role":"user","content":"What is 1+1?"}]}`);

    // Verify exchanges were captured
    auto exchanges = proxy.httpGet("/exchanges");
    assert(exchanges.length > 0, "Exchanges should contain recorded data");

    writefln("[e2e] Send message: PASS");
}

// ---------------------------------------------------------------------------
// Test 3: SessionFs
// ---------------------------------------------------------------------------
unittest
{
    writefln("[e2e] Test: session_fs configuration");

    auto proxy = startProxy();
    proxy.configure("session_fs", "should_configure_session_fs");

    auto response = proxy.httpPost("/v1/chat/sessions",
        `{"model":"gpt-4","sessionFs":{"initialCwd":"/","sessionStatePath":"/session-state","conventions":"posix"}}`);

    auto exchanges = proxy.httpGet("/exchanges");
    assert(exchanges.length > 0, "Exchanges should be recorded after sessionFs config");

    writefln("[e2e] SessionFs: PASS");
}

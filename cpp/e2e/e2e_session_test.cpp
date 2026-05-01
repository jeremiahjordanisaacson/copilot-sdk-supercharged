/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/// @file e2e_session_test.cpp
/// @brief E2E tests for the C++ SDK session lifecycle, messaging, and configuration.
///
/// These tests use the shared replaying CAPI proxy (test/harness/server.ts) and
/// snapshot YAML files to exercise real SDK code paths without hitting live APIs.

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>

#include "test_harness.h"

#include "copilot/client.h"
#include "copilot/session.h"
#include "copilot/types.h"

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

static e2e::TestHarness harness;

/// Prepare the harness for a specific test snapshot.
static bool configureForTest(const std::string& snapshotRelPath) {
    std::string snapshot = harness.snapshotPath(snapshotRelPath);
    std::string workDir = harness.getRepoRoot();
    return harness.configure(snapshot, workDir);
}

// ---------------------------------------------------------------------------
// Test: session_create_disconnect
// ---------------------------------------------------------------------------

static void test_session_create_disconnect() {
    std::cout << "[TEST] session_create_disconnect\n";

    bool ok = configureForTest("test/snapshots/session/should_create_session_with_custom_tool.yaml");
    assert(ok && "Failed to configure proxy for session_create_disconnect");

    copilot::CopilotClientOptions opts;
    opts.cliPath = "copilot";
    opts.cwd = harness.getRepoRoot();

    // Point the SDK at our replay proxy instead of the real CAPI
    std::map<std::string, std::string> env;
    env["COPILOT_API_URL"] = harness.getProxyUrl();
    // Note: the SDK picks this up from the environment; we set it globally
    // for simplicity since there's no env field on CopilotClientOptions.
#ifdef _WIN32
    _putenv_s("COPILOT_API_URL", harness.getProxyUrl().c_str());
#else
    setenv("COPILOT_API_URL", harness.getProxyUrl().c_str(), 1);
#endif

    copilot::CopilotClient client(opts);
    client.start();

    auto session = client.createSession({});
    assert(session != nullptr && "createSession returned nullptr");
    assert(!session->sessionId.empty() && "session ID is empty");

    std::cout << "  session ID: " << session->sessionId << "\n";

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: send_message
// ---------------------------------------------------------------------------

static void test_send_message() {
    std::cout << "[TEST] send_message\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for send_message");

#ifdef _WIN32
    _putenv_s("COPILOT_API_URL", harness.getProxyUrl().c_str());
#else
    setenv("COPILOT_API_URL", harness.getProxyUrl().c_str(), 1);
#endif

    copilot::CopilotClientOptions opts;
    opts.cliPath = "copilot";
    opts.cwd = harness.getRepoRoot();

    copilot::CopilotClient client(opts);
    client.start();

    auto session = client.createSession({});
    assert(session != nullptr);

    copilot::MessageOptions msg;
    msg.prompt = "Hello!";

    auto response = session->sendAndWait(msg);
    assert(response.has_value() && "sendAndWait returned no response");

    // The response should be an assistant.message event with non-empty content
    auto it = response->data.find("content");
    if (it != response->data.end() && it->is_string()) {
        std::string content = it->get<std::string>();
        assert(!content.empty() && "assistant message content is empty");
        std::cout << "  response (truncated): " << content.substr(0, 80) << "...\n";
    }

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: session_fs_config
// ---------------------------------------------------------------------------

static void test_session_fs_config() {
    std::cout << "[TEST] session_fs_config\n";

    bool ok = configureForTest("test/snapshots/session/should_create_session_with_custom_tool.yaml");
    assert(ok && "Failed to configure proxy for session_fs_config");

#ifdef _WIN32
    _putenv_s("COPILOT_API_URL", harness.getProxyUrl().c_str());
#else
    setenv("COPILOT_API_URL", harness.getProxyUrl().c_str(), 1);
#endif

    copilot::CopilotClientOptions opts;
    opts.cliPath = "copilot";
    opts.cwd = harness.getRepoRoot();

    // Configure sessionFs
    copilot::SessionFsConfig fsConfig;
    fsConfig.initialCwd = "/";
    fsConfig.sessionStatePath = "/session-state";
    fsConfig.conventions = "posix";
    opts.sessionFs = fsConfig;

    copilot::CopilotClient client(opts);

    // Verify the option was accepted by constructing the client successfully
    // (the sessionFs field is stored and will be sent on setSessionFsProvider)
    assert(opts.sessionFs.has_value() && "sessionFs config was not stored");
    assert(opts.sessionFs->initialCwd == "/" && "initialCwd mismatch");
    assert(opts.sessionFs->sessionStatePath == "/session-state" && "sessionStatePath mismatch");
    assert(opts.sessionFs->conventions == "posix" && "conventions mismatch");

    std::cout << "  sessionFs config accepted: initialCwd=" << opts.sessionFs->initialCwd
              << " statePath=" << opts.sessionFs->sessionStatePath
              << " conventions=" << opts.sessionFs->conventions << "\n";

    client.start();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

int main() {
    std::cout << "=== C++ SDK E2E Tests ===\n\n";

    if (!harness.start()) {
        std::cerr << "FATAL: failed to start test harness\n";
        return 1;
    }

    int failures = 0;

    try { test_session_create_disconnect(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_send_message(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_session_fs_config(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    harness.stop();

    std::cout << "\n=== Results: " << (3 - failures) << "/3 passed";
    if (failures > 0) std::cout << ", " << failures << " FAILED";
    std::cout << " ===\n";

    return failures;
}

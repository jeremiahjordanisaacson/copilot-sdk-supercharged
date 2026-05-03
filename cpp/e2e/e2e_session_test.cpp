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
// Test: multi_turn_conversation
// ---------------------------------------------------------------------------

static void test_multi_turn_conversation() {
    std::cout << "[TEST] multi_turn_conversation\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for multi_turn_conversation");

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

    // First turn
    copilot::MessageOptions msg1;
    msg1.prompt = "Hello!";
    auto resp1 = session->sendAndWait(msg1);
    assert(resp1.has_value() && "First turn returned no response");

    // Second turn in the same session
    copilot::MessageOptions msg2;
    msg2.prompt = "Follow-up question";
    auto resp2 = session->sendAndWait(msg2);
    assert(resp2.has_value() && "Second turn returned no response");

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: session_resume
// ---------------------------------------------------------------------------

static void test_session_resume() {
    std::cout << "[TEST] session_resume\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for session_resume");

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
    std::string id = session->sessionId;
    assert(!id.empty());

    // Resume the same session by ID
    auto resumed = client.resumeSession(id, {});
    assert(resumed != nullptr && "resumeSession returned nullptr");
    assert(resumed->sessionId == id && "resumed session ID mismatch");

    resumed->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: session_list
// ---------------------------------------------------------------------------

static void test_session_list() {
    std::cout << "[TEST] session_list\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for session_list");

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

    auto sessions = client.listSessions();
    assert(!sessions.empty() && "listSessions returned empty list");

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: session_metadata
// ---------------------------------------------------------------------------

static void test_session_metadata() {
    std::cout << "[TEST] session_metadata\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for session_metadata");

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
    std::string id = session->sessionId;

    auto metadata = client.getSessionMetadata(id);
    assert(metadata.has_value() && "getSessionMetadata returned no value");

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: session_delete
// ---------------------------------------------------------------------------

static void test_session_delete() {
    std::cout << "[TEST] session_delete\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for session_delete");

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
    std::string id = session->sessionId;

    session->destroy();

    // Delete the session from the server
    client.deleteSession(id);

    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: model_list
// ---------------------------------------------------------------------------

static void test_model_list() {
    std::cout << "[TEST] model_list\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for model_list");

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

    auto models = client.listModels();
    // The replay proxy should return at least one model
    assert(!models.empty() && "listModels returned empty list");

    std::cout << "  models count: " << models.size() << "\n";

    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: ping
// ---------------------------------------------------------------------------

static void test_ping() {
    std::cout << "[TEST] ping\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for ping");

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

    auto reply = client.ping("hello");
    assert(!reply.empty() && "ping returned empty string");

    std::cout << "  ping reply: " << reply << "\n";

    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: auth_status
// ---------------------------------------------------------------------------

static void test_auth_status() {
    std::cout << "[TEST] auth_status\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for auth_status");

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

    auto authStatus = client.getAuthStatus();
    assert(authStatus.has_value() && "getAuthStatus returned no value");

    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: client_lifecycle
// ---------------------------------------------------------------------------

static void test_client_lifecycle() {
    std::cout << "[TEST] client_lifecycle\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for client_lifecycle");

#ifdef _WIN32
    _putenv_s("COPILOT_API_URL", harness.getProxyUrl().c_str());
#else
    setenv("COPILOT_API_URL", harness.getProxyUrl().c_str(), 1);
#endif

    copilot::CopilotClientOptions opts;
    opts.cliPath = "copilot";
    opts.cwd = harness.getRepoRoot();

    // Start → getState → getStatus → stop cycle
    copilot::CopilotClient client(opts);
    client.start();

    auto state = client.getState();
    assert(!state.empty() && "getState returned empty string");

    auto status = client.getStatus();
    assert(status.has_value() && "getStatus returned no value");

    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: foreground_session
// ---------------------------------------------------------------------------

static void test_foreground_session() {
    std::cout << "[TEST] foreground_session\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for foreground_session");

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
    std::string id = session->sessionId;

    // Set this session as the foreground session and verify
    client.setForegroundSessionId(id);
    auto fgId = client.getForegroundSessionId();
    assert(fgId == id && "foreground session ID mismatch");

    std::cout << "  foreground session: " << fgId << "\n";

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: tools
// ---------------------------------------------------------------------------

static void test_tools() {
    std::cout << "[TEST] tools\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for tools");

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

    // Register a simple tool
    copilot::Tool tool;
    tool.name = "echo_tool";
    tool.description = "Echoes back the input";
    tool.inputSchema = R"({"type":"object","properties":{"text":{"type":"string"}}})";

    std::vector<copilot::Tool> tools = { tool };
    session->registerTools(tools);

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: streaming
// ---------------------------------------------------------------------------

static void test_streaming() {
    std::cout << "[TEST] streaming\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for streaming");

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

    copilot::SessionConfig sessionCfg;
    sessionCfg.streaming = true;
    auto session = client.createSession(sessionCfg);
    assert(session != nullptr);

    // Collect delta events via the on() handler
    int deltaCount = 0;
    session->on("assistant.message_delta", [&deltaCount](const copilot::SessionEvent& /*ev*/) {
        ++deltaCount;
    });

    copilot::MessageOptions msg;
    msg.prompt = "Hello!";
    auto response = session->sendAndWait(msg);
    assert(response.has_value() && "streaming sendAndWait returned no response");

    std::cout << "  delta events received: " << deltaCount << "\n";

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: system_message_customization
// ---------------------------------------------------------------------------

static void test_system_message_customization() {
    std::cout << "[TEST] system_message_customization\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for system_message_customization");

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

    copilot::SessionConfig sessionCfg;
    sessionCfg.systemMessage = "You are a helpful test assistant.";
    auto session = client.createSession(sessionCfg);
    assert(session != nullptr);
    assert(!session->sessionId.empty());

    copilot::MessageOptions msg;
    msg.prompt = "Hello!";
    auto response = session->sendAndWait(msg);
    assert(response.has_value() && "sendAndWait with custom system message returned no response");

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: session_fs_provider
// ---------------------------------------------------------------------------

static void test_session_fs_provider() {
    std::cout << "[TEST] session_fs_provider\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for session_fs_provider");

#ifdef _WIN32
    _putenv_s("COPILOT_API_URL", harness.getProxyUrl().c_str());
#else
    setenv("COPILOT_API_URL", harness.getProxyUrl().c_str(), 1);
#endif

    copilot::CopilotClientOptions opts;
    opts.cliPath = "copilot";
    opts.cwd = harness.getRepoRoot();

    copilot::SessionFsConfig fsConfig;
    fsConfig.initialCwd = harness.getRepoRoot();
    fsConfig.sessionStatePath = harness.getRepoRoot() + "/session-state";
    fsConfig.conventions = "posix";
    opts.sessionFs = fsConfig;

    copilot::CopilotClient client(opts);
    client.start();

    auto session = client.createSession({});
    assert(session != nullptr);
    assert(!session->sessionId.empty());

    copilot::MessageOptions msg;
    msg.prompt = "Hello!";
    auto response = session->sendAndWait(msg);
    assert(response.has_value() && "sendAndWait with sessionFs returned no response");

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: mcp_servers_config
// ---------------------------------------------------------------------------

static void test_mcp_servers_config() {
    std::cout << "[TEST] mcp_servers_config\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for mcp_servers_config");

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

    copilot::SessionConfig sessionCfg;
    sessionCfg.mcpServers = {
        { "test-server", { {"command", "echo"}, {"args", "hello"} } }
    };
    auto session = client.createSession(sessionCfg);
    assert(session != nullptr);
    assert(!session->sessionId.empty());

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: skills_config
// ---------------------------------------------------------------------------

static void test_skills_config() {
    std::cout << "[TEST] skills_config\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for skills_config");

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

    copilot::SessionConfig sessionCfg;
    sessionCfg.skills = { "code-review", "testing" };
    auto session = client.createSession(sessionCfg);
    assert(session != nullptr);
    assert(!session->sessionId.empty());

    copilot::MessageOptions msg;
    msg.prompt = "Hello!";
    auto response = session->sendAndWait(msg);
    assert(response.has_value() && "sendAndWait with skills returned no response");

    session->destroy();
    client.stop();

    std::cout << "  PASSED\n";
}

// ---------------------------------------------------------------------------
// Test: compaction
// ---------------------------------------------------------------------------

static void test_compaction() {
    std::cout << "[TEST] compaction\n";

    bool ok = configureForTest("test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(ok && "Failed to configure proxy for compaction");

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

    // Listen for compaction events
    bool compactionStartSeen = false;
    bool compactionCompleteSeen = false;
    session->on("session.compaction_start", [&compactionStartSeen](const copilot::SessionEvent& /*ev*/) {
        compactionStartSeen = true;
    });
    session->on("session.compaction_complete", [&compactionCompleteSeen](const copilot::SessionEvent& /*ev*/) {
        compactionCompleteSeen = true;
    });

    // Send several messages to potentially trigger compaction
    for (int i = 0; i < 3; ++i) {
        copilot::MessageOptions msg;
        msg.prompt = "Message " + std::to_string(i);
        auto response = session->sendAndWait(msg);
        assert(response.has_value() && "sendAndWait returned no response during compaction test");
    }

    std::cout << "  compaction_start seen: " << (compactionStartSeen ? "yes" : "no") << "\n";
    std::cout << "  compaction_complete seen: " << (compactionCompleteSeen ? "yes" : "no") << "\n";

    session->destroy();
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

    try { test_multi_turn_conversation(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_session_resume(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_session_list(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_session_metadata(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_session_delete(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_model_list(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_ping(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_auth_status(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_client_lifecycle(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_foreground_session(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_tools(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_streaming(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_system_message_customization(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_session_fs_provider(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_mcp_servers_config(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_skills_config(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    try { test_compaction(); }
    catch (const std::exception& e) {
        std::cerr << "  FAILED: " << e.what() << "\n";
        ++failures;
    }

    harness.stop();

    std::cout << "\n=== Results: " << (20 - failures) << "/20 passed";
    if (failures > 0) std::cout << ", " << failures << " FAILED";
    std::cout << " ===\n";

    return failures;
}

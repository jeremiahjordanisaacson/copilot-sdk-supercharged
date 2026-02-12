/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <vector>

#include "copilot/json_rpc_client.h"
#include "copilot/session.h"
#include "copilot/types.h"

namespace copilot {

/// Main client for interacting with the Copilot CLI.
///
/// The CopilotClient manages the connection to the Copilot CLI server and provides
/// methods to create and manage conversation sessions. It can either spawn a CLI
/// server process or connect to an existing server.
///
/// Example:
/// @code
///   copilot::CopilotClient client;
///   client.start();
///
///   auto session = client.createSession({});
///   auto response = session->sendAndWait({"Hello, world!"});
///   if (response) {
///       std::cout << response->data["content"].get<std::string>() << std::endl;
///   }
///
///   session->destroy();
///   client.stop();
/// @endcode
class CopilotClient {
public:
    /// Creates a new CopilotClient with the given options.
    explicit CopilotClient(const CopilotClientOptions& options = {});

    ~CopilotClient();

    // Non-copyable
    CopilotClient(const CopilotClient&) = delete;
    CopilotClient& operator=(const CopilotClient&) = delete;

    /// Starts the CLI server and establishes a connection.
    /// Called automatically on first use if autoStart is true.
    void start();

    /// Stops the CLI server and closes all active sessions.
    /// Returns a list of errors encountered during cleanup (empty = success).
    std::vector<std::string> stop();

    /// Forcefully stops the CLI server without graceful cleanup.
    void forceStop();

    /// Creates a new conversation session.
    std::shared_ptr<CopilotSession> createSession(const SessionConfig& config = {});

    /// Resumes an existing session by ID.
    std::shared_ptr<CopilotSession> resumeSession(const std::string& sessionId,
                                                    const ResumeSessionConfig& config = {});

    /// Gets the current connection state.
    ConnectionState getState() const;

    /// Sends a ping to verify connectivity.
    PingResponse ping(const std::string& message = "");

    /// Get CLI status including version and protocol information.
    GetStatusResponse getStatus();

    /// Get current authentication status.
    GetAuthStatusResponse getAuthStatus();

    /// List available models with their metadata (cached after first call).
    std::vector<ModelInfo> listModels();

    /// Gets the ID of the most recently updated session.
    std::optional<std::string> getLastSessionId();

    /// Deletes a session permanently.
    void deleteSession(const std::string& sessionId);

    /// Lists all available sessions.
    std::vector<SessionMetadata> listSessions();

    /// Gets the foreground session ID (TUI+server mode only).
    std::optional<std::string> getForegroundSessionId();

    /// Sets the foreground session (TUI+server mode only).
    void setForegroundSessionId(const std::string& sessionId);

    /// Subscribe to all session lifecycle events. Returns unsubscribe function.
    std::function<void()> onLifecycle(SessionLifecycleHandler handler);

    /// Subscribe to a specific lifecycle event type. Returns unsubscribe function.
    std::function<void()> onLifecycle(const std::string& eventType, SessionLifecycleHandler handler);

private:
    void ensureConnected();
    void startCLIServer();
    void connectToServer();
    void verifyProtocolVersion();
    void setupHandlers();

    // Server request handlers
    void handleSessionEvent(const nlohmann::json& params);
    void handleSessionLifecycle(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleToolCall(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handlePermissionRequest(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleUserInputRequest(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleHooksInvoke(const nlohmann::json& params);

    ToolResultObject executeToolCall(ToolHandler handler, const ToolInvocation& invocation);
    ToolResultObject normalizeToolResult(const nlohmann::json& result);
    static ToolResultObject buildFailedToolResult(const std::string& error);
    static ToolResultObject buildUnsupportedToolResult(const std::string& toolName);

    nlohmann::json buildCreateSessionParams(const SessionConfig& config);
    nlohmann::json buildResumeSessionParams(const std::string& sessionId, const ResumeSessionConfig& config);
    nlohmann::json buildToolsJson(const std::vector<Tool>& tools);

    CopilotClientOptions options_;
    ConnectionState state_ = ConnectionState::Disconnected;
    bool isExternalServer_ = false;

    // Process management
#ifdef _WIN32
    void* processHandle_ = nullptr;   // HANDLE for Win32
    void* stdinWrite_ = nullptr;       // HANDLE
    void* stdoutRead_ = nullptr;       // HANDLE
#else
    pid_t processPid_ = -1;
    int stdinWriteFd_ = -1;
    int stdoutReadFd_ = -1;
#endif

    // JSON-RPC client
    std::unique_ptr<JsonRpcClient> rpcClient_;

    // Sessions
    mutable std::mutex sessionsMutex_;
    std::map<std::string, std::shared_ptr<CopilotSession>> sessions_;

    // Models cache
    mutable std::mutex modelsCacheMutex_;
    std::optional<std::vector<ModelInfo>> modelsCache_;

    // Lifecycle handlers
    struct LifecycleEntry {
        uint64_t id;
        std::string eventType; // empty = wildcard
        SessionLifecycleHandler fn;
    };
    mutable std::mutex lifecycleMutex_;
    std::vector<LifecycleEntry> lifecycleHandlers_;
    uint64_t nextLifecycleId_ = 0;
};

} // namespace copilot

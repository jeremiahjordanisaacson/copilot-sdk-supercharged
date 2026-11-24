/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

#include <atomic>
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
    /// Throws JsonRpcException when the server rejects the request; use
    /// JsonRpcException::policyHelperFailure() for a structured Policy Helper failure.
    /// In ClientMode::Empty, throws std::invalid_argument unless availableTools is set.
    std::shared_ptr<CopilotSession> createSession(const SessionConfig& config = {});

    /// Resumes an existing session by ID (same error behavior as createSession()).
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

    /// Gets metadata for a specific session.
    nlohmann::json getSessionMetadata(const std::string& sessionId);

    /// Deletes a session permanently.
    void deleteSession(const std::string& sessionId);

    /// Lists all available sessions.
    std::vector<SessionMetadata> listSessions();

    /// Gets the foreground session ID (TUI+server mode only).
    std::optional<std::string> getForegroundSessionId();

    /// Sets the foreground session (TUI+server mode only).
    void setForegroundSessionId(const std::string& sessionId);

    /// Sets the session filesystem provider configuration.
    void setSessionFsProvider(const SessionFsConfig& config = {});

    /// Clears the runtime's managed-settings cache (`managedSettings.clearCache`).
    void clearManagedSettingsCache();

    /// Saves a legacy MCP configuration credential in the runtime-owned secret store
    /// (`mcp.secrets.save`). `value` is a credential and is never logged. `configDir`
    /// selects the configuration directory (the runtime's default when std::nullopt).
    /// McpSecretsSaveResult::usedKeychain is true when the OS keychain stored the
    /// credential and false for the file backend. Requires Copilot CLI 1.0.116 or newer.
    McpSecretsSaveResult mcpSecretsSave(const std::string& secretId, const std::string& value,
                                        const std::optional<std::string>& configDir = std::nullopt);

    /// Deletes a legacy MCP configuration credential from the runtime-owned secret store
    /// (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
    void mcpSecretsDelete(const std::string& secretId,
                          const std::optional<std::string>& configDir = std::nullopt);

    /// Deletes the legacy credentials indexed for an MCP server (`mcp.secrets.deleteServer`).
    /// Requires Copilot CLI 1.0.116 or newer.
    void mcpSecretsDeleteServer(const std::string& serverName,
                                const std::optional<std::string>& configDir = std::nullopt);

    /// Persists a pending plugin extension permission request as a user-scoped approval,
    /// before the host answers that request normally (`permissions.extensionApprovals.grant`).
    /// Returns whether the request was eligible and persisted, and the stored approval.
    /// Requires Copilot CLI 1.0.116 or newer.
    PermissionsExtensionApprovalsGrantResult extensionApprovalsGrant(const std::string& sessionId,
                                                                     const std::string& requestId);

    /// Lists the user-scoped plugin extension approvals (`permissions.extensionApprovals.list`).
    /// `sessionId` selects an attached session whose configuration profile to list.
    /// Requires Copilot CLI 1.0.116 or newer.
    PermissionsExtensionApprovalsListResult extensionApprovalsList(
        const std::optional<std::string>& sessionId = std::nullopt);

    /// Revokes one user-scoped plugin extension approval (`permissions.extensionApprovals.revoke`).
    /// Pass the approval's extensionName and installationKey unchanged; `sessionId` selects an
    /// attached session whose configuration profile to use. Requires Copilot CLI 1.0.116 or newer.
    PermissionsExtensionApprovalsRevokeResult extensionApprovalsRevoke(
        const std::string& extensionName, const std::string& installationKey,
        const std::optional<std::string>& sessionId = std::nullopt);

    /// Subscribe to all session lifecycle events. Returns unsubscribe function.
    std::function<void()> onLifecycle(SessionLifecycleHandler handler);

    /// Subscribe to a specific lifecycle event type. Returns unsubscribe function.
    std::function<void()> onLifecycle(const std::string& eventType, SessionLifecycleHandler handler);

private:
    void ensureConnected();
    void startCLIServer();
    void connectToServer();
    void sendConnectHandshake();
    void verifyProtocolVersion();
    void setupHandlers();

    // Server request handlers
    void handleSessionEvent(const nlohmann::json& params);
    void handleSessionLifecycle(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleToolCall(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handlePermissionRequest(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleUserInputRequest(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleHooksInvoke(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleExitPlanModeRequest(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleGitHubTokenGetToken(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleShellAuthenticationRequest(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleShellAuthenticationCancel(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleMemoryStore(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleNetworkResolveProxy(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleNetworkAuthenticateNegotiate(const nlohmann::json& params);
    std::pair<nlohmann::json, std::optional<JsonRpcError>> handleNetworkAuthenticateBasic(const nlohmann::json& params);

    // Registers network callbacks with `network.setProvider` (no-op without callbacks).
    void registerNetworkProvider();
    std::shared_ptr<CopilotSession> findSession(const std::string& sessionId);

    // GitHub token provider registry (gitHubToken.getToken)
    std::string registerGitHubTokenProvider(const GitHubTokenProvider& provider, const std::string& sessionId);
    void assignGitHubTokenProviderSession(const std::string& registrationId, const std::string& sessionId);
    void unregisterGitHubTokenProvider(const std::string& registrationId);
    void clearGitHubTokenProviders();

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

    // GitHub token providers keyed by registration ID
    struct GitHubTokenRegistration {
        GitHubTokenProvider provider;
        std::string sessionId; // empty until the session ID is known
    };
    mutable std::mutex gitHubTokenProvidersMutex_;
    std::map<std::string, GitHubTokenRegistration> gitHubTokenProviders_;

    // Cancellation flags of in-flight shell authentication requests, keyed by
    // "<sessionId>\n<requestId>" (set by shellAuthentication.cancel).
    std::mutex shellAuthMutex_;
    std::map<std::string, std::shared_ptr<std::atomic<bool>>> pendingShellAuth_;

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

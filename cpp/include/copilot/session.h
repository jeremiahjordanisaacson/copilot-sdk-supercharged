/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

#include <condition_variable>
#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <vector>

#include "copilot/json_rpc_client.h"
#include "copilot/types.h"

namespace copilot {

/// Represents a single conversation session with the Copilot CLI.
///
/// A session maintains conversation state, handles events, and manages tool execution.
/// Sessions are created via CopilotClient::createSession() or resumed via
/// CopilotClient::resumeSession().
///
/// Thread-safe: all public methods can be called from any thread.
class CopilotSession {
public:
    /// The unique identifier for this session.
    const std::string sessionId;

    /// Path to the session workspace directory when infinite sessions are enabled.
    /// Empty if infinite sessions are disabled.
    std::string workspacePath() const;

    /// Sends a message to this session.
    /// @return The message ID of the queued message.
    std::string send(const MessageOptions& options);

    /// Sends a message and waits until the session becomes idle.
    /// Returns the last assistant.message event received, or std::nullopt if none.
    /// @param options  The message to send.
    /// @param timeoutMs  Timeout in milliseconds (default: 60000).
    /// @throws std::runtime_error on timeout or session error.
    std::optional<SessionEvent> sendAndWait(const MessageOptions& options, int timeoutMs = 60000);

    /// Subscribe to all events from this session.
    /// @return An ID that can be passed to off() to unsubscribe.
    uint64_t on(SessionEventHandler handler);

    /// Subscribe to events of a specific type from this session.
    /// @return An ID that can be passed to off() to unsubscribe.
    uint64_t on(const std::string& eventType, SessionEventHandler handler);

    /// Unsubscribe a previously registered event handler.
    void off(uint64_t handlerId);

    /// Get all events/messages from this session's history.
    std::vector<SessionEvent> getMessages();

    /// Destroy this session and release resources.
    void destroy();

    /// Abort the currently processing message.
    void abort();

    // -- Internal methods called by CopilotClient --

    /// @internal Dispatch an event to all registered handlers.
    void dispatchEvent(const SessionEvent& event);

    /// @internal Register tools for this session.
    void registerTools(const std::vector<Tool>& tools);

    /// @internal Get a tool handler by name.
    ToolHandler getToolHandler(const std::string& name) const;

    /// @internal Register a permission handler.
    void registerPermissionHandler(PermissionHandler handler);

    /// @internal Handle a permission request from the CLI.
    PermissionRequestResult handlePermissionRequest(const PermissionRequest& request);

    /// @internal Register a user input handler.
    void registerUserInputHandler(UserInputHandler handler);

    /// @internal Handle a user input request from the CLI.
    UserInputResponse handleUserInputRequest(const UserInputRequest& request);

    /// @internal Register hook handlers.
    void registerHooks(const SessionHooks& hooks);

    /// @internal Handle a hook invocation from the CLI.
    nlohmann::json handleHooksInvoke(const std::string& hookType, const nlohmann::json& input);

private:
    friend class CopilotClient;

    CopilotSession(const std::string& sessionId, JsonRpcClient* client, const std::string& workspacePath);

    JsonRpcClient* client_;
    std::string workspacePath_;

    // Event handlers
    struct HandlerEntry {
        uint64_t id;
        std::string eventType; // empty = wildcard
        SessionEventHandler fn;
    };
    mutable std::mutex handlerMutex_;
    std::vector<HandlerEntry> handlers_;
    uint64_t nextHandlerId_ = 0;

    // Tool handlers
    mutable std::mutex toolMutex_;
    std::map<std::string, ToolHandler> toolHandlers_;

    // Permission handler
    mutable std::mutex permissionMutex_;
    PermissionHandler permissionHandler_;

    // User input handler
    mutable std::mutex userInputMutex_;
    UserInputHandler userInputHandler_;

    // Hooks
    mutable std::mutex hooksMutex_;
    std::optional<SessionHooks> hooks_;
};

} // namespace copilot

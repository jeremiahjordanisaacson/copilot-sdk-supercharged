/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

#include <atomic>
#include <condition_variable>
#include <functional>
#include <future>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <thread>

#include <nlohmann/json.hpp>

namespace copilot {

/// JSON-RPC 2.0 error
struct JsonRpcError {
    int code = 0;
    std::string message;
    nlohmann::json data;
};

/// Handler for incoming server requests.
/// Returns (result_json, error_or_null). If error is non-null, it is sent as an error response.
using RequestHandler = std::function<std::pair<nlohmann::json, std::optional<JsonRpcError>>(
    const nlohmann::json& params)>;

/// Minimal JSON-RPC 2.0 client for Content-Length framed stdio/pipe transport.
///
/// The client reads messages from an input stream (stdout of the subprocess) and
/// writes messages to an output stream (stdin of the subprocess). Messages are
/// framed with Content-Length headers per the LSP base protocol.
///
/// Incoming messages are dispatched:
/// - Responses (messages with "id" and "result"/"error") are matched to pending requests.
/// - Requests/Notifications (messages with "method") are dispatched to registered handlers.
///   Notifications (no "id") run synchronously on the reader thread.
///   Requests (with "id") run in a detached thread and responses are sent back.
class JsonRpcClient {
public:
    /// Construct a client operating on the given file descriptors.
    /// @param readFd  File descriptor to read from (server stdout).
    /// @param writeFd File descriptor to write to (server stdin).
    JsonRpcClient(int readFd, int writeFd);

    ~JsonRpcClient();

    /// Start the background reader thread.
    void start();

    /// Stop the client and join the reader thread.
    void stop();

    /// Register a handler for incoming requests/notifications with the given method name.
    void setRequestHandler(const std::string& method, RequestHandler handler);

    /// Send a JSON-RPC request and wait for the response.
    /// @return The result field of the response, or throws std::runtime_error on error.
    nlohmann::json request(const std::string& method, const nlohmann::json& params);

    /// Send a JSON-RPC notification (no response expected).
    void notify(const std::string& method, const nlohmann::json& params);

private:
    struct PendingRequest {
        std::promise<nlohmann::json> promise;
    };

    void readLoop();
    void handleIncoming(const nlohmann::json& msg);
    void handleResponse(const nlohmann::json& msg);
    void handleRequest(const nlohmann::json& msg);
    void sendMessage(const nlohmann::json& msg);
    void sendResponse(const nlohmann::json& id, const nlohmann::json& result);
    void sendErrorResponse(const nlohmann::json& id, int code, const std::string& message);

    static std::string generateUUID();

    int readFd_;
    int writeFd_;
    std::atomic<bool> running_{false};
    std::thread readerThread_;

    std::mutex writeMutex_;

    std::mutex pendingMutex_;
    std::map<std::string, std::shared_ptr<PendingRequest>> pendingRequests_;

    std::mutex handlerMutex_;
    std::map<std::string, RequestHandler> requestHandlers_;
};

} // namespace copilot

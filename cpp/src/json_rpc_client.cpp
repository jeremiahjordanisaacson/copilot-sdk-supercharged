/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#include "copilot/json_rpc_client.h"

#include <cstdio>
#include <cstring>
#include <random>
#include <sstream>
#include <stdexcept>
#include <vector>

#ifdef _WIN32
#include <io.h>
#define COPILOT_READ(fd, buf, len)  _read(fd, buf, static_cast<unsigned int>(len))
#define COPILOT_WRITE(fd, buf, len) _write(fd, buf, static_cast<unsigned int>(len))
#else
#include <unistd.h>
#define COPILOT_READ(fd, buf, len)  ::read(fd, buf, len)
#define COPILOT_WRITE(fd, buf, len) ::write(fd, buf, len)
#endif

namespace copilot {

// ============================================================================
// Construction / Destruction
// ============================================================================

JsonRpcClient::JsonRpcClient(int readFd, int writeFd)
    : readFd_(readFd), writeFd_(writeFd) {}

JsonRpcClient::~JsonRpcClient() {
    stop();
}

// ============================================================================
// Lifecycle
// ============================================================================

void JsonRpcClient::start() {
    if (running_.load()) return;
    running_.store(true);
    readerThread_ = std::thread(&JsonRpcClient::readLoop, this);
}

void JsonRpcClient::stop() {
    if (!running_.load()) return;
    running_.store(false);

    // Join the reader thread. It will exit because running_ is false
    // and the pipe will be closed by the process teardown.
    if (readerThread_.joinable()) {
        readerThread_.join();
    }

    // Fail any pending requests
    std::lock_guard<std::mutex> lock(pendingMutex_);
    for (auto& [id, pending] : pendingRequests_) {
        try {
            pending->promise.set_exception(
                std::make_exception_ptr(std::runtime_error("client stopped")));
        } catch (...) {
            // Promise may already be satisfied
        }
    }
    pendingRequests_.clear();
}

// ============================================================================
// Handler Registration
// ============================================================================

void JsonRpcClient::setRequestHandler(const std::string& method, RequestHandler handler) {
    std::lock_guard<std::mutex> lock(handlerMutex_);
    if (handler) {
        requestHandlers_[method] = std::move(handler);
    } else {
        requestHandlers_.erase(method);
    }
}

// ============================================================================
// Request / Notify
// ============================================================================

nlohmann::json JsonRpcClient::request(const std::string& method, const nlohmann::json& params) {
    auto requestId = generateUUID();

    auto pending = std::make_shared<PendingRequest>();
    auto future = pending->promise.get_future();

    {
        std::lock_guard<std::mutex> lock(pendingMutex_);
        pendingRequests_[requestId] = pending;
    }

    // Build and send request
    nlohmann::json msg = {
        {"jsonrpc", "2.0"},
        {"id", requestId},
        {"method", method},
        {"params", params}
    };

    try {
        sendMessage(msg);
    } catch (...) {
        std::lock_guard<std::mutex> lock(pendingMutex_);
        pendingRequests_.erase(requestId);
        throw;
    }

    // Wait for response
    nlohmann::json result = future.get();

    // Clean up
    {
        std::lock_guard<std::mutex> lock(pendingMutex_);
        pendingRequests_.erase(requestId);
    }

    return result;
}

void JsonRpcClient::notify(const std::string& method, const nlohmann::json& params) {
    nlohmann::json msg = {
        {"jsonrpc", "2.0"},
        {"method", method},
        {"params", params}
    };
    sendMessage(msg);
}

// ============================================================================
// Reader Loop
// ============================================================================

/// Read exactly `len` bytes from `fd` into `buf`.
static bool readFull(int fd, char* buf, size_t len) {
    size_t totalRead = 0;
    while (totalRead < len) {
        auto n = COPILOT_READ(fd, buf + totalRead, len - totalRead);
        if (n <= 0) return false;
        totalRead += static_cast<size_t>(n);
    }
    return true;
}

/// Read a line (up to \n) from fd. Returns false on EOF/error.
static bool readLine(int fd, std::string& out) {
    out.clear();
    char ch;
    while (true) {
        auto n = COPILOT_READ(fd, &ch, 1);
        if (n <= 0) return false;
        out += ch;
        if (ch == '\n') return true;
    }
}

void JsonRpcClient::readLoop() {
    while (running_.load()) {
        // Read headers until blank line
        int contentLength = 0;
        while (true) {
            std::string line;
            if (!readLine(readFd_, line)) {
                // EOF or error
                return;
            }
            // Blank line = end of headers
            if (line == "\r\n" || line == "\n") {
                break;
            }
            // Parse Content-Length
            int length = 0;
            if (std::sscanf(line.c_str(), "Content-Length: %d", &length) == 1) {
                contentLength = length;
            }
        }

        if (contentLength <= 0) continue;

        // Read the message body
        std::vector<char> body(contentLength);
        if (!readFull(readFd_, body.data(), contentLength)) {
            return; // EOF or error
        }

        // Parse JSON
        try {
            auto msg = nlohmann::json::parse(body.begin(), body.end());
            handleIncoming(msg);
        } catch (const nlohmann::json::exception&) {
            // Malformed JSON, skip
        }
    }
}

// ============================================================================
// Message Dispatch
// ============================================================================

void JsonRpcClient::handleIncoming(const nlohmann::json& msg) {
    // Is it a request/notification from server? (has "method")
    if (msg.contains("method")) {
        handleRequest(msg);
        return;
    }

    // Is it a response to one of our requests? (has "id" and "result" or "error")
    if (msg.contains("id")) {
        handleResponse(msg);
        return;
    }
}

void JsonRpcClient::handleResponse(const nlohmann::json& msg) {
    std::string id;
    if (msg["id"].is_string()) {
        id = msg["id"].get<std::string>();
    } else {
        return; // Non-string IDs not supported
    }

    std::shared_ptr<PendingRequest> pending;
    {
        std::lock_guard<std::mutex> lock(pendingMutex_);
        auto it = pendingRequests_.find(id);
        if (it == pendingRequests_.end()) return;
        pending = it->second;
    }

    if (msg.contains("error") && !msg["error"].is_null()) {
        auto& err = msg["error"];
        std::string errMsg = "JSON-RPC Error";
        if (err.contains("code")) errMsg += " " + std::to_string(err["code"].get<int>());
        if (err.contains("message")) errMsg += ": " + err["message"].get<std::string>();
        try {
            pending->promise.set_exception(
                std::make_exception_ptr(std::runtime_error(errMsg)));
        } catch (...) {}
    } else {
        nlohmann::json result = nullptr;
        if (msg.contains("result")) result = msg["result"];
        try {
            pending->promise.set_value(std::move(result));
        } catch (...) {}
    }
}

void JsonRpcClient::handleRequest(const nlohmann::json& msg) {
    std::string method = msg["method"].get<std::string>();
    nlohmann::json params = msg.contains("params") ? msg["params"] : nlohmann::json::object();
    bool isCall = msg.contains("id") && !msg["id"].is_null();

    RequestHandler handler;
    {
        std::lock_guard<std::mutex> lock(handlerMutex_);
        auto it = requestHandlers_.find(method);
        if (it != requestHandlers_.end()) {
            handler = it->second;
        }
    }

    if (!handler) {
        if (isCall) {
            sendErrorResponse(msg["id"], -32601, "Method not found: " + method);
        }
        return;
    }

    if (!isCall) {
        // Notification: run synchronously on reader thread
        try {
            handler(params);
        } catch (...) {}
        return;
    }

    // Request: run in detached thread to avoid blocking the reader
    nlohmann::json requestId = msg["id"];
    std::thread([this, handler = std::move(handler), params = std::move(params),
                 requestId = std::move(requestId)]() {
        try {
            auto [result, error] = handler(params);
            if (error) {
                sendErrorResponse(requestId, error->code, error->message);
            } else {
                sendResponse(requestId, result);
            }
        } catch (const std::exception& e) {
            sendErrorResponse(requestId, -32603, std::string("Handler error: ") + e.what());
        } catch (...) {
            sendErrorResponse(requestId, -32603, "Unknown handler error");
        }
    }).detach();
}

// ============================================================================
// Message Sending
// ============================================================================

void JsonRpcClient::sendMessage(const nlohmann::json& msg) {
    std::string body = msg.dump();
    std::string header = "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n";

    std::lock_guard<std::mutex> lock(writeMutex_);
    auto headerWritten = COPILOT_WRITE(writeFd_, header.c_str(), header.size());
    if (headerWritten < 0) {
        throw std::runtime_error("Failed to write message header");
    }
    auto bodyWritten = COPILOT_WRITE(writeFd_, body.c_str(), body.size());
    if (bodyWritten < 0) {
        throw std::runtime_error("Failed to write message body");
    }
}

void JsonRpcClient::sendResponse(const nlohmann::json& id, const nlohmann::json& result) {
    nlohmann::json msg = {
        {"jsonrpc", "2.0"},
        {"id", id},
        {"result", result}
    };
    try {
        sendMessage(msg);
    } catch (...) {
        // Best-effort send
    }
}

void JsonRpcClient::sendErrorResponse(const nlohmann::json& id, int code, const std::string& message) {
    nlohmann::json msg = {
        {"jsonrpc", "2.0"},
        {"id", id},
        {"error", {{"code", code}, {"message", message}}}
    };
    try {
        sendMessage(msg);
    } catch (...) {
        // Best-effort send
    }
}

// ============================================================================
// UUID Generation
// ============================================================================

std::string JsonRpcClient::generateUUID() {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<uint32_t> dist(0, 0xFFFFFFFF);

    uint32_t a = dist(gen);
    uint32_t b = dist(gen);
    uint32_t c = dist(gen);
    uint32_t d = dist(gen);

    // Set version 4
    b = (b & 0xFFFF0FFF) | 0x00004000;
    // Set variant
    c = (c & 0x3FFFFFFF) | 0x80000000;

    char buf[37];
    std::snprintf(buf, sizeof(buf),
        "%08x-%04x-%04x-%04x-%04x%08x",
        a,
        (b >> 16) & 0xFFFF,
        b & 0xFFFF,
        (c >> 16) & 0xFFFF,
        c & 0xFFFF,
        d);
    return std::string(buf);
}

} // namespace copilot

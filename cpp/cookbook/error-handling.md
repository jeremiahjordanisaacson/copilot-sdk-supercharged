# Error Handling in C++

Patterns for handling errors when using the GitHub Copilot SDK in C++.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors gracefully while ensuring resources are properly cleaned up via RAII.

## Wrapping Client Lifecycle with RAII

Use a wrapper class to guarantee cleanup even when exceptions are thrown.

```cpp
#include <copilot/client.h>
#include <iostream>
#include <stdexcept>

class ScopedCopilotClient {
public:
    ScopedCopilotClient(const CopilotOptions& opts) : client_(opts) {
        client_.start();
    }

    ~ScopedCopilotClient() {
        try {
            client_.stop();
        } catch (const std::exception& e) {
            std::cerr << "Error stopping client: " << e.what() << std::endl;
        }
    }

    CopilotClient& get() { return client_; }

    // Prevent copying
    ScopedCopilotClient(const ScopedCopilotClient&) = delete;
    ScopedCopilotClient& operator=(const ScopedCopilotClient&) = delete;

private:
    CopilotClient client_;
};
```

## Catching Connection Errors

```cpp
#include <copilot/client.h>
#include <iostream>
#include <stdexcept>
#include <thread>
#include <chrono>

bool tryConnect(CopilotOptions& opts, int maxRetries) {
    for (int attempt = 1; attempt <= maxRetries; ++attempt) {
        try {
            CopilotClient client(opts);
            client.start();
            std::cout << "Connected on attempt " << attempt << std::endl;
            client.stop();
            return true;
        } catch (const std::runtime_error& e) {
            std::cerr << "Attempt " << attempt << " failed: " << e.what() << std::endl;
            if (attempt < maxRetries) {
                auto delay = std::chrono::seconds(attempt * 2);
                std::this_thread::sleep_for(delay);
            }
        }
    }
    return false;
}
```

## Handling Session and Request Errors

```cpp
#include <copilot/client.h>
#include <iostream>
#include <stdexcept>

void safeConversation(CopilotClient& client) {
    try {
        SessionConfig config;
        auto session = client.createSession(config);

        SendOptions opts;
        opts.message = "Explain RAII in C++";

        auto response = session.sendAndWait(opts);
        std::cout << "Response: " << response.message << std::endl;
    } catch (const std::invalid_argument& e) {
        std::cerr << "Invalid request: " << e.what() << std::endl;
    } catch (const std::runtime_error& e) {
        std::cerr << "Runtime error: " << e.what() << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "Unexpected error: " << e.what() << std::endl;
    }
}
```

## Timeout Handling

```cpp
#include <copilot/client.h>
#include <future>
#include <chrono>
#include <iostream>
#include <stdexcept>

std::string sendWithTimeout(CopilotSession& session, const std::string& message,
                            std::chrono::seconds timeout) {
    SendOptions opts;
    opts.message = message;

    auto future = std::async(std::launch::async, [&]() {
        return session.sendAndWait(opts);
    });

    if (future.wait_for(timeout) == std::future_status::timeout) {
        throw std::runtime_error("Request timed out after " +
                                  std::to_string(timeout.count()) + " seconds");
    }

    auto response = future.get();
    return response.message;
}

// Usage
void example(CopilotSession& session) {
    try {
        auto reply = sendWithTimeout(session, "Hello", std::chrono::seconds(30));
        std::cout << reply << std::endl;
    } catch (const std::runtime_error& e) {
        std::cerr << "Timeout or error: " << e.what() << std::endl;
    }
}
```

## Full Example with Comprehensive Error Handling

```cpp
#include <copilot/client.h>
#include <iostream>
#include <stdexcept>

int main() {
    try {
        CopilotOptions opts;
        ScopedCopilotClient scopedClient(opts);
        auto& client = scopedClient.get();

        SessionConfig config;
        auto session = client.createSession(config);

        SendOptions sendOpts;
        sendOpts.message = "What is the rule of five in C++?";

        auto response = session.sendAndWait(sendOpts);
        std::cout << "Reply: " << response.message << std::endl;

        return 0;
    } catch (const std::runtime_error& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return 1;
    } catch (const std::exception& e) {
        std::cerr << "Unexpected error: " << e.what() << std::endl;
        return 2;
    }
}
```

## Best Practices

- **Use RAII for cleanup**: Wrap `CopilotClient` in a scoped object to guarantee `stop()` is called.
- **Catch specific exceptions first**: Handle `std::invalid_argument`, `std::runtime_error`, then `std::exception`.
- **Implement exponential backoff**: Retry connection failures with increasing delay intervals.
- **Use `std::async` for timeouts**: Wrap blocking calls in a future and wait with a timeout.
- **Log errors before rethrowing**: Always capture error context before propagating exceptions.
- **Never swallow exceptions silently**: At minimum, log the error message for debugging.

# Multiple Sessions in C++

Patterns for managing multiple independent conversations with the GitHub Copilot SDK in C++.

## Scenario

Your application needs to run several conversations at the same time, each with its own context and history, sharing a single client connection.

## Creating Multiple Sessions from One Client

```cpp
#include <copilot/client.h>
#include <iostream>
#include <string>
#include <vector>

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    // Create separate sessions for different tasks
    SessionConfig codeConfig;
    codeConfig.systemPrompt = "You are a C++ expert.";
    auto codeSession = client.createSession(codeConfig);

    SessionConfig docsConfig;
    docsConfig.systemPrompt = "You are a technical writer.";
    auto docsSession = client.createSession(docsConfig);

    // Each session maintains its own conversation history
    auto codeReply = codeSession.sendAndWait(
        SendOptions{.message = "Explain smart pointers"}
    );
    std::cout << "Code: " << codeReply.message << std::endl;

    auto docsReply = docsSession.sendAndWait(
        SendOptions{.message = "Write a README template"}
    );
    std::cout << "Docs: " << docsReply.message << std::endl;

    client.stop();
    return 0;
}
```

## Managing Sessions in a Map

```cpp
#include <copilot/client.h>
#include <unordered_map>
#include <string>
#include <iostream>
#include <memory>

class SessionManager {
public:
    explicit SessionManager(CopilotClient& client) : client_(client) {}

    CopilotSession& getOrCreate(const std::string& name,
                                 const std::string& systemPrompt = "") {
        auto it = sessions_.find(name);
        if (it != sessions_.end()) {
            return *(it->second);
        }

        SessionConfig config;
        if (!systemPrompt.empty()) {
            config.systemPrompt = systemPrompt;
        }

        auto session = std::make_unique<CopilotSession>(
            client_.createSession(config)
        );
        auto& ref = *session;
        sessions_[name] = std::move(session);
        return ref;
    }

    void remove(const std::string& name) {
        sessions_.erase(name);
    }

    size_t count() const { return sessions_.size(); }

private:
    CopilotClient& client_;
    std::unordered_map<std::string, std::unique_ptr<CopilotSession>> sessions_;
};

// Usage
int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    SessionManager manager(client);

    auto& frontend = manager.getOrCreate("frontend", "You are a React expert.");
    auto& backend = manager.getOrCreate("backend", "You are a Go backend expert.");

    auto r1 = frontend.sendAndWait(SendOptions{.message = "Explain hooks"});
    auto r2 = backend.sendAndWait(SendOptions{.message = "Explain goroutines"});

    std::cout << "Frontend: " << r1.message << std::endl;
    std::cout << "Backend: " << r2.message << std::endl;

    client.stop();
    return 0;
}
```

## Concurrent Sessions with std::async

```cpp
#include <copilot/client.h>
#include <future>
#include <vector>
#include <string>
#include <iostream>

struct Task {
    std::string name;
    std::string prompt;
    std::string systemPrompt;
};

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    std::vector<Task> tasks = {
        {"code-review", "Review this function for bugs", "You review C++ code."},
        {"docs", "Generate API docs", "You write documentation."},
        {"tests", "Write unit tests", "You write C++ tests with GoogleTest."},
    };

    std::vector<std::future<std::pair<std::string, std::string>>> futures;

    for (const auto& task : tasks) {
        futures.push_back(std::async(std::launch::async, [&client, task]() {
            SessionConfig config;
            config.systemPrompt = task.systemPrompt;
            auto session = client.createSession(config);

            auto response = session.sendAndWait(
                SendOptions{.message = task.prompt}
            );
            return std::make_pair(task.name, response.message);
        }));
    }

    for (auto& f : futures) {
        auto [name, result] = f.get();
        std::cout << "[" << name << "] " << result << std::endl;
    }

    client.stop();
    return 0;
}
```

## Best Practices

- **Reuse a single client**: Create one `CopilotClient` and share it across all sessions.
- **Use unique_ptr for session ownership**: Store sessions in smart pointers to avoid dangling references.
- **Use std::async for parallel requests**: Run independent sessions concurrently for better throughput.
- **Name your sessions**: Use a map with descriptive keys to manage sessions by purpose.
- **Set distinct system prompts**: Give each session a focused persona for better results.
- **Clean up sessions you no longer need**: Remove finished sessions from the map to free memory.

# Persisting Sessions in C++

Patterns for saving and resuming Copilot sessions across application restarts in C++.

## Scenario

Your application needs to save a conversation so users can close the app and return later to continue exactly where they left off.

## Saving a Session ID to Disk

```cpp
#include <copilot/client.h>
#include <fstream>
#include <string>
#include <iostream>

bool saveSessionId(const std::string& sessionId, const std::string& filePath) {
    std::ofstream out(filePath);
    if (!out.is_open()) {
        std::cerr << "Failed to open file for writing: " << filePath << std::endl;
        return false;
    }
    out << sessionId;
    return out.good();
}

std::string loadSessionId(const std::string& filePath) {
    std::ifstream in(filePath);
    if (!in.is_open()) return "";

    std::string sessionId;
    std::getline(in, sessionId);
    return sessionId;
}
```

## Resuming a Session

```cpp
#include <copilot/client.h>
#include <iostream>
#include <string>
#include <fstream>

const std::string SESSION_FILE = "session_state.txt";

CopilotSession getOrCreateSession(CopilotClient& client) {
    std::string savedId = loadSessionId(SESSION_FILE);

    if (!savedId.empty()) {
        try {
            SessionConfig config;
            config.sessionId = savedId;
            auto session = client.createSession(config);
            std::cout << "Resumed session: " << savedId << std::endl;
            return session;
        } catch (const std::exception& e) {
            std::cerr << "Could not resume session: " << e.what() << std::endl;
        }
    }

    // Create a new session and save the ID
    SessionConfig config;
    auto session = client.createSession(config);
    saveSessionId(session.getId(), SESSION_FILE);
    std::cout << "Created new session: " << session.getId() << std::endl;
    return session;
}
```

## Full Example: Persistent Chat

```cpp
#include <copilot/client.h>
#include <iostream>
#include <string>
#include <fstream>

const std::string SESSION_FILE = "session_state.txt";

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    auto session = getOrCreateSession(client);

    std::string input;
    std::cout << "Chat (type 'quit' to exit):" << std::endl;

    while (true) {
        std::cout << "> ";
        std::getline(std::cin, input);
        if (input == "quit") break;

        try {
            auto response = session.sendAndWait(
                SendOptions{.message = input}
            );
            std::cout << "Assistant: " << response.message << std::endl;
        } catch (const std::exception& e) {
            std::cerr << "Error: " << e.what() << std::endl;
        }
    }

    // Session ID is already saved, so next run will resume
    client.stop();
    std::cout << "Session saved. You can resume later." << std::endl;
    return 0;
}
```

## Storing Multiple Sessions with JSON

```cpp
#include <copilot/client.h>
#include <fstream>
#include <string>
#include <unordered_map>
#include <iostream>
#include <sstream>

// Simple key=value storage (use a JSON library for production)
using SessionMap = std::unordered_map<std::string, std::string>;

SessionMap loadSessions(const std::string& filePath) {
    SessionMap sessions;
    std::ifstream in(filePath);
    std::string line;

    while (std::getline(in, line)) {
        auto pos = line.find('=');
        if (pos != std::string::npos) {
            sessions[line.substr(0, pos)] = line.substr(pos + 1);
        }
    }
    return sessions;
}

void saveSessions(const SessionMap& sessions, const std::string& filePath) {
    std::ofstream out(filePath);
    for (const auto& [name, id] : sessions) {
        out << name << "=" << id << "\n";
    }
}
```

## Best Practices

- **Persist only the session ID**: The SDK and CLI handle conversation state internally. You just need the ID.
- **Try to resume before creating**: Attempt to restore a saved session first, then fall back to a new one.
- **Handle stale sessions**: If resuming fails (session expired), catch the error and create a fresh session.
- **Use RAII for file handles**: Let `std::ifstream`/`std::ofstream` destructors close files automatically.
- **Store session IDs per user or workspace**: Use a map to support multiple persistent sessions.
- **Save on creation, not on exit**: Write the session ID immediately after creating it to avoid data loss on crashes.

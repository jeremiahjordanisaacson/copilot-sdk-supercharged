# Advanced v2.0 Features in C++

Recipes for using advanced v2.0 features of the GitHub Copilot SDK in C++, including per-session auth, SessionFs, commands, system prompts, skills, streaming, and more.

## Scenario

Your application needs fine-grained control over authentication, filesystem access, prompt customization, agent skills, and other advanced capabilities introduced in SDK v2.0.

## 1. Per-Session Authentication

Supply a GitHub token per session instead of globally.

```cpp
#include <copilot/client.h>
#include <iostream>

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    SessionConfig config;
    config.githubToken = "ghu_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";

    auto session = client.createSession(config);
    auto response = session.sendAndWait(
        SendOptions{.message = "Who am I authenticated as?"}
    );
    std::cout << response.message << std::endl;

    client.stop();
    return 0;
}
```

## 2. Session Idle Timeout

Automatically expire sessions after a period of inactivity.

```cpp
SessionConfig config;
config.sessionIdleTimeoutSeconds = 600;  // 10-minute idle timeout

auto session = client.createSession(config);

// Session will be cleaned up automatically if idle for 10 minutes
auto response = session.sendAndWait(
    SendOptions{.message = "This session expires after 10 minutes of inactivity."}
);
```

## 3. SessionFs (Session Filesystem)

Configure a filesystem provider for the session with fine-grained I/O operations.

```cpp
#include <copilot/client.h>
#include <copilot/session_fs.h>
#include <string>
#include <vector>

// Implement the 10 required I/O operations
class MySessionFsProvider : public SessionFsProvider {
public:
    FsResult readFile(const std::string& path) override {
        // Read file contents from your backing store
        return FsResult{.content = "file content here"};
    }

    FsResult writeFile(const std::string& path,
                       const std::string& content) override {
        // Write content to your backing store
        return FsResult{.success = true};
    }

    FsResult deleteFile(const std::string& path) override {
        return FsResult{.success = true};
    }

    FsResult listDirectory(const std::string& path) override {
        return FsResult{.entries = {"file1.txt", "file2.txt"}};
    }

    FsResult createDirectory(const std::string& path) override {
        return FsResult{.success = true};
    }

    FsResult stat(const std::string& path) override {
        return FsResult{.size = 1024, .isDirectory = false};
    }

    FsResult rename(const std::string& oldPath,
                    const std::string& newPath) override {
        return FsResult{.success = true};
    }

    FsResult copy(const std::string& src,
                  const std::string& dst) override {
        return FsResult{.success = true};
    }

    FsResult exists(const std::string& path) override {
        return FsResult{.exists = true};
    }

    FsResult watch(const std::string& path,
                   FsWatchCallback callback) override {
        // Set up file watching
        return FsResult{.success = true};
    }
};

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    auto fsProvider = std::make_shared<MySessionFsProvider>();

    SessionConfig config;
    config.sessionFs = SessionFsConfig{
        .enabled = true,
        .rootPath = "/workspace/project",
        .provider = fsProvider
    };

    auto session = client.createSession(config);
    auto response = session.sendAndWait(
        SendOptions{.message = "List the files in the workspace."}
    );
    std::cout << response.message << std::endl;

    client.stop();
    return 0;
}
```

## 4. Commands and UI Elicitation

Register slash commands and handle UI elicitation requests from the agent.

```cpp
#include <copilot/client.h>
#include <iostream>
#include <vector>

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);
    client.start();

    // Define available commands
    std::vector<Command> commands = {
        Command{.name = "/deploy", .description = "Deploy the current project"},
        Command{.name = "/test", .description = "Run the test suite"},
        Command{.name = "/status", .description = "Show project status"},
    };

    // Handle elicitation: agent asks the user for input
    auto elicitationHandler = [](const ElicitationRequest& req) -> ElicitationResponse {
        std::cout << "Agent asks: " << req.message << std::endl;
        // In a real app, prompt the user for input here
        if (req.type == ElicitationType::Confirmation) {
            return ElicitationResponse{.confirmed = true};
        }
        return ElicitationResponse{.text = "user-provided-value"};
    };

    SessionConfig config;
    config.commands = commands;
    config.elicitationHandler = elicitationHandler;

    auto session = client.createSession(config);
    auto response = session.sendAndWait(
        SendOptions{.message = "/deploy to staging"}
    );
    std::cout << response.message << std::endl;

    client.stop();
    return 0;
}
```

## 5. System Prompt Customization

Use replace or customize modes with structured sections to control the system prompt.

```cpp
#include <copilot/client.h>

// Mode 1: Replace the entire system prompt
SessionConfig replaceConfig;
replaceConfig.systemPrompt = SystemPrompt{
    .mode = SystemPromptMode::Replace,
    .content = "You are a C++ build system expert. Only discuss CMake and Bazel."
};

auto session1 = client.createSession(replaceConfig);

// Mode 2: Customize with structured sections
SessionConfig customizeConfig;
customizeConfig.systemPrompt = SystemPrompt{
    .mode = SystemPromptMode::Customize,
    .sections = {
        PromptSection{
            .id = "role",
            .content = "You are a senior C++ developer."
        },
        PromptSection{
            .id = "constraints",
            .content = "Always use modern C++20 features. Prefer std::span over raw pointers."
        },
        PromptSection{
            .id = "output_format",
            .content = "Format all code with clang-format Google style."
        },
    }
};

auto session2 = client.createSession(customizeConfig);
```

## 6. Per-Agent Skills

Configure skill directories and disable specific skills per agent.

```cpp
SessionConfig config;
config.skillDirectories = {
    "/home/user/.copilot/skills",
    "/project/.copilot/skills"
};
config.disabledSkills = {"web-search", "code-execution"};

auto session = client.createSession(config);
auto response = session.sendAndWait(
    SendOptions{.message = "Use the custom project skills to analyze the codebase."}
);
```

## 7. Per-Agent Tool Visibility

Hide specific tools from certain agents using excludedTools.

```cpp
// Create a read-only agent that cannot modify files
SessionConfig readOnlyConfig;
readOnlyConfig.systemPrompt = "You are a code reviewer. Analyze but do not modify.";
readOnlyConfig.excludedTools = {
    "write_file", "delete_file", "execute_command", "create_directory"
};

auto reviewer = client.createSession(readOnlyConfig);

// Create a full-access agent
SessionConfig fullConfig;
fullConfig.systemPrompt = "You are a developer with full access.";

auto developer = client.createSession(fullConfig);
```

## 8. Runtime Request Headers

Attach custom headers to individual requests for tracing or auth.

```cpp
SendOptions sendOpts;
sendOpts.message = "Summarize the latest deploy logs.";
sendOpts.requestHeaders = {
    {"X-Request-Id", "req-abc-123"},
    {"X-Trace-Id", "trace-xyz-789"},
    {"Authorization", "Bearer custom-token-here"},
};

auto response = session.sendAndWait(sendOpts);
```

## 9. Model Capabilities Override

Override model capabilities for a session to control behavior.

```cpp
SessionConfig config;
config.modelCapabilities = ModelCapabilities{
    .streaming = true,
    .toolCalling = true,
    .vision = false,
    .maxTokens = 8192,
    .contextWindow = 128000,
};

auto session = client.createSession(config);
```

## 10. Config Discovery

Enable automatic discovery of project-level configuration files.

```cpp
SessionConfig config;
config.enableConfigDiscovery = true;

auto session = client.createSession(config);

// The SDK will automatically discover and load:
//   .copilot/config.yml
//   .copilot/prompts/*.md
//   .copilot/skills/
// from the project root and parent directories.
auto response = session.sendAndWait(
    SendOptions{.message = "What project configuration did you discover?"}
);
```

## 11. Sub-Agent Streaming Events

Receive streaming events from sub-agents during orchestration.

```cpp
#include <copilot/client.h>
#include <iostream>

SessionConfig config;
config.includeSubAgentStreamingEvents = true;

auto session = client.createSession(config);

session.onEvent([](const SessionEvent& event) {
    if (event.type == "assistant.message_delta") {
        std::cout << "[delta] " << event.content << std::flush;
    } else if (event.type == "sub_agent.message_delta") {
        std::cout << "[sub-agent:" << event.agentId << "] "
                  << event.content << std::flush;
    }
});

session.send(SendOptions{.message = "Coordinate the planner and coder agents."});
```

## 12. Session Metadata

Retrieve metadata about the current session.

```cpp
#include <copilot/client.h>
#include <iostream>

auto session = client.createSession(config);

auto metadata = session.getSessionMetadata();
std::cout << "Session ID:  " << metadata.sessionId << std::endl;
std::cout << "Created at:  " << metadata.createdAt << std::endl;
std::cout << "Turn count:  " << metadata.turnCount << std::endl;
std::cout << "Model:       " << metadata.model << std::endl;
std::cout << "Token usage: " << metadata.totalTokens << std::endl;
```

## 13. MCP Server Configuration

Configure Model Context Protocol servers using stdio or HTTP transports.

```cpp
#include <copilot/client.h>

SessionConfig config;

// Stdio transport: launch a local MCP server process
config.mcpServers = {
    McpServerConfig{
        .name = "filesystem",
        .transport = McpTransport::Stdio,
        .command = "npx",
        .args = {"-y", "@modelcontextprotocol/server-filesystem", "/workspace"},
    },
    // HTTP transport: connect to a remote MCP server
    McpServerConfig{
        .name = "remote-db",
        .transport = McpTransport::Http,
        .url = "https://mcp.example.com/db",
        .headers = {{"Authorization", "Bearer token123"}},
    },
};

auto session = client.createSession(config);
auto response = session.sendAndWait(
    SendOptions{.message = "List the files in the workspace using the MCP server."}
);
```

## 14. Image Generation

Configure the response format to request image generation.

```cpp
#include <copilot/client.h>
#include <fstream>
#include <iostream>

SessionConfig config;
config.responseFormat = ResponseFormat{
    .type = ResponseFormatType::ImageGeneration,
    .imageSize = "1024x1024",
    .imageQuality = "high",
};

auto session = client.createSession(config);
auto response = session.sendAndWait(
    SendOptions{.message = "Generate an image of a futuristic cityscape at sunset."}
);

// Save the generated image
if (!response.imageData.empty()) {
    std::ofstream out("cityscape.png", std::ios::binary);
    out.write(response.imageData.data(), response.imageData.size());
    std::cout << "Image saved to cityscape.png" << std::endl;
}
```

## Best Practices

- **Scope tokens tightly**: Use per-session `githubToken` with minimal scopes for multi-tenant apps.
- **Use RAII for cleanup**: Wrap sessions in smart pointers or scoped objects to ensure proper cleanup.
- **Combine features**: Per-agent skills, tool visibility, and system prompts work together to create specialized agents.
- **Use config discovery in dev**: Enable `enableConfigDiscovery` during development so project-level configs are picked up automatically.
- **Handle streaming events asynchronously**: Use callbacks or event loops for delta events to avoid blocking the main thread.
- **Set idle timeouts in server apps**: Use `sessionIdleTimeoutSeconds` to prevent resource leaks from abandoned sessions.

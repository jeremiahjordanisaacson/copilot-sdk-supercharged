# Tools and Skills in C++

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in C++.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <iostream>
#include <string>
#include <ctime>

// Define a tool that returns the current time
auto timeTool = DefineTool(
    "get_current_time",
    "Returns the current date and time",
    [](const ToolContext& ctx) -> ToolResult {
        auto now = std::time(nullptr);
        char buf[64];
        std::strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S",
                      std::localtime(&now));
        return ToolResult{.content = std::string(buf)};
    }
);
```

## Defining a Tool with Parameters

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <fstream>
#include <sstream>
#include <string>

// Tool that reads a file
auto readFileTool = DefineTool(
    "read_file",
    "Reads the contents of a file given its path",
    [](const ToolContext& ctx) -> ToolResult {
        auto path = ctx.getParam<std::string>("path");
        if (path.empty()) {
            return ToolResult{.error = "Missing required parameter: path"};
        }

        std::ifstream file(path);
        if (!file.is_open()) {
            return ToolResult{.error = "Could not open file: " + path};
        }

        std::ostringstream ss;
        ss << file.rdbuf();
        return ToolResult{.content = ss.str()};
    }
);
```

## Registering Tools with a Client

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <iostream>

int main() {
    CopilotOptions opts;
    CopilotClient client(opts);

    // Register tools before starting
    client.addTool(timeTool);
    client.addTool(readFileTool);

    client.start();

    SessionConfig config;
    config.systemPrompt = "You have access to file reading and time tools.";
    auto session = client.createSession(config);

    auto response = session.sendAndWait(
        SendOptions{.message = "What time is it and what is in config.json?"}
    );
    std::cout << response.message << std::endl;

    client.stop();
    return 0;
}
```

## Defining Multiple Related Tools (Skill Pattern)

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <vector>
#include <string>
#include <unordered_map>

// In-memory key-value store as a skill
class KVStoreSkill {
public:
    std::vector<ToolDefinition> tools() {
        return {
            DefineTool("kv_get", "Get a value by key",
                [this](const ToolContext& ctx) -> ToolResult {
                    auto key = ctx.getParam<std::string>("key");
                    auto it = store_.find(key);
                    if (it == store_.end()) {
                        return ToolResult{.content = "Key not found: " + key};
                    }
                    return ToolResult{.content = it->second};
                }
            ),
            DefineTool("kv_set", "Set a key-value pair",
                [this](const ToolContext& ctx) -> ToolResult {
                    auto key = ctx.getParam<std::string>("key");
                    auto value = ctx.getParam<std::string>("value");
                    store_[key] = value;
                    return ToolResult{.content = "Stored: " + key};
                }
            ),
            DefineTool("kv_list", "List all keys",
                [this](const ToolContext& ctx) -> ToolResult {
                    std::string result;
                    for (const auto& [k, v] : store_) {
                        result += k + "\n";
                    }
                    return ToolResult{.content = result.empty() ? "Empty" : result};
                }
            ),
        };
    }

private:
    std::unordered_map<std::string, std::string> store_;
};

// Register the skill
int main() {
    CopilotOptions opts;
    CopilotClient client(opts);

    KVStoreSkill kvSkill;
    for (auto& tool : kvSkill.tools()) {
        client.addTool(tool);
    }

    client.start();

    SessionConfig config;
    auto session = client.createSession(config);
    auto response = session.sendAndWait(
        SendOptions{.message = "Store my name as 'Alice' and then retrieve it"}
    );
    std::cout << response.message << std::endl;

    client.stop();
    return 0;
}
```

## Sub-Agent Orchestration

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <iostream>
#include <string>

void orchestrate(CopilotClient& client) {
    // Create specialized sessions as sub-agents
    SessionConfig plannerConfig;
    plannerConfig.systemPrompt = "You are a planner. Break tasks into steps.";
    auto planner = client.createSession(plannerConfig);

    SessionConfig coderConfig;
    coderConfig.systemPrompt = "You are a coder. Write C++ code for given tasks.";
    auto coder = client.createSession(coderConfig);

    SessionConfig reviewerConfig;
    reviewerConfig.systemPrompt = "You are a code reviewer. Review C++ code for bugs.";
    auto reviewer = client.createSession(reviewerConfig);

    // Step 1: Plan
    auto plan = planner.sendAndWait(
        SendOptions{.message = "Plan a linked list implementation in C++"}
    );
    std::cout << "Plan: " << plan.message << std::endl;

    // Step 2: Code
    auto code = coder.sendAndWait(
        SendOptions{.message = "Implement this plan:\n" + plan.message}
    );
    std::cout << "Code: " << code.message << std::endl;

    // Step 3: Review
    auto review = reviewer.sendAndWait(
        SendOptions{.message = "Review this code:\n" + code.message}
    );
    std::cout << "Review: " << review.message << std::endl;
}
```

## Best Practices

- **Keep tool handlers focused**: Each tool should do one thing and return a clear result.
- **Validate parameters early**: Check for required parameters at the top of the handler.
- **Return errors in ToolResult**: Use the error field rather than throwing exceptions from tool handlers.
- **Group related tools into skills**: Use a class to encapsulate state shared across related tools.
- **Use separate sessions for sub-agents**: Each agent gets its own system prompt and conversation context.
- **Chain agent outputs**: Pass the output of one agent as input to the next for pipeline workflows.

# Copilot Supercharged SDK for C++

C++ SDK for the GitHub Copilot CLI. Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio pipes using Content-Length header framing.

## Requirements

- C++17 compatible compiler (GCC 8+, Clang 7+, MSVC 2019+)
- CMake 3.16+
- [nlohmann/json](https://github.com/nlohmann/json) (automatically fetched via CMake FetchContent)
- [GitHub Copilot CLI](https://github.com/github/copilot) 1.0.113 or newer, installed and available on PATH (or specify path via `CopilotClientOptions::cliPath`; `.js` entry points such as `npm-loader.js` run under `node`)

## Building

```bash
mkdir build && cd build
cmake ..
cmake --build .
```

To disable building examples:

```bash
cmake .. -DCOPILOT_SDK_BUILD_EXAMPLES=OFF
```

## Quick Start

```cpp
#include <copilot/client.h>
#include <copilot/define_tool.h>
#include <copilot/types.h>
#include <iostream>

int main() {
    // Create and start client
    copilot::CopilotClient client;
    client.start();

    // Create a session
    auto session = client.createSession();

    // Subscribe to events
    session->on([](const copilot::SessionEvent& event) {
        if (event.type == "assistant.message") {
            std::cout << event.data.value("content", "") << std::endl;
        }
    });

    // Send a message and wait for completion
    auto response = session->sendAndWait({"What is 2+2?"});

    // Clean up
    session->destroy();
    client.stop();
    return 0;
}
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `CopilotClient` methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, thrown as `copilot::JsonRpcException` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store (`mcp.secrets.save`, `mcp.secrets.delete`, `mcp.secrets.deleteServer`). Each method takes an optional `configDir` as its last argument. The secret value is never logged:

```cpp
copilot::McpSecretsSaveResult saved = client.mcpSecretsSave("my-server-token", readToken());
std::cout << (saved.usedKeychain ? "Stored in the OS keychain" : "Stored in the file backend") << "\n";

client.mcpSecretsDelete("my-server-token");
client.mcpSecretsDeleteServer("my-server");  // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list, and revoke user-scoped approvals for installed plugin extensions (`permissions.extensionApprovals.grant`, `.list`, `.revoke`). `extensionApprovalsList()` and `extensionApprovalsRevoke()` take an optional session ID to use an attached session's configuration profile:

```cpp
// Persist a pending extension permission request before answering it normally
auto grant = client.extensionApprovalsGrant(session->sessionId, requestId);
if (grant.granted && grant.approval) {
    std::cout << "Approved " << grant.approval->extensionName << "\n";
}

for (const auto& approval : client.extensionApprovalsList().approvals) {
    // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
    client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey);
}
```

### Ask-user image attachments

When the runtime accepts images in an answer, `UserInputRequest::imageAttachmentConstraints` is set (`allowedMimeTypes`, `maxBytesPerImage`, `maxCount`); it is `std::nullopt` otherwise. The request also carries `requestId` and `meta` (the opaque `_meta` object) when the runtime sends them. Answer with `UserInputResponse::attachments`, a list of `copilot::UserInputImageAttachment` (base64 `data`, `mimeType`, optional `description` and `displayName`; the wire `type` is always `"image"`). Attachments are sent only when set; image-only answers use an empty `answer` and `wasFreeform = true`, and the runtime enforces the limits:

```cpp
config.onUserInputRequest = [](const copilot::UserInputRequest& req, const std::string& sessionId) {
    copilot::UserInputResponse response{"No screenshot available", true};
    const auto& constraints = req.imageAttachmentConstraints;
    if (constraints && std::find(constraints->allowedMimeTypes.begin(), constraints->allowedMimeTypes.end(),
                                 "image/png") != constraints->allowedMimeTypes.end()) {
        copilot::UserInputImageAttachment image;
        image.data = base64Encode(readFile("screenshot.png"));
        image.mimeType = "image/png";
        image.displayName = "screenshot.png";
        response = copilot::UserInputResponse{"", true, std::vector<copilot::UserInputImageAttachment>{image}};
    }
    return response;
};
```

## What's New in v3.0.0

The SDK now speaks **SDK protocol 4** (the protocol of the Microsoft Copilot SDK 2.0 runtime) and requires **Copilot CLI 1.0.113 or newer**. Older CLIs are rejected during `start()` with an "SDK protocol version mismatch" error.

Existing code keeps working unchanged. You still fill the same flat `SessionConfig` / `ResumeSessionConfig` fields; the SDK translates them into the grouped protocol 4 `session.create` / `session.resume` payload internally (for example `streaming` is sent as `events.streaming` and `onPermissionRequest` as `permissions.request`). A legacy single `provider` is sent as a named provider called `default` with Copilot API models disabled, which preserves the v1 BYOK behavior. `ping()` now also reads the ISO 8601 timestamp that current CLIs report.

### Named BYOK providers

Register provider connections and model definitions, then select a model as `<provider>/<id>`. A default provider requires Copilot API models to be disabled:

```cpp
copilot::NamedProviderConfig gateway;
gateway.name = "gateway";
gateway.type = "openai";
gateway.baseUrl = "https://models.example.com/v1";
gateway.apiKeyCommand = "get-model-key";

copilot::ProviderModelConfig chat;
chat.provider = "gateway";
chat.id = "chat";
chat.wireModel = "org/chat-large";
chat.maxOutputTokens = 8192;

copilot::SessionConfig config;
config.model = "gateway/chat";
config.providers = std::vector<copilot::NamedProviderConfig>{gateway};
config.models = std::vector<copilot::ProviderModelConfig>{chat};
config.defaultProvider = "gateway";
config.capi = copilot::CapiConfig{false};
```

On resume, set `ResumeSessionConfig::clearDefaultProvider = true` to clear the default provider.

### Idempotent tools and client turn IDs

```cpp
copilot::Tool lookup;
lookup.name = "lookup_order";
lookup.description = "Looks up an order";
lookup.handler = lookupOrder;
lookup.idempotent = true;  // safe to retry

copilot::MessageOptions msg;
msg.prompt = "Summarize the diff";
msg.clientTurnId = "turn-42";
session->send(msg);
```

### Client mode

`copilot::ClientMode::CodingAgent` (the default, identical to earlier releases) or `copilot::ClientMode::Empty`. Empty mode requires `availableTools` on every create and resume (otherwise `std::invalid_argument` is thrown) and applies minimal defaults (experimental mode, telemetry, cross-session store, memory, on-demand instructions, repository hooks, and host git operations off; in-memory OAuth and embedding storage) underneath your own values:

```cpp
copilot::CopilotClientOptions options;
options.mode = copilot::ClientMode::Empty;
copilot::CopilotClient client(options);

copilot::SessionConfig config;
config.availableTools = std::vector<std::string>{"view"};
auto session = client.createSession(config);
```

### Live shell output

```cpp
session->on(copilot::SessionEventType::ToolShellOutput, [](const copilot::SessionEvent& event) {
    if (auto output = event.shellOutput()) {
        std::cerr << "[" << output->stream.value_or(copilot::ShellOutputStream::Stdout)
                  << " #" << output->sequence << "] " << output->text;
    }
});
```

### Shell authentication

Answer password and confirmation prompts from commands such as `sudo`, `ssh`, or `git`. The answer goes only to the program that asked. `invocation.isCancelled()` turns true when the runtime withdraws the prompt:

```cpp
config.onShellAuthenticationRequest = [](const copilot::ShellAuthenticationRequest& request,
                                         const copilot::ShellAuthenticationInvocation& invocation) {
    if (request.program != "sudo" || invocation.isCancelled()) {
        return copilot::ShellAuthenticationResponse::cancel();
    }
    return copilot::ShellAuthenticationResponse::accept(readSecret(request.prompt));
};
```

### Memory store

```cpp
config.storeMemory = [&memoryLog](const copilot::StoredMemory& memory) {
    memoryLog << memory.subject << ": " << memory.fact << " (" << memory.citations << ")\n";
};
```

### Network callbacks

Callbacks are registered with `network.setProvider` during `start()`. The SDK never logs tokens or passwords:

```cpp
copilot::NetworkOptions network;
network.scope = copilot::NetworkScope::Connection;
network.resolveProxy = [](const std::string& url) -> std::optional<std::string> {
    return std::string("PROXY proxy.corp.example:8080; DIRECT");
};
network.authenticateBasic = [](const copilot::NetworkBasicAuthRequest& request) {
    return copilot::NetworkBasicCredentials{"proxy-user", lookupProxyPassword(request.proxy)};
};

copilot::CopilotClientOptions options;
options.network = network;
```

### Policy Helper failures and permission mode refusals

RPC failures now throw `copilot::JsonRpcException` (a `std::runtime_error` with `code()`, `rpcMessage()`, and `data()`):

```cpp
try {
    auto session = client.createSession(config);
} catch (const copilot::JsonRpcException& e) {
    if (auto failure = e.policyHelperFailure()) {
        std::cerr << "Policy Helper (" << failure->category << "): " << failure->message << "\n";
    }
}

auto result = session->setPermissionMode(copilot::PermissionMode::AllowAll);
if (!result.success && result.reason == copilot::PermissionModeRefusalReason::ManagedPolicy) {
    std::cerr << "Managed policy keeps the permission mode at " << result.mode << "\n";
}
```

`copilot::PolicyHelperFailure::from(session->getManagedSettings())` reads the same structure from a managed-settings snapshot. Set `config.permissionMode` to request a mode when the session is created.

### Resume without side effects

```cpp
copilot::ResumeSessionConfig resume;
resume.suppressResumeEvent = true;  // disableResume remains a legacy alias
resume.allowTranscriptRecovery = true;
auto session = client.resumeSession(sessionId, resume);
```

### Also included (upstream v2.6.0 parity)

```cpp
// Client identity on the connect handshake
options.clientInfo = copilot::CopilotClientInfo{std::string("my-editor"), std::string("1.2.0"),
                                                std::nullopt, std::nullopt};

// GitHub token provider (gitHubToken.getToken); mutually exclusive with gitHubToken
config.gitHubTokenProvider = [](const copilot::GitHubTokenProviderArgs& args) {
    return copilot::GitHubTokenProviderResult::token(fetchTokenForHost(args.host), 3600);
};

// Session options
config.askUserVariant = copilot::AskUserVariant::Elicitation;
config.featureFlags = std::map<std::string, bool>{{"newPlanner", true}};
config.allowedModels = std::vector<std::string>{"claude-sonnet-4.5"};
config.authClientIdMetadataUrl = "https://example.com/oauth/client-metadata.json";
config.agentDirectories = std::vector<std::string>{"./agents"};
config.hookDirectories = std::vector<std::string>{"./hooks"};
config.trustWorkingDirectory = copilot::TrustWorkingDirectory::Session;  // create only
config.modelCapabilities = nlohmann::json(copilot::ModelCapabilitiesOverride{
    std::nullopt, copilot::ModelLimitsOverride{std::nullopt, 4096, std::nullopt}});
copilot::SandboxConfig sandbox;
sandbox.enabled = true;
sandbox.allowBypass = true;
config.sandboxConfig = sandbox;
config.imageGeneration = copilot::ImageGenerationConfig{std::nullopt, std::nullopt, true};  // saveToDisk
config.autopilot = copilot::AutopilotOptions{true, 3};  // continuationEnabled, maxContinues
copilot::ProviderConfig provider;
provider.baseUrl = "https://models.example.com/v1";
provider.apiKeyCommand = "get-model-key";

// Sub-agent lifecycle hooks
copilot::SessionHooks hooks;
hooks.onSubagentStart = [](const copilot::SubagentStartHookInput& input, const std::string& sessionId) {
    return std::optional<copilot::SubagentStartHookOutput>{};
};
config.hooks = hooks;

// Message source and structured output
copilot::MessageOptions msg;
msg.prompt = "List three colors";
msg.source = copilot::MessageSource::System;
nlohmann::json colors = session->sendAndWaitStructured(
    msg, {{"type", "array"}, {"items", {{"type", "string"}}}});

// Session and client RPCs
session->setAutoTier(copilot::AutoTier::Fast);  // session.model.switchAutoTier
session->recheckPolicyHelper();
client.clearManagedSettingsCache();
session->destroy();  // session.detach: the session can be resumed later
```

### Known limitations

* The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not implemented. It is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process native runtime hosting is not the default connection. The C++ SDK keeps spawning the CLI over stdio (`inProcess` is unchanged).
* Elicitation requests are not dispatched to `onElicitationRequest`, so the elicitation `requestId` / `_meta` passthrough added upstream in 2.0.3-preview.0 does not apply to this SDK.

## Architecture

The SDK mirrors the architecture of all other SDKs in this repository (40 languages supported):

```
CopilotClient
  |-- Spawns CLI process (copilot --headless --no-auto-update --log-level info --stdio)
  |-- JsonRpcClient (Content-Length framed JSON-RPC 2.0 over pipes)
  |     |-- Reader thread (reads from CLI stdout)
  |     |-- Pending requests (std::promise/future)
  |     |-- Request handlers (for server->client calls)
  |-- Sessions (CopilotSession)
        |-- Event handlers (wildcard and typed)
        |-- Tool handlers
        |-- Permission handler
        |-- User input handler
        |-- Hook handlers
```

### Transport Protocol

Messages are framed using the LSP base protocol:

```
Content-Length: <byte-count>\r\n
\r\n
<JSON-RPC message>
```

### JSON-RPC Methods

**Client -> Server (requests):**
- `ping` - Verify connectivity and protocol version
- `status.get` - Get CLI version info
- `auth.getStatus` - Get authentication status
- `models.list` - List available models
- `session.create` - Create a new session
- `session.resume` - Resume an existing session
- `session.send` - Send a message to a session
- `session.getMessages` - Get session history
- `session.destroy` - Destroy a session
- `session.abort` - Abort current processing
- `session.delete` - Delete a session permanently
- `session.list` - List all sessions
- `session.getLastId` - Get the last session ID
- `session.getForeground` - Get foreground session (TUI mode)
- `session.setForeground` - Set foreground session (TUI mode)

**Server -> Client (notifications):**
- `session.event` - Session events (assistant.message, session.idle, etc.)
- `session.lifecycle` - Session lifecycle events (created, deleted, etc.)

**Server -> Client (requests):**
- `tool.call` - Execute a registered tool
- `permission.request` - Request permission for an operation
- `userInput.request` - Request input from the user
- `hooks.invoke` - Invoke a hook handler

## API Reference

### CopilotClient

The main entry point. Manages the CLI server connection and session lifecycle.

```cpp
copilot::CopilotClientOptions options;
options.cliPath = "/usr/local/bin/copilot";
options.logLevel = "debug";

copilot::CopilotClient client(options);
client.start();

// ... use sessions ...

client.stop();
```

### CopilotSession

Represents a conversation session.

```cpp
// Create with configuration
copilot::SessionConfig config;
config.model = "gpt-4";
config.tools = {myTool};
config.streaming = true;

auto session = client.createSession(config);

// Subscribe to specific event types
auto id = session->on("assistant.message", [](const copilot::SessionEvent& event) {
    std::cout << event.data.value("content", "") << std::endl;
});

// Send and wait
auto response = session->sendAndWait({"Hello!"}, 30000); // 30s timeout

// Unsubscribe
session->off(id);

// Clean up
session->destroy();
```

### Tools

Define custom tools that the assistant can invoke:

```cpp
auto tool = copilot::defineTool(
    "calculate",
    "Perform a calculation",
    nlohmann::json::parse(R"({
        "type": "object",
        "properties": {
            "expression": {"type": "string"}
        },
        "required": ["expression"]
    })"),
    [](const nlohmann::json& args, const copilot::ToolInvocation& inv)
        -> copilot::ToolResultObject {
        std::string expr = args.value("expression", "");
        // ... evaluate expression ...
        return copilot::toolSuccess("Result: 42");
    });
```

Helper functions for tool results:
- `copilot::toolSuccess(text)` - Create a successful result
- `copilot::toolFailure(message, error)` - Create a failed result
- `copilot::toolSuccessJson(json)` - Create a successful result from JSON

### Permissions

Handle permission requests from the assistant:

```cpp
config.onPermissionRequest = [](const copilot::PermissionRequest& req,
                                const std::string& sessionId)
    -> copilot::PermissionRequestResult {
    if (req.kind == "read") {
        return {"approved"};
    }
    return {"denied-interactively-by-user"};
};
```

### User Input

Handle user input requests (enables the ask_user tool):

```cpp
config.onUserInputRequest = [](const copilot::UserInputRequest& req,
                               const std::string& sessionId)
    -> copilot::UserInputResponse {
    std::cout << "Agent asks: " << req.question << std::endl;
    std::string answer;
    std::getline(std::cin, answer);
    return {answer, true};
};
```

When `req.imageAttachmentConstraints` is set, the answer may also carry image `attachments` (see [Ask-user image attachments](#ask-user-image-attachments)).

### Hooks

Intercept session lifecycle events:

```cpp
copilot::SessionHooks hooks;
hooks.onPreToolUse = [](const copilot::PreToolUseHookInput& input,
                        const std::string& sessionId)
    -> std::optional<copilot::PreToolUseHookOutput> {
    std::cout << "About to use tool: " << input.toolName << std::endl;
    return copilot::PreToolUseHookOutput{.permissionDecision = "allow"};
};

config.hooks = hooks;
```

### Session Lifecycle Events

Monitor session lifecycle at the client level:

```cpp
auto unsub = client.onLifecycle("session.created",
    [](const copilot::SessionLifecycleEvent& event) {
        std::cout << "Session created: " << event.sessionId << std::endl;
    });

// Later: unsub() to unsubscribe
```

### BYOK (Bring Your Own Key)

Use a custom model provider:

```cpp
copilot::SessionConfig config;
config.provider = copilot::ProviderConfig{
    .type = "openai",
    .baseUrl = "https://api.openai.com/v1",
    .apiKey = "sk-..."
};
config.model = "gpt-4";
```

### MCP Servers

Configure MCP (Model Context Protocol) servers:

```cpp
config.mcpServers = std::map<std::string, copilot::MCPServerConfig>{
    {"my-server", nlohmann::json{
        {"command", "node"},
        {"args", {"./mcp-server.js"}},
        {"tools", {"*"}}
    }}
};
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```cpp
CopilotClientOptions opts;
opts.sessionIdleTimeoutSeconds = 300;
CopilotClient client(opts);
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```cpp
CopilotClientOptions opts;
opts.sessionFs = SessionFsConfig{"/repo", "/state", "posix"};
CopilotClient client(opts);
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```cpp
auto meta = client.getSessionMetadata("session-123");
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```cpp
SessionConfig config;
config.skillDirectories = {"./skills"};
config.disabledSkills = {"test-skill"};
config.includeSubAgentStreamingEvents = true;
```

- `skillDirectories` - list of directories containing skill definitions to register with the session
- `disabledSkills` - list of skill names to explicitly disable
- `includeSubAgentStreamingEvents` - when true, surfaces streaming events from sub-agents in the event stream

## Thread Safety

- All public methods on `CopilotClient` and `CopilotSession` are thread-safe.
- Event handlers are called from the JSON-RPC reader thread (for notifications) or from
  detached threads (for request handlers). Handlers should be thread-safe.
- The `sendAndWait` method uses `std::condition_variable` internally and is safe to call
  from any thread.

## Protocol Version

The SDK verifies protocol compatibility on connection. The expected protocol version (4) is
defined in `copilot/sdk_protocol_version.h`. If the server reports a different version,
an error is thrown during `start()`.

## Platform Support

- Linux (GCC, Clang)
- macOS (Clang)
- Windows (MSVC) - Uses Win32 `CreateProcess` and pipe APIs

## Image Generation

Request image responses using `responseFormat` and `imageOptions`:

```cpp
copilot::MessageOptions opts;
opts.prompt = "Generate a sunset over mountains";
opts.responseFormat = copilot::ResponseFormat::Image;
opts.imageOptions = copilot::ImageOptions{"1024x1024", "hd", "natural"};
auto response = session->sendAndWait(opts);
```

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk` v2.4–v2.5. Unless noted, the items below are members of `SessionConfig` (via `std::optional`), so existing code is unaffected.

### v2.5.0

- **Reasoning effort** — `config.reasoningEffort` (`"low"`/`"medium"`/`"high"`/`"xhigh"`).
- **Session rewind** — `config.rewindEnabled`.
- **Additional directories** — `config.additionalDirectories`.
- **Content exclusion** — `config.contentExclusion`.
- **Tool search** — `config.toolSearch`.
- **Disabled MCP servers** — `config.disabledMcpServers`.
- **GitHub MCP tool config** — `config.githubMcpToolConfig`.
- **Canvas provider** — `config.canvasProvider`.
- **Custom agents local-only** — `config.customAgentsLocalOnly`.
- **Experimental mode** — `config.experimentalMode`.
- **Agent factory args schema** — `CustomAgentConfig::argsSchema`.
- **Permission decision context** — `decisionContext` on permission results.
- **Built-in plugin directories** — `CopilotClientOptions::builtinPluginDirectories`.
- **In-process FFI transport** — `CopilotClientOptions::inProcess`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions::bearerTokenProvider` (`BearerTokenProvider`).
- **MCP OAuth token handler** — `config.onMcpAuthRequest` (`McpAuthHandler`).
- **HTTP request handler** — `CopilotClientOptions::requestHandler` (`CopilotRequestHandler`).
- **Session citations** — `config.enableCitations`.
- **Excluded built-in agents** — `config.excludedBuiltinAgents`.
- **Session spending limits** — `config.sessionLimits` (`SessionLimitsConfig::maxAiCredits`).
- **Session memory** — `config.memory` (`MemoryConfiguration`).
- **OTLP telemetry protocol** — `config.otlpProtocol`.
- **WebSocket transport** — `config.enableWebSocketResponses`.
- **Experiment assignments** — `config.expAssignments`.
- **Per-message agent mode** — `MessageOptions::agentMode` and `displayPrompt`.
- **Post-tool-use / pre-MCP hooks** — `SessionHooks::onPostToolUse`, `onPreMcpToolCall`, `onUserPromptTransformed`.
- **Tool defer loading** — `ToolDefer::Auto` / `ToolDefer::Never`.
- **GitHub attachments** — `GitHubAttachment::GitHubCommit` / `GitHubRepository`.

Configure reasoning effort, rewind, extra directories, content exclusion, and tool search:

```cpp
copilot::SessionConfig config;
config.reasoningEffort = "high";
config.rewindEnabled = true;
config.additionalDirectories = std::vector<std::string>{"../shared", "/data/corpus"};
config.contentExclusion = true;
config.toolSearch = nlohmann::json{{"enabled", true}};
auto session = client.createSession(config);
```

Supply bring-your-own-key bearer tokens, minted per session:

```cpp
copilot::CopilotClientOptions options;
options.bearerTokenProvider = [](const copilot::ProviderTokenArgs& args) -> std::string {
    return mintTokenFor(args.sessionId);
};
copilot::CopilotClient client(options);
```

Cap spend and turn on citations and persistent memory:

```cpp
copilot::SessionConfig config;
config.sessionLimits = copilot::SessionLimitsConfig{5.0};   // maxAiCredits
config.enableCitations = true;
config.memory = copilot::MemoryConfiguration{true};         // enabled
```

Choose an agent mode and display prompt for a single turn:

```cpp
copilot::MessageOptions opts;
opts.prompt = "Refactor the auth module";
opts.agentMode = "plan";
opts.displayPrompt = "Refactor auth (planning)";
auto response = session->sendAndWait(opts);
```

## License

See the LICENSE file in the repository root.

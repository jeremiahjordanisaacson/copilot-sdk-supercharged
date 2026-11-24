# D SDK for GitHub Copilot CLI

A native D language SDK for the [GitHub Copilot CLI](https://github.com/github/copilot-sdk), providing idiomatic D bindings with UFCS, ranges, `Nullable!T`, `scope(exit)` resource management, and `@safe`/`@trusted` annotations.

## Quick Start

### Prerequisites

- **D compiler** (DMD, LDC, or GDC)
- **DUB** package manager (ships with DMD)
- **Copilot CLI 1.0.113 or newer** installed and on `PATH`, or specify `cliPath` / `COPILOT_CLI_PATH`

### Installation

Add to your `dub.json`:

```json
{
    "dependencies": {
        "copilot-sdk": { "path": "../dlang" }
    }
}
```

Or clone and use directly:

```bash
cd dlang
dub build
```

### Minimal Example

```d
import copilot;
import std.stdio : writeln;

void main()
{
    auto client = new CopilotClient();
    scope(exit) client.stop();

    auto session = client.createSession();

    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto content = ev.stringField("content");
            if (!content.isNull)
                writeln("Assistant: ", content.get);
        }
    });

    session.sendAndWait("Explain D's scope guards in two sentences.");
}
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, thrown as `JsonRpcError` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret value is never logged. The optional `configDir` argument defaults to the runtime's configuration directory.

```d
auto saved = client.mcpSecretsSave("my-server-token", environment["MY_SERVER_TOKEN"]);
writeln(saved.usedKeychain ? "Stored in the OS keychain" : "Stored in the file backend");

client.mcpSecretsDelete("my-server-token");
client.mcpSecretsDeleteServer("my-server"); // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions. `extensionApprovalsList` and `extensionApprovalsRevoke` take an optional `sessionId` to use an attached session's configuration profile.

```d
// Persist a pending extension permission request before answering it normally
auto grant = client.extensionApprovalsGrant(session.sessionId, requestId);
if (grant.granted && !grant.approval.isNull)
    writeln("Approved ", grant.approval.get.extensionName);

foreach (approval; client.extensionApprovalsList().approvals)
{
    // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
    client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey);
}
```

### Ask-user image attachments

Set `onUserInputRequest` on `SessionConfig` or `ResumeSessionConfig` (it sends `requestUserInput`) to answer the agent's ask_user questions with a `UserInputResponse`. When the runtime accepts images in the answer, the `UserInputRequest` carries `imageAttachmentConstraints` (`allowedMimeTypes`, `maxBytesPerImage`, `maxCount`); it is null otherwise. The request also exposes `requestId` and `meta` (the opaque `_meta` object) when sent. Answer with `attachments`, an array of `UserInputImageAttachment` (base64 `data`, `mimeType`, optional `description` and `displayName`; the wire `type` is always `"image"`). Image-only answers use an empty `answer` with `wasFreeform` true (the default); the runtime enforces the limits.

```d
import std.algorithm.searching : canFind;
import std.base64 : Base64;
import std.file : read;

config.onUserInputRequest = (UserInputRequest request) {
    UserInputResponse response;
    if (!request.imageAttachmentConstraints.isNull
        && request.imageAttachmentConstraints.get.allowedMimeTypes.canFind("image/png"))
    {
        UserInputImageAttachment image;
        image.data = Base64.encode(cast(ubyte[]) read("screenshot.png")).idup;
        image.mimeType = "image/png";
        image.displayName = "screenshot.png";
        response.attachments = [image];
    }
    else
    {
        response.answer = "No screenshot available";
    }
    return response;
};
```

## What's New in v3.0.0

**SDK protocol 4.** The SDK now speaks SDK protocol 4 (`SdkProtocolVersion == 4`), which requires Copilot CLI 1.0.113 or newer. `start()` checks the version reported by `connect` / `ping` and rejects older CLIs with an `SDK protocol version mismatch` error. Existing options keep working: `SessionConfig` and `ResumeSessionConfig` still produce the same flat options, and the client translates them into the grouped protocol 4 payload (`groupSessionParams`) right before `session.create` and `session.resume`. A legacy single `provider` is translated into a named `"default"` provider automatically. Session events now arrive through `session.event` notifications, and `send` / `abort` use the `session.send` / `session.abort` requests, so runtime refusals surface as `JsonRpcError`.

New client options and methods:

```d
CopilotClientOptions opts;
opts.mode = ClientMode.codingAgent; // default; ClientMode.empty requires availableTools
opts.clientInfo = ClientInfo("my-editor", "1.2.0", "my-extension", "0.3.0");
opts.network.scope_ = "connection";
opts.network.resolveProxy = (string url) => Nullable!string("PROXY proxy.example:8080; DIRECT");
opts.network.authenticateBasic = (NetworkBasicAuthRequest req) =>
    Nullable!NetworkBasicCredentials(NetworkBasicCredentials("user", environment["PROXY_PASSWORD"]));
auto client = new CopilotClient(opts);
client.clearManagedSettingsCache(); // managedSettings.clearCache
```

With `ClientMode.empty`, every session must set `availableTools`, and conservative defaults (no telemetry, in-memory OAuth and embedding storage, no repository hooks, no host git operations) are applied beneath your own values:

```d
CopilotClientOptions emptyOpts;
emptyOpts.mode = ClientMode.empty;
auto minimalClient = new CopilotClient(emptyOpts);
SessionConfig minimal;
minimal.availableTools = ["view", "grep"];
auto minimalSession = minimalClient.createSession(minimal);
```

New session options:

```d
SessionConfig config;
config.model = "gateway/chat";
config.autoTier = AutoTier.balance;
config.askUserVariant = AskUserVariant.elicitation;
config.featureFlags = ["my_flag": true];
config.allowedModels = ["gpt-5", "claude-sonnet-4.5"];
config.authClientIdMetadataUrl = "https://example.com/oauth/client.json";
config.agentDirectories = ["./agents"];
config.hookDirectories = ["./hooks"];
config.trustWorkingDirectory = TrustWorkingDirectory.session;

SandboxConfig sandbox;
sandbox.enabled = true;
sandbox.allowBypass = true;
config.sandboxConfig = sandbox;

ModelLimitsOverride limits;
limits.maxOutputTokens = 4096; // sent as limits.max_output_tokens
config.modelCapabilities = limits.toModelCapabilities();

ImageGenerationConfig images;
images.enabled = true;
images.saveToDisk = false;
config.imageGeneration = images;

AutopilotConfig autopilot;
autopilot.continuationEnabled = true;
autopilot.maxContinues = 3;
config.autopilot = autopilot;
```

Named BYOK providers (select a registered model with `"provider/id"`), with `apiKeyCommand` for fresh keys:

```d
NamedProviderConfig gateway;
gateway.name = "gateway";
gateway.type = "openai";
gateway.baseUrl = "https://models.example.com/v1";
gateway.apiKeyCommand = "get-model-key";

ProviderModelConfig chat;
chat.provider = "gateway";
chat.id = "chat";
chat.wireModel = "gpt-5";

SessionConfig byok;
byok.model = "gateway/chat";
byok.providers = [gateway];
byok.models = [chat];
byok.defaultProvider = "gateway";
byok.capiEnabled = false;

// The legacy single provider still works and gains apiKeyCommand:
ProviderConfig legacy;
legacy.type = "openai";
legacy.baseUrl = "https://models.example.com/v1";
legacy.apiKeyCommand = "get-model-key";
SessionConfig legacySession;
legacySession.model = "my-deployment";
legacySession.provider = legacy;
```

Callbacks: GitHub token provider (mutually exclusive with `gitHubToken`), subagent lifecycle hooks, shell authentication, and memory store:

```d
config.gitHubTokenProvider = (GitHubTokenProviderArgs args) =>
    GitHubTokenProviderResult.token(environment["GH_TOKEN"], 7200);
config.onSubagentStart = (SubagentStartHookInput input) {
    SubagentStartHookOutput output;
    output.additionalContext = "Focus on " ~ input.agentName;
    return output;
};
config.onSubagentStop = (SubagentStopHookInput input) => SubagentStopHookOutput.init;
config.onShellAuthenticationRequest = (ShellAuthenticationRequest req) =>
    req.program == "sudo"
        ? ShellAuthenticationResponse.accept(environment["SUDO_PASSWORD"])
        : ShellAuthenticationResponse.cancel();
config.onStoreMemory = (StoredMemory memory) {
    writeln("remember: ", memory.subject, " = ", memory.fact);
};
```

Idempotent tools and message options (source, client turn id, structured output):

```d
auto lookup = defineTool("lookup", "Look up a record",
    `{"type":"object","properties":{"id":{"type":"string"}}}`,
    (string callId, JSONValue args) @safe => successResult(callId, "found"));
lookup.tool.idempotent = true;
auto session = client.createSession(config, [lookup]);

MessageOptions msg;
msg.source = MessageSource.system; // or MessageSource.agent("reviewer")
msg.clientTurnId = "turn-42";
msg.responseSchema = parseJSON(`{"type":"object","properties":{"colors":{"type":"array"}}}`);
session.send("List three colors", msg);
```

Session methods, resume options, shell output events, and Policy Helper failures:

```d
session.setAutoTier(AutoTier.balance); // session.model.switchAutoTier
session.recheckPolicyHelper();         // session.managedSettings.recheckPolicyHelper
auto modeResult = session.setPermissionMode("yolo");
if (!modeResult.success && !modeResult.reason.isNull
    && modeResult.reason.get == permissionModeRefusalManagedPolicy)
    writeln("Blocked by managed policy");

session.addEventListener((SessionEvent ev) {
    if (ev.type == SessionEventType.toolShellOutput)
        write(ShellOutputData.fromEvent(ev).text);
});
session.detach(); // session.detach

ResumeSessionConfig resume;
resume.sessionId = "existing-session-id";
resume.allowTranscriptRecovery = true;
resume.disableResume = true; // sent as suppressResumeEvent
auto resumed = client.resumeSession(resume);

try
{
    client.createSession(config);
}
catch (JsonRpcError e)
{
    auto failure = e.policyHelperFailure();
    if (!failure.isNull)
        writeln("Policy Helper: ", failure.get.message);
}
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection. The SDK keeps spawning the Copilot CLI over stdio; `cliUrl` (TCP) is still not implemented.
- Server requests are answered on the reader thread one at a time, so `shellAuthentication.cancel` cannot interrupt a handler that is already running; the cancellation is acknowledged.
- The `connect` handshake is only sent when `clientInfo` is set; otherwise the SDK uses `ping`.

## Architecture

```
Your App  -->  CopilotClient  -->  JsonRpcClient  -->  Copilot CLI (stdio)
                    |
               CopilotSession
                    |
              EventListeners + ToolHandlers
```

The SDK spawns the Copilot CLI as a child process and communicates over JSON-RPC 2.0 with Content-Length framing (the Language Server Protocol transport).

## API Reference

### CopilotClient

| Method | Description |
|---|---|
| `this(CopilotClientOptions)` | Construct with options; auto-starts if `autoStart` is true |
| `start()` | Start the CLI process or connect to an external server |
| `stop()` | Graceful shutdown |
| `forceStop()` | Kill the CLI process immediately |
| `createSession(SessionConfig)` | Create a new conversation session |
| `createSession(SessionConfig, ToolBinding[])` | Create a session with pre-registered tools |
| `resumeSession(ResumeSessionConfig)` | Resume an existing session by ID |
| `ping()` | Ping the CLI server; returns `true` if reachable |
| `getStatus()` | Get CLI status and version |
| `listModels()` | List available LLM models |
| `getAuthStatus()` | Check authentication status |
| `clearManagedSettingsCache()` | Clear the runtime's managed-settings cache |
| `mcpSecretsSave(secretId, value, configDir)` | Save an MCP credential; returns `McpSecretsSaveResult` (`usedKeychain`) |
| `mcpSecretsDelete(secretId, configDir)` | Delete an MCP credential |
| `mcpSecretsDeleteServer(serverName, configDir)` | Delete the MCP credentials indexed for a server |
| `extensionApprovalsGrant(sessionId, requestId)` | Persist a pending plugin extension approval |
| `extensionApprovalsList(sessionId)` | List plugin extension approvals |
| `extensionApprovalsRevoke(extensionName, installationKey, sessionId)` | Revoke a plugin extension approval |

### CopilotClientOptions

| Field | Type | Default | Description |
|---|---|---|---|
| `cliPath` | `string` | `"github-copilot"` | Path to CLI binary |
| `cliArgs` | `string[]` | `[]` | Extra CLI arguments |
| `cwd` | `Nullable!string` | null | Working directory |
| `port` | `Nullable!ushort` | null | TCP port (non-stdio) |
| `useStdio` | `bool` | `true` | Use stdio transport |
| `cliUrl` | `Nullable!string` | null | External server URL |
| `logLevel` | `string` | `"info"` | CLI log level |
| `autoStart` | `bool` | `true` | Auto-start on construction |
| `autoRestart` | `bool` | `false` | Restart on unexpected exit |
| `env` | `string[string]` | empty | Extra environment variables |
| `gitHubToken` | `Nullable!string` | null | GitHub auth token |
| `useLoggedInUser` | `bool` | `true` | Use logged-in GitHub user |
| `sessionFs` | `Nullable!SessionFsConfig` | null | Session filesystem provider |
| `sessionIdleTimeoutSeconds` | `Nullable!uint` | null | Idle timeout |
| `mode` | `ClientMode` | `codingAgent` | `codingAgent` or `empty` |
| `clientInfo` | `ClientInfo` | empty | Application identity sent on `connect` |
| `network` | `NetworkOptions` | empty | Proxy and proxy-authentication callbacks |

When `cliPath` is left at its default, the client uses `COPILOT_CLI_PATH` if it is set. A path ending in `.js` (for example `npm-loader.js`) runs under `node`.

### CopilotSession

| Method | Description |
|---|---|
| `send(message, opts)` | Send a message (`session.send`); returns once the runtime accepts it |
| `sendAndWait(message, opts)` | Send and block until session is idle; returns collected events |
| `abort()` | Abort the current in-flight request |
| `disconnect()` / `detach()` | Release the session (`session.detach`); it can be resumed later |
| `setAutoTier(tier)` | Switch the Auto routing tier |
| `recheckPolicyHelper()` | Re-run the managed-settings Policy Helper |
| `setPermissionMode(mode)` | Set the permission mode; returns `PermissionModeResult` |
| `addEventListener(listener)` | Register an event listener |
| `registerTool(ToolBinding)` | Register a tool handler |
| `registerTools(ToolBinding[])` | Register multiple tool handlers |
| `sessionId` | Property: the session's unique ID |
| `isIdle` | Property: whether the session is idle |

### SessionEventType

| Value | Wire String |
|---|---|
| `assistantMessage` | `assistant.message` |
| `assistantMessageDelta` | `assistant.message_delta` |
| `assistantReasoning` | `assistant.reasoning` |
| `assistantReasoningDelta` | `assistant.reasoning_delta` |
| `toolCall` | `tool.call` |
| `sessionIdle` | `session.idle` |
| `sessionError` | `session.error` |
| `sessionEnd` | `session.end` |
| `compactionStart` | `session.compaction_start` |
| `compactionComplete` | `session.compaction_complete` |
| `toolShellOutput` | `tool.shell_output` |

### Tool Definition

Use `defineTool` for a concise tool registration:

```d
import copilot.tools;

auto weatherTool = defineTool(
    "get_weather",
    "Get current weather for a city",
    `{
        "type": "object",
        "properties": {
            "city": { "type": "string", "description": "City name" }
        },
        "required": ["city"]
    }`,
    (string callId, JSONValue args) @safe {
        string city = args["city"].str;
        return successResult(callId, "Sunny, 22C in " ~ city);
    },
);

auto session = client.createSession(SessionConfig.init, [weatherTool]);
```

## Module Structure

| Module | Description |
|---|---|
| `copilot.client` | `CopilotClient` -- lifecycle, process management |
| `copilot.session` | `CopilotSession` -- send/receive, event dispatch |
| `copilot.types` | Structs: options, configs, events, responses |
| `copilot.jsonrpc` | JSON-RPC 2.0 client with Content-Length framing |
| `copilot.tools` | `defineTool`, `ToolBinding`, result helpers |
| `copilot.session_params` | `groupSessionParams`: protocol 4 session payload translation |
| `copilot.sdk_protocol_version` | `SdkProtocolVersion` (4) |
| `copilot` | Package re-export of all public modules |

## Testing

```bash
cd dlang
dub test
```

The test suite uses D's built-in `unittest` blocks and covers:

- Type serialization round-trips
- Tool definition and handler invocation
- Event type parsing
- JSON extraction helpers
- Error type construction
- Default option validation

## D Idioms Used

- **`scope(exit)`** for deterministic cleanup
- **`Nullable!T`** for optional fields (no null pointer surprises)
- **UFCS** (Uniform Function Call Syntax) throughout
- **`@safe` / `@trusted`** annotations for memory safety
- **Contracts** via `enforce` for precondition checks
- **`std.json`** for JSON manipulation
- **`std.process`** for subprocess management
- **Delegates** for event listeners and tool handlers
- **Module system** with `package.d` for clean imports

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk` v2.4–v2.5. Unless noted, the items below are fields on `SessionConfig` and default to unset, so existing code is unaffected.

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
- **Agent factory args schema** — `AgentFactoryOptions.argsSchema`.
- **Permission decision context** — `decisionContext` on permission results.
- **Built-in plugin directories** — `CopilotClientOptions.builtinPluginDirectories`.
- **In-process FFI transport** — `CopilotClientOptions.inProcess`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions.bearerTokenProvider` (`BearerTokenProvider`).
- **MCP OAuth token handler** — `config.onMcpAuthRequest` (`McpAuthHandler`).
- **HTTP request handler** — `CopilotClientOptions.requestHandler` (`CopilotRequestHandler`).
- **Session citations** — `config.enableCitations`.
- **Excluded built-in agents** — `config.excludedBuiltinAgents`.
- **Session spending limits** — `config.sessionLimits` (`SessionLimitsConfig.maxAiCredits`).
- **Session memory** — `config.memory` (`MemoryConfiguration`).
- **OTLP telemetry protocol** — `config.otlpProtocol`.
- **WebSocket transport** — `config.enableWebSocketResponses`.
- **Experiment assignments** — `config.expAssignments`.
- **Per-message agent mode** — `MessageOptions.agentMode` and `displayPrompt`.
- **Tool defer loading** — `ToolDefer` (`auto_`/`never`).
- **Hook identifiers** — `HookType.postToolUse`, `preMcpToolCall`, `userPromptTransformed`.
- **GitHub attachments** — `GitHubAttachmentType.commit` / `repository`.

Configure reasoning effort, rewind, extra directories, content exclusion, and tool search:

```d
import std.json : parseJSON;

SessionConfig config;
config.reasoningEffort = "high";
config.rewindEnabled = true;
config.additionalDirectories = ["../shared", "/data/corpus"];
config.contentExclusion = true;
config.toolSearch = parseJSON(`{ "enabled": true }`);
auto session = client.createSession(config);
```

Supply bring-your-own-key bearer tokens, minted per session:

```d
CopilotClientOptions opts;
opts.bearerTokenProvider = (ProviderTokenArgs args) {
    return mintTokenFor(args.sessionId);
};
auto client = new CopilotClient(opts);
```

Cap spend and turn on citations and persistent memory:

```d
SessionConfig config;
SessionLimitsConfig limits;
limits.maxAiCredits = 5.0;
config.sessionLimits = limits;
config.enableCitations = true;
config.memory = MemoryConfiguration(true);
```

Choose an agent mode and display prompt for a single turn:

```d
MessageOptions opts;
opts.agentMode = "plan";
opts.displayPrompt = "Refactor auth (planning)";
session.send("Refactor the auth module", opts);
```

## Cookbook

See the [`cookbook/`](cookbook/) directory for focused recipes:

| Recipe | Description |
|---|---|
| [Error Handling](cookbook/error_handling.d) | Robust error handling with scope guards |
| [Multiple Sessions](cookbook/multiple_sessions.d) | Run parallel conversations |
| [Persisting Sessions](cookbook/persisting_sessions.d) | Save and resume sessions |
| [Tool Usage](cookbook/tool_usage.d) | Define and register custom tools |
| [Streaming](cookbook/streaming.d) | Handle streaming response deltas |
| [Advanced Config](cookbook/advanced_config.d) | Provider overrides, system messages, idle timeout |

## License

MIT

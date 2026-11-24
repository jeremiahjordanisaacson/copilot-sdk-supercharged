# MATLAB SDK for the GitHub Copilot CLI

> Part of [copilot-sdk-supercharged](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged) -- 40 languages, one SDK.

Embed Copilot's agentic workflows in your MATLAB application. The SDK
speaks JSON-RPC 2.0 over stdio to a local Copilot CLI process, giving
you programmatic access to the same agent runtime behind GitHub Copilot.

## Requirements

| Requirement | Version |
|-------------|---------|
| MATLAB | R2020b or later |
| Copilot CLI | Latest (`npm install -g @anthropic/copilot-cli` or via GitHub) |
| Java Runtime | Bundled with MATLAB (used for process I/O) |

## Installation

### Option 1 -- `addpath` (recommended for development)

```matlab
addpath('/path/to/copilot-sdk-supercharged/matlab');
```

Add the line to your `startup.m` to make it permanent.

### Option 2 -- MATLAB Add-Ons

Copy the `matlab/` folder into your MATLAB Add-Ons directory:

```
%USERPROFILE%/Documents/MATLAB/Add-Ons/copilot-sdk/
```

Then add it to the path via **Home > Set Path > Add with Subfolders**.

## Quick Start

```matlab
% 1. Create and start the client
client = copilot.CopilotClient();
client.start();

% 2. Open a session
config  = copilot.SessionConfig('Model', 'gpt-5');
session = client.createSession(config);

% 3. Send a message and wait for the response
response = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'What is 2+2?'));
disp(response.Content);

% 4. Clean up
session.disconnect();
client.stop();
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods
below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**;
older CLIs answer with a JSON-RPC method-not-found error, raised as
`copilot:rpcError` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned
secret store. The secret value is never logged. Each method takes an optional
trailing `configDir` (the runtime's default when omitted).

```matlab
result = client.mcpSecretsSave('my-server-token', getenv('MY_SERVER_TOKEN'));  % mcp.secrets.save
if result.UsedKeychain
    disp('Stored in the OS keychain');          % false: the file backend
end
client.mcpSecretsDelete('my-server-token');     % mcp.secrets.delete
client.mcpSecretsDeleteServer('my-server');     % every credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions.
`extensionApprovalsList` and `extensionApprovalsRevoke` take an optional
trailing session id to use an attached session's configuration profile.

```matlab
% Persist a pending extension permission request before answering it normally
grant = client.extensionApprovalsGrant(session.SessionId, requestId);
if grant.Granted
    fprintf('Approved %s\n', grant.Approval.ExtensionName);
end

list = client.extensionApprovalsList();
for i = 1:numel(list.Approvals)
    approval = list.Approvals{i};   % copilot.PermissionsExtensionApproval
    % ExtensionName, PluginName, Capabilities, GrantedAt, InstallationKey, InstallationLabel
    client.extensionApprovalsRevoke(approval.ExtensionName, approval.InstallationKey);
end
```

### Ask-user image attachments

Set `'OnUserInputRequest'` on `copilot.SessionConfig` to answer the agent's
ask_user questions (this sends `requestUserInput`). The handler receives a
`copilot.UserInputRequest`. When the runtime accepts images in the answer, its
`ImageAttachmentConstraints` is a `copilot.UserInputImageAttachmentConstraints`
(`AllowedMimeTypes`, `MaxBytesPerImage`, `MaxCount`); it is `[]` otherwise.
The request also carries `RequestId` and `Meta` (the decoded `_meta` object)
when the runtime sends them. Return a `copilot.UserInputResponse` whose
`'Attachments'` is a cell of `copilot.UserInputImageAttachment` (base64 `Data`,
`MimeType`, optional `Description` and `DisplayName`; the wire `type` is always
`"image"`). Attachments are sent only when set. Image-only answers use
`'Answer', ''` and `'WasFreeform', true`; the runtime enforces the limits.

```matlab
config = copilot.SessionConfig('OnUserInputRequest', @answerUser);
session = client.createSession(config);

function response = answerUser(request)
    constraints = request.ImageAttachmentConstraints;
    if ~isempty(constraints) && any(strcmp(constraints.AllowedMimeTypes, 'image/png'))
        fid = fopen('screenshot.png', 'r');
        bytes = fread(fid, Inf, '*uint8')';
        fclose(fid);
        image = copilot.UserInputImageAttachment( ...
            'Data', matlab.net.base64encode(bytes), ...   % base64
            'MimeType', 'image/png', 'DisplayName', 'screenshot.png');
        response = copilot.UserInputResponse('Answer', '', 'WasFreeform', true, ...
            'Attachments', {image});
    else
        response = copilot.UserInputResponse('Answer', 'No screenshot available', ...
            'WasFreeform', true);
    end
end
```

## What's New in v3.0.0

The MATLAB SDK now speaks **SDK protocol 4** (the Microsoft Copilot SDK 2.0
protocol). It requires **Copilot CLI 1.0.113 or newer**: `start()` rejects
older CLIs with `SDK protocol version mismatch: SDK supports version 4, but
server reports version N. Please update your SDK or server to ensure
compatibility.` The handshake now uses `connect` (falling back to `ping`).

Existing options keep working. `copilot.SessionConfig` still takes the same
name-value pairs; `copilot.groupSessionParams` converts the flat payload into
the protocol 4 grouped shape (`inference`, `tools`, `permissions`, `events`,
...) right before `session.create` and `session.resume` are sent. Server
callbacks (token providers, shell authentication, memory store, hooks, network)
are answered by `copilot.JsonRpcClient` while MATLAB waits or idles.

### Client options

```matlab
opts = copilot.CopilotClientOptions( ...
    'ClientInfo', struct('EditorName', 'MyEditor', 'EditorVersion', '1.2.0', ...
                         'ExtensionName', 'my-ext', 'ExtensionVersion', '0.1.0'), ...
    'Mode', 'empty', ...                       % or 'coding-agent' (default)
    'Network', struct( ...
        'Scope', 'connection', ...
        'ResolveProxy', @(url) 'http://proxy.local:8080', ...
        'AuthenticateBasic', @(proxy, realm, previousRejected) ...
            struct('username', 'me', 'password', getenv('PROXY_PASSWORD'))));
client = copilot.CopilotClient(opts);
client.start();
client.clearManagedSettingsCache();            % managedSettings.clearCache
```

`'Mode', 'empty'` requires `'AvailableTools'` on every create and resume and
turns off ambient features (telemetry, repository hooks, embeddings, memory).

### Session options

```matlab
config = copilot.SessionConfig( ...
    'Model',                   'claude-sonnet-4.5', ...
    'AvailableTools',          {'view', 'grep'}, ...
    'AskUserVariant',          'elicitation', ...
    'Sandbox',                 copilot.SandboxConfig('Enabled', true, 'AllowBypass', true), ...
    'FeatureFlags',            struct('my_flag', true), ...
    'AllowedModels',           {'claude-sonnet-4.5'}, ...
    'AuthClientIdMetadataUrl', 'https://example.com/client.json', ...
    'MaxOutputTokens',         4096, ...    % limits.max_output_tokens
    'AgentDirectories',        {'/repo/.agents'}, ...
    'HookDirectories',         {'/repo/.hooks'}, ...
    'WorkingDirectory',        '/repo', ...
    'TrustWorkingDirectory',   'session', ... % create only: 'session' or 'user'
    'ImageGeneration',         copilot.ImageGenerationConfig('Enabled', true, 'SaveToDisk', false), ...
    'Autopilot',               copilot.AutopilotConfig('ContinuationEnabled', true, 'MaxContinues', 5));
session = client.createSession(config);
```

### Named BYOK providers

```matlab
local = copilot.ProviderConfig('Name', 'local', 'Type', 'openai', ...
    'BaseUrl', 'http://localhost:11434/v1', ...
    'ApiKeyCommand', 'cat ~/.keys/local');     % apiKeyCommand
llama = copilot.ProviderModelConfig('Provider', 'local', 'Id', 'llama3', ...
    'MaxOutputTokens', 2048);
config = copilot.SessionConfig( ...
    'Providers', {local}, 'Models', {llama}, ...
    'DefaultProvider', 'local', 'Model', 'local/llama3', ...
    'CapiEnabled', false);                       % BYOK only
```

The legacy single provider still works: `'Provider', copilot.ProviderConfig(...)`
is sent as the named provider `"default"`.

### Idempotent tools, message options and session methods

```matlab
tool = copilot.DefineTool('Name', 'lookup', 'Description', 'Read-only lookup', ...
    'Idempotent', true, 'Handler', @(p) 'ok');

msg = copilot.MessageOptions( ...
    'Prompt',         'Summarize the diff', ...
    'Source',         'agent-reviewer', ...   % MessageSource: user, system or agent-<id>
    'ClientTurnId',   'turn-42', ...
    'ResponseSchema', struct('type', 'object', ...
        'properties', struct('summary', struct('type', 'string'))));
response = session.sendAndWait(msg);

session.switchAutoTier('balance');           % session.model.switchAutoTier
snapshot = session.recheckPolicyHelper();    % session.managedSettings.recheckPolicyHelper
result = session.setPermissionMode('allow-all');
if isfield(result, 'reason') && strcmp(result.reason, copilot.Types.PermissionModeReasonManagedPolicy)
    disp('Blocked by enterprise policy');
end
session.detach();                            % session.detach
```

### Resume options

```matlab
session = client.resumeSession(sessionId, copilot.SessionConfig( ...
    'DisableResume', true, ...               % suppressResumeEvent
    'AllowTranscriptRecovery', true));
```

### Callbacks

```matlab
config = copilot.SessionConfig( ...
    'GitHubTokenProvider', @(req) struct('accessToken', getenv('GH_TOKEN'), ...
                                         'tokenType', 'Bearer', 'expiresIn', 3600), ...
    'OnShellAuthenticationRequest', @(req) getenv('SUDO_PASSWORD'), ... % [] cancels
    'StoreMemory', @(memory) fprintf('Remember: %s\n', memory.fact), ...
    'OnSubagentStart', @(input) struct('additionalContext', 'Be concise.'), ...
    'OnSubagentStop', @(input) struct('modifiedResponse', input.response));
```

The token provider answers `gitHubToken.getToken` (return `[]` to cancel) and is
released when the session is detached.

### Events and errors

```matlab
addlistener(session, 'SessionEvent', @(~, evt) onEvent(evt));

function onEvent(evt)
    if isfield(evt.Payload, 'event') && ...
            strcmp(evt.Payload.event.type, copilot.Types.EventToolShellOutput)
        fprintf('%s', evt.Payload.event.data.text);   % tool.shell_output
    end
end
```

```matlab
try
    session = client.createSession(config);
catch me
    failure = client.LastPolicyHelperFailure;   % copilot.PolicyHelperFailure or []
    if ~isempty(failure)
        fprintf('Policy helper: %s\n', failure.Message);
    end
end
```

JSON-RPC errors (including `session.abort`, `session.delete` and
`session.setForeground` failures) are raised as `copilot:rpcError`.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*`) is not
  available: it is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: the MATLAB
  SDK spawns the Copilot CLI over stdio through Java.
- Server callbacks run on MATLAB's timer-driven reader, so they are answered
  only while MATLAB is idle or waiting (`pause`, `sendAndWait`, requests).
  Shell authentication prompts are answered synchronously, so
  `shellAuthentication.cancel` is acknowledged without further action.

## Architecture

```
Your MATLAB App
      |
      v
copilot.CopilotClient          (process lifecycle, session registry)
      |
      v
copilot.JsonRpcClient           (JSON-RPC 2.0 over stdio, Java I/O)
      |
      v
Copilot CLI  (--headless --stdio)  (agent runtime, tool execution)
```

The SDK uses MATLAB's bundled Java runtime to manage the child process
and stream JSON-RPC messages through `java.io.BufferedReader` /
`java.io.BufferedWriter`.

## API Reference

### copilot.CopilotClient

| Method | Description |
|--------|-------------|
| `CopilotClient(options)` | Create a client. Accepts a `CopilotClientOptions`. |
| `start()` | Spawn the CLI and negotiate the protocol. |
| `stop()` | Gracefully shut down the CLI. |
| `forceStop()` | Kill the CLI process immediately. |
| `createSession(config)` | Open a new session. Returns `CopilotSession`. |
| `resumeSession(id, config)` | Reconnect to a disconnected session. |
| `getSessionMetadata(id)` | Retrieve metadata for a session. |
| `setForegroundSessionId(id)` | Mark a session as foreground. |
| `getForegroundSessionId()` | Get the foreground session ID. |
| `listSessions(filter)` | List active sessions. |
| `deleteSession(id)` | Permanently delete a session. |
| `ping(message)` | Ping the server. |
| `getStatus()` | Return the CLI server status. |
| `getAuthStatus()` | Return the auth status. |
| `listModels()` | Return available models. |
| `getLastSessionId()` | Return the last-used session ID. |
| `clearManagedSettingsCache()` | Clear cached enterprise managed settings. |
| `mcpSecretsSave(secretId, value, configDir)` | Save an MCP credential (returns `McpSecretsSaveResult`). |
| `mcpSecretsDelete(secretId, configDir)` | Delete an MCP credential. |
| `mcpSecretsDeleteServer(serverName, configDir)` | Delete the MCP credentials indexed for a server. |
| `extensionApprovalsGrant(sessionId, requestId)` | Persist a pending plugin extension approval. |
| `extensionApprovalsList(sessionId)` | List plugin extension approvals. |
| `extensionApprovalsRevoke(extensionName, installationKey, sessionId)` | Revoke a plugin extension approval. |
| `LastPolicyHelperFailure` | Policy Helper failure of the last failed create/resume. |

### copilot.CopilotClientOptions

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `CliPath` | char | `''` | Path to the Copilot CLI binary. |
| `CliArgs` | cell | `{}` | Extra CLI arguments. |
| `Cwd` | char | `''` | Working directory for the CLI process. |
| `CliUrl` | char | `''` | URL of an external CLI server. |
| `LogLevel` | char | `'info'` | Log level (`debug`, `info`, `warn`, `error`). |
| `Env` | containers.Map | empty | Extra environment variables. |
| `GitHubToken` | char | `''` | GitHub personal-access token. |
| `UseLoggedInUser` | logical | `false` | Authenticate with the logged-in GitHub user. |
| `UseStdio` | logical | `true` | Use stdio transport. |
| `Port` | double | `0` | TCP port for a non-stdio connection. |

### copilot.SessionConfig

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `Model` | char | `''` | Model identifier (e.g. `'gpt-5'`). |
| `SystemMessage` | char | `''` | System prompt text. |
| `SystemMessageMode` | char | `'append'` | `'append'`, `'replace'`, or `'customize'`. |
| `Tools` | cell | `{}` | Cell array of `DefineTool` objects. |
| `IdleTimeout` | double | `0` | Session idle timeout in seconds. |
| `Streaming` | logical | `true` | Enable streaming deltas. |
| `Skills` | cell | `{}` | Skills to preload. |
| `ExcludedTools` | cell | `{}` | Built-in tools to exclude. |
| `Metadata` | containers.Map | empty | Arbitrary key-value metadata. |
| `RequestHeaders` | containers.Map | empty | Custom HTTP headers. |
| `ModelCapabilities` | struct | `struct()` | Model capability overrides. |
| `OnUserInputRequest` | function_handle | `[]` | ask_user handler: receives a `UserInputRequest`, returns a `UserInputResponse`. |

### copilot.CopilotSession

| Method | Description |
|--------|-------------|
| `send(msgOpts)` | Send a message without waiting. |
| `sendAndWait(msgOpts)` | Send a message and block until the turn completes. |
| `abort()` | Cancel the active turn. |
| `getMessages()` | Return all collected messages. |
| `disconnect()` | Detach from the session (ignores errors). |
| `detach()` | Detach from the session (`session.detach`). |
| `switchAutoTier(tier)` | Request an Auto routing tier. |
| `recheckPolicyHelper()` | Re-run the enterprise Policy Helper. |
| `setPermissionMode(mode)` | Change the permission mode. |

**Events:**

| Event | Description |
|-------|-------------|
| `SessionEvent` | Every session notification from the server. |
| `TurnComplete` | The agent finished processing the turn. |

### copilot.MessageOptions

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `Prompt` | char | `''` | User prompt text. |
| `RequestHeaders` | containers.Map | empty | Per-turn HTTP headers. |
| `Timeout` | double | `120` | Timeout in seconds for `sendAndWait`. |

### copilot.DefineTool

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `Name` | char | `''` | Tool name (snake_case). |
| `Description` | char | `''` | Human-readable description. |
| `Parameters` | struct | `struct(...)` | JSON Schema for tool parameters. |
| `Handler` | function_handle | `@(~) struct()` | Callback invoked by the agent. |
| `OverridesBuiltInTool` | logical | `false` | Override a built-in tool. |
| `SkipPermission` | logical | `false` | Skip the permission prompt. |

### copilot.SdkProtocolVersion

| Constant | Value |
|----------|-------|
| `VERSION` | `4` |
| `MIN_VERSION` | `4` |
| `MAX_VERSION` | `4` |

## Custom Tools

```matlab
tool = copilot.DefineTool( ...
    'Name',        'list_files', ...
    'Description', 'List files in a directory.', ...
    'Parameters',  struct( ...
        'type', 'object', ...
        'properties', struct( ...
            'directory', struct('type', 'string')), ...
        'required', {{'directory'}}), ...
    'Handler', @(p) dir(p.directory));

config = copilot.SessionConfig('Model', 'gpt-5', 'Tools', {tool});
session = client.createSession(config);
```

The agent can now call `list_files` during a conversation. The SDK
invokes your handler and returns the result automatically.

## Streaming Events

```matlab
addlistener(session, 'SessionEvent', @(~, evt) onSessionEvent(evt));

function onSessionEvent(evt)
    if isfield(evt.Payload, 'event')
        e = evt.Payload.event;
        if isfield(e, 'type') && strcmp(e.type, 'assistant.message_delta')
            fprintf('%s', e.content);
        end
    end
end
```

## System Prompt

```matlab
% Append to the default prompt
cfg = copilot.SessionConfig( ...
    'SystemMessage',     'Always include MATLAB examples.', ...
    'SystemMessageMode', 'append');

% Replace the default prompt
cfg = copilot.SessionConfig( ...
    'SystemMessage',     'You are a Simulink expert.', ...
    'SystemMessageMode', 'replace');
```

## Persisting Sessions

```matlab
% Save
sessionId = session.SessionId;
save('session.mat', 'sessionId');
session.disconnect();

% Resume later
load('session.mat', 'sessionId');
session = client.resumeSession(sessionId);
```

## Error Handling

```matlab
try
    client.start();
catch me
    switch me.identifier
        case 'copilot:cliNotFound'
            error('Install the Copilot CLI first.');
        case 'copilot:timeout'
            error('CLI did not respond in time.');
        otherwise
            rethrow(me);
    end
end
```

Use `onCleanup` for guaranteed teardown:

```matlab
client.start();
c = onCleanup(@() client.stop());
```

## Cookbook

| Recipe | Description |
|--------|-------------|
| [Error Handling](./cookbook/error-handling.md) | Connection failures, timeouts, cleanup |
| [Multiple Sessions](./cookbook/multiple-sessions.md) | Parallel conversations |
| [Persisting Sessions](./cookbook/persisting-sessions.md) | Save and resume |
| [Tools and Skills](./cookbook/tools-and-skills.md) | Custom tools, skills, sub-agents |
| [Advanced Features](./cookbook/advanced-features.md) | Auth, SessionFs, commands, images |
| [Streaming Events](./cookbook/streaming-events.md) | Real-time delta processing |

## Examples

- [`examples/basic_example.m`](./examples/basic_example.m) -- Minimal usage
- [`examples/tool_example.m`](./examples/tool_example.m) -- Custom tools
- [`examples/streaming_example.m`](./examples/streaming_example.m) -- Streaming events

## Running Tests

```matlab
cd matlab
results = runtests('tests/TestClient.m');
disp(results);
```

## Module Layout

```
matlab/
  +copilot/
    CopilotClient.m          Main client class
    CopilotClientOptions.m   Client configuration
    CopilotSession.m         Session handle
    SessionConfig.m          Session configuration
    JsonRpcClient.m          JSON-RPC 2.0 transport
    DefineTool.m             Tool definition helper
    MessageOptions.m         Message send options
    RpcEventData.m           Event data carrier
    SdkProtocolVersion.m     Protocol version constant
  examples/
    basic_example.m
    tool_example.m
    streaming_example.m
  cookbook/
    README.md
    error-handling.md
    multiple-sessions.md
    persisting-sessions.md
    tools-and-skills.md
    advanced-features.md
    streaming-events.md
  tests/
    TestClient.m
  info.xml                   Toolbox metadata
  README.md                  This file
```

## v2.0 Features

All v2.0 features from the upstream SDK are available:

- Per-session GitHub authentication
- Session idle timeout
- System prompt customization (append / replace / customize)
- Per-agent skills and excluded tools
- Custom request headers per turn
- Model capabilities override
- Session persistence (save / resume / delete)
- Streaming events (message deltas, reasoning deltas)
- Custom tool definitions with JSON Schema
- Image generation support
- Config discovery

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk`. These are name-value pairs on
`copilot.SessionConfig`, with per-message options on `copilot.MessageOptions`.

### v2.5

- **Reasoning effort** — `'ReasoningEffort'` (`'low'`, `'medium'`, `'high'`).
- **Session rewind** — `'RewindEnabled'` rolls the conversation back to an earlier turn.
- **Additional directories** — `'AdditionalDirectories'` (cell of char).
- **Content exclusion** — `'ContentExclusion'`.
- **Tool search** — `'ToolSearch'` discovers tools on demand.
- **Disabled MCP servers** — `'DisabledMcpServers'`.
- **GitHub MCP tool config** — `'GithubMcpToolConfig'`.
- **Canvas provider** — `'CanvasProvider'`.
- **Custom agents local-only** — `'CustomAgentsLocalOnly'`.
- **Experimental mode / in-process FFI** — `'ExperimentalMode'`, `'InProcess'`.
- **User-prompt-transformed hook** — `'OnUserPromptTransformed'` (function handle).
- **Permission decision context** — `'DecisionContext'`.
- **Agent-factory args schema** — `'ArgsSchema'`.
- **Built-in plugin directories** — `'BuiltinPluginDirectories'`.

### v2.4

- **BYOK bearer token provider** — `'BearerTokenProvider'` (function handle).
- **MCP OAuth token handler** — `'OnMcpAuthRequest'` (function handle).
- **HTTP request handler** — `'RequestHandler'`.
- **Session citations** — `'EnableCitations'`.
- **Excluded built-in agents** — `'ExcludedBuiltinAgents'`.
- **Spending limits** — `'SessionLimits'` with `copilot.SessionLimits('MaxAiCredits', ...)`.
- **Session memory** — `'Memory'` with `copilot.MemoryConfiguration('Enabled', ...)`.
- **OTLP protocol / WebSocket transport** — `'OtlpProtocol'`, `'EnableWebSocketResponses'`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `'OnPostToolUse'`, `'OnPreMcpToolCall'`.
- **Message agent mode / display prompt** — `copilot.MessageOptions('AgentMode', ...)`, `'DisplayPrompt'`.
- **Experiment assignments** — `'ExpAssignments'`.
- **Constants** — `copilot.Types.SystemMessagePreamble`, `copilot.Types.SystemMessagePreserve`, `copilot.Types.GitHubCommit`, `copilot.Types.GitHubRepository`, `copilot.Types.OtlpProtocolGrpc`.

```matlab
% Reasoning effort + session rewind + extra workspace roots
config = copilot.SessionConfig( ...
    'Model',                 'gpt-5', ...
    'ReasoningEffort',       'high', ...
    'RewindEnabled',         true, ...
    'AdditionalDirectories', {'/repo/pkg-a', '/repo/pkg-b'});
session = client.createSession(config);
```

```matlab
% Honor content exclusion and discover tools on demand
config = copilot.SessionConfig( ...
    'ContentExclusion', true, ...
    'ToolSearch',       struct('mode', 'auto'));
```

```matlab
% Cap AI-credit spend and emit citations
config = copilot.SessionConfig( ...
    'EnableCitations', true, ...
    'SessionLimits',   copilot.SessionLimits('MaxAiCredits', 5));
```

```matlab
% Bring-your-own-key: supply bearer tokens dynamically
config = copilot.SessionConfig( ...
    'BearerTokenProvider', @(args) myTokenFor(args));
```

```matlab
% Per-message agent mode and display prompt
msg = copilot.MessageOptions( ...
    'Prompt',        'Refactor this module', ...
    'AgentMode',     'plan', ...
    'DisplayPrompt', 'Refactor (planning)');
response = session.sendAndWait(msg);
```

## License

MIT. See the repository [LICENSE](../LICENSE) for details.

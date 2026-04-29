# MATLAB SDK for the GitHub Copilot CLI

> Part of [copilot-sdk-supercharged](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged) -- 22 languages, one SDK.

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
Copilot CLI  (--server --stdio)  (agent runtime, tool execution)
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

### copilot.CopilotSession

| Method | Description |
|--------|-------------|
| `send(msgOpts)` | Send a message without waiting. |
| `sendAndWait(msgOpts)` | Send a message and block until the turn completes. |
| `abort()` | Cancel the active turn. |
| `getMessages()` | Return all collected messages. |
| `disconnect()` | Detach from the session. |

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
| `VERSION` | `3` |

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

## License

MIT. See the repository [LICENSE](../LICENSE) for details.

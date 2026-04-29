# Copilot Supercharged SDK for Delphi/Object Pascal

Delphi SDK for the GitHub Copilot CLI. Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio pipes using Content-Length header framing.

## Requirements

- Delphi 11 Alexandria or later (RAD Studio 11+)
- Free Pascal 3.2+ with Lazarus is also supported for core types
- [GitHub Copilot CLI](https://github.com/github/copilot) installed and available on PATH (or specify path via `TCopilotClientOptions.CliPath`)

## Installation

### Boss Package Manager

```bash
boss install copilot-sdk-supercharged
```

### Manual Installation

1. Clone or download this repository.
2. Add the `delphi/src` directory to your project's search path.
3. Add the units to your `uses` clause:

```pascal
uses
  Copilot.Client, Copilot.Session, Copilot.Types, Copilot.DefineTool;
```

### Search Path Configuration

In your `.dproj` or IDE options, add:

```
..\delphi\src
```

## Quick Start

```pascal
program QuickStart;
{$APPTYPE CONSOLE}
uses
  System.SysUtils, System.JSON,
  Copilot.Client, Copilot.Session, Copilot.Types;

var
  Client: TCopilotClient;
  Session: TCopilotSession;
  Config: TSessionConfig;
  Opts: TMessageOptions;
  Response: TJSONValue;
begin
  Client := TCopilotClient.Create;
  try
    Client.Start;

    Config := Default(TSessionConfig);
    Config.Streaming := True;
    Config.OnPermissionRequest :=
      function(const Req: TPermissionRequest): TPermissionResult
      begin
        Result.Decision := pdApproved;
      end;

    Session := Client.CreateSession(Config);

    // Subscribe to streaming events
    Session.On_(
      procedure(const Event: TSessionEvent)
      begin
        if Event.EventType = 'assistant.message_delta' then
          Write(Event.Data.GetValue<string>('content', ''));
      end
    );

    Opts := Default(TMessageOptions);
    Opts.Prompt := 'What is 2 + 2?';
    Response := Session.SendAndWait(Opts);
    try
      WriteLn;
      WriteLn('Final: ', Response.ToJSON);
    finally
      Response.Free;
    end;

    Session.Disconnect;
  finally
    Client.Stop;
    Client.Free;
  end;
end.
```

## Architecture

The SDK mirrors the architecture of all other SDKs in this repository:

```
TCopilotClient
  |-- Spawns CLI process (copilot --headless --no-auto-update --log-level info --stdio)
  |-- TJsonRpcClient (Content-Length framed JSON-RPC 2.0 over pipes)
  |     |-- Reader task (TTask reading from CLI stdout)
  |     |-- Pending requests (TEvent-based futures)
  |     |-- Request handlers (for server->client calls)
  |-- Sessions (TCopilotSession)
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
- `session.create` - Create a new session
- `session.resume` - Resume an existing session
- `session.send` - Send a message to a session
- `session.getMessages` - Get session history
- `session.destroy` - Destroy a session
- `session.abort` - Abort current processing
- `session.delete` - Delete a session permanently
- `session.list` - List all sessions
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

### TCopilotClient

The main entry point. Manages the CLI server connection and session lifecycle.

```pascal
var
  Opts: TCopilotClientOptions;
  Client: TCopilotClient;
begin
  Opts := Default(TCopilotClientOptions);
  Opts.CliPath := 'C:\Program Files\copilot\copilot.exe';
  Opts.LogLevel := 'debug';

  Client := TCopilotClient.Create(Opts);
  try
    Client.Start;
    // ... use sessions ...
  finally
    Client.Stop;
    Client.Free;
  end;
end;
```

#### Methods

| Method | Description |
|--------|-------------|
| `Create` / `Create(Options)` | Constructor with optional configuration |
| `Start` | Spawn CLI process and connect |
| `Stop` | Terminate CLI process and clean up |
| `Ping` | Verify connectivity, returns `TPingResponse` |
| `CreateSession(Config)` | Create a new session |
| `ResumeSession(Config)` | Resume an existing session |
| `GetSessionMetadata(Id)` | Retrieve session metadata |
| `SetForegroundSessionId(Id)` | Set the foreground session (TUI) |
| `ListSessions` / `ListSessions(Filter)` | List sessions |
| `DeleteSession(Id)` | Permanently delete a session |
| `OnLifecycle(Handler)` | Subscribe to lifecycle events |

### TCopilotSession

Represents a conversation session.

```pascal
var
  Config: TSessionConfig;
  Session: TCopilotSession;
  Unsub: TUnsubscribeProc;
  MsgOpts: TMessageOptions;
begin
  Config := Default(TSessionConfig);
  Config.Model := 'gpt-4';
  Config.Streaming := True;

  Session := Client.CreateSession(Config);

  // Subscribe to a specific event type
  Unsub := Session.OnEvent('assistant.message',
    procedure(const Event: TSessionEvent)
    begin
      WriteLn(Event.Data.GetValue<string>('content', ''));
    end
  );

  // Send and wait
  MsgOpts := Default(TMessageOptions);
  MsgOpts.Prompt := 'Hello!';
  var Response := Session.SendAndWait(MsgOpts, 30000);
  Response.Free;

  // Unsubscribe
  Unsub();

  // Clean up
  Session.Disconnect;
end;
```

#### Methods

| Method | Description |
|--------|-------------|
| `Send(Options)` | Send a message (non-blocking) |
| `SendAndWait(Options, Timeout)` | Send and wait for response |
| `Abort` | Abort current processing |
| `GetMessages` | Get message history |
| `Disconnect` | Destroy the session |
| `On_(Handler)` | Subscribe to all events |
| `OnEvent(Type, Handler)` | Subscribe to specific event type |
| `Off(HandlerId)` | Remove a handler |

### Tools

Define custom tools that the assistant can invoke:

```pascal
uses
  Copilot.DefineTool;

var
  CalcTool: TTool;
begin
  CalcTool := DefineTool(
    'calculate',
    'Perform a calculation',
    TJSONObject.ParseJSONValue(
      '{"type":"object","properties":{"expression":{"type":"string"}},' +
      '"required":["expression"]}') as TJSONObject,
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      var Expr := Args.GetValue<string>('expression', '');
      // ... evaluate expression ...
      Result := ToolSuccess('Result: 42');
    end
  );
end;
```

Helper functions for tool results:
- `ToolSuccess(Text)` - Create a successful result
- `ToolFailure(ErrorMsg)` - Create a failed result

### Permissions

Handle permission requests from the assistant:

```pascal
Config.OnPermissionRequest :=
  function(const Req: TPermissionRequest): TPermissionResult
  begin
    if Req.Kind = 'read' then
      Result.Decision := pdApproved
    else
      Result.Decision := pdDeniedByUser;
  end;
```

### User Input

Handle user input requests (enables the ask_user tool):

```pascal
Config.OnUserInputRequest :=
  function(const Req: TUserInputRequest): TUserInputResponse
  begin
    WriteLn('Agent asks: ', Req.Question);
    ReadLn(Result.Answer);
    Result.Accepted := True;
  end;
```

### Hooks

Intercept session lifecycle events:

```pascal
var
  Hooks: TSessionHooks;
begin
  Hooks.OnPreToolUse :=
    function(const Input: TPreToolUseHookInput;
      const SessionId: string): TPreToolUseHookOutput
    begin
      WriteLn('About to use tool: ', Input.ToolName);
      Result.PermissionDecision := 'allow';
    end;

  Config.Hooks := Hooks;
end;
```

### Session Lifecycle Events

Monitor session lifecycle at the client level:

```pascal
var
  Unsub: TUnsubscribeProc;
begin
  Unsub := Client.OnLifecycle(
    procedure(const Event: TSessionLifecycleEvent)
    begin
      WriteLn('Lifecycle: ', Event.EventType, ' session=', Event.SessionId);
    end
  );

  // Later: Unsub() to unsubscribe
end;
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```pascal
var
  Opts: TCopilotClientOptions;
begin
  Opts := Default(TCopilotClientOptions);
  Opts.SessionIdleTimeoutSeconds := 300;

  Client := TCopilotClient.Create(Opts);
end;
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```pascal
var
  Opts: TCopilotClientOptions;
begin
  Opts := Default(TCopilotClientOptions);
  Opts.SessionFs.WorkspaceRoot := '/repo';
  Opts.SessionFs.StateRoot := '/state';
  Opts.SessionFs.PathStyle := 'posix';

  Client := TCopilotClient.Create(Opts);
end;
```

### Session Metadata

Retrieve metadata about a session:

```pascal
var
  Meta: TSessionMetadata;
begin
  Meta := Client.GetSessionMetadata('session-123');
  WriteLn('Model: ', Meta.Model);
  WriteLn('Status: ', Meta.Status);
  WriteLn('Messages: ', Meta.MessageCount);
end;
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```pascal
Config.SkillDirectories := ['./skills'];
Config.DisabledSkills := ['test-skill'];
Config.IncludeSubAgentStreamingEvents := True;
```

- `SkillDirectories` - list of directories containing skill definitions
- `DisabledSkills` - list of skill names to explicitly disable
- `IncludeSubAgentStreamingEvents` - surfaces streaming events from sub-agents

### System Message Customization

Customize the system prompt:

```pascal
Config.SystemMessage.Mode := 'customize';
// Sections can be configured via the Sections dictionary
```

### Instructions

Add custom instructions to the session:

```pascal
Config.Instructions := [
  'Always respond in French.',
  'Use formal language.'
];
```

### BYOK (Bring Your Own Key)

Use a custom model provider:

```pascal
Config.Provider.ProviderType := 'openai';
Config.Provider.BaseUrl := 'https://api.openai.com/v1';
Config.Provider.ApiKey := 'sk-...';
Config.Model := 'gpt-4';
```

### Streaming

When `Streaming` is True, you receive delta events:

```pascal
Session.On_(
  procedure(const Event: TSessionEvent)
  begin
    case IndexStr(Event.EventType, [
      'assistant.message_delta',
      'assistant.message',
      'assistant.reasoning_delta',
      'session.idle'
    ]) of
      0: Write(Event.Data.GetValue<string>('content', ''));
      1: WriteLn;  // Final message
      2: Write('[Reasoning] ', Event.Data.GetValue<string>('content', ''));
      3: WriteLn('[Idle]');
    end;
  end
);
```

### Infinite Sessions

Sessions persist workspace state to `~/.copilot/session-state/{sessionId}`. Compaction events are emitted:

- `session.compaction_start` - Compaction has begun
- `session.compaction_complete` - Compaction finished

### Image Generation

Request image responses:

```pascal
MsgOpts.ResponseFormat := rfImage;
MsgOpts.ImageOptions.Size := '1024x1024';
MsgOpts.ImageOptions.Quality := 'hd';
MsgOpts.ImageOptions.Style := 'natural';
MsgOpts.Prompt := 'Generate a sunset over mountains';
```

## Thread Safety

- All public methods on `TCopilotClient` and `TCopilotSession` use `TCriticalSection` for thread safety.
- Event handlers are called from the JSON-RPC reader task. Handlers should be thread-safe.
- `SendAndWait` uses `TEvent` internally and is safe to call from any thread.

## Protocol Version

The SDK verifies protocol compatibility on connection. The expected protocol version is defined in `Copilot.SdkProtocolVersion.pas`. If the server reports a version below the minimum, an exception is raised during `Start`.

## Unit Structure

| Unit | Description |
|------|-------------|
| `Copilot.Client` | Main client class, process management |
| `Copilot.Session` | Session class, event handling |
| `Copilot.JsonRpc` | JSON-RPC 2.0 transport layer |
| `Copilot.Types` | All type definitions, records, callbacks |
| `Copilot.DefineTool` | Tool definition helpers |
| `Copilot.SdkProtocolVersion` | Protocol version constant |

## Platform Support

- Windows (Delphi, primary target) - Uses Win32 `CreateProcess` and pipe APIs
- Cross-platform support via `CliUrl` option for connecting to an external server

## Running Tests

Tests use the DUnitX framework:

```bash
# From IDE: Run the test project
# From command line (if using test runner):
cd delphi/tests
# Build and run TestClient.pas with DUnitX runner
```

## License

See the LICENSE file in the repository root.

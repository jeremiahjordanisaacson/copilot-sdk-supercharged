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

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `TCopilotClient`
methods below are server-scoped requests that need **Copilot CLI 1.0.116 or
newer**; older CLIs answer method-not-found, raised as `EJsonRpcError`
(`Code = -32601`) like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned
secret store (`mcp.secrets.save`, `mcp.secrets.delete`,
`mcp.secrets.deleteServer`). Each method also takes an optional `ConfigDir`.
The secret value is never logged.

```pascal
var Saved := Client.McpSecretsSave('my-server-token',
  GetEnvironmentVariable('MY_SERVER_TOKEN'));
if Saved.UsedKeychain then
  WriteLn('Stored in the OS keychain')
else
  WriteLn('Stored in the file backend');

Client.McpSecretsDelete('my-server-token');
Client.McpSecretsDeleteServer('my-server'); // every credential of the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`permissions.extensionApprovals.grant`, `.list`, `.revoke`).
`ExtensionApprovalsList` and `ExtensionApprovalsRevoke` take an optional
`ASessionId` to use an attached session's configuration profile.

```pascal
// Persist a pending extension permission request before answering it.
var Grant := Client.ExtensionApprovalsGrant(Session.SessionId, PendingRequestId);
if Grant.Granted and Grant.HasApproval then
  WriteLn('Approved ', Grant.Approval.ExtensionName);

for var Approval in Client.ExtensionApprovalsList do
  // ExtensionName, PluginName, Capabilities, GrantedAt, InstallationKey,
  // InstallationLabel
  if Client.ExtensionApprovalsRevoke(Approval.ExtensionName,
    Approval.InstallationKey).Revoked then
    WriteLn('Revoked ', Approval.ExtensionName);
```

### Ask-user image attachments

`TSessionConfig.OnUserInputRequest` answers the runtime's `userInput.request`
(the ask_user tool); setting it now also sends `requestUserInput`, so the
runtime offers the tool. `TUserInputRequest` carries `Choices`,
`AllowFreeform`, `RequestId`, `MetaJson` (the opaque `_meta` object) and
`ImageAttachmentConstraints` (`Present`, `AllowedMimeTypes`,
`MaxBytesPerImage`, `MaxCount`); `Present` is False when the runtime does not
accept images. Answer with `Attachments`, an array of
`TUserInputImageAttachment` (base64 `Data`, `MimeType`, optional `Description`
and `DisplayName`; the wire `type` is always `'image'`). They are sent only
when the array is not empty. The reply also carries `wasFreeform`, True unless
`Answer` is one of the offered `Choices`, so an image-only answer simply uses
an empty `Answer`. The runtime enforces the limits.

```pascal
Config.OnUserInputRequest :=
  function(const Req: TUserInputRequest): TUserInputResponse
  var
    Image: TUserInputImageAttachment;
  begin
    Result := Default(TUserInputResponse);
    if Req.ImageAttachmentConstraints.Present and
      MatchStr('image/png', Req.ImageAttachmentConstraints.AllowedMimeTypes) then
    begin
      Image := Default(TUserInputImageAttachment);
      Image.Data := ScreenshotBase64; // base64 image bytes, no line breaks
      Image.MimeType := 'image/png';
      Image.DisplayName := 'screenshot.png';
      Result.Attachments := [Image]; // image-only answer: Answer stays ''
    end
    else
      Result.Answer := 'No screenshot available';
  end;
```

## What's New in v3.0.0

The Delphi SDK now speaks **SDK protocol 4** (the Microsoft Copilot SDK 2.0
protocol). It requires **Copilot CLI 1.0.113 or newer**: `Start` rejects older
CLIs with `SDK protocol version mismatch: SDK supports version 4, but server
reports version N. Please update your SDK or server to ensure compatibility.`

Existing options keep working. `CreateSession` and `ResumeSession` take the same
`TSessionConfig` fields; `GroupSessionParams` (unit `Copilot.SessionParams`)
converts the flat payload into the protocol 4 grouped shape (`inference`,
`tools`, `permissions`, `events`, ...) right before `session.create` and
`session.resume` are sent. JSON-RPC errors (including `session.abort`,
`session.delete` and `session.setForeground` failures) raise `EJsonRpcError`.

### Client options

```pascal
var
  Opts: TCopilotClientOptions;
begin
  Opts := Default(TCopilotClientOptions);
  // Client info handshake: sent as clientInfo on connect.
  Opts.ClientInfo.EditorName := 'MyEditor';
  Opts.ClientInfo.EditorVersion := '1.2.0';
  Opts.ClientInfo.ExtensionName := 'my-extension';
  Opts.ClientInfo.ExtensionVersion := '0.1.0';
  // Client mode: 'coding-agent' (default) or 'empty'. Empty mode requires
  // AvailableTools on every session and turns off ambient features.
  Opts.Mode := ClientModeEmpty;
  // Network callbacks (network.setProvider).
  Opts.Network.Scope := 'connection';
  Opts.Network.ResolveProxy :=
    function(const Url: string): string
    begin
      Result := 'http://proxy.local:8080';
    end;
  Client := TCopilotClient.Create(Opts);
  Client.Start;
  Client.ClearManagedSettingsCache; // managedSettings.clearCache
end;
```

### Session options

```pascal
Config := Default(TSessionConfig);
Config.AvailableTools := ['view', 'grep'];
Config.AskUserVariant := AskUserVariantElicitation;
Config.Sandbox.Enabled := True;
Config.Sandbox.AllowBypass := True;
Config.FeatureFlags := TDictionary<string, Boolean>.Create;
Config.FeatureFlags.Add('my-flag', True);
Config.AllowedModels := ['claude-sonnet-4.5'];
Config.AuthClientIdMetadataUrl := 'https://example.com/client.json';
Config.MaxOutputTokens := 4096; // limits.max_output_tokens
Config.AgentDirectories := ['C:\repo\.agents'];
Config.HookDirectories := ['C:\repo\.hooks'];
Config.WorkingDirectory := 'C:\repo';
Config.TrustWorkingDirectory := WorkingDirectoryTrustSession; // create only
Config.ImageGeneration.Enabled := obTrue;
Config.ImageGeneration.SaveToDisk := obFalse;
Config.Autopilot.ContinuationEnabled := obTrue;
Config.Autopilot.MaxContinues := 5;
Session := Client.CreateSession(Config);
```

### Named BYOK providers

```pascal
var
  Local: TNamedProviderConfig;
  Llama: TProviderModelConfig;
begin
  Local := Default(TNamedProviderConfig);
  Local.Name := 'local';
  Local.ProviderType := 'openai';
  Local.BaseUrl := 'http://localhost:11434/v1';
  Local.ApiKeyCommand := 'type C:\keys\local.txt'; // apiKeyCommand
  Llama := Default(TProviderModelConfig);
  Llama.Provider := 'local';
  Llama.Id := 'llama3';
  Llama.MaxOutputTokens := 2048;
  Config.Providers := [Local];
  Config.Models := [Llama];
  Config.DefaultProvider := 'local';
  Config.Model := 'local/llama3';
  Config.CapiEnabled := obFalse; // BYOK only
end;
```

The legacy single provider still works: `Config.Provider.BaseUrl` (plus
`ApiKey` or `ApiKeyCommand`) is sent as the named provider `"default"`.

### Idempotent tools and message options

```pascal
Tool := DefineTool('lookup', 'Read-only lookup', Params, Handler);
Tool.Idempotent := True; // safe to retry
Config.Tools := [Tool];

Opts := Default(TMessageOptions);
Opts.Prompt := 'Summarize the diff';
Opts.Source := 'agent-reviewer';  // MessageSource: user, system or agent-<id>
Opts.ClientTurnId := 'turn-42';
Opts.ResponseSchema := TJSONObject.ParseJSONValue(
  '{"type":"object","properties":{"summary":{"type":"string"}}}') as TJSONObject;
Session.Send(Opts); // responseFormat json_schema
```

### Session methods

```pascal
Session.SwitchAutoTier(AutoTierBalance); // session.model.switchAutoTier
Snapshot := Session.RecheckPolicyHelper; // caller frees
try
  ModeResult := Session.SetPermissionMode('allow-all');
  if ModeResult.Reason = PermissionModeReasonManagedPolicy then
    WriteLn('Blocked by enterprise policy');
finally
  Snapshot.Free;
end;
Session.Detach; // session.detach
```

### Resume options

```pascal
Resume := Default(TResumeSessionConfig);
Resume.SessionId := SavedId;
Resume.Streaming := True;
Resume.DisableResume := True;            // suppressResumeEvent
Resume.AllowTranscriptRecovery := True;
Resume.Options.AvailableTools := ['view'];
Session := Client.ResumeSession(Resume);
```

### Callbacks

```pascal
Config.GitHubTokenProvider :=
  function(const Request: TGitHubTokenRequest): TGitHubTokenResult
  begin
    Result := Default(TGitHubTokenResult);
    Result.AccessToken := GetEnvironmentVariable('GH_TOKEN');
    Result.TokenType := 'Bearer';
    Result.ExpiresIn := 3600;
  end;
Config.OnShellAuthenticationRequest :=
  function(const Request: TShellAuthenticationRequest): TShellAuthenticationResponse
  begin
    Result.Accepted := Request.InputKind = 'secret';
    Result.Value := GetEnvironmentVariable('SUDO_PASSWORD');
  end;
Config.StoreMemory :=
  procedure(const Memory: TStoredMemory)
  begin
    WriteLn('Remember: ', Memory.Fact);
  end;
Config.Hooks.OnSubagentStart :=
  function(const Input: TSubagentHookInput; const SessionId: string): string
  begin
    Result := 'Be concise.';
  end;
Config.Hooks.OnSubagentStop :=
  function(const Input: TSubagentHookInput; const SessionId: string): TSubagentStopDecision
  begin
    Result := Default(TSubagentStopDecision);
    Result.ModifiedResponse := Input.Response;
  end;
```

`GitHubTokenProvider` answers `gitHubToken.getToken`, is mutually exclusive with
`GithubToken`, and is released when the session is detached.

### Events and errors

```pascal
Session.OnEvent(EventToolShellOutput,
  procedure(const Event: TSessionEvent)
  begin
    Write(TCopilotSession.ParseShellOutput(Event.Data).Text);
  end);

try
  Session := Client.CreateSession(Config);
except
  on E: EJsonRpcError do
    if Client.LastPolicyHelperFailure.Present then
      WriteLn('Policy helper: ', Client.LastPolicyHelperFailure.Message);
end;
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*`) is not
  available: it is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: the Delphi
  SDK spawns the Copilot CLI over stdio (Windows) or needs an external server.
- `TSessionConfig.Instructions` (free-form strings) has no protocol 4
  equivalent and is not sent; use `InstructionDirectories`.
- Shell authentication prompts are answered synchronously, so
  `shellAuthentication.cancel` is acknowledged without further action.
- The SDK targets Delphi 10.3 or newer (`System.JSON`, inline variables,
  anonymous methods); Free Pascal cannot build the `src` units.

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
- `connect` - Handshake (clientInfo, protocol version); falls back to `ping`
- `ping` - Verify connectivity and protocol version
- `session.create` - Create a new session (protocol 4 grouped payload)
- `session.resume` - Resume an existing session (protocol 4 grouped payload)
- `session.send` - Send a message to a session
- `session.getMessages` - Get session history
- `session.detach` - Detach from a session (`Disconnect` / `Detach`)
- `session.abort` - Abort current processing
- `session.delete` - Delete a session permanently
- `session.list` - List all sessions
- `session.setForeground` - Set foreground session (TUI mode)
- `session.model.switchAutoTier` - Request an Auto routing tier
- `session.managedSettings.recheckPolicyHelper` - Re-run the Policy Helper
- `session.permissions.setMode` - Change the permission mode
- `managedSettings.clearCache` - Clear cached managed settings
- `mcp.secrets.save` / `mcp.secrets.delete` / `mcp.secrets.deleteServer` - MCP secret store
- `permissions.extensionApprovals.grant` / `.list` / `.revoke` - Plugin extension approvals
- `network.setProvider` - Register network callbacks

**Server -> Client (notifications):**
- `session.event` - Session events (assistant.message, session.idle, etc.)
- `session.lifecycle` - Session lifecycle events (created, deleted, etc.)

**Server -> Client (requests):**
- `tool.call` - Execute a registered tool
- `permission.request` - Request permission for an operation
- `userInput.request` - Request input from the user
- `hooks.invoke` - Invoke a hook handler (including `subagentStart` / `subagentStop`)
- `gitHubToken.getToken` - Mint a GitHub token (`GitHubTokenProvider`)
- `shellAuthentication.request` / `shellAuthentication.cancel` - Shell authentication prompts
- `memoryStore.store` - Store a memory (`StoreMemory`)
- `network.resolveProxy` / `network.authenticateNegotiate` / `network.authenticateBasic` - Network callbacks

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
| `ClearManagedSettingsCache` | Clear cached enterprise managed settings |
| `McpSecretsSave(SecretId, Value, ConfigDir)` | Save an MCP credential, returns `TMcpSecretsSaveResult` |
| `McpSecretsDelete(SecretId, ConfigDir)` | Delete an MCP credential |
| `McpSecretsDeleteServer(ServerName, ConfigDir)` | Delete the MCP credentials indexed for a server |
| `ExtensionApprovalsGrant(SessionId, RequestId)` | Persist a pending plugin extension approval |
| `ExtensionApprovalsList(SessionId)` | List plugin extension approvals |
| `ExtensionApprovalsRevoke(ExtensionName, InstallationKey, SessionId)` | Revoke a plugin extension approval |
| `LastPolicyHelperFailure` | Policy Helper failure of the last failed create/resume |

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
| `Send(Options)` | Send a message (returns once admitted) |
| `SendAndWait(Options, Timeout)` | Send and wait for response |
| `Abort` | Abort current processing (errors are raised) |
| `GetMessages` | Get message history |
| `Disconnect` | Detach from the session |
| `Detach` | Detach from the session (`session.detach`) |
| `SwitchAutoTier(Tier)` | Request an Auto routing tier |
| `RecheckPolicyHelper` | Re-run the enterprise Policy Helper |
| `SetPermissionMode(Mode)` | Change the permission mode |
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

## Recent Features (v2.4–v2.5)

Fields added in the v2.4 and v2.5 upstream syncs. Unless noted, they are fields of
`TSessionConfig` (passed to `CreateSession`); a few live on `TCopilotClientOptions`
or `TMessageOptions`. JSON-valued fields (e.g. `ToolSearch`) take a JSON string.

### v2.5 session options

- **Reasoning effort** — `Config.ReasoningEffort` (`'low'`/`'medium'`/`'high'`).
- **Tool search** — `Config.ToolSearch` (JSON string).
- **Session rewind** — `Config.RewindEnabled`.
- **Content exclusion** — `Config.ContentExclusion`.
- **Additional directories** — `Config.AdditionalDirectories` (`TArray<string>`).
- **Disabled MCP servers** — `Config.DisabledMcpServers` (`TArray<string>`).
- **GitHub MCP tool config** — `Config.GithubMcpToolConfig` (JSON string).
- **Canvas provider** — `Config.CanvasProvider` (JSON string).
- **Custom agents local-only** — `Config.CustomAgentsLocalOnly`.
- **Permission decision context** — `Config.DecisionContext` (JSON string).
- **User-prompt-transformed hook** — `Config.Hooks.OnUserPromptTransformed`.
- **Built-in plugin directories** — `Config.BuiltinPluginDirectories` (`TArray<string>`).
- **Agent-factory args schema** — `Config.ArgsSchema` (JSON string).
- **In-process FFI transport** — `Config.InProcess`.
- **Experimental mode** — `Config.ExperimentalMode`.

### v2.4 session options

- **BYOK bearer token provider** — `Options.BearerTokenProvider` (`TBearerTokenProvider`).
- **HTTP request handler** — `Options.RequestHandler` (`TCopilotRequestHandler`).
- **MCP OAuth token handler** — `Config.OnMcpAuthRequest` (`TMcpAuthHandler`).
- **Session citations** — `Config.EnableCitations`.
- **Excluded built-in agents** — `Config.ExcludedBuiltinAgents` (`TArray<string>`).
- **Session spending limits** — `Config.SessionLimits.MaxAiCredits`.
- **Session memory** — `Config.Memory.Enabled`.
- **OTLP protocol** — `Config.OtlpProtocol`.
- **WebSocket transport** — `Config.EnableWebSocketResponses`.
- **Experiment assignments** — `Config.ExpAssignments`.
- **Post/pre-tool hooks** — `Config.Hooks.OnPostToolUse`, `Config.Hooks.OnPreMcpToolCall`.
- **Per-message overrides** — `MsgOpts.AgentMode`, `MsgOpts.DisplayPrompt`.
- **Constants** — `ToolDeferAuto`/`ToolDeferNever`, `SectionPreamble`/`SectionPreserve`, `GitHubCommit`/`GitHubRepository`.

### Reasoning effort and tool search

```pascal
Config := Default(TSessionConfig);
Config.ReasoningEffort := 'high';
Config.ToolSearch := '{"enabled":true}';
Session := Client.CreateSession(Config);
```

### Session rewind

```pascal
Config := Default(TSessionConfig);
Config.RewindEnabled := True;
Session := Client.CreateSession(Config);
```

### Content exclusion and additional directories

```pascal
Config := Default(TSessionConfig);
Config.ContentExclusion := True;
Config.AdditionalDirectories := ['C:\shared', 'C:\reference'];
Session := Client.CreateSession(Config);
```

### BYOK bearer token provider

```pascal
Opts := Default(TCopilotClientOptions);
Opts.BearerTokenProvider :=
  function(const Args: TProviderTokenArgs): string
  begin
    // Return a fresh bearer token for Args.SessionId
    Result := 'Bearer-token-value';
  end;
Client := TCopilotClient.Create(Opts);
```

### Session spending limits

```pascal
Config := Default(TSessionConfig);
Config.SessionLimits.MaxAiCredits := 500;
Session := Client.CreateSession(Config);
```

## Thread Safety

- All public methods on `TCopilotClient` and `TCopilotSession` use `TCriticalSection` for thread safety.
- Event handlers are called from the JSON-RPC reader task. Handlers should be thread-safe.
- `SendAndWait` uses `TEvent` internally and is safe to call from any thread.

## Protocol Version

The SDK speaks SDK protocol 4 (`Copilot.SdkProtocolVersion.pas`). `Start` sends `connect` (falling back to `ping` on runtimes without it) and accepts exactly protocol version 4; any other version, or a server that reports none, raises `SDK protocol version mismatch: ...` during `Start`.

## Unit Structure

| Unit | Description |
|------|-------------|
| `Copilot.Client` | Main client class, process management |
| `Copilot.Session` | Session class, event handling |
| `Copilot.JsonRpc` | JSON-RPC 2.0 transport layer (`EJsonRpcError`) |
| `Copilot.Types` | All type definitions, records, callbacks |
| `Copilot.DefineTool` | Tool definition helpers |
| `Copilot.SessionParams` | SDK protocol 4 grouped session payloads (`GroupSessionParams`) |
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

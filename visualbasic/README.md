# Copilot SDK for Visual Basic .NET

Visual Basic .NET SDK for programmatic control of the GitHub Copilot CLI.

> **Note:** This SDK is in public preview and may change in breaking ways.

## Installation

```bash
dotnet add package CopilotSDK.Supercharged.VB
```

Or add a project reference when building from source:

```xml
<ProjectReference Include="..\visualbasic\CopilotSDK.Supercharged.VB.vbproj" />
```

## Quick Start

```vb
Imports GitHub.Copilot.SDK

' Create and start client
Using client As New CopilotClient()
    Await client.StartAsync()

    ' Create a session (OnPermissionRequest is required)
    Dim session = Await client.CreateSessionAsync(New SessionConfig With {
        .Model = "gpt-4",
        .OnPermissionRequest = PermissionHandlers.ApproveAll()
    })

    ' Listen for events
    Dim done As New TaskCompletionSource(Of Boolean)()

    session.On(Sub(evt)
        If TypeOf evt Is AssistantMessageEvent Then
            Console.WriteLine(DirectCast(evt, AssistantMessageEvent).Data?.Content)
        ElseIf TypeOf evt Is SessionIdleEvent Then
            done.TrySetResult(True)
        End If
    End Sub)

    ' Send a message and wait for completion
    Await session.SendAsync(New MessageOptions With { .Prompt = "What is 2+2?" })
    Await done.Task
End Using
```

## What's New in v3.0.0

This release speaks **SDK protocol 4** (the protocol of the Microsoft Copilot SDK 2.0 runtime) and
requires **Copilot CLI 1.0.113 or newer**. `StartAsync` sends `connect` (falling back to `ping` on
older servers) and rejects any server that does not report protocol version 4 with an
`SDK protocol version mismatch: SDK supports version 4, but server reports version N` error.
`SdkProtocolVersion.Version` is now the integer `4`.

Existing options keep working. `SessionConfig`, `ResumeSessionConfig`, and `MessageOptions` keep
their flat properties; right before sending `session.create` and `session.resume`, the client
translates the flat payload into the grouped v4 shape (`inference`, `tools`, `permissions`,
`interaction`, `events`, `agents`, `mcp`, `github`, `state`, ...) with
`SessionWire.GroupSessionParams`. The legacy single `Provider` is sent as a named provider called
`default` (with `inference.defaultProvider = "default"` and `github.capi.enabled = false`, as v1 BYOK
behaved) unless you also pass the new `Providers` / `DefaultProvider` / `CapiEnabled` options.
`Skills` is sent as `skills.includedBuiltinSkills`.

The client now answers the runtime's server-to-client requests (`gitHubToken.getToken`,
`hooks.invoke`, `exitPlanMode.request`, `shellAuthentication.request` / `.cancel`,
`memoryStore.store`, and the `network.*` callbacks), so the options below are live, not just typed.

| Feature | Visual Basic API |
|---|---|
| Client info handshake | `CopilotClientOptions.ClientInfo` (`CopilotClientInfo`), sent with `connect` |
| Client mode | `CopilotClientOptions.Mode = ClientMode.Empty` (default `ClientMode.CodingAgent`; `"copilot-cli"` is accepted as an alias). Empty mode requires `AvailableTools` on every create/resume and applies locked-down defaults under your values |
| Network callbacks | `CopilotClientOptions.Network` (`NetworkOptions`: `ResolveProxy`, `AuthenticateNegotiate`, `AuthenticateBasic`, `Scope`), registered with `network.setProvider` |
| Auto routing tier | `SessionConfig.AutoTier` (sent as `github.capi.autoTier`); `session.SetAutoTierAsync(AutoTier.Fast)` sends `session.model.switchAutoTier` |
| Ask-user variant | `AskUserVariant = AskUserVariant.Elicitation` |
| GitHub token provider | `GitHubTokenProvider` answers `gitHubToken.getToken` with `GitHubTokenProviderResult.Token(...)` or `.Cancelled()`; mutually exclusive with `SessionConfig.GitHubToken` |
| Sandbox bypass | `SandboxConfig = New SandboxConfig With {.Enabled = True, .AllowBypass = True}` |
| Feature flags | `FeatureFlags` (`experiments.flags`) |
| Session detach | `session.DisconnectAsync()` sends `session.detach` (falls back to `session.destroy`) |
| Message source | `MessageOptions.Source = MessageSource.User` / `MessageSource.System` / `MessageSource.Agent("id")` |
| Managed settings | `client.ClearManagedSettingsCacheAsync()`, `session.GetManagedSettingsAsync()`, `session.RecheckPolicyHelperAsync()` |
| Model allowlist | `AllowedModels` (`policy.allowedModels`) |
| OAuth client metadata URL | `AuthClientIdMetadataUrl` (`mcp.authClientIdMetadataUrl`) |
| Max output tokens | `ModelCapabilities` with `ModelCapabilitiesLimits.MaxOutputTokens` (`limits.max_output_tokens`) |
| Structured output | `MessageOptions.ResponseSchema` (sent as a `json_schema` `responseFormat`) |
| Agent / hook directories | `AgentDirectories`, `HookDirectories` |
| Working directory trust | `TrustWorkingDirectory = WorkingDirectoryTrust.Session` (create only) |
| Transcript recovery | `ResumeSessionConfig.AllowTranscriptRecovery` |
| Image generation | `ImageGeneration = New ImageGenerationConfig With {.Enabled = True, .SaveToDisk = True}` |
| Autopilot | `Autopilot = New AutopilotOptions With {.ContinuationEnabled = True, .MaxContinues = 5}` |
| Sub-agent hooks | `OnSubagentStart` / `OnSubagentStop` (dispatched from `hooks.invoke`) |
| BYOK API key command | `ProviderConfig.ApiKeyCommand`, `NamedProviderConfig.ApiKeyCommand` |
| Named BYOK providers | `Providers` (`NamedProviderConfig`), `Models` (`ProviderModelConfig`), `DefaultProvider`, `CapiEnabled`; `ResumeSessionConfig.ClearDefaultProvider` sends `defaultProvider: null` |
| Idempotent tools | `DefineTool.Idempotent = True` (sent in `tools.definitions`) |
| Client turn ID | `MessageOptions.ClientTurnId` |
| Shell output events | `ToolShellOutputEvent` (`tool.shell_output`): `ToolCallId`, `Text`, `Sequence`, `Stream` |
| Shell authentication | `OnShellAuthenticationRequest` returns `ShellAuthenticationResponse.Accept(value)` or `.Cancel()`; the token fires on `shellAuthentication.cancel` |
| Memory store | `OnStoreMemory` receives `StoredMemory` (sends `state.hasMemoryStore`) |
| Policy helper failure | `JsonRpcException.PolicyHelperFailure` reads `error.data.policyHelperFailure` |
| Permission mode refusal reason | `session.SetPermissionModeAsync(PermissionMode.AllowAll)` returns `PermissionModeSetResult` whose `Reason` can be `PermissionModeRefusalReason.ManagedPolicy` (`"managed_policy"`) |
| Resume event suppression | `ResumeSessionConfig.SuppressResumeEvent = True` (wire `suppressResumeEvent`) |

```vb
Imports GitHub.Copilot.SDK

Dim client As New CopilotClient(New CopilotClientOptions With {
    .ClientInfo = New CopilotClientInfo With {.ApplicationName = "my-ide", .ApplicationVersion = "1.4.0"},
    .Mode = ClientMode.Empty, ' every session must list AvailableTools
    .Network = New NetworkOptions With {
        .Scope = NetworkScope.Connection,
        .ResolveProxy = Function(url) Task.FromResult("PROXY proxy.contoso.com:8080; DIRECT"),
        .AuthenticateBasic = Function(request) Task.FromResult(New NetworkBasicCredentials With {
            .Username = "svc-copilot", .Password = LoadSecret()})
    }
})
Await client.StartAsync()

Dim lookupOrder As New DefineTool("lookup_order", "Look up an order", Nothing,
                                  Function(input) Task.FromResult("shipped")) With {.Idempotent = True}

Dim session As CopilotSession
Try
    session = Await client.CreateSessionAsync(New SessionConfig With {
        .Model = "ollama/llama3",
        .Name = "release-triage",
        .AvailableTools = New List(Of String) From {"lookup_order", "view"},
        .Tools = New List(Of DefineTool) From {lookupOrder},
        .AutoTier = AutoTier.Intelligence,
        .AskUserVariant = AskUserVariant.Elicitation,
        .SandboxConfig = New SandboxConfig With {.Enabled = True, .AllowBypass = True},
        .FeatureFlags = New Dictionary(Of String, Boolean) From {{"my-flag", True}},
        .AllowedModels = New List(Of String) From {"ollama/llama3"},
        .AuthClientIdMetadataUrl = "https://example.com/oauth/client.json",
        .AgentDirectories = New List(Of String) From {"./agents"},
        .HookDirectories = New List(Of String) From {"./hooks"},
        .TrustWorkingDirectory = WorkingDirectoryTrust.Session,
        .ImageGeneration = New ImageGenerationConfig With {.Enabled = True, .SaveToDisk = True},
        .Autopilot = New AutopilotOptions With {.ContinuationEnabled = True, .MaxContinues = 5},
        .ModelCapabilities = New Dictionary(Of String, Object) From {
            {"limits", New ModelCapabilitiesLimits With {.MaxOutputTokens = 16384}.ToWire()}},
        .Providers = New List(Of NamedProviderConfig) From {
            New NamedProviderConfig With {
                .Name = "ollama", .Type = "openai", .BaseUrl = "http://localhost:11434/v1",
                .ApiKeyCommand = "op read op://dev/ollama/key"}},
        .Models = New List(Of ProviderModelConfig) From {
            New ProviderModelConfig With {.Provider = "ollama", .Id = "llama3", .MaxOutputTokens = 4096}},
        .DefaultProvider = "ollama",
        .CapiEnabled = False,
        .GitHubTokenProvider = Function(args) Task.FromResult(
            GitHubTokenProviderResult.Token(LoadToken(args.Host), expiresIn:=28800)),
        .OnSubagentStart = Function(input) Task.FromResult(New SubagentStartHookOutput With {
            .AdditionalContext = $"You are {input.AgentName}; be concise."}),
        .OnShellAuthenticationRequest = Function(request, cancellationToken)
                                            If request.InputKind = ShellAuthenticationInputKind.Confirm Then
                                                Return Task.FromResult(ShellAuthenticationResponse.Cancel())
                                            End If
                                            Return Task.FromResult(ShellAuthenticationResponse.Accept(AskUser(request.Prompt)))
                                        End Function,
        .OnStoreMemory = Function(memory) SaveMemoryAsync(memory.Subject, memory.Fact, memory.Citations)
    })
Catch ex As JsonRpcException When ex.PolicyHelperFailure IsNot Nothing
    Console.WriteLine($"Blocked by the policy helper ({ex.PolicyHelperFailure.Category}): {ex.PolicyHelperFailure.Message}")
    Throw
End Try

session.On(Of ToolShellOutputEvent)(Sub(evt) Console.Write(evt.Data?.Text))

Await session.SendAsync(New MessageOptions With {
    .Prompt = "Where is order 42? Answer as JSON.",
    .Source = MessageSource.Agent("reviewer"),
    .ClientTurnId = "turn-1",
    .ResponseSchema = New Dictionary(Of String, Object) From {{"type", "object"}}
})

Await session.SetAutoTierAsync(AutoTier.Fast)
Dim settings = Await session.GetManagedSettingsAsync()
Dim policy = Await session.RecheckPolicyHelperAsync()
Await client.ClearManagedSettingsCacheAsync()

Dim modeResult = Await session.SetPermissionModeAsync(PermissionMode.AllowAll)
If Not modeResult.Success AndAlso modeResult.Reason = PermissionModeRefusalReason.ManagedPolicy Then
    Console.WriteLine($"Managed policy keeps the permission mode at {modeResult.Mode}")
End If

' Detach (state on disk is kept) and resume later
Await session.DisconnectAsync()
Dim resumed = Await client.ResumeSessionAsync(session.SessionId, New ResumeSessionConfig With {
    .AvailableTools = New List(Of String) From {"lookup_order", "view"},
    .AllowTranscriptRecovery = True,
    .SuppressResumeEvent = True,
    .ClearDefaultProvider = True
})
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not
  available: it is an experimental upstream surface that depends on the runtime's in-process
  hosting library.
- In-process native runtime hosting is not the default connection: this SDK keeps spawning the
  CLI over stdio. The existing `InProcess` option is unchanged.
- Tool invocations and permission prompts still use this SDK's `session.toolCall` /
  `session.permissionRequest` notification path; `Tools` declares the definitions (including
  `idempotent`) to the runtime.

## API Reference

### CopilotClient

#### Constructor

```vb
Dim client As New CopilotClient(Optional options As CopilotClientOptions = Nothing)
```

**Options:**

| Property | Type | Default | Description |
|---|---|---|---|
| `CliPath` | `String` | env or `"copilot"` | Path to CLI executable |
| `CliArgs` | `String()` | `Nothing` | Extra arguments for the CLI process |
| `CliUrl` | `String` | `Nothing` | URL of existing CLI server |
| `Port` | `Integer` | `0` | Server port (0 = random) |
| `UseStdio` | `Boolean` | `True` | Use stdio transport |
| `LogLevel` | `String` | `"info"` | Log level |
| `AutoStart` | `Boolean` | `True` | Auto-start server when needed |
| `Cwd` | `String` | `Nothing` | Working directory |
| `Environment` | `Dictionary(Of String, String)` | `Nothing` | Env vars for CLI |
| `GitHubToken` | `String` | `Nothing` | GitHub token for auth |
| `UseLoggedInUser` | `Boolean?` | `Nothing` | Use logged-in user auth |
| `Streaming` | `Boolean` | `False` | Enable streaming deltas |
| `SessionFs` | `SessionFsConfig` | `Nothing` | Session filesystem config |
| `Telemetry` | `TelemetryConfig` | `Nothing` | OpenTelemetry config |
| `SessionIdleTimeoutSeconds` | `Integer?` | `Nothing` | Idle timeout in seconds |

#### Methods

##### StartAsync

```vb
Await client.StartAsync(Optional cancellationToken As CancellationToken = Nothing)
```

Starts the CLI server process and connects. Called automatically if `AutoStart` is `True`.

##### StopAsync

```vb
Await client.StopAsync()
```

Gracefully shuts down all sessions and kills the CLI process.

##### CreateSessionAsync

```vb
Dim session = Await client.CreateSessionAsync(Optional config As SessionConfig = Nothing)
```

Creates a new conversation session. See [SessionConfig](#sessionconfig) for options.

##### ResumeSessionAsync

```vb
Dim session = Await client.ResumeSessionAsync(sessionId, Optional config As ResumeSessionConfig = Nothing)
```

Resumes a previously created session by its ID.

##### GetSessionMetadataAsync

```vb
Dim metadata = Await client.GetSessionMetadataAsync(sessionId)
```

Returns metadata (creation time, model, workspace path) for a session.

##### SetForegroundSessionIdAsync

```vb
Await client.SetForegroundSessionIdAsync(sessionId)
```

Tells the server which session is in the foreground.

##### ListSessionsAsync

```vb
Dim sessions = Await client.ListSessionsAsync()
```

Lists all sessions on the server.

##### DeleteSessionAsync

```vb
Await client.DeleteSessionAsync(sessionId)
```

Permanently deletes a session and its on-disk data.

##### PingAsync

```vb
Dim response = Await client.PingAsync(Optional message As String = Nothing)
```

Pings the CLI server. Returns a `PingResponse` with `Message` and `ServerTime`.

##### ListModelsAsync

```vb
Dim models = Await client.ListModelsAsync()
```

Lists available models from the server.

---

### CopilotSession

#### Properties

| Property | Type | Description |
|---|---|---|
| `SessionId` | `String` | Unique session identifier |
| `WorkspacePath` | `String` | Workspace path (infinite sessions) |
| `Capabilities` | `SessionCapabilities` | Host capabilities |

#### Methods

##### SendAsync

```vb
Dim messageId = Await session.SendAsync(New MessageOptions With { .Prompt = "Hello" })
```

Sends a message and returns immediately with the message ID.

##### SendAndWaitAsync

```vb
Dim response = Await session.SendAndWaitAsync(
    New MessageOptions With { .Prompt = "Hello" },
    Optional timeout As TimeSpan? = Nothing
)
```

Sends a message and waits until the session becomes idle. Returns the final `AssistantMessageEvent`.

##### On

```vb
Dim subscription = session.On(Sub(evt)
    ' Handle all events
End Sub)

' Or typed:
Dim sub2 = session.On(Of AssistantMessageEvent)(Sub(msg)
    Console.WriteLine(msg.Data?.Content)
End Sub)
```

Registers a callback for session events. Returns an `IDisposable` for unsubscribing.

##### RegisterTool

```vb
session.RegisterTool(New DefineTool With {
    .Name = "my_tool",
    .Description = "Does something",
    .Handler = Async Function(input) "result"
})
```

##### AbortAsync

```vb
Await session.AbortAsync()
```

Aborts the current in-flight operation.

##### GetMessagesAsync

```vb
Dim messages = Await session.GetMessagesAsync()
```

Retrieves the conversation message history.

##### DisconnectAsync

```vb
Await session.DisconnectAsync()
```

Disconnects from the session without deleting server-side data.

---

### SessionConfig

| Property | Type | Description |
|---|---|---|
| `Model` | `String` | Model to use (e.g. "gpt-4") |
| `SystemMessage` | `String` | System message prepended to conversation |
| `OnPermissionRequest` | `PermissionHandler` | Permission request handler |
| `OnUserInput` | `Func(Of UserInputRequest, Task(Of String))` | User input handler |
| `OnElicitation` | `Func(Of ElicitationRequest, Task(Of ElicitationResponse))` | Elicitation handler |
| `Tools` | `List(Of DefineTool)` | Custom tools |
| `Skills` | `List(Of String)` | Enabled skills |
| `CustomizeMode` | `Boolean` | Enable customize/edit mode |
| `AgentMode` | `Boolean` | Enable agent mode |
| `Attachments` | `List(Of Attachment)` | Initial attachments |

---

### Event Types

| Event Class | Type String | Description |
|---|---|---|
| `AssistantMessageEvent` | `assistant.message` | Complete assistant message |
| `AssistantMessageDeltaEvent` | `assistant.message_delta` | Streaming partial message |
| `AssistantReasoningEvent` | `assistant.reasoning` | Reasoning content |
| `SessionIdleEvent` | `session.idle` | Session finished processing |
| `SessionErrorEvent` | `session.error` | Session error |
| `ToolCallEvent` | `tool.call` | Tool invocation |
| `SessionCompactionStartEvent` | `session.compaction_start` | Compaction started |
| `SessionCompactionCompleteEvent` | `session.compaction_complete` | Compaction finished |

---

### DefineTool

```vb
Dim tool As New DefineTool With {
    .Name = "calculator",
    .Description = "Evaluate a math expression",
    .ParametersSchema = New Dictionary(Of String, Object) From {
        {"type", "object"},
        {"properties", New Dictionary(Of String, Object) From {
            {"expression", New Dictionary(Of String, Object) From {
                {"type", "string"},
                {"description", "Math expression to evaluate"}
            }}
        }},
        {"required", New String() {"expression"}}
    },
    .Handler = Async Function(input)
                   Return "42"
               End Function
}
```

---

### Permission Handling

```vb
' Approve everything
Dim config As New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
}

' Deny everything
Dim config2 As New SessionConfig With {
    .OnPermissionRequest = PermissionHandlers.DenyAll()
}

' Custom logic
Dim config3 As New SessionConfig With {
    .OnPermissionRequest = Async Function(req)
        If req.Tool = "dangerous_tool" Then
            Return New PermissionResponse With { .Allowed = False, .Reason = "blocked" }
        End If
        Return New PermissionResponse With { .Allowed = True }
    End Function
}
```

---

## v2.0 Features

### Session Filesystem (SessionFs)

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .SessionFs = New SessionFsConfig With {
        .InitialCwd = "C:\MyProject",
        .SessionStatePath = "C:\SessionData"
    }
})
```

### Idle Timeout

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .SessionIdleTimeoutSeconds = 300
})
```

### Skills

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Skills = New List(Of String) From {"code-review", "testing"}
})
```

### Session Metadata

```vb
Dim metadata = Await client.GetSessionMetadataAsync(session.SessionId)
Console.WriteLine($"Created: {metadata.CreatedAt}")
Console.WriteLine($"Model: {metadata.Model}")
```

### Customize Mode

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .CustomizeMode = True
})
```

### Agent Mode

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .AgentMode = True,
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### Streaming

Subscribe to delta events for real-time streaming:

```vb
session.On(Of AssistantMessageDeltaEvent)(Sub(delta)
    Console.Write(delta.Data?.Delta)
End Sub)
```

### Infinite Sessions

Sessions persist by default. Resume any session later:

```vb
Dim session = Await client.ResumeSessionAsync("previous-session-id")
Console.WriteLine($"Workspace: {session.WorkspacePath}")
```

### Tools

Register custom tools for the assistant to invoke:

```vb
Dim tool As New DefineTool("search_db", "Search the database",
    schema, Async Function(input)
        Return "found 42 results"
    End Function)

Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .Tools = New List(Of DefineTool) From {tool},
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### Telemetry

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .Telemetry = New TelemetryConfig With {
        .ExporterType = "otlp-http",
        .OtlpEndpoint = "http://localhost:4318",
        .SourceName = "my-app"
    }
})
```

### Multiple Sessions

```vb
Dim session1 = Await client.CreateSessionAsync(New SessionConfig With { .Model = "gpt-4" })
Dim session2 = Await client.CreateSessionAsync(New SessionConfig With { .Model = "gpt-5" })

Await client.SetForegroundSessionIdAsync(session1.SessionId)
```

### Listing and Deleting Sessions

```vb
Dim sessions = Await client.ListSessionsAsync()
For Each meta In sessions
    Console.WriteLine($"{meta.SessionId} - {meta.Model}")
Next

Await client.DeleteSessionAsync(sessions(0).SessionId)
```

---

## Recent Features (v2.4–v2.5)

Options added in the v2.4 and v2.5 upstream syncs. Unless noted, these are properties
of `SessionConfig` (passed to `CreateSessionAsync`); a few live on `CopilotClientOptions`
or `MessageOptions`.

### v2.5 session options

- **Reasoning effort** — `SessionConfig.ReasoningEffort` (`"low"`/`"medium"`/`"high"`).
- **Tool search** — `SessionConfig.ToolSearch` (discover tools on demand).
- **Session rewind** — `SessionConfig.RewindEnabled` (restore an earlier turn).
- **Content exclusion** — `SessionConfig.ContentExclusion` (honor content-exclusion rules).
- **Additional directories** — `SessionConfig.AdditionalDirectories` (extra workspace dirs).
- **Disabled MCP servers** — `SessionConfig.DisabledMcpServers`.
- **GitHub MCP tool config** — `SessionConfig.GithubMcpToolConfig`.
- **Canvas provider** — `SessionConfig.CanvasProvider`.
- **Custom agents local-only** — `SessionConfig.CustomAgentsLocalOnly`.
- **Permission decision context** — `SessionConfig.DecisionContext`.
- **User-prompt-transformed hook** — `SessionConfig.OnUserPromptTransformed`.
- **Built-in plugin directories** — `SessionConfig.BuiltinPluginDirectories`.
- **Agent-factory args schema** — `SessionConfig.ArgsSchema`.
- **In-process FFI transport** — `SessionConfig.InProcess`.
- **Experimental mode** — `SessionConfig.ExperimentalMode`.

### v2.4 session options

- **BYOK bearer token provider** — `CopilotClientOptions.BearerTokenProvider` (receives `ProviderTokenArgs`).
- **HTTP request handler** — `CopilotClientOptions.RequestHandler` (`CopilotRequestHandler`).
- **MCP OAuth token handler** — `SessionConfig.OnMcpAuthRequest`.
- **Session citations** — `SessionConfig.EnableCitations`.
- **Excluded built-in agents** — `SessionConfig.ExcludedBuiltinAgents`.
- **Session spending limits** — `SessionConfig.SessionLimits` (`SessionLimitsConfig.MaxAiCredits`).
- **Session memory** — `SessionConfig.Memory` (`MemoryConfiguration`).
- **OTLP protocol** — `SessionConfig.OtlpProtocol`.
- **WebSocket transport** — `SessionConfig.EnableWebSocketResponses`.
- **Experiment assignments** — `SessionConfig.ExpAssignments`.
- **Post/pre-tool hooks** — `SessionConfig.OnPostToolUse`, `SessionConfig.OnPreMcpToolCall`.
- **Per-message overrides** — `MessageOptions.AgentMode`, `MessageOptions.DisplayPrompt`.
- **Constants** — `ToolDefer.Auto`/`ToolDefer.Never`, `SystemMessageSection.Preamble`/`Preserve`, `GitHubAttachment.GitHubCommit`/`GitHubRepository`.

### Reasoning effort and tool search

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .ReasoningEffort = "high",
    .ToolSearch = New Dictionary(Of String, Object) From {{"enabled", True}},
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### Session rewind

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .RewindEnabled = True,
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### Content exclusion and additional directories

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .ContentExclusion = True,
    .AdditionalDirectories = New List(Of String) From {"C:\shared", "C:\reference"},
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

### BYOK bearer token provider

```vb
Dim client As New CopilotClient(New CopilotClientOptions With {
    .BearerTokenProvider = Function(args As ProviderTokenArgs)
        ' Return a fresh bearer token for args.SessionId
        Return Task.FromResult("Bearer-token-value")
    End Function
})
```

### Session spending limits

```vb
Dim session = Await client.CreateSessionAsync(New SessionConfig With {
    .SessionLimits = New SessionLimitsConfig With { .MaxAiCredits = 500 },
    .OnPermissionRequest = PermissionHandlers.ApproveAll()
})
```

---

## Building from Source

```bash
cd visualbasic
dotnet restore
dotnet build
```

## Running Tests

```bash
cd visualbasic
dotnet test tests/CopilotSDK.Supercharged.VB.Tests.vbproj
```

## License

MIT

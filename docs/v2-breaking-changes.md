# SDK v2 breaking changes

This document collects SDK v2 breaking changes and the corresponding migrations, tracked in [github/copilot-sdk#2522](https://github.com/github/copilot-sdk/issues/2522). Planned but unimplemented changes are not included. SDK v2 is the package major version; the restructured client/session API requires SDK protocol **4**. These are separate version numbers. Upgrade the SDK and runtime together: incompatible pairs fail connection negotiation.

For an existing integration, migrate runtime ownership first, then the restructured client and session options, then provider routing. If you call JSON-RPC directly, also update the lifecycle payloads; renaming SDK imports alone is not sufficient. The option restructuring does not change the persisted session/event format or require rewriting saved sessions. Runtime-only configuration and callbacks still need to be supplied again where indicated below.

## Table of contents

* [In-process runtime hosting by default](#in-process-runtime-hosting-by-default)
* [Explicit runtime connection APIs](#explicit-runtime-connection-apis)
* [Process options on spawned connections](#process-options-on-spawned-connections)
* [Runtime startup validation for attached connections](#runtime-startup-validation-for-attached-connections)
* [Restructured client options](#restructured-client-options)
* [Restructured session options](#restructured-session-options)
* [Restructured lifecycle RPC payloads](#restructured-lifecycle-rpc-payloads)
* [Named provider connections and model definitions](#named-provider-connections-and-model-definitions)
* [Provider routing on resume](#provider-routing-on-resume)
* [Convenience-method startup](#convenience-method-startup)
* [Operation failure reporting](#operation-failure-reporting)
* [Canonical model and protocol types](#canonical-model-and-protocol-types)
* [.NET cancellation and generated RPC arguments](#net-cancellation-and-generated-rpc-arguments)
* [Go composable sessions and RPC requests](#go-composable-sessions-and-rpc-requests)
* [Rust canonical RPC requests and results](#rust-canonical-rpc-requests-and-results)
* [Removed deprecated APIs and fields](#removed-deprecated-apis-and-fields)
* [Microsoft Copilot SDK identities](#microsoft-copilot-sdk-identities)
* [Python generated protocol imports](#python-generated-protocol-imports)
* [Explicit client mode](#explicit-client-mode)
* [Java generated DTO construction](#java-generated-dto-construction)

## In-process runtime hosting by default

All six SDKs now default to loading the native Rust runtime into your application's process instead of starting a separate runtime process. This removes the child-process requirement for ordinary SDK use: applications can host the runtime directly without creating and managing a separate runtime process or sending runtime requests across process boundaries.

> [!WARNING]
> In-process hosting is not a sandbox and provides no crash isolation. A Rust panic in the shipped runtime terminates your application, even during a single request or model turn.

To keep the runtime in a separate child process, explicitly select a stdio connection. For example, in TypeScript:

<!-- docs-validate: strict -->

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forSpawnedStdio(),
});
```

Alternatively, set the environment override before starting your application. This works across all six SDKs when no explicit connection is configured:

```bash
export COPILOT_SDK_DEFAULT_CONNECTION=stdio
```

An explicit connection takes precedence over the environment override.

Consider opting out of in-process hosting when your application needs:

* **Independent runtime lifetimes**: Clients share a native library and worker pool, which can remain loaded until the application exits. Use subprocesses if clients need independent runtime versions or process-level restarts.
* **Per-client process configuration**: Process environment maps and working directories belong only on spawned connections. Select a subprocess for those options, or use supported typed client settings and per-session working directories.
* **Interactive Entra on macOS**: The runtime's OneAuth interactive Microsoft Entra sign-in requires a host-provided UI-thread scheduler (`runOnUiThread` in TypeScript) that executes native UI actions on the process main thread. This applies across all six SDKs. If your application cannot provide that scheduler, select an explicit stdio connection, as shown above, to retain subprocess-hosted interactive sign-in.
* **Different native deployment requirements**: In-process hosting requires a compatible native runtime for your operating system, CPU architecture, and Linux C library. Missing native assets or unsupported hosts fail without automatic subprocess fallback. Java GraalVM native-image cannot use JNA callback upcalls; choose a subprocess or external-server connection.
* **Native-free Go builds**: Selecting stdio at runtime does not remove Go's FFI dependency. Build with `-tags copilot_no_inprocess` and select a subprocess or external-server connection to exclude it. Alpine/musl in-process builds instead require `CGO_ENABLED=1` and a musl C toolchain.

See [Run the Copilot runtime in process](./setup/in-process-runtime.md) for subprocess selection in every language, native packaging requirements, and lifecycle details, and the [Go SDK README](../go/README.md#in-process-transport) for its native-free build option.

SDK asset-extraction options also use expanded directory names: Go's `embeddedcli.Config.Dir` becomes `embeddedcli.Config.Directory`; Rust's `bundled_cli_extract_dir` and `with_bundled_cli_extract_dir` become `bundled_cli_extract_directory` and `with_bundled_cli_extract_directory` on `OwnedRuntimeOptions`, rather than `ClientOptions`. Extraction behavior and environment overrides are unchanged.

## Explicit runtime connection APIs

A connection determines both the transport and who owns the runtime's lifetime. Connection names now distinguish spawning a runtime from attaching to an existing one so that ownership is visible: the client owns a spawned child runtime, whereas attaching must not shut down an existing server.

URI-based connection factories and types are removed; attached TCP connections instead require a string host and an integer port. These describe the actual plaintext TCP transport without implying support for URL paths, HTTP semantics, or TLS. Separate factory names also avoid relying on overloads that are not available uniformly across the six languages.

Apply the following renames. Factory methods belong to `RuntimeConnection`; Go uses connection structs. Rust's `Transport` type is replaced by `RuntimeConnection` with ownership-specific options.

| SDK               | Spawned stdio                                           | Spawned TCP                                         | Attached TCP                                      |
| ----------------- | ------------------------------------------------------- | --------------------------------------------------- | ------------------------------------------------- |
| TypeScript / Java | `forStdio` -> `forSpawnedStdio`                         | `forTcp` -> `forSpawnedTcp`                         | `forUri(url)` -> `forTcp(host, port)`             |
| .NET              | `ForStdio` -> `ForSpawnedStdio`                         | `ForTcp` -> `ForSpawnedTcp`                         | `ForUri(uri)` -> `ForTcp(host, port)`             |
| Python            | `for_stdio` -> `for_spawned_stdio`                      | `for_tcp` -> `for_spawned_tcp`                      | `for_uri(uri)` -> `for_tcp(host, port)`           |
| Go                | `StdioConnection` -> `SpawnedStdioConnection`           | `TCPConnection` -> `SpawnedTCPConnection`           | `URIConnection` -> `TCPConnection`                |
| Rust              | `Transport::Stdio` -> `RuntimeConnection::SpawnedStdio` | `Transport::Tcp` -> `RuntimeConnection::SpawnedTcp` | `Transport::External` -> `RuntimeConnection::Tcp` |

In .NET, TypeScript, Python, and Java, the corresponding spawned connection types become `SpawnedStdioRuntimeConnection` and `SpawnedTcpRuntimeConnection`. `TcpRuntimeConnection` now represents an attached connection with host and port; `UriRuntimeConnection` is removed. Go's attached `TCPConnection` likewise uses `Host` and `Port` instead of `URL`. Rust's attached variant retains its separate `host` and `port` fields.

TypeScript connection object literals also change their `kind` values: `"stdio"` becomes `"spawned-stdio"`, the old spawning `"tcp"` becomes `"spawned-tcp"`, and the old attached `"uri"` becomes `"tcp"`.

For attached TCP, pass a hostname or IP address without a scheme, embedded port, path, or IPv6 brackets, and a separate port from 1 through 65535. Keep any connection token as a separate option. Java's legacy `CopilotClientOptions.setCliUrl(...)` is also removed; use `setRuntime(RuntimeConnection.forTcp(host, port))`.

Use `ForInProcess` in .NET, `forInProcess` in TypeScript and Java, `for_inprocess` in Python, `InProcessConnection` in Go, or `RuntimeConnection::InProcess` in Rust for owned in-process hosting. Rust also supplies `for_in_process`, `for_spawned_stdio`, `for_spawned_tcp`, and `for_tcp` factories. These connection API changes preserve the existing ownership and teardown behavior; the [new default hosting choice](#in-process-runtime-hosting-by-default) is a separate change.

## Process options on spawned connections

Only a connection that starts a child process can choose its executable, arguments, environment, and working directory. Keeping those settings on the spawned connection makes invalid combinations with attached TCP or in-process hosting unrepresentable, removes competing client and connection environment settings, and avoids suggesting that an SDK can reconfigure an existing server or change its application's process directory.

Move client-level environment and process working-directory settings onto the spawned connection. Executable paths and arguments also belong there, including Java's legacy client setters and Rust's process builders:

| SDK        | Spawned process configuration                                                                                                                                                                      |
| ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| TypeScript | `forSpawnedStdio({ path, args, env, workingDirectory })` or the corresponding `forSpawnedTcp` options                                                                                              |
| .NET       | `ForSpawnedStdio(path: ..., args: ...)` or `ForSpawnedTcp`; set `Environment` and `WorkingDirectory` on the returned connection                                                                    |
| Python     | `for_spawned_stdio(path=..., args=..., env=..., working_directory=...)` or `for_spawned_tcp`                                                                                                       |
| Go         | `SpawnedStdioConnection{Path: ..., Args: ..., Env: ..., WorkingDirectory: ...}` or `SpawnedTCPConnection`                                                                                          |
| Java       | `forSpawnedStdio(path).setArgs(...).setEnvironment(...).setWorkingDirectory(...)` or `forSpawnedTcp`                                                                                               |
| Rust       | `RuntimeConnection::SpawnedStdio(SpawnedProcessOptions::default().with_program(...).with_args(...).with_env(...).with_working_directory(...))`; spawned TCP has a `process` field of the same type |

Java also removes the client-level `port`, `useStdio`, and `tcpConnectionToken` shortcuts; configure the corresponding connection instead. In Rust, `program`, `working_directory`, `env`, and `env_remove` and their builders move from `ClientOptions` to `SpawnedProcessOptions`. Combine the former `prefix_args` and `extra_args`, in that order, into `args` (`with_args`); all supplied arguments now precede SDK-managed arguments, so trailing overrides of SDK-managed flags are no longer supported.

Java's data-directory accessors are renamed from `getCopilotHome` / `setCopilotHome` to `getBaseDirectory` / `setBaseDirectory` on owned runtime connections; the runtime environment variable remains `COPILOT_HOME`. Rust's process-directory builder is renamed from `with_cwd` to `with_working_directory`, matching its field and the session builders.

Across all SDKs, omitted environments inherit; supplied environments replace, including empty ones. Explicit typed settings and required SDK settings win. Rust's `env` becomes optional; removals precede typed settings. For inheritance plus overrides, use `.with_env(std::env::vars_os().chain(overrides))` in Rust. `COPILOT_SDK_DEFAULT_CONNECTION` reads the application's environment, not the child's. Replacement must include needed variables such as `PATH` and Windows `SystemRoot`.

Typed authentication and telemetry move into nested client options; owned settings such as `baseDirectory` move onto the runtime connection. Session working directories remain session options on every transport; use them instead of process configuration when attaching to an existing runtime or hosting in process.

## Runtime startup validation for attached connections

Runtime startup settings work with both spawned and SDK-owned in-process runtimes. Restructured client authentication and telemetry options do not make those startup settings valid for attached clients. When attaching to an existing runtime, the SDK rejects inapplicable startup settings instead of silently ignoring them. Errors identify the offending options before a connection is opened.

Remove default authentication (`github.token` / `github.useLoggedInUser`), telemetry exporter and runtime log-level overrides (`observability.telemetry` / `observability.logLevel`), and other runtime-only overrides from attached clients; configure those on the existing runtime instead. Runtime home and idle timeout are available only on owned connection types. Rust applies the same validation when borrowing an in-process server. Session-scoped authentication, SDK diagnostics, callbacks, and connection settings remain supported.

Normal client defaults remain valid. Optional settings are checked for presence, including supplied `false`, zero, and empty values. Where an existing non-nullable option cannot distinguish omission from its default value, the default remains accepted and only nondefault configuration is rejected.

Python's `observability.log_level` and Java's `observability.logLevel` now default to `None` and `null`, leaving the runtime's logging default unchanged instead of always passing `"info"`. Attached connections reject every explicitly configured level, including `"info"`; omit the setting or clear it with `None` / `setLogLevel(null)` on the observability options.

## Restructured client options

Replace the client's `connection` option with `runtime` (`Connection` -> `Runtime` in .NET and Go; `setConnection` -> `setRuntime` in Java; Rust uses `ClientOptions::with_runtime`). Keep the explicit `mode`; restructuring does not change its capability defaults.

Move related options together. Paths in this table use TypeScript names; the corresponding options use each language's ordinary casing.

| Before                                                | After                                                                                  |
| ----------------------------------------------------- | -------------------------------------------------------------------------------------- |
| `baseDirectory`, `sessionIdleTimeoutSeconds`          | Owned `runtime` connection options                                                     |
| `logLevel`, `telemetry`, `onGetTraceContext`          | `observability.logLevel`, `observability.telemetry`, `observability.onGetTraceContext` |
| `gitHubToken`, `useLoggedInUser`, `onGitHubTelemetry` | `github.token`, `github.useLoggedInUser`, `github.onTelemetry`                         |
| `onListModels`, `requestHandler`                      | `models.onListModels`, `models.requestHandler`                                         |
| `builtinPluginDirectories`                            | `plugins.builtinDirectories`                                                           |
| `extensionLaunchProvider`                             | `extensions.launchProvider`                                                            |
| `installationConfirmationHandler`                     | `installations.confirmationHandler`                                                    |

Already-structured options such as `entra`, `network`, `sessionFs`, and `clientInfo` remain at the client root. Client `models` supplies discovery/transport callbacks; it is not the session's provider model registry.

Before:

<!-- docs-validate: skip -->

```typescript
const options = {
    mode: "coding-agent",
    connection: RuntimeConnection.forSpawnedStdio(),
    baseDirectory: "/path/to/copilot-data",
    sessionIdleTimeoutSeconds: 600,
    logLevel: "debug",
    gitHubToken: process.env.COPILOT_GITHUB_TOKEN,
};
```

After:

<!-- docs-validate: strict -->

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forSpawnedStdio({
        baseDirectory: "/path/to/copilot-data",
        sessionIdleTimeoutSeconds: 600,
    }),
    observability: { logLevel: "debug" },
    github: { token: process.env.COPILOT_GITHUB_TOKEN },
});
```

To keep the deployment's choice of in-process or stdio hosting while supplying owned settings, use `RuntimeConnection.forDefault({ baseDirectory: "/path/to/copilot-data" })`. The corresponding factories are .NET `ForDefault`, Python `for_default`, Go `copilot.ForDefault`, Java `forDefault`, and Rust `RuntimeConnection::for_default`. They resolve the deployment default synchronously when called, without starting a runtime. `COPILOT_SDK_DEFAULT_CONNECTION` accepts `stdio` and `inprocess`, case-insensitively; unset or empty selects in-process hosting, and invalid values fail. Use a concrete factory when the hosting mode must not depend on that environment variable.

`OwnedRuntimeOptions` contains runtime home and session idle timeout, not process environment or working directory. Rust embeds it as `owned` in `SpawnedProcessOptions` / `InProcessRuntimeOptions`; spawned TCP wraps `SpawnedProcessOptions` in `SpawnedTcpRuntimeOptions.process`. Attached `TcpRuntimeOptions` has only endpoint/connection settings. In the other SDKs, owned connection types expose the owned settings directly. Omitting runtime home keeps the runtime default (`~/.copilot`); omitting idle timeout or setting it to zero disables idle cleanup.

Client `github.useLoggedInUser` defaults to true unless an explicit `github.token` is supplied. It configures owned-runtime authentication, not per-session identity and not CAPI model availability. A trace-context callback does not enable a telemetry exporter.

## Restructured session options

Replace `SessionConfig` with `CreateSessionOptions` and `ResumeSessionConfig` with `ResumeSessionOptions` in TypeScript, .NET, Go, Java, and Rust. TypeScript and .NET's shared base becomes `SessionOptionsBase`. Python exposes `CreateSessionOptions` and `ResumeSessionOptions` as typed keyword dictionaries; pass nested options as keyword arguments or unpack a dictionary with `**options`. Create/resume method names remain unchanged.

The selected `model`, `workingDirectory`, `systemMessage`, `clientName`, and diagnostics stay at the session root. `configDirectory` and `enableConfigDiscovery` also stay at the root: do not put them under `state`, `filesystem`, or `instructions`. `sessionId`, `name`, and `cloud` remain create-specific. Resume controls such as `continuePendingWork`, `allowTranscriptRecovery`, and `suppressResumeEvent` belong at the resume root; Java renames its former `disableResume` accessors as shown below.

Move flat settings into the corresponding nested options:

| Before                                                                                           | After                                                                                                                    |
| ------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------ |
| `reasoningEffort`, `reasoningSummary`, `contextTier`                                             | `inference.reasoningEffort`, `inference.reasoningSummary`, `inference.contextTier`                                       |
| `allowMultipleToolCalls`, `modelCapabilities`, `enableCitations`                                 | `inference.allowMultipleToolCalls`, `inference.capabilities`, `inference.enableCitations`                                |
| `tools`, `availableTools`, `excludedTools`, `toolSearch`, `largeOutput`                          | `tools.definitions`, `tools.availableTools`, `tools.excludedTools`, `tools.search`, `tools.largeOutput`                  |
| `imageGeneration`, `manageScheduleEnabled`                                                       | `tools.imageGeneration`, `tools.manageScheduleEnabled`                                                                   |
| `onPermissionRequest`, `sandboxConfig`, `permissionMode`                                         | `permissions.onRequest`, `permissions.sandbox`, `permissions.mode`                                                       |
| `commands`, `askUserVariant`, `autoModeRecovery`                                                 | `interaction.commands`, `interaction.askUserVariant`, `interaction.autoModeRecovery`                                     |
| `onUserInputRequest`, `onElicitationRequest`, `onShellAuthenticationRequest`                     | `interaction.onUserInputRequest`, `interaction.onElicitationRequest`, `interaction.onShellAuthenticationRequest`         |
| `onExitPlanModeRequest`, `onAutoModeSwitchRequest`                                               | `interaction.onExitPlanModeRequest`, `interaction.onAutoModeSwitchRequest`                                               |
| `streaming`, `includeSubAgentStreamingEvents`, `onEvent`                                         | `events.streaming`, `events.includeSubAgentStreamingEvents`, `events.onEvent`                                            |
| `customAgents`, `defaultAgent`, `agent`                                                          | `agents.definitions`, `agents.default`, `agents.selected`                                                                |
| `includedBuiltinAgents`, `excludedBuiltinAgents`, `customAgentsLocalOnly`, `agentDirectories`    | `agents.includedBuiltinAgents`, `agents.excludedBuiltinAgents`, `agents.localDiscoveryOnly`, `agents.directories`        |
| `skillDirectories`, `skillProvider`, `enableSkills`, `includedBuiltinSkills`, `disabledSkills`   | `skills.directories`, `skills.provider`, `skills.enabled`, `skills.includedBuiltinSkills`, `skills.disabledSkills`       |
| `mcpServers`, `disabledMcpServers`, `onMcpAuthRequest`, `enableMcpApps`                          | `mcp.servers`, `mcp.disabledServers`, `mcp.onAuthRequest`, `mcp.enableApps`                                              |
| `mcpOAuthTokenStorage`, `authClientIdMetadataUrl`                                                | `mcp.oauthTokenStorage`, `mcp.authClientIdMetadataUrl`                                                                   |
| `gitHubToken`, `gitHubTokenProvider`, `capi`, `githubMcpToolConfig`                              | `github.token`, `github.tokenProvider`, `github.capi`, `github.mcp`                                                      |
| `enableSessionTelemetry`, `enableManagedSettings`, `interactiveUser`                             | `github.telemetryEnabled`, `github.fetchManagedSettings`, `github.interactiveUser`                                       |
| `additionalDirectories`, `enableFileChangeTracking`, `trustWorkingDirectory`                     | `filesystem.additionalDirectories`, `filesystem.trackFileChanges`, create-only `filesystem.trustWorkingDirectory`        |
| `sessionLimits`, `autopilot`                                                                     | `execution.limits`, `execution.autopilot`                                                                                |
| `instructionDirectories`, `organizationCustomInstructions`, `enableOnDemandInstructionDiscovery` | `instructions.directories`, `instructions.organizationText`, `instructions.onDemandDiscovery`                            |
| `skipCustomInstructions`, `refreshCustomInstructions`                                            | `instructions.skipCustomInstructions`, create-only `instructions.refreshCustomInstructions`                              |
| `hooks`, `hookDirectories`, `enableFileHooks`                                                    | `hooks.handlers`, `hooks.directories`, `hooks.enableRepositoryHooks`                                                     |
| `coauthorEnabled`, `enableHostGitOperations`                                                     | `git.coauthorEnabled`, `git.enableHostGitOperations`                                                                     |
| `infiniteSessions`, `memory`, `enableSessionStore`                                               | `state.infiniteSessions`, `state.memory`, `state.enableCrossSessionStore`                                                |
| `skipEmbeddingRetrieval`, `embeddingCacheStorage`, `createSessionFsProvider`                     | `state.skipEmbeddingRetrieval`, `state.embeddingCacheStorage`, `state.createSessionFsProvider`                           |
| `pluginDirectories`                                                                              | `plugins.directories`                                                                                                    |
| `canvases`, `canvasProvider`, `requestCanvasRenderer`, `openCanvases`                            | `canvases.definitions`, `canvases.provider`, `canvases.requestRenderer`, resume-only `canvases.open`                     |
| `requestExtensions`, `extensionSdkPath`, `extensionInfo`                                         | `extensions.enabled`, `extensions.sdkPath`, `extensions.identity`                                                        |
| `enableExperimentalMode`, `featureFlags`, internal `expAssignments`                              | `experiments.enableExperimentalMode`, `experiments.flags`, internal `experiments.assignments`                            |
| `allowedModels`, `managedSettings`, `excludedManagedSettings`, `enforceManagedModelDefaults`     | `policy.allowedModels`, `policy.managedSettings`, `policy.excludedManagedSettings`, `policy.enforceManagedModelDefaults` |

For `provider`, `providers`, and `models`, see [Named provider connections and model definitions](#named-provider-connections-and-model-definitions).

Before:

<!-- docs-validate: skip -->

```typescript
const session = await client.createSession({
    model: "gpt-5.4",
    reasoningEffort: "medium",
    streaming: true,
    availableTools: ["custom:*"],
    onEvent: (event) => console.log(event.type),
    configDirectory: "/path/to/session-config",
    enableConfigDiscovery: false,
});
```

After:

<!-- docs-validate: strict -->

```typescript
import type { CreateSessionOptions } from "@microsoft/copilot-sdk";

const options: CreateSessionOptions = {
    model: "gpt-5.4",
    inference: { reasoningEffort: "medium" },
    events: {
        streaming: true,
        onEvent: (event) => console.log(event.type),
    },
    tools: { availableTools: ["custom:*"] },
    configDirectory: "/path/to/session-config",
    enableConfigDiscovery: false,
};
const session = await client.createSession(options);
```

Nested option types have names such as `SessionInferenceOptions`, `SessionEventOptions`, and `SessionToolOptions`. The language-specific construction differences are:

| SDK        | Equivalent restructured settings                                                                                                                                                                                                                                   |
| ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| TypeScript | Object literals: `inference: { reasoningEffort: "medium" }`, `events: { streaming: true }`                                                                                                                                                                         |
| C# / .NET  | Object initializers: `new CreateSessionOptions { Inference = new() { ReasoningEffort = "medium" }, Events = new() { Streaming = true } }`                                                                                                                          |
| Python     | Keyword dictionaries: `await client.create_session(inference={"reasoning_effort": "medium"}, events={"streaming": True})`; typed reusable options are `TypedDict`s, not generated RPC dataclasses                                                                  |
| Go         | Nested structs: `&copilot.CreateSessionOptions{Inference: copilot.SessionInferenceOptions{ReasoningEffort: "medium"}, Events: copilot.SessionEventOptions{Streaming: copilot.Bool(true)}}`; optional scalar pointers distinguish omission from explicit false      |
| Java       | Fluent SDK options: `new CreateSessionOptions().setInference(new SessionInferenceOptions().setReasoningEffort("medium")).setEvents(new SessionEventOptions().setStreaming(true))`; these are not generated-record builders                                         |
| Rust       | Nested option builders: `CreateSessionOptions::default().with_inference(\|inference\| inference.with_reasoning_effort("medium")).with_events(\|events\| events.with_streaming(true))`; closures receive the existing options so successive calls preserve siblings |

Some SDK-specific names need more than a casing change. Apply these migrations in addition to the shared table:

| SDK       | Before                                                                                                  | After                                                                                                                                   |
| --------- | ------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| Java      | `ResumeSessionConfig.isDisableResume()` / `setDisableResume(...)`                                       | `ResumeSessionOptions.isSuppressResumeEvent()` / `setSuppressResumeEvent(...)`                                                          |
| Java      | `setOnExitPlanMode(...)`, `setOnAutoModeSwitch(...)`                                                    | `SessionInteractionOptions.setOnExitPlanModeRequest(...)`, `SessionInteractionOptions.setOnAutoModeSwitchRequest(...)`                  |
| Java      | `setWorkingDirectoryTrust(...)` on create options                                                       | `CreateSessionFilesystemOptions.setWorkingDirectoryTrust(...)`, supplied through the create options' `setFilesystem(...)`               |
| Go        | `OnPermissionRequestWithEnvelope`, `OnPermissionRequestWithPrompt`                                      | `Permissions.OnRequestWithEnvelope`, `Permissions.OnRequestWithPrompt`                                                                  |
| Go        | `OnMCPAuthRuntimeRefresh`, `OnEventWithPermissionEnvelope`, `StoreMemory`                               | `MCP.OnAuthRuntimeRefresh`, `Events.OnEventWithPermissionEnvelope`, `State.StoreMemory`                                                 |
| Go        | `OnAttributedUserInputRequest`, `OnAttributedElicitationRequest`, `OnAttributedExitPlanModeRequest`     | `Interaction.OnAttributedUserInputRequest`, `Interaction.OnAttributedElicitationRequest`, `Interaction.OnAttributedExitPlanModeRequest` |
| Go / .NET | `CanvasHandler`                                                                                         | `Canvases.Handler`                                                                                                                      |
| Python    | `create_session_fs_handler=...`, `canvas_handler=...`                                                   | `state={"create_session_fs_provider": ...}`, `canvases={"handler": ...}`                                                                |
| Rust      | `approve_all_permissions()`, `deny_all_permissions()`, `approve_permissions_if(...)` on session options | The same methods on `SessionPermissionOptions`, configured through `.with_permissions(...)`                                             |
| Rust      | `with_permission_handler(...)`                                                                          | `SessionPermissionOptions::with_on_request(...)`                                                                                        |
| Rust      | `with_event_buffer_capacity(...)`                                                                       | `SessionEventOptions::with_buffer_capacity(...)`                                                                                        |
| Rust      | `with_canvas_handler(...)`                                                                              | `SessionCanvasOptions::with_handler(...)` on create; `ResumeSessionCanvasOptions::with_handler(...)` on resume                          |
| Rust      | `with_memory_store(...)`, `with_session_fs_provider(...)` on session options                            | The same methods on `SessionStateOptions`, configured through `.with_state(...)`                                                        |

Do not use a shallow object spread to merge two independently supplied option objects: `{ ...base, ...overrides }` replaces `base.inference` when `overrides.inference` is present. Merge the members you intend to retain.

### Defaults and presence

Omit options to keep their documented defaults; do not populate every nested object with false, null, or empty values during migration.

| Setting                                                                   | Meaning                                                                                                                                                                                     |
| ------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `events.streaming`                                                        | Defaults to false. `includeSubAgentStreamingEvents` defaults to true; it does not itself enable streaming.                                                                                  |
| `permissions.onRequest`                                                   | Omission leaves permission requests pending for the host to resolve; it does not approve or deny them. Tool filters are not permission grants.                                              |
| `tools.availableTools: []`                                                | Exposes no tools. Omission does not install an empty allowlist; exclusions still take precedence.                                                                                           |
| `agents.includedBuiltinAgents: []`                                        | Disables built-in subagents, not custom agents.                                                                                                                                             |
| Explicit `agents`, `skills`, `instructions`, or `hooks` `directories: []` | Clears that explicit SDK source contribution. Omission retains the contribution where supported; these roots neither grant general filesystem access nor depend on `enableConfigDiscovery`. |
| `enableConfigDiscovery`                                                   | Defaults to false. Explicit feature roots retain their own enablement and managed-policy rules.                                                                                             |
| `state.infiniteSessions`                                                  | Enabled by default; disable with `{ enabled: false }`, not an empty object.                                                                                                                 |
| `execution.autopilot`                                                     | Configures continuation, not session mode. New sessions default to continuation enabled and at most five continuations; omitted members preserve resident values on warm resume.            |

Re-supply nonpersisted configuration on cold resume, including provider connections/model definitions, filesystem grants, and feature settings whose reference documentation requires it. Some settings also need to be supplied on warm resume: omitting `policy.managedSettings` clears the host-injected layer, and omitting `policy.excludedManagedSettings` restores full managed-setting enforcement. Reattach a skill provider on resume or its contributed skills are removed. Re-register bearer-token callbacks as described in [Provider routing on resume](#provider-routing-on-resume).

## Restructured lifecycle RPC payloads

The wire contract changes with the SDK surface. JSON continues to use camelCase in every language, and the JSON-RPC envelope still uses `"jsonrpc": "2.0"`; SDK protocol 4 is the negotiated SDK/runtime contract version. The runtime consumes restructured options.

| RPC                      | Location of restructured options                                                                                                                                                                                 |
| ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `session.create`         | Directly in `params`, alongside root fields such as `sessionId`, `model`, `configDirectory`, and `workingDirectory`                                                                                              |
| `session.resume`         | Directly in `params`, alongside the required `sessionId` and resume controls                                                                                                                                     |
| `sessions.open`          | In `params.options`, inside the existing discriminated request such as `{ "kind": "create", "options": ... }` or `{ "kind": "resume", "sessionId": "...", "options": ... }`; generated type `SessionOpenOptions` |
| `session.options.update` | Directly in `params`, alongside `sessionId`; generated patch type `SessionUpdateOptionsParams`                                                                                                                   |

For raw RPC callers, apply the session-option mappings where the JSON names match. Also migrate these wire names, which differ from the high-level SDK names or exist only on the wire:

| Before                                                                    | After                                                                                                         |
| ------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| `configDir`                                                               | Root `configDirectory`                                                                                        |
| `disableResume`                                                           | Root `suppressResumeEvent` on resume                                                                          |
| `requestPermission`                                                       | `permissions.request`                                                                                         |
| `requestUserInput`, `requestElicitation`, `requestShellAuthentication`    | `interaction.requestUserInput`, `interaction.requestElicitation`, `interaction.requestShellAuthentication`    |
| `requestExitPlanMode`, `requestAutoModeSwitch`, `requestAutoModeRecovery` | `interaction.requestExitPlanMode`, `interaction.requestAutoModeSwitch`, `interaction.requestAutoModeRecovery` |
| `requestMcpApps`, `requestExtensions`                                     | `mcp.enableApps`, `extensions.enabled`                                                                        |
| `isExperimentalMode`                                                      | `experiments.enableExperimentalMode`                                                                          |
| Boolean `hooks`, `hasSkillProvider`, `hasMemoryStore`                     | `hooks.hasHandlers`, `skills.hasProvider`, `state.hasMemoryStore`                                             |
| `autopilotContinuation`, `verbosity`                                      | `execution.autopilotContinuation`, `inference.verbosity`                                                      |
| `webSearch`, `shell`, `maxInlineBinaryBytes`                              | `tools.webSearch`, `tools.shell`, `tools.maxInlineBinaryBytes`                                                |
| `managedMcpServers`, `envValueMode`, `allowAllMcpServerInstructions`      | `mcp.managedServers`, `mcp.envValueMode`, `mcp.allowAllServerInstructions`                                    |
| `ignoredSkillsLocations`                                                  | `skills.ignoredLocations`                                                                                     |
| `gitHubTokenProviderRegistrationId`, `enableGitHubTelemetryForwarding`    | `github.tokenProviderRegistrationId`, `github.telemetryForwarding`                                            |
| `featureFlagConfiguration`                                                | `experiments.configuration`                                                                                   |
| `authInfo`, `copilotUrl`                                                  | `github.authInfo`, `github.copilotUrl`                                                                        |
| `installedPlugins`, `sandboxConfigSource`                                 | `plugins.installed`, `permissions.sandboxSource`                                                              |
| `provider`                                                                | Split into `inference.providers` and `inference.models`; see the provider migration below                     |

Moving a field does not make it valid for every lifecycle method: keep create-only, resume-only, and mutable-update restrictions.

`sessions.open` exposes additional host settings. If you use these fields, move them within `params.options`:

| Before                                                               | After                                                                                  |
| -------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| `autoTierIsExplicit`                                                 | `github.autoTierIsExplicit`                                                            |
| `workingDirectoryContext`                                            | `filesystem.workingDirectoryContext`                                                   |
| `shellProcessFlags`, `logInteractiveShells`                          | `tools.shellProcessFlags`, `tools.logInteractiveShells`                                |
| `disabledInstructionSources`                                         | `instructions.disabledSources`                                                         |
| `enableStreaming`, `modelCapabilitiesOverrides`                      | `events.streaming`, `inference.capabilities`                                           |
| `askUserDisabled`, `runningInInteractiveMode`                        | `interaction.askUserDisabled`, `interaction.runningInInteractiveMode`                  |
| `continueOnAutoMode`, `agentContext`                                 | `execution.continueOnAutoMode`, `agents.context`                                       |
| `eventsLogDirectory`, `trajectoryFile`, `eventsLogIncludesSubagents` | `state.eventsLogDirectory`, `state.trajectoryFile`, `state.eventsLogIncludesSubagents` |
| `additionalContentExclusionPolicies`                                 | `policy.additionalContentExclusionPolicies`                                            |

For example, the flat create payload:

<!-- docs-validate: skip -->

```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "session.create",
    "params": {
        "model": "gpt-5.4",
        "reasoningEffort": "medium",
        "streaming": true
    }
}
```

becomes:

```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "session.create",
    "params": {
        "model": "gpt-5.4",
        "inference": { "reasoningEffort": "medium" },
        "events": { "streaming": true }
    }
}
```

High-level SDK options and raw RPC options are not interchangeable. SDK handlers stay in the application; the SDK registers them and sends declarations, capability flags, or registration IDs. For example, `permissions.onRequest` becomes a wire `permissions.request` capability, and `hooks.handlers` becomes `hooks.hasHandlers`. Do not serialize callback functions into raw requests or copy the high-level options object as an RPC payload.

### Update only the mutable subset

`session.options.update` is a patch, not a create/resume options object. It keeps separate `SessionUpdate...Options` types. Mutable settings include model tuning, tool filters, streaming, sandbox configuration, execution limits/autopilot, and the exposed state settings. It does not accept create-time tool handlers, provider connections/model definitions, configuration-directory discovery settings, or host-injected managed settings even when their containing option objects appear in the patch.

Its update-specific names also move:

| Before                                                                          | After                                                                                              |
| ------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `toolFilterPrecedence`, `suppressCustomAgentPrompt`, `enableReasoningSummaries` | `tools.filterPrecedence`, `agents.suppressCustomAgentPrompt`, `inference.enableReasoningSummaries` |
| `shellToolProcessFlags`, `shellInitScripts`, `shellCredentials`                 | `tools.shell.processFlags`, `tools.shell.initScripts`, `tools.shell.credentials`                   |

For example, replace a flat update's `reasoningEffort`, `streaming`, and `sessionLimits` with:

```typescript
await session.rpc.options.update({
    inference: { reasoningEffort: "medium" },
    events: { streaming: true },
    execution: { limits: null },
});
```

The bound SDK method adds `sessionId` to this patch. With raw JSON-RPC, send `{ "sessionId": "...", "execution": { "limits": null } }` as `params`; there is no `options` wrapper for this method.

Omitted option objects or members leave mutable values unchanged; an empty nested object in a patch does not reset sibling settings. Explicit clears are field-specific: `execution.limits: null` clears session limits, and `agents.includedBuiltinAgents: null` / `skills.includedBuiltinSkills: null` remove those allowlist restrictions. An empty allowlist instead allows no built-ins. `execution.autopilot: {}` makes no changes. Do not treat every nullable field or empty collection as a global reset.

## Named provider connections and model definitions

The singular session `provider` option is no longer accepted by create or resume. Replace uses of `ProviderConfig` with `NamedProviderConfig` for connections and `ProviderModelConfig` for model definitions. Some SDKs still expose the old type, but no session option accepts it. Move existing top-level `providers` and `models` under `inference` as well.

A connection owns its endpoint, wire API, transport, and credentials. A model definition references the connection by `provider` and owns model-specific limits, capabilities, and names.

Before:

<!-- docs-validate: skip -->

```typescript
const session = await client.createSession({
    model: "deployment",
    provider: {
        type: "openai",
        baseUrl: "https://models.example.com/v1",
        apiKey: process.env.MODEL_API_KEY,
    },
});
```

After, with an explicit model definition:

<!-- docs-validate: strict -->

```typescript
import type { NamedProviderConfig, ProviderModelConfig } from "@microsoft/copilot-sdk";

const providers: NamedProviderConfig[] = [
    {
        name: "gateway",
        type: "openai",
        baseUrl: "https://models.example.com/v1",
        apiKey: process.env.MODEL_API_KEY,
    },
];
const models: ProviderModelConfig[] = [
    {
        provider: "gateway",
        id: "chat",
        modelId: "gpt-4o",
        wireModel: "deployment",
    },
];

const session = await client.createSession({
    model: "gateway/chat",
    inference: { providers, models },
    github: { capi: { enabled: false } },
});
```

This example explicitly disables Copilot API (CAPI) models to keep a BYOK-only catalog. Omit that setting, or set it to true, to keep CAPI models alongside named providers. Adding a connection no longer implicitly selects a provider or disables CAPI.

Keep these identifiers distinct:

| Field             | Purpose                                                                                                                                            |
| ----------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| Connection `name` | Stable connection identifier; cannot contain `/`. Model definitions reference it through `provider`.                                               |
| Model `id`        | Provider-local identifier. Select the registered model with canonical `provider/id`, such as `gateway/chat`. Duplicate canonical IDs are rejected. |
| Model `modelId`   | Known behavior/capability base model; defaults to `id`.                                                                                            |
| Model `wireModel` | Model/deployment name sent to the provider API; defaults to `id`. Use this for remote names that must not be interpreted as routing prefixes.      |
| Model `name`      | Optional display label; defaults to the canonical selection ID.                                                                                    |

Move the former provider's `modelId`, `wireModel`, `maxPromptTokens`, `maxContextWindowTokens`, `maxOutputTokens`, and `capabilities` to each model definition that needs them. These definitions are distinct from the generated `Model` DTO returned by model-listing APIs.

### CAPI policy and pass-through aliases

`github.capi.enabled` controls Copilot catalog/auth/inference work independently of GitHub identity and tools. Setting it to false also excludes CAPI from child and auxiliary model work; it does not disable the GitHub MCP server, erase a session's GitHub identity, or disable OpenTelemetry.

| Effective CAPI setting      | `inference.defaultProvider`  | Routing                                                                                       |
| --------------------------- | ---------------------------- | --------------------------------------------------------------------------------------------- |
| Enabled (default on create) | Omitted or cleared           | CAPI catalog IDs and qualified registered BYOK models coexist.                                |
| Disabled                    | Omitted or cleared           | Use an explicitly registered BYOK model; there is no default pass-through connection.         |
| Disabled                    | A configured connection name | Unqualified aliases use that connection; registered models take precedence over pass-through. |
| Enabled                     | A connection name            | Invalid configuration; clear the default when enabling CAPI.                                  |

For a gateway that accepts arbitrary model names, a default connection needs no model definitions:

<!-- docs-validate: strict -->

```typescript
const session = await client.createSession({
    model: "deployment",
    github: { capi: { enabled: false } },
    inference: {
        providers: [
            {
                name: "gateway",
                type: "openai",
                baseUrl: "https://models.example.com/v1",
                apiKey: process.env.MODEL_API_KEY,
            },
        ],
        defaultProvider: "gateway",
    },
});
```

The selected ID is reported canonically as `gateway/deployment`. A default provider does not choose the initial model: when CAPI is disabled, supply `model` explicitly. `auto` is not an arbitrary gateway alias. With CAPI disabled, unknown provider prefixes fail rather than falling back to the default connection; use `wireModel` on a model definition for a remote model name such as `org/model`.

With CAPI enabled, a slash-qualified ID outside the configured BYOK connections is accepted only if the authenticated CAPI catalog contains it. An unknown model under a configured BYOK connection never falls back to CAPI.

Pass-through aliases use inferred model metadata and normal session processing. Register a model definition when custom limits or capabilities are needed or when the model must appear in the catalog. Use the returned canonical IDs when comparing current models, switching models, and assigning models to custom agents; do not compare only the original bare alias.

### Credentials and transport

Credential fields stay on each connection. The default provider type is `openai`, the default `wireApi` is `completions`, and the default `transport` is `http`. `websockets` requires the OpenAI Responses API. Azure `apiVersion` selects the versioned deployment route; omission uses the versionless v1 route.

Credential precedence is `bearerTokenProvider`, then `bearerToken`, then `apiKeyCommand`, then `apiKey`. Callback/command credentials are acquired before each outbound request. A callback returns the raw token without `Bearer ` and owns caching/refresh; failures fail the request rather than falling back to a static credential. SDK callbacks are registered locally, not serialized as functions. Credentials are not persisted in session history; restore them on cold resume and re-register callbacks on SDK resume as described below.

## Provider routing on resume

Resume distinguishes routing policy, registered model metadata, and live credentials. Do not infer one from another.

| Resume input                                           | Meaning                                                                                                                                                   |
| ------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Omit `github.capi.enabled`                             | Preserve the resident or persisted routing choice; older sessions without that choice use the default of CAPI enabled.                                    |
| Omit `inference.defaultProvider`                       | Preserve the existing default. An empty `inference` object does not clear it.                                                                             |
| `inference.defaultProvider: null`                      | Clear the default connection. This does not remove named connections or model definitions.                                                                |
| Re-supply `inference.providers` and `inference.models` | Re-establish the live catalog and credentials after a runtime restart. Persisted selection/routing does not reconstruct those objects.                    |
| `inference.models: []`                                 | Supply no registered model definitions. This is valid for a pass-through alias, but cannot silently turn a previously registered selection into an alias. |

As in v1, re-register bearer-token callbacks when calling SDK resume, including a warm resume while the runtime session remains resident. In TypeScript, `resumeSession` creates a new SDK handle; it does not copy the previous handle's callback registrations. Supply callbacks in `inference.providers` and include the model definitions needed to preserve the catalog when replacing provider configuration. A resident runtime registry does not restore application callbacks.

A saved registered selection must still resolve to its model definition. Resume fails if that definition is missing rather than routing its name through the default connection. Withdrawal blocks pass-through fallback for both bare and canonical IDs after restart, but explicit registration or provider catalog rediscovery can make the model available again. Canonicalizing the live selection does not rewrite historical events. A passive resume with `suppressResumeEvent` does not itself durably replace the saved selection; do not use it as a persistence operation.

To re-enable CAPI on a session that has a default connection, clear the default in the same resume or update operation and select a valid model:

<!-- docs-validate: strict -->

```typescript
import type { ResumeSessionOptions } from "@microsoft/copilot-sdk";

const options: ResumeSessionOptions = {
    model: "gpt-5.4",
    github: { capi: { enabled: true } },
    inference: { defaultProvider: null },
};
```

Pass these options to `resumeSession(sessionId, options)` and provide the required GitHub authentication. For an already running session, the same routing fields are supported by `session.rpc.options.update(...)`.

The high-level SDK representations of omitted versus explicitly cleared defaults differ:

| SDK        | Omit / preserve                                            | Clear                                                                                              |
| ---------- | ---------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| TypeScript | Leave out `defaultProvider` or use `undefined`             | `defaultProvider: null`                                                                            |
| C# / .NET  | Do not assign `SessionInferenceOptions.DefaultProvider`    | Assign `DefaultProvider = null`; the setter records presence                                       |
| Python     | Leave `"default_provider"` out of the inference dictionary | `"default_provider": None`                                                                         |
| Go         | `DefaultProvider: nil`                                     | `var cleared *string;` then `DefaultProvider: &cleared` (`**string`)                               |
| Java       | Do not call `setDefaultProvider`                           | `setDefaultProvider(null)`; `hasDefaultProvider()` distinguishes explicit null                     |
| Rust       | `default_provider: None`                                   | `Some(None)` or `.clear_default_provider()` on the inference options; a name is `Some(Some(name))` |

These rules are specific to `defaultProvider`, not all optional strings. Generated RPC DTOs have their own presence-aware representations; preserve the distinction when converting application configuration into JSON.

In Python's generated `SessionOpenInferenceOptions` and `SessionUpdateInferenceOptions`, omitted `default_provider` uses `UNSET_SENTINEL`, not `None`. Import `Unset` from `copilot.rpc` and use `isinstance(value, Unset)` when narrowing the value; `None` represents an explicit clear.

## Convenience-method startup

Go, TypeScript, and Python client convenience methods now start the client when needed, including ping, status, model listing, and session management. Concurrent callers share startup and observe its failure without retrying within the same call. Calls during shutdown fail rather than reopening the connection; a later call after shutdown or failed startup can start the client again.

Custom model-listing callbacks still work without starting a runtime. Raw RPC access does not auto-start: explicitly start the client before using it. Java's `autoStart=false` remains supported; .NET, Java, and Rust retain their existing startup policies.

Go convenience calls can replace an unexpectedly lost connection, whereas TypeScript and Python convenience calls require the dead connection to be cleaned up or explicitly restarted. Requests are not automatically retried, and existing session objects stay bound to their original connections.

## Operation failure reporting

The shared runtime now reports failed `session.abort`, `session.delete`, and `session.setForeground` operations as JSON-RPC errors instead of successful responses containing `success: false`. Convenience methods retain their void/unit/error-only signatures; callers must handle their ordinary SDK errors. Raw RPC callers should handle an RPC error instead of inspecting a failed success envelope. Successful responses and generated result types are unchanged.

Aborting an idle session still succeeds. Deleting a missing or already-deleted session remains an error. This change does not reinterpret other operations' `success: false` responses, including destroy/disconnect results and legitimate no-op or denial outcomes.

.NET now propagates the existing `RemoteRpcException` for server RPC rejections, preserving `ErrorCode` and `ErrorData`, rather than wrapping it in `IOException` with a misleading communication-error message. Catch `RemoteRpcException` directly instead of inspecting an `IOException.InnerException`; actual transport failures remain `IOException`.

## Canonical model and protocol types

Model-listing helpers and custom model-listing callbacks now use the same generated `Model` types as the RPC API instead of separate handwritten `ModelInfo` types. Update explicit return types, imports, and nested capability, billing, and policy construction to the generated types:

| SDK        | Model type                                              |
| ---------- | ------------------------------------------------------- |
| TypeScript | `Model` from `@microsoft/copilot-sdk`                   |
| Python     | `Model` from `copilot`; nested types from `copilot.rpc` |
| Go         | `copilot.Model`                                         |
| .NET       | `Microsoft.Copilot.Rpc.Model`                           |
| Java       | `com.microsoft.copilot.generated.rpc.Model`             |
| Rust       | `copilot_sdk::Model` (already generated; unchanged)     |

Generated nested names, nullability, and enum representations differ. Java models use builders; picker, policy-state, and adaptive-thinking enums become open string-value classes: compare `getValue()`. Python defaults omitted vision fields to runtime zero/empty values. See [Custom model listing](./auth/byok.md#custom-model-listing) for examples.

TypeScript's `ModelCapabilitiesOverride` and its nested override types also come from the generated contract. Do not derive overrides from the read-model capabilities: the runtime accepts only the fields defined by the override contract.

The top-level model-listing helpers remain: they provide SDK caching and custom-provider behavior rather than merely aliasing the raw RPC method. The session-listing helper and raw `sessions.list` RPC also remain distinct operations.

Python and .NET ping helpers now return the generated `PingResult`. Python callers use attributes such as `result.message`, not dictionary indexing. .NET also uses the generated `Microsoft.Copilot.Rpc.SessionListFilter`; update its namespace while retaining the `WorkingDirectory` property, which still serializes as `cwd`.

Python's `GetAuthStatusResponse` attributes and constructor keywords change from `isAuthenticated`, `authType`, and `statusMessage` to `is_authenticated`, `auth_type`, and `status_message`. Its JSON keys remain unchanged, as do the wire protocol and saved-session formats.

## .NET cancellation and generated RPC arguments

All public .NET APIs accepting `CancellationToken` put it last and give it `= default`. Remove calls to compatibility overloads that placed cancellation before additional options, and update positional calls to the current signatures. Prefer named arguments for optional generated RPC parameters, for example `await session.Rpc.Model.SwitchToAsync(modelId: "gpt-5.4", cancellationToken: cancellationToken)`.

Generated RPC APIs no longer preserve v1 positional parameter lists. Their signatures include the current schema fields rather than retaining old overloads or hiding newer fields to keep historical parameter positions. This changes SDK construction and invocation APIs, not JSON-RPC method names or wire fields.

Direct Auto-switch registration uses one `RegisterDirectAutoModeSwitchHandlerAsync(request: ..., cancellationToken: ...)` method. Remove `WithAutoModeRecovery` calls and pass cancellation by name when omitting the optional request.

## Go composable sessions and RPC requests

Use one `CreateSession(ctx, options)` and one `ResumeSession(ctx, sessionID, options)` method. The former permission-envelope, MCP-refresh, and Auto-recovery method variants are removed. Configure those features together on `CreateSessionOptions` or `ResumeSessionOptions` using `Permissions.OnRequestWithEnvelope`, `MCP.OnAuthRuntimeRefresh`, `Interaction.AutoModeRecovery`, and `Interaction.OnAutoModeSwitchRequest`; no separate recovery-options argument is needed.

Generated RPC methods with request parameters take one explicit argument, not variadic pseudo-optionals. Struct requests use `*Params` or `*Request`; union request types are passed as interface values, not pointers. Pass `nil` for an all-optional object request to use defaults; the SDK sends an empty object, or only the bound session ID. Required-input requests still need their required values. Calls that previously omitted an optional request argument must now pass `nil`.

Canonical Go types no longer hide newer fields to preserve v1 unkeyed struct literals. Use keyed literals. Auto-recovery context is on `AutoModeSwitchRequest.AutoModeRecovery`; acknowledgments are on `session.Capabilities().UI.AutoModeRecovery` when `UI` is non-nil. Ordinary event subscriptions expose recovery fields on the canonical capability event. Remove `WithAutoModeRecovery` request, capability, event, result, and subscription variants; use the ordinary types and APIs. Permission-envelope callbacks remain distinct where their semantics differ.

## Rust canonical RPC requests and results

Generated Rust RPC methods now accept complete canonical `*Request` or `*Params` structs. Replace compatibility-only `*Options` builders and `*_with_options` calls with the ordinary method and its generated request type. This RPC change does not affect SDK-owned options such as `ClientOptions` and `MessageOptions`; migrate client configuration separately as described above.

For example:

```rust
use copilot_sdk::{Error, rpc::ModelSwitchToRequest, session::Session};

async fn change_model(session: &Session) -> Result<(), Error> {
    session.rpc().model().switch_to(ModelSwitchToRequest {
        model_id: "gpt-5.4".into(),
        reasoning_effort: Some("low".into()),
        ..Default::default()
    }).await?;
    Ok(())
}
```

Canonical requests include fields previously available only through compatibility extensions, including model-switch options, session configuration directories, permission-provider scope, connector reconciliation, and settings updates. Use named fields and `..Default::default()` for optional request fields.

Auto-recovery acknowledgments and feature-flag display state now appear on the canonical generated result and event types. Remove duplicate `WithAutoModeRecovery` and `WithDisplayState` types. Replace `capabilities_changed_data_with_auto_mode_recovery()` with `capabilities_changed_data()`. Direct Auto registration uses its ordinary result type and the existing parameterized registration method when enrollment options are needed.

The ordinary `session.rpc().event_log().read(...)` now returns `EventsReadResultWithMetadata`, preserving checkpoint markers on each record. Access its event through `record.event` and its marker through `record.checkpoint`; a separate options-only method is no longer needed to retain this metadata. These changes do not add RPC operations or change their wire representations.

## Removed deprecated APIs and fields

SDK v2 also removes deprecated surfaces that were still present in v1. These removals include changes made before the connection and RPC consolidation described above.

| Removed surface                                                                                | Migration                                                                                                                                |
| ---------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| Rust `Session::get_messages()` and `Session::destroy()`                                        | Use `get_events()` and `disconnect()`, respectively.                                                                                     |
| Rust `ClientOptions::enable_remote_sessions` and `with_enable_remote_sessions(...)`            | Remove the option. The SDK no longer adds the legacy `--remote` startup flag.                                                            |
| Java `getGithubToken()` / `setGithubToken(...)` and `getGitHubToken()` / `setGitHubToken(...)` | Use `ClientGitHubOptions.getToken()` / `setToken(...)` through the client's `github` options; session tokens use `SessionGitHubOptions`. |
| Java `SystemPromptSections`                                                                    | Use `SystemMessageSections`.                                                                                                             |
| Java `PermissionRequestResultKind.DENIED_INTERACTIVELY_BY_USER`                                | Use `REJECTED`.                                                                                                                          |
| Java `PermissionRequestResultKind.DENIED_COULD_NOT_REQUEST_FROM_USER` and `DENIED_BY_RULES`    | Use `USER_NOT_AVAILABLE`, which both aliases already represented.                                                                        |
| Java `CopilotClientOptions.isAutoRestart()` and `setAutoRestart(...)`                          | Remove calls to this unused option; it did not enable automatic restarts.                                                                |
| Session open/update `shellInitProfile`                                                         | Use `tools.shell.initProfile`.                                                                                                           |
| Session open/update `enableScriptSafety`                                                       | Remove this ignored option. Shell command analysis remains enabled.                                                                      |
| `McpFilteredServer.enterpriseName`                                                             | Remove reads of this field, which was not populated.                                                                                     |
| Model billing `tokenPrices.cachePrice` and `tokenPrices.longContext.cachePrice`                | Use `cacheReadPrice` at the corresponding level.                                                                                         |
| Model billing `tokenPrices.contextMax` and `tokenPrices.longContext.contextMax`                | Use `maxPromptTokens` at the corresponding level.                                                                                        |
| `WorkflowAgentSummary.requestedModel` and `resolvedModel`                                      | Use `model.requested` and `model.resolved`.                                                                                              |
| Sandbox policy `experimental.seatbelt`                                                         | Move the configuration to the policy's top-level `seatbelt` property.                                                                    |

Contract paths in this table use JSON names; use the corresponding language's generated property names.

Event payloads no longer duplicate sub-agent attribution in `data.parentToolCallId` on `assistant.message`, `assistant.message_delta`, `assistant.usage`, `tool.execution_start`, and `tool.execution_complete`. Use the envelope's `agentId`; correlate `subagent.started` with its `data.toolCallId` when the owning task call is needed. Other events that define `parentToolCallId` retain it.

Replace subscriptions to `tool.execution_partial_result` with `tool.shell_output`. The old event supplied a cumulative replacement snapshot; the replacement supplies append-only chunks identified by `toolCallId`, `stream`, and `sequence`. These live-only events are not replayed after resume. The deprecated `"terminal"` tool-result content block and its generated types are also removed; consume `"shell_exit"` blocks for exit metadata and `tool.shell_output` for live output. See [Tool execution events](./features/streaming-events.md#tool-execution-events).

## Microsoft Copilot SDK identities

The SDK is renamed from GitHub Copilot SDK to Microsoft Copilot SDK. Update package references and imports:

| Surface                   | Before                                | After                               |
| ------------------------- | ------------------------------------- | ----------------------------------- |
| npm package               | `@github/copilot-sdk`                 | `@microsoft/copilot-sdk`            |
| npm extension entry point | `@github/copilot-sdk/extension`       | `@microsoft/copilot-sdk/extension`  |
| .NET package and assembly | `GitHub.Copilot.SDK`                  | `Microsoft.Copilot.SDK`             |
| .NET namespaces           | `GitHub.Copilot`                      | `Microsoft.Copilot`                 |
| Python distribution       | `github-copilot-sdk`                  | `microsoft-copilot-sdk`             |
| Rust crate                | `github-copilot-sdk`                  | `copilot-sdk`                       |
| Rust import               | `github_copilot_sdk`                  | `copilot_sdk`                       |
| Java dependency           | `com.github:copilot-sdk-java`         | `com.microsoft:copilot-sdk`         |
| Java parent dependency    | `com.github:copilot-sdk-java-parent`  | `com.microsoft:copilot-sdk-parent`  |
| Java runtime dependency   | `com.github:copilot-sdk-java-runtime` | `com.microsoft:copilot-sdk-runtime` |
| Java packages             | `com.github.copilot`                  | `com.microsoft.copilot`             |
| Java module               | `com.github.copilot.java`             | `com.microsoft.copilot.java`        |
| Go module                 | `github.com/github/copilot-sdk/go`    | `github.com/microsoft/copilot-sdk/go/v2` |

The npm platform packages also move from the `@github` scope to `@microsoft`. Python's `import copilot` remains unchanged. GitHub authentication and Copilot CLI artifact names do not change. The public SDK repository moves to `microsoft/copilot-sdk`.

Update Go imports, including the bundler, to `github.com/microsoft/copilot-sdk/go/v2`. Source stays in `go/`; releases pair `go/v2.X.Y[-preview.N]` with root `v2.X.Y[-preview.N]` tags at the same public commit. Both public scopes include Go. Existing V1 identities and tags remain unchanged.

Before:

<!-- docs-validate: skip -->

```typescript
import { CopilotClient } from "@github/copilot-sdk";
```

After:

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";
```

Uninstall `github-copilot-sdk` or `copilot-sdk` before installing `microsoft-copilot-sdk`; update dependency declarations. All share `copilot`. The existing .NET and Python [Microsoft Agent Framework adapters](./integrations/microsoft-agent-framework.md) depend on the old SDK and are not compatible with v2; keep those applications on v1 until compatible adapters are available.

## Python generated protocol imports

Public re-export modules separate supported application imports from the code generator's internal package layout. Depending on those public modules lets the SDK reorganize generated implementation files without requiring another import migration.

`copilot.generated` therefore moves to the private `copilot._generated` package. Replace direct generated imports with public exports from `copilot`, `copilot.rpc`, or `copilot.session_events`; do not migrate to another private import. The types are re-exported, not replaced with a different wire representation.

Generated RPC and session-event dataclasses also stop preserving v1 positional field order. Use keyword arguments, for example `CapabilitiesChangedUI(canvases=True)` instead of `CapabilitiesChangedUI(True)`, which now sets `auto_mode_recovery`. This is a source-construction change: JSON field names and saved session histories are unchanged.

Before:

<!-- docs-validate: skip -->

```python
from copilot.generated.rpc import SessionUpdateOptionsParams
from copilot.generated.session_events import SessionEvent
```

After:

```python
from copilot.rpc import SessionUpdateOptionsParams
from copilot.session_events import SessionEvent
```

## Explicit client mode

A missing mode previously selected capabilities implicitly, including coding tools that can access the host filesystem. SDK v2 therefore removes the default client mode and requires applications to declare whether they want those defaults or the explicitly enabled capabilities of `Empty`, especially for shared and multi-user hosts.

`CopilotCli` is also renamed to `CodingAgent`, without changing either mode's behavior. The new name describes the mode's behavior without suggesting that it requires a CLI process; the same mode works with in-process, subprocess, and external-server connections.

* `CodingAgent` provides CLI-like coding tools and defaults, including tools that can access the host filesystem. Use it for single-user coding assistants.
* `Empty` disables ambient tools and optional CLI behavior by default. Use it for multi-user services or shared runtimes, explicitly enabling each session's capabilities.

See [Client mode selection for multi-user deployments](./setup/multi-tenancy.md#mode-empty) for details.

Choose a mode explicitly:

| Language     | Before                     | After                                                                                   |
| ------------ | -------------------------- | --------------------------------------------------------------------------------------- |
| TypeScript   | `new CopilotClient()`      | `new CopilotClient({ mode: "coding-agent" })`                                           |
| Python       | `CopilotClient()`          | `CopilotClient(mode="coding-agent")`                                                    |
| Go           | `copilot.NewClient(nil)`   | `copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent})`              |
| .NET         | `new CopilotClient()`      | `new CopilotClient(new() { Mode = CopilotClientMode.CodingAgent })`                     |
| Java         | `new CopilotClient()`      | `new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))` |
| Rust options | `ClientOptions::default()` | `ClientOptions::new(ClientMode::CodingAgent)`                                           |

Before:

<!-- docs-validate: skip -->

```typescript
const client = new CopilotClient();
```

After:

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";
const client = new CopilotClient({ mode: "coding-agent" });
```

Replace explicit `"copilot-cli"` strings with `"coding-agent"` and the corresponding enum members with the names shown above.

Python requires the `mode` keyword argument. Go and Java reject omitted or invalid modes at client construction. .NET no longer accepts omitted or null constructor options and requires their `Mode` property. Rust's `ClientMode` and `ClientOptions` no longer implement `Default`; its public custom-stream constructors also require an explicit mode argument.

## Java generated DTO construction

Positional construction couples callers to schema field order and constructor arity: adding an optional property can otherwise force unrelated callers to append a `null` argument. Named builders avoid those placeholders and allow additive optional reference properties without changing existing builder calls, subject to the compatibility limits below.

SDK v2 removes deprecated positional constructor overloads on builder-enabled generated DTOs, retiring historical constructor signatures at the major-version boundary instead of continuing to accumulate them as schemas evolve. Replace those calls with `Type.builder().property(value).build()`. Previously compiled callers of removed overloads must be migrated and recompiled. Mandatory canonical record constructors remain available, but are not the compatibility path for ordinary construction.

For example, construct a model-switch request with named properties:

```java
import com.microsoft.copilot.AllowCopilotExperimental;
import com.microsoft.copilot.generated.rpc.SessionModelSwitchToParams;

@AllowCopilotExperimental
class ModelSwitchExample {
    static SessionModelSwitchToParams request(String sessionId) {
        return SessionModelSwitchToParams.builder()
            .sessionId(sessionId)
            .modelId("gpt-5.4")
            .reasoningEffort("low")
            .build();
    }
}
```

Records retain their mandatory canonical constructor, without deprecation, but
builders are the supported idiom for ordinary construction. Builders configure
new instances; they do not mutate existing records. This change preserves
serialization and builder validation. Extensible generated request classes keep
their nondeprecated required-input constructors and fluent setters; raw JSON
wrappers and SDK-owned options keep their existing construction APIs. See
[Named construction and compatibility](../java/README.md#named-construction-and-compatibility)
for the construction families and compatibility limits.

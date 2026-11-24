# Copilot Supercharged SDK for Rust

A Rust client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of 40 supported languages](../README.md#available-sdks)).

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
copilot-sdk-supercharged = { path = "../rust" }
tokio = { version = "1", features = ["full"] }
serde_json = "1"
```

Or if published to crates.io:

```toml
[dependencies]
copilot-sdk-supercharged = "0.1"
tokio = { version = "1", features = ["full"] }
```

## Quick Start

```rust
use copilot_sdk::*;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), CopilotError> {
    // Create and start a client
    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some("/path/to/copilot-cli".to_string()),
        ..Default::default()
    });
    client.start().await?;

    // Create a session
    let session = client.create_session(SessionConfig::default()).await?;

    // Subscribe to events
    let _sub = session.on(|event| {
        if let Some(content) = event.assistant_message_content() {
            println!("Assistant: {}", content);
        }
    }).await;

    // Send a message and wait for the response
    let response = session.send_and_wait(
        MessageOptions {
            prompt: "What is 2 + 2?".to_string(),
            attachments: None,
            mode: None,
        },
        None, // default 60s timeout
    ).await?;

    if let Some(event) = response {
        if let Some(content) = event.assistant_message_content() {
            println!("Answer: {}", content);
        }
    }

    // Clean up
    session.destroy().await?;
    client.stop().await?;
    Ok(())
}
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `CopilotClient` methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, returned as `CopilotError::JsonRpc { code: -32601, .. }` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret value is never logged. Each method takes an optional `config_dir` (the runtime's configuration directory when `None`).

```rust
let token = std::env::var("MY_SERVER_TOKEN").unwrap_or_default();
let saved = client.mcp_secrets_save("my-server-token", &token, None).await?; // mcp.secrets.save
if saved.used_keychain {
    println!("Stored in the OS keychain");
} else {
    println!("Stored in the file backend");
}

client.mcp_secrets_delete("my-server-token", None).await?; // mcp.secrets.delete
client.mcp_secrets_delete_server("my-server", None).await?; // mcp.secrets.deleteServer: every credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions (`PermissionsExtensionApproval`: `extension_name`, `plugin_name`, `capabilities`, `granted_at`, `installation_key`, `installation_label`). `extension_approvals_list` and `extension_approvals_revoke` take an optional `session_id` to use an attached session's configuration profile.

```rust
// Persist a pending extension permission request before answering it normally.
let grant = client.extension_approvals_grant(session.session_id(), &request_id).await?;
if grant.granted {
    if let Some(approval) = &grant.approval {
        println!("Approved {}", approval.extension_name);
    }
}

let list = client.extension_approvals_list(None).await?;
for approval in &list.approvals {
    client
        .extension_approvals_revoke(&approval.extension_name, &approval.installation_key, None)
        .await?;
}
```

### Ask-user image attachments

`UserInputResponse` stays text only, so existing `UserInputHandlerFn` handlers keep compiling and working unchanged. To answer with images, set `on_user_input_request_with_images` on `SessionConfig` (or `ResumeSessionConfig`) to a `UserInputImageHandlerFn` that returns a `UserInputImageResponse`. The SDK then sends `requestUserInput` for you, and this handler takes precedence over one registered with `session.register_user_input_handler`.

When the runtime accepts images in an answer, the `UserInputRequest` carries `image_attachment_constraints` (`UserInputImageAttachmentConstraints`: `allowed_mime_types`, `max_bytes_per_image`, `max_count`); it is `None` otherwise. Both kinds of handler also see `request_id` and `meta` (the opaque `_meta` object) when the runtime sends them. `UserInputImageResponse::attachments` is a `Vec<UserInputImageAttachment>` (base64 `data`, `mime_type`, optional `description` and `display_name`; serialized with `type: "image"`) and is left out of the response when empty. Image-only answers use an empty `answer` with `was_freeform: true`; the runtime enforces the limits.

```rust
let session = client.create_session(SessionConfig {
    on_user_input_request_with_images: Some(UserInputImageHandlerFn::new(
        |request: UserInputRequest, _session_id: String| async move {
            let accepts_png = request
                .image_attachment_constraints
                .as_ref()
                .is_some_and(|c| c.allowed_mime_types.iter().any(|m| m == "image/png"));
            if !accepts_png {
                return Ok(UserInputImageResponse {
                    answer: "No screenshot available".into(),
                    was_freeform: true,
                    ..Default::default()
                });
            }
            Ok(UserInputImageResponse {
                answer: String::new(),
                was_freeform: true,
                attachments: vec![UserInputImageAttachment {
                    data: screenshot_base64(), // your base64-encoded PNG bytes
                    mime_type: "image/png".into(),
                    display_name: Some("screenshot.png".into()),
                    ..Default::default()
                }],
            })
        },
    )),
    ..Default::default()
}).await?;
```

## What's New in v3.0.0

This release speaks **SDK protocol 4**, the contract of the Microsoft Copilot SDK 2.0 runtime. It requires **Copilot CLI 1.0.113 or newer**: `start()` rejects older CLIs with `CopilotError::ProtocolMismatch`. Existing options keep working. You fill `SessionConfig` and `ResumeSessionConfig` exactly as before, and the SDK converts the flat options into the grouped v4 `session.create` / `session.resume` payload right before sending (for example `streaming` becomes `events.streaming`, `request_permission` becomes `permissions.request`, `hooks: Some(true)` becomes `hooks.hasHandlers`, and `disable_resume` becomes `suppressResumeEvent`). The legacy single `provider` is translated into a named `"default"` connection with Copilot API (CAPI) models disabled, which preserves v1 BYOK behavior. A `cli_path` ending in `.js` (such as the CLI's `npm-loader.js`) is now run with `node`.

New in this release:

* **Named BYOK providers**: `providers` (`Vec<NamedProviderConfig>`), `models` (`Vec<ProviderModelConfig>`), `default_provider`, and `capi` (`CapiConfig`) on `SessionConfig` and `ResumeSessionConfig`. Select a registered model as `"<provider>/<id>"`. On resume, `default_provider: Some(None)` sends `null` to clear the default connection.
* **Idempotent tools**: `ToolDefinition::idempotent` marks a tool as safe to retry. `ToolDefinition` now implements `Default`.
* **Client turn ID**: `MessageOptions::client_turn_id` is sent as `clientTurnId` on `session.send`.
* **Client mode**: `CopilotClientOptions::mode` is `ClientMode::CodingAgent` (the default, unchanged behavior) or `ClientMode::Empty`. In empty mode every create and resume must set `available_tools` (otherwise `CopilotError::Configuration`), and safe defaults are applied under your values: experimental mode, telemetry, cross-session store, memory, on-demand instructions, repository hooks, and host Git operations off; in-memory MCP OAuth token and embedding cache storage; embedding retrieval skipped. `"copilot-cli"` parses as an alias of `"coding-agent"`.
* **Shell output events**: `event_types::TOOL_SHELL_OUTPUT` (`"tool.shell_output"`) and `SessionEvent::shell_output()`, which returns `ToolShellOutputData` (`tool_call_id`, `text`, `sequence`, `stream`). Unknown event types are still delivered unchanged.
* **Shell authentication**: `on_shell_authentication_request: Option<ShellAuthenticationHandlerFn>` answers `sudo`, `ssh`, and `git` prompts with `ShellAuthenticationResponse::Accept { value }` or `Cancel`. When the runtime cancels a prompt, the SDK drops your pending future.
* **Memory store**: `store_memory: Option<MemoryStoreHandlerFn>` receives each approved `StoredMemory` (`subject`, `fact`, `citations`, `reason`) instead of the built-in memory backends.
* **Network callbacks**: `CopilotClientOptions::network: Option<NetworkConfig>` with `ResolveProxyFn`, `AuthenticateNegotiateFn`, and `AuthenticateBasicFn`, registered through `network.setProvider` after connecting. Tokens and passwords are never logged.
* **Policy helper failure**: `CopilotError::policy_helper_failure()` returns the `PolicyHelperFailure` carried in the error data of a refused `session.create` / `session.resume`; `PolicyHelperFailure::from_data(&snapshot)` reads it from `session.get_managed_settings()`.
* **Permission mode refusal reason**: `session.set_permission_mode(mode)` returns `PermissionsSetModeResult` with an optional `reason`; `permission_mode_refusal::MANAGED_POLICY` (`"managed_policy"`) marks a managed-policy refusal.

Run a shared service in empty mode with proxy callbacks:

```rust
let client = CopilotClient::new(CopilotClientOptions {
    mode: ClientMode::Empty,
    network: Some(NetworkConfig {
        scope: Some(NetworkScope::Connection),
        resolve_proxy: Some(ResolveProxyFn::new(|request: NetworkResolveProxyRequest| async move {
            Ok(if request.url.contains(".internal.example") {
                Some("DIRECT".to_string())
            } else {
                Some("PROXY proxy.example:8080".to_string())
            })
        })),
        authenticate_basic: Some(AuthenticateBasicFn::new(
            |request: NetworkAuthenticateBasicRequest| async move {
                if request.previous_rejected {
                    return Ok(None);
                }
                Ok(Some(BasicCredentials {
                    username: "svc-proxy".into(),
                    password: std::env::var("PROXY_PASSWORD").ok(),
                }))
            },
        )),
        ..Default::default()
    }),
    ..Default::default()
});
```

Use named providers, an idempotent tool, shell authentication, and an application memory store:

```rust
let session = client.create_session(SessionConfig {
    model: Some("gateway/chat".into()),
    available_tools: Some(vec!["view".into(), "lookup_order".into()]),
    providers: Some(vec![NamedProviderConfig {
        name: "gateway".into(),
        provider_type: Some("openai".into()),
        base_url: "https://models.example.com/v1".into(),
        api_key_command: Some("print-gateway-key".into()),
        ..Default::default()
    }]),
    models: Some(vec![ProviderModelConfig {
        provider: "gateway".into(),
        id: "chat".into(),
        wire_model: Some("deployment-1".into()),
        max_output_tokens: Some(8192),
        ..Default::default()
    }]),
    capi: Some(CapiConfig { enabled: Some(false) }),
    tools: Some(vec![ToolDefinition {
        name: "lookup_order".into(),
        description: Some("Look up an order by ID".into()),
        idempotent: Some(true),
        ..Default::default()
    }]),
    on_shell_authentication_request: Some(ShellAuthenticationHandlerFn::new(
        |request: ShellAuthenticationRequest, _session_id: String| async move {
            match std::env::var("SUDO_PASSWORD") {
                Ok(value) if request.input_kind != "confirm" => {
                    Ok(ShellAuthenticationResponse::Accept { value })
                }
                _ => Ok(ShellAuthenticationResponse::Cancel),
            }
        },
    )),
    store_memory: Some(MemoryStoreHandlerFn::new(|memory: StoredMemory| async move {
        println!("remember [{}]: {}", memory.subject, memory.fact);
        Ok(())
    })),
    ..Default::default()
}).await?;
```

Stream shell output, tag a turn, and change the permission mode:

```rust
let _sub = session.on_event(event_types::TOOL_SHELL_OUTPUT, |event| {
    if let Some(chunk) = event.shell_output() {
        print!("{}", chunk.text);
    }
}).await;

session.send(MessageOptions {
    prompt: "Run the test suite".into(),
    client_turn_id: Some("turn-42".into()),
    ..Default::default()
}).await?;

let result = session.set_permission_mode("allow-all").await?;
if result.refused_by_managed_policy() {
    println!("Managed policy keeps the session in {} mode", result.mode);
}
```

Inspect a policy helper failure, and clear the default provider on resume:

```rust
if let Err(err) = client.create_session(SessionConfig::default()).await {
    if let Some(failure) = err.policy_helper_failure() {
        eprintln!("Policy helper failed ({}): {}", failure.category, failure.message);
    }
}

let resumed = client.resume_session(ResumeSessionConfig {
    session_id: "abc-123".into(),
    default_provider: Some(None), // sends `inference.defaultProvider: null`
    disable_resume: Some(true),   // sent as `suppressResumeEvent`
    ..Default::default()
}).await?;
```

### Known limitations

* The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not available. It is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process native runtime hosting is not the default connection. This SDK keeps spawning the CLI (stdio or TCP) or connecting to an external server; the existing `in_process` option is unchanged.
* Option keys this SDK sent before v3.0.0 that have no protocol 4 equivalent (`rewindEnabled`, `otlpProtocol`, `contentExclusion`, `mcpAuthHandler`) are kept unchanged at the top level of the payload for forward compatibility.

## Custom Tools

You can register custom tools that the Copilot agent can invoke during a session:

```rust
use copilot_sdk::*;
use std::sync::Arc;

// Define a tool
let tool_def = ToolDefinition {
    name: "get_weather".to_string(),
    description: Some("Get weather for a location".to_string()),
    parameters: Some(serde_json::json!({
        "type": "object",
        "properties": {
            "location": {
                "type": "string",
                "description": "City name"
            }
        },
        "required": ["location"]
    })),
    ..Default::default()
};

// Create a handler
let handler: ToolHandler = Arc::new(|args, _invocation| {
    Box::pin(async move {
        let location = args.get("location")
            .and_then(|v| v.as_str())
            .unwrap_or("Unknown");
        Ok(serde_json::json!({
            "location": location,
            "temperature": 72,
            "condition": "sunny"
        }))
    })
});

// Create session with the tool
let config = SessionConfig {
    tools: Some(vec![tool_def.clone()]),
    ..Default::default()
};
let session = client.create_session(config).await?;

// Register the handler
session.register_tool("get_weather", handler).await;
```

## Permission Handling

Register a handler to approve or deny permission requests from the agent:

```rust
use copilot_sdk::*;
use std::sync::Arc;

let perm_handler: PermissionHandlerFn = Arc::new(|request, session_id| {
    Box::pin(async move {
        println!("Permission request from session {}: {:?}", session_id, request.kind);
        // Auto-approve read operations
        if request.kind == PermissionKind::Read {
            Ok(PermissionRequestResult {
                kind: PermissionResultKind::Approved,
                rules: None,
            })
        } else {
            Ok(PermissionRequestResult {
                kind: PermissionResultKind::DeniedInteractivelyByUser,
                rules: None,
            })
        }
    })
});

session.register_permission_handler(perm_handler).await;
```

## User Input Handling

Enable the ask_user tool by registering a user input handler:

```rust
use copilot_sdk::*;
use std::sync::Arc;

let input_handler: UserInputHandlerFn = Arc::new(|request, _session_id| {
    Box::pin(async move {
        println!("Agent asks: {}", request.question);
        Ok(UserInputResponse {
            answer: "Yes, proceed.".to_string(),
            was_freeform: true,
        })
    })
});

session.register_user_input_handler(input_handler).await;
```

## Event Subscription

Subscribe to all events or specific event types:

```rust
// All events
let sub = session.on(|event| {
    println!("[{}] {}", event.event_type, event.id);
}).await;

// Specific event type
let sub = session.on_event("assistant.message", |event| {
    if let Some(content) = event.assistant_message_content() {
        println!("Message: {}", content);
    }
}).await;

// Unsubscribe explicitly (also happens on drop)
sub.unsubscribe();
```

### Event Types

| Event Type | Description |
|---|---|
| `session.start` | Session started |
| `session.resume` | Session resumed |
| `session.idle` | Session finished processing |
| `session.error` | Error occurred |
| `session.info` | Informational message |
| `session.model_change` | Model changed |
| `session.truncation` | Context truncated |
| `session.compaction_start` | Context compaction started |
| `session.compaction_complete` | Context compaction finished |
| `user.message` | User message sent |
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning chunk |
| `assistant.turn_start` | Assistant turn started |
| `assistant.turn_end` | Assistant turn ended |
| `assistant.usage` | Token usage information |
| `tool.execution_start` | Tool execution started |
| `tool.execution_complete` | Tool execution finished |
| `tool.execution_progress` | Tool execution progress |
| `abort` | Session aborted |

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```rust
let client = CopilotClient::new(CopilotClientOptions {
    session_idle_timeout_seconds: Some(300),
    ..Default::default()
});
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```rust
let opts = CopilotClientOptions {
    session_fs: Some(SessionFsConfig {
        initial_cwd: "/repo".into(),
        session_state_path: "/tmp/state".into(),
        conventions: "posix".into(),
    }),
    ..Default::default()
};
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```rust
let meta = client.get_session_metadata("session-123").await?;
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```rust
let config = SessionConfig {
    skill_directories: vec!["./skills".into()],
    disabled_skills: vec!["test-skill".into()],
    include_sub_agent_streaming_events: Some(true),
    ..Default::default()
};
```

- `skill_directories` - Paths to directories containing skill definitions
- `disabled_skills` - Skills to exclude from the session
- `include_sub_agent_streaming_events` - When `Some(true)`, receive streaming events from sub-agents

## API Reference

### CopilotClient

| Method | Description |
|---|---|
| `CopilotClient::new(options)` | Create a new client |
| `client.start()` | Start the CLI server and connect |
| `client.stop()` | Stop the server and clean up sessions |
| `client.force_stop()` | Force stop without graceful cleanup |
| `client.create_session(config)` | Create a new session |
| `client.resume_session(config)` | Resume an existing session |
| `client.delete_session(id)` | Delete a session permanently |
| `client.list_sessions()` | List all sessions |
| `client.get_last_session_id()` | Get the most recent session ID |
| `client.ping(message)` | Ping the server |
| `client.get_status()` | Get CLI version and protocol info |
| `client.get_auth_status()` | Get authentication status |
| `client.list_models()` | List available models (cached) |
| `client.clear_managed_settings_cache()` | Clear cached managed settings |
| `client.mcp_secrets_save(secret_id, value, config_dir)` | Save an MCP credential (`mcp.secrets.save`); returns `McpSecretsSaveResult` |
| `client.mcp_secrets_delete(secret_id, config_dir)` | Delete an MCP credential (`mcp.secrets.delete`) |
| `client.mcp_secrets_delete_server(server_name, config_dir)` | Delete a server's MCP credentials (`mcp.secrets.deleteServer`) |
| `client.extension_approvals_grant(session_id, request_id)` | Persist a pending plugin extension approval (`permissions.extensionApprovals.grant`) |
| `client.extension_approvals_list(session_id)` | List plugin extension approvals (`permissions.extensionApprovals.list`) |
| `client.extension_approvals_revoke(extension_name, installation_key, session_id)` | Revoke a plugin extension approval (`permissions.extensionApprovals.revoke`) |
| `client.get_state()` | Get current connection state |
| `client.mode()` | Get the client mode (`ClientMode`) |
| `client.on_lifecycle(handler)` | Subscribe to lifecycle events |

### CopilotSession

| Method | Description |
|---|---|
| `session.send(options)` | Send a message (async, non-blocking) |
| `session.send_and_wait(options, timeout)` | Send and wait for idle |
| `session.send_and_wait_structured(options, schema, timeout)` | Send with a JSON Schema and return the parsed JSON reply |
| `session.on(handler)` | Subscribe to all events |
| `session.on_event(type, handler)` | Subscribe to specific event type |
| `session.register_tool(name, handler)` | Register a tool handler |
| `session.register_permission_handler(h)` | Register permission handler |
| `session.register_user_input_handler(h)` | Register user input handler |
| `session.register_hooks_handler(h)` | Register hooks handler |
| `session.get_messages()` | Get session history |
| `session.set_auto_tier(tier)` | Change the Auto routing tier (experimental) |
| `session.get_managed_settings()` | Get managed settings for the session |
| `session.recheck_policy_helper()` | Re-run the managed-settings policy helper |
| `session.set_permission_mode(mode)` | Change the permission mode; returns `PermissionsSetModeResult` (experimental) |
| `session.destroy()` | Detach the session (state stays resumable) |
| `session.abort()` | Abort current processing |
| `session.session_id()` | Get session ID |
| `session.workspace_path()` | Get workspace path |

### CopilotClientOptions

| Field | Type | Default | Description |
|---|---|---|---|
| `cli_path` | `Option<String>` | `None` | Path to CLI executable |
| `cli_args` | `Vec<String>` | `[]` | Extra CLI arguments |
| `cwd` | `Option<String>` | `None` | Working directory |
| `port` | `u16` | `0` | TCP port (0 = random) |
| `use_stdio` | `bool` | `true` | Use stdio transport |
| `cli_url` | `Option<String>` | `None` | External server URL |
| `log_level` | `String` | `"info"` | CLI log level |
| `auto_start` | `bool` | `true` | Auto-start on first use |
| `auto_restart` | `bool` | `true` | Auto-restart on crash |
| `env` | `Option<HashMap>` | `None` | Environment variables |
| `github_token` | `Option<String>` | `None` | GitHub auth token |
| `use_logged_in_user` | `Option<bool>` | `None` | Use stored OAuth |
| `client_info` | `Option<CopilotClientInfo>` | `None` | App identity sent on the `connect` handshake |
| `mode` | `ClientMode` | `ClientMode::CodingAgent` | Session defaults policy (`CodingAgent` or `Empty`) |
| `network` | `Option<NetworkConfig>` | `None` | Proxy resolution and proxy authentication callbacks |

## Connection Modes

### stdio (default)

The client spawns the CLI process and communicates via stdin/stdout pipes.
This is the recommended mode for most use cases.

```rust
let client = CopilotClient::new(CopilotClientOptions {
    cli_path: Some("/path/to/copilot-cli".to_string()),
    use_stdio: true, // default
    ..Default::default()
});
```

### TCP

The client spawns the CLI process with a TCP listener and connects via socket.

```rust
let client = CopilotClient::new(CopilotClientOptions {
    cli_path: Some("/path/to/copilot-cli".to_string()),
    use_stdio: false,
    port: 0, // random port
    ..Default::default()
});
```

### External Server

Connect to an already-running CLI server.

```rust
let client = CopilotClient::new(CopilotClientOptions {
    cli_url: Some("localhost:3000".to_string()),
    ..Default::default()
});
```

## Protocol Version

The SDK verifies protocol compatibility with the server on startup. The current SDK protocol version is **4**, which requires Copilot CLI 1.0.113 or newer. If the server reports another version (or none), `start()` returns `CopilotError::ProtocolMismatch` with the message `SDK protocol version mismatch: SDK supports version 4, but server reports version N. Please update your SDK or server to ensure compatibility.`

## Error Handling

All fallible operations return `Result<T, CopilotError>`. Error variants include:

- `CopilotError::JsonRpc` - Server returned a JSON-RPC error
- `CopilotError::Timeout` - Request timed out
- `CopilotError::ConnectionClosed` - Connection dropped
- `CopilotError::ProtocolMismatch` - SDK/server version incompatible
- `CopilotError::SessionError` - Session-level error
- `CopilotError::NotConnected` - Client not connected
- `CopilotError::ProcessSpawn` - Failed to start CLI process

## Image Generation

Request image responses using `response_format` and `image_options`:

```rust
let response = session.send_and_wait(MessageOptions {
    prompt: "Generate a sunset over mountains".into(),
    response_format: Some(ResponseFormat::Image),
    image_options: Some(ImageOptions {
        size: Some("1024x1024".into()),
        quality: Some("hd".into()),
        style: Some("natural".into()),
    }),
    ..Default::default()
}, 60_000).await?;
```

## What's New in v2.6.0

Parity with upstream `@github/copilot-sdk` (Copilot CLI 1.0.106). Every new option is an `Option` that defaults to `None` and is left off the wire, so existing code keeps working unchanged.

- **Client info** — `CopilotClientOptions::client_info` (`CopilotClientInfo`) is declared on a `connect` handshake sent before `ping`. Older CLIs without `connect` are tolerated.
- **Auto routing tier** (experimental) — `auto_tier: Option<AutoTier>` (`Efficiency`/`Balance`/`Intelligence`/`Fast`) on `SessionConfig` and `ResumeSessionConfig`, sent as `capi.autoTier`. `enable_web_socket_responses` is now sent as `capi.enableWebSocketResponses`. To change the tier on a live session, call `session.set_auto_tier(Some(tier))`; pass `None` to go back to default routing.
- **Ask-user variant** — `ask_user_variant: Option<AskUserVariant>` (`Legacy` / `Elicitation`).
- **GitHub token provider** — `github_token_provider: Option<GitHubTokenProviderFn>` answers the runtime's `gitHubToken.getToken` callbacks. You can't set it together with `github_token`.
- **Sandbox** — `sandbox_config: Option<SandboxConfig>`, including the opt-in `allow_bypass`. With `allow_bypass`, the agent can ask to run a single command outside the sandbox; the request goes through the normal permission flow.
- **Feature flags** — `feature_flags: Option<HashMap<String, bool>>`. It's sent whenever it's `Some`, even if the map is empty.
- **Session detach** — `session.destroy()` now sends `session.detach`. On older CLIs it falls back to `session.destroy`. The session's state on disk is kept, so you can still `resume_session` it.
- **Message source** — `MessageOptions::source: Option<MessageSource>` (`MessageSource::User`, `MessageSource::System`, `MessageSource::agent("id")`).
- **Managed settings** — `client.clear_managed_settings_cache()`, `session.get_managed_settings()`, `session.recheck_policy_helper()`.
- **Model allowlist** — `allowed_models: Option<Vec<String>>`.
- **OAuth client metadata URL** — `auth_client_id_metadata_url: Option<String>`.
- **Max output tokens** — `ModelLimits::max_output_tokens`. To override it, set `"limits": { "max_output_tokens": N }` in `model_capabilities`.
- **Structured output** — `MessageOptions::response_schema` plus `session.send_and_wait_structured(prompt_or_options, schema, timeout)`.
- **Session name & directories** — `name` (create only), `agent_directories`, `hook_directories`.
- **Runtime toggles** — `interactive_user`, `excluded_managed_settings`, `enforce_managed_model_defaults`. Create-only: `trust_working_directory` (`TrustWorkingDirectory::Session` / `User`) and `refresh_custom_instructions`. Resume-only: `allow_transcript_recovery`.
- **Image generation** — `image_generation: Option<ImageGenerationConfig>` (`enabled`, `watermark`, `save_to_disk`).
- **Autopilot** — `autopilot: Option<AutopilotOptions>` (`continuation_enabled`, `max_continues`).
- **Sub-agent hooks** — `hook_types::SUBAGENT_START` / `hook_types::SUBAGENT_STOP` with `SubagentStartHookInput`/`SubagentStartHookOutput` and `SubagentStopHookInput`/`SubagentStopHookOutput`.
- **BYOK provider** — `ProviderConfig::model_provider` (`"openai"`, `"anthropic"`, `"azure_openai"`, `"ollama"`, `"lm_studio"`, `"foundry_local"`, `"llama_cpp"`) and `api_key_command`. `ProviderConfig` now implements `Default`.

Identify your application on the connection:

```rust
let client = CopilotClient::new(CopilotClientOptions {
    client_info: Some(CopilotClientInfo {
        application_name: Some("acme-portal".into()),
        application_version: Some("2.4.0".into()),
        ..Default::default()
    }),
    ..Default::default()
});
```

Combine the new session options, then adjust Auto routing on the live session:

```rust
use std::collections::HashMap;

let config = SessionConfig {
    name: Some("nightly-triage".into()),
    model: Some("auto".into()),
    auto_tier: Some(AutoTier::Balance),
    ask_user_variant: Some(AskUserVariant::Elicitation),
    allowed_models: Some(vec!["gpt-5".into(), "claude-sonnet-4.5".into()]),
    feature_flags: Some(HashMap::from([("my_flag".to_string(), true)])),
    sandbox_config: Some(SandboxConfig {
        enabled: true,
        allow_bypass: Some(true),
        ..Default::default()
    }),
    image_generation: Some(ImageGenerationConfig {
        enabled: Some(true),
        save_to_disk: Some(false),
        ..Default::default()
    }),
    autopilot: Some(AutopilotOptions {
        continuation_enabled: Some(true),
        max_continues: Some(5),
    }),
    agent_directories: Some(vec!["./agents".into()]),
    hook_directories: Some(vec!["./hooks".into()]),
    trust_working_directory: Some(TrustWorkingDirectory::Session),
    interactive_user: Some(false),
    ..Default::default()
};
let session = client.create_session(config).await?;

// `None` returns to the provider's default Auto routing.
let ack = session.set_auto_tier(Some(AutoTier::Intelligence)).await?;
println!("auto tier request: {}", ack["status"]);
```

Supply GitHub tokens per session through a provider callback:

```rust
let config = SessionConfig {
    github_token_provider: Some(GitHubTokenProviderFn::new(
        |args: GitHubTokenProviderArgs| async move {
            // args.host, args.session_id, and args.reason ("initial" | "refresh")
            let access_token = fetch_token_for_host(&args.host).await?;
            Ok(GitHubTokenProviderResult::Token {
                access_token,
                token_type: None,
                expires_in: 8 * 60 * 60,
            })
        },
    )),
    ..Default::default()
};
```

Request structured output and tag a message's source:

```rust
let reply = session
    .send_and_wait_structured(
        "List three primary colors",
        serde_json::json!({
            "type": "object",
            "properties": { "colors": { "type": "array", "items": { "type": "string" } } },
            "required": ["colors"]
        }),
        None,
    )
    .await?;
println!("{}", reply["colors"]);

session.send(MessageOptions {
    prompt: "Review complete: 2 issues found".into(),
    source: Some(MessageSource::agent("reviewer")),
    ..Default::default()
}).await?;
```

Managed settings, and resuming with transcript recovery:

```rust
client.clear_managed_settings_cache().await?;
let settings = session.get_managed_settings().await?;
let policy = session.recheck_policy_helper().await?;

let resumed = client.resume_session(ResumeSessionConfig {
    session_id: "abc-123".into(),
    allow_transcript_recovery: Some(true),
    auto_tier: Some(AutoTier::Fast),
    ..Default::default()
}).await?;
```

Handle sub-agent lifecycle hooks (set `hooks: Some(true)` on the session config):

```rust
use std::sync::Arc;

session.register_hooks_handler(Arc::new(|hook_type, input, _session_id| {
    Box::pin(async move {
        match hook_type.as_str() {
            hook_types::SUBAGENT_START => {
                let start: SubagentStartHookInput = serde_json::from_value(input)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                let output = SubagentStartHookOutput {
                    additional_context: Some(format!("You are running as {}", start.agent_name)),
                };
                Ok(serde_json::to_value(output).ok())
            }
            hook_types::SUBAGENT_STOP => {
                let stop: SubagentStopHookInput = serde_json::from_value(input)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                let output = SubagentStopHookOutput {
                    modified_response: Some(stop.response.trim().to_string()),
                    ..Default::default()
                };
                Ok(serde_json::to_value(output).ok())
            }
            _ => Ok(None),
        }
    })
})).await;
```

BYOK with a key command and a max-output-tokens override:

```rust
let config = SessionConfig {
    provider: Some(ProviderConfig {
        base_url: "http://localhost:11434/v1".into(),
        model_provider: Some("ollama".into()),
        api_key_command: Some("op read op://dev/ollama/api-key".into()),
        ..Default::default()
    }),
    model_capabilities: Some(HashMap::from([(
        "limits".to_string(),
        serde_json::json!({ "max_output_tokens": 8192 }),
    )])),
    ..Default::default()
};
```

### Parity notes

- External-tool cancellation doesn't apply to this SDK because it receives tool calls on the `tool.call` request path.
- Experimental host-integration callbacks (skill provider, installation confirmation, extension launch provider, Entra/Kerberos auth callbacks) aren't available in this SDK yet.

## Recent Features (v2.4–v2.5)

The SDK tracks the upstream `@github/copilot-sdk` v2.4–v2.5 releases. Unless noted, the items below are fields on `SessionConfig` and default to `None`, so existing code is unaffected.

### v2.5.0

- **Reasoning effort** — `reasoning_effort: Option<ReasoningEffort>` (`Low`/`Medium`/`High`/`Xhigh`).
- **Session rewind** — roll the conversation back to an earlier turn with `rewind_enabled: Option<bool>`.
- **Additional directories** — expose extra workspace roots via `additional_directories: Option<Vec<String>>`.
- **Content exclusion** — honor content-exclusion rules with `content_exclusion: Option<bool>`.
- **Tool search** — discover tools on demand via `tool_search: Option<serde_json::Value>`.
- **Disabled MCP servers** — `disabled_mcp_servers: Option<Vec<String>>`.
- **GitHub MCP tool config** — `github_mcp_tool_config: Option<serde_json::Value>`.
- **Canvas provider** — `canvas_provider: Option<serde_json::Value>`.
- **Custom agents local-only** — `custom_agents_local_only: Option<bool>`.
- **Experimental mode** — `experimental_mode: Option<bool>`.
- **Agent factory args schema** — `CustomAgentConfig::args_schema`.
- **Permission decision context** — `decision_context` on permission results.
- **Built-in plugin directories** — `CopilotClientOptions::builtin_plugin_directories`.
- **In-process FFI transport** — `CopilotClientOptions::in_process`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions::bearer_token_provider` (`BearerTokenProviderFn`).
- **MCP OAuth token handler** — `SessionConfig::on_mcp_auth_request` (`McpAuthHandlerFn`).
- **HTTP request handler** — `CopilotClientOptions::request_handler` (`CopilotRequestHandler`).
- **Session citations** — `enable_citations`.
- **Excluded built-in agents** — `excluded_builtin_agents`.
- **Session spending limits** — `session_limits` (`SessionLimitsConfig { max_ai_credits }`).
- **Session memory** — `memory` (`MemoryConfiguration`).
- **OTLP telemetry protocol** — `otlp_protocol`.
- **WebSocket transport** — `enable_web_socket_responses`.
- **Experiment assignments** — `exp_assignments`.
- **Per-message agent mode** — `MessageOptions::agent_mode` and `display_prompt`.
- **Tool defer loading** — `ToolDefer` (`Auto`/`Never`).
- **Pre-MCP-tool-call hook** — `PreMcpToolCallHookInput`.
- **GitHub attachments** — `github_attachment::GITHUB_COMMIT` / `GITHUB_REPOSITORY`.

Configure reasoning effort, rewind, extra directories, content exclusion, and tool search on one session:

```rust
let config = SessionConfig {
    reasoning_effort: Some(ReasoningEffort::High),
    rewind_enabled: Some(true),
    additional_directories: Some(vec!["../shared".into(), "/data/corpus".into()]),
    content_exclusion: Some(true),
    tool_search: Some(serde_json::json!({ "enabled": true })),
    ..Default::default()
};
let session = client.create_session(config).await?;
```

Supply bring-your-own-key bearer tokens, minted per session:

```rust
use std::sync::Arc;

let client = CopilotClient::new(CopilotClientOptions {
    bearer_token_provider: Some(BearerTokenProviderFn(Arc::new(|args: ProviderTokenArgs| {
        // Return a freshly minted bearer token for this session.
        Ok(mint_token(&args.session_id))
    }))),
    ..Default::default()
});
```

Cap spend and turn on citations and persistent memory:

```rust
let config = SessionConfig {
    session_limits: Some(SessionLimitsConfig { max_ai_credits: Some(5.0) }),
    enable_citations: Some(true),
    memory: Some(MemoryConfiguration { enabled: Some(true) }),
    ..Default::default()
};
```

Choose an agent mode and display prompt for a single turn:

```rust
session.send(MessageOptions {
    prompt: "Refactor the auth module".into(),
    agent_mode: Some("plan".into()),
    display_prompt: Some("Refactor auth (planning)".into()),
    ..Default::default()
}).await?;
```

## License

MIT - See [LICENSE](../LICENSE) for details.

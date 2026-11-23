# GitHub Copilot Supercharged SDK for C

A C11 SDK for interacting with the GitHub Copilot CLI server via JSON-RPC 2.0 over stdio. This SDK follows the same architecture and patterns as all other SDKs in this repository (see the [full list of 40 supported languages](../README.md#available-sdks)).

## Requirements

- **C11 compiler** (GCC, Clang, or MSVC)
- **CMake** 3.14 or later
- **pthreads** (POSIX threads; included on Linux/macOS, via pthreads-win32 or native on Windows with MSVC)
- **Copilot CLI** 1.0.113 or newer, installed and on PATH (or specify `cli_path`; `.js` entry points such as `npm-loader.js` run under `node`)
- **GitHub authentication** (OAuth token, `gh` CLI auth, or `GITHUB_TOKEN` environment variable)

### Dependencies (automatically fetched)

- **[cJSON](https://github.com/DaveGamble/cJSON)** - Lightweight JSON parser for C (fetched via CMake FetchContent)

## Building

```bash
mkdir build && cd build
cmake ..
cmake --build .
```

To build without examples:

```bash
cmake -DCOPILOT_SDK_BUILD_EXAMPLES=OFF ..
```

## Quick Start

```c
#include "copilot/copilot.h"
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    /* Create and start client */
    copilot_client_options_t opts = copilot_client_options_default();
    copilot_client_t *client = copilot_client_create(&opts);
    copilot_client_start(client);

    /* Create a session */
    copilot_session_t *session = NULL;
    copilot_client_create_session(client, NULL, &session);

    /* Send a message and wait for the response */
    copilot_message_options_t msg = { .prompt = "What is 2 + 2?" };
    char *response = NULL;
    copilot_error_t err = copilot_session_send_and_wait(session, &msg, 60000, &response);

    if (err == COPILOT_OK && response) {
        printf("Assistant: %s\n", response);
        free(response);
    }

    /* Clean up */
    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);
    return 0;
}
```

## What's New in v3.0.0

The SDK now speaks **SDK protocol 4** (the protocol of the Microsoft Copilot SDK 2.0 runtime) and requires **Copilot CLI 1.0.113 or newer**. Older CLIs are rejected by `copilot_client_start()` with `COPILOT_ERROR_PROTOCOL_MISMATCH`.

Existing code keeps working unchanged. You still fill the same flat `copilot_session_config_t` fields; the SDK translates them into the grouped protocol 4 `session.create` / `session.resume` payload internally (for example `streaming` is sent as `events.streaming` and `on_permission_request` as `permissions.request`). A legacy single `provider` is sent as a named provider called `default` with Copilot API models disabled, which preserves the v1 BYOK behavior.

### Named BYOK providers

Register provider connections and model definitions, then select a model as `<provider>/<id>`:

```c
copilot_named_provider_config_t providers[] = {
    { .name = "gateway", .type = "openai", .base_url = "https://models.example.com/v1",
      .api_key_command = "get-model-key" },
};
copilot_provider_model_config_t models[] = {
    { .provider = "gateway", .id = "chat", .wire_model = "org/chat-large", .max_output_tokens = 8192 },
};
copilot_capi_config_t capi = { .enabled = false, .has_enabled = true };

copilot_session_config_t config = copilot_session_config_default();
config.model = "gateway/chat";
config.providers = providers;
config.providers_count = 1;
config.models = models;
config.models_count = 1;
config.default_provider = "gateway";
config.capi = &capi;
```

On resume, set `clear_default_provider = true` (with `default_provider` left NULL) to clear the default provider.

### Idempotent tools and client turn IDs

```c
copilot_tool_t tools[] = {
    { .name = "lookup_order", .description = "Looks up an order", .handler = lookup_order,
      .idempotent = true },  /* safe to retry */
};

copilot_message_options_t msg = { .prompt = "Summarize the diff", .client_turn_id = "turn-42" };
copilot_session_send(session, &msg, NULL);
```

### Client mode

`COPILOT_CLIENT_MODE_CODING_AGENT` (the default, identical to earlier releases) or `COPILOT_CLIENT_MODE_EMPTY`. Empty mode requires `available_tools` on every create and resume, and applies minimal defaults (experimental mode, telemetry, cross-session store, memory, on-demand instructions, repository hooks, and host git operations off; in-memory OAuth and embedding storage) underneath your own values:

```c
copilot_client_options_t opts = copilot_client_options_default();
opts.mode = COPILOT_CLIENT_MODE_EMPTY;
copilot_client_t *client = copilot_client_create(&opts);

const char *available[] = { "view", NULL };
copilot_session_config_t config = copilot_session_config_default();
config.available_tools = available;  /* required in empty mode */
```

### Live shell output

`tool.shell_output` events carry parsed data in `event->shell_output`:

```c
static void on_shell_output(const copilot_session_event_t *event, void *user_data)
{
    (void)user_data;
    const copilot_shell_output_data_t *out = event->shell_output;
    fprintf(stderr, "[%s #%lld] %s", out->stream ? out->stream : COPILOT_SHELL_OUTPUT_STREAM_STDOUT,
            (long long)out->sequence, out->text);
}

copilot_session_on_type(session, COPILOT_EVENT_TOOL_SHELL_OUTPUT, on_shell_output, NULL);
```

### Shell authentication

Answer password and confirmation prompts from commands such as `sudo`, `ssh`, or `git`. The answer goes only to the program that asked:

```c
static copilot_error_t on_shell_auth(const copilot_shell_authentication_request_t *request,
                                     void *user_data,
                                     copilot_shell_authentication_response_t *out)
{
    (void)user_data;
    if (strcmp(request->program, "sudo") != 0) {
        out->action = COPILOT_SHELL_AUTH_CANCEL;
        return COPILOT_OK;
    }
    out->action = COPILOT_SHELL_AUTH_ACCEPT;
    out->value = read_secret(request->prompt);  /* heap string; the SDK wipes and frees it */
    return COPILOT_OK;
}

config.on_shell_authentication_request = on_shell_auth;
```

### Memory store

```c
static copilot_error_t store_memory(const copilot_stored_memory_t *memory, void *user_data)
{
    FILE *log = (FILE *)user_data;
    fprintf(log, "%s: %s (%s)\n", memory->subject, memory->fact, memory->citations);
    return COPILOT_OK;
}

config.on_memory_store = store_memory;
config.memory_store_user_data = memory_log;
```

### Network callbacks

Callbacks are registered with `network.setProvider` when the client starts. The SDK never logs tokens or passwords:

```c
static char *resolve_proxy(const char *url, void *user_data)
{
    (void)url;
    (void)user_data;
    return strdup("PROXY proxy.corp.example:8080; DIRECT");  /* the SDK frees it */
}

copilot_network_options_t network = {
    .scope = COPILOT_NETWORK_SCOPE_CONNECTION,
    .resolve_proxy = resolve_proxy,
};
copilot_client_options_t opts = copilot_client_options_default();
opts.network = &network;
```

`authenticate_negotiate` (Kerberos SPNEGO tokens) and `authenticate_basic` (proxy Basic credentials) work the same way.

### Policy Helper failures and permission mode refusals

```c
if (copilot_client_create_session(client, &config, &session) != COPILOT_OK) {
    copilot_policy_helper_failure_t failure;
    if (copilot_client_get_last_policy_helper_failure(client, &failure)) {
        fprintf(stderr, "Policy Helper (%s): %s\n", failure.category, failure.message);
        copilot_policy_helper_failure_free(&failure);
    }
}

copilot_permission_mode_result_t result;
if (copilot_session_set_permission_mode(session, COPILOT_PERMISSION_MODE_ALLOW_ALL, &result) == COPILOT_OK &&
    !result.success && result.reason &&
    strcmp(result.reason, COPILOT_PERMISSION_MODE_REASON_MANAGED_POLICY) == 0) {
    fprintf(stderr, "Managed policy keeps the permission mode at %s\n", result.mode);
}
copilot_permission_mode_result_free(&result);
```

`copilot_policy_helper_failure_parse()` reads the same structure from the JSON returned by `copilot_session_get_managed_settings()`. Set `config.permission_mode` to request a mode when the session is created.

### Resume without side effects

```c
copilot_session_config_t resume = copilot_session_config_default();
resume.suppress_resume_event = true;
resume.has_suppress_resume_event = true;
resume.allow_transcript_recovery = true;
resume.has_allow_transcript_recovery = true;
copilot_client_resume_session(client, session_id, &resume, &session);
```

### Also included (upstream v2.6.0 parity)

```c
/* Client identity on the connect handshake */
copilot_client_info_t info = { .application_name = "my-editor", .application_version = "1.2.0" };
opts.client_info = &info;

/* GitHub token provider (gitHubToken.getToken); mutually exclusive with config.github_token */
static copilot_error_t provide_github_token(const copilot_github_token_provider_args_t *args,
                                            void *user_data, copilot_github_token_result_t *out)
{
    (void)user_data;
    out->kind = COPILOT_GITHUB_TOKEN_RESULT_TOKEN;
    out->access_token = fetch_token_for_host(args->host);  /* heap string; the SDK frees it */
    out->expires_in = 3600;
    return COPILOT_OK;
}
config.github_token_provider = provide_github_token;

/* Session options */
config.ask_user_variant = COPILOT_ASK_USER_VARIANT_ELICITATION;
config.feature_flags_json = "{\"newPlanner\":true}";
const char *allowed[] = { "claude-sonnet-4.5", NULL };
config.allowed_models = allowed;
config.auth_client_id_metadata_url = "https://example.com/oauth/client-metadata.json";
const char *agent_dirs[] = { "./agents", NULL };
const char *hook_dirs[] = { "./hooks", NULL };
config.agent_directories = agent_dirs;
config.hook_directories = hook_dirs;
config.trust_working_directory = COPILOT_TRUST_WORKING_DIRECTORY_SESSION;  /* create only */
config.max_output_tokens = 4096;
config.has_max_output_tokens = true;
copilot_sandbox_config_t sandbox = { .enabled = true, .allow_bypass = true, .has_allow_bypass = true };
config.sandbox_config = &sandbox;
copilot_image_generation_config_t images = { .save_to_disk = true, .has_save_to_disk = true };
config.image_generation = &images;
copilot_autopilot_options_t autopilot = { .continuation_enabled = true, .has_continuation_enabled = true,
                                          .max_continues = 3, .has_max_continues = true };
config.autopilot = &autopilot;
copilot_provider_config_t provider = { .base_url = "https://models.example.com/v1",
                                       .api_key_command = "get-model-key" };

/* Sub-agent lifecycle hooks arrive through on_hook with COPILOT_HOOK_SUBAGENT_START / _STOP */

/* Message source and structured output */
copilot_message_options_t msg = { .prompt = "List three colors", .source = COPILOT_MESSAGE_SOURCE_SYSTEM };
char *json = NULL;
copilot_session_send_and_wait_structured(session, &msg,
    "{\"type\":\"array\",\"items\":{\"type\":\"string\"}}", 60000, &json);

/* Session and client RPCs */
copilot_session_set_auto_tier(session, COPILOT_AUTO_TIER_FAST, NULL);  /* session.model.switchAutoTier */
copilot_session_recheck_policy_helper(session, NULL);
copilot_client_clear_managed_settings_cache(client);
copilot_session_destroy(session);  /* session.detach: the session can be resumed later */
```

### Known limitations

* The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not implemented. It is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process native runtime hosting is not the default connection. The C SDK keeps spawning the CLI over stdio.
* Server callbacks (tools, permissions, shell authentication, memory store, network) run synchronously on the SDK's reader thread. A `shellAuthentication.cancel` therefore always arrives after its request was answered, and the SDK acknowledges it without further action.

## Architecture

```
copilot_client_create()
    |
copilot_client_start()
    |-- Spawns CLI process with --headless --no-auto-update --log-level info --stdio
    |-- Creates JSON-RPC 2.0 connection over stdin/stdout pipes
    |-- Starts background reader thread for incoming messages
    |-- Verifies protocol version via ping
    |
copilot_client_create_session()
    |-- Sends session.create RPC request
    |-- Returns opaque session handle
    |
copilot_session_send() / copilot_session_send_and_wait()
    |-- Sends session.send RPC request
    |-- Events delivered via registered callbacks
    |-- send_and_wait blocks on pthread_cond_wait until session.idle
    |
copilot_session_destroy() + copilot_session_free()
copilot_client_stop() + copilot_client_free()
```

## API Reference

### Client

| Function | Description |
|----------|-------------|
| `copilot_client_options_default()` | Returns default client options |
| `copilot_client_create(opts)` | Creates a new client |
| `copilot_client_start(client)` | Starts CLI server and connects |
| `copilot_client_stop(client)` | Stops client and kills CLI process |
| `copilot_client_free(client)` | Frees client memory |
| `copilot_client_get_state(client)` | Returns connection state |
| `copilot_client_ping(client, msg, out)` | Pings server |
| `copilot_client_list_models(client, out)` | Lists available models |
| `copilot_client_list_sessions(client, out)` | Lists server sessions |
| `copilot_client_delete_session(client, id)` | Deletes a session |
| `copilot_client_create_session(client, cfg, out)` | Creates a session |
| `copilot_client_resume_session(client, id, cfg, out)` | Resumes a session |
| `copilot_client_clear_managed_settings_cache(client)` | Clears the managed-settings cache |
| `copilot_client_get_last_policy_helper_failure(client, out)` | Policy Helper failure of the last failed create/resume |
| `copilot_policy_helper_failure_parse(json, out)` | Parses a Policy Helper failure from JSON |

### Session

| Function | Description |
|----------|-------------|
| `copilot_session_get_id(session)` | Returns session ID |
| `copilot_session_get_workspace_path(session)` | Returns workspace path |
| `copilot_session_send(session, opts, out_mid)` | Sends a message (async) |
| `copilot_session_send_and_wait(session, opts, timeout, out)` | Sends and waits for idle |
| `copilot_session_on(session, handler, data)` | Registers event handler |
| `copilot_session_on_type(session, type, handler, data)` | Registers typed event handler |
| `copilot_session_off(session, handler_id)` | Unregisters event handler |
| `copilot_session_destroy(session)` | Releases the session server-side (`session.detach`) |
| `copilot_session_free(session)` | Frees session memory |
| `copilot_session_abort(session)` | Aborts current processing |
| `copilot_session_set_auto_tier(session, tier, out)` | Switches the Auto routing tier |
| `copilot_session_set_permission_mode(session, mode, out)` | Requests a permission mode |
| `copilot_session_recheck_policy_helper(session, out)` | Re-runs the managed-settings Policy Helper |

### Tools

Define tools that the Copilot assistant can invoke:

```c
static copilot_error_t my_tool_handler(
    const copilot_tool_invocation_t *invocation,
    void *user_data,
    copilot_tool_result_t *out_result)
{
    /* Parse invocation->arguments_json, do work, fill out_result */
    out_result->text_result_for_llm = "Result text";
    out_result->result_type = COPILOT_TOOL_RESULT_SUCCESS;
    return COPILOT_OK;
}

copilot_tool_t tools[] = {
    {
        .name = "my_tool",
        .description = "Does something useful",
        .parameters_json = "{\"type\":\"object\",\"properties\":{}}",
        .handler = my_tool_handler,
        .user_data = NULL,
    },
};

copilot_session_config_t config = copilot_session_config_default();
config.tools = tools;
config.tools_count = 1;
```

### Permission Handling

```c
static copilot_error_t on_permission(
    const copilot_permission_request_t *request,
    const char *session_id,
    void *user_data,
    copilot_permission_result_t *out_result)
{
    /* Decide based on request->kind: "shell", "write", "read", etc. */
    out_result->kind = COPILOT_PERMISSION_APPROVED;
    return COPILOT_OK;
}

config.on_permission_request = on_permission;
```

### Event Handling

```c
static void on_event(const copilot_session_event_t *event, void *user_data)
{
    if (strcmp(event->type, "assistant.message") == 0) {
        printf("Assistant: %s\n", event->content);
    }
}

int handler_id = copilot_session_on(session, on_event, NULL);
/* ... */
copilot_session_off(session, handler_id);
```

### Streaming

Enable streaming to receive incremental content deltas:

```c
copilot_session_config_t config = copilot_session_config_default();
config.streaming = true;

/* In your event handler: */
if (strcmp(event->type, "assistant.message_delta") == 0) {
    printf("%s", event->delta_content);  /* Print incrementally */
    fflush(stdout);
}
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```c
copilot_client_options_t opts = copilot_client_options_default();
opts.session_idle_timeout_seconds = 300;
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```c
copilot_client_options_t opts = copilot_client_options_default();
opts.session_fs.initial_cwd = "/repo";
opts.session_fs.session_state_path = "/state";
opts.session_fs.conventions = "posix";
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```c
copilot_session_metadata_t *meta = copilot_get_session_metadata(client, "session-123");
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```c
copilot_session_config_t config = copilot_session_config_default();
const char *skills[] = {"./skills"};
config.skill_directories = skills;
config.skill_directory_count = 1;
const char *disabled[] = {"test-skill"};
config.disabled_skills = disabled;
config.disabled_skill_count = 1;
config.include_sub_agent_streaming_events = true;
```

- `skill_directories` - array of directories containing skill definitions to register with the session
- `disabled_skills` - array of skill names to explicitly disable
- `include_sub_agent_streaming_events` - when true, surfaces streaming events from sub-agents in the event stream

### Custom Provider (BYOK)

```c
copilot_provider_config_t provider = {
    .type = "openai",
    .base_url = "http://localhost:11434/v1",
    .api_key = NULL,  /* Ollama doesn't need a key */
};

copilot_session_config_t config = copilot_session_config_default();
config.model = "llama3";
config.provider = &provider;
```

## Protocol

The SDK communicates with the Copilot CLI server using:

- **Transport**: stdin/stdout pipes (stdio mode)
- **Protocol**: JSON-RPC 2.0
- **Framing**: Content-Length headers (same as LSP)
- **Threading**: Background pthread for reading incoming messages

### Message Flow

```
SDK                             CLI Server
 |                                   |
 |--- session.create --------------->|
 |<-- {sessionId} ------------------|
 |                                   |
 |--- session.send ----------------->|
 |<-- {messageId} ------------------|
 |                                   |
 |<-- session.event (tool.executing)|
 |<-- tool.call --------------------|
 |--- {result} -------------------->|
 |<-- session.event (tool.executed) |
 |<-- session.event (assistant.message)|
 |<-- session.event (session.idle)  |
 |                                   |
 |--- session.detach --------------->|
```

## Memory Management

The SDK uses a create/destroy/free pattern:

- `*_create()` allocates and initializes
- `*_destroy()` releases server-side resources
- `*_free()` releases client-side memory
- Always call `destroy` before `free`
- Strings returned via output parameters must be freed with `free()`
- List results (models, sessions) must be freed with their respective `*_free()` functions

## Thread Safety

- The JSON-RPC client uses a background reader thread (pthread)
- Event handlers are called from the reader thread context
- `send_and_wait` uses pthread_cond_wait for blocking synchronization
- The session and client structures use mutexes for handler registration
- Tool handlers, permission handlers, and hook handlers are called synchronously from the reader thread; avoid blocking for extended periods

## Error Handling

All functions that can fail return `copilot_error_t`. Use `copilot_error_string()` for human-readable descriptions:

```c
copilot_error_t err = copilot_client_start(client);
if (err != COPILOT_OK) {
    fprintf(stderr, "Error: %s\n", copilot_error_string(err));
}
```

## Image Generation

Request image responses using `response_format` and `image_options`:

```c
copilot_image_options_t img_opts = { .size = "1024x1024", .quality = "hd", .style = "natural" };
copilot_message_options_t opts = {
    .prompt = "Generate a sunset over mountains",
    .response_format = COPILOT_RESPONSE_FORMAT_IMAGE,
    .image_options = &img_opts,
};
copilot_session_send_and_wait(session, &opts, 60000, &content);
```

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk` v2.4–v2.5. Unless noted, fields live on `copilot_session_config_t`; boolean options pair with a `has_*` flag so a zero-initialized config stays unset.

### v2.5.0

- **Reasoning effort** — `config.reasoning_effort` (`"low"`/`"medium"`/`"high"`/`"xhigh"`).
- **Session rewind** — `config.rewind_enabled` (+ `has_rewind_enabled`).
- **Additional directories** — `config.additional_directories` (NULL-terminated).
- **Content exclusion** — `config.content_exclusion` (+ `has_content_exclusion`).
- **Tool search** — `config.tool_search_json`.
- **Disabled MCP servers** — `config.disabled_mcp_servers` (NULL-terminated).
- **GitHub MCP tool config** — `config.github_mcp_tool_config_json`.
- **Canvas provider** — `config.canvas_provider_json`.
- **Custom agents local-only** — `config.custom_agents_local_only`.
- **Experimental mode** — `config.experimental_mode`.
- **Agent factory args schema** — `config.args_schema_json`.
- **Permission decision context** — `decision_context_json` on the permission result.
- **Built-in plugin directories** — `copilot_client_options_t::builtin_plugin_directories`.
- **In-process FFI transport** — `copilot_client_options_t::in_process`.

### v2.4.0

- **BYOK bearer token provider** — `copilot_client_options_t::bearer_token_provider` (`copilot_bearer_token_provider_fn`).
- **MCP OAuth token handler** — `config.on_mcp_auth_request` (`copilot_mcp_auth_handler_fn`).
- **HTTP request handler** — `copilot_client_options_t::request_handler` (`copilot_request_handler_fn`).
- **Session citations** — `config.enable_citations`.
- **Excluded built-in agents** — `config.excluded_builtin_agents`.
- **Session spending limits** — `config.session_limits` (`copilot_session_limits_config_t::max_ai_credits`).
- **Session memory** — `config.memory` (`copilot_memory_config_t`).
- **OTLP telemetry protocol** — `config.otlp_protocol`.
- **WebSocket transport** — `config.enable_web_socket_responses`.
- **Experiment assignments** — `config.exp_assignments_json`.
- **Per-message agent mode** — `copilot_message_options_t::agent_mode` and `display_prompt`.
- **Hook identifiers** — `COPILOT_HOOK_POST_TOOL_USE`, `COPILOT_HOOK_PRE_MCP_TOOL_CALL`, `COPILOT_HOOK_USER_PROMPT_TRANSFORMED`.
- **Tool defer loading** — `COPILOT_TOOL_DEFER_AUTO` / `COPILOT_TOOL_DEFER_NEVER`.
- **GitHub attachments** — `COPILOT_GITHUB_COMMIT` / `COPILOT_GITHUB_REPOSITORY`.

Configure reasoning effort, rewind, extra directories, content exclusion, and tool search:

```c
copilot_session_config_t config = copilot_session_config_default();
config.reasoning_effort = "high";
config.rewind_enabled = true;
config.has_rewind_enabled = true;
const char *dirs[] = { "../shared", "/data/corpus", NULL };
config.additional_directories = dirs;
config.content_exclusion = true;
config.has_content_exclusion = true;
config.tool_search_json = "{\"enabled\":true}";
```

Supply bring-your-own-key bearer tokens, minted per session:

```c
static char *provide_token(const copilot_provider_token_args_t *args, void *user_data)
{
    (void)user_data;
    return mint_token_for(args->session_id);  /* newly-allocated; the SDK frees it */
}

copilot_client_options_t opts = copilot_client_options_default();
opts.bearer_token_provider = provide_token;
```

Cap spend and turn on citations:

```c
copilot_session_limits_config_t limits = { .max_ai_credits = 5.0, .has_max_ai_credits = true };
copilot_session_config_t config = copilot_session_config_default();
config.session_limits = &limits;
config.enable_citations = true;
config.has_enable_citations = true;
```

Choose an agent mode and display prompt for a single turn:

```c
copilot_message_options_t opts = {
    .prompt = "Refactor the auth module",
    .agent_mode = "plan",
    .display_prompt = "Refactor auth (planning)",
};
```

## License

See the [LICENSE](../LICENSE) file in the repository root.

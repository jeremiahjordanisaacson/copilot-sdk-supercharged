# GitHub Copilot Supercharged SDK for C

A C11 SDK for interacting with the GitHub Copilot CLI server via JSON-RPC 2.0 over stdio. This SDK follows the same architecture and patterns as all other SDKs in this repository (see the [full list of 40 supported languages](../README.md#available-sdks)).

## Requirements

- **C11 compiler** (GCC, Clang, or MSVC)
- **CMake** 3.14 or later
- **pthreads** (POSIX threads; included on Linux/macOS, via pthreads-win32 or native on Windows with MSVC)
- **Copilot CLI** installed and on PATH (or specify `cli_path`)
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
| `copilot_session_destroy(session)` | Destroys session (server-side) |
| `copilot_session_free(session)` | Frees session memory |
| `copilot_session_abort(session)` | Aborts current processing |

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
 |--- session.destroy -------------->|
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

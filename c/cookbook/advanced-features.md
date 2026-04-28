# Advanced v2.0 Features in C

Recipes for using advanced v2.0 features of the GitHub Copilot SDK in C, including per-session auth, SessionFs, commands, system prompts, skills, streaming, and more.

## Scenario

Your application needs fine-grained control over authentication, filesystem access, prompt customization, agent skills, and other advanced capabilities introduced in SDK v2.0.

## 1. Per-Session Authentication

Supply a GitHub token per session instead of globally.

```c
#include <copilot/client.h>
#include <stdio.h>

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;
    copilot_client_create(&opts, &client);
    copilot_start(client);

    copilot_session_config config = {0};
    config.github_token = "ghu_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";

    copilot_session *session = NULL;
    copilot_create_session(client, &config, &session);

    copilot_response *response = NULL;
    copilot_send_options send_opts = {
        .message = "Who am I authenticated as?"
    };
    if (copilot_send_and_wait(session, &send_opts, &response) == COPILOT_OK) {
        printf("%s\n", copilot_response_message(response));
        copilot_free_response(response);
    }

    copilot_free_session(session);
    copilot_stop(client);
    copilot_free_client(client);
    return 0;
}
```

## 2. Session Idle Timeout

Automatically expire sessions after a period of inactivity.

```c
copilot_session_config config = {0};
config.session_idle_timeout_seconds = 600;  /* 10-minute idle timeout */

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);

/* Session will be cleaned up automatically if idle for 10 minutes */
copilot_send_options send_opts = {
    .message = "This session expires after 10 minutes of inactivity."
};
copilot_response *response = NULL;
copilot_send_and_wait(session, &send_opts, &response);
```

## 3. SessionFs (Session Filesystem)

Configure a filesystem provider with the 10 required I/O operations.

```c
#include <copilot/client.h>
#include <copilot/session_fs.h>
#include <string.h>
#include <stdlib.h>

/* Implement the 10 required I/O operations */
int fs_read_file(const char *path, copilot_fs_result *result, void *userdata) {
    result->content = strdup("file content here");
    return COPILOT_OK;
}

int fs_write_file(const char *path, const char *content,
                  copilot_fs_result *result, void *userdata) {
    result->success = 1;
    return COPILOT_OK;
}

int fs_delete_file(const char *path, copilot_fs_result *result,
                   void *userdata) {
    result->success = 1;
    return COPILOT_OK;
}

int fs_list_directory(const char *path, copilot_fs_result *result,
                      void *userdata) {
    result->entries = (const char *[]){"file1.txt", "file2.txt", NULL};
    result->entry_count = 2;
    return COPILOT_OK;
}

int fs_create_directory(const char *path, copilot_fs_result *result,
                        void *userdata) {
    result->success = 1;
    return COPILOT_OK;
}

int fs_stat(const char *path, copilot_fs_result *result, void *userdata) {
    result->size = 1024;
    result->is_directory = 0;
    return COPILOT_OK;
}

int fs_rename(const char *old_path, const char *new_path,
              copilot_fs_result *result, void *userdata) {
    result->success = 1;
    return COPILOT_OK;
}

int fs_copy(const char *src, const char *dst,
            copilot_fs_result *result, void *userdata) {
    result->success = 1;
    return COPILOT_OK;
}

int fs_exists(const char *path, copilot_fs_result *result, void *userdata) {
    result->exists = 1;
    return COPILOT_OK;
}

int fs_watch(const char *path, copilot_fs_watch_cb callback,
             copilot_fs_result *result, void *userdata) {
    result->success = 1;
    return COPILOT_OK;
}

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;
    copilot_client_create(&opts, &client);
    copilot_start(client);

    /* Wire up all 10 operations */
    copilot_session_fs_provider provider = {
        .read_file = fs_read_file,
        .write_file = fs_write_file,
        .delete_file = fs_delete_file,
        .list_directory = fs_list_directory,
        .create_directory = fs_create_directory,
        .stat = fs_stat,
        .rename = fs_rename,
        .copy = fs_copy,
        .exists = fs_exists,
        .watch = fs_watch,
        .userdata = NULL,
    };

    copilot_session_config config = {0};
    config.session_fs.enabled = 1;
    config.session_fs.root_path = "/workspace/project";
    config.session_fs.provider = &provider;

    copilot_session *session = NULL;
    copilot_create_session(client, &config, &session);

    copilot_send_options send_opts = {.message = "List the workspace files."};
    copilot_response *response = NULL;
    copilot_send_and_wait(session, &send_opts, &response);
    if (response) {
        printf("%s\n", copilot_response_message(response));
        copilot_free_response(response);
    }

    copilot_free_session(session);
    copilot_stop(client);
    copilot_free_client(client);
    return 0;
}
```

## 4. Commands and UI Elicitation

Register slash commands and handle elicitation requests from the agent.

```c
#include <copilot/client.h>
#include <stdio.h>

/* Elicitation callback */
int elicitation_handler(const copilot_elicitation_request *req,
                        copilot_elicitation_response *resp,
                        void *userdata) {
    printf("Agent asks: %s\n", req->message);
    if (req->type == COPILOT_ELICITATION_CONFIRMATION) {
        resp->confirmed = 1;
    } else {
        resp->text = strdup("user-provided-value");
    }
    return COPILOT_OK;
}

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;
    copilot_client_create(&opts, &client);
    copilot_start(client);

    /* Define commands */
    copilot_command commands[] = {
        {.name = "/deploy", .description = "Deploy the current project"},
        {.name = "/test",   .description = "Run the test suite"},
        {.name = "/status", .description = "Show project status"},
    };

    copilot_session_config config = {0};
    config.commands = commands;
    config.command_count = 3;
    config.elicitation_handler = elicitation_handler;
    config.elicitation_userdata = NULL;

    copilot_session *session = NULL;
    copilot_create_session(client, &config, &session);

    copilot_send_options send_opts = {.message = "/deploy to staging"};
    copilot_response *response = NULL;
    copilot_send_and_wait(session, &send_opts, &response);
    if (response) {
        printf("%s\n", copilot_response_message(response));
        copilot_free_response(response);
    }

    copilot_free_session(session);
    copilot_stop(client);
    copilot_free_client(client);
    return 0;
}
```

## 5. System Prompt Customization

Use replace or customize modes with structured sections.

```c
/* Mode 1: Replace the entire system prompt */
copilot_session_config replace_config = {0};
replace_config.system_prompt_mode = COPILOT_PROMPT_REPLACE;
replace_config.system_prompt = "You are a C build system expert. Only discuss Make and CMake.";

copilot_session *session1 = NULL;
copilot_create_session(client, &replace_config, &session1);

/* Mode 2: Customize with structured sections */
copilot_prompt_section sections[] = {
    {.id = "role",        .content = "You are a senior C developer."},
    {.id = "constraints", .content = "Always use C11 features. Prefer fixed-size buffers."},
    {.id = "output",      .content = "Include error handling in all examples."},
};

copilot_session_config custom_config = {0};
custom_config.system_prompt_mode = COPILOT_PROMPT_CUSTOMIZE;
custom_config.prompt_sections = sections;
custom_config.prompt_section_count = 3;

copilot_session *session2 = NULL;
copilot_create_session(client, &custom_config, &session2);
```

## 6. Per-Agent Skills

Configure skill directories and disable specific skills.

```c
const char *skill_dirs[] = {
    "/home/user/.copilot/skills",
    "/project/.copilot/skills",
};

const char *disabled[] = {"web-search", "code-execution"};

copilot_session_config config = {0};
config.skill_directories = skill_dirs;
config.skill_directory_count = 2;
config.disabled_skills = disabled;
config.disabled_skill_count = 2;

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);
```

## 7. Per-Agent Tool Visibility

Hide specific tools from certain agents using excluded_tools.

```c
/* Create a read-only agent that cannot modify files */
const char *excluded[] = {
    "write_file", "delete_file", "execute_command", "create_directory"
};

copilot_session_config config = {0};
config.system_prompt = "You are a code reviewer. Analyze but do not modify.";
config.excluded_tools = excluded;
config.excluded_tool_count = 4;

copilot_session *reviewer = NULL;
copilot_create_session(client, &config, &reviewer);
```

## 8. Runtime Request Headers

Attach custom headers to individual requests.

```c
copilot_header headers[] = {
    {.name = "X-Request-Id",  .value = "req-abc-123"},
    {.name = "X-Trace-Id",    .value = "trace-xyz-789"},
    {.name = "Authorization",  .value = "Bearer custom-token-here"},
};

copilot_send_options send_opts = {
    .message = "Summarize the latest deploy logs.",
    .request_headers = headers,
    .request_header_count = 3,
};

copilot_response *response = NULL;
copilot_send_and_wait(session, &send_opts, &response);
```

## 9. Model Capabilities Override

Override model capabilities for a session.

```c
copilot_model_capabilities caps = {
    .streaming = 1,
    .tool_calling = 1,
    .vision = 0,
    .max_tokens = 8192,
    .context_window = 128000,
};

copilot_session_config config = {0};
config.model_capabilities = &caps;

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);
```

## 10. Config Discovery

Enable automatic discovery of project-level configuration.

```c
copilot_session_config config = {0};
config.enable_config_discovery = 1;

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);

/*
 * The SDK will automatically discover and load:
 *   .copilot/config.yml
 *   .copilot/prompts/*.md
 *   .copilot/skills/
 * from the project root and parent directories.
 */
copilot_send_options send_opts = {
    .message = "What project configuration did you discover?"
};
copilot_response *response = NULL;
copilot_send_and_wait(session, &send_opts, &response);
```

## 11. Sub-Agent Streaming Events

Receive streaming events from sub-agents during orchestration.

```c
#include <copilot/client.h>
#include <stdio.h>

void on_event(const copilot_session_event *event, void *userdata) {
    if (strcmp(event->type, "assistant.message_delta") == 0) {
        printf("[delta] %s", event->content);
        fflush(stdout);
    } else if (strcmp(event->type, "sub_agent.message_delta") == 0) {
        printf("[sub-agent:%s] %s", event->agent_id, event->content);
        fflush(stdout);
    }
}

/* Enable sub-agent streaming */
copilot_session_config config = {0};
config.include_sub_agent_streaming_events = 1;

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);

copilot_set_event_handler(session, on_event, NULL);

copilot_send_options send_opts = {
    .message = "Coordinate the planner and coder agents."
};
copilot_send(session, &send_opts);
```

## 12. Session Metadata

Retrieve metadata about the current session.

```c
copilot_session_metadata metadata = {0};
if (copilot_get_session_metadata(session, &metadata) == COPILOT_OK) {
    printf("Session ID:  %s\n", metadata.session_id);
    printf("Created at:  %s\n", metadata.created_at);
    printf("Turn count:  %d\n", metadata.turn_count);
    printf("Model:       %s\n", metadata.model);
    printf("Token usage: %d\n", metadata.total_tokens);
    copilot_free_session_metadata(&metadata);
}
```

## 13. MCP Server Configuration

Configure Model Context Protocol servers using stdio or HTTP transports.

```c
/* Stdio transport: launch a local MCP server process */
copilot_mcp_server stdio_server = {
    .name = "filesystem",
    .transport = COPILOT_MCP_STDIO,
    .command = "npx",
    .args = (const char *[]){"-y", "@modelcontextprotocol/server-filesystem",
                              "/workspace", NULL},
    .arg_count = 3,
};

/* HTTP transport: connect to a remote MCP server */
copilot_header http_headers[] = {
    {.name = "Authorization", .value = "Bearer token123"},
};
copilot_mcp_server http_server = {
    .name = "remote-db",
    .transport = COPILOT_MCP_HTTP,
    .url = "https://mcp.example.com/db",
    .headers = http_headers,
    .header_count = 1,
};

copilot_mcp_server *servers[] = {&stdio_server, &http_server};

copilot_session_config config = {0};
config.mcp_servers = servers;
config.mcp_server_count = 2;

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);
```

## 14. Image Generation

Configure the response format to request image generation.

```c
#include <copilot/client.h>
#include <stdio.h>

copilot_response_format fmt = {
    .type = COPILOT_RESPONSE_IMAGE_GENERATION,
    .image_size = "1024x1024",
    .image_quality = "high",
};

copilot_session_config config = {0};
config.response_format = &fmt;

copilot_session *session = NULL;
copilot_create_session(client, &config, &session);

copilot_send_options send_opts = {
    .message = "Generate an image of a futuristic cityscape at sunset."
};
copilot_response *response = NULL;
copilot_send_and_wait(session, &send_opts, &response);

/* Save the generated image */
if (copilot_response_image_size(response) > 0) {
    FILE *fp = fopen("cityscape.png", "wb");
    fwrite(copilot_response_image_data(response),
           1, copilot_response_image_size(response), fp);
    fclose(fp);
    printf("Image saved to cityscape.png\n");
}

copilot_free_response(response);
```

## Best Practices

- **Scope tokens tightly**: Use per-session `github_token` with minimal scopes for multi-tenant apps.
- **Free all resources**: Always call the matching `copilot_free_*` function for every allocated resource.
- **Use the userdata pointer**: Pass context structs through `void *userdata` for callbacks and handlers.
- **Combine features**: Per-agent skills, tool visibility, and system prompts work together to create specialized agents.
- **Set idle timeouts in server apps**: Use `session_idle_timeout_seconds` to prevent resource leaks from abandoned sessions.
- **Check return codes**: Every `copilot_*` function returns an error code; always check for `COPILOT_OK`.

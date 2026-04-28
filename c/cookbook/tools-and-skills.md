# Tools and Skills in C

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in C.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```c
#include <copilot/client.h>
#include <stdio.h>
#include <time.h>
#include <string.h>

/* Tool callback: returns the current time */
int get_time_handler(const copilot_tool_context *ctx,
                     copilot_tool_result *result,
                     void *userdata) {
    (void)ctx;
    (void)userdata;

    time_t now = time(NULL);
    char buf[64];
    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", localtime(&now));

    result->content = strdup(buf);
    return COPILOT_OK;
}
```

## Defining a Tool with Parameters

```c
#include <copilot/client.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Tool callback: reads a file */
int read_file_handler(const copilot_tool_context *ctx,
                      copilot_tool_result *result,
                      void *userdata) {
    (void)userdata;

    const char *path = copilot_tool_get_param(ctx, "path");
    if (!path || path[0] == '\0') {
        result->error = strdup("Missing required parameter: path");
        return COPILOT_OK;
    }

    FILE *fp = fopen(path, "r");
    if (!fp) {
        char err[256];
        snprintf(err, sizeof(err), "Could not open file: %s", path);
        result->error = strdup(err);
        return COPILOT_OK;
    }

    fseek(fp, 0, SEEK_END);
    long len = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *content = malloc(len + 1);
    fread(content, 1, len, fp);
    content[len] = '\0';
    fclose(fp);

    result->content = content;
    return COPILOT_OK;
}
```

## Registering Tools with the Client

```c
#include <copilot/client.h>
#include <stdio.h>

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;

    if (copilot_client_create(&opts, &client) != COPILOT_OK) return 1;

    /* Register tools before starting */
    copilot_define_tool(client, "get_current_time",
                        "Returns the current date and time",
                        get_time_handler, NULL);

    copilot_define_tool(client, "read_file",
                        "Reads the contents of a file given its path",
                        read_file_handler, NULL);

    if (copilot_start(client) != COPILOT_OK) {
        copilot_free_client(client);
        return 1;
    }

    copilot_session_config config = {0};
    config.system_prompt = "You have access to file reading and time tools.";
    copilot_session *session = NULL;
    copilot_create_session(client, &config, &session);

    copilot_response *response = NULL;
    copilot_send_options send_opts = {
        .message = "What time is it right now?"
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

## Defining Multiple Related Tools with Shared State (Skill Pattern)

```c
#include <copilot/client.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ENTRIES 64
#define MAX_KEY_LEN 128
#define MAX_VAL_LEN 512

typedef struct {
    char key[MAX_KEY_LEN];
    char value[MAX_VAL_LEN];
} kv_entry;

typedef struct {
    kv_entry entries[MAX_ENTRIES];
    int count;
} kv_store;

int kv_get_handler(const copilot_tool_context *ctx,
                   copilot_tool_result *result, void *userdata) {
    kv_store *store = (kv_store *)userdata;
    const char *key = copilot_tool_get_param(ctx, "key");

    for (int i = 0; i < store->count; i++) {
        if (strcmp(store->entries[i].key, key) == 0) {
            result->content = strdup(store->entries[i].value);
            return COPILOT_OK;
        }
    }
    result->content = strdup("Key not found");
    return COPILOT_OK;
}

int kv_set_handler(const copilot_tool_context *ctx,
                   copilot_tool_result *result, void *userdata) {
    kv_store *store = (kv_store *)userdata;
    const char *key = copilot_tool_get_param(ctx, "key");
    const char *value = copilot_tool_get_param(ctx, "value");

    if (store->count >= MAX_ENTRIES) {
        result->error = strdup("Store is full");
        return COPILOT_OK;
    }

    strncpy(store->entries[store->count].key, key, MAX_KEY_LEN - 1);
    strncpy(store->entries[store->count].value, value, MAX_VAL_LEN - 1);
    store->count++;

    result->content = strdup("Stored successfully");
    return COPILOT_OK;
}

/* Register the skill */
void register_kv_skill(copilot_client *client, kv_store *store) {
    copilot_define_tool(client, "kv_get", "Get a value by key",
                        kv_get_handler, store);
    copilot_define_tool(client, "kv_set", "Set a key-value pair",
                        kv_set_handler, store);
}
```

## Sub-Agent Orchestration

```c
#include <copilot/client.h>
#include <stdio.h>

void orchestrate(copilot_client *client) {
    /* Create specialized sessions as sub-agents */
    copilot_session_config planner_cfg = {
        .system_prompt = "You are a planner. Break tasks into steps."
    };
    copilot_session_config coder_cfg = {
        .system_prompt = "You are a coder. Write C code for given tasks."
    };

    copilot_session *planner = NULL, *coder = NULL;
    copilot_create_session(client, &planner_cfg, &planner);
    copilot_create_session(client, &coder_cfg, &coder);

    /* Step 1: Plan */
    copilot_response *plan_resp = NULL;
    copilot_send_options plan_opts = {.message = "Plan a hash table in C"};
    copilot_send_and_wait(planner, &plan_opts, &plan_resp);
    printf("Plan: %s\n", copilot_response_message(plan_resp));

    /* Step 2: Code using the plan */
    char code_msg[4096];
    snprintf(code_msg, sizeof(code_msg), "Implement this plan:\n%s",
             copilot_response_message(plan_resp));

    copilot_response *code_resp = NULL;
    copilot_send_options code_opts = {.message = code_msg};
    copilot_send_and_wait(coder, &code_opts, &code_resp);
    printf("Code: %s\n", copilot_response_message(code_resp));

    copilot_free_response(plan_resp);
    copilot_free_response(code_resp);
    copilot_free_session(planner);
    copilot_free_session(coder);
}
```

## Best Practices

- **Use the userdata pointer for shared state**: Pass a struct pointer so related tools can share data.
- **Return errors via ToolResult, not return codes**: Set `result->error` instead of returning error codes from handlers.
- **Use `strdup` for result strings**: The SDK frees result content, so allocate with `strdup` or `malloc`.
- **Group related tools into a registration function**: Create a `register_*_skill` function for each skill.
- **Validate parameters early**: Check for NULL or empty parameters at the start of each handler.
- **Use separate sessions for sub-agents**: Each agent gets its own system prompt and conversation context.

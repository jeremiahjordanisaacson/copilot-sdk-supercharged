# Multiple Sessions in C

Patterns for managing multiple independent conversations with the GitHub Copilot SDK in C.

## Scenario

Your application needs to run several conversations at the same time, each with its own context and history, sharing a single client connection.

## Creating Multiple Sessions

```c
#include <copilot/client.h>
#include <stdio.h>

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;
    copilot_session *code_session = NULL;
    copilot_session *docs_session = NULL;

    if (copilot_client_create(&opts, &client) != COPILOT_OK) goto cleanup;
    if (copilot_start(client) != COPILOT_OK) goto cleanup;

    /* Create separate sessions for different tasks */
    copilot_session_config code_config = {0};
    code_config.system_prompt = "You are a C expert.";
    if (copilot_create_session(client, &code_config, &code_session) != COPILOT_OK)
        goto cleanup;

    copilot_session_config docs_config = {0};
    docs_config.system_prompt = "You are a technical writer.";
    if (copilot_create_session(client, &docs_config, &docs_session) != COPILOT_OK)
        goto cleanup;

    /* Use each session independently */
    copilot_response *r1 = NULL;
    copilot_send_options s1 = {.message = "Explain malloc vs calloc"};
    if (copilot_send_and_wait(code_session, &s1, &r1) == COPILOT_OK) {
        printf("Code: %s\n", copilot_response_message(r1));
        copilot_free_response(r1);
    }

    copilot_response *r2 = NULL;
    copilot_send_options s2 = {.message = "Write a man page template"};
    if (copilot_send_and_wait(docs_session, &s2, &r2) == COPILOT_OK) {
        printf("Docs: %s\n", copilot_response_message(r2));
        copilot_free_response(r2);
    }

cleanup:
    if (docs_session) copilot_free_session(docs_session);
    if (code_session) copilot_free_session(code_session);
    if (client) { copilot_stop(client); copilot_free_client(client); }
    return 0;
}
```

## Session Array Manager

```c
#include <copilot/client.h>
#include <stdio.h>
#include <string.h>

#define MAX_SESSIONS 16

typedef struct {
    char name[64];
    copilot_session *session;
} named_session;

typedef struct {
    copilot_client *client;
    named_session slots[MAX_SESSIONS];
    int count;
} session_manager;

int manager_init(session_manager *mgr, copilot_client *client) {
    mgr->client = client;
    mgr->count = 0;
    memset(mgr->slots, 0, sizeof(mgr->slots));
    return 0;
}

copilot_session *manager_create(session_manager *mgr, const char *name,
                                 const char *system_prompt) {
    if (mgr->count >= MAX_SESSIONS) return NULL;

    copilot_session_config config = {0};
    config.system_prompt = system_prompt;

    copilot_session *session = NULL;
    if (copilot_create_session(mgr->client, &config, &session) != COPILOT_OK) {
        return NULL;
    }

    strncpy(mgr->slots[mgr->count].name, name, 63);
    mgr->slots[mgr->count].session = session;
    mgr->count++;
    return session;
}

copilot_session *manager_find(session_manager *mgr, const char *name) {
    for (int i = 0; i < mgr->count; i++) {
        if (strcmp(mgr->slots[i].name, name) == 0) {
            return mgr->slots[i].session;
        }
    }
    return NULL;
}

void manager_cleanup(session_manager *mgr) {
    for (int i = 0; i < mgr->count; i++) {
        if (mgr->slots[i].session) {
            copilot_free_session(mgr->slots[i].session);
        }
    }
    mgr->count = 0;
}

/* Usage */
int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;
    session_manager mgr;

    if (copilot_client_create(&opts, &client) != COPILOT_OK) return 1;
    if (copilot_start(client) != COPILOT_OK) {
        copilot_free_client(client);
        return 1;
    }

    manager_init(&mgr, client);
    manager_create(&mgr, "frontend", "You are a CSS expert.");
    manager_create(&mgr, "backend", "You are a database expert.");

    copilot_session *fe = manager_find(&mgr, "frontend");
    if (fe) {
        copilot_response *resp = NULL;
        copilot_send_options so = {.message = "Explain flexbox"};
        if (copilot_send_and_wait(fe, &so, &resp) == COPILOT_OK) {
            printf("Frontend: %s\n", copilot_response_message(resp));
            copilot_free_response(resp);
        }
    }

    manager_cleanup(&mgr);
    copilot_stop(client);
    copilot_free_client(client);
    return 0;
}
```

## Best Practices

- **Reuse a single client**: Create one `copilot_client` and share it across all sessions.
- **Track sessions with a manager struct**: Use a simple array or hash table to store named sessions.
- **Free sessions in reverse order**: Deallocate sessions before stopping the client.
- **Check every return code**: Verify `copilot_create_session` succeeded before using the session.
- **Set distinct system prompts**: Give each session a focused persona for better results.
- **Limit concurrent sessions**: Use a fixed-size array with a maximum count to prevent unbounded growth.

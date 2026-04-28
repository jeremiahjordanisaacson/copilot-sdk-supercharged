# Persisting Sessions in C

Patterns for saving and resuming Copilot sessions across application restarts in C.

## Scenario

Your application needs to save a conversation so users can close the app and return later to continue exactly where they left off.

## Saving and Loading Session IDs

```c
#include <copilot/client.h>
#include <stdio.h>
#include <string.h>

#define SESSION_FILE "session_state.txt"
#define MAX_ID_LEN 256

int save_session_id(const char *session_id) {
    FILE *fp = fopen(SESSION_FILE, "w");
    if (!fp) {
        perror("Failed to save session");
        return -1;
    }
    fprintf(fp, "%s", session_id);
    fclose(fp);
    return 0;
}

int load_session_id(char *buffer, size_t buf_size) {
    FILE *fp = fopen(SESSION_FILE, "r");
    if (!fp) return -1;

    if (!fgets(buffer, (int)buf_size, fp)) {
        fclose(fp);
        return -1;
    }

    /* Strip trailing newline */
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
    }

    fclose(fp);
    return 0;
}
```

## Resuming a Session

```c
#include <copilot/client.h>
#include <stdio.h>
#include <string.h>

copilot_session *get_or_create_session(copilot_client *client) {
    char saved_id[MAX_ID_LEN] = {0};
    copilot_session *session = NULL;

    /* Try to resume a saved session */
    if (load_session_id(saved_id, sizeof(saved_id)) == 0 && saved_id[0] != '\0') {
        copilot_session_config config = {0};
        config.session_id = saved_id;

        if (copilot_create_session(client, &config, &session) == COPILOT_OK) {
            printf("Resumed session: %s\n", saved_id);
            return session;
        }
        fprintf(stderr, "Could not resume session, creating new one\n");
    }

    /* Create a new session */
    copilot_session_config config = {0};
    if (copilot_create_session(client, &config, &session) != COPILOT_OK) {
        fprintf(stderr, "Failed to create session: %s\n",
                copilot_get_error(client));
        return NULL;
    }

    const char *new_id = copilot_session_get_id(session);
    save_session_id(new_id);
    printf("Created new session: %s\n", new_id);
    return session;
}
```

## Full Example: Persistent Chat

```c
#include <copilot/client.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;
    copilot_session *session = NULL;

    if (copilot_client_create(&opts, &client) != COPILOT_OK) return 1;
    if (copilot_start(client) != COPILOT_OK) {
        copilot_free_client(client);
        return 1;
    }

    session = get_or_create_session(client);
    if (!session) {
        copilot_stop(client);
        copilot_free_client(client);
        return 1;
    }

    char input[1024];
    printf("Chat (type 'quit' to exit):\n");

    while (1) {
        printf("> ");
        if (!fgets(input, sizeof(input), stdin)) break;

        /* Strip newline */
        size_t len = strlen(input);
        if (len > 0 && input[len - 1] == '\n') input[len - 1] = '\0';

        if (strcmp(input, "quit") == 0) break;

        copilot_response *response = NULL;
        copilot_send_options send_opts = {.message = input};

        if (copilot_send_and_wait(session, &send_opts, &response) == COPILOT_OK) {
            printf("Assistant: %s\n", copilot_response_message(response));
            copilot_free_response(response);
        } else {
            fprintf(stderr, "Error: %s\n", copilot_get_error(client));
        }
    }

    copilot_free_session(session);
    copilot_stop(client);
    copilot_free_client(client);
    printf("Session saved. You can resume later.\n");
    return 0;
}
```

## Best Practices

- **Persist only the session ID**: The SDK and CLI handle conversation state internally. You just need the ID.
- **Try to resume before creating**: Attempt to restore a saved session first, then fall back to a new one.
- **Handle stale sessions**: If resuming fails (session expired), create a fresh session.
- **Strip newlines from file reads**: Use `fgets` and trim to avoid invisible characters in session IDs.
- **Close file handles immediately**: Use `fclose` right after reading or writing to avoid resource leaks.
- **Save on creation, not on exit**: Write the session ID immediately after creating it to avoid data loss on crashes.

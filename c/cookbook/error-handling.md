# Error Handling in C

Patterns for handling errors when using the GitHub Copilot SDK in C.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors gracefully using C-style error codes and cleanup functions.

## Checking Return Codes

The C SDK uses integer return codes. Always check them after every call.

```c
#include <copilot/client.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    copilot_options opts = {0};
    copilot_client *client = NULL;

    int rc = copilot_client_create(&opts, &client);
    if (rc != COPILOT_OK) {
        fprintf(stderr, "Failed to create client: %s\n", copilot_get_error(client));
        return 1;
    }

    rc = copilot_start(client);
    if (rc != COPILOT_OK) {
        fprintf(stderr, "Failed to start client: %s\n", copilot_get_error(client));
        copilot_free_client(client);
        return 1;
    }

    printf("Client started successfully\n");

    copilot_stop(client);
    copilot_free_client(client);
    return 0;
}
```

## Using goto for Cleanup

A common C pattern to avoid duplicated cleanup code.

```c
#include <copilot/client.h>
#include <stdio.h>

int run_conversation(void) {
    copilot_client *client = NULL;
    copilot_session *session = NULL;
    copilot_response *response = NULL;
    int result = -1;

    copilot_options opts = {0};
    if (copilot_client_create(&opts, &client) != COPILOT_OK) {
        fprintf(stderr, "Create failed: %s\n", copilot_get_error(NULL));
        goto cleanup;
    }

    if (copilot_start(client) != COPILOT_OK) {
        fprintf(stderr, "Start failed: %s\n", copilot_get_error(client));
        goto cleanup;
    }

    copilot_session_config config = {0};
    if (copilot_create_session(client, &config, &session) != COPILOT_OK) {
        fprintf(stderr, "Session failed: %s\n", copilot_get_error(client));
        goto cleanup;
    }

    copilot_send_options send_opts = {0};
    send_opts.message = "Explain pointers in C";
    if (copilot_send_and_wait(session, &send_opts, &response) != COPILOT_OK) {
        fprintf(stderr, "Send failed: %s\n", copilot_get_error(client));
        goto cleanup;
    }

    printf("Response: %s\n", copilot_response_message(response));
    result = 0;

cleanup:
    if (response) copilot_free_response(response);
    if (session) copilot_free_session(session);
    if (client) {
        copilot_stop(client);
        copilot_free_client(client);
    }
    return result;
}
```

## Retry with Exponential Backoff

```c
#include <copilot/client.h>
#include <stdio.h>
#include <unistd.h>

int connect_with_retry(copilot_options *opts, copilot_client **out_client,
                       int max_retries) {
    for (int attempt = 1; attempt <= max_retries; attempt++) {
        copilot_client *client = NULL;
        int rc = copilot_client_create(opts, &client);
        if (rc != COPILOT_OK) {
            fprintf(stderr, "Attempt %d: create failed: %s\n",
                    attempt, copilot_get_error(NULL));
            goto retry;
        }

        rc = copilot_start(client);
        if (rc == COPILOT_OK) {
            printf("Connected on attempt %d\n", attempt);
            *out_client = client;
            return COPILOT_OK;
        }

        fprintf(stderr, "Attempt %d: start failed: %s\n",
                attempt, copilot_get_error(client));
        copilot_free_client(client);

    retry:
        if (attempt < max_retries) {
            unsigned int delay = attempt * 2;
            fprintf(stderr, "Retrying in %u seconds...\n", delay);
            sleep(delay);
        }
    }

    return COPILOT_ERROR_CONNECTION;
}
```

## Error Code Helper

Wrap common checks into a macro for concise error handling.

```c
#include <copilot/client.h>
#include <stdio.h>

#define COPILOT_CHECK(expr, client, label)                              \
    do {                                                                 \
        int _rc = (expr);                                                \
        if (_rc != COPILOT_OK) {                                         \
            fprintf(stderr, "%s failed: %s (line %d)\n",                 \
                    #expr, copilot_get_error(client), __LINE__);         \
            goto label;                                                  \
        }                                                                \
    } while (0)

int main(void) {
    copilot_client *client = NULL;
    copilot_session *session = NULL;

    copilot_options opts = {0};
    COPILOT_CHECK(copilot_client_create(&opts, &client), NULL, cleanup);
    COPILOT_CHECK(copilot_start(client), client, cleanup);

    copilot_session_config config = {0};
    COPILOT_CHECK(copilot_create_session(client, &config, &session), client, cleanup);

    printf("Session created successfully\n");

cleanup:
    if (session) copilot_free_session(session);
    if (client) {
        copilot_stop(client);
        copilot_free_client(client);
    }
    return 0;
}
```

## Best Practices

- **Always check return codes**: Every SDK function returns an error code. Never ignore it.
- **Use goto for cleanup**: The goto-cleanup pattern prevents resource leaks in error paths.
- **Free resources in reverse order**: Deallocate in the opposite order of allocation.
- **Use `copilot_get_error()` for messages**: Extract human-readable error descriptions for logging.
- **Use macros for repetitive checks**: A `COPILOT_CHECK` macro reduces boilerplate and catches the line number.
- **Initialize pointers to NULL**: This makes conditional cleanup safe with null checks.

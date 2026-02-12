/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file basic_example.c
 * @brief Basic usage example for the GitHub Copilot C SDK.
 *
 * This example demonstrates:
 *   1. Creating a CopilotClient
 *   2. Starting the client (spawning CLI process)
 *   3. Creating a session with a custom tool
 *   4. Registering event handlers
 *   5. Sending a message and waiting for a response
 *   6. Graceful cleanup
 *
 * Build:
 *   mkdir build && cd build
 *   cmake .. && cmake --build .
 *   ./copilot_basic_example
 *
 * Prerequisites:
 *   - The Copilot CLI (copilot or @github/copilot) must be installed and on PATH,
 *     or specify cli_path in client options.
 *   - Valid GitHub authentication (OAuth token, gh CLI auth, or GITHUB_TOKEN env var).
 */

#include "copilot/copilot.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ============================================================================
 * Custom tool: get_weather
 *
 * Demonstrates how to define a tool that the Copilot assistant can invoke.
 * ============================================================================ */

static copilot_error_t weather_tool_handler(
    const copilot_tool_invocation_t *invocation,
    void *user_data,
    copilot_tool_result_t *out_result)
{
    (void)user_data;

    printf("[Tool] '%s' called with args: %s\n",
           invocation->tool_name, invocation->arguments_json);

    /*
     * In a real application, you would parse invocation->arguments_json
     * to extract the location, call a weather API, etc.
     * For this example, we return a hardcoded result.
     */
    out_result->text_result_for_llm =
        "Weather in San Francisco: 68F (20C), partly cloudy, wind 12mph W";
    out_result->result_type = COPILOT_TOOL_RESULT_SUCCESS;
    out_result->error = NULL;

    return COPILOT_OK;
}

/* JSON schema for the get_weather tool parameters */
static const char *weather_params_schema =
    "{"
    "  \"type\": \"object\","
    "  \"properties\": {"
    "    \"location\": {"
    "      \"type\": \"string\","
    "      \"description\": \"The city and state, e.g. San Francisco, CA\""
    "    }"
    "  },"
    "  \"required\": [\"location\"]"
    "}";

/* ============================================================================
 * Event handler: prints events as they arrive
 * ============================================================================ */

static void on_session_event(const copilot_session_event_t *event, void *user_data)
{
    (void)user_data;

    if (strcmp(event->type, "assistant.message") == 0) {
        printf("\n[Assistant] %s\n", event->content ? event->content : "(no content)");
    } else if (strcmp(event->type, "assistant.message_delta") == 0) {
        /* Streaming delta - print incrementally */
        if (event->delta_content) {
            printf("%s", event->delta_content);
            fflush(stdout);
        }
    } else if (strcmp(event->type, "tool.executing") == 0) {
        printf("[Event] Executing tool: %s\n", event->tool_name ? event->tool_name : "?");
    } else if (strcmp(event->type, "tool.executed") == 0) {
        printf("[Event] Tool executed: %s\n", event->tool_name ? event->tool_name : "?");
    } else if (strcmp(event->type, "session.idle") == 0) {
        printf("[Event] Session is idle\n");
    } else if (strcmp(event->type, "session.error") == 0) {
        fprintf(stderr, "[Error] %s\n", event->message ? event->message : "Unknown error");
    } else {
        printf("[Event] %s\n", event->type);
    }
}

/* ============================================================================
 * Permission handler: auto-approve all (for demonstration)
 * ============================================================================ */

static copilot_error_t on_permission_request(
    const copilot_permission_request_t *request,
    const char *session_id,
    void *user_data,
    copilot_permission_result_t *out_result)
{
    (void)user_data;
    (void)session_id;

    printf("[Permission] Request kind: %s - auto-approving\n",
           request->kind ? request->kind : "unknown");

    out_result->kind = COPILOT_PERMISSION_APPROVED;
    return COPILOT_OK;
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    printf("=== GitHub Copilot C SDK - Basic Example ===\n\n");

    /* ---- 1. Create client ---- */
    copilot_client_options_t client_opts = copilot_client_options_default();
    /* Uncomment to specify a custom CLI path: */
    /* client_opts.cli_path = "/usr/local/bin/copilot"; */

    copilot_client_t *client = copilot_client_create(&client_opts);
    if (!client) {
        fprintf(stderr, "Failed to create client\n");
        return 1;
    }

    /* ---- 2. Start client ---- */
    printf("Starting Copilot CLI server...\n");
    copilot_error_t err = copilot_client_start(client);
    if (err != COPILOT_OK) {
        fprintf(stderr, "Failed to start client: %s\n", copilot_error_string(err));
        copilot_client_free(client);
        return 1;
    }
    printf("Connected!\n\n");

    /* ---- 3. Ping the server ---- */
    copilot_ping_response_t ping;
    err = copilot_client_ping(client, "hello from C SDK", &ping);
    if (err == COPILOT_OK) {
        printf("Ping response: %s (protocol v%d)\n\n",
               ping.message, ping.protocol_version);
        copilot_ping_response_free(&ping);
    }

    /* ---- 4. List available models ---- */
    copilot_model_list_t models;
    err = copilot_client_list_models(client, &models);
    if (err == COPILOT_OK) {
        printf("Available models (%zu):\n", models.count);
        for (size_t i = 0; i < models.count && i < 5; i++) {
            printf("  - %s (%s) [context: %d tokens]\n",
                   models.models[i].id, models.models[i].name,
                   models.models[i].max_context_window_tokens);
        }
        if (models.count > 5) {
            printf("  ... and %zu more\n", models.count - 5);
        }
        printf("\n");
        copilot_model_list_free(&models);
    }

    /* ---- 5. Create a session with a custom tool ---- */
    copilot_tool_t tools[] = {
        {
            .name = "get_weather",
            .description = "Get the current weather for a location",
            .parameters_json = weather_params_schema,
            .handler = weather_tool_handler,
            .user_data = NULL,
        },
    };

    copilot_session_config_t session_config = copilot_session_config_default();
    session_config.tools = tools;
    session_config.tools_count = 1;
    session_config.on_permission_request = on_permission_request;
    /* Uncomment to use a specific model: */
    /* session_config.model = "claude-sonnet-4"; */

    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, &session_config, &session);
    if (err != COPILOT_OK) {
        fprintf(stderr, "Failed to create session: %s\n", copilot_error_string(err));
        copilot_client_stop(client);
        copilot_client_free(client);
        return 1;
    }
    printf("Session created: %s\n\n", copilot_session_get_id(session));

    /* ---- 6. Register event handler ---- */
    int handler_id = copilot_session_on(session, on_session_event, NULL);

    /* ---- 7. Send a message and wait for response ---- */
    printf("Sending message...\n");

    copilot_message_options_t msg = {
        .prompt = "What is the weather in San Francisco? Use the get_weather tool.",
        .attachments = NULL,
        .attachments_count = 0,
        .mode = NULL,
    };

    char *response_content = NULL;
    err = copilot_session_send_and_wait(session, &msg, 120000, &response_content);

    if (err == COPILOT_OK) {
        printf("\n--- Final response ---\n");
        if (response_content) {
            printf("%s\n", response_content);
            free(response_content);
        } else {
            printf("(no assistant message received)\n");
        }
    } else if (err == COPILOT_ERROR_TIMEOUT) {
        printf("\nRequest timed out.\n");
    } else {
        fprintf(stderr, "\nError: %s\n", copilot_error_string(err));
    }

    /* ---- 8. Clean up ---- */
    printf("\nCleaning up...\n");
    copilot_session_off(session, handler_id);
    copilot_session_destroy(session);
    copilot_session_free(session);

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("Done.\n");
    return 0;
}

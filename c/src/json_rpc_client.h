/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file json_rpc_client.h
 * @brief Internal JSON-RPC 2.0 client for stdio transport with Content-Length framing.
 *
 * This module implements the JSON-RPC 2.0 protocol used to communicate with the
 * Copilot CLI server. It handles:
 *   - Content-Length header framing for message delimiting
 *   - Sending requests and receiving responses with correlation by ID
 *   - Receiving server-initiated notifications and requests
 *   - Background reader thread for incoming messages
 *   - Thread-safe request/response matching
 */

#ifndef COPILOT_JSON_RPC_CLIENT_H
#define COPILOT_JSON_RPC_CLIENT_H

#include <stdbool.h>
#include <stddef.h>
#include <pthread.h>
#include <cjson/cJSON.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declaration */
typedef struct json_rpc_client json_rpc_client_t;

/**
 * Handler for server-initiated requests (tool.call, permission.request, etc.)
 * The handler must return a cJSON result object (transferred to caller) or NULL.
 * On error, set *out_error_code and *out_error_message.
 */
typedef cJSON *(*json_rpc_request_handler_fn)(
    const char *method,
    cJSON *params,
    void *user_data,
    int *out_error_code,
    char **out_error_message
);

/**
 * Handler for server-initiated notifications (session.event, session.lifecycle, etc.)
 */
typedef void (*json_rpc_notification_handler_fn)(
    const char *method,
    cJSON *params,
    void *user_data
);

/* ============================================================================
 * Pending request tracking
 * ============================================================================ */

typedef struct pending_request {
    char *id;
    cJSON *result;           /**< Set when response arrives */
    int error_code;          /**< Non-zero if error response */
    char *error_message;     /**< Error message from response */
    bool completed;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    struct pending_request *next;
} pending_request_t;

/* ============================================================================
 * Request handler registration
 * ============================================================================ */

typedef struct request_handler_entry {
    char *method;
    json_rpc_request_handler_fn handler;
    void *user_data;
    struct request_handler_entry *next;
} request_handler_entry_t;

/* ============================================================================
 * Notification handler registration
 * ============================================================================ */

typedef struct notification_handler_entry {
    char *method;
    json_rpc_notification_handler_fn handler;
    void *user_data;
    struct notification_handler_entry *next;
} notification_handler_entry_t;

/* ============================================================================
 * JSON-RPC Client
 * ============================================================================ */

struct json_rpc_client {
    /* I/O file descriptors */
    int write_fd;            /**< Write end (to CLI stdin) */
    int read_fd;             /**< Read end (from CLI stdout) */

    /* Reader thread */
    pthread_t reader_thread;
    bool reader_running;

    /* Synchronization */
    pthread_mutex_t write_mutex;       /**< Protects write_fd writes */
    pthread_mutex_t pending_mutex;     /**< Protects pending_requests list */
    pthread_mutex_t handlers_mutex;    /**< Protects handler lists */

    /* Pending requests awaiting responses */
    pending_request_t *pending_requests;

    /* Registered handlers */
    request_handler_entry_t *request_handlers;
    notification_handler_entry_t *notification_handlers;

    /* ID counter */
    unsigned long next_id;
    pthread_mutex_t id_mutex;

    /* Stop signal */
    volatile bool stopping;
};

/**
 * Creates a new JSON-RPC client.
 *
 * @param write_fd  File descriptor for writing (connected to CLI stdin).
 * @param read_fd   File descriptor for reading (connected to CLI stdout).
 * @return New client, or NULL on failure.
 */
json_rpc_client_t *json_rpc_client_create(int write_fd, int read_fd);

/**
 * Starts the background reader thread.
 *
 * @return 0 on success, -1 on failure.
 */
int json_rpc_client_start(json_rpc_client_t *client);

/**
 * Stops the client and waits for the reader thread to finish.
 */
void json_rpc_client_stop(json_rpc_client_t *client);

/**
 * Frees all resources associated with the client.
 * Must call json_rpc_client_stop() first.
 */
void json_rpc_client_free(json_rpc_client_t *client);

/**
 * Sends a JSON-RPC request and waits for the response.
 *
 * @param client   The client.
 * @param method   The RPC method name.
 * @param params   The parameters as cJSON (ownership NOT transferred).
 * @param timeout_ms Timeout in milliseconds (0 = 30000ms default).
 * @return The result as cJSON (caller must cJSON_Delete), or NULL on error.
 *         On error, *out_error_code and *out_error_message are set if non-NULL.
 */
cJSON *json_rpc_client_request(
    json_rpc_client_t *client,
    const char *method,
    cJSON *params,
    int timeout_ms,
    int *out_error_code,
    char **out_error_message
);

/**
 * Sends a JSON-RPC notification (no response expected).
 *
 * @param client  The client.
 * @param method  The RPC method name.
 * @param params  The parameters as cJSON (ownership NOT transferred).
 * @return 0 on success, -1 on failure.
 */
int json_rpc_client_notify(
    json_rpc_client_t *client,
    const char *method,
    cJSON *params
);

/**
 * Registers a handler for server-initiated requests.
 */
void json_rpc_client_set_request_handler(
    json_rpc_client_t *client,
    const char *method,
    json_rpc_request_handler_fn handler,
    void *user_data
);

/**
 * Registers a handler for server-initiated notifications.
 */
void json_rpc_client_set_notification_handler(
    json_rpc_client_t *client,
    const char *method,
    json_rpc_notification_handler_fn handler,
    void *user_data
);

#ifdef __cplusplus
}
#endif

#endif /* COPILOT_JSON_RPC_CLIENT_H */

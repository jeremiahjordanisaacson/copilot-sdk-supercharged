/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file json_rpc_client.c
 * @brief Internal JSON-RPC 2.0 client implementation.
 */

#include "json_rpc_client.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#ifdef _WIN32
#include <io.h>
#define read_fd_fn  _read
#define write_fd_fn _write
#else
#include <unistd.h>
#define read_fd_fn  read
#define write_fd_fn write
#endif

/* ============================================================================
 * Internal helpers
 * ============================================================================ */

static char *generate_request_id(json_rpc_client_t *client)
{
    pthread_mutex_lock(&client->id_mutex);
    unsigned long id = client->next_id++;
    pthread_mutex_unlock(&client->id_mutex);

    char buf[32];
    snprintf(buf, sizeof(buf), "%lu", id);
    return strdup(buf);
}

/**
 * Write exactly 'len' bytes to the file descriptor.
 */
static int write_all(int fd, const char *buf, size_t len)
{
    size_t written = 0;
    while (written < len) {
        int n = write_fd_fn(fd, buf + written, (unsigned int)(len - written));
        if (n <= 0) {
            return -1;
        }
        written += (size_t)n;
    }
    return 0;
}

/**
 * Send a JSON-RPC message with Content-Length framing.
 */
static int send_message(json_rpc_client_t *client, cJSON *message)
{
    char *json_str = cJSON_PrintUnformatted(message);
    if (!json_str) {
        return -1;
    }

    size_t json_len = strlen(json_str);
    char header[64];
    int header_len = snprintf(header, sizeof(header),
                              "Content-Length: %zu\r\n\r\n", json_len);

    pthread_mutex_lock(&client->write_mutex);
    int rc = write_all(client->write_fd, header, (size_t)header_len);
    if (rc == 0) {
        rc = write_all(client->write_fd, json_str, json_len);
    }
    pthread_mutex_unlock(&client->write_mutex);

    free(json_str);
    return rc;
}

/**
 * Read a line (up to \n) from the file descriptor.
 * Returns the number of bytes read (including \n), or -1 on error/EOF.
 */
static int read_line(int fd, char *buf, size_t max_len)
{
    size_t pos = 0;
    while (pos < max_len - 1) {
        char c;
        int n = read_fd_fn(fd, &c, 1);
        if (n <= 0) {
            return -1;
        }
        buf[pos++] = c;
        if (c == '\n') {
            break;
        }
    }
    buf[pos] = '\0';
    return (int)pos;
}

/**
 * Read exactly 'len' bytes from the file descriptor.
 */
static int read_exact(int fd, char *buf, size_t len)
{
    size_t total = 0;
    while (total < len) {
        int n = read_fd_fn(fd, buf + total, (unsigned int)(len - total));
        if (n <= 0) {
            return -1;
        }
        total += (size_t)n;
    }
    return 0;
}

/* ============================================================================
 * Pending request management
 * ============================================================================ */

static pending_request_t *pending_request_create(const char *id)
{
    pending_request_t *pr = calloc(1, sizeof(pending_request_t));
    if (!pr) return NULL;

    pr->id = strdup(id);
    pr->completed = false;
    pr->result = NULL;
    pr->error_code = 0;
    pr->error_message = NULL;
    pr->next = NULL;
    pthread_mutex_init(&pr->mutex, NULL);
    pthread_cond_init(&pr->cond, NULL);
    return pr;
}

static void pending_request_free(pending_request_t *pr)
{
    if (!pr) return;
    free(pr->id);
    if (pr->result) cJSON_Delete(pr->result);
    free(pr->error_message);
    pthread_mutex_destroy(&pr->mutex);
    pthread_cond_destroy(&pr->cond);
    free(pr);
}

static void add_pending(json_rpc_client_t *client, pending_request_t *pr)
{
    pthread_mutex_lock(&client->pending_mutex);
    pr->next = client->pending_requests;
    client->pending_requests = pr;
    pthread_mutex_unlock(&client->pending_mutex);
}

static void remove_pending(json_rpc_client_t *client, pending_request_t *pr)
{
    pthread_mutex_lock(&client->pending_mutex);
    pending_request_t **pp = &client->pending_requests;
    while (*pp) {
        if (*pp == pr) {
            *pp = pr->next;
            break;
        }
        pp = &(*pp)->next;
    }
    pthread_mutex_unlock(&client->pending_mutex);
}

static pending_request_t *find_pending_by_id(json_rpc_client_t *client, const char *id)
{
    pthread_mutex_lock(&client->pending_mutex);
    pending_request_t *pr = client->pending_requests;
    while (pr) {
        if (strcmp(pr->id, id) == 0) {
            pthread_mutex_unlock(&client->pending_mutex);
            return pr;
        }
        pr = pr->next;
    }
    pthread_mutex_unlock(&client->pending_mutex);
    return NULL;
}

/* ============================================================================
 * Handler lookup
 * ============================================================================ */

static request_handler_entry_t *find_request_handler(json_rpc_client_t *client, const char *method)
{
    pthread_mutex_lock(&client->handlers_mutex);
    request_handler_entry_t *entry = client->request_handlers;
    while (entry) {
        if (strcmp(entry->method, method) == 0) {
            pthread_mutex_unlock(&client->handlers_mutex);
            return entry;
        }
        entry = entry->next;
    }
    pthread_mutex_unlock(&client->handlers_mutex);
    return NULL;
}

static notification_handler_entry_t *find_notification_handler(
    json_rpc_client_t *client, const char *method)
{
    pthread_mutex_lock(&client->handlers_mutex);
    notification_handler_entry_t *entry = client->notification_handlers;
    while (entry) {
        if (strcmp(entry->method, method) == 0) {
            pthread_mutex_unlock(&client->handlers_mutex);
            return entry;
        }
        entry = entry->next;
    }
    pthread_mutex_unlock(&client->handlers_mutex);
    return NULL;
}

/* ============================================================================
 * Message dispatch
 * ============================================================================ */

static void handle_response(json_rpc_client_t *client, cJSON *message)
{
    cJSON *id_item = cJSON_GetObjectItem(message, "id");
    if (!id_item) return;

    const char *id_str;
    char id_buf[32];
    if (cJSON_IsString(id_item)) {
        id_str = id_item->valuestring;
    } else if (cJSON_IsNumber(id_item)) {
        snprintf(id_buf, sizeof(id_buf), "%d", id_item->valueint);
        id_str = id_buf;
    } else {
        return;
    }

    pending_request_t *pr = find_pending_by_id(client, id_str);
    if (!pr) return;

    pthread_mutex_lock(&pr->mutex);

    cJSON *error = cJSON_GetObjectItem(message, "error");
    if (error && cJSON_IsObject(error)) {
        cJSON *code = cJSON_GetObjectItem(error, "code");
        cJSON *msg = cJSON_GetObjectItem(error, "message");
        pr->error_code = code ? code->valueint : -1;
        pr->error_message = msg && cJSON_IsString(msg) ? strdup(msg->valuestring) : strdup("Unknown error");
        pr->result = NULL;
    } else {
        cJSON *result = cJSON_GetObjectItem(message, "result");
        pr->result = result ? cJSON_Duplicate(result, 1) : cJSON_CreateObject();
        pr->error_code = 0;
        pr->error_message = NULL;
    }

    pr->completed = true;
    pthread_cond_signal(&pr->cond);
    pthread_mutex_unlock(&pr->mutex);
}

static void handle_server_request(json_rpc_client_t *client, cJSON *message)
{
    cJSON *method_item = cJSON_GetObjectItem(message, "method");
    cJSON *id_item = cJSON_GetObjectItem(message, "id");
    cJSON *params_item = cJSON_GetObjectItem(message, "params");

    if (!method_item || !cJSON_IsString(method_item)) return;

    const char *method = method_item->valuestring;
    bool is_call = (id_item != NULL && !cJSON_IsNull(id_item));

    if (is_call) {
        /* Server request: look for registered request handler */
        request_handler_entry_t *entry = find_request_handler(client, method);

        cJSON *response = cJSON_CreateObject();
        cJSON_AddStringToObject(response, "jsonrpc", "2.0");

        /* Copy the ID */
        cJSON_AddItemToObject(response, "id", cJSON_Duplicate(id_item, 1));

        if (entry && entry->handler) {
            int err_code = 0;
            char *err_msg = NULL;
            cJSON *result = entry->handler(method, params_item, entry->user_data,
                                           &err_code, &err_msg);
            if (err_code != 0) {
                cJSON *error = cJSON_CreateObject();
                cJSON_AddNumberToObject(error, "code", err_code);
                cJSON_AddStringToObject(error, "message", err_msg ? err_msg : "Handler error");
                cJSON_AddItemToObject(response, "error", error);
                free(err_msg);
                if (result) cJSON_Delete(result);
            } else {
                cJSON_AddItemToObject(response, "result",
                                      result ? result : cJSON_CreateObject());
            }
        } else {
            cJSON *error = cJSON_CreateObject();
            cJSON_AddNumberToObject(error, "code", -32601);
            char msg[256];
            snprintf(msg, sizeof(msg), "Method not found: %s", method);
            cJSON_AddStringToObject(error, "message", msg);
            cJSON_AddItemToObject(response, "error", error);
        }

        send_message(client, response);
        cJSON_Delete(response);
    } else {
        /* Server notification */
        notification_handler_entry_t *entry = find_notification_handler(client, method);
        if (entry && entry->handler) {
            entry->handler(method, params_item, entry->user_data);
        }
    }
}

/* ============================================================================
 * Reader thread
 * ============================================================================ */

static void *reader_thread_fn(void *arg)
{
    json_rpc_client_t *client = (json_rpc_client_t *)arg;
    char line_buf[1024];

    while (!client->stopping) {
        /* Read headers to find Content-Length */
        int content_length = 0;

        while (1) {
            int n = read_line(client->read_fd, line_buf, sizeof(line_buf));
            if (n <= 0) {
                /* EOF or error */
                goto done;
            }

            /* End of headers: blank line */
            if (strcmp(line_buf, "\r\n") == 0 || strcmp(line_buf, "\n") == 0) {
                break;
            }

            /* Parse Content-Length */
            int len;
            if (sscanf(line_buf, "Content-Length: %d", &len) == 1) {
                content_length = len;
            }
        }

        if (content_length <= 0) {
            continue;
        }

        /* Read message body */
        char *body = malloc((size_t)content_length + 1);
        if (!body) {
            goto done;
        }

        if (read_exact(client->read_fd, body, (size_t)content_length) != 0) {
            free(body);
            goto done;
        }
        body[content_length] = '\0';

        /* Parse JSON */
        cJSON *message = cJSON_Parse(body);
        free(body);
        if (!message) {
            continue;
        }

        /* Determine message type */
        cJSON *method = cJSON_GetObjectItem(message, "method");
        cJSON *id = cJSON_GetObjectItem(message, "id");

        if (method && cJSON_IsString(method)) {
            /* Server request or notification */
            handle_server_request(client, message);
        } else if (id) {
            /* Response to a pending request */
            handle_response(client, message);
        }

        cJSON_Delete(message);
    }

done:
    client->reader_running = false;
    /* Wake up any pending requests */
    pthread_mutex_lock(&client->pending_mutex);
    pending_request_t *pr = client->pending_requests;
    while (pr) {
        pthread_mutex_lock(&pr->mutex);
        if (!pr->completed) {
            pr->completed = true;
            pr->error_code = -32000;
            pr->error_message = strdup("Connection closed");
            pthread_cond_signal(&pr->cond);
        }
        pthread_mutex_unlock(&pr->mutex);
        pr = pr->next;
    }
    pthread_mutex_unlock(&client->pending_mutex);

    return NULL;
}

/* ============================================================================
 * Public API
 * ============================================================================ */

json_rpc_client_t *json_rpc_client_create(int write_fd, int read_fd)
{
    json_rpc_client_t *client = calloc(1, sizeof(json_rpc_client_t));
    if (!client) return NULL;

    client->write_fd = write_fd;
    client->read_fd = read_fd;
    client->reader_running = false;
    client->stopping = false;
    client->pending_requests = NULL;
    client->request_handlers = NULL;
    client->notification_handlers = NULL;
    client->next_id = 1;

    pthread_mutex_init(&client->write_mutex, NULL);
    pthread_mutex_init(&client->pending_mutex, NULL);
    pthread_mutex_init(&client->handlers_mutex, NULL);
    pthread_mutex_init(&client->id_mutex, NULL);

    return client;
}

int json_rpc_client_start(json_rpc_client_t *client)
{
    if (!client || client->reader_running) return -1;

    client->reader_running = true;
    client->stopping = false;

    int rc = pthread_create(&client->reader_thread, NULL, reader_thread_fn, client);
    if (rc != 0) {
        client->reader_running = false;
        return -1;
    }

    return 0;
}

void json_rpc_client_stop(json_rpc_client_t *client)
{
    if (!client) return;

    client->stopping = true;

    /* Close read fd to unblock the reader thread */
    if (client->read_fd >= 0) {
#ifdef _WIN32
        _close(client->read_fd);
#else
        close(client->read_fd);
#endif
        client->read_fd = -1;
    }

    if (client->reader_running) {
        pthread_join(client->reader_thread, NULL);
        client->reader_running = false;
    }
}

void json_rpc_client_free(json_rpc_client_t *client)
{
    if (!client) return;

    /* Free pending requests */
    pending_request_t *pr = client->pending_requests;
    while (pr) {
        pending_request_t *next = pr->next;
        pending_request_free(pr);
        pr = next;
    }

    /* Free request handlers */
    request_handler_entry_t *rh = client->request_handlers;
    while (rh) {
        request_handler_entry_t *next = rh->next;
        free(rh->method);
        free(rh);
        rh = next;
    }

    /* Free notification handlers */
    notification_handler_entry_t *nh = client->notification_handlers;
    while (nh) {
        notification_handler_entry_t *next = nh->next;
        free(nh->method);
        free(nh);
        nh = next;
    }

    pthread_mutex_destroy(&client->write_mutex);
    pthread_mutex_destroy(&client->pending_mutex);
    pthread_mutex_destroy(&client->handlers_mutex);
    pthread_mutex_destroy(&client->id_mutex);

    free(client);
}

cJSON *json_rpc_client_request(
    json_rpc_client_t *client,
    const char *method,
    cJSON *params,
    int timeout_ms,
    int *out_error_code,
    char **out_error_message)
{
    if (!client || !method) return NULL;

    if (timeout_ms <= 0) {
        timeout_ms = 30000;
    }

    /* Generate request ID */
    char *id = generate_request_id(client);
    if (!id) return NULL;

    /* Create pending request */
    pending_request_t *pr = pending_request_create(id);
    if (!pr) {
        free(id);
        return NULL;
    }
    add_pending(client, pr);

    /* Build JSON-RPC request */
    cJSON *request = cJSON_CreateObject();
    cJSON_AddStringToObject(request, "jsonrpc", "2.0");
    cJSON_AddStringToObject(request, "id", id);
    cJSON_AddStringToObject(request, "method", method);
    if (params) {
        cJSON_AddItemToObject(request, "params", cJSON_Duplicate(params, 1));
    } else {
        cJSON_AddItemToObject(request, "params", cJSON_CreateObject());
    }

    /* Send */
    int rc = send_message(client, request);
    cJSON_Delete(request);
    free(id);

    if (rc != 0) {
        remove_pending(client, pr);
        pending_request_free(pr);
        if (out_error_code) *out_error_code = -32000;
        if (out_error_message) *out_error_message = strdup("Failed to send request");
        return NULL;
    }

    /* Wait for response with timeout */
    struct timespec ts;
#ifdef _WIN32
    struct timespec now;
    timespec_get(&now, TIME_UTC);
    ts.tv_sec = now.tv_sec + timeout_ms / 1000;
    ts.tv_nsec = now.tv_nsec + (timeout_ms % 1000) * 1000000L;
#else
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (long)(timeout_ms % 1000) * 1000000L;
#endif
    if (ts.tv_nsec >= 1000000000L) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1000000000L;
    }

    pthread_mutex_lock(&pr->mutex);
    while (!pr->completed) {
        int wait_rc = pthread_cond_timedwait(&pr->cond, &pr->mutex, &ts);
        if (wait_rc == ETIMEDOUT) {
            pthread_mutex_unlock(&pr->mutex);
            remove_pending(client, pr);
            pending_request_free(pr);
            if (out_error_code) *out_error_code = -32000;
            if (out_error_message) *out_error_message = strdup("Request timed out");
            return NULL;
        }
    }

    cJSON *result = pr->result;
    pr->result = NULL;  /* Transfer ownership */
    int err_code = pr->error_code;
    char *err_msg = pr->error_message;
    pr->error_message = NULL;  /* Transfer ownership */

    pthread_mutex_unlock(&pr->mutex);
    remove_pending(client, pr);
    pending_request_free(pr);

    if (err_code != 0) {
        if (out_error_code) *out_error_code = err_code;
        if (out_error_message) *out_error_message = err_msg;
        else free(err_msg);
        return NULL;
    }

    free(err_msg);
    return result;
}

int json_rpc_client_notify(
    json_rpc_client_t *client,
    const char *method,
    cJSON *params)
{
    if (!client || !method) return -1;

    cJSON *notification = cJSON_CreateObject();
    cJSON_AddStringToObject(notification, "jsonrpc", "2.0");
    cJSON_AddStringToObject(notification, "method", method);
    if (params) {
        cJSON_AddItemToObject(notification, "params", cJSON_Duplicate(params, 1));
    } else {
        cJSON_AddItemToObject(notification, "params", cJSON_CreateObject());
    }

    int rc = send_message(client, notification);
    cJSON_Delete(notification);
    return rc;
}

void json_rpc_client_set_request_handler(
    json_rpc_client_t *client,
    const char *method,
    json_rpc_request_handler_fn handler,
    void *user_data)
{
    if (!client || !method) return;

    pthread_mutex_lock(&client->handlers_mutex);

    /* Check if handler already exists for this method */
    request_handler_entry_t *entry = client->request_handlers;
    while (entry) {
        if (strcmp(entry->method, method) == 0) {
            if (handler) {
                entry->handler = handler;
                entry->user_data = user_data;
            } else {
                /* Remove handler: unlink from list */
                /* (Simple removal: just NULL out handler) */
                entry->handler = NULL;
                entry->user_data = NULL;
            }
            pthread_mutex_unlock(&client->handlers_mutex);
            return;
        }
        entry = entry->next;
    }

    if (!handler) {
        pthread_mutex_unlock(&client->handlers_mutex);
        return;
    }

    /* Add new entry */
    entry = calloc(1, sizeof(request_handler_entry_t));
    if (!entry) {
        pthread_mutex_unlock(&client->handlers_mutex);
        return;
    }
    entry->method = strdup(method);
    entry->handler = handler;
    entry->user_data = user_data;
    entry->next = client->request_handlers;
    client->request_handlers = entry;

    pthread_mutex_unlock(&client->handlers_mutex);
}

void json_rpc_client_set_notification_handler(
    json_rpc_client_t *client,
    const char *method,
    json_rpc_notification_handler_fn handler,
    void *user_data)
{
    if (!client || !method) return;

    pthread_mutex_lock(&client->handlers_mutex);

    /* Check if handler already exists for this method */
    notification_handler_entry_t *entry = client->notification_handlers;
    while (entry) {
        if (strcmp(entry->method, method) == 0) {
            if (handler) {
                entry->handler = handler;
                entry->user_data = user_data;
            } else {
                entry->handler = NULL;
                entry->user_data = NULL;
            }
            pthread_mutex_unlock(&client->handlers_mutex);
            return;
        }
        entry = entry->next;
    }

    if (!handler) {
        pthread_mutex_unlock(&client->handlers_mutex);
        return;
    }

    /* Add new entry */
    entry = calloc(1, sizeof(notification_handler_entry_t));
    if (!entry) {
        pthread_mutex_unlock(&client->handlers_mutex);
        return;
    }
    entry->method = strdup(method);
    entry->handler = handler;
    entry->user_data = user_data;
    entry->next = client->notification_handlers;
    client->notification_handlers = entry;

    pthread_mutex_unlock(&client->handlers_mutex);
}

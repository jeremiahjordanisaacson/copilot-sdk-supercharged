/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file client.c
 * @brief CopilotClient implementation - spawns CLI process, manages sessions.
 */

#ifndef _POSIX_C_SOURCE
/* 200809L declares strdup() under strict C11 and keeps ETIMEDOUT/clock_gettime available. */
#define _POSIX_C_SOURCE 200809L
#endif

#include "copilot/copilot.h"
#include "copilot/sdk_protocol_version.h"
#include "json_rpc_client.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include "cJSON.h"

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>
#else
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#endif

/* ============================================================================
 * Internal session structure (forward declared in copilot.h as opaque)
 * ============================================================================ */

/* Session event handler entry */
typedef struct session_handler_entry {
    int id;
    const char *event_type_filter;  /* NULL = wildcard */
    copilot_session_event_handler_fn handler;
    void *user_data;
    struct session_handler_entry *next;
} session_handler_entry_t;

struct copilot_session {
    char *session_id;
    char *workspace_path;
    json_rpc_client_t *rpc;
    struct copilot_client *owner;

    /* Event handlers */
    session_handler_entry_t *handlers;
    int next_handler_id;
    pthread_mutex_t handlers_mutex;

    /* Tools */
    copilot_tool_t *tools;
    size_t tools_count;

    /* Permission handler */
    copilot_permission_handler_fn permission_handler;
    void *permission_user_data;

    /* User input handler */
    copilot_user_input_handler_fn user_input_handler;
    void *user_input_user_data;

    /* Hook handler */
    copilot_hook_handler_fn hook_handler;
    void *hook_user_data;

    /* Exit plan mode handler */
    copilot_exit_plan_mode_handler_fn exit_plan_mode_handler;
    void *exit_plan_mode_user_data;

    /* Shell authentication handler (shellAuthentication.request) */
    copilot_shell_authentication_handler_fn shell_authentication_handler;
    void *shell_authentication_user_data;

    /* Memory store callback (memoryStore.store) */
    copilot_memory_store_handler_fn memory_store_handler;
    void *memory_store_user_data;

    /* GitHub token provider registration (removed on destroy) */
    char *github_token_registration_id;

    /* SendAndWait synchronization */
    pthread_mutex_t idle_mutex;
    pthread_cond_t idle_cond;
    bool idle_signaled;
    bool error_signaled;
    char *error_message;
    char *last_assistant_content;

    /* Linked list for client's session tracking */
    struct copilot_session *next;
};

/* ============================================================================
 * Internal client structure
 * ============================================================================ */

/* GitHub token provider registration (gitHubToken.getToken) */
typedef struct github_token_registration {
    char *registration_id;
    char *session_id;  /* NULL until the session ID is known */
    copilot_github_token_provider_fn provider;
    void *user_data;
    struct github_token_registration *next;
} github_token_registration_t;

struct copilot_client {
    /* Options (owned copies) */
    char *cli_path;
    char *cwd;
    char *log_level;
    bool use_stdio;
    bool auto_start;
    bool auto_restart;
    char *github_token;
    bool use_logged_in_user;
    char **extra_args;
    int extra_args_count;

    /* Client info for the connect handshake (owned copies) */
    char *client_info_application_name;
    char *client_info_application_version;
    char *client_info_integration_name;
    char *client_info_integration_version;

    /* Trace context */
    copilot_trace_context_provider_fn trace_context_provider;
    void *trace_context_user_data;

    /* GitHub token providers */
    github_token_registration_t *github_token_providers;
    unsigned long github_token_seq;
    pthread_mutex_t github_token_mutex;

    /* Client mode: "empty" requires available_tools and applies minimal defaults */
    bool empty_mode;

    /* Network callbacks (network.setProvider) */
    char *network_scope;
    copilot_network_resolve_proxy_fn network_resolve_proxy;
    copilot_network_authenticate_negotiate_fn network_authenticate_negotiate;
    copilot_network_authenticate_basic_fn network_authenticate_basic;
    void *network_user_data;

    /* error.data.policyHelperFailure of the last failed session.create/resume (JSON) */
    char *last_policy_helper_failure_json;
    pthread_mutex_t last_error_mutex;

    /* State */
    copilot_connection_state_t state;

    /* Process */
#ifdef _WIN32
    HANDLE process_handle;
    HANDLE stdin_write;
    HANDLE stdout_read;
#else
    pid_t cli_pid;
    int stdin_write_fd;
    int stdout_read_fd;
#endif

    /* JSON-RPC client */
    json_rpc_client_t *rpc;

    /* Sessions */
    copilot_session_t *sessions;
    pthread_mutex_t sessions_mutex;
};

/* ============================================================================
 * Error strings
 * ============================================================================ */

const char *copilot_error_string(copilot_error_t err)
{
    switch (err) {
    case COPILOT_OK:                     return "OK";
    case COPILOT_ERROR_INVALID_ARGUMENT: return "Invalid argument";
    case COPILOT_ERROR_NOT_CONNECTED:    return "Not connected";
    case COPILOT_ERROR_SPAWN_FAILED:     return "Failed to spawn CLI process";
    case COPILOT_ERROR_PROTOCOL_MISMATCH:return "Protocol version mismatch";
    case COPILOT_ERROR_TIMEOUT:          return "Timeout";
    case COPILOT_ERROR_JSON_PARSE:       return "JSON parse error";
    case COPILOT_ERROR_RPC:              return "JSON-RPC error";
    case COPILOT_ERROR_SESSION_NOT_FOUND:return "Session not found";
    case COPILOT_ERROR_TOOL_NOT_FOUND:   return "Tool not found";
    case COPILOT_ERROR_HANDLER_FAILED:   return "Handler failed";
    case COPILOT_ERROR_OUT_OF_MEMORY:    return "Out of memory";
    case COPILOT_ERROR_IO:               return "I/O error";
    case COPILOT_ERROR_ALREADY_STARTED:  return "Already started";
    case COPILOT_ERROR_SESSION_ERROR:    return "Session error";
    default:                             return "Unknown error";
    }
}

/* ============================================================================
 * Defaults
 * ============================================================================ */

copilot_client_options_t copilot_client_options_default(void)
{
    copilot_client_options_t opts;
    memset(&opts, 0, sizeof(opts));
    opts.cli_path = NULL;
    opts.cwd = NULL;
    opts.log_level = "info";
    opts.use_stdio = true;
    opts.auto_start = true;
    opts.auto_restart = true;
    opts.github_token = NULL;
    opts.use_logged_in_user = true;
    opts.extra_args = NULL;
    return opts;
}

copilot_session_config_t copilot_session_config_default(void)
{
    copilot_session_config_t cfg;
    memset(&cfg, 0, sizeof(cfg));
    return cfg;
}

/* ============================================================================
 * Internal: find session by ID
 * ============================================================================ */

static copilot_session_t *find_session(copilot_client_t *client, const char *session_id)
{
    pthread_mutex_lock(&client->sessions_mutex);
    copilot_session_t *s = client->sessions;
    while (s) {
        if (strcmp(s->session_id, session_id) == 0) {
            pthread_mutex_unlock(&client->sessions_mutex);
            return s;
        }
        s = s->next;
    }
    pthread_mutex_unlock(&client->sessions_mutex);
    return NULL;
}

static void add_session(copilot_client_t *client, copilot_session_t *session)
{
    pthread_mutex_lock(&client->sessions_mutex);
    session->next = client->sessions;
    client->sessions = session;
    pthread_mutex_unlock(&client->sessions_mutex);
}

static void remove_session(copilot_client_t *client, copilot_session_t *session)
{
    pthread_mutex_lock(&client->sessions_mutex);
    copilot_session_t **pp = &client->sessions;
    while (*pp) {
        if (*pp == session) {
            *pp = session->next;
            break;
        }
        pp = &(*pp)->next;
    }
    pthread_mutex_unlock(&client->sessions_mutex);
}

/* ============================================================================
 * Internal: JSON-RPC helpers
 * ============================================================================ */

/* JSON-RPC "method not found" error code. */
#define COPILOT_JSONRPC_METHOD_NOT_FOUND (-32601)

/* True when an RPC failed because the connected CLI does not implement the method. */
static bool is_method_not_found_error(int err_code, const char *err_msg)
{
    return err_code == COPILOT_JSONRPC_METHOD_NOT_FOUND ||
           (err_msg && strstr(err_msg, "Unhandled method") != NULL);
}

/*
 * Releases a session on the server with session.detach, falling back to
 * session.destroy on CLIs that predate session.detach. Best-effort.
 */
static void release_session_on_server(json_rpc_client_t *rpc, const char *session_id,
                                      int timeout_ms)
{
    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session_id);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(rpc, "session.detach", params, timeout_ms,
                                            &err_code, &err_msg);
    if (!result && is_method_not_found_error(err_code, err_msg)) {
        free(err_msg);
        err_msg = NULL;
        err_code = 0;
        result = json_rpc_client_request(rpc, "session.destroy", params, timeout_ms,
                                         &err_code, &err_msg);
    }
    cJSON_Delete(params);

    if (result) cJSON_Delete(result);
    free(err_msg);
}

/* Adds a NULL-terminated string list as a JSON array (no-op when list is NULL). */
static void add_string_array(cJSON *obj, const char *key, const char **list)
{
    if (!list) return;
    cJSON *arr = cJSON_CreateArray();
    for (const char **p = list; *p; p++) {
        cJSON_AddItemToArray(arr, cJSON_CreateString(*p));
    }
    cJSON_AddItemToObject(obj, key, arr);
}

/* ============================================================================
 * Internal: GitHub token provider registry (gitHubToken.getToken)
 * ============================================================================ */

static void free_github_token_registration(github_token_registration_t *reg)
{
    if (!reg) return;
    free(reg->registration_id);
    free(reg->session_id);
    free(reg);
}

/*
 * Registers a session's GitHub token provider under a fresh UUID-like ID.
 * Returns a heap copy of the registration ID (caller frees), or NULL on OOM.
 */
static char *register_github_token_provider(copilot_client_t *client,
                                            copilot_github_token_provider_fn provider,
                                            void *user_data,
                                            const char *session_id)
{
    github_token_registration_t *reg = calloc(1, sizeof(github_token_registration_t));
    if (!reg) return NULL;

    pthread_mutex_lock(&client->github_token_mutex);
    unsigned long seq = ++client->github_token_seq;
    pthread_mutex_unlock(&client->github_token_mutex);

    char id_buf[64];
    snprintf(id_buf, sizeof(id_buf), "%08lx-%04x-4%03x-%04x-%04x%08lx",
             (unsigned long)time(NULL) & 0xffffffffUL,
             (unsigned)rand() & 0xffffu,
             (unsigned)rand() & 0x0fffu,
             ((unsigned)rand() & 0x3fffu) | 0x8000u,
             (unsigned)((uintptr_t)client & 0xffffu),
             seq & 0xffffffffUL);

    reg->registration_id = strdup(id_buf);
    reg->session_id = session_id ? strdup(session_id) : NULL;
    reg->provider = provider;
    reg->user_data = user_data;

    char *id_copy = strdup(id_buf);
    if (!reg->registration_id || (session_id && !reg->session_id) || !id_copy) {
        free(id_copy);
        free_github_token_registration(reg);
        return NULL;
    }

    pthread_mutex_lock(&client->github_token_mutex);
    reg->next = client->github_token_providers;
    client->github_token_providers = reg;
    pthread_mutex_unlock(&client->github_token_mutex);

    return id_copy;
}

/* Records the session ID a registration belongs to once it is known. */
static void assign_github_token_provider_session(copilot_client_t *client,
                                                 const char *registration_id,
                                                 const char *session_id)
{
    if (!session_id) return;

    pthread_mutex_lock(&client->github_token_mutex);
    for (github_token_registration_t *r = client->github_token_providers; r; r = r->next) {
        if (strcmp(r->registration_id, registration_id) == 0) {
            char *copy = strdup(session_id);
            if (copy) {
                free(r->session_id);
                r->session_id = copy;
            }
            break;
        }
    }
    pthread_mutex_unlock(&client->github_token_mutex);
}

static void unregister_github_token_provider(copilot_client_t *client, const char *registration_id)
{
    pthread_mutex_lock(&client->github_token_mutex);
    github_token_registration_t **pp = &client->github_token_providers;
    while (*pp) {
        if (strcmp((*pp)->registration_id, registration_id) == 0) {
            github_token_registration_t *reg = *pp;
            *pp = reg->next;
            free_github_token_registration(reg);
            break;
        }
        pp = &(*pp)->next;
    }
    pthread_mutex_unlock(&client->github_token_mutex);
}

static void clear_github_token_providers(copilot_client_t *client)
{
    pthread_mutex_lock(&client->github_token_mutex);
    github_token_registration_t *reg = client->github_token_providers;
    while (reg) {
        github_token_registration_t *next = reg->next;
        free_github_token_registration(reg);
        reg = next;
    }
    client->github_token_providers = NULL;
    pthread_mutex_unlock(&client->github_token_mutex);
}

/* ============================================================================
 * Internal: dispatch session event
 * ============================================================================ */

static void dispatch_session_event(copilot_session_t *session,
                                   const copilot_session_event_t *event)
{
    pthread_mutex_lock(&session->handlers_mutex);
    session_handler_entry_t *h = session->handlers;
    while (h) {
        if (!h->event_type_filter || strcmp(h->event_type_filter, event->type) == 0) {
            h->handler(event, h->user_data);
        }
        h = h->next;
    }
    pthread_mutex_unlock(&session->handlers_mutex);
}

/* ============================================================================
 * Internal: session event notification handler
 * ============================================================================ */

static void on_session_event_notification(const char *method, cJSON *params, void *user_data)
{
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *session_id_item = cJSON_GetObjectItem(params, "sessionId");
    cJSON *event_item = cJSON_GetObjectItem(params, "event");
    if (!session_id_item || !cJSON_IsString(session_id_item) || !event_item) return;

    const char *session_id = session_id_item->valuestring;
    copilot_session_t *session = find_session(client, session_id);
    if (!session) return;

    /* Build event struct */
    copilot_session_event_t event;
    memset(&event, 0, sizeof(event));
    copilot_shell_output_data_t shell_output;
    memset(&shell_output, 0, sizeof(shell_output));

    cJSON *type_item = cJSON_GetObjectItem(event_item, "type");
    if (type_item && cJSON_IsString(type_item)) {
        event.type = type_item->valuestring;
    } else {
        event.type = "unknown";
    }

    char *raw = cJSON_PrintUnformatted(event_item);
    event.raw_json = raw;

    /* Parse convenience fields */
    cJSON *data = cJSON_GetObjectItem(event_item, "data");
    if (data) {
        cJSON *content = cJSON_GetObjectItem(data, "content");
        if (content && cJSON_IsString(content)) {
            event.content = content->valuestring;
        }
        cJSON *message = cJSON_GetObjectItem(data, "message");
        if (message && cJSON_IsString(message)) {
            event.message = message->valuestring;
        }
        cJSON *delta_content = cJSON_GetObjectItem(data, "deltaContent");
        if (delta_content && cJSON_IsString(delta_content)) {
            event.delta_content = delta_content->valuestring;
        }
        cJSON *tool_name = cJSON_GetObjectItem(data, "toolName");
        if (tool_name && cJSON_IsString(tool_name)) {
            event.tool_name = tool_name->valuestring;
        }
        cJSON *tool_call_id = cJSON_GetObjectItem(data, "toolCallId");
        if (tool_call_id && cJSON_IsString(tool_call_id)) {
            event.tool_call_id = tool_call_id->valuestring;
        }
        if (strcmp(event.type, COPILOT_EVENT_TOOL_SHELL_OUTPUT) == 0) {
            cJSON *text = cJSON_GetObjectItem(data, "text");
            cJSON *sequence = cJSON_GetObjectItem(data, "sequence");
            cJSON *stream = cJSON_GetObjectItem(data, "stream");
            shell_output.tool_call_id = event.tool_call_id;
            shell_output.text = (text && cJSON_IsString(text)) ? text->valuestring : "";
            shell_output.sequence = (sequence && cJSON_IsNumber(sequence))
                                        ? (int64_t)sequence->valuedouble : 0;
            shell_output.stream = (stream && cJSON_IsString(stream)) ? stream->valuestring : NULL;
            event.shell_output = &shell_output;
        }
    }

    /* Dispatch to handlers */
    dispatch_session_event(session, &event);

    /* Update SendAndWait state */
    if (strcmp(event.type, "assistant.message") == 0 && event.content) {
        pthread_mutex_lock(&session->idle_mutex);
        free(session->last_assistant_content);
        session->last_assistant_content = strdup(event.content);
        pthread_mutex_unlock(&session->idle_mutex);
    } else if (strcmp(event.type, "session.idle") == 0) {
        pthread_mutex_lock(&session->idle_mutex);
        session->idle_signaled = true;
        pthread_cond_signal(&session->idle_cond);
        pthread_mutex_unlock(&session->idle_mutex);
    } else if (strcmp(event.type, "session.error") == 0) {
        pthread_mutex_lock(&session->idle_mutex);
        session->error_signaled = true;
        free(session->error_message);
        session->error_message = event.message ? strdup(event.message) : strdup("Session error");
        pthread_cond_signal(&session->idle_cond);
        pthread_mutex_unlock(&session->idle_mutex);
    }

    free(raw);
}

/* ============================================================================
 * Internal: tool call request handler
 * ============================================================================ */

static cJSON *on_tool_call_request(const char *method, cJSON *params, void *user_data,
                                   int *out_error_code, char **out_error_message)
{
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *session_id_item = cJSON_GetObjectItem(params, "sessionId");
    cJSON *tool_call_id_item = cJSON_GetObjectItem(params, "toolCallId");
    cJSON *tool_name_item = cJSON_GetObjectItem(params, "toolName");
    cJSON *arguments_item = cJSON_GetObjectItem(params, "arguments");

    if (!session_id_item || !tool_call_id_item || !tool_name_item) {
        *out_error_code = -32602;
        *out_error_message = strdup("Invalid tool call params");
        return NULL;
    }

    const char *session_id = session_id_item->valuestring;
    const char *tool_name = tool_name_item->valuestring;
    const char *tool_call_id = tool_call_id_item->valuestring;

    copilot_session_t *session = find_session(client, session_id);
    if (!session) {
        *out_error_code = -32000;
        *out_error_message = strdup("Session not found");
        return NULL;
    }

    /* Find tool handler */
    copilot_tool_handler_fn handler = NULL;
    void *handler_user_data = NULL;
    for (size_t i = 0; i < session->tools_count; i++) {
        if (strcmp(session->tools[i].name, tool_name) == 0) {
            handler = session->tools[i].handler;
            handler_user_data = session->tools[i].user_data;
            break;
        }
    }

    cJSON *result_obj = cJSON_CreateObject();
    cJSON *result_inner = cJSON_CreateObject();

    if (!handler) {
        char msg[256];
        snprintf(msg, sizeof(msg), "Tool '%s' is not supported by this client instance.", tool_name);
        cJSON_AddStringToObject(result_inner, "textResultForLlm", msg);
        cJSON_AddStringToObject(result_inner, "resultType", "failure");
        snprintf(msg, sizeof(msg), "tool '%s' not supported", tool_name);
        cJSON_AddStringToObject(result_inner, "error", msg);
    } else {
        char *args_json = arguments_item ? cJSON_PrintUnformatted(arguments_item) : strdup("{}");

        copilot_tool_invocation_t invocation;
        invocation.session_id = session_id;
        invocation.tool_call_id = tool_call_id;
        invocation.tool_name = tool_name;
        invocation.arguments_json = args_json;

        copilot_tool_result_t tool_result;
        memset(&tool_result, 0, sizeof(tool_result));

        copilot_error_t err = handler(&invocation, handler_user_data, &tool_result);

        if (err != COPILOT_OK || !tool_result.text_result_for_llm) {
            cJSON_AddStringToObject(result_inner, "textResultForLlm",
                "Invoking this tool produced an error. Detailed information is not available.");
            cJSON_AddStringToObject(result_inner, "resultType", "failure");
            if (tool_result.error) {
                cJSON_AddStringToObject(result_inner, "error", tool_result.error);
            }
        } else {
            cJSON_AddStringToObject(result_inner, "textResultForLlm",
                                    tool_result.text_result_for_llm);
            const char *type_str;
            switch (tool_result.result_type) {
            case COPILOT_TOOL_RESULT_FAILURE:  type_str = "failure"; break;
            case COPILOT_TOOL_RESULT_REJECTED: type_str = "rejected"; break;
            case COPILOT_TOOL_RESULT_DENIED:   type_str = "denied"; break;
            default:                           type_str = "success"; break;
            }
            cJSON_AddStringToObject(result_inner, "resultType", type_str);
            if (tool_result.error) {
                cJSON_AddStringToObject(result_inner, "error", tool_result.error);
            }
        }
        free(args_json);
    }

    cJSON_AddItemToObject(result_obj, "result", result_inner);
    return result_obj;
}

/* ============================================================================
 * Internal: permission request handler
 * ============================================================================ */

static cJSON *on_permission_request(const char *method, cJSON *params, void *user_data,
                                    int *out_error_code, char **out_error_message)
{
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *session_id_item = cJSON_GetObjectItem(params, "sessionId");
    cJSON *perm_req_item = cJSON_GetObjectItem(params, "permissionRequest");

    if (!session_id_item || !cJSON_IsString(session_id_item) || !perm_req_item) {
        *out_error_code = -32602;
        *out_error_message = strdup("Invalid permission request params");
        return NULL;
    }

    copilot_session_t *session = find_session(client, session_id_item->valuestring);
    if (!session || !session->permission_handler) {
        /* Deny by default */
        cJSON *result = cJSON_CreateObject();
        cJSON *inner = cJSON_CreateObject();
        cJSON_AddStringToObject(inner, "kind",
            "denied-no-approval-rule-and-could-not-request-from-user");
        cJSON_AddItemToObject(result, "result", inner);
        return result;
    }

    copilot_permission_request_t request;
    memset(&request, 0, sizeof(request));

    cJSON *kind_item = cJSON_GetObjectItem(perm_req_item, "kind");
    if (kind_item && cJSON_IsString(kind_item)) {
        request.kind = kind_item->valuestring;
    }
    cJSON *tcid = cJSON_GetObjectItem(perm_req_item, "toolCallId");
    if (tcid && cJSON_IsString(tcid)) {
        request.tool_call_id = tcid->valuestring;
    }
    char *raw = cJSON_PrintUnformatted(perm_req_item);
    request.raw_json = raw;

    copilot_permission_result_t perm_result;
    memset(&perm_result, 0, sizeof(perm_result));

    copilot_error_t err = session->permission_handler(
        &request, session->session_id, session->permission_user_data, &perm_result);

    free(raw);

    cJSON *result = cJSON_CreateObject();
    cJSON *inner = cJSON_CreateObject();

    if (err != COPILOT_OK) {
        cJSON_AddStringToObject(inner, "kind",
            "denied-no-approval-rule-and-could-not-request-from-user");
    } else {
        const char *kind_str;
        switch (perm_result.kind) {
        case COPILOT_PERMISSION_APPROVED:        kind_str = "approved"; break;
        case COPILOT_PERMISSION_DENIED_BY_RULES: kind_str = "denied-by-rules"; break;
        case COPILOT_PERMISSION_DENIED_BY_USER:  kind_str = "denied-interactively-by-user"; break;
        default:
            kind_str = "denied-no-approval-rule-and-could-not-request-from-user"; break;
        }
        cJSON_AddStringToObject(inner, "kind", kind_str);
    }

    if (perm_result.decision_context_json) {
        cJSON *dctx = cJSON_Parse(perm_result.decision_context_json);
        if (dctx) {
            cJSON_AddItemToObject(inner, "decisionContext", dctx);
        }
    }

    cJSON_AddItemToObject(result, "result", inner);
    return result;
}

/* ============================================================================
 * Internal: user input request handler
 * ============================================================================ */

static cJSON *on_user_input_request(const char *method, cJSON *params, void *user_data,
                                    int *out_error_code, char **out_error_message)
{
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *session_id_item = cJSON_GetObjectItem(params, "sessionId");
    cJSON *question_item = cJSON_GetObjectItem(params, "question");

    if (!session_id_item) {
        *out_error_code = -32602;
        *out_error_message = strdup("Invalid user input request params");
        return NULL;
    }

    copilot_session_t *session = find_session(client, session_id_item->valuestring);
    if (!session || !session->user_input_handler) {
        *out_error_code = -32000;
        *out_error_message = strdup("No user input handler registered");
        return NULL;
    }

    copilot_user_input_request_t request;
    memset(&request, 0, sizeof(request));
    request.question = (question_item && cJSON_IsString(question_item)) ? question_item->valuestring : NULL;
    request.allow_freeform = false;
    request.has_allow_freeform = false;

    cJSON *choices_item = cJSON_GetObjectItem(params, "choices");
    const char *choices_arr[64];
    if (choices_item && cJSON_IsArray(choices_item)) {
        int count = cJSON_GetArraySize(choices_item);
        if (count > 63) count = 63;
        request.choices_count = count;
        for (int i = 0; i < count; i++) {
            cJSON *c = cJSON_GetArrayItem(choices_item, i);
            choices_arr[i] = cJSON_IsString(c) ? c->valuestring : "";
        }
        choices_arr[count] = NULL;
        request.choices = choices_arr;
    }

    cJSON *freeform_item = cJSON_GetObjectItem(params, "allowFreeform");
    if (freeform_item && cJSON_IsBool(freeform_item)) {
        request.allow_freeform = cJSON_IsTrue(freeform_item);
        request.has_allow_freeform = true;
    }

    copilot_user_input_response_t response;
    memset(&response, 0, sizeof(response));

    copilot_error_t err = session->user_input_handler(
        &request, session->session_id, session->user_input_user_data, &response);

    if (err != COPILOT_OK || !response.answer) {
        *out_error_code = -32000;
        *out_error_message = strdup("User input handler failed");
        free(response.answer);
        return NULL;
    }

    cJSON *result = cJSON_CreateObject();
    cJSON_AddStringToObject(result, "answer", response.answer);
    cJSON_AddBoolToObject(result, "wasFreeform", response.was_freeform);
    free(response.answer);
    return result;
}

/* ============================================================================
 * Internal: hooks invoke handler
 * ============================================================================ */

static cJSON *on_hooks_invoke(const char *method, cJSON *params, void *user_data,
                              int *out_error_code, char **out_error_message)
{
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *session_id_item = cJSON_GetObjectItem(params, "sessionId");
    cJSON *hook_type_item = cJSON_GetObjectItem(params, "hookType");
    cJSON *input_item = cJSON_GetObjectItem(params, "input");

    if (!session_id_item || !hook_type_item) {
        *out_error_code = -32602;
        *out_error_message = strdup("Invalid hooks invoke params");
        return NULL;
    }

    copilot_session_t *session = find_session(client, session_id_item->valuestring);
    if (!session || !session->hook_handler) {
        cJSON *result = cJSON_CreateObject();
        return result;
    }

    char *input_json = input_item ? cJSON_PrintUnformatted(input_item) : strdup("{}");
    char *output_json = session->hook_handler(
        hook_type_item->valuestring, input_json,
        session->session_id, session->hook_user_data);
    free(input_json);

    cJSON *result = cJSON_CreateObject();
    if (output_json) {
        cJSON *output = cJSON_Parse(output_json);
        if (output) {
            cJSON_AddItemToObject(result, "output", output);
        }
        free(output_json);
    }
    return result;
}

/* ============================================================================
 * Internal: exit plan mode request handler
 * ============================================================================ */

static cJSON *on_exit_plan_mode_request(const char *method, cJSON *params, void *user_data,
                                        int *out_error_code, char **out_error_message)
{
    (void)method;
    (void)out_error_code;
    (void)out_error_message;

    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *sid_json = cJSON_GetObjectItem(params, "sessionId");
    const char *sid = (sid_json && cJSON_IsString(sid_json)) ? sid_json->valuestring : "";

    copilot_session_t *session = find_session(client, sid);

    cJSON *response = cJSON_CreateObject();

    if (session && session->exit_plan_mode_handler) {
        copilot_exit_plan_mode_request_t req;
        memset(&req, 0, sizeof(req));
        req.session_id = sid;

        cJSON *summary_item = cJSON_GetObjectItem(params, "summary");
        req.summary = (summary_item && cJSON_IsString(summary_item)) ? summary_item->valuestring : NULL;

        cJSON *plan_item = cJSON_GetObjectItem(params, "planContent");
        req.plan_content = (plan_item && cJSON_IsString(plan_item)) ? plan_item->valuestring : NULL;

        cJSON *rec_item = cJSON_GetObjectItem(params, "recommendedAction");
        req.recommended_action = (rec_item && cJSON_IsString(rec_item)) ? rec_item->valuestring : NULL;

        const char *actions_arr[64];
        cJSON *actions_item = cJSON_GetObjectItem(params, "actions");
        if (actions_item && cJSON_IsArray(actions_item)) {
            int count = cJSON_GetArraySize(actions_item);
            if (count > 63) count = 63;
            for (int i = 0; i < count; i++) {
                cJSON *a = cJSON_GetArrayItem(actions_item, i);
                actions_arr[i] = cJSON_IsString(a) ? a->valuestring : "";
            }
            actions_arr[count] = NULL;
            req.actions = actions_arr;
            req.actions_count = (size_t)count;
        }

        copilot_exit_plan_mode_response_t resp;
        memset(&resp, 0, sizeof(resp));
        resp.approved = true;

        copilot_error_t err = session->exit_plan_mode_handler(&req, session->exit_plan_mode_user_data, &resp);
        cJSON_AddBoolToObject(response, "approved", (err == COPILOT_OK) ? resp.approved : true);
        if (err == COPILOT_OK && resp.selected_action) {
            cJSON_AddStringToObject(response, "selectedAction", resp.selected_action);
        }
        if (err == COPILOT_OK && resp.feedback) {
            cJSON_AddStringToObject(response, "feedback", resp.feedback);
        }
    } else {
        /* Default: approve */
        cJSON_AddBoolToObject(response, "approved", true);
    }

    return response;
}

/* ============================================================================
 * Internal: GitHub token request handler (gitHubToken.getToken)
 * ============================================================================ */

static cJSON *on_github_token_get_token(const char *method, cJSON *params, void *user_data,
                                        int *out_error_code, char **out_error_message)
{
    (void)method;
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *reg_item = cJSON_GetObjectItem(params, "registrationId");
    cJSON *host_item = cJSON_GetObjectItem(params, "host");
    cJSON *sid_item = cJSON_GetObjectItem(params, "sessionId");
    cJSON *reason_item = cJSON_GetObjectItem(params, "reason");

    if (!reg_item || !cJSON_IsString(reg_item)) {
        *out_error_code = -32602;
        *out_error_message = strdup("Invalid gitHubToken.getToken params");
        return NULL;
    }

    copilot_github_token_provider_fn provider = NULL;
    void *provider_user_data = NULL;
    char *registered_session_id = NULL;

    pthread_mutex_lock(&client->github_token_mutex);
    for (github_token_registration_t *r = client->github_token_providers; r; r = r->next) {
        if (strcmp(r->registration_id, reg_item->valuestring) == 0) {
            provider = r->provider;
            provider_user_data = r->user_data;
            registered_session_id = r->session_id ? strdup(r->session_id) : NULL;
            break;
        }
    }
    pthread_mutex_unlock(&client->github_token_mutex);

    if (!provider) {
        char msg[256];
        snprintf(msg, sizeof(msg),
                 "No GitHub token provider registered for registration ID \"%s\"",
                 reg_item->valuestring);
        *out_error_code = -32603;
        *out_error_message = strdup(msg);
        return NULL;
    }

    copilot_github_token_provider_args_t args;
    memset(&args, 0, sizeof(args));
    args.host = (host_item && cJSON_IsString(host_item)) ? host_item->valuestring : "";
    args.session_id = (sid_item && cJSON_IsString(sid_item)) ? sid_item->valuestring
                                                              : registered_session_id;
    args.reason = (reason_item && cJSON_IsString(reason_item)) ? reason_item->valuestring
                                                                : "initial";

    copilot_github_token_result_t token_result;
    memset(&token_result, 0, sizeof(token_result));

    copilot_error_t err = provider(&args, provider_user_data, &token_result);
    free(registered_session_id);

    cJSON *result = NULL;
    if (err != COPILOT_OK) {
        *out_error_code = -32603;
        *out_error_message = strdup("GitHub token provider failed");
    } else if (token_result.kind == COPILOT_GITHUB_TOKEN_RESULT_CANCELLED) {
        result = cJSON_CreateObject();
        cJSON_AddStringToObject(result, "kind", "cancelled");
    } else if (!token_result.access_token) {
        *out_error_code = -32603;
        *out_error_message = strdup("GitHub token provider returned no access token");
    } else {
        result = cJSON_CreateObject();
        cJSON_AddStringToObject(result, "kind", "token");
        cJSON_AddStringToObject(result, "accessToken", token_result.access_token);
        if (token_result.token_type) {
            cJSON_AddStringToObject(result, "tokenType", token_result.token_type);
        }
        cJSON_AddNumberToObject(result, "expiresIn", (double)token_result.expires_in);
    }

    free(token_result.access_token);
    free(token_result.token_type);
    return result;
}

/* ============================================================================
 * Internal: v3.0.0 server request handlers (shell authentication, memory store,
 * network callbacks)
 * ============================================================================ */

/* Returns obj[key] when it is a string, else fallback. */
static const char *json_string_or(cJSON *obj, const char *key, const char *fallback)
{
    cJSON *item = cJSON_GetObjectItemCaseSensitive(obj, key);
    return (item && cJSON_IsString(item)) ? item->valuestring : fallback;
}

/* Overwrites a heap-allocated secret before freeing it. */
static void free_secret(char *secret)
{
    if (!secret) return;
    volatile char *p = secret;
    while (*p) {
        *p = '\0';
        p++;
    }
    free(secret);
}

/* shellAuthentication.request: answers a shell prompt with {action:"accept", value} or {action:"cancel"}. */
static cJSON *on_shell_authentication_request(const char *method, cJSON *params, void *user_data,
                                              int *out_error_code, char **out_error_message)
{
    (void)method;
    copilot_client_t *client = (copilot_client_t *)user_data;

    const char *session_id = json_string_or(params, "sessionId", NULL);
    const char *request_id = json_string_or(params, "requestId", NULL);
    if (!session_id || !request_id) {
        *out_error_code = -32602;
        *out_error_message = strdup("Invalid shellAuthentication.request params");
        return NULL;
    }

    cJSON *result = cJSON_CreateObject();
    copilot_session_t *session = find_session(client, session_id);
    if (!session || !session->shell_authentication_handler) {
        cJSON_AddStringToObject(result, "action", "cancel");
        return result;
    }

    copilot_shell_authentication_request_t request;
    memset(&request, 0, sizeof(request));
    request.session_id = session_id;
    request.request_id = request_id;
    request.program = json_string_or(params, "program", "");
    request.prompt = json_string_or(params, "prompt", "");
    request.input_kind = json_string_or(params, "inputKind", COPILOT_SHELL_AUTH_INPUT_SECRET);
    request.command = json_string_or(params, "command", "");
    request.cwd = json_string_or(params, "cwd", "");

    copilot_shell_authentication_response_t response;
    memset(&response, 0, sizeof(response));
    response.action = COPILOT_SHELL_AUTH_CANCEL;

    copilot_error_t err = session->shell_authentication_handler(
        &request, session->shell_authentication_user_data, &response);

    if (err == COPILOT_OK && response.action == COPILOT_SHELL_AUTH_ACCEPT && response.value) {
        cJSON_AddStringToObject(result, "action", "accept");
        cJSON_AddStringToObject(result, "value", response.value);
    } else {
        cJSON_AddStringToObject(result, "action", "cancel");
    }
    free_secret(response.value);
    return result;
}

/*
 * shellAuthentication.cancel: requests are answered synchronously on the reader
 * thread, so a cancellation always arrives after its request was answered.
 * Acknowledge it with {}.
 */
static cJSON *on_shell_authentication_cancel(const char *method, cJSON *params, void *user_data,
                                             int *out_error_code, char **out_error_message)
{
    (void)method;
    (void)params;
    (void)user_data;
    (void)out_error_code;
    (void)out_error_message;
    return cJSON_CreateObject();
}

/* memoryStore.store: hands a stored memory to the session's callback and replies null. */
static cJSON *on_memory_store(const char *method, cJSON *params, void *user_data,
                              int *out_error_code, char **out_error_message)
{
    (void)method;
    copilot_client_t *client = (copilot_client_t *)user_data;

    const char *session_id = json_string_or(params, "sessionId", NULL);
    copilot_session_t *session = session_id ? find_session(client, session_id) : NULL;
    if (!session || !session->memory_store_handler) {
        *out_error_code = -32603;
        *out_error_message = strdup("Memory store owner is unavailable");
        return NULL;
    }

    copilot_stored_memory_t memory;
    memset(&memory, 0, sizeof(memory));
    memory.session_id = session_id;
    memory.subject = json_string_or(params, "subject", "");
    memory.fact = json_string_or(params, "fact", "");
    memory.citations = json_string_or(params, "citations", "");
    memory.reason = json_string_or(params, "reason", "");

    if (session->memory_store_handler(&memory, session->memory_store_user_data) != COPILOT_OK) {
        *out_error_code = -32603;
        *out_error_message = strdup("Memory store callback failed");
        return NULL;
    }
    return cJSON_CreateNull();
}

/* network.resolveProxy: {url} -> {proxy?} */
static cJSON *on_network_resolve_proxy(const char *method, cJSON *params, void *user_data,
                                       int *out_error_code, char **out_error_message)
{
    (void)method;
    (void)out_error_code;
    (void)out_error_message;
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *result = cJSON_CreateObject();
    if (!client->network_resolve_proxy) return result;

    char *proxy = client->network_resolve_proxy(json_string_or(params, "url", ""),
                                                client->network_user_data);
    if (proxy) {
        if (proxy[0] != '\0') {
            cJSON_AddStringToObject(result, "proxy", proxy);
        }
        free(proxy);
    }
    return result;
}

/* network.authenticateNegotiate: {spn} -> {token?} */
static cJSON *on_network_authenticate_negotiate(const char *method, cJSON *params, void *user_data,
                                                int *out_error_code, char **out_error_message)
{
    (void)method;
    (void)out_error_code;
    (void)out_error_message;
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *result = cJSON_CreateObject();
    if (!client->network_authenticate_negotiate) return result;

    char *token = client->network_authenticate_negotiate(json_string_or(params, "spn", ""),
                                                         client->network_user_data);
    if (token) {
        if (token[0] != '\0') {
            cJSON_AddStringToObject(result, "token", token);
        }
        free_secret(token);
    }
    return result;
}

/* network.authenticateBasic: {proxy, realm?, previousRejected} -> {username?, password?} */
static cJSON *on_network_authenticate_basic(const char *method, cJSON *params, void *user_data,
                                            int *out_error_code, char **out_error_message)
{
    (void)method;
    copilot_client_t *client = (copilot_client_t *)user_data;

    cJSON *result = cJSON_CreateObject();
    if (!client->network_authenticate_basic) return result;

    copilot_network_basic_auth_request_t request;
    memset(&request, 0, sizeof(request));
    request.proxy = json_string_or(params, "proxy", "");
    request.realm = json_string_or(params, "realm", NULL);
    cJSON *rejected = cJSON_GetObjectItemCaseSensitive(params, "previousRejected");
    request.previous_rejected = rejected && cJSON_IsTrue(rejected);

    copilot_network_basic_credentials_t credentials;
    memset(&credentials, 0, sizeof(credentials));

    copilot_error_t err = client->network_authenticate_basic(&request, client->network_user_data,
                                                             &credentials);
    if (err != COPILOT_OK) {
        cJSON_Delete(result);
        result = NULL;
        *out_error_code = -32603;
        *out_error_message = strdup("Network Basic authentication callback failed");
    } else if (credentials.username && credentials.username[0] != '\0') {
        cJSON_AddStringToObject(result, "username", credentials.username);
        if (credentials.password && credentials.password[0] != '\0') {
            cJSON_AddStringToObject(result, "password", credentials.password);
        }
    }

    free_secret(credentials.username);
    free_secret(credentials.password);
    return result;
}

/* ============================================================================
 * Internal: spawn CLI process
 * ============================================================================ */

/* True when the CLI path is a JavaScript entry point (such as npm-loader.js) that runs under node. */
static bool cli_path_is_javascript(const char *path)
{
    size_t len = strlen(path);
    return len > 3 && (strcmp(path + len - 3, ".js") == 0 || strcmp(path + len - 3, ".JS") == 0);
}

#ifdef _WIN32

static copilot_error_t spawn_cli_process(copilot_client_t *client)
{
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;

    HANDLE stdin_read, stdin_write, stdout_read, stdout_write;

    if (!CreatePipe(&stdin_read, &stdin_write, &sa, 0)) return COPILOT_ERROR_SPAWN_FAILED;
    SetHandleInformation(stdin_write, HANDLE_FLAG_INHERIT, 0);

    if (!CreatePipe(&stdout_read, &stdout_write, &sa, 0)) {
        CloseHandle(stdin_read);
        CloseHandle(stdin_write);
        return COPILOT_ERROR_SPAWN_FAILED;
    }
    SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, 0);

    /* Build command line */
    const char *cli_path = client->cli_path ? client->cli_path : "copilot";
    bool is_js = cli_path_is_javascript(cli_path);

    /* Estimate command line length */
    size_t cmd_len = strlen(cli_path) + 256;
    for (int i = 0; i < client->extra_args_count; i++) {
        cmd_len += strlen(client->extra_args[i]) + 3;
    }
    char *cmd_line = malloc(cmd_len);
    if (!cmd_line) {
        CloseHandle(stdin_read); CloseHandle(stdin_write);
        CloseHandle(stdout_read); CloseHandle(stdout_write);
        return COPILOT_ERROR_OUT_OF_MEMORY;
    }

    /* JavaScript entry points (npm-loader.js, legacy index.js) run under node */
    int pos = is_js ? snprintf(cmd_line, cmd_len, "node \"%s\"", cli_path)
                    : snprintf(cmd_line, cmd_len, "\"%s\"", cli_path);

    /* Add extra args first */
    for (int i = 0; i < client->extra_args_count; i++) {
        pos += snprintf(cmd_line + pos, cmd_len - pos, " %s", client->extra_args[i]);
    }

    /* Add standard args */
    pos += snprintf(cmd_line + pos, cmd_len - pos,
                    " --headless --no-auto-update --log-level %s --stdio",
                    client->log_level ? client->log_level : "info");

    if (client->github_token) {
        pos += snprintf(cmd_line + pos, cmd_len - pos, " --auth-token-env COPILOT_SDK_AUTH_TOKEN");
    }
    if (!client->use_logged_in_user) {
        pos += snprintf(cmd_line + pos, cmd_len - pos, " --no-auto-login");
    }

    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.hStdInput = stdin_read;
    si.hStdOutput = stdout_write;
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.dwFlags |= STARTF_USESTDHANDLES;

    /* Set environment if github token provided */
    char *env_block = NULL;
    if (client->github_token) {
        char env_var[4096];
        snprintf(env_var, sizeof(env_var), "COPILOT_SDK_AUTH_TOKEN=%s", client->github_token);
        /* For simplicity, pass through environment with additional var */
        SetEnvironmentVariableA("COPILOT_SDK_AUTH_TOKEN", client->github_token);
    }

    BOOL success = CreateProcessA(
        NULL, cmd_line, NULL, NULL, TRUE,
        CREATE_NO_WINDOW, env_block, client->cwd,
        &si, &pi);

    free(cmd_line);
    CloseHandle(stdin_read);
    CloseHandle(stdout_write);

    if (!success) {
        CloseHandle(stdin_write);
        CloseHandle(stdout_read);
        return COPILOT_ERROR_SPAWN_FAILED;
    }

    CloseHandle(pi.hThread);
    client->process_handle = pi.hProcess;
    client->stdin_write = stdin_write;
    client->stdout_read = stdout_read;

    return COPILOT_OK;
}

#else /* POSIX */

static copilot_error_t spawn_cli_process(copilot_client_t *client)
{
    int stdin_pipe[2];
    int stdout_pipe[2];

    if (pipe(stdin_pipe) != 0) return COPILOT_ERROR_SPAWN_FAILED;
    if (pipe(stdout_pipe) != 0) {
        close(stdin_pipe[0]);
        close(stdin_pipe[1]);
        return COPILOT_ERROR_SPAWN_FAILED;
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(stdin_pipe[0]); close(stdin_pipe[1]);
        close(stdout_pipe[0]); close(stdout_pipe[1]);
        return COPILOT_ERROR_SPAWN_FAILED;
    }

    if (pid == 0) {
        /* Child process */
        close(stdin_pipe[1]);   /* Close write end of stdin */
        close(stdout_pipe[0]);  /* Close read end of stdout */

        dup2(stdin_pipe[0], STDIN_FILENO);
        dup2(stdout_pipe[1], STDOUT_FILENO);
        /* Leave stderr as-is for debug logs */

        close(stdin_pipe[0]);
        close(stdout_pipe[1]);

        if (client->cwd) {
            if (chdir(client->cwd) != 0) {
                _exit(1);
            }
        }

        if (client->github_token) {
            setenv("COPILOT_SDK_AUTH_TOKEN", client->github_token, 1);
        }
        /* Remove NODE_DEBUG to prevent pollution of stdout */
        unsetenv("NODE_DEBUG");

        const char *cli_path = client->cli_path ? client->cli_path : "copilot";
        /* JavaScript entry points (npm-loader.js, legacy index.js) run under node */
        bool is_js = cli_path_is_javascript(cli_path);

        /* Build argv: [node] cli_path [extra_args...] --headless --no-auto-update --log-level <level> --stdio */
        int base_args = 7; /* cli_path + 5 standard args + NULL */
        int extra_count = client->extra_args_count;
        int auth_args = 0;
        if (client->github_token) auth_args += 2;
        if (!client->use_logged_in_user) auth_args += 1;

        int total = extra_count + base_args + auth_args + (is_js ? 1 : 0);
        char **argv = calloc((size_t)(total + 1), sizeof(char *));
        if (!argv) _exit(1);
        int idx = 0;

        if (is_js) argv[idx++] = "node";
        argv[idx++] = (char *)cli_path;
        for (int i = 0; i < extra_count; i++) {
            argv[idx++] = client->extra_args[i];
        }
        argv[idx++] = "--headless";
        argv[idx++] = "--no-auto-update";
        argv[idx++] = "--log-level";
        argv[idx++] = client->log_level ? client->log_level : "info";
        argv[idx++] = "--stdio";
        if (client->github_token) {
            argv[idx++] = "--auth-token-env";
            argv[idx++] = "COPILOT_SDK_AUTH_TOKEN";
        }
        if (!client->use_logged_in_user) {
            argv[idx++] = "--no-auto-login";
        }
        argv[idx] = NULL;

        execvp(is_js ? "node" : cli_path, argv);
        /* If exec fails */
        _exit(1);
    }

    /* Parent process */
    close(stdin_pipe[0]);   /* Close read end of stdin */
    close(stdout_pipe[1]);  /* Close write end of stdout */

    client->cli_pid = pid;
    client->stdin_write_fd = stdin_pipe[1];
    client->stdout_read_fd = stdout_pipe[0];

    return COPILOT_OK;
}

#endif /* _WIN32 */

/* ============================================================================
 * Internal: verify protocol version
 * ============================================================================ */

static copilot_error_t verify_protocol_version(copilot_client_t *client)
{
    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "message", "sdk-init");

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "ping", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON *pv = cJSON_GetObjectItem(result, "protocolVersion");
    if (!pv || !cJSON_IsNumber(pv)) {
        cJSON_Delete(result);
        fprintf(stderr,
            "SDK protocol version mismatch: SDK supports version %d, but server does not "
            "report a protocol version. Please update your SDK or server to ensure "
            "compatibility.\n",
            COPILOT_SDK_PROTOCOL_VERSION);
        return COPILOT_ERROR_PROTOCOL_MISMATCH;
    }

    int server_version = pv->valueint;
    cJSON_Delete(result);

    if (server_version != COPILOT_SDK_PROTOCOL_VERSION) {
        fprintf(stderr,
            "SDK protocol version mismatch: SDK supports version %d, but server reports "
            "version %d. Please update your SDK or server to ensure compatibility.\n",
            COPILOT_SDK_PROTOCOL_VERSION, server_version);
        return COPILOT_ERROR_PROTOCOL_MISMATCH;
    }

    return COPILOT_OK;
}

/* ============================================================================
 * Internal: network callbacks registration (network.setProvider)
 * ============================================================================ */

/* Registers the configured network callbacks; each present member marks one callback. */
static copilot_error_t register_network_provider(copilot_client_t *client)
{
    if (!client->network_resolve_proxy && !client->network_authenticate_negotiate &&
        !client->network_authenticate_basic) {
        return COPILOT_OK;
    }

    cJSON *params = cJSON_CreateObject();
    if (client->network_scope) {
        cJSON_AddStringToObject(params, "scope", client->network_scope);
    }
    if (client->network_resolve_proxy) {
        cJSON_AddObjectToObject(params, "resolveProxy");
    }
    if (client->network_authenticate_negotiate) {
        cJSON_AddObjectToObject(params, "authenticateNegotiate");
    }
    if (client->network_authenticate_basic) {
        cJSON_AddObjectToObject(params, "authenticateBasic");
    }

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "network.setProvider", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

/* ============================================================================
 * Internal: connect handshake (clientInfo)
 * ============================================================================ */

static void add_non_empty_string(cJSON *obj, const char *key, const char *value)
{
    if (value && value[0] != '\0') {
        cJSON_AddStringToObject(obj, key, value);
    }
}

/*
 * Declares the integrating application's identity via `connect` before the
 * ping handshake. Skipped when no client_info field is set; ignored by older
 * CLIs that do not implement `connect`.
 */
static copilot_error_t send_connect_handshake(copilot_client_t *client)
{
    cJSON *info = cJSON_CreateObject();
    add_non_empty_string(info, "editorName", client->client_info_application_name);
    add_non_empty_string(info, "editorVersion", client->client_info_application_version);
    add_non_empty_string(info, "extensionName", client->client_info_integration_name);
    add_non_empty_string(info, "extensionVersion", client->client_info_integration_version);

    if (cJSON_GetArraySize(info) == 0) {
        cJSON_Delete(info);
        return COPILOT_OK;
    }

    cJSON *params = cJSON_CreateObject();
    cJSON_AddItemToObject(params, "clientInfo", info);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "connect", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        bool legacy = is_method_not_found_error(err_code, err_msg);
        free(err_msg);
        return legacy ? COPILOT_OK : COPILOT_ERROR_RPC;
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

/* ============================================================================
 * Internal: inject trace context into RPC params
 * ============================================================================ */

static void inject_trace_context(copilot_client_t *client, cJSON *params)
{
    if (!client->trace_context_provider) return;

    copilot_trace_context_t ctx = client->trace_context_provider(client->trace_context_user_data);
    if (ctx.traceparent) {
        cJSON_AddStringToObject(params, "traceparent", ctx.traceparent);
    }
    if (ctx.tracestate) {
        cJSON_AddStringToObject(params, "tracestate", ctx.tracestate);
    }
}

/* ============================================================================
 * Internal: build session create/resume params
 * ============================================================================ */

static cJSON *build_session_params(const copilot_session_config_t *config, bool is_resume)
{
    cJSON *params = cJSON_CreateObject();

    if (config->session_id) {
        cJSON_AddStringToObject(params, "sessionId", config->session_id);
    }
    if (config->model) {
        cJSON_AddStringToObject(params, "model", config->model);
    }
    if (config->reasoning_effort) {
        cJSON_AddStringToObject(params, "reasoningEffort", config->reasoning_effort);
    }
    if (config->config_dir) {
        cJSON_AddStringToObject(params, "configDir", config->config_dir);
    }
    if (config->working_directory) {
        cJSON_AddStringToObject(params, "workingDirectory", config->working_directory);
    }
    if (config->streaming) {
        cJSON_AddBoolToObject(params, "streaming", true);
    }

    /* Tools */
    if (config->tools && config->tools_count > 0) {
        cJSON *tools = cJSON_CreateArray();
        for (size_t i = 0; i < config->tools_count; i++) {
            cJSON *tool = cJSON_CreateObject();
            cJSON_AddStringToObject(tool, "name", config->tools[i].name);
            if (config->tools[i].description) {
                cJSON_AddStringToObject(tool, "description", config->tools[i].description);
            }
            if (config->tools[i].parameters_json) {
                cJSON *schema = cJSON_Parse(config->tools[i].parameters_json);
                if (schema) {
                    cJSON_AddItemToObject(tool, "parameters", schema);
                }
            }
            if (config->tools[i].defer) {
                cJSON_AddStringToObject(tool, "defer", config->tools[i].defer);
            }
            if (config->tools[i].idempotent) {
                cJSON_AddBoolToObject(tool, "idempotent", true);
            }
            cJSON_AddItemToArray(tools, tool);
        }
        cJSON_AddItemToObject(params, "tools", tools);
    }

    /* Available/excluded tools */
    if (config->available_tools) {
        cJSON *arr = cJSON_CreateArray();
        for (const char **p = config->available_tools; *p; p++) {
            cJSON_AddItemToArray(arr, cJSON_CreateString(*p));
        }
        cJSON_AddItemToObject(params, "availableTools", arr);
    }
    if (config->excluded_tools) {
        cJSON *arr = cJSON_CreateArray();
        for (const char **p = config->excluded_tools; *p; p++) {
            cJSON_AddItemToArray(arr, cJSON_CreateString(*p));
        }
        cJSON_AddItemToObject(params, "excludedTools", arr);
    }

    /* System message */
    if (config->system_message) {
        cJSON *sm = cJSON_CreateObject();
        if (config->system_message->mode) {
            cJSON_AddStringToObject(sm, "mode", config->system_message->mode);
        }
        if (config->system_message->content) {
            cJSON_AddStringToObject(sm, "content", config->system_message->content);
        }
        cJSON_AddItemToObject(params, "systemMessage", sm);
    }

    /* Provider */
    if (config->provider) {
        cJSON *prov = cJSON_CreateObject();
        if (config->provider->type) {
            cJSON_AddStringToObject(prov, "type", config->provider->type);
        }
        if (config->provider->wire_api) {
            cJSON_AddStringToObject(prov, "wireApi", config->provider->wire_api);
        }
        if (config->provider->base_url) {
            cJSON_AddStringToObject(prov, "baseUrl", config->provider->base_url);
        }
        if (config->provider->api_key) {
            cJSON_AddStringToObject(prov, "apiKey", config->provider->api_key);
        }
        if (config->provider->bearer_token) {
            cJSON_AddStringToObject(prov, "bearerToken", config->provider->bearer_token);
        }
        if (config->provider->azure_api_version) {
            cJSON *azure = cJSON_CreateObject();
            cJSON_AddStringToObject(azure, "apiVersion", config->provider->azure_api_version);
            cJSON_AddItemToObject(prov, "azure", azure);
        }
        if (config->provider->model_provider) {
            cJSON_AddStringToObject(prov, "modelProvider", config->provider->model_provider);
        }
        if (config->provider->api_key_command) {
            cJSON_AddStringToObject(prov, "apiKeyCommand", config->provider->api_key_command);
        }
        cJSON_AddItemToObject(params, "provider", prov);
    }

    /* Infinite sessions */
    if (config->infinite_sessions) {
        cJSON *inf = cJSON_CreateObject();
        if (config->infinite_sessions->has_enabled) {
            cJSON_AddBoolToObject(inf, "enabled", config->infinite_sessions->enabled);
        }
        if (config->infinite_sessions->has_background_threshold) {
            cJSON_AddNumberToObject(inf, "backgroundCompactionThreshold",
                                    config->infinite_sessions->background_compaction_threshold);
        }
        if (config->infinite_sessions->has_buffer_threshold) {
            cJSON_AddNumberToObject(inf, "bufferExhaustionThreshold",
                                    config->infinite_sessions->buffer_exhaustion_threshold);
        }
        cJSON_AddItemToObject(params, "infiniteSessions", inf);
    }

    /* Handler flags */
    if (config->on_permission_request) {
        cJSON_AddBoolToObject(params, "requestPermission", true);
    }
    if (config->on_user_input_request) {
        cJSON_AddBoolToObject(params, "requestUserInput", true);
    }
    if (config->on_hook) {
        cJSON_AddBoolToObject(params, "hooks", true);
    }
    if (config->on_exit_plan_mode) {
        cJSON_AddBoolToObject(params, "requestExitPlanMode", true);
    }

    /* --- Upstream-sync session options (parity with @github/copilot-sdk) --- */
    if (config->has_enable_citations) {
        cJSON_AddBoolToObject(params, "enableCitations", config->enable_citations);
    }
    if (config->excluded_builtin_agents) {
        cJSON *arr = cJSON_CreateArray();
        for (const char **p = config->excluded_builtin_agents; *p; p++) {
            cJSON_AddItemToArray(arr, cJSON_CreateString(*p));
        }
        cJSON_AddItemToObject(params, "excludedBuiltinAgents", arr);
    }
    if (config->session_limits) {
        cJSON *sl = cJSON_CreateObject();
        if (config->session_limits->has_max_ai_credits) {
            cJSON_AddNumberToObject(sl, "maxAiCredits", config->session_limits->max_ai_credits);
        }
        cJSON_AddItemToObject(params, "sessionLimits", sl);
    }
    if (config->memory) {
        cJSON *mem = cJSON_CreateObject();
        if (config->memory->has_enabled) {
            cJSON_AddBoolToObject(mem, "enabled", config->memory->enabled);
        }
        cJSON_AddItemToObject(params, "memory", mem);
    }
    if (config->otlp_protocol) {
        cJSON_AddStringToObject(params, "otlpProtocol", config->otlp_protocol);
    }
    if (config->exp_assignments_json) {
        cJSON *exp = cJSON_Parse(config->exp_assignments_json);
        if (exp) {
            cJSON_AddItemToObject(params, "expAssignments", exp);
        }
    }
    if (config->on_mcp_auth_request) {
        cJSON_AddBoolToObject(params, "mcpAuthHandler", true);
    }

    /* --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) --- */
    if (config->has_rewind_enabled) {
        cJSON_AddBoolToObject(params, "rewindEnabled", config->rewind_enabled);
    }
    if (config->additional_directories) {
        cJSON *arr = cJSON_CreateArray();
        for (const char **p = config->additional_directories; *p; p++) {
            cJSON_AddItemToArray(arr, cJSON_CreateString(*p));
        }
        cJSON_AddItemToObject(params, "additionalDirectories", arr);
    }
    if (config->disabled_mcp_servers) {
        cJSON *arr = cJSON_CreateArray();
        for (const char **p = config->disabled_mcp_servers; *p; p++) {
            cJSON_AddItemToArray(arr, cJSON_CreateString(*p));
        }
        cJSON_AddItemToObject(params, "disabledMcpServers", arr);
    }
    if (config->github_mcp_tool_config_json) {
        cJSON *gh = cJSON_Parse(config->github_mcp_tool_config_json);
        if (gh) {
            cJSON_AddItemToObject(params, "githubMcpToolConfig", gh);
        }
    }
    if (config->canvas_provider_json) {
        cJSON *cv = cJSON_Parse(config->canvas_provider_json);
        if (cv) {
            cJSON_AddItemToObject(params, "canvasProvider", cv);
        }
    }
    if (config->has_custom_agents_local_only) {
        cJSON_AddBoolToObject(params, "customAgentsLocalOnly", config->custom_agents_local_only);
    }
    if (config->tool_search_json) {
        cJSON *ts = cJSON_Parse(config->tool_search_json);
        if (ts) {
            cJSON_AddItemToObject(params, "toolSearch", ts);
        }
    }
    if (config->has_experimental_mode) {
        cJSON_AddBoolToObject(params, "enableExperimentalMode", config->experimental_mode);
    }
    if (config->has_content_exclusion) {
        cJSON_AddBoolToObject(params, "contentExclusion", config->content_exclusion);
    }
    if (config->args_schema_json) {
        cJSON *as = cJSON_Parse(config->args_schema_json);
        if (as) {
            cJSON_AddItemToObject(params, "argsSchema", as);
        }
    }

    /* --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) --- */

    /* Model capabilities override (modelCapabilities), with optional limits.max_output_tokens */
    {
        cJSON *caps = config->model_capabilities_json ? cJSON_Parse(config->model_capabilities_json) : NULL;
        if (config->has_max_output_tokens) {
            if (!caps || !cJSON_IsObject(caps)) {
                cJSON_Delete(caps);
                caps = cJSON_CreateObject();
            }
            cJSON *limits = cJSON_GetObjectItem(caps, "limits");
            if (!limits || !cJSON_IsObject(limits)) {
                cJSON_DeleteItemFromObject(caps, "limits");
                limits = cJSON_AddObjectToObject(caps, "limits");
            }
            cJSON_DeleteItemFromObject(limits, "max_output_tokens");
            cJSON_AddNumberToObject(limits, "max_output_tokens", config->max_output_tokens);
        }
        if (caps) {
            cJSON_AddItemToObject(params, "modelCapabilities", caps);
        }
    }

    /* CAPI options (github.capi): enabled, autoTier, enableWebSocketResponses */
    if (config->auto_tier || config->has_enable_web_socket_responses ||
        (config->capi && config->capi->has_enabled)) {
        cJSON *capi = cJSON_CreateObject();
        if (config->capi && config->capi->has_enabled) {
            cJSON_AddBoolToObject(capi, "enabled", config->capi->enabled);
        }
        if (config->auto_tier) {
            cJSON_AddStringToObject(capi, "autoTier", config->auto_tier);
        }
        if (config->has_enable_web_socket_responses) {
            cJSON_AddBoolToObject(capi, "enableWebSocketResponses", config->enable_web_socket_responses);
        }
        cJSON_AddItemToObject(params, "capi", capi);
    }
    if (config->ask_user_variant) {
        cJSON_AddStringToObject(params, "askUserVariant", config->ask_user_variant);
    }
    if (config->sandbox_config) {
        const copilot_sandbox_config_t *sc = config->sandbox_config;
        cJSON *sb = cJSON_CreateObject();
        cJSON_AddBoolToObject(sb, "enabled", sc->enabled);
        if (sc->user_policy_json) {
            cJSON *policy = cJSON_Parse(sc->user_policy_json);
            if (policy) {
                cJSON_AddItemToObject(sb, "userPolicy", policy);
            }
        }
        if (sc->has_add_current_working_directory) {
            cJSON_AddBoolToObject(sb, "addCurrentWorkingDirectory", sc->add_current_working_directory);
        }
        if (sc->has_sandbox_mcp_servers) {
            cJSON_AddBoolToObject(sb, "sandboxMcpServers", sc->sandbox_mcp_servers);
        }
        if (sc->has_sandbox_lsp_servers) {
            cJSON_AddBoolToObject(sb, "sandboxLspServers", sc->sandbox_lsp_servers);
        }
        if (sc->has_sandbox_extensions) {
            cJSON_AddBoolToObject(sb, "sandboxExtensions", sc->sandbox_extensions);
        }
        if (sc->has_allow_bypass) {
            cJSON_AddBoolToObject(sb, "allowBypass", sc->allow_bypass);
        }
        cJSON_AddItemToObject(params, "sandboxConfig", sb);
    }
    if (config->feature_flags_json) {
        cJSON *ff = cJSON_Parse(config->feature_flags_json);
        if (ff) {
            cJSON_AddItemToObject(params, "featureFlags", ff);
        }
    }
    add_string_array(params, "allowedModels", config->allowed_models);
    if (config->auth_client_id_metadata_url) {
        cJSON_AddStringToObject(params, "authClientIdMetadataUrl", config->auth_client_id_metadata_url);
    }
    add_string_array(params, "agentDirectories", config->agent_directories);
    add_string_array(params, "hookDirectories", config->hook_directories);
    if (config->has_interactive_user) {
        cJSON_AddBoolToObject(params, "interactiveUser", config->interactive_user);
    }
    add_string_array(params, "excludedManagedSettings", config->excluded_managed_settings);
    if (config->has_enforce_managed_model_defaults) {
        cJSON_AddBoolToObject(params, "enforceManagedModelDefaults", config->enforce_managed_model_defaults);
    }
    if (config->image_generation) {
        const copilot_image_generation_config_t *ig = config->image_generation;
        cJSON *img = cJSON_CreateObject();
        if (ig->has_enabled) {
            cJSON_AddBoolToObject(img, "enabled", ig->enabled);
        }
        if (ig->has_watermark) {
            cJSON_AddBoolToObject(img, "watermark", ig->watermark);
        }
        if (ig->has_save_to_disk) {
            cJSON_AddBoolToObject(img, "saveToDisk", ig->save_to_disk);
        }
        cJSON_AddItemToObject(params, "imageGeneration", img);
    }
    if (config->autopilot) {
        const copilot_autopilot_options_t *ap = config->autopilot;
        cJSON *auto_obj = cJSON_CreateObject();
        if (ap->has_continuation_enabled) {
            cJSON_AddBoolToObject(auto_obj, "continuationEnabled", ap->continuation_enabled);
        }
        if (ap->has_max_continues) {
            cJSON_AddNumberToObject(auto_obj, "maxContinues", ap->max_continues);
        }
        cJSON_AddItemToObject(params, "autopilot", auto_obj);
    }

    /* --- v3.0.0 session options (SDK protocol 4 parity with @github/copilot-sdk 2.0) --- */
    if (config->providers && config->providers_count > 0) {
        cJSON *arr = cJSON_AddArrayToObject(params, "providers");
        for (size_t i = 0; arr && i < config->providers_count; i++) {
            const copilot_named_provider_config_t *p = &config->providers[i];
            cJSON *prov = cJSON_CreateObject();
            if (p->name) cJSON_AddStringToObject(prov, "name", p->name);
            if (p->type) cJSON_AddStringToObject(prov, "type", p->type);
            if (p->base_url) cJSON_AddStringToObject(prov, "baseUrl", p->base_url);
            if (p->api_key) cJSON_AddStringToObject(prov, "apiKey", p->api_key);
            if (p->api_key_command) cJSON_AddStringToObject(prov, "apiKeyCommand", p->api_key_command);
            if (p->bearer_token) cJSON_AddStringToObject(prov, "bearerToken", p->bearer_token);
            if (p->wire_api) cJSON_AddStringToObject(prov, "wireApi", p->wire_api);
            if (p->transport) cJSON_AddStringToObject(prov, "transport", p->transport);
            if (p->azure_api_version) {
                cJSON *azure = cJSON_AddObjectToObject(prov, "azure");
                if (azure) cJSON_AddStringToObject(azure, "apiVersion", p->azure_api_version);
            }
            if (p->model_provider) cJSON_AddStringToObject(prov, "modelProvider", p->model_provider);
            if (p->headers_json) {
                cJSON *headers = cJSON_Parse(p->headers_json);
                if (headers) cJSON_AddItemToObject(prov, "headers", headers);
            }
            cJSON_AddItemToArray(arr, prov);
        }
    }
    if (config->models && config->models_count > 0) {
        cJSON *arr = cJSON_AddArrayToObject(params, "models");
        for (size_t i = 0; arr && i < config->models_count; i++) {
            const copilot_provider_model_config_t *m = &config->models[i];
            cJSON *model = cJSON_CreateObject();
            if (m->provider) cJSON_AddStringToObject(model, "provider", m->provider);
            if (m->id) cJSON_AddStringToObject(model, "id", m->id);
            if (m->model_id) cJSON_AddStringToObject(model, "modelId", m->model_id);
            if (m->wire_model) cJSON_AddStringToObject(model, "wireModel", m->wire_model);
            if (m->name) cJSON_AddStringToObject(model, "name", m->name);
            if (m->max_prompt_tokens > 0) {
                cJSON_AddNumberToObject(model, "maxPromptTokens", m->max_prompt_tokens);
            }
            if (m->max_context_window_tokens > 0) {
                cJSON_AddNumberToObject(model, "maxContextWindowTokens", m->max_context_window_tokens);
            }
            if (m->max_output_tokens > 0) {
                cJSON_AddNumberToObject(model, "maxOutputTokens", m->max_output_tokens);
            }
            if (m->capabilities_json) {
                cJSON *caps = cJSON_Parse(m->capabilities_json);
                if (caps) cJSON_AddItemToObject(model, "capabilities", caps);
            }
            cJSON_AddItemToArray(arr, model);
        }
    }
    if (config->default_provider) {
        cJSON_AddStringToObject(params, "defaultProvider", config->default_provider);
    } else if (is_resume && config->clear_default_provider) {
        cJSON_AddNullToObject(params, "defaultProvider");
    }
    if (config->permission_mode) {
        cJSON_AddStringToObject(params, "permissionMode", config->permission_mode);
    }
    if (config->on_shell_authentication_request) {
        cJSON_AddBoolToObject(params, "requestShellAuthentication", true);
    }
    if (config->on_memory_store) {
        cJSON_AddBoolToObject(params, "hasMemoryStore", true);
    }
    if (config->has_enable_config_discovery) {
        cJSON_AddBoolToObject(params, "enableConfigDiscovery", config->enable_config_discovery);
    }
    if (config->has_include_sub_agent_streaming_events) {
        cJSON_AddBoolToObject(params, "includeSubAgentStreamingEvents",
                              config->include_sub_agent_streaming_events);
    }
    if (config->github_token) {
        cJSON_AddStringToObject(params, "gitHubToken", config->github_token);
    }
    if (config->instruction_directories) {
        cJSON *arr = cJSON_CreateArray();
        int count = config->instruction_directories_count;
        for (int i = 0; count > 0 ? i < count : config->instruction_directories[i] != NULL; i++) {
            if (config->instruction_directories[i]) {
                cJSON_AddItemToArray(arr, cJSON_CreateString(config->instruction_directories[i]));
            }
        }
        cJSON_AddItemToObject(params, "instructionDirectories", arr);
    }

    if (is_resume) {
        if (config->has_allow_transcript_recovery) {
            cJSON_AddBoolToObject(params, "allowTranscriptRecovery", config->allow_transcript_recovery);
        }
        if (config->has_suppress_resume_event) {
            cJSON_AddBoolToObject(params, "suppressResumeEvent", config->suppress_resume_event);
        }
    } else {
        if (config->name) {
            cJSON_AddStringToObject(params, "name", config->name);
        }
        if (config->trust_working_directory) {
            cJSON_AddStringToObject(params, "trustWorkingDirectory", config->trust_working_directory);
        }
        if (config->has_refresh_custom_instructions) {
            cJSON_AddBoolToObject(params, "refreshCustomInstructions", config->refresh_custom_instructions);
        }
    }

    return params;
}

/* ============================================================================
 * Internal: SDK protocol 4 grouped session payloads
 * ============================================================================ */

/* Lifecycle methods that accept a key. Keys sent to the wrong method are dropped. */
typedef enum {
    PARAM_SCOPE_BOTH = 0,
    PARAM_SCOPE_CREATE,
    PARAM_SCOPE_RESUME,
} session_param_scope_t;

/* Maps a flat session.create/resume key to its protocol 4 location (group NULL = root). */
typedef struct {
    const char *flat_key;
    const char *group;
    const char *member;
    session_param_scope_t scope;
} session_param_rule_t;

static const session_param_rule_t k_session_param_rules[] = {
    /* Root fields (configDir and disableResume are renamed) */
    { "sessionId",                     NULL, "sessionId",                     PARAM_SCOPE_BOTH },
    { "model",                         NULL, "model",                         PARAM_SCOPE_BOTH },
    { "name",                          NULL, "name",                          PARAM_SCOPE_CREATE },
    { "clientName",                    NULL, "clientName",                    PARAM_SCOPE_BOTH },
    { "workingDirectory",              NULL, "workingDirectory",              PARAM_SCOPE_BOTH },
    { "configDirectory",               NULL, "configDirectory",               PARAM_SCOPE_BOTH },
    { "configDir",                     NULL, "configDirectory",               PARAM_SCOPE_BOTH },
    { "enableConfigDiscovery",         NULL, "enableConfigDiscovery",         PARAM_SCOPE_BOTH },
    { "systemMessage",                 NULL, "systemMessage",                 PARAM_SCOPE_BOTH },
    { "diagnostics",                   NULL, "diagnostics",                   PARAM_SCOPE_BOTH },
    { "lifecycleDiagnostics",          NULL, "lifecycleDiagnostics",          PARAM_SCOPE_BOTH },
    { "cloud",                         NULL, "cloud",                         PARAM_SCOPE_CREATE },
    { "traceparent",                   NULL, "traceparent",                   PARAM_SCOPE_BOTH },
    { "tracestate",                    NULL, "tracestate",                    PARAM_SCOPE_BOTH },
    { "suppressResumeEvent",           NULL, "suppressResumeEvent",           PARAM_SCOPE_RESUME },
    { "disableResume",                 NULL, "suppressResumeEvent",           PARAM_SCOPE_RESUME },
    { "continuePendingWork",           NULL, "continuePendingWork",           PARAM_SCOPE_RESUME },
    { "allowTranscriptRecovery",       NULL, "allowTranscriptRecovery",       PARAM_SCOPE_RESUME },
    { "requestedEnvironmentVariables", NULL, "requestedEnvironmentVariables", PARAM_SCOPE_RESUME },
    /* inference */
    { "reasoningEffort",        "inference", "reasoningEffort",        PARAM_SCOPE_BOTH },
    { "reasoningSummary",       "inference", "reasoningSummary",       PARAM_SCOPE_BOTH },
    { "contextTier",            "inference", "contextTier",            PARAM_SCOPE_BOTH },
    { "allowMultipleToolCalls", "inference", "allowMultipleToolCalls", PARAM_SCOPE_BOTH },
    { "enableCitations",        "inference", "enableCitations",        PARAM_SCOPE_BOTH },
    { "modelCapabilities",      "inference", "capabilities",           PARAM_SCOPE_BOTH },
    { "verbosity",              "inference", "verbosity",              PARAM_SCOPE_BOTH },
    { "providers",              "inference", "providers",              PARAM_SCOPE_BOTH },
    { "models",                 "inference", "models",                 PARAM_SCOPE_BOTH },
    { "defaultProvider",        "inference", "defaultProvider",        PARAM_SCOPE_BOTH },
    /* tools */
    { "tools",                 "tools", "definitions",           PARAM_SCOPE_BOTH },
    { "availableTools",        "tools", "availableTools",        PARAM_SCOPE_BOTH },
    { "excludedTools",         "tools", "excludedTools",         PARAM_SCOPE_BOTH },
    { "toolSearch",            "tools", "search",                PARAM_SCOPE_BOTH },
    { "largeOutput",           "tools", "largeOutput",           PARAM_SCOPE_BOTH },
    { "imageGeneration",       "tools", "imageGeneration",       PARAM_SCOPE_BOTH },
    { "manageScheduleEnabled", "tools", "manageScheduleEnabled", PARAM_SCOPE_BOTH },
    { "webSearch",             "tools", "webSearch",             PARAM_SCOPE_BOTH },
    { "shell",                 "tools", "shell",                 PARAM_SCOPE_BOTH },
    { "maxInlineBinaryBytes",  "tools", "maxInlineBinaryBytes",  PARAM_SCOPE_BOTH },
    /* permissions */
    { "requestPermission",   "permissions", "request",       PARAM_SCOPE_BOTH },
    { "sandboxConfig",       "permissions", "sandbox",       PARAM_SCOPE_BOTH },
    { "sandbox",             "permissions", "sandbox",       PARAM_SCOPE_BOTH },
    { "permissionMode",      "permissions", "mode",          PARAM_SCOPE_BOTH },
    { "sandboxConfigSource", "permissions", "sandboxSource", PARAM_SCOPE_BOTH },
    /* interaction */
    { "commands",                   "interaction", "commands",                   PARAM_SCOPE_BOTH },
    { "askUserVariant",             "interaction", "askUserVariant",             PARAM_SCOPE_BOTH },
    { "requestUserInput",           "interaction", "requestUserInput",           PARAM_SCOPE_BOTH },
    { "requestElicitation",         "interaction", "requestElicitation",         PARAM_SCOPE_BOTH },
    { "requestShellAuthentication", "interaction", "requestShellAuthentication", PARAM_SCOPE_BOTH },
    { "requestExitPlanMode",        "interaction", "requestExitPlanMode",        PARAM_SCOPE_BOTH },
    { "requestAutoModeSwitch",      "interaction", "requestAutoModeSwitch",      PARAM_SCOPE_BOTH },
    { "requestAutoModeRecovery",    "interaction", "requestAutoModeRecovery",    PARAM_SCOPE_BOTH },
    /* events */
    { "streaming",                      "events", "streaming",                      PARAM_SCOPE_BOTH },
    { "includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents", PARAM_SCOPE_BOTH },
    /* agents */
    { "customAgents",          "agents", "definitions",           PARAM_SCOPE_BOTH },
    { "defaultAgent",          "agents", "default",               PARAM_SCOPE_BOTH },
    { "agent",                 "agents", "selected",              PARAM_SCOPE_BOTH },
    { "includedBuiltinAgents", "agents", "includedBuiltinAgents", PARAM_SCOPE_BOTH },
    { "excludedBuiltinAgents", "agents", "excludedBuiltinAgents", PARAM_SCOPE_BOTH },
    { "customAgentsLocalOnly", "agents", "localDiscoveryOnly",    PARAM_SCOPE_BOTH },
    { "agentDirectories",      "agents", "directories",           PARAM_SCOPE_BOTH },
    /* skills */
    { "skillDirectories",       "skills", "directories",           PARAM_SCOPE_BOTH },
    { "enableSkills",           "skills", "enabled",               PARAM_SCOPE_BOTH },
    { "includedBuiltinSkills",  "skills", "includedBuiltinSkills", PARAM_SCOPE_BOTH },
    { "disabledSkills",         "skills", "disabledSkills",        PARAM_SCOPE_BOTH },
    { "hasSkillProvider",       "skills", "hasProvider",           PARAM_SCOPE_BOTH },
    { "ignoredSkillsLocations", "skills", "ignoredLocations",      PARAM_SCOPE_BOTH },
    /* mcp */
    { "mcpServers",                    "mcp", "servers",                    PARAM_SCOPE_BOTH },
    { "disabledMcpServers",            "mcp", "disabledServers",            PARAM_SCOPE_BOTH },
    { "requestMcpApps",                "mcp", "enableApps",                 PARAM_SCOPE_BOTH },
    { "enableMcpApps",                 "mcp", "enableApps",                 PARAM_SCOPE_BOTH },
    { "mcpOAuthTokenStorage",          "mcp", "oauthTokenStorage",          PARAM_SCOPE_BOTH },
    { "authClientIdMetadataUrl",       "mcp", "authClientIdMetadataUrl",    PARAM_SCOPE_BOTH },
    { "envValueMode",                  "mcp", "envValueMode",               PARAM_SCOPE_BOTH },
    { "managedMcpServers",             "mcp", "managedServers",             PARAM_SCOPE_BOTH },
    { "allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions", PARAM_SCOPE_BOTH },
    /* github */
    { "gitHubToken",                       "github", "token",                       PARAM_SCOPE_BOTH },
    { "gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId", PARAM_SCOPE_BOTH },
    { "githubMcpToolConfig",               "github", "mcp",                         PARAM_SCOPE_BOTH },
    { "capi",                              "github", "capi",                        PARAM_SCOPE_BOTH },
    { "enableSessionTelemetry",            "github", "telemetryEnabled",            PARAM_SCOPE_BOTH },
    { "enableManagedSettings",             "github", "fetchManagedSettings",        PARAM_SCOPE_BOTH },
    { "interactiveUser",                   "github", "interactiveUser",             PARAM_SCOPE_BOTH },
    { "enableGitHubTelemetryForwarding",   "github", "telemetryForwarding",         PARAM_SCOPE_BOTH },
    { "authInfo",                          "github", "authInfo",                    PARAM_SCOPE_BOTH },
    { "copilotUrl",                        "github", "copilotUrl",                  PARAM_SCOPE_BOTH },
    /* filesystem */
    { "additionalDirectories",    "filesystem", "additionalDirectories", PARAM_SCOPE_BOTH },
    { "enableFileChangeTracking", "filesystem", "trackFileChanges",      PARAM_SCOPE_BOTH },
    { "trustWorkingDirectory",    "filesystem", "trustWorkingDirectory", PARAM_SCOPE_CREATE },
    /* execution */
    { "sessionLimits",         "execution", "limits",                PARAM_SCOPE_BOTH },
    { "autopilot",             "execution", "autopilot",             PARAM_SCOPE_BOTH },
    { "autopilotContinuation", "execution", "autopilotContinuation", PARAM_SCOPE_BOTH },
    /* instructions */
    { "instructionDirectories",             "instructions", "directories",               PARAM_SCOPE_BOTH },
    { "organizationCustomInstructions",     "instructions", "organizationText",          PARAM_SCOPE_BOTH },
    { "enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery",         PARAM_SCOPE_BOTH },
    { "skipCustomInstructions",             "instructions", "skipCustomInstructions",    PARAM_SCOPE_BOTH },
    { "refreshCustomInstructions",          "instructions", "refreshCustomInstructions", PARAM_SCOPE_CREATE },
    /* hooks (boolean "hooks" becomes hooks.hasHandlers) */
    { "hooks",           "hooks", "hasHandlers",           PARAM_SCOPE_BOTH },
    { "hookDirectories", "hooks", "directories",           PARAM_SCOPE_BOTH },
    { "enableFileHooks", "hooks", "enableRepositoryHooks", PARAM_SCOPE_BOTH },
    /* git */
    { "coauthorEnabled",         "git", "coauthorEnabled",         PARAM_SCOPE_BOTH },
    { "enableHostGitOperations", "git", "enableHostGitOperations", PARAM_SCOPE_BOTH },
    /* state */
    { "infiniteSessions",       "state", "infiniteSessions",        PARAM_SCOPE_BOTH },
    { "memory",                 "state", "memory",                  PARAM_SCOPE_BOTH },
    { "hasMemoryStore",         "state", "hasMemoryStore",          PARAM_SCOPE_BOTH },
    { "enableSessionStore",     "state", "enableCrossSessionStore", PARAM_SCOPE_BOTH },
    { "skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval",  PARAM_SCOPE_BOTH },
    { "embeddingCacheStorage",  "state", "embeddingCacheStorage",   PARAM_SCOPE_BOTH },
    /* plugins */
    { "pluginDirectories", "plugins", "directories", PARAM_SCOPE_BOTH },
    { "installedPlugins",  "plugins", "installed",   PARAM_SCOPE_BOTH },
    /* canvases */
    { "canvases",              "canvases", "definitions",     PARAM_SCOPE_BOTH },
    { "canvasProvider",        "canvases", "provider",        PARAM_SCOPE_BOTH },
    { "requestCanvasRenderer", "canvases", "requestRenderer", PARAM_SCOPE_BOTH },
    { "openCanvases",          "canvases", "open",            PARAM_SCOPE_RESUME },
    /* extensions */
    { "requestExtensions", "extensions", "enabled",  PARAM_SCOPE_BOTH },
    { "extensionSdkPath",  "extensions", "sdkPath",  PARAM_SCOPE_BOTH },
    { "extensionInfo",     "extensions", "identity", PARAM_SCOPE_BOTH },
    /* experiments */
    { "enableExperimentalMode",   "experiments", "enableExperimentalMode", PARAM_SCOPE_BOTH },
    { "isExperimentalMode",       "experiments", "enableExperimentalMode", PARAM_SCOPE_BOTH },
    { "featureFlags",             "experiments", "flags",                  PARAM_SCOPE_BOTH },
    { "expAssignments",           "experiments", "assignments",            PARAM_SCOPE_BOTH },
    { "featureFlagConfiguration", "experiments", "configuration",          PARAM_SCOPE_BOTH },
    /* policy */
    { "allowedModels",               "policy", "allowedModels",               PARAM_SCOPE_BOTH },
    { "managedSettings",             "policy", "managedSettings",             PARAM_SCOPE_BOTH },
    { "excludedManagedSettings",     "policy", "excludedManagedSettings",     PARAM_SCOPE_BOTH },
    { "enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults", PARAM_SCOPE_BOTH },
};

/* Protocol 4 option groups. A key naming a group that holds an object is already grouped. */
static const char *const k_session_param_groups[] = {
    "inference", "tools", "permissions", "interaction", "events", "agents", "skills",
    "mcp", "github", "filesystem", "execution", "instructions", "hooks", "git",
    "state", "plugins", "canvases", "extensions", "experiments", "policy",
};

/* Model-specific fields of the legacy single provider (moved to inference.models[]). */
static const char *const k_legacy_provider_model_fields[] = {
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens",
    "capabilities",
};

static const session_param_rule_t *find_session_param_rule(const char *key)
{
    for (size_t i = 0; i < sizeof(k_session_param_rules) / sizeof(k_session_param_rules[0]); i++) {
        if (strcmp(k_session_param_rules[i].flat_key, key) == 0) return &k_session_param_rules[i];
    }
    return NULL;
}

static bool string_in_list(const char *value, const char *const *list, size_t count)
{
    for (size_t i = 0; i < count; i++) {
        if (strcmp(list[i], value) == 0) return true;
    }
    return false;
}

/* Returns parent[name] as an object, creating it (or replacing a non-object) when needed. */
static cJSON *ensure_child_object(cJSON *parent, const char *name)
{
    cJSON *obj = cJSON_GetObjectItemCaseSensitive(parent, name);
    if (obj && cJSON_IsObject(obj)) return obj;
    if (obj) cJSON_DeleteItemFromObjectCaseSensitive(parent, name);
    return cJSON_AddObjectToObject(parent, name);
}

/* Stores a detached value at out[group][member] unless an explicit member exists. Takes ownership. */
static void put_group_member(cJSON *out, const char *group, const char *member, cJSON *item)
{
    if (!item) return;
    cJSON *obj = ensure_child_object(out, group);
    if (!obj || cJSON_GetObjectItemCaseSensitive(obj, member)) {
        cJSON_Delete(item);
        return;
    }
    if (!cJSON_AddItemToObject(obj, member, item)) cJSON_Delete(item);
}

/* Merges an already-grouped object into out[group]; its members win. Takes ownership. */
static void merge_group_object(cJSON *out, const char *group, cJSON *grouped)
{
    cJSON *obj = ensure_child_object(out, group);
    cJSON *member = grouped->child;
    while (member) {
        cJSON *next = member->next;
        cJSON_DetachItemViaPointer(grouped, member);
        if (!obj || !member->string) {
            cJSON_Delete(member);
        } else {
            cJSON_DeleteItemFromObjectCaseSensitive(obj, member->string);
            if (!cJSON_AddItemToObject(obj, member->string, member)) cJSON_Delete(member);
        }
        member = next;
    }
    cJSON_Delete(grouped);
}

/*
 * Translates the legacy single `provider` (no longer accepted by protocol 4) into
 * inference.providers [{name:"default", ...connection}], inference.defaultProvider
 * "default" and github.capi.enabled false, plus inference.models when model-specific
 * fields are present and the session model is set. Explicit providers,
 * defaultProvider, or capi.enabled win: the legacy provider is then dropped.
 * Takes ownership of `provider`.
 */
static void translate_legacy_provider(cJSON *out, cJSON *provider)
{
    cJSON *inference = cJSON_GetObjectItemCaseSensitive(out, "inference");
    cJSON *github = cJSON_GetObjectItemCaseSensitive(out, "github");
    cJSON *capi = github ? cJSON_GetObjectItemCaseSensitive(github, "capi") : NULL;
    bool explicit_routing =
        (inference && (cJSON_GetObjectItemCaseSensitive(inference, "providers") ||
                       cJSON_GetObjectItemCaseSensitive(inference, "defaultProvider"))) ||
        (capi && cJSON_GetObjectItemCaseSensitive(capi, "enabled"));

    cJSON *connection = explicit_routing ? NULL : cJSON_CreateObject();
    cJSON *model_fields = explicit_routing ? NULL : cJSON_CreateObject();
    if (!connection || !model_fields) {
        cJSON_Delete(connection);
        cJSON_Delete(model_fields);
        cJSON_Delete(provider);
        return;
    }

    cJSON_AddStringToObject(connection, "name", "default");
    cJSON *field = provider->child;
    while (field) {
        cJSON *next = field->next;
        cJSON_DetachItemViaPointer(provider, field);
        if (!field->string || cJSON_IsNull(field) || strcmp(field->string, "name") == 0) {
            cJSON_Delete(field);
        } else {
            bool model_field = string_in_list(
                field->string, k_legacy_provider_model_fields,
                sizeof(k_legacy_provider_model_fields) / sizeof(k_legacy_provider_model_fields[0]));
            if (!cJSON_AddItemToObject(model_field ? model_fields : connection, field->string, field)) {
                cJSON_Delete(field);
            }
        }
        field = next;
    }
    cJSON_Delete(provider);

    inference = ensure_child_object(out, "inference");
    github = ensure_child_object(out, "github");
    capi = github ? ensure_child_object(github, "capi") : NULL;
    cJSON *providers = inference ? cJSON_AddArrayToObject(inference, "providers") : NULL;
    if (!providers || !capi) {
        cJSON_Delete(connection);
        cJSON_Delete(model_fields);
        return;
    }
    cJSON_AddItemToArray(providers, connection);
    cJSON_AddStringToObject(inference, "defaultProvider", "default");
    cJSON_AddFalseToObject(capi, "enabled");

    cJSON *model = cJSON_GetObjectItemCaseSensitive(out, "model");
    cJSON *definition = (model_fields->child && model && cJSON_IsString(model))
                            ? cJSON_CreateObject() : NULL;
    if (definition) {
        cJSON_AddStringToObject(definition, "provider", "default");
        cJSON_AddStringToObject(definition, "id", model->valuestring);
        cJSON *item = model_fields->child;
        while (item) {
            cJSON *next = item->next;
            cJSON_DetachItemViaPointer(model_fields, item);
            if (!cJSON_AddItemToObject(definition, item->string, item)) cJSON_Delete(item);
            item = next;
        }
        cJSON *models = cJSON_GetObjectItemCaseSensitive(inference, "models");
        if (!models || !cJSON_IsArray(models)) {
            cJSON_DeleteItemFromObjectCaseSensitive(inference, "models");
            models = cJSON_AddArrayToObject(inference, "models");
        }
        if (models) {
            cJSON_AddItemToArray(models, definition);
        } else {
            cJSON_Delete(definition);
        }
    }
    cJSON_Delete(model_fields);
}

/*
 * Converts the flat session.create/session.resume payload into the SDK protocol 4
 * grouped payload: root keys stay at the top level, every known flat key moves to
 * its option group, unknown keys stay at the root (forward compatibility), null
 * values are skipped (except defaultProvider: null, which clears it on resume),
 * explicit grouped members win over flat ones, and empty groups are not emitted.
 * Consumes `flat`; returns the grouped payload, or NULL on allocation failure.
 */
static cJSON *group_session_params(cJSON *flat, bool is_resume)
{
    if (!flat) return NULL;
    cJSON *out = cJSON_CreateObject();
    if (!out) {
        cJSON_Delete(flat);
        return NULL;
    }

    const size_t group_count = sizeof(k_session_param_groups) / sizeof(k_session_param_groups[0]);
    cJSON *legacy_provider = NULL;
    cJSON *item = flat->child;
    while (item) {
        cJSON *next = item->next;
        cJSON_DetachItemViaPointer(flat, item);
        const char *key = item->string;
        const session_param_rule_t *rule = key ? find_session_param_rule(key) : NULL;

        if (!key) {
            cJSON_Delete(item);
        } else if (strcmp(key, "provider") == 0) {
            /* Translated after the walk, once explicit routing options are known */
            cJSON_Delete(legacy_provider);
            legacy_provider = item;
        } else if (cJSON_IsObject(item) && string_in_list(key, k_session_param_groups, group_count)) {
            merge_group_object(out, key, item);
        } else if (cJSON_IsNull(item) && !(is_resume && strcmp(key, "defaultProvider") == 0)) {
            cJSON_Delete(item);
        } else if (!rule) {
            cJSON_DeleteItemFromObjectCaseSensitive(out, key);
            if (!cJSON_AddItemToObject(out, key, item)) cJSON_Delete(item);
        } else if ((rule->scope == PARAM_SCOPE_CREATE && is_resume) ||
                   (rule->scope == PARAM_SCOPE_RESUME && !is_resume)) {
            cJSON_Delete(item);
        } else if (!rule->group) {
            cJSON_DeleteItemFromObjectCaseSensitive(out, rule->member);
            if (!cJSON_AddItemToObject(out, rule->member, item)) cJSON_Delete(item);
        } else {
            put_group_member(out, rule->group, rule->member, item);
        }
        item = next;
    }
    cJSON_Delete(flat);

    /* MCP server definitions use direct environment values */
    cJSON *mcp = cJSON_GetObjectItemCaseSensitive(out, "mcp");
    if (mcp && cJSON_GetObjectItemCaseSensitive(mcp, "servers")) {
        put_group_member(out, "mcp", "envValueMode", cJSON_CreateString("direct"));
    }

    if (legacy_provider) {
        if (cJSON_IsObject(legacy_provider)) {
            translate_legacy_provider(out, legacy_provider);
        } else {
            cJSON_Delete(legacy_provider);
        }
    }

    for (size_t i = 0; i < group_count; i++) {
        cJSON *group = cJSON_GetObjectItemCaseSensitive(out, k_session_param_groups[i]);
        if (group && cJSON_IsObject(group) && !group->child) {
            cJSON_DeleteItemFromObjectCaseSensitive(out, k_session_param_groups[i]);
        }
    }
    return out;
}

/* Client mode "empty": minimal defaults applied underneath the caller's own values. */
static void apply_empty_mode_defaults(cJSON *grouped)
{
    put_group_member(grouped, "experiments", "enableExperimentalMode", cJSON_CreateFalse());
    put_group_member(grouped, "github", "telemetryEnabled", cJSON_CreateFalse());
    put_group_member(grouped, "mcp", "oauthTokenStorage", cJSON_CreateString("in-memory"));
    put_group_member(grouped, "state", "skipEmbeddingRetrieval", cJSON_CreateTrue());
    put_group_member(grouped, "state", "embeddingCacheStorage", cJSON_CreateString("in-memory"));
    put_group_member(grouped, "state", "enableCrossSessionStore", cJSON_CreateFalse());
    cJSON *memory = cJSON_CreateObject();
    if (memory) cJSON_AddFalseToObject(memory, "enabled");
    put_group_member(grouped, "state", "memory", memory);
    put_group_member(grouped, "instructions", "onDemandDiscovery", cJSON_CreateFalse());
    put_group_member(grouped, "hooks", "enableRepositoryHooks", cJSON_CreateFalse());
    put_group_member(grouped, "git", "enableHostGitOperations", cJSON_CreateFalse());
}

/* Remembers error.data.policyHelperFailure of a failed create/resume (NULL clears it). */
static void record_policy_helper_failure(copilot_client_t *client, cJSON *error_data)
{
    cJSON *failure = error_data
                         ? cJSON_GetObjectItemCaseSensitive(error_data, "policyHelperFailure")
                         : NULL;
    char *json = (failure && cJSON_IsObject(failure)) ? cJSON_PrintUnformatted(failure) : NULL;

    pthread_mutex_lock(&client->last_error_mutex);
    free(client->last_policy_helper_failure_json);
    client->last_policy_helper_failure_json = json;
    pthread_mutex_unlock(&client->last_error_mutex);
}

/* ============================================================================
 * Internal: create session object from RPC response
 * ============================================================================ */

static copilot_session_t *create_session_from_response(
    copilot_client_t *client,
    cJSON *response,
    const copilot_session_config_t *config)
{
    cJSON *sid = cJSON_GetObjectItem(response, "sessionId");
    if (!sid || !cJSON_IsString(sid)) return NULL;

    copilot_session_t *session = calloc(1, sizeof(copilot_session_t));
    if (!session) return NULL;

    session->session_id = strdup(sid->valuestring);
    session->rpc = client->rpc;
    session->owner = client;
    session->handlers = NULL;
    session->next_handler_id = 0;
    pthread_mutex_init(&session->handlers_mutex, NULL);
    pthread_mutex_init(&session->idle_mutex, NULL);
    pthread_cond_init(&session->idle_cond, NULL);
    session->idle_signaled = false;
    session->error_signaled = false;
    session->error_message = NULL;
    session->last_assistant_content = NULL;

    cJSON *wp = cJSON_GetObjectItem(response, "workspacePath");
    if (wp && cJSON_IsString(wp)) {
        session->workspace_path = strdup(wp->valuestring);
    }

    /* Copy tool definitions and handlers */
    if (config && config->tools && config->tools_count > 0) {
        session->tools = calloc(config->tools_count, sizeof(copilot_tool_t));
        session->tools_count = config->tools_count;
        for (size_t i = 0; i < config->tools_count; i++) {
            session->tools[i].name = config->tools[i].name;
            session->tools[i].description = config->tools[i].description;
            session->tools[i].parameters_json = config->tools[i].parameters_json;
            session->tools[i].handler = config->tools[i].handler;
            session->tools[i].user_data = config->tools[i].user_data;
        }
    }

    /* Copy handler callbacks */
    if (config) {
        session->permission_handler = config->on_permission_request;
        session->permission_user_data = config->permission_user_data;
        session->user_input_handler = config->on_user_input_request;
        session->user_input_user_data = config->user_input_user_data;
        session->hook_handler = config->on_hook;
        session->hook_user_data = config->hook_user_data;
        session->exit_plan_mode_handler = config->on_exit_plan_mode;
        session->exit_plan_mode_user_data = config->exit_plan_mode_user_data;
        session->shell_authentication_handler = config->on_shell_authentication_request;
        session->shell_authentication_user_data = config->shell_authentication_user_data;
        session->memory_store_handler = config->on_memory_store;
        session->memory_store_user_data = config->memory_store_user_data;
    }

    return session;
}

/* ============================================================================
 * Public API: Client
 * ============================================================================ */

copilot_client_t *copilot_client_create(const copilot_client_options_t *options)
{
    copilot_client_options_t opts = options ? *options : copilot_client_options_default();

    /* Client mode: "coding-agent" (default; legacy alias "copilot-cli") or "empty" */
    bool empty_mode = false;
    if (opts.mode) {
        if (strcmp(opts.mode, COPILOT_CLIENT_MODE_EMPTY) == 0) {
            empty_mode = true;
        } else if (strcmp(opts.mode, COPILOT_CLIENT_MODE_CODING_AGENT) != 0 &&
                   strcmp(opts.mode, "copilot-cli") != 0) {
            return NULL;
        }
    }

    copilot_client_t *client = calloc(1, sizeof(copilot_client_t));
    if (!client) return NULL;

    client->empty_mode = empty_mode;
    client->cli_path = opts.cli_path ? strdup(opts.cli_path) : NULL;
    client->cwd = opts.cwd ? strdup(opts.cwd) : NULL;
    client->log_level = strdup(opts.log_level ? opts.log_level : "info");
    client->use_stdio = opts.use_stdio;
    client->auto_start = opts.auto_start;
    client->auto_restart = opts.auto_restart;
    client->github_token = opts.github_token ? strdup(opts.github_token) : NULL;
    client->use_logged_in_user = opts.use_logged_in_user;

    /* Copy extra args */
    if (opts.extra_args) {
        int count = 0;
        while (opts.extra_args[count]) count++;
        client->extra_args = calloc((size_t)(count + 1), sizeof(char *));
        for (int i = 0; i < count; i++) {
            client->extra_args[i] = strdup(opts.extra_args[i]);
        }
        client->extra_args_count = count;
    }

    client->state = COPILOT_STATE_DISCONNECTED;
    client->rpc = NULL;
    client->sessions = NULL;
    pthread_mutex_init(&client->sessions_mutex, NULL);

    /* Trace context provider */
    client->trace_context_provider = opts.on_get_trace_context;
    client->trace_context_user_data = opts.trace_context_user_data;

    /* Client info for the connect handshake */
    if (opts.client_info) {
        const copilot_client_info_t *ci = opts.client_info;
        client->client_info_application_name =
            ci->application_name ? strdup(ci->application_name) : NULL;
        client->client_info_application_version =
            ci->application_version ? strdup(ci->application_version) : NULL;
        client->client_info_integration_name =
            ci->integration_name ? strdup(ci->integration_name) : NULL;
        client->client_info_integration_version =
            ci->integration_version ? strdup(ci->integration_version) : NULL;
    }

    /* GitHub token provider registry */
    client->github_token_providers = NULL;
    client->github_token_seq = 0;
    pthread_mutex_init(&client->github_token_mutex, NULL);

    /* Network callbacks */
    if (opts.network) {
        client->network_scope = opts.network->scope ? strdup(opts.network->scope) : NULL;
        client->network_resolve_proxy = opts.network->resolve_proxy;
        client->network_authenticate_negotiate = opts.network->authenticate_negotiate;
        client->network_authenticate_basic = opts.network->authenticate_basic;
        client->network_user_data = opts.network->user_data;
    }

    /* Last Policy Helper failure of a failed create/resume */
    client->last_policy_helper_failure_json = NULL;
    pthread_mutex_init(&client->last_error_mutex, NULL);

#ifdef _WIN32
    client->process_handle = NULL;
    client->stdin_write = NULL;
    client->stdout_read = NULL;
#else
    client->cli_pid = 0;
    client->stdin_write_fd = -1;
    client->stdout_read_fd = -1;
#endif

    return client;
}

copilot_error_t copilot_client_start(copilot_client_t *client)
{
    if (!client) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (client->state == COPILOT_STATE_CONNECTED) return COPILOT_ERROR_ALREADY_STARTED;

    client->state = COPILOT_STATE_CONNECTING;

    /* Spawn CLI process */
    copilot_error_t err = spawn_cli_process(client);
    if (err != COPILOT_OK) {
        client->state = COPILOT_STATE_ERROR;
        return err;
    }

    /* Create JSON-RPC client */
#ifdef _WIN32
    int write_fd = _open_osfhandle((intptr_t)client->stdin_write, _O_WRONLY);
    int read_fd = _open_osfhandle((intptr_t)client->stdout_read, _O_RDONLY);
    client->rpc = json_rpc_client_create(write_fd, read_fd);
#else
    client->rpc = json_rpc_client_create(client->stdin_write_fd, client->stdout_read_fd);
#endif

    if (!client->rpc) {
        client->state = COPILOT_STATE_ERROR;
        return COPILOT_ERROR_OUT_OF_MEMORY;
    }

    /* Register handlers */
    json_rpc_client_set_notification_handler(client->rpc, "session.event",
                                             on_session_event_notification, client);
    json_rpc_client_set_request_handler(client->rpc, "tool.call",
                                        on_tool_call_request, client);
    json_rpc_client_set_request_handler(client->rpc, "permission.request",
                                        on_permission_request, client);
    json_rpc_client_set_request_handler(client->rpc, "userInput.request",
                                        on_user_input_request, client);
    json_rpc_client_set_request_handler(client->rpc, "hooks.invoke",
                                        on_hooks_invoke, client);
    json_rpc_client_set_request_handler(client->rpc, "exitPlanMode.request",
                                        on_exit_plan_mode_request, client);
    json_rpc_client_set_request_handler(client->rpc, "gitHubToken.getToken",
                                        on_github_token_get_token, client);
    json_rpc_client_set_request_handler(client->rpc, "shellAuthentication.request",
                                        on_shell_authentication_request, client);
    json_rpc_client_set_request_handler(client->rpc, "shellAuthentication.cancel",
                                        on_shell_authentication_cancel, client);
    json_rpc_client_set_request_handler(client->rpc, "memoryStore.store",
                                        on_memory_store, client);
    if (client->network_resolve_proxy) {
        json_rpc_client_set_request_handler(client->rpc, "network.resolveProxy",
                                            on_network_resolve_proxy, client);
    }
    if (client->network_authenticate_negotiate) {
        json_rpc_client_set_request_handler(client->rpc, "network.authenticateNegotiate",
                                            on_network_authenticate_negotiate, client);
    }
    if (client->network_authenticate_basic) {
        json_rpc_client_set_request_handler(client->rpc, "network.authenticateBasic",
                                            on_network_authenticate_basic, client);
    }

    /* Start reader thread */
    if (json_rpc_client_start(client->rpc) != 0) {
        json_rpc_client_free(client->rpc);
        client->rpc = NULL;
        client->state = COPILOT_STATE_ERROR;
        return COPILOT_ERROR_IO;
    }

    /* Declare the integrating application's identity (no-op without client_info) */
    err = send_connect_handshake(client);
    if (err != COPILOT_OK) {
        json_rpc_client_stop(client->rpc);
        json_rpc_client_free(client->rpc);
        client->rpc = NULL;
        client->state = COPILOT_STATE_ERROR;
        return err;
    }

    /* Verify protocol version */
    err = verify_protocol_version(client);
    if (err == COPILOT_OK) {
        /* Register network callbacks (no-op without them) */
        err = register_network_provider(client);
    }
    if (err != COPILOT_OK) {
        json_rpc_client_stop(client->rpc);
        json_rpc_client_free(client->rpc);
        client->rpc = NULL;
        client->state = COPILOT_STATE_ERROR;
        return err;
    }

    client->state = COPILOT_STATE_CONNECTED;
    return COPILOT_OK;
}

copilot_error_t copilot_client_stop(copilot_client_t *client)
{
    if (!client) return COPILOT_ERROR_INVALID_ARGUMENT;

    /* Destroy all sessions */
    pthread_mutex_lock(&client->sessions_mutex);
    copilot_session_t *s = client->sessions;
    while (s) {
        copilot_session_t *next = s->next;
        /* Best-effort release (session.detach, falling back to session.destroy) */
        if (client->rpc) {
            release_session_on_server(client->rpc, s->session_id, 5000);
        }
        s = next;
    }
    pthread_mutex_unlock(&client->sessions_mutex);

    /* Stop RPC client */
    if (client->rpc) {
        json_rpc_client_stop(client->rpc);
        json_rpc_client_free(client->rpc);
        client->rpc = NULL;
    }

    clear_github_token_providers(client);

    /* Kill CLI process */
#ifdef _WIN32
    if (client->stdin_write) {
        CloseHandle(client->stdin_write);
        client->stdin_write = NULL;
    }
    if (client->process_handle) {
        TerminateProcess(client->process_handle, 0);
        WaitForSingleObject(client->process_handle, 5000);
        CloseHandle(client->process_handle);
        client->process_handle = NULL;
    }
#else
    if (client->stdin_write_fd >= 0) {
        close(client->stdin_write_fd);
        client->stdin_write_fd = -1;
    }
    if (client->cli_pid > 0) {
        kill(client->cli_pid, SIGTERM);
        int status;
        waitpid(client->cli_pid, &status, 0);
        client->cli_pid = 0;
    }
#endif

    client->state = COPILOT_STATE_DISCONNECTED;
    return COPILOT_OK;
}

void copilot_client_free(copilot_client_t *client)
{
    if (!client) return;

    /* Free sessions */
    copilot_session_t *s = client->sessions;
    while (s) {
        copilot_session_t *next = s->next;
        copilot_session_free(s);
        s = next;
    }

    free(client->cli_path);
    free(client->cwd);
    free(client->log_level);
    free(client->github_token);
    free(client->client_info_application_name);
    free(client->client_info_application_version);
    free(client->client_info_integration_name);
    free(client->client_info_integration_version);

    if (client->extra_args) {
        for (int i = 0; i < client->extra_args_count; i++) {
            free(client->extra_args[i]);
        }
        free(client->extra_args);
    }

    clear_github_token_providers(client);
    pthread_mutex_destroy(&client->github_token_mutex);
    free(client->network_scope);
    free(client->last_policy_helper_failure_json);
    pthread_mutex_destroy(&client->last_error_mutex);
    pthread_mutex_destroy(&client->sessions_mutex);
    free(client);
}

copilot_connection_state_t copilot_client_get_state(const copilot_client_t *client)
{
    return client ? client->state : COPILOT_STATE_DISCONNECTED;
}

/* Parses an ISO 8601 UTC timestamp ("2026-11-22T21:29:43.527Z") into Unix epoch milliseconds (0 if malformed). */
static int64_t parse_iso8601_utc_ms(const char *text)
{
    int year, month, day, hour, minute, second;
    if (sscanf(text, "%4d-%2d-%2dT%2d:%2d:%2d", &year, &month, &day, &hour, &minute, &second) != 6 ||
        month < 1 || month > 12) {
        return 0;
    }

    int64_t millis = 0;
    const char *frac = strchr(text, '.');
    if (frac) {
        int64_t scale = 100;
        for (const char *p = frac + 1; *p >= '0' && *p <= '9' && scale > 0; p++, scale /= 10) {
            millis += (int64_t)(*p - '0') * scale;
        }
    }

    /* Days since 1970-01-01 for the proleptic Gregorian calendar */
    int64_t y = (int64_t)year - (month <= 2 ? 1 : 0);
    int64_t era = (y >= 0 ? y : y - 399) / 400;
    int64_t yoe = y - era * 400;
    int64_t doy = (153 * (int64_t)(month > 2 ? month - 3 : month + 9) + 2) / 5 + day - 1;
    int64_t doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    int64_t days = era * 146097 + doe - 719468;

    return ((days * 24 + hour) * 60 + minute) * 60000 + (int64_t)second * 1000 + millis;
}

copilot_error_t copilot_client_ping(
    copilot_client_t *client,
    const char *message,
    copilot_ping_response_t *out)
{
    if (!client || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    if (message) {
        cJSON_AddStringToObject(params, "message", message);
    }

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "ping", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    memset(out, 0, sizeof(*out));

    cJSON *msg = cJSON_GetObjectItem(result, "message");
    out->message = msg && cJSON_IsString(msg) ? strdup(msg->valuestring) : strdup("");

    cJSON *ts = cJSON_GetObjectItem(result, "timestamp");
    if (ts && cJSON_IsNumber(ts)) {
        out->timestamp = (int64_t)ts->valuedouble;
    } else if (ts && cJSON_IsString(ts)) {
        out->timestamp = parse_iso8601_utc_ms(ts->valuestring);
    }

    cJSON *pv = cJSON_GetObjectItem(result, "protocolVersion");
    out->protocol_version = pv && cJSON_IsNumber(pv) ? pv->valueint : -1;

    cJSON_Delete(result);
    return COPILOT_OK;
}

void copilot_ping_response_free(copilot_ping_response_t *response)
{
    if (!response) return;
    free(response->message);
    response->message = NULL;
    response->timestamp = 0;
}

copilot_error_t copilot_client_list_models(
    copilot_client_t *client,
    copilot_model_list_t *out)
{
    if (!client || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "models.list", params, 30000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    memset(out, 0, sizeof(*out));

    cJSON *models = cJSON_GetObjectItem(result, "models");
    if (models && cJSON_IsArray(models)) {
        int count = cJSON_GetArraySize(models);
        out->models = calloc((size_t)count, sizeof(copilot_model_info_t));
        out->count = (size_t)count;

        for (int i = 0; i < count; i++) {
            cJSON *m = cJSON_GetArrayItem(models, i);
            cJSON *id = cJSON_GetObjectItem(m, "id");
            cJSON *name = cJSON_GetObjectItem(m, "name");
            cJSON *caps = cJSON_GetObjectItem(m, "capabilities");

            out->models[i].id = id && cJSON_IsString(id) ? strdup(id->valuestring) : strdup("");
            out->models[i].name = name && cJSON_IsString(name) ? strdup(name->valuestring) : strdup("");

            if (caps) {
                cJSON *supports = cJSON_GetObjectItem(caps, "supports");
                if (supports) {
                    cJSON *v = cJSON_GetObjectItem(supports, "vision");
                    out->models[i].supports_vision = v && cJSON_IsTrue(v);
                    cJSON *re = cJSON_GetObjectItem(supports, "reasoningEffort");
                    out->models[i].supports_reasoning_effort = re && cJSON_IsTrue(re);
                }
                cJSON *limits = cJSON_GetObjectItem(caps, "limits");
                if (limits) {
                    cJSON *mctw = cJSON_GetObjectItem(limits, "max_context_window_tokens");
                    out->models[i].max_context_window_tokens =
                        mctw && cJSON_IsNumber(mctw) ? mctw->valueint : 0;
                    cJSON *mot = cJSON_GetObjectItem(limits, "max_output_tokens");
                    out->models[i].max_output_tokens =
                        mot && cJSON_IsNumber(mot) ? mot->valueint : 0;
                }
            }
        }
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

void copilot_model_list_free(copilot_model_list_t *list)
{
    if (!list) return;
    for (size_t i = 0; i < list->count; i++) {
        free(list->models[i].id);
        free(list->models[i].name);
    }
    free(list->models);
    list->models = NULL;
    list->count = 0;
}

copilot_error_t copilot_client_list_sessions(
    copilot_client_t *client,
    copilot_session_list_t *out)
{
    if (!client || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "session.list", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    memset(out, 0, sizeof(*out));

    cJSON *sessions = cJSON_GetObjectItem(result, "sessions");
    if (sessions && cJSON_IsArray(sessions)) {
        int count = cJSON_GetArraySize(sessions);
        out->sessions = calloc((size_t)count, sizeof(copilot_session_metadata_t));
        out->count = (size_t)count;

        for (int i = 0; i < count; i++) {
            cJSON *s = cJSON_GetArrayItem(sessions, i);
            cJSON *sid = cJSON_GetObjectItem(s, "sessionId");
            cJSON *st = cJSON_GetObjectItem(s, "startTime");
            cJSON *mt = cJSON_GetObjectItem(s, "modifiedTime");
            cJSON *sum = cJSON_GetObjectItem(s, "summary");
            cJSON *rem = cJSON_GetObjectItem(s, "isRemote");

            out->sessions[i].session_id = sid && cJSON_IsString(sid) ? strdup(sid->valuestring) : strdup("");
            out->sessions[i].start_time = st && cJSON_IsString(st) ? strdup(st->valuestring) : strdup("");
            out->sessions[i].modified_time = mt && cJSON_IsString(mt) ? strdup(mt->valuestring) : strdup("");
            out->sessions[i].summary = sum && cJSON_IsString(sum) ? strdup(sum->valuestring) : NULL;
            out->sessions[i].is_remote = rem && cJSON_IsTrue(rem);
        }
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

void copilot_session_list_free(copilot_session_list_t *list)
{
    if (!list) return;
    for (size_t i = 0; i < list->count; i++) {
        free(list->sessions[i].session_id);
        free(list->sessions[i].start_time);
        free(list->sessions[i].modified_time);
        free(list->sessions[i].summary);
    }
    free(list->sessions);
    list->sessions = NULL;
    list->count = 0;
}

copilot_error_t copilot_client_delete_session(
    copilot_client_t *client,
    const char *session_id)
{
    if (!client || !session_id) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session_id);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "session.delete", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON *success = cJSON_GetObjectItem(result, "success");
    bool ok = success && cJSON_IsTrue(success);
    cJSON_Delete(result);

    if (!ok) return COPILOT_ERROR_RPC;

    /* Remove from local tracking */
    copilot_session_t *s = find_session(client, session_id);
    if (s) {
        remove_session(client, s);
    }

    return COPILOT_OK;
}

copilot_error_t copilot_client_get_last_session_id(
    copilot_client_t *client,
    char **out)
{
    if (!client || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    *out = NULL;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "session.getLastId", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON *sid = cJSON_GetObjectItem(result, "sessionId");
    if (sid && cJSON_IsString(sid)) {
        *out = strdup(sid->valuestring);
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

copilot_error_t copilot_client_get_session_metadata(
    copilot_client_t *client,
    const char *session_id,
    char **out_json)
{
    if (!client || !session_id || !out_json) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    *out_json = NULL;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session_id);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "session.getMetadata", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    *out_json = cJSON_PrintUnformatted(result);
    cJSON_Delete(result);
    return COPILOT_OK;
}

copilot_error_t copilot_client_get_status(
    copilot_client_t *client,
    char **out_json)
{
    if (!client || !out_json) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    *out_json = NULL;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "status.get", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    *out_json = cJSON_PrintUnformatted(result);
    cJSON_Delete(result);
    return COPILOT_OK;
}

copilot_error_t copilot_client_get_auth_status(
    copilot_client_t *client,
    char **out_json)
{
    if (!client || !out_json) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    *out_json = NULL;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "auth.getStatus", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    *out_json = cJSON_PrintUnformatted(result);
    cJSON_Delete(result);
    return COPILOT_OK;
}

copilot_error_t copilot_client_get_foreground_session_id(
    copilot_client_t *client,
    char **out)
{
    if (!client || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "session.getForeground", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON *sid = cJSON_GetObjectItem(result, "sessionId");
    *out = (sid && cJSON_IsString(sid)) ? strdup(sid->valuestring) : NULL;
    cJSON_Delete(result);

    return COPILOT_OK;
}

copilot_error_t copilot_client_set_foreground_session_id(
    copilot_client_t *client,
    const char *session_id)
{
    if (!client || !session_id) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session_id);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "session.setForeground", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON *success = cJSON_GetObjectItem(result, "success");
    bool ok = success && cJSON_IsTrue(success);
    cJSON_Delete(result);

    if (!ok) return COPILOT_ERROR_RPC;

    return COPILOT_OK;
}

copilot_error_t copilot_client_clear_managed_settings_cache(copilot_client_t *client)
{
    if (!client) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "managedSettings.clearCache", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

/* ============================================================================
 * Public API: Session creation
 * ============================================================================ */

copilot_error_t copilot_client_create_session(
    copilot_client_t *client,
    const copilot_session_config_t *config,
    copilot_session_t **out)
{
    if (!client || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (config && config->github_token && config->github_token_provider) {
        /* github_token and github_token_provider are mutually exclusive */
        return COPILOT_ERROR_INVALID_ARGUMENT;
    }
    if (client->empty_mode && (!config || !config->available_tools)) {
        /* Client mode "empty" requires an explicit available_tools list */
        return COPILOT_ERROR_INVALID_ARGUMENT;
    }

    /* Auto-start if needed */
    if (client->state != COPILOT_STATE_CONNECTED && client->auto_start) {
        copilot_error_t err = copilot_client_start(client);
        if (err != COPILOT_OK) return err;
    }
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    copilot_session_config_t default_config = copilot_session_config_default();
    if (!config) config = &default_config;

    cJSON *params = build_session_params(config, false);
    inject_trace_context(client, params);

    char *gh_registration_id = NULL;
    if (config->github_token_provider) {
        gh_registration_id = register_github_token_provider(
            client, config->github_token_provider,
            config->github_token_provider_user_data, config->session_id);
        if (!gh_registration_id) {
            cJSON_Delete(params);
            return COPILOT_ERROR_OUT_OF_MEMORY;
        }
        cJSON_AddStringToObject(params, "gitHubTokenProviderRegistrationId", gh_registration_id);
    }

    /* SDK protocol 4: move the flat options into their option groups */
    params = group_session_params(params, false);
    if (!params) {
        if (gh_registration_id) {
            unregister_github_token_provider(client, gh_registration_id);
            free(gh_registration_id);
        }
        return COPILOT_ERROR_OUT_OF_MEMORY;
    }
    if (client->empty_mode) {
        apply_empty_mode_defaults(params);
    }

    record_policy_helper_failure(client, NULL);
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *err_data = NULL;
    cJSON *result = json_rpc_client_request_ex(client->rpc, "session.create", params, 30000,
                                               &err_code, &err_msg, &err_data);
    cJSON_Delete(params);

    if (!result) {
        record_policy_helper_failure(client, err_data);
        cJSON_Delete(err_data);
        free(err_msg);
        if (gh_registration_id) {
            unregister_github_token_provider(client, gh_registration_id);
            free(gh_registration_id);
        }
        return COPILOT_ERROR_RPC;
    }

    copilot_session_t *session = create_session_from_response(client, result, config);
    cJSON_Delete(result);

    if (!session) {
        if (gh_registration_id) {
            unregister_github_token_provider(client, gh_registration_id);
            free(gh_registration_id);
        }
        return COPILOT_ERROR_OUT_OF_MEMORY;
    }

    if (gh_registration_id) {
        assign_github_token_provider_session(client, gh_registration_id, session->session_id);
        session->github_token_registration_id = gh_registration_id;
    }

    add_session(client, session);
    *out = session;
    return COPILOT_OK;
}

copilot_error_t copilot_client_resume_session(
    copilot_client_t *client,
    const char *session_id,
    const copilot_session_config_t *config,
    copilot_session_t **out)
{
    if (!client || !session_id || !out) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (config && config->github_token && config->github_token_provider) {
        /* github_token and github_token_provider are mutually exclusive */
        return COPILOT_ERROR_INVALID_ARGUMENT;
    }
    if (client->empty_mode && (!config || !config->available_tools)) {
        /* Client mode "empty" requires an explicit available_tools list */
        return COPILOT_ERROR_INVALID_ARGUMENT;
    }

    /* Auto-start if needed */
    if (client->state != COPILOT_STATE_CONNECTED && client->auto_start) {
        copilot_error_t err = copilot_client_start(client);
        if (err != COPILOT_OK) return err;
    }
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    copilot_session_config_t default_config = copilot_session_config_default();
    if (!config) config = &default_config;

    cJSON *params = build_session_params(config, true);
    cJSON_DeleteItemFromObjectCaseSensitive(params, "sessionId");
    cJSON_AddStringToObject(params, "sessionId", session_id);
    inject_trace_context(client, params);

    char *gh_registration_id = NULL;
    if (config->github_token_provider) {
        gh_registration_id = register_github_token_provider(
            client, config->github_token_provider,
            config->github_token_provider_user_data, session_id);
        if (!gh_registration_id) {
            cJSON_Delete(params);
            return COPILOT_ERROR_OUT_OF_MEMORY;
        }
        cJSON_AddStringToObject(params, "gitHubTokenProviderRegistrationId", gh_registration_id);
    }

    /* SDK protocol 4: move the flat options into their option groups */
    params = group_session_params(params, true);
    if (!params) {
        if (gh_registration_id) {
            unregister_github_token_provider(client, gh_registration_id);
            free(gh_registration_id);
        }
        return COPILOT_ERROR_OUT_OF_MEMORY;
    }
    if (client->empty_mode) {
        apply_empty_mode_defaults(params);
    }

    record_policy_helper_failure(client, NULL);
    int err_code = 0;
    char *err_msg = NULL;
    cJSON *err_data = NULL;
    cJSON *result = json_rpc_client_request_ex(client->rpc, "session.resume", params, 30000,
                                               &err_code, &err_msg, &err_data);
    cJSON_Delete(params);

    if (!result) {
        record_policy_helper_failure(client, err_data);
        cJSON_Delete(err_data);
        free(err_msg);
        if (gh_registration_id) {
            unregister_github_token_provider(client, gh_registration_id);
            free(gh_registration_id);
        }
        return COPILOT_ERROR_RPC;
    }

    copilot_session_t *session = create_session_from_response(client, result, config);
    cJSON_Delete(result);

    if (!session) {
        if (gh_registration_id) {
            unregister_github_token_provider(client, gh_registration_id);
            free(gh_registration_id);
        }
        return COPILOT_ERROR_OUT_OF_MEMORY;
    }

    if (gh_registration_id) {
        assign_github_token_provider_session(client, gh_registration_id, session->session_id);
        session->github_token_registration_id = gh_registration_id;
    }

    add_session(client, session);
    *out = session;
    return COPILOT_OK;
}

/* ============================================================================
 * Public API: Session Filesystem Provider
 * ============================================================================ */

copilot_error_t copilot_set_session_fs_provider(
    copilot_client_t *client,
    const char *initial_cwd,
    const char *session_state_path)
{
    if (!client) return COPILOT_ERROR_INVALID_ARGUMENT;

    /* Auto-start if needed */
    if (client->state != COPILOT_STATE_CONNECTED && client->auto_start) {
        copilot_error_t err = copilot_client_start(client);
        if (err != COPILOT_OK) return err;
    }
    if (!client->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    if (initial_cwd) {
        cJSON_AddStringToObject(params, "initialCwd", initial_cwd);
    }
    if (session_state_path) {
        cJSON_AddStringToObject(params, "sessionStatePath", session_state_path);
    }

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(client->rpc, "sessionFs.setProvider", params, 30000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

/* ============================================================================
 * Public API: Session
 * ============================================================================ */

const char *copilot_session_get_id(const copilot_session_t *session)
{
    return session ? session->session_id : NULL;
}

const char *copilot_session_get_workspace_path(const copilot_session_t *session)
{
    return session ? session->workspace_path : NULL;
}

copilot_error_t copilot_session_send(
    copilot_session_t *session,
    const copilot_message_options_t *options,
    char **out_message_id)
{
    if (!session || !options || !options->prompt) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);
    cJSON_AddStringToObject(params, "prompt", options->prompt);

    if (options->mode) {
        cJSON_AddStringToObject(params, "mode", options->mode);
    }
    if (options->agent_mode) {
        cJSON_AddStringToObject(params, "agentMode", options->agent_mode);
    }
    if (options->display_prompt) {
        cJSON_AddStringToObject(params, "displayPrompt", options->display_prompt);
    }
    if (options->source) {
        cJSON_AddStringToObject(params, "source", options->source);
    }
    if (options->client_turn_id) {
        cJSON_AddStringToObject(params, "clientTurnId", options->client_turn_id);
    }
    if (options->request_headers_keys && options->request_headers_values) {
        cJSON *headers = cJSON_CreateObject();
        for (size_t i = 0; options->request_headers_keys[i] && options->request_headers_values[i]; i++) {
            cJSON_AddStringToObject(headers, options->request_headers_keys[i],
                                    options->request_headers_values[i]);
        }
        cJSON_AddItemToObject(params, "requestHeaders", headers);
    }

    /* Response format: structured output (responseSchema) supersedes the legacy string */
    if (options->response_schema_json) {
        cJSON *schema = cJSON_Parse(options->response_schema_json);
        if (!schema) {
            cJSON_Delete(params);
            return COPILOT_ERROR_JSON_PARSE;
        }
        cJSON *response_format = cJSON_CreateObject();
        cJSON_AddStringToObject(response_format, "type", "json_schema");
        cJSON *json_schema = cJSON_AddObjectToObject(response_format, "jsonSchema");
        cJSON_AddStringToObject(json_schema, "name", "response");
        cJSON_AddBoolToObject(json_schema, "strict", true);
        cJSON_AddItemToObject(json_schema, "schema", schema);
        cJSON_AddItemToObject(params, "responseFormat", response_format);
    } else if (options->response_format != COPILOT_RESPONSE_FORMAT_TEXT) {
        switch (options->response_format) {
        case COPILOT_RESPONSE_FORMAT_IMAGE:
            cJSON_AddStringToObject(params, "responseFormat", "image");
            break;
        case COPILOT_RESPONSE_FORMAT_JSON_OBJECT:
            cJSON_AddStringToObject(params, "responseFormat", "json_object");
            break;
        default:
            break;
        }
    }

    /* Image generation options */
    if (options->image_options) {
        cJSON *img_opts = cJSON_CreateObject();
        if (options->image_options->size)
            cJSON_AddStringToObject(img_opts, "size", options->image_options->size);
        if (options->image_options->quality)
            cJSON_AddStringToObject(img_opts, "quality", options->image_options->quality);
        if (options->image_options->style)
            cJSON_AddStringToObject(img_opts, "style", options->image_options->style);
        cJSON_AddItemToObject(params, "imageOptions", img_opts);
    }

    /* Attachments */
    if (options->attachments && options->attachments_count > 0) {
        cJSON *arr = cJSON_CreateArray();
        for (size_t i = 0; i < options->attachments_count; i++) {
            const copilot_attachment_t *a = &options->attachments[i];
            cJSON *att = cJSON_CreateObject();

            switch (a->type) {
            case COPILOT_ATTACHMENT_FILE:
                cJSON_AddStringToObject(att, "type", "file");
                cJSON_AddStringToObject(att, "path", a->path);
                break;
            case COPILOT_ATTACHMENT_DIRECTORY:
                cJSON_AddStringToObject(att, "type", "directory");
                cJSON_AddStringToObject(att, "path", a->path);
                break;
            case COPILOT_ATTACHMENT_SELECTION:
                cJSON_AddStringToObject(att, "type", "selection");
                if (a->file_path) cJSON_AddStringToObject(att, "filePath", a->file_path);
                if (a->display_name) cJSON_AddStringToObject(att, "displayName", a->display_name);
                if (a->text) cJSON_AddStringToObject(att, "text", a->text);
                break;
            }

            if (a->display_name && a->type != COPILOT_ATTACHMENT_SELECTION) {
                cJSON_AddStringToObject(att, "displayName", a->display_name);
            }

            cJSON_AddItemToArray(arr, att);
        }
        cJSON_AddItemToObject(params, "attachments", arr);
    }

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(session->rpc, "session.send", params, 30000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        free(err_msg);
        return COPILOT_ERROR_RPC;
    }

    if (out_message_id) {
        cJSON *mid = cJSON_GetObjectItem(result, "messageId");
        *out_message_id = mid && cJSON_IsString(mid) ? strdup(mid->valuestring) : NULL;
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

copilot_error_t copilot_session_send_and_wait(
    copilot_session_t *session,
    const copilot_message_options_t *options,
    int timeout_ms,
    char **out_content)
{
    if (!session || !options) return COPILOT_ERROR_INVALID_ARGUMENT;

    if (timeout_ms <= 0) timeout_ms = 60000;

    /* Reset idle state before sending */
    pthread_mutex_lock(&session->idle_mutex);
    session->idle_signaled = false;
    session->error_signaled = false;
    free(session->error_message);
    session->error_message = NULL;
    free(session->last_assistant_content);
    session->last_assistant_content = NULL;
    pthread_mutex_unlock(&session->idle_mutex);

    /* Send message */
    copilot_error_t err = copilot_session_send(session, options, NULL);
    if (err != COPILOT_OK) return err;

    /* Wait for idle or error */
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

    pthread_mutex_lock(&session->idle_mutex);
    while (!session->idle_signaled && !session->error_signaled) {
        int rc = pthread_cond_timedwait(&session->idle_cond, &session->idle_mutex, &ts);
        if (rc == ETIMEDOUT) {
            pthread_mutex_unlock(&session->idle_mutex);
            return COPILOT_ERROR_TIMEOUT;
        }
    }

    if (session->error_signaled) {
        pthread_mutex_unlock(&session->idle_mutex);
        return COPILOT_ERROR_SESSION_ERROR;
    }

    if (out_content && session->last_assistant_content) {
        *out_content = strdup(session->last_assistant_content);
    } else if (out_content) {
        *out_content = NULL;
    }

    pthread_mutex_unlock(&session->idle_mutex);
    return COPILOT_OK;
}

copilot_error_t copilot_session_send_and_wait_structured(
    copilot_session_t *session,
    const copilot_message_options_t *options,
    const char *response_schema_json,
    int timeout_ms,
    char **out_json)
{
    if (!session || !options || !response_schema_json || !out_json) {
        return COPILOT_ERROR_INVALID_ARGUMENT;
    }
    *out_json = NULL;

    copilot_message_options_t structured = *options;
    structured.response_schema_json = response_schema_json;

    char *content = NULL;
    copilot_error_t err = copilot_session_send_and_wait(session, &structured, timeout_ms, &content);
    if (err != COPILOT_OK) {
        free(content);
        return err;
    }

    /* The run must complete with non-blank assistant content */
    if (!content || content[strspn(content, " \t\r\n")] == '\0') {
        free(content);
        return COPILOT_ERROR_SESSION_ERROR;
    }

    cJSON *parsed = cJSON_Parse(content);
    free(content);
    if (!parsed) return COPILOT_ERROR_JSON_PARSE;

    *out_json = cJSON_PrintUnformatted(parsed);
    cJSON_Delete(parsed);
    return *out_json ? COPILOT_OK : COPILOT_ERROR_OUT_OF_MEMORY;
}

int copilot_session_on(
    copilot_session_t *session,
    copilot_session_event_handler_fn handler,
    void *user_data)
{
    return copilot_session_on_type(session, NULL, handler, user_data);
}

int copilot_session_on_type(
    copilot_session_t *session,
    const char *event_type,
    copilot_session_event_handler_fn handler,
    void *user_data)
{
    if (!session || !handler) return -1;

    session_handler_entry_t *entry = calloc(1, sizeof(session_handler_entry_t));
    if (!entry) return -1;

    pthread_mutex_lock(&session->handlers_mutex);
    int id = session->next_handler_id++;
    entry->id = id;
    entry->event_type_filter = event_type ? event_type : NULL;
    entry->handler = handler;
    entry->user_data = user_data;
    entry->next = session->handlers;
    session->handlers = entry;
    pthread_mutex_unlock(&session->handlers_mutex);

    return id;
}

void copilot_session_off(copilot_session_t *session, int handler_id)
{
    if (!session) return;

    pthread_mutex_lock(&session->handlers_mutex);
    session_handler_entry_t **pp = &session->handlers;
    while (*pp) {
        if ((*pp)->id == handler_id) {
            session_handler_entry_t *entry = *pp;
            *pp = entry->next;
            free(entry);
            break;
        }
        pp = &(*pp)->next;
    }
    pthread_mutex_unlock(&session->handlers_mutex);
}

copilot_error_t copilot_session_destroy(copilot_session_t *session)
{
    if (!session) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    /* session.detach, falling back to session.destroy on older CLIs */
    release_session_on_server(session->rpc, session->session_id, 10000);

    /* Remove from client tracking */
    if (session->owner) {
        remove_session(session->owner, session);
    }

    /* Drop the GitHub token provider registration */
    if (session->github_token_registration_id) {
        if (session->owner) {
            unregister_github_token_provider(session->owner, session->github_token_registration_id);
        }
        free(session->github_token_registration_id);
        session->github_token_registration_id = NULL;
    }

    /* Clear handlers */
    pthread_mutex_lock(&session->handlers_mutex);
    session_handler_entry_t *h = session->handlers;
    while (h) {
        session_handler_entry_t *next = h->next;
        free(h);
        h = next;
    }
    session->handlers = NULL;
    pthread_mutex_unlock(&session->handlers_mutex);

    return COPILOT_OK;
}

void copilot_session_free(copilot_session_t *session)
{
    if (!session) return;

    free(session->session_id);
    free(session->workspace_path);
    free(session->tools);
    free(session->last_assistant_content);
    free(session->error_message);
    free(session->github_token_registration_id);

    /* Free remaining handlers */
    session_handler_entry_t *h = session->handlers;
    while (h) {
        session_handler_entry_t *next = h->next;
        free(h);
        h = next;
    }

    pthread_mutex_destroy(&session->handlers_mutex);
    pthread_mutex_destroy(&session->idle_mutex);
    pthread_cond_destroy(&session->idle_cond);

    free(session);
}

copilot_error_t copilot_session_abort(copilot_session_t *session)
{
    if (!session) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(session->rpc, "session.abort", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        /* Protocol 4 reports abort failures as JSON-RPC errors */
        copilot_error_t err = err_code ? COPILOT_ERROR_RPC : COPILOT_ERROR_IO;
        free(err_msg);
        return err;
    }

    cJSON_Delete(result);
    free(err_msg);

    return COPILOT_OK;
}

copilot_error_t copilot_session_get_metadata(
    copilot_session_t *session,
    char **out_json)
{
    if (!session) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(session->rpc, "session.getMetadata", params, 10000,
                                            &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        copilot_error_t err = err_code ? COPILOT_ERROR_RPC : COPILOT_ERROR_IO;
        free(err_msg);
        return err;
    }

    if (out_json) {
        *out_json = cJSON_PrintUnformatted(result);
    }

    cJSON_Delete(result);
    free(err_msg);

    return COPILOT_OK;
}

/* Sends a session-scoped request and returns its result as a JSON string. */
static copilot_error_t session_request_json(copilot_session_t *session, const char *method,
                                            cJSON *params, char **out_json)
{
    if (out_json) *out_json = NULL;

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(session->rpc, method, params, 30000,
                                            &err_code, &err_msg);
    if (!result) {
        copilot_error_t err = err_code ? COPILOT_ERROR_RPC : COPILOT_ERROR_IO;
        free(err_msg);
        return err;
    }

    if (out_json) {
        *out_json = cJSON_PrintUnformatted(result);
    }

    cJSON_Delete(result);
    free(err_msg);
    return COPILOT_OK;
}

copilot_error_t copilot_session_set_auto_tier(
    copilot_session_t *session,
    const char *auto_tier,
    char **out_json)
{
    if (!session) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);
    if (auto_tier) {
        cJSON_AddStringToObject(params, "autoTier", auto_tier);
    } else {
        /* null returns to the provider's default Auto routing */
        cJSON_AddNullToObject(params, "autoTier");
    }

    copilot_error_t err = session_request_json(session, "session.model.switchAutoTier", params, out_json);
    cJSON_Delete(params);
    return err;
}

copilot_error_t copilot_session_get_managed_settings(
    copilot_session_t *session,
    char **out_json)
{
    if (!session) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);

    copilot_error_t err = session_request_json(session, "session.managedSettings.get", params, out_json);
    cJSON_Delete(params);
    return err;
}

copilot_error_t copilot_session_recheck_policy_helper(
    copilot_session_t *session,
    char **out_json)
{
    if (!session) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);

    copilot_error_t err = session_request_json(session, "session.managedSettings.recheckPolicyHelper",
                                               params, out_json);
    cJSON_Delete(params);
    return err;
}

/* Returns a heap copy of obj[key] when it is a string, else NULL. */
static char *dup_json_string(cJSON *obj, const char *key)
{
    cJSON *item = cJSON_GetObjectItemCaseSensitive(obj, key);
    return (item && cJSON_IsString(item)) ? strdup(item->valuestring) : NULL;
}

copilot_error_t copilot_session_set_permission_mode(
    copilot_session_t *session,
    const char *mode,
    copilot_permission_mode_result_t *out)
{
    if (out) memset(out, 0, sizeof(*out));
    if (!session || !mode) return COPILOT_ERROR_INVALID_ARGUMENT;
    if (!session->rpc) return COPILOT_ERROR_NOT_CONNECTED;

    cJSON *params = cJSON_CreateObject();
    cJSON_AddStringToObject(params, "sessionId", session->session_id);
    cJSON_AddStringToObject(params, "mode", mode);

    int err_code = 0;
    char *err_msg = NULL;
    cJSON *result = json_rpc_client_request(session->rpc, "session.permissions.setMode", params,
                                            30000, &err_code, &err_msg);
    cJSON_Delete(params);

    if (!result) {
        copilot_error_t err = err_code ? COPILOT_ERROR_RPC : COPILOT_ERROR_IO;
        free(err_msg);
        return err;
    }

    if (out) {
        cJSON *success = cJSON_GetObjectItemCaseSensitive(result, "success");
        out->success = success && cJSON_IsTrue(success);
        out->mode = dup_json_string(result, "mode");
        /* Known refusal reason: COPILOT_PERMISSION_MODE_REASON_MANAGED_POLICY */
        out->reason = dup_json_string(result, "reason");
    }

    cJSON_Delete(result);
    return COPILOT_OK;
}

void copilot_permission_mode_result_free(copilot_permission_mode_result_t *result)
{
    if (!result) return;
    free(result->mode);
    free(result->reason);
    memset(result, 0, sizeof(*result));
}

/* ============================================================================
 * Public API: Policy Helper failures
 * ============================================================================ */

/* Parses a PolicyHelperFailure from `root` or from root.policyHelperFailure. */
static bool parse_policy_helper_failure(cJSON *root, copilot_policy_helper_failure_t *out)
{
    memset(out, 0, sizeof(*out));
    cJSON *nested = cJSON_GetObjectItemCaseSensitive(root, "policyHelperFailure");
    cJSON *failure = nested ? nested : root;
    if (!failure || !cJSON_IsObject(failure)) return false;

    cJSON *category = cJSON_GetObjectItemCaseSensitive(failure, "category");
    cJSON *message = cJSON_GetObjectItemCaseSensitive(failure, "message");
    if (!category || !cJSON_IsString(category) || !message || !cJSON_IsString(message)) {
        return false;
    }

    out->category = dup_json_string(failure, "category");
    out->failure_mode = dup_json_string(failure, "failureMode");
    out->message = dup_json_string(failure, "message");
    cJSON *retained = cJSON_GetObjectItemCaseSensitive(failure, "previousPolicyRetained");
    out->previous_policy_retained = retained && cJSON_IsTrue(retained);
    out->reason = dup_json_string(failure, "reason");
    out->severity = dup_json_string(failure, "severity");
    out->authority = dup_json_string(failure, "authority");
    out->code = dup_json_string(failure, "code");
    out->diagnostic = dup_json_string(failure, "diagnostic");
    out->remediation = dup_json_string(failure, "remediation");

    cJSON *details = cJSON_GetObjectItemCaseSensitive(failure, "details");
    if (details && cJSON_IsArray(details)) {
        out->details_json = cJSON_PrintUnformatted(details);
    }
    cJSON *help_link = cJSON_GetObjectItemCaseSensitive(failure, "helpLink");
    if (help_link && cJSON_IsObject(help_link)) {
        out->help_link_url = dup_json_string(help_link, "url");
        out->help_link_label = dup_json_string(help_link, "label");
    }
    return true;
}

bool copilot_policy_helper_failure_parse(const char *json, copilot_policy_helper_failure_t *out)
{
    if (!out) return false;
    memset(out, 0, sizeof(*out));
    if (!json) return false;

    cJSON *root = cJSON_Parse(json);
    if (!root) return false;

    bool found = parse_policy_helper_failure(root, out);
    cJSON_Delete(root);
    return found;
}

bool copilot_client_get_last_policy_helper_failure(
    copilot_client_t *client,
    copilot_policy_helper_failure_t *out)
{
    if (!out) return false;
    memset(out, 0, sizeof(*out));
    if (!client) return false;

    pthread_mutex_lock(&client->last_error_mutex);
    char *json = client->last_policy_helper_failure_json
                     ? strdup(client->last_policy_helper_failure_json) : NULL;
    pthread_mutex_unlock(&client->last_error_mutex);

    bool found = copilot_policy_helper_failure_parse(json, out);
    free(json);
    return found;
}

void copilot_policy_helper_failure_free(copilot_policy_helper_failure_t *failure)
{
    if (!failure) return;
    free(failure->category);
    free(failure->failure_mode);
    free(failure->message);
    free(failure->reason);
    free(failure->severity);
    free(failure->authority);
    free(failure->code);
    free(failure->details_json);
    free(failure->diagnostic);
    free(failure->help_link_url);
    free(failure->help_link_label);
    free(failure->remediation);
    memset(failure, 0, sizeof(*failure));
}

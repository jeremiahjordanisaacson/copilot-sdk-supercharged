/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file copilot.h
 * @brief GitHub Copilot SDK for C - Main public API header.
 *
 * This SDK communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio.
 * It follows the same patterns as the Node.js, Python, Go, and .NET SDKs.
 *
 * Usage:
 *   1. Create a client with copilot_client_create()
 *   2. Start the client with copilot_client_start()
 *   3. Create a session with copilot_client_create_session()
 *   4. Send messages with copilot_session_send() or copilot_session_send_and_wait()
 *   5. Clean up with copilot_session_destroy(), copilot_client_stop(), copilot_client_free()
 */

#ifndef COPILOT_COPILOT_H
#define COPILOT_COPILOT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Version
 * ============================================================================ */

#define COPILOT_SDK_VERSION_MAJOR 0
#define COPILOT_SDK_VERSION_MINOR 1
#define COPILOT_SDK_VERSION_PATCH 0

/* ============================================================================
 * Error codes
 * ============================================================================ */

typedef enum {
    COPILOT_OK = 0,
    COPILOT_ERROR_INVALID_ARGUMENT = -1,
    COPILOT_ERROR_NOT_CONNECTED = -2,
    COPILOT_ERROR_SPAWN_FAILED = -3,
    COPILOT_ERROR_PROTOCOL_MISMATCH = -4,
    COPILOT_ERROR_TIMEOUT = -5,
    COPILOT_ERROR_JSON_PARSE = -6,
    COPILOT_ERROR_RPC = -7,
    COPILOT_ERROR_SESSION_NOT_FOUND = -8,
    COPILOT_ERROR_TOOL_NOT_FOUND = -9,
    COPILOT_ERROR_HANDLER_FAILED = -10,
    COPILOT_ERROR_OUT_OF_MEMORY = -11,
    COPILOT_ERROR_IO = -12,
    COPILOT_ERROR_ALREADY_STARTED = -13,
    COPILOT_ERROR_SESSION_ERROR = -14,
} copilot_error_t;

/**
 * Returns a human-readable description of an error code.
 */
const char *copilot_error_string(copilot_error_t err);

/* ============================================================================
 * Connection state
 * ============================================================================ */

typedef enum {
    COPILOT_STATE_DISCONNECTED = 0,
    COPILOT_STATE_CONNECTING,
    COPILOT_STATE_CONNECTED,
    COPILOT_STATE_ERROR,
} copilot_connection_state_t;

/* ============================================================================
 * Opaque types
 * ============================================================================ */

/** Opaque client handle. */
typedef struct copilot_client copilot_client_t;

/** Opaque session handle. */
typedef struct copilot_session copilot_session_t;

/* ============================================================================
 * Tool types
 * ============================================================================ */

/**
 * Result type for tool invocations.
 */
typedef enum {
    COPILOT_TOOL_RESULT_SUCCESS = 0,
    COPILOT_TOOL_RESULT_FAILURE,
    COPILOT_TOOL_RESULT_REJECTED,
    COPILOT_TOOL_RESULT_DENIED,
} copilot_tool_result_type_t;

/**
 * Tool invocation context passed to tool handlers.
 */
typedef struct {
    const char *session_id;
    const char *tool_call_id;
    const char *tool_name;
    const char *arguments_json;  /**< JSON string of the arguments */
} copilot_tool_invocation_t;

/**
 * Tool result returned by tool handlers.
 * The caller must set text_result_for_llm and result_type at minimum.
 * All string fields must remain valid until the result is consumed by the SDK.
 */
typedef struct {
    const char *text_result_for_llm;
    copilot_tool_result_type_t result_type;
    const char *error;           /**< Optional error message (NULL if none) */
    const char *session_log;     /**< Optional session log (NULL if none) */
} copilot_tool_result_t;

/**
 * Tool handler function pointer.
 *
 * @param invocation  The tool invocation context.
 * @param user_data   User-provided context pointer from copilot_tool_t.
 * @param out_result  Output: the handler must fill this with the result.
 * @return COPILOT_OK on success, or an error code.
 */
typedef copilot_error_t (*copilot_tool_handler_fn)(
    const copilot_tool_invocation_t *invocation,
    void *user_data,
    copilot_tool_result_t *out_result
);

/**
 * Tool definition.
 */
typedef struct {
    const char *name;
    const char *description;         /**< Optional (NULL for none) */
    const char *parameters_json;     /**< Optional JSON Schema string (NULL for none) */
    const char *defer;               /**< Tool loading policy: "auto" (lazy) or "never" (eager), or NULL */
    copilot_tool_handler_fn handler;
    void *user_data;                 /**< Passed to handler on invocation */
    bool idempotent;                 /**< Safe to retry: repeated calls with the same arguments have no extra effect (idempotent) */
} copilot_tool_t;

/* ============================================================================
 * Session event types
 * ============================================================================ */

/** Live shell output event type (tool.shell_output). Not persisted or replayed. */
#define COPILOT_EVENT_TOOL_SHELL_OUTPUT    "tool.shell_output"
/** Shell output streams (copilot_shell_output_data_t.stream). */
#define COPILOT_SHELL_OUTPUT_STREAM_STDOUT "stdout"
#define COPILOT_SHELL_OUTPUT_STREAM_STDERR "stderr"

/** Parsed data of a tool.shell_output event. Strings are valid only during the callback. */
typedef struct {
    const char *tool_call_id;   /**< Tool call that owns this output */
    const char *text;           /**< New output to append (chunks need not be complete lines) */
    int64_t sequence;           /**< Zero-based publication sequence across streams for this tool call */
    const char *stream;         /**< "stdout", "stderr", or NULL (omitted means stdout) */
} copilot_shell_output_data_t;

/**
 * Session event. The type field determines which data fields are populated.
 * String fields are owned by the SDK and valid only for the duration of the callback.
 */
typedef struct {
    const char *type;       /**< Event type string (e.g., "assistant.message", "session.idle") */
    const char *raw_json;   /**< Full raw JSON of the event data */

    /* Convenience fields populated for common event types */
    const char *content;           /**< For assistant.message: message content */
    const char *message;           /**< For session.error: error message */
    const char *delta_content;     /**< For assistant.message_delta: incremental content */
    const char *tool_name;         /**< For tool.executing/tool.executed: tool name */
    const char *tool_call_id;      /**< For tool events: tool call ID */
    const copilot_shell_output_data_t *shell_output; /**< For tool.shell_output: parsed data, else NULL */
} copilot_session_event_t;

/**
 * Session event handler callback.
 *
 * @param event      The session event. Valid only for the duration of this call.
 * @param user_data  User-provided context pointer.
 */
typedef void (*copilot_session_event_handler_fn)(
    const copilot_session_event_t *event,
    void *user_data
);

/* ============================================================================
 * Permission types
 * ============================================================================ */

typedef struct {
    const char *kind;          /**< "shell", "write", "mcp", "read", "url" */
    const char *tool_call_id;  /**< Optional */
    const char *raw_json;      /**< Full permission request JSON */
} copilot_permission_request_t;

typedef enum {
    COPILOT_PERMISSION_APPROVED = 0,
    COPILOT_PERMISSION_DENIED_BY_RULES,
    COPILOT_PERMISSION_DENIED_NO_RULE,
    COPILOT_PERMISSION_DENIED_BY_USER,
} copilot_permission_kind_t;

typedef struct {
    copilot_permission_kind_t kind;
    const char *decision_context_json; /**< Opaque decision context (decisionContext) echoed on the reply as JSON, or NULL */
} copilot_permission_result_t;

/**
 * Permission handler callback.
 */
typedef copilot_error_t (*copilot_permission_handler_fn)(
    const copilot_permission_request_t *request,
    const char *session_id,
    void *user_data,
    copilot_permission_result_t *out_result
);

/* ============================================================================
 * User input types
 * ============================================================================ */

/** Image support advertised by a userInput.request (imageAttachmentConstraints).
 *  Owned by the SDK and valid only for the duration of the handler call. */
typedef struct {
    const char **allowed_mime_types;   /**< NULL-terminated array of accepted MIME types, such as "image/png" */
    size_t allowed_mime_types_count;   /**< Number of entries in allowed_mime_types */
    int64_t max_bytes_per_image;       /**< Maximum size of one image, in bytes */
    int max_count;                     /**< Maximum number of images in one answer */
} copilot_user_input_image_attachment_constraints_t;

/** An inline image attached to an ask_user answer (copilot_user_input_response_t.attachments).
 *  The wire type is always "image". Allocate every string with malloc()/strdup(); the SDK frees them. */
typedef struct {
    char *data;           /**< Base64-encoded image bytes (required) */
    char *mime_type;      /**< MIME type of the image, such as "image/png" (required) */
    char *description;    /**< Optional description, or NULL */
    char *display_name;   /**< Optional display name, or NULL */
} copilot_user_input_image_attachment_t;

/** A user input request (userInput.request). Strings and arrays are owned by the SDK
 *  and valid only for the duration of the handler call. */
typedef struct {
    const char *question;
    const char **choices;       /**< NULL-terminated array, or NULL if no choices */
    int choices_count;
    bool allow_freeform;
    bool has_allow_freeform;    /**< Whether allow_freeform was explicitly provided */
    const char *request_id;     /**< Pending request id (requestId), or NULL */
    /** Image support for the answer, or NULL when the runtime does not accept images */
    const copilot_user_input_image_attachment_constraints_t *image_attachment_constraints;
    const char *meta_json;      /**< The request's opaque _meta object as JSON, or NULL */
} copilot_user_input_request_t;

typedef struct {
    char *answer;               /**< Caller-allocated; SDK will copy */
    bool was_freeform;
    /** Images attached to the answer, or NULL (only valid when the request advertised
     *  image_attachment_constraints; sent only when set). Allocate the array with
     *  malloc()/calloc(); the SDK frees it and its strings. Image-only answers use
     *  answer = strdup("") and was_freeform = true. */
    copilot_user_input_image_attachment_t *attachments;
    size_t attachments_count;   /**< Number of entries in attachments */
} copilot_user_input_response_t;

/**
 * User input handler callback.
 */
typedef copilot_error_t (*copilot_user_input_handler_fn)(
    const copilot_user_input_request_t *request,
    const char *session_id,
    void *user_data,
    copilot_user_input_response_t *out_response
);

/* ============================================================================
 * Hook types
 * ============================================================================ */

/**
 * Generic hook handler. Receives the hook type, raw JSON input, and must
 * return a JSON string result (or NULL for no output). The returned string
 * must be heap-allocated; the SDK will free it.
 */
typedef char * (*copilot_hook_handler_fn)(
    const char *hook_type,
    const char *input_json,
    const char *session_id,
    void *user_data
);

/* ============================================================================
 * System message configuration
 * ============================================================================ */

/** Known system prompt section identifiers for the "customize" mode. */
#define COPILOT_SECTION_IDENTITY            "identity"
#define COPILOT_SECTION_TONE                "tone"
#define COPILOT_SECTION_TOOL_EFFICIENCY     "tool_efficiency"
#define COPILOT_SECTION_ENVIRONMENT_CONTEXT "environment_context"
#define COPILOT_SECTION_CODE_CHANGE_RULES   "code_change_rules"
#define COPILOT_SECTION_GUIDELINES          "guidelines"
#define COPILOT_SECTION_SAFETY              "safety"
#define COPILOT_SECTION_TOOL_INSTRUCTIONS   "tool_instructions"
#define COPILOT_SECTION_CUSTOM_INSTRUCTIONS "custom_instructions"
#define COPILOT_SECTION_LAST_INSTRUCTIONS   "last_instructions"

/** Override action for a system prompt section. */
#define COPILOT_OVERRIDE_REPLACE "replace"
#define COPILOT_OVERRIDE_REMOVE  "remove"
#define COPILOT_OVERRIDE_APPEND  "append"
#define COPILOT_OVERRIDE_PREPEND "prepend"

/** Override operation for a single system prompt section. */
typedef struct {
    const char *action;     /**< "replace", "remove", "append", or "prepend" */
    const char *content;    /**< Optional content for the override (NULL if none) */
} copilot_section_override_t;

/** A key-value pair mapping a section name to its override. */
typedef struct {
    const char *section;                    /**< Section identifier (use COPILOT_SECTION_* constants) */
    copilot_section_override_t override;    /**< The override to apply */
} copilot_section_override_entry_t;

typedef struct {
    const char *mode;       /**< "append" (default), "replace", or "customize" */
    const char *content;    /**< Additional/replacement content */
    const copilot_section_override_entry_t *section_overrides; /**< Array of section overrides (customize mode), or NULL */
    size_t section_overrides_count;                             /**< Number of section overrides */
} copilot_system_message_config_t;

/* ============================================================================
 * Provider configuration (BYOK)
 * ============================================================================ */

typedef struct {
    const char *type;          /**< "openai", "azure", or "anthropic" (default: "openai") */
    const char *wire_api;      /**< "completions" or "responses" (default: "completions") */
    const char *base_url;      /**< API endpoint URL (required) */
    const char *api_key;       /**< API key (optional) */
    const char *bearer_token;  /**< Bearer token (optional, overrides api_key) */
    const char *azure_api_version; /**< Azure API version (optional) */
    const char *model_provider;    /**< Product serving the model (modelProvider): one of COPILOT_MODEL_PROVIDER_*, or NULL */
    const char *api_key_command;   /**< Command whose stdout is the API key (apiKeyCommand), run before each request, or NULL */
} copilot_provider_config_t;

/** Products serving a BYOK model (copilot_provider_config_t.model_provider). */
#define COPILOT_MODEL_PROVIDER_OPENAI        "openai"
#define COPILOT_MODEL_PROVIDER_ANTHROPIC     "anthropic"
#define COPILOT_MODEL_PROVIDER_AZURE_OPENAI  "azure_openai"
#define COPILOT_MODEL_PROVIDER_OLLAMA        "ollama"
#define COPILOT_MODEL_PROVIDER_LM_STUDIO     "lm_studio"
#define COPILOT_MODEL_PROVIDER_FOUNDRY_LOCAL "foundry_local"
#define COPILOT_MODEL_PROVIDER_LLAMA_CPP     "llama_cpp"

/* ============================================================================
 * Infinite session configuration
 * ============================================================================ */

typedef struct {
    bool enabled;                             /**< Default: true */
    double background_compaction_threshold;    /**< Default: 0.80 */
    double buffer_exhaustion_threshold;        /**< Default: 0.95 */
    bool has_enabled;                         /**< Whether 'enabled' was explicitly set */
    bool has_background_threshold;
    bool has_buffer_threshold;
} copilot_infinite_session_config_t;

/* ============================================================================
 * Upstream-sync feature types (parity with @github/copilot-sdk)
 * ============================================================================ */

/** Hook type identifiers passed to copilot_hook_handler_fn (hook_type argument). */
#define COPILOT_HOOK_PRE_TOOL_USE          "preToolUse"
#define COPILOT_HOOK_POST_TOOL_USE         "postToolUse"
#define COPILOT_HOOK_USER_PROMPT_SUBMITTED "userPromptSubmitted"
#define COPILOT_HOOK_USER_PROMPT_TRANSFORMED "userPromptTransformed"
#define COPILOT_HOOK_SESSION_START         "sessionStart"
#define COPILOT_HOOK_SESSION_END           "sessionEnd"
#define COPILOT_HOOK_ERROR_OCCURRED        "errorOccurred"
#define COPILOT_HOOK_PRE_MCP_TOOL_CALL     "preMcpToolCall"
/** Fired before a sub-agent's first turn. input_json: {transcriptPath, agentName,
 *  agentDisplayName?, agentDescription?, timestamp, cwd}; output: {additionalContext?}. */
#define COPILOT_HOOK_SUBAGENT_START        "subagentStart"
/** Fired after a sub-agent completes a turn. input_json: subagentStart fields plus
 *  {agentId?, agentType, stopReason: "end_turn", response}; output:
 *  {decision?: "block"|"allow", reason?, modifiedResponse?}. */
#define COPILOT_HOOK_SUBAGENT_STOP         "subagentStop"

/** System-message section identifiers. "preamble" targets the identity preamble;
 *  "preserve" protects an individually-addressable section from a group-level remove. */
#define COPILOT_SECTION_PREAMBLE "preamble"
#define COPILOT_SECTION_PRESERVE "preserve"

/** Tool "defer" loading policy (a tool's `defer` field): eager pre-load ("never")
 *  or lazy load via search ("auto"). */
#define COPILOT_TOOL_DEFER_AUTO  "auto"
#define COPILOT_TOOL_DEFER_NEVER "never"

/** GitHub-anchored attachment type identifiers. */
#define COPILOT_GITHUB_COMMIT           "GitHubCommit"
#define COPILOT_GITHUB_RELEASE          "GitHubRelease"
#define COPILOT_GITHUB_ACTIONS_JOB      "GitHubActionsJob"
#define COPILOT_GITHUB_REPOSITORY       "GitHubRepository"
#define COPILOT_GITHUB_FILE_DIFF        "GitHubFileDiff"
#define COPILOT_GITHUB_TREE_COMPARISON  "GitHubTreeComparison"
#define COPILOT_GITHUB_URL              "GitHubUrl"
#define COPILOT_GITHUB_FILE             "GitHubFile"
#define COPILOT_GITHUB_SNIPPET          "GitHubSnippet"

/** Per-session AI-credit budget; set has_max_ai_credits + max_ai_credits to cap spend. */
typedef struct {
    double max_ai_credits;      /**< Maximum AI credits for the session */
    bool has_max_ai_credits;    /**< Whether max_ai_credits was explicitly set */
} copilot_session_limits_config_t;

/** Opt-in persistent session memory configuration. */
typedef struct {
    bool enabled;               /**< Whether persistent memory is enabled */
    bool has_enabled;           /**< Whether 'enabled' was explicitly set */
} copilot_memory_config_t;

/** Arguments passed to a bearer-token provider / MCP auth handler (per-session scoping). */
typedef struct {
    const char *session_id;     /**< The session the token is requested for */
} copilot_provider_token_args_t;

/**
 * Handler invoked when an MCP server requests an OAuth host token.
 * Returns a newly-allocated token string (caller frees), or NULL.
 */
typedef char * (*copilot_mcp_auth_handler_fn)(
    const copilot_provider_token_args_t *args,
    void *user_data
);

/**
 * BYOK bearer-token provider: returns a newly-allocated token string
 * (caller frees) for outbound model requests, or NULL.
 */
typedef char * (*copilot_bearer_token_provider_fn)(
    const copilot_provider_token_args_t *args,
    void *user_data
);

/**
 * Interceptor for outbound LLM inference HTTP/WebSocket requests. Receives the
 * request payload as a JSON string and returns a (possibly modified) JSON string
 * (caller frees), or NULL to forward the request unchanged.
 */
typedef char * (*copilot_request_handler_fn)(
    const char *request_json,
    void *user_data
);

/* ============================================================================
 * Attachment types
 * ============================================================================ */

typedef enum {
    COPILOT_ATTACHMENT_FILE = 0,
    COPILOT_ATTACHMENT_DIRECTORY,
    COPILOT_ATTACHMENT_SELECTION,
} copilot_attachment_type_t;

typedef struct {
    copilot_attachment_type_t type;
    const char *path;
    const char *display_name;  /**< Optional */
    /* For selection type: */
    const char *file_path;     /**< For selection: the file path */
    const char *text;          /**< For selection: the selected text */
    int start_line;
    int start_character;
    int end_line;
    int end_character;
} copilot_attachment_t;

/* ============================================================================
 * MCP Server Configuration
 * ============================================================================ */

/**
 * MCP server type.
 */
typedef enum {
    COPILOT_MCP_SERVER_STDIO = 0,   /**< Local/stdio server */
    COPILOT_MCP_SERVER_HTTP,        /**< Remote HTTP server */
    COPILOT_MCP_SERVER_SSE,         /**< Remote SSE server */
} copilot_mcp_server_type_t;

/**
 * MCP server configuration.
 *
 * For stdio servers: set command, args, args_count, and optionally env_* and cwd.
 * For HTTP/SSE servers: set url and optionally headers_*.
 * Both variants use tools, tools_count, and optionally timeout.
 */
typedef struct {
    copilot_mcp_server_type_t type;

    /* Base fields (all variants) */
    const char **tools;            /**< NULL-terminated array of tool names, or NULL */
    size_t tools_count;
    int timeout;                   /**< Tool call timeout in ms (0 = not set) */
    bool has_timeout;              /**< Whether timeout was explicitly set */

    /* Stdio variant fields */
    const char *command;           /**< Command to run (NULL for HTTP/SSE) */
    const char **args;             /**< NULL-terminated array of args, or NULL */
    size_t args_count;
    const char **env_keys;         /**< NULL-terminated array of env var keys, or NULL */
    const char **env_values;       /**< Parallel array of env var values, or NULL */
    const char *cwd;               /**< Working directory, or NULL */

    /* HTTP/SSE variant fields */
    const char *url;               /**< Server URL (NULL for stdio) */
    const char **headers_keys;     /**< NULL-terminated array of header keys, or NULL */
    const char **headers_values;   /**< Parallel array of header values, or NULL */
} copilot_mcp_server_config_t;

/* ============================================================================
 * Session filesystem types
 * ============================================================================ */

/**
 * Configuration for a custom session filesystem provider.
 */
typedef struct {
    const char *initial_cwd;        /**< Initial working directory */
    const char *session_state_path; /**< Path for session state storage */
    const char *conventions;        /**< Path conventions ("windows" or "posix") */
} copilot_session_fs_config_t;

/**
 * File metadata returned by session filesystem operations.
 */
typedef struct {
    const char *name;
    int64_t size;
    bool is_directory;
    bool is_file;
    const char *created_at;   /**< ISO 8601 timestamp or NULL */
    const char *modified_at;  /**< ISO 8601 timestamp or NULL */
} copilot_session_fs_file_info_t;

/**
 * Session filesystem provider — a struct of function pointers.
 * Implementors fill in each callback to provide file operations scoped to a session.
 */
typedef struct {
    copilot_error_t (*read_file)(const char *session_id, const char *path,
                                  char **out_content, void *user_data);
    copilot_error_t (*write_file)(const char *session_id, const char *path,
                                   const char *content, void *user_data);
    copilot_error_t (*append_file)(const char *session_id, const char *path,
                                    const char *content, void *user_data);
    copilot_error_t (*exists)(const char *session_id, const char *path,
                               bool *out_exists, void *user_data);
    copilot_error_t (*stat)(const char *session_id, const char *path,
                             copilot_session_fs_file_info_t *out_info, void *user_data);
    copilot_error_t (*mkdir)(const char *session_id, const char *path,
                              bool recursive, void *user_data);
    copilot_error_t (*readdir)(const char *session_id, const char *path,
                                char ***out_entries, size_t *out_count, void *user_data);
    copilot_error_t (*readdir_with_types)(const char *session_id, const char *path,
                                           copilot_session_fs_file_info_t **out_entries,
                                           size_t *out_count, void *user_data);
    copilot_error_t (*rm)(const char *session_id, const char *path,
                           bool recursive, void *user_data);
    copilot_error_t (*rename_file)(const char *session_id, const char *old_path,
                                    const char *new_path, void *user_data);
    void *user_data; /**< User context passed to all callbacks */
} copilot_session_fs_provider_t;

/* ============================================================================
 * Commands
 * ============================================================================ */

/** Context for a slash-command invocation. */
typedef struct {
    const char *session_id;     /**< Session ID where the command was invoked */
    const char *command;        /**< Full command text (e.g. "/deploy production") */
    const char *command_name;   /**< Command name without leading / */
    const char *args;           /**< Raw argument string after the command name */
} copilot_command_context_t;

/** Handler invoked when a registered slash-command is executed. */
typedef void (*copilot_command_handler_fn)(
    const copilot_command_context_t *context,
    void *user_data
);

/** Definition of a slash command registered with the session. */
typedef struct {
    const char *name;           /**< Command name (without leading /) */
    const char *description;    /**< Human-readable description (NULL for none) */
    copilot_command_handler_fn handler;  /**< Handler callback */
    void *user_data;            /**< User data passed to handler */
} copilot_command_definition_t;

/* ============================================================================
 * UI Elicitation
 * ============================================================================ */

/** Context for an elicitation request from the server. */
typedef struct {
    const char *session_id;         /**< Session that triggered the request */
    const char *message;            /**< Prompt message to show the user */
    const char *requested_schema_json;  /**< JSON schema for expected input, or NULL */
    const char *mode;               /**< Elicitation mode, or NULL */
    const char *elicitation_source;  /**< Source that initiated the request, or NULL */
    const char *url;                /**< Optional URL for context, or NULL */
} copilot_elicitation_context_t;

/** Result returned from an elicitation handler. */
typedef struct {
    const char *action;     /**< "accept", "decline", or "cancel" */
    const char *content_json;  /**< JSON string of form values (NULL when not accept) */
} copilot_elicitation_result_t;

/** Handler for elicitation requests from the server. */
typedef copilot_elicitation_result_t (*copilot_elicitation_handler_fn)(
    const copilot_elicitation_context_t *context,
    void *user_data
);

/* ============================================================================
 * Exit plan mode
 * ============================================================================ */

/** Request passed to the exit-plan-mode handler. */
typedef struct {
    const char *session_id;         /**< Session identifier */
    const char *summary;            /**< Summary of the proposed plan (may be NULL) */
    const char *plan_content;       /**< Full plan content (may be NULL) */
    const char **actions;           /**< NULL-terminated array of action labels, or NULL */
    size_t actions_count;           /**< Number of entries in actions */
    const char *recommended_action; /**< Recommended action label (may be NULL) */
} copilot_exit_plan_mode_request_t;

/** Response returned from the exit-plan-mode handler. */
typedef struct {
    bool approved;                  /**< Whether the plan is approved */
    const char *selected_action;    /**< Selected action label (may be NULL) */
    const char *feedback;           /**< Optional feedback for the assistant (may be NULL) */
} copilot_exit_plan_mode_response_t;

/**
 * Handler invoked when the assistant requests to exit plan mode.
 *
 * Populate *response and return COPILOT_OK. Returning any other value
 * causes the plan to be approved by default.
 */
typedef copilot_error_t (*copilot_exit_plan_mode_handler_fn)(
    const copilot_exit_plan_mode_request_t *request,
    void *user_data,
    copilot_exit_plan_mode_response_t *response
);

/* ============================================================================
 * Trace context
 * ============================================================================ */

/** W3C trace context propagated on outbound requests. */
typedef struct {
    const char *traceparent;  /**< W3C traceparent header value, or NULL */
    const char *tracestate;   /**< W3C tracestate header value, or NULL */
} copilot_trace_context_t;

/**
 * Provider invoked to obtain the current trace context for outbound requests.
 * Return a copilot_trace_context_t; NULL fields are omitted.
 */
typedef copilot_trace_context_t (*copilot_trace_context_provider_fn)(void *user_data);

/* ============================================================================
 * v2.6.0 upstream-sync feature types (parity with @github/copilot-sdk)
 * ============================================================================ */

/**
 * Identity of the integrating application, sent on the `connect` handshake as
 * clientInfo {editorName, editorVersion, extensionName, extensionVersion}.
 * NULL/empty fields are dropped; an all-empty identity skips `connect` entirely.
 * The SDK copies these strings in copilot_client_create().
 */
typedef struct {
    const char *application_name;     /**< editorName, or NULL */
    const char *application_version;  /**< editorVersion, or NULL */
    const char *integration_name;     /**< extensionName, or NULL */
    const char *integration_version;  /**< extensionVersion, or NULL */
} copilot_client_info_t;

/** Auto routing preferences for the `auto` model (session config auto_tier; experimental). */
#define COPILOT_AUTO_TIER_EFFICIENCY   "efficiency"
#define COPILOT_AUTO_TIER_BALANCE      "balance"
#define COPILOT_AUTO_TIER_INTELLIGENCE "intelligence"
#define COPILOT_AUTO_TIER_FAST         "fast"

/** Ask-user tool variants (session config ask_user_variant). */
#define COPILOT_ASK_USER_VARIANT_LEGACY      "legacy"
#define COPILOT_ASK_USER_VARIANT_ELICITATION "elicitation"

/** Working-directory trust scopes (session config trust_working_directory). */
#define COPILOT_TRUST_WORKING_DIRECTORY_SESSION "session"
#define COPILOT_TRUST_WORKING_DIRECTORY_USER    "user"

/** Message provenance values (copilot_message_options_t.source).
 *  Use copilot_message_source_agent() to build "agent-<id>". */
#define COPILOT_MESSAGE_SOURCE_USER         "user"
#define COPILOT_MESSAGE_SOURCE_SYSTEM       "system"
#define COPILOT_MESSAGE_SOURCE_AGENT_PREFIX "agent-"

/** Arguments passed to a GitHub token provider (gitHubToken.getToken). */
typedef struct {
    const char *host;        /**< Effective GitHub host the token is requested for */
    const char *session_id;  /**< Session receiving the token, or NULL before it has an ID */
    const char *reason;      /**< "initial" or "refresh" */
} copilot_github_token_provider_args_t;

/** Result kinds for a GitHub token provider. */
typedef enum {
    COPILOT_GITHUB_TOKEN_RESULT_TOKEN = 0,   /**< A token was acquired */
    COPILOT_GITHUB_TOKEN_RESULT_CANCELLED,   /**< The request was cancelled */
} copilot_github_token_result_kind_t;

/** Result filled by a GitHub token provider. */
typedef struct {
    copilot_github_token_result_kind_t kind;
    char *access_token;   /**< Heap-allocated token (kind TOKEN); the SDK frees it */
    char *token_type;     /**< Optional heap-allocated OAuth token type (NULL = bearer); the SDK frees it */
    int64_t expires_in;   /**< Remaining token lifetime in seconds (kind TOKEN) */
} copilot_github_token_result_t;

/**
 * GitHub token provider callback. Fill *out_result and return COPILOT_OK.
 * Called on the SDK's reader thread; do not call blocking SDK RPCs from it.
 */
typedef copilot_error_t (*copilot_github_token_provider_fn)(
    const copilot_github_token_provider_args_t *args,
    void *user_data,
    copilot_github_token_result_t *out_result
);

/** Sandbox configuration (sandboxConfig; experimental). */
typedef struct {
    bool enabled;                          /**< Whether sandboxing is enabled */
    const char *user_policy_json;          /**< User policy fragment (userPolicy) as JSON, or NULL */
    bool add_current_working_directory;    /**< addCurrentWorkingDirectory (default: true) */
    bool has_add_current_working_directory;
    bool sandbox_mcp_servers;              /**< sandboxMcpServers (default: true) */
    bool has_sandbox_mcp_servers;
    bool sandbox_lsp_servers;              /**< sandboxLspServers (default: true) */
    bool has_sandbox_lsp_servers;
    bool sandbox_extensions;               /**< sandboxExtensions (default: true) */
    bool has_sandbox_extensions;
    bool allow_bypass;                     /**< allowBypass: let the agent request an individual command
                                                outside the sandbox via the permission flow (default: false) */
    bool has_allow_bypass;
} copilot_sandbox_config_t;

/** Session-level image generation configuration (imageGeneration). */
typedef struct {
    bool enabled;
    bool has_enabled;
    bool watermark;
    bool has_watermark;
    bool save_to_disk;
    bool has_save_to_disk;
} copilot_image_generation_config_t;

/** Autopilot continuation options (autopilot). */
typedef struct {
    bool continuation_enabled;      /**< Enable runtime-owned automatic continuation */
    bool has_continuation_enabled;
    int max_continues;              /**< Max automatic continuations outside an objective (0 = none) */
    bool has_max_continues;
} copilot_autopilot_options_t;

/* ============================================================================
 * v3.0.0 feature types (SDK protocol 4 parity with @github/copilot-sdk 2.0)
 * ============================================================================ */

/** Client modes (copilot_client_options_t.mode). NULL means COPILOT_CLIENT_MODE_CODING_AGENT.
 *  "empty" requires available_tools on every create/resume and applies minimal defaults.
 *  The legacy value "copilot-cli" is accepted as an alias of "coding-agent". */
#define COPILOT_CLIENT_MODE_CODING_AGENT "coding-agent"
#define COPILOT_CLIENT_MODE_EMPTY        "empty"

/** Permission modes (session config permission_mode, copilot_session_set_permission_mode()). */
#define COPILOT_PERMISSION_MODE_MANUAL    "manual"
#define COPILOT_PERMISSION_MODE_ASSISTED  "assisted"
#define COPILOT_PERMISSION_MODE_ALLOW_ALL "allow-all"
/** Known refusal reason reported when managed policy blocks a permission mode change. */
#define COPILOT_PERMISSION_MODE_REASON_MANAGED_POLICY "managed_policy"

/** Result of copilot_session_set_permission_mode(). Free with copilot_permission_mode_result_free(). */
typedef struct {
    bool success;     /**< Whether the requested mode was applied */
    char *mode;       /**< Authoritative permission mode after the request, or NULL */
    char *reason;     /**< Why the mode was refused (for example "managed_policy"), or NULL */
} copilot_permission_mode_result_t;

/** Named BYOK provider connection (inference.providers[]). */
typedef struct {
    const char *name;               /**< Provider name referenced by models (required, must not contain '/') */
    const char *type;               /**< "openai", "azure", or "anthropic", or NULL */
    const char *base_url;           /**< API endpoint URL (required) */
    const char *api_key;            /**< API key, or NULL */
    const char *api_key_command;    /**< Command whose stdout is the API key (apiKeyCommand), or NULL */
    const char *bearer_token;       /**< Bearer token, or NULL */
    const char *wire_api;           /**< "completions" or "responses", or NULL */
    const char *transport;          /**< "http" or "websockets", or NULL */
    const char *azure_api_version;  /**< Azure API version (azure.apiVersion), or NULL */
    const char *model_provider;     /**< Product serving the models (modelProvider): COPILOT_MODEL_PROVIDER_*, or NULL */
    const char *headers_json;       /**< Extra HTTP headers as a JSON object of strings, or NULL */
} copilot_named_provider_config_t;

/** BYOK model definition (inference.models[]). Select it with model "<provider>/<id>". */
typedef struct {
    const char *provider;           /**< Name of the copilot_named_provider_config_t serving this model (required) */
    const char *id;                 /**< Provider-local model id (required) */
    const char *model_id;           /**< Well-known base model for capability lookup (modelId), or NULL */
    const char *wire_model;         /**< Model name sent to the provider API (wireModel), or NULL */
    const char *name;               /**< Display name, or NULL */
    int max_prompt_tokens;          /**< 0 = not set */
    int max_context_window_tokens;  /**< 0 = not set */
    int max_output_tokens;          /**< 0 = not set */
    const char *capabilities_json;  /**< Capability overrides as JSON, or NULL */
} copilot_provider_model_config_t;

/** Copilot API options (github.capi). */
typedef struct {
    bool enabled;                   /**< Enables Copilot API models alongside or instead of BYOK providers */
    bool has_enabled;               /**< Whether 'enabled' was explicitly set */
} copilot_capi_config_t;

/** Input kinds of a shell authentication request. */
#define COPILOT_SHELL_AUTH_INPUT_SECRET  "secret"
#define COPILOT_SHELL_AUTH_INPUT_TEXT    "text"
#define COPILOT_SHELL_AUTH_INPUT_CONFIRM "confirm"

/** A password or other answer a shell command run by the agent asks for (shellAuthentication.request).
 *  The answer goes only to the program that asked; it is not shown to the model. */
typedef struct {
    const char *session_id;
    const char *request_id;   /**< Identifies the request; a later cancellation carries the same ID */
    const char *program;      /**< Program that asked, such as "sudo", "ssh", or "git" */
    const char *prompt;       /**< Prompt text the program supplied */
    const char *input_kind;   /**< COPILOT_SHELL_AUTH_INPUT_* ("secret" input must be masked) */
    const char *command;      /**< Shell command that triggered the prompt */
    const char *cwd;          /**< The command's working directory */
} copilot_shell_authentication_request_t;

typedef enum {
    COPILOT_SHELL_AUTH_ACCEPT = 0,  /**< Answer with value (answer "confirm" requests with "yes") */
    COPILOT_SHELL_AUTH_CANCEL,      /**< Decline the prompt */
} copilot_shell_authentication_action_t;

typedef struct {
    copilot_shell_authentication_action_t action;
    char *value;              /**< Heap-allocated answer for ACCEPT; the SDK wipes and frees it */
} copilot_shell_authentication_response_t;

/**
 * Shell authentication handler. Fill *out_response and return COPILOT_OK; any
 * other return value cancels the prompt. Called on the SDK's reader thread.
 */
typedef copilot_error_t (*copilot_shell_authentication_handler_fn)(
    const copilot_shell_authentication_request_t *request,
    void *user_data,
    copilot_shell_authentication_response_t *out_response
);

/** A memory the agent asked to store, after validation, secret filtering and approval (memoryStore.store). */
typedef struct {
    const char *session_id;
    const char *subject;      /**< Short topic the memory is about */
    const char *fact;         /**< The fact to remember */
    const char *citations;    /**< Sources supporting the fact */
    const char *reason;       /**< Why the agent considers the fact worth remembering */
} copilot_stored_memory_t;

/**
 * Memory store callback. Persist the memory and return COPILOT_OK; any other
 * value fails the store request. Called on the SDK's reader thread.
 */
typedef copilot_error_t (*copilot_memory_store_handler_fn)(
    const copilot_stored_memory_t *memory,
    void *user_data
);

/** Traffic network callbacks serve (copilot_network_options_t.scope). */
#define COPILOT_NETWORK_SCOPE_CONNECTION "connection"
#define COPILOT_NETWORK_SCOPE_RUNTIME    "runtime"

/**
 * Resolves the proxies for a request target (network.resolveProxy). Return a
 * heap-allocated PAC-style list such as "PROXY proxy.example:8080; DIRECT" (the
 * SDK frees it), or NULL to leave the request to the runtime's own proxy resolution.
 */
typedef char * (*copilot_network_resolve_proxy_fn)(const char *url, void *user_data);

/**
 * Supplies a fresh base64 SPNEGO token for a Kerberos service principal
 * (network.authenticateNegotiate). Return a heap-allocated token (the SDK wipes
 * and frees it), or NULL when unavailable.
 */
typedef char * (*copilot_network_authenticate_negotiate_fn)(const char *spn, void *user_data);

/** A proxy's Basic authentication challenge (network.authenticateBasic). */
typedef struct {
    const char *proxy;        /**< Proxy that challenged, as "http://host:port" */
    const char *realm;        /**< Realm named by the proxy (untrusted), or NULL */
    bool previous_rejected;   /**< True when the proxy rejected the credentials sent last */
} copilot_network_basic_auth_request_t;

/** Basic credentials. Both fields are heap-allocated or NULL; the SDK wipes and frees them. */
typedef struct {
    char *username;
    char *password;
} copilot_network_basic_credentials_t;

/** Answers a proxy Basic challenge. Leave the credentials NULL to supply none. */
typedef copilot_error_t (*copilot_network_authenticate_basic_fn)(
    const copilot_network_basic_auth_request_t *request,
    void *user_data,
    copilot_network_basic_credentials_t *out_credentials
);

/**
 * Network callbacks registered with network.setProvider after the client connects.
 * Callbacks run on the SDK's reader thread. The SDK never logs tokens or passwords.
 */
typedef struct {
    const char *scope;                                        /**< COPILOT_NETWORK_SCOPE_*, or NULL (runtime default "connection") */
    copilot_network_resolve_proxy_fn resolve_proxy;           /**< Or NULL */
    copilot_network_authenticate_negotiate_fn authenticate_negotiate; /**< Or NULL */
    copilot_network_authenticate_basic_fn authenticate_basic; /**< Or NULL */
    void *user_data;                                          /**< Passed to every callback */
} copilot_network_options_t;

/**
 * Structured Policy Helper failure (PolicyHelperFailure). Reported in the JSON-RPC
 * error data of a failed session.create/session.resume and on managed-settings
 * snapshots. Helper-authored text is untrusted: render it as plain text and never
 * execute it. Free with copilot_policy_helper_failure_free().
 */
typedef struct {
    char *category;                 /**< Stable runtime classification (open set) */
    char *failure_mode;             /**< failureMode: "ignore" or "failClosed" */
    char *message;                  /**< Primary plain-text explanation */
    bool previous_policy_retained;  /**< An earlier helper-derived layer still contributes */
    char *reason;                   /**< Invocation reason, such as "sessionStart" */
    char *severity;                 /**< "warning" or "error", or NULL */
    char *authority;                /**< "device" or "server", or NULL */
    char *code;                     /**< Helper-defined identifier, or NULL */
    char *details_json;             /**< JSON array of {message, path?} findings, or NULL */
    char *diagnostic;               /**< Secondary troubleshooting text, or NULL */
    char *help_link_url;            /**< helpLink.url, or NULL */
    char *help_link_label;          /**< helpLink.label, or NULL */
    char *remediation;              /**< Plain-text recovery guidance (never execute), or NULL */
} copilot_policy_helper_failure_t;

/* ============================================================================
 * v3.1.0 feature types (parity with the Microsoft Copilot SDK 2.0.3-preview.0)
 * ============================================================================ */

/** Result of copilot_client_mcp_secrets_save(). Owns no heap memory. */
typedef struct {
    bool used_keychain;   /**< True when the OS keychain stored the credential, false for the file backend */
} copilot_mcp_secrets_save_result_t;

/** A user-scoped approval for one installed plugin extension (PermissionsExtensionApproval).
 *  Strings are owned by the containing result and released by its *_free() function. */
typedef struct {
    char *extension_name;       /**< Stable identity, for example "plugin:plugin-name:extension-name" */
    char *plugin_name;
    char **capabilities;        /**< NULL-terminated array of the exact privileged capabilities the user approved */
    size_t capabilities_count;  /**< Number of entries in capabilities */
    char *granted_at;           /**< RFC 3339 timestamp */
    char *installation_key;     /**< Opaque; pass it back unchanged to copilot_client_extension_approvals_revoke() */
    char *installation_label;   /**< Or NULL */
} copilot_permissions_extension_approval_t;

/** Result of copilot_client_extension_approvals_grant().
 *  Free with copilot_permissions_extension_approvals_grant_result_free(). */
typedef struct {
    bool granted;                                       /**< Whether the pending request was eligible and persisted */
    copilot_permissions_extension_approval_t *approval; /**< The stored approval, or NULL */
} copilot_permissions_extension_approvals_grant_result_t;

/** Result of copilot_client_extension_approvals_list().
 *  Free with copilot_permissions_extension_approvals_list_result_free(). */
typedef struct {
    copilot_permissions_extension_approval_t *approvals;
    size_t count;
} copilot_permissions_extension_approvals_list_result_t;

/** Result of copilot_client_extension_approvals_revoke(). Owns no heap memory. */
typedef struct {
    bool revoked;         /**< Whether a matching approval was revoked */
} copilot_permissions_extension_approvals_revoke_result_t;

/* ============================================================================
 * Client options
 * ============================================================================ */

typedef struct {
    const char *cli_path;      /**< Path to CLI executable (NULL = use "copilot") */
    const char *cwd;           /**< Working directory for CLI process (NULL = inherit) */
    const char *log_level;     /**< "none","error","warning","info","debug","all" (default: "info") */
    bool use_stdio;            /**< Use stdio transport (default: true) */
    bool auto_start;           /**< Auto-start on first use (default: true) */
    bool auto_restart;         /**< Auto-restart on crash (default: true) */
    const char *github_token;  /**< GitHub token (NULL = none) */
    bool use_logged_in_user;   /**< Use stored OAuth tokens (default: true) */
    const char **extra_args;   /**< NULL-terminated array of extra CLI args, or NULL */
    int session_idle_timeout_seconds; /**< Server-wide idle timeout for sessions in seconds (0 = not set) */
    const copilot_session_fs_config_t *session_fs; /**< Session filesystem config, or NULL */
    const char *copilot_home;  /**< Override the Copilot home directory (NULL = default) */
    const char *tcp_connection_token; /**< Token for TCP connection authentication (NULL = none) */
    copilot_request_handler_fn request_handler; /**< Interceptor for outbound model requests, or NULL */
    void *request_handler_user_data;            /**< User data passed to request_handler */
    copilot_bearer_token_provider_fn bearer_token_provider; /**< BYOK bearer-token provider, or NULL */
    void *bearer_token_user_data;               /**< User data passed to bearer_token_provider */
    const char **builtin_plugin_directories;    /**< Built-in plugin directories (builtinPluginDirectories), NULL-terminated, or NULL */
    bool in_process;                            /**< Use the in-process FFI transport (inProcess) instead of spawning a CLI */
    copilot_trace_context_provider_fn on_get_trace_context; /**< Trace-context provider for outbound requests, or NULL */
    void *trace_context_user_data;              /**< User data passed to on_get_trace_context */
    const copilot_client_info_t *client_info;   /**< Application identity for the connect handshake (clientInfo), or NULL */
    const char *mode;                           /**< Client mode: COPILOT_CLIENT_MODE_CODING_AGENT (default when NULL) or COPILOT_CLIENT_MODE_EMPTY */
    const copilot_network_options_t *network;   /**< Network callbacks (network.setProvider), or NULL. Copied by copilot_client_create(). */
} copilot_client_options_t;

/**
 * Returns a copilot_client_options_t initialized with default values.
 * Callers should modify the returned struct before passing to copilot_client_create().
 */
copilot_client_options_t copilot_client_options_default(void);

/* ============================================================================
 * Session configuration
 * ============================================================================ */

typedef struct {
    const char *session_id;             /**< Custom session ID (NULL = server-generated) */
    const char *model;                  /**< Model identifier (NULL = default) */
    const char *reasoning_effort;       /**< "low","medium","high","xhigh" or NULL */
    const char *config_dir;             /**< Override config directory (NULL = default) */
    const char *working_directory;      /**< Working directory for tool ops (NULL = default) */
    bool streaming;                     /**< Enable streaming deltas */

    /* Tools */
    const copilot_tool_t *tools;        /**< Array of tool definitions */
    size_t tools_count;

    /* Available/excluded tools */
    const char **available_tools;       /**< NULL-terminated list, or NULL */
    const char **excluded_tools;        /**< NULL-terminated list, or NULL */

    /* System message */
    const copilot_system_message_config_t *system_message;  /**< NULL = default */

    /* Provider (BYOK) */
    const copilot_provider_config_t *provider;  /**< NULL = use Copilot API */

    /* Infinite sessions */
    const copilot_infinite_session_config_t *infinite_sessions;  /**< NULL = default */

    /* Model capabilities overrides */
    const char *model_capabilities_json; /**< Per-property overrides for model capabilities as JSON string, or NULL */

    /* Config discovery */
    bool enable_config_discovery;  /**< When true, auto-discovers MCP server configs from working directory */
    bool has_enable_config_discovery; /**< Whether enable_config_discovery was explicitly set */

    /* Sub-agent streaming */
    bool include_sub_agent_streaming_events; /**< Include sub-agent streaming events in the event stream (default: true) */
    bool has_include_sub_agent_streaming_events; /**< Whether include_sub_agent_streaming_events was explicitly set */

    /* Skills for custom agents */
    const char **skills;           /**< NULL-terminated array of skill names to preload, or NULL */

    /* GitHub token */
    const char *github_token;      /**< GitHub token for authentication. Overrides client-level token for this session only. (NULL = none) */

    /* Instruction directories */
    const char **instruction_directories;  /**< NULL-terminated array of instruction directory paths, or NULL */
    int instruction_directories_count;     /**< Number of entries in instruction_directories (0 if NULL) */

    /* Handlers */
    copilot_permission_handler_fn on_permission_request;
    void *permission_user_data;

    copilot_user_input_handler_fn on_user_input_request;
    void *user_input_user_data;

    copilot_hook_handler_fn on_hook;
    void *hook_user_data;

    /* Commands */
    const copilot_command_definition_t *commands;  /**< Array of command definitions */
    size_t commands_count;

    /* Elicitation */
    copilot_elicitation_handler_fn on_elicitation_request;
    void *elicitation_user_data;

    /* --- Upstream-sync session options (parity with @github/copilot-sdk) --- */
    bool enable_citations;                  /**< Enable inline source citations in responses */
    bool has_enable_citations;              /**< Whether enable_citations was explicitly set */
    const char **excluded_builtin_agents;   /**< NULL-terminated list of built-in agents to exclude, or NULL */
    const copilot_session_limits_config_t *session_limits; /**< Per-session spending limits, or NULL */
    const copilot_memory_config_t *memory;  /**< Persistent session memory config, or NULL */
    const char *otlp_protocol;              /**< OTLP telemetry protocol ("grpc" or "http/protobuf"), or NULL */
    bool enable_web_socket_responses;       /**< Enable the WebSocket transport for streamed responses */
    bool has_enable_web_socket_responses;   /**< Whether enable_web_socket_responses was explicitly set */
    const char *exp_assignments_json;       /**< Experiment (feature-flag) assignment overrides as JSON, or NULL */
    copilot_mcp_auth_handler_fn on_mcp_auth_request; /**< MCP OAuth host-token handler, or NULL */
    void *mcp_auth_user_data;               /**< User data passed to on_mcp_auth_request */

    /* --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) --- */
    bool rewind_enabled;                    /**< Enable session rewind (rewindEnabled) */
    bool has_rewind_enabled;                /**< Whether rewind_enabled was explicitly set */
    const char **additional_directories;    /**< Extra workspace directories (additionalDirectories), NULL-terminated, or NULL */
    const char **disabled_mcp_servers;      /**< MCP servers to disable (disabledMcpServers), NULL-terminated, or NULL */
    const char *github_mcp_tool_config_json;/**< GitHub MCP tool config (githubMcpToolConfig) as JSON, or NULL */
    const char *canvas_provider_json;       /**< Canvas provider config (canvasProvider) as JSON, or NULL */
    bool custom_agents_local_only;          /**< Restrict custom agents to local only (customAgentsLocalOnly) */
    bool has_custom_agents_local_only;      /**< Whether custom_agents_local_only was explicitly set */
    const char *tool_search_json;           /**< Tool-search configuration (toolSearch) as JSON, or NULL */
    bool experimental_mode;                 /**< Enable experimental mode (experimentalMode) */
    bool has_experimental_mode;             /**< Whether experimental_mode was explicitly set */
    bool content_exclusion;                 /**< Enable content-exclusion enforcement (contentExclusion) */
    bool has_content_exclusion;             /**< Whether content_exclusion was explicitly set */
    const char *args_schema_json;           /**< Agent-factory authoring args schema (argsSchema) as JSON, or NULL */
    copilot_exit_plan_mode_handler_fn on_exit_plan_mode; /**< Handler for exit-plan-mode requests, or NULL */
    void *exit_plan_mode_user_data;         /**< User data passed to on_exit_plan_mode */

    /* --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) --- */
    const char *name;                       /**< Session name (name), or NULL. session.create only. */
    const char *auto_tier;                  /**< Auto routing preference (capi.autoTier): COPILOT_AUTO_TIER_*, or NULL. Experimental. */
    const char *ask_user_variant;           /**< Ask-user variant (askUserVariant): "legacy" or "elicitation", or NULL */
    copilot_github_token_provider_fn github_token_provider; /**< GitHub token provider (gitHubToken.getToken), or NULL. Mutually exclusive with github_token. */
    void *github_token_provider_user_data;  /**< User data passed to github_token_provider */
    const copilot_sandbox_config_t *sandbox_config; /**< Sandbox configuration (sandboxConfig), or NULL */
    const char *feature_flags_json;         /**< Feature-flag overrides (featureFlags) as a JSON object of booleans, or NULL. Sent whenever set, even if "{}". */
    const char **allowed_models;            /**< Model allowlist (allowedModels), NULL-terminated, or NULL */
    const char *auth_client_id_metadata_url; /**< OAuth client ID metadata document URL (authClientIdMetadataUrl), or NULL */
    const char **agent_directories;         /**< Custom agent directories (agentDirectories), NULL-terminated, or NULL */
    const char **hook_directories;          /**< Hook directories (hookDirectories), NULL-terminated, or NULL */
    bool interactive_user;                  /**< Whether an interactive user is present (interactiveUser) */
    bool has_interactive_user;              /**< Whether interactive_user was explicitly set */
    const char **excluded_managed_settings; /**< Managed settings to exclude (excludedManagedSettings), NULL-terminated, or NULL */
    bool enforce_managed_model_defaults;    /**< Enforce managed model defaults (enforceManagedModelDefaults) */
    bool has_enforce_managed_model_defaults; /**< Whether enforce_managed_model_defaults was explicitly set */
    const char *trust_working_directory;    /**< Working-directory trust (trustWorkingDirectory): "session" or "user", or NULL. session.create only. */
    bool refresh_custom_instructions;       /**< Re-read custom instructions (refreshCustomInstructions). session.create only. */
    bool has_refresh_custom_instructions;   /**< Whether refresh_custom_instructions was explicitly set */
    bool allow_transcript_recovery;         /**< Allow transcript recovery (allowTranscriptRecovery). session.resume only. */
    bool has_allow_transcript_recovery;     /**< Whether allow_transcript_recovery was explicitly set */
    const copilot_image_generation_config_t *image_generation; /**< Image generation config (imageGeneration), or NULL */
    const copilot_autopilot_options_t *autopilot; /**< Autopilot options (autopilot), or NULL */
    int max_output_tokens;                  /**< Model capabilities override modelCapabilities.limits.max_output_tokens */
    bool has_max_output_tokens;             /**< Whether max_output_tokens was explicitly set */

    /* --- v3.0.0 session options (SDK protocol 4 parity with @github/copilot-sdk 2.0) --- */
    bool suppress_resume_event;             /**< Skip the session.resume event and its side effects (suppressResumeEvent). session.resume only. */
    bool has_suppress_resume_event;         /**< Whether suppress_resume_event was explicitly set */
    const copilot_named_provider_config_t *providers; /**< Named BYOK provider connections (inference.providers), or NULL */
    size_t providers_count;                 /**< Number of entries in providers */
    const copilot_provider_model_config_t *models;    /**< BYOK model definitions (inference.models), or NULL */
    size_t models_count;                    /**< Number of entries in models */
    const char *default_provider;           /**< Provider for unqualified model IDs (inference.defaultProvider), or NULL */
    bool clear_default_provider;            /**< Send defaultProvider: null to clear it (session.resume only; ignored when default_provider is set) */
    const copilot_capi_config_t *capi;      /**< Copilot API options (github.capi.enabled), or NULL */
    const char *permission_mode;            /**< Initial permission mode (permissions.mode): COPILOT_PERMISSION_MODE_*, or NULL */
    copilot_shell_authentication_handler_fn on_shell_authentication_request; /**< Shell authentication handler, or NULL */
    void *shell_authentication_user_data;   /**< User data passed to on_shell_authentication_request */
    copilot_memory_store_handler_fn on_memory_store; /**< Memory store callback (memoryStore.store), or NULL */
    void *memory_store_user_data;           /**< User data passed to on_memory_store */
} copilot_session_config_t;

/**
 * Returns a copilot_session_config_t initialized with default/zero values.
 */
copilot_session_config_t copilot_session_config_default(void);

/* ============================================================================
 * Image generation types
 * ============================================================================ */

/** Response format for message responses. */
typedef enum {
    COPILOT_RESPONSE_FORMAT_TEXT = 0,
    COPILOT_RESPONSE_FORMAT_IMAGE = 1,
    COPILOT_RESPONSE_FORMAT_JSON_OBJECT = 2
} copilot_response_format_t;

/** Options for image generation. */
typedef struct {
    const char *size;       /**< e.g. "1024x1024" */
    const char *quality;    /**< "hd" or "standard" */
    const char *style;      /**< "natural" or "vivid" */
} copilot_image_options_t;

/** Image data from an assistant image response. */
typedef struct {
    const char *format;          /**< "png", "jpeg", "webp" */
    const char *base64;          /**< Base64-encoded image bytes */
    const char *url;             /**< Optional temporary URL (may be NULL) */
    const char *revised_prompt;  /**< The prompt the model actually used (may be NULL) */
    int width;
    int height;
} copilot_assistant_image_data_t;

/** A content block in a mixed text+image response. */
typedef struct {
    const char *type;                        /**< "text" or "image" */
    const char *text;                        /**< Text content (when type is "text", else NULL) */
    copilot_assistant_image_data_t *image;   /**< Image data (when type is "image", else NULL) */
} copilot_content_block_t;

/* ============================================================================
 * Message options
 * ============================================================================ */

typedef struct {
    const char *prompt;                     /**< The message text (required) */
    const copilot_attachment_t *attachments; /**< Array of attachments, or NULL */
    size_t attachments_count;
    const char *mode;                       /**< "enqueue" (default) or "immediate" */
    const char *agent_mode;                 /**< Agent mode to run this turn under, or NULL */
    const char *display_prompt;             /**< Alternate prompt text to show in the transcript, or NULL */
    copilot_response_format_t response_format;        /**< Response format (default: TEXT) */
    const copilot_image_options_t *image_options;      /**< Image generation options, or NULL */
    const char **request_headers_keys;   /**< Keys for custom HTTP headers (NULL-terminated), or NULL */
    const char **request_headers_values; /**< Values for custom HTTP headers (parallel to keys), or NULL */
    const char *source;                  /**< Message provenance (source): COPILOT_MESSAGE_SOURCE_* or "agent-<id>", or NULL */
    const char *response_schema_json;    /**< JSON Schema for structured output (responseSchema), or NULL. Supersedes response_format. */
    const char *client_turn_id;          /**< Opaque client-generated turn ID echoed on the turn's events (clientTurnId), or NULL */
} copilot_message_options_t;

/* ============================================================================
 * Model info
 * ============================================================================ */

typedef struct {
    char *id;
    char *name;
    bool supports_vision;
    bool supports_reasoning_effort;
    int max_context_window_tokens;
    int max_output_tokens;      /**< capabilities.limits.max_output_tokens (0 if not reported) */
} copilot_model_info_t;

typedef struct {
    copilot_model_info_t *models;
    size_t count;
} copilot_model_list_t;

/**
 * Frees a model list returned by copilot_client_list_models().
 */
void copilot_model_list_free(copilot_model_list_t *list);

/* ============================================================================
 * Ping response
 * ============================================================================ */

typedef struct {
    char *message;
    int64_t timestamp;         /**< Server time in Unix epoch milliseconds (0 if not reported) */
    int protocol_version;      /**< -1 if not present */
} copilot_ping_response_t;

/**
 * Frees a ping response.
 */
void copilot_ping_response_free(copilot_ping_response_t *response);

/* ============================================================================
 * Session metadata
 * ============================================================================ */

typedef struct {
    char *session_id;
    char *start_time;
    char *modified_time;
    char *summary;       /**< May be NULL */
    bool is_remote;
} copilot_session_metadata_t;

typedef struct {
    copilot_session_metadata_t *sessions;
    size_t count;
} copilot_session_list_t;

void copilot_session_list_free(copilot_session_list_t *list);

/* ============================================================================
 * Client API
 * ============================================================================ */

/**
 * Creates a new Copilot client.
 *
 * @param options  Client configuration. NULL for defaults.
 * @return New client handle, or NULL on allocation failure or an unknown options->mode.
 */
copilot_client_t *copilot_client_create(const copilot_client_options_t *options);

/**
 * Starts the CLI server and establishes a connection.
 * Spawns the CLI process (unless connecting to external server), sends the
 * `connect` handshake when client_info is set (ignored by older CLIs), and
 * verifies protocol compatibility via ping. The SDK speaks protocol 4 and
 * requires Copilot CLI 1.0.113 or newer. When network callbacks are configured,
 * they are registered with network.setProvider before this returns.
 *
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_start(copilot_client_t *client);

/**
 * Stops the client, destroys all sessions, and kills the CLI process.
 *
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_stop(copilot_client_t *client);

/**
 * Frees all memory associated with the client.
 * The client must be stopped first.
 */
void copilot_client_free(copilot_client_t *client);

/**
 * Returns the current connection state.
 */
copilot_connection_state_t copilot_client_get_state(const copilot_client_t *client);

/**
 * Pings the server.
 *
 * @param client   The client.
 * @param message  Optional ping message (NULL for none).
 * @param out      Output: ping response. Caller must free with copilot_ping_response_free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_ping(
    copilot_client_t *client,
    const char *message,
    copilot_ping_response_t *out
);

/**
 * Lists available models.
 *
 * @param client  The client.
 * @param out     Output: model list. Caller must free with copilot_model_list_free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_list_models(
    copilot_client_t *client,
    copilot_model_list_t *out
);

/**
 * Lists all sessions known to the server.
 *
 * @param client  The client.
 * @param out     Output: session list. Caller must free with copilot_session_list_free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_list_sessions(
    copilot_client_t *client,
    copilot_session_list_t *out
);

/**
 * Deletes a session from the server.
 *
 * @param client      The client.
 * @param session_id  The session ID to delete.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_delete_session(
    copilot_client_t *client,
    const char *session_id
);

/**
 * Gets the ID of the most recently updated session.
 *
 * @param client  The client.
 * @param out     Output: session ID string (may be NULL if no sessions). Caller must free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_get_last_session_id(
    copilot_client_t *client,
    char **out
);

/**
 * Gets metadata for a specific session.
 *
 * @param client      The client.
 * @param session_id  The session ID to get metadata for.
 * @param out_json    Output: JSON string with session metadata. Caller must free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_get_session_metadata(
    copilot_client_t *client,
    const char *session_id,
    char **out_json
);

/**
 * Gets the CLI status including version and protocol information.
 *
 * @param client    The client.
 * @param out_json  Output: JSON string with status information. Caller must free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_get_status(
    copilot_client_t *client,
    char **out_json
);

/**
 * Gets the current authentication status.
 *
 * @param client    The client.
 * @param out_json  Output: JSON string with auth status. Caller must free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_get_auth_status(
    copilot_client_t *client,
    char **out_json
);

/**
 * Gets the foreground session ID.
 *
 * @param client  The client.
 * @param out     Output: session ID string. Caller must free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_get_foreground_session_id(
    copilot_client_t *client,
    char **out
);

/**
 * Sets the foreground session ID.
 *
 * @param client      The client.
 * @param session_id  The session ID to set as foreground.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_set_foreground_session_id(
    copilot_client_t *client,
    const char *session_id
);

/**
 * Clears the runtime's managed-settings cache (managedSettings.clearCache).
 *
 * @param client  The client.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_clear_managed_settings_cache(copilot_client_t *client);

/*
 * MCP secret store and plugin extension approvals (server-scoped RPCs). They require
 * Copilot CLI 1.0.116 or newer; older CLIs answer with a JSON-RPC method-not-found
 * error, reported as COPILOT_ERROR_RPC like any other RPC error. Each call blocks until
 * the runtime answers, and SDK callbacks run on the reader thread, so call these
 * functions from your own thread rather than from inside an SDK callback.
 */

/**
 * Saves a legacy MCP configuration credential in the runtime-owned secret store
 * (mcp.secrets.save). The value is a credential: the SDK never logs it.
 *
 * @param client      The client.
 * @param secret_id   Identifier of the credential (secretId).
 * @param value       The credential.
 * @param config_dir  Configuration directory (configDir), or NULL for the runtime's default.
 * @param out         Output (may be NULL): whether the OS keychain stored the credential.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_mcp_secrets_save(
    copilot_client_t *client,
    const char *secret_id,
    const char *value,
    const char *config_dir,
    copilot_mcp_secrets_save_result_t *out
);

/**
 * Deletes a legacy MCP configuration credential from the runtime-owned secret store
 * (mcp.secrets.delete).
 *
 * @param client      The client.
 * @param secret_id   Identifier of the credential (secretId).
 * @param config_dir  Configuration directory (configDir), or NULL for the runtime's default.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_mcp_secrets_delete(
    copilot_client_t *client,
    const char *secret_id,
    const char *config_dir
);

/**
 * Deletes the legacy credentials indexed for an MCP server (mcp.secrets.deleteServer).
 *
 * @param client       The client.
 * @param server_name  The MCP server name (serverName).
 * @param config_dir   Configuration directory (configDir), or NULL for the runtime's default.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_mcp_secrets_delete_server(
    copilot_client_t *client,
    const char *server_name,
    const char *config_dir
);

/**
 * Persists a pending plugin extension permission request as a user-scoped approval,
 * before the host answers that request normally (permissions.extensionApprovals.grant).
 *
 * @param client      The client.
 * @param session_id  Session that owns the pending request (sessionId).
 * @param request_id  ID of the pending extension permission request (requestId).
 * @param out         Output (may be NULL): whether the request was eligible and persisted,
 *                    and the stored approval. Free with
 *                    copilot_permissions_extension_approvals_grant_result_free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_extension_approvals_grant(
    copilot_client_t *client,
    const char *session_id,
    const char *request_id,
    copilot_permissions_extension_approvals_grant_result_t *out
);

/**
 * Frees the approval owned by a grant result and zeroes it.
 */
void copilot_permissions_extension_approvals_grant_result_free(
    copilot_permissions_extension_approvals_grant_result_t *result);

/**
 * Lists the user-scoped plugin extension approvals (permissions.extensionApprovals.list).
 *
 * @param client      The client.
 * @param session_id  Attached session whose configuration profile to list (sessionId), or NULL.
 * @param out         Output: the approvals. Free with
 *                    copilot_permissions_extension_approvals_list_result_free().
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_extension_approvals_list(
    copilot_client_t *client,
    const char *session_id,
    copilot_permissions_extension_approvals_list_result_t *out
);

/**
 * Frees the approvals owned by a list result and zeroes it.
 */
void copilot_permissions_extension_approvals_list_result_free(
    copilot_permissions_extension_approvals_list_result_t *result);

/**
 * Revokes one user-scoped plugin extension approval (permissions.extensionApprovals.revoke).
 *
 * @param client            The client.
 * @param extension_name    The approval's extension_name (extensionName).
 * @param installation_key  The approval's installation_key, unchanged (installationKey).
 * @param session_id        Attached session whose configuration profile to use (sessionId), or NULL.
 * @param out               Output (may be NULL): whether a matching approval was revoked.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_extension_approvals_revoke(
    copilot_client_t *client,
    const char *extension_name,
    const char *installation_key,
    const char *session_id,
    copilot_permissions_extension_approvals_revoke_result_t *out
);

/**
 * Retrieves the structured Policy Helper failure (error.data.policyHelperFailure)
 * reported when the most recent copilot_client_create_session() or
 * copilot_client_resume_session() call on this client failed.
 *
 * @param client  The client.
 * @param out     Output: the failure. Free with copilot_policy_helper_failure_free().
 * @return true when the last failed create/resume reported a Policy Helper failure.
 */
bool copilot_client_get_last_policy_helper_failure(
    copilot_client_t *client,
    copilot_policy_helper_failure_t *out
);

/**
 * Parses a Policy Helper failure from JSON. Accepts the failure object itself or
 * any object carrying it under `policyHelperFailure`, such as JSON-RPC error data
 * or the managed-settings snapshot from copilot_session_get_managed_settings().
 *
 * @param json  JSON text.
 * @param out   Output: the failure. Free with copilot_policy_helper_failure_free().
 * @return true when a failure was found and parsed.
 */
bool copilot_policy_helper_failure_parse(const char *json, copilot_policy_helper_failure_t *out);

/**
 * Frees the strings owned by a Policy Helper failure and zeroes it.
 */
void copilot_policy_helper_failure_free(copilot_policy_helper_failure_t *failure);

/* ============================================================================
 * Session API
 * ============================================================================ */

/**
 * Creates a new conversation session.
 * If the client is not started and auto_start is true, starts the client first.
 *
 * @param client  The client.
 * @param config  Session configuration. NULL for defaults.
 * @param out     Output: session handle.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_create_session(
    copilot_client_t *client,
    const copilot_session_config_t *config,
    copilot_session_t **out
);

/**
 * Resumes an existing session by its ID.
 *
 * @param client      The client.
 * @param session_id  The session ID to resume.
 * @param config      Optional session configuration (NULL for defaults).
 * @param out         Output: session handle.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_client_resume_session(
    copilot_client_t *client,
    const char *session_id,
    const copilot_session_config_t *config,
    copilot_session_t **out
);

/**
 * Sets the session filesystem provider configuration.
 *
 * @param client              The client.
 * @param initial_cwd         Optional initial working directory (NULL to omit).
 * @param session_state_path  Optional session state path (NULL to omit).
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_set_session_fs_provider(
    copilot_client_t *client,
    const char *initial_cwd,
    const char *session_state_path
);

/**
 * Returns the session ID for the given session.
 */
const char *copilot_session_get_id(const copilot_session_t *session);

/**
 * Returns the workspace path (when infinite sessions are enabled), or NULL.
 */
const char *copilot_session_get_workspace_path(const copilot_session_t *session);

/**
 * Sends a message to the session. Returns immediately; responses arrive via events.
 *
 * @param session  The session.
 * @param options  Message options.
 * @param out_message_id  Output: message ID string (caller must free). NULL to ignore.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_session_send(
    copilot_session_t *session,
    const copilot_message_options_t *options,
    char **out_message_id
);

/**
 * Sends a message and blocks until the session becomes idle or timeout.
 *
 * @param session      The session.
 * @param options      Message options.
 * @param timeout_ms   Timeout in milliseconds (0 = 60000ms default).
 * @param out_content  Output: final assistant message content (caller must free), or NULL.
 * @return COPILOT_OK on success, COPILOT_ERROR_TIMEOUT on timeout,
 *         COPILOT_ERROR_SESSION_ERROR if a session error event occurs.
 */
copilot_error_t copilot_session_send_and_wait(
    copilot_session_t *session,
    const copilot_message_options_t *options,
    int timeout_ms,
    char **out_content
);

/**
 * Sends a message requesting structured output (responseSchema) and blocks until
 * the session becomes idle, then returns the final assistant content as JSON.
 *
 * @param session               The session.
 * @param options               Message options (response_schema_json is overridden).
 * @param response_schema_json  JSON Schema the response must conform to.
 * @param timeout_ms            Timeout in milliseconds (0 = 60000ms default).
 * @param out_json              Output: the parsed response re-serialized as JSON (caller must free).
 * @return COPILOT_OK on success, COPILOT_ERROR_SESSION_ERROR if the run produced no
 *         assistant content, COPILOT_ERROR_JSON_PARSE if the content is not valid JSON.
 */
copilot_error_t copilot_session_send_and_wait_structured(
    copilot_session_t *session,
    const copilot_message_options_t *options,
    const char *response_schema_json,
    int timeout_ms,
    char **out_json
);

/**
 * Builds the message source value "agent-<agent_id>" for copilot_message_options_t.source.
 *
 * @return Newly-allocated string (caller must free), or NULL on invalid input/allocation failure.
 */
char *copilot_message_source_agent(const char *agent_id);

/**
 * Registers an event handler for this session. Multiple handlers can be registered.
 *
 * @param session    The session.
 * @param handler    The callback function.
 * @param user_data  Context pointer passed to the handler.
 * @return A handler ID (>= 0) that can be used with copilot_session_off(), or -1 on error.
 */
int copilot_session_on(
    copilot_session_t *session,
    copilot_session_event_handler_fn handler,
    void *user_data
);

/**
 * Registers an event handler for a specific event type only.
 *
 * @param session     The session.
 * @param event_type  The event type string to filter on (e.g., "assistant.message").
 * @param handler     The callback function.
 * @param user_data   Context pointer passed to the handler.
 * @return A handler ID (>= 0) that can be used with copilot_session_off(), or -1 on error.
 */
int copilot_session_on_type(
    copilot_session_t *session,
    const char *event_type,
    copilot_session_event_handler_fn handler,
    void *user_data
);

/**
 * Unregisters an event handler.
 *
 * @param session     The session.
 * @param handler_id  The handler ID returned by copilot_session_on().
 */
void copilot_session_off(copilot_session_t *session, int handler_id);

/**
 * Destroys the session and releases server-side resources.
 * Sends session.detach (falling back to session.destroy on older CLIs); on-disk
 * session state is preserved so the session can be resumed later.
 * The session handle remains valid until copilot_session_free() is called.
 *
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_session_destroy(copilot_session_t *session);

/**
 * Frees all memory associated with a session handle.
 * Call copilot_session_destroy() first to release server-side resources.
 */
void copilot_session_free(copilot_session_t *session);

/**
 * Aborts the currently processing message in this session.
 *
 * @return COPILOT_OK on success, COPILOT_ERROR_RPC when the server rejects the request.
 */
copilot_error_t copilot_session_abort(copilot_session_t *session);

/**
 * Retrieves metadata for this session.
 *
 * @param session    The session.
 * @param out_json   Output: JSON string containing the metadata (caller must free), or NULL to ignore.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_session_get_metadata(
    copilot_session_t *session,
    char **out_json
);

/**
 * Changes the Auto routing preference without changing the selected model
 * (session.model.switchAutoTier). Experimental.
 *
 * @param session    The session.
 * @param auto_tier  COPILOT_AUTO_TIER_* value, or NULL to return to default Auto routing.
 * @param out_json   Output: JSON result (status, effectiveAutoTier, pendingAutoTier, ...)
 *                   (caller must free), or NULL to ignore.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_session_set_auto_tier(
    copilot_session_t *session,
    const char *auto_tier,
    char **out_json
);

/**
 * Retrieves the effective managed settings for this session (session.managedSettings.get).
 *
 * @param session   The session.
 * @param out_json  Output: JSON string (caller must free), or NULL to ignore.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_session_get_managed_settings(
    copilot_session_t *session,
    char **out_json
);

/**
 * Re-runs the managed-settings policy helper (session.managedSettings.recheckPolicyHelper).
 *
 * @param session   The session.
 * @param out_json  Output: JSON string (caller must free), or NULL to ignore.
 * @return COPILOT_OK on success.
 */
copilot_error_t copilot_session_recheck_policy_helper(
    copilot_session_t *session,
    char **out_json
);

/**
 * Requests a permission mode for this session (session.permissions.setMode).
 * When managed policy refuses the change, out->success is false and out->reason
 * is COPILOT_PERMISSION_MODE_REASON_MANAGED_POLICY.
 *
 * @param session  The session.
 * @param mode     COPILOT_PERMISSION_MODE_* value.
 * @param out      Output (may be NULL): the result. Free with copilot_permission_mode_result_free().
 * @return COPILOT_OK when the server answered (check out->success), or an error code.
 */
copilot_error_t copilot_session_set_permission_mode(
    copilot_session_t *session,
    const char *mode,
    copilot_permission_mode_result_t *out
);

/**
 * Frees the strings owned by a permission mode result and zeroes it.
 */
void copilot_permission_mode_result_free(copilot_permission_mode_result_t *result);

#ifdef __cplusplus
}
#endif

#endif /* COPILOT_COPILOT_H */

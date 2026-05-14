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
    copilot_tool_handler_fn handler;
    void *user_data;                 /**< Passed to handler on invocation */
} copilot_tool_t;

/* ============================================================================
 * Session event types
 * ============================================================================ */

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

typedef struct {
    const char *question;
    const char **choices;       /**< NULL-terminated array, or NULL if no choices */
    int choices_count;
    bool allow_freeform;
} copilot_user_input_request_t;

typedef struct {
    char *answer;               /**< Caller-allocated; SDK will copy */
    bool was_freeform;
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
} copilot_provider_config_t;

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
 * For stdio servers: set command, args, args_count, and optionally env_*/cwd.
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
 * Exit Plan Mode
 * ============================================================================ */

/** Request sent by the server when asking the user to approve exiting plan mode. */
typedef struct {
    const char *session_id;     /**< Session ID that sent the request */
} copilot_exit_plan_mode_request_t;

/** Response to an exit plan mode request. */
typedef struct {
    bool approved;              /**< Whether the user approved exiting plan mode */
} copilot_exit_plan_mode_response_t;

/** Handler for exit plan mode requests. */
typedef copilot_error_t (*copilot_exit_plan_mode_handler_fn)(
    const copilot_exit_plan_mode_request_t *request,
    void *user_data,
    copilot_exit_plan_mode_response_t *out_response
);

/* ============================================================================
 * Trace Context
 * ============================================================================ */

/** Trace context for distributed tracing (W3C Trace Context format). */
typedef struct {
    const char *traceparent;    /**< W3C traceparent header value, or NULL */
    const char *tracestate;     /**< W3C tracestate header value, or NULL */
} copilot_trace_context_t;

/** Callback that returns the current trace context. The returned strings must
 *  remain valid until the next call to this callback. */
typedef copilot_trace_context_t (*copilot_trace_context_provider_fn)(void *user_data);

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
    copilot_trace_context_provider_fn on_get_trace_context; /**< Trace context provider, or NULL */
    void *trace_context_user_data;  /**< User data for trace context provider */
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

    /* Exit plan mode */
    copilot_exit_plan_mode_handler_fn on_exit_plan_mode;
    void *exit_plan_mode_user_data;
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
    copilot_response_format_t response_format;        /**< Response format (default: TEXT) */
    const copilot_image_options_t *image_options;      /**< Image generation options, or NULL */
    const char **request_headers_keys;   /**< Keys for custom HTTP headers (NULL-terminated), or NULL */
    const char **request_headers_values; /**< Values for custom HTTP headers (parallel to keys), or NULL */
} copilot_message_options_t;

/* ============================================================================
 * Slash Commands
 * ============================================================================ */

/** Completion type for slash command inputs. */
typedef enum {
    COPILOT_SLASH_COMMAND_INPUT_COMPLETION_DIRECTORY = 0,
} copilot_slash_command_input_completion_t;

/** Kind of slash command. */
typedef enum {
    COPILOT_SLASH_COMMAND_KIND_BUILTIN = 0,
    COPILOT_SLASH_COMMAND_KIND_CLIENT,
    COPILOT_SLASH_COMMAND_KIND_SKILL,
} copilot_slash_command_kind_t;

/** Price category for model picker. */
typedef enum {
    COPILOT_MODEL_PICKER_PRICE_HIGH = 0,
    COPILOT_MODEL_PICKER_PRICE_LOW,
    COPILOT_MODEL_PICKER_PRICE_MEDIUM,
    COPILOT_MODEL_PICKER_PRICE_VERY_HIGH,
} copilot_model_picker_price_category_t;

/** Input definition for a slash command. */
typedef struct {
    const char *hint;                                   /**< Hint text for the input */
    copilot_slash_command_input_completion_t completion; /**< Completion type */
    bool has_completion;                                 /**< Whether completion is set */
} copilot_slash_command_input_t;

/** Information about a slash command. */
typedef struct {
    bool allow_during_agent_execution;                  /**< Allow during agent execution */
    const char *description;                            /**< Human-readable description */
    copilot_slash_command_kind_t kind;                   /**< Kind of command */
    const char *name;                                   /**< Command name */
    const char **aliases;                               /**< Array of aliases, or NULL */
    size_t aliases_count;                                /**< Number of aliases */
    bool experimental;                                  /**< Whether command is experimental */
    bool has_experimental;                              /**< Whether experimental is set */
    const copilot_slash_command_input_t *input;          /**< Input definition, or NULL */
} copilot_slash_command_info_t;

/** Request to invoke a command. */
typedef struct {
    const char *name;       /**< Command name (required) */
    const char *input;      /**< Command input, or NULL */
} copilot_commands_invoke_request_t;

/** Request to list available commands. */
typedef struct {
    bool include_builtins;          /**< Include built-in commands */
    bool has_include_builtins;      /**< Whether include_builtins is set */
    bool include_client_commands;   /**< Include client commands */
    bool has_include_client_commands; /**< Whether include_client_commands is set */
    bool include_skills;            /**< Include skill commands */
    bool has_include_skills;        /**< Whether include_skills is set */
} copilot_commands_list_request_t;

/** Token pricing information for model billing. */
typedef struct {
    int batch_size;         /**< Batch size, -1 if not set */
    int cache_price;        /**< Cache price, -1 if not set */
    int input_price;        /**< Input price, -1 if not set */
    int output_price;       /**< Output price, -1 if not set */
} copilot_model_billing_token_prices_t;

/* Experimental */
/** Diagnostics from loading skills. */
typedef struct {
    const char **errors;        /**< Array of error messages */
    size_t errors_count;        /**< Number of errors */
    const char **warnings;      /**< Array of warning messages */
    size_t warnings_count;      /**< Number of warnings */
} copilot_skills_load_diagnostics_t;

/* ============================================================================
 * Model info
 * ============================================================================ */

typedef struct {
    char *id;
    char *name;
    bool supports_vision;
    bool supports_reasoning_effort;
    int max_context_window_tokens;
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
    int64_t timestamp;
    int protocol_version;      /**< -1 if not present */
} copilot_ping_response_t;

/**
 * Frees a ping response.
 */
void copilot_ping_response_free(copilot_ping_response_t *response);

/* ============================================================================
 * Remote Session
 * ============================================================================ */

/** Mode for remote session control. */
typedef enum {
    COPILOT_REMOTE_SESSION_MODE_EXPORT = 0,
    COPILOT_REMOTE_SESSION_MODE_OFF,
    COPILOT_REMOTE_SESSION_MODE_ON,
} copilot_remote_session_mode_t;

/* Experimental */
/** Request to enable or configure a remote session. */
typedef struct {
    copilot_remote_session_mode_t mode;
    bool has_mode;              /**< Whether mode is set */
} copilot_remote_enable_request_t;

/* Experimental */
/** Result of enabling a remote session. */
typedef struct {
    bool remote_steerable;      /**< Whether the remote session is steerable */
    char *url;                  /**< Remote session URL, may be NULL */
} copilot_remote_enable_result_t;

/**
 * Frees a remote enable result.
 */
void copilot_remote_enable_result_free(copilot_remote_enable_result_t *result);

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
 * @return New client handle, or NULL on allocation failure.
 */
copilot_client_t *copilot_client_create(const copilot_client_options_t *options);

/**
 * Starts the CLI server and establishes a connection.
 * Spawns the CLI process (unless connecting to external server) and verifies
 * protocol compatibility via ping.
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
 * @return COPILOT_OK on success.
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

#ifdef __cplusplus
}
#endif

#endif /* COPILOT_COPILOT_H */

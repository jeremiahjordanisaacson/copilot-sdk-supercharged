#!/usr/bin/env bash
# Copilot CLI SDK Client - Main entry point for the Shell/Bash Copilot SDK.
#
# This module provides functions for managing the connection to the Copilot CLI
# server and session management capabilities.
#
# Requires: json_rpc.sh, sdk_protocol_version.sh

# --- Client State ---
COPILOT_CLIENT_STATE="disconnected"  # disconnected | connecting | connected | error
COPILOT_CLIENT_CLI_PATH=""
COPILOT_CLIENT_LOG_LEVEL="info"

# --- Client Config Variables ---
# Path to the Copilot home directory (string, optional)
COPILOT_HOME=""
# TCP connection token for authenticated server connections (string, optional)
COPILOT_TCP_CONNECTION_TOKEN=""
# Client info (ClientInfo): identity of the integrating application (strings, optional).
# When at least one field is set, copilot_client_start sends a `connect` request with
# {"clientInfo":{"editorName","editorVersion","extensionName","extensionVersion"}}
# before the ping handshake (older CLIs without `connect` are tolerated).
COPILOT_CLIENT_INFO_APPLICATION_NAME=""
COPILOT_CLIENT_INFO_APPLICATION_VERSION=""
COPILOT_CLIENT_INFO_INTEGRATION_NAME=""
COPILOT_CLIENT_INFO_INTEGRATION_VERSION=""

# --- Session Config Variables ---
# Set these before calling copilot_client_create_session to include in the payload.
# Server-wide idle timeout for sessions in seconds (integer, optional)
COPILOT_SESSION_IDLE_TIMEOUT_SECONDS=""
# Alias: idle_timeout (seconds) for session inactivity before auto-close
COPILOT_IDLE_TIMEOUT=""
# Elicitation handler callback function name (optional)
# When set, the client calls this function to handle elicitation requests from the server.
COPILOT_ELICITATION_HANDLER=""
# Custom HTTP headers for outbound model requests (JSON object string, optional)
# Example: '{"X-Custom":"value"}'
COPILOT_REQUEST_HEADERS=""
# List of skill names to preload (JSON array string, optional)
# Example: '["skill1","skill2"]'
COPILOT_AGENT_SKILLS=""
# Model capabilities overrides (JSON object string, optional)
COPILOT_MODEL_CAPABILITIES=""
# Auto-discover MCP server configs (boolean string: "true"/"false", optional, default: false)
COPILOT_ENABLE_CONFIG_DISCOVERY=""
# Include sub-agent streaming events (boolean string: "true"/"false", optional, default: true)
COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS=""
# MCP servers config (JSON object string, optional)
# Example: '{"myserver":{"tools":["*"],"command":"npx","args":["-y","my-mcp-server"]}}'
# For local/stdio servers, include: tools, command, args, and optionally type, timeout, env, cwd
# For remote HTTP/SSE servers, include: tools, type ("http"/"sse"), url, and optionally timeout, headers
COPILOT_MCP_SERVERS=""
# GitHub token for authentication (string, optional)
# When set, overrides the client-level token for this session only.
COPILOT_GITHUB_TOKEN=""

# Instruction directories (JSON array string of directory paths, optional)
# Example: '["./instructions","/etc/copilot/instructions"]'
COPILOT_INSTRUCTION_DIRECTORIES=""

# Excluded built-in tools (JSON array string, optional)
# Example: '["view","edit"]'
COPILOT_EXCLUDED_TOOLS=""
# Available/allowed tools (JSON array string, optional; takes precedence over excluded_tools)
# Example: '["grep","glob"]'
COPILOT_AVAILABLE_TOOLS=""
# Commands (JSON array string of {name, description} objects, optional)
# Example: '[{"name":"deploy","description":"Deploy to prod"}]'
COPILOT_COMMANDS=""

# --- Upstream-sync Session Config (parity with @github/copilot-sdk) ---
# Enable inline source citations in assistant responses (boolean string "true"/"false", optional)
COPILOT_ENABLE_CITATIONS=""
# Built-in agents to exclude from this session (JSON array string, optional)
# Example: '["copilot-code-review"]'
COPILOT_EXCLUDED_BUILTIN_AGENTS=""
# Per-session spending limits as a JSON object (SessionLimitsConfig, optional)
# Example: '{"maxAiCredits":100}'
COPILOT_SESSION_LIMITS=""
# Convenience: cap AI-credit spend; folded into sessionLimits as {"maxAiCredits":N}
COPILOT_MAX_AI_CREDITS=""
# Persistent session memory (MemoryConfiguration); enable with "true"/"false" (optional)
COPILOT_MEMORY_ENABLED=""
# OTLP telemetry export protocol, e.g. "grpc" or "http/protobuf" (string, optional)
COPILOT_OTLP_PROTOCOL=""
# Stream model responses over a WebSocket transport (boolean string "true"/"false", optional)
COPILOT_ENABLE_WEB_SOCKET_RESPONSES=""
# Experiment assignment overrides (JSON object string, optional)
# Example: '{"my-experiment":"treatment"}'
COPILOT_EXP_ASSIGNMENTS=""
# --- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk) ---
# Reasoning effort control (string: minimal|low|medium|high|max, optional)
COPILOT_REASONING_EFFORT=""
# Enable session rewind (boolean string "true"/"false", optional)
COPILOT_REWIND_ENABLED=""
# Additional session directories (JSON array string, optional)
COPILOT_ADDITIONAL_DIRECTORIES=""
# MCP server names to disable (JSON array string, optional)
COPILOT_DISABLED_MCP_SERVERS=""
# GitHub MCP tool configuration (JSON object string, optional)
COPILOT_GITHUB_MCP_TOOL_CONFIG=""
# Canvas provider config (JSON object string or provider name, optional)
COPILOT_CANVAS_PROVIDER=""
# Restrict custom agents to locally-defined agents only (boolean string, optional)
COPILOT_CUSTOM_AGENTS_LOCAL_ONLY=""
# Built-in plugin directories (JSON array string, optional)
COPILOT_BUILTIN_PLUGIN_DIRECTORIES=""
# Agent-factory authoring args schema (JSON object string, optional)
COPILOT_ARGS_SCHEMA=""
# Permission decision context (JSON object string, optional)
COPILOT_DECISION_CONTEXT=""
# Tool search configuration (JSON object string or boolean, optional)
COPILOT_TOOL_SEARCH=""
# Use the in-process FFI transport (boolean string "true"/"false", optional)
COPILOT_IN_PROCESS=""
# Enable experimental mode (boolean string "true"/"false", optional)
COPILOT_EXPERIMENTAL_MODE=""
# Enable content exclusion (boolean string "true"/"false", optional)
COPILOT_CONTENT_EXCLUSION=""
# User-prompt-transformed hook function name (optional).
# When set, session.create signals userPromptTransformed=true. Convention:
#   copilot_on_user_prompt_transformed <context_json>  → echoes JSON result
COPILOT_ON_USER_PROMPT_TRANSFORMED=""
# MCP OAuth host token handler function name (optional).
# When set, session.create signals mcpAuthHandler=true so the runtime routes
# MCP OAuth token requests to this handler. Convention:
#   copilot_on_mcp_auth_request <context_json>  → echoes JSON token result
COPILOT_ON_MCP_AUTH_REQUEST=""

# --- v2.6.0 upstream-sync Session Config (parity with @github/copilot-sdk) ---
# Unless noted, these apply to both copilot_client_create_session and
# copilot_client_resume_session. Unset (empty) values are not sent.
# Auto routing tier, experimental (string: efficiency|balance|intelligence|fast).
# Sent inside "capi" together with COPILOT_ENABLE_WEB_SOCKET_RESPONSES.
COPILOT_AUTO_TIER=""
# Ask-user tool variant (string: legacy|elicitation)
COPILOT_ASK_USER_VARIANT=""
# GitHub token provider function name (mutually exclusive with COPILOT_GITHUB_TOKEN).
# The runtime calls back with gitHubToken.getToken; the SDK invokes:
#   my_github_token_provider <args_json>  → echoes JSON result
#   args_json: {"host":"github.com","sessionId":"..."|null,"reason":"initial"|"refresh"}
#   result:    {"kind":"token","accessToken":"...","tokenType":"...","expiresIn":3600}
#              or {"kind":"cancelled"}
COPILOT_GITHUB_TOKEN_PROVIDER=""
# Sandbox configuration (JSON object string, e.g. from copilot_sandbox_config)
COPILOT_SANDBOX_CONFIG=""
# Runtime feature flags (JSON object string of booleans; sent whenever set, even '{}')
COPILOT_FEATURE_FLAGS=""
# Model allowlist (JSON array string)
COPILOT_ALLOWED_MODELS=""
# OAuth client ID metadata document URL (string)
COPILOT_AUTH_CLIENT_ID_METADATA_URL=""
# Session name (string; create only)
COPILOT_SESSION_NAME=""
# Custom agent directories / hook directories (JSON array strings)
COPILOT_AGENT_DIRECTORIES=""
COPILOT_HOOK_DIRECTORIES=""
# Whether a human user is present (boolean string "true"/"false")
COPILOT_INTERACTIVE_USER=""
# Managed settings to ignore (JSON array string)
COPILOT_EXCLUDED_MANAGED_SETTINGS=""
# Enforce managed model defaults (boolean string "true"/"false")
COPILOT_ENFORCE_MANAGED_MODEL_DEFAULTS=""
# Working-directory trust scope (string: session|user; create only)
COPILOT_TRUST_WORKING_DIRECTORY=""
# Re-read custom instructions (boolean string "true"/"false"; create only)
COPILOT_REFRESH_CUSTOM_INSTRUCTIONS=""
# Allow recovering a damaged transcript (boolean string "true"/"false"; resume only)
COPILOT_ALLOW_TRANSCRIPT_RECOVERY=""
# Image generation config (JSON object string, e.g. from copilot_image_generation_config)
COPILOT_IMAGE_GENERATION=""
# Autopilot options (JSON object string, e.g. from copilot_autopilot_options)
COPILOT_AUTOPILOT=""
# BYOK provider config (JSON object string, e.g. from copilot_provider_config)
COPILOT_PROVIDER=""

# --- GitHub Token Provider Registry (client-level) ---
# Newline-separated "registration_id|function_name|session_id" records used to answer
# gitHubToken.getToken requests. Cleared by copilot_client_stop.
COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY=""
# Registration ID generated by the last create/resume that used a token provider.
COPILOT_GITHUB_TOKEN_REGISTRATION_ID=""
_COPILOT_GITHUB_TOKEN_REGISTRATION_COUNTER=0

# --- Session Filesystem Config ---
# Session filesystem configuration (JSON object string, optional)
# Example: '{"initialCwd":"/home/user/project","sessionStatePath":"/tmp/state","conventions":"posix"}'
COPILOT_SESSION_FS=""
# Session filesystem provider RPC registration fields (optional)
# When COPILOT_SESSION_FS_INITIAL_CWD is set, sessionFs.setProvider is called after connect.
COPILOT_SESSION_FS_INITIAL_CWD=""
COPILOT_SESSION_FS_STATE_PATH=""
COPILOT_SESSION_FS_CONVENTIONS=""

# Session Filesystem Provider:
# To implement a session filesystem provider in Shell, define the following
# functions and set COPILOT_SESSION_FS_PROVIDER="custom":
#
#   copilot_session_fs_read_file       <session_id> <path>             → stdout: file content
#   copilot_session_fs_write_file      <session_id> <path> <content>   → exit 0
#   copilot_session_fs_append_file     <session_id> <path> <content>   → exit 0
#   copilot_session_fs_exists          <session_id> <path>             → exit 0 (exists) / exit 1 (not)
#   copilot_session_fs_stat            <session_id> <path>             → stdout: JSON SessionFsFileInfo
#   copilot_session_fs_mkdir           <session_id> <path> [recursive] → exit 0
#   copilot_session_fs_readdir         <session_id> <path>             → stdout: newline-separated names
#   copilot_session_fs_readdir_types   <session_id> <path>             → stdout: JSON array of FileInfo
#   copilot_session_fs_rm              <session_id> <path> [recursive] → exit 0
#   copilot_session_fs_rename          <session_id> <old> <new>        → exit 0
COPILOT_SESSION_FS_PROVIDER=""

# --- MCP Server Config Helpers ---

# Build a local/stdio MCP server config JSON.
#
# Arguments:
#   $1 - command (required)
#   $2 - args as JSON array string (default: "[]")
#   $3 - tools as JSON array string (default: '["*"]')
#
# Optional env vars to set before calling:
#   MCP_TYPE    - "local" or "stdio" (default: "local")
#   MCP_TIMEOUT - timeout in ms
#   MCP_ENV     - JSON object of env vars
#   MCP_CWD     - working directory
#
# Outputs the JSON object to stdout.
copilot_mcp_stdio_config() {
    local command="$1"
    local mcp_args="${2:-[]}"
    local mcp_tools="${3:-[\"*\"]}"

    local json
    json=$(jq -c -n \
        --arg cmd "$command" \
        --argjson args "$mcp_args" \
        --argjson tools "$mcp_tools" \
        '{"command":$cmd,"args":$args,"tools":$tools}')

    if [[ -n "${MCP_TYPE:-}" ]]; then
        json=$(echo "$json" | jq -c --arg t "$MCP_TYPE" '. + {"type":$t}')
    fi
    if [[ -n "${MCP_TIMEOUT:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson t "$MCP_TIMEOUT" '. + {"timeout":$t}')
    fi
    if [[ -n "${MCP_ENV:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson e "$MCP_ENV" '. + {"env":$e}')
    fi
    if [[ -n "${MCP_CWD:-}" ]]; then
        json=$(echo "$json" | jq -c --arg c "$MCP_CWD" '. + {"cwd":$c}')
    fi

    echo "$json"
}

# Build a remote HTTP/SSE MCP server config JSON.
#
# Arguments:
#   $1 - url (required)
#   $2 - type: "http" or "sse" (default: "http")
#   $3 - tools as JSON array string (default: '["*"]')
#
# Optional env vars to set before calling:
#   MCP_TIMEOUT - timeout in ms
#   MCP_HEADERS - JSON object of HTTP headers
#
# Outputs the JSON object to stdout.
copilot_mcp_http_config() {
    local url="$1"
    local mcp_type="${2:-http}"
    local mcp_tools="${3:-[\"*\"]}"

    local json
    json=$(jq -c -n \
        --arg url "$url" \
        --arg type "$mcp_type" \
        --argjson tools "$mcp_tools" \
        '{"url":$url,"type":$type,"tools":$tools}')

    if [[ -n "${MCP_TIMEOUT:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson t "$MCP_TIMEOUT" '. + {"timeout":$t}')
    fi
    if [[ -n "${MCP_HEADERS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson h "$MCP_HEADERS" '. + {"headers":$h}')
    fi

    echo "$json"
}

# --- Client Functions ---

# Start the Copilot CLI client.
#
# Spawns the CLI server process and verifies the protocol version.
#
# Arguments:
#   $1 - Path to the copilot CLI binary (default: "copilot" from PATH)
#   $2 - Log level (default: "info")
#
# Returns 0 on success, 1 on failure.
copilot_client_start() {
    local cli_path="${1:-copilot}"
    local log_level="${2:-info}"

    if [[ "$COPILOT_CLIENT_STATE" == "connected" ]]; then
        echo "Client is already connected." >&2
        return 0
    fi

    COPILOT_CLIENT_STATE="connecting"
    COPILOT_CLIENT_CLI_PATH="$cli_path"
    COPILOT_CLIENT_LOG_LEVEL="$log_level"

    # Start the JSON-RPC transport with the CLI process
    local extra_args=()
    if [[ "${COPILOT_REMOTE:-}" == "true" ]]; then
        extra_args+=("--remote")
    fi
    if ! copilot_jsonrpc_start "$cli_path" "--log-level" "$log_level" "${extra_args[@]}"; then
        COPILOT_CLIENT_STATE="error"
        echo "ERROR: Failed to start Copilot CLI process" >&2
        return 1
    fi

    # Declare the integrating application's identity (connect handshake) before ping
    if ! _copilot_client_send_connect; then
        COPILOT_CLIENT_STATE="error"
        copilot_jsonrpc_stop
        return 1
    fi

    # Verify protocol version
    if ! _copilot_client_verify_protocol_version; then
        COPILOT_CLIENT_STATE="error"
        copilot_jsonrpc_stop
        return 1
    fi

    # Register session filesystem provider if configured
    if [[ -n "${COPILOT_SESSION_FS_INITIAL_CWD:-}" ]]; then
        local fs_params
        fs_params=$(jq -c -n \
            --arg cwd "$COPILOT_SESSION_FS_INITIAL_CWD" \
            --arg state "${COPILOT_SESSION_FS_STATE_PATH:-}" \
            --arg conv "${COPILOT_SESSION_FS_CONVENTIONS:-}" \
            '{initialCwd: $cwd, sessionStatePath: $state, conventions: $conv}')
        if ! copilot_jsonrpc_request "sessionFs.setProvider" "$fs_params"; then
            echo "WARN: Failed to register sessionFs provider" >&2
        fi
    fi

    COPILOT_CLIENT_STATE="connected"
    return 0
}

# Stop the Copilot CLI client and clean up resources.
#
# Returns 0 on success.
copilot_client_stop() {
    copilot_jsonrpc_stop
    COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY=""
    COPILOT_CLIENT_STATE="disconnected"
    return 0
}

# Send a ping to the server and verify connectivity.
#
# Arguments:
#   $1 - Optional message to include in the ping
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full ping response
#
# Usage:
#   copilot_client_ping "hello"
#   protocol_version=$(copilot_jsonrpc_get_result_field '.protocolVersion')
#
# Returns 0 on success, 1 on failure.
copilot_client_ping() {
    local message="${1:-}"

    local params
    if [[ -n "$message" ]]; then
        params=$(jq -c -n --arg msg "$message" '{"message":$msg}')
    else
        params='{}'
    fi

    copilot_jsonrpc_request "ping" "$params"
}

# Create a new conversation session.
#
# Arguments:
#   $1 - Optional model name (string)
#   $2 - Optional system message (string)
#
# Sets:
#   COPILOT_SESSION_ID - The ID of the created session
#   COPILOT_JSONRPC_LAST_RESPONSE - The full create session response
#
# Returns 0 on success, 1 on failure.
copilot_client_create_session() {
    local model="${1:-}"
    local system_message="${2:-}"

    _copilot_client_ensure_connected || return 1

    local params='{}'
    if [[ -n "$model" ]] && [[ -n "$system_message" ]]; then
        params=$(jq -c -n \
            --arg model "$model" \
            --arg sysMsg "$system_message" \
            '{"model":$model,"systemMessage":$sysMsg}')
    elif [[ -n "$model" ]]; then
        params=$(jq -c -n --arg model "$model" '{"model":$model}')
    elif [[ -n "$system_message" ]]; then
        params=$(jq -c -n --arg sysMsg "$system_message" '{"systemMessage":$sysMsg}')
    fi

    # Append optional session config fields if set
    if [[ -n "$COPILOT_MODEL_CAPABILITIES" ]]; then
        params=$(echo "$params" | jq -c --argjson mc "$COPILOT_MODEL_CAPABILITIES" '. + {"modelCapabilities":$mc}')
    fi
    if [[ -n "$COPILOT_ENABLE_CONFIG_DISCOVERY" ]]; then
        if [[ "$COPILOT_ENABLE_CONFIG_DISCOVERY" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"enableConfigDiscovery":true}')
        else
            params=$(echo "$params" | jq -c '. + {"enableConfigDiscovery":false}')
        fi
    fi
    if [[ -n "$COPILOT_SESSION_FS" ]]; then
        params=$(echo "$params" | jq -c --argjson sf "$COPILOT_SESSION_FS" '. + {"sessionFs":$sf}')
    fi
    if [[ -n "$COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS" ]]; then
        if [[ "$COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"includeSubAgentStreamingEvents":true}')
        else
            params=$(echo "$params" | jq -c '. + {"includeSubAgentStreamingEvents":false}')
        fi
    fi
    if [[ -n "$COPILOT_GITHUB_TOKEN" ]]; then
        params=$(echo "$params" | jq -c --arg tok "$COPILOT_GITHUB_TOKEN" '. + {"gitHubToken":$tok}')
    fi
    if [[ -n "$COPILOT_EXCLUDED_TOOLS" ]]; then
        params=$(echo "$params" | jq -c --argjson et "$COPILOT_EXCLUDED_TOOLS" '. + {"excludedTools":$et}')
    fi
    if [[ -n "$COPILOT_AVAILABLE_TOOLS" ]]; then
        params=$(echo "$params" | jq -c --argjson at "$COPILOT_AVAILABLE_TOOLS" '. + {"availableTools":$at}')
    fi
    if [[ -n "$COPILOT_COMMANDS" ]]; then
        params=$(echo "$params" | jq -c --argjson cmds "$COPILOT_COMMANDS" '. + {"commands":$cmds}')
    fi
    if [[ -n "$COPILOT_INSTRUCTION_DIRECTORIES" ]]; then
        params=$(echo "$params" | jq -c --argjson id "$COPILOT_INSTRUCTION_DIRECTORIES" '. + {"instructionDirectories":$id}')
    fi
    if [[ "${COPILOT_ENABLE_SESSION_TELEMETRY:-}" == "true" ]]; then
        params=$(echo "$params" | jq -c '. + {"enableSessionTelemetry":true}')
    fi
    if [[ -n "$COPILOT_EXIT_PLAN_MODE_HANDLER" ]] && declare -f "$COPILOT_EXIT_PLAN_MODE_HANDLER" > /dev/null 2>&1; then
        params=$(echo "$params" | jq -c '. + {"requestExitPlanMode":true}')
    fi

    # --- Upstream-sync session config passthroughs (camelCase wire keys) ---
    if [[ -n "$COPILOT_ENABLE_CITATIONS" ]]; then
        if [[ "$COPILOT_ENABLE_CITATIONS" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"enableCitations":true}')
        else
            params=$(echo "$params" | jq -c '. + {"enableCitations":false}')
        fi
    fi
    if [[ -n "$COPILOT_EXCLUDED_BUILTIN_AGENTS" ]]; then
        params=$(echo "$params" | jq -c --argjson eba "$COPILOT_EXCLUDED_BUILTIN_AGENTS" '. + {"excludedBuiltinAgents":$eba}')
    fi
    if [[ -n "$COPILOT_SESSION_LIMITS" ]]; then
        params=$(echo "$params" | jq -c --argjson sl "$COPILOT_SESSION_LIMITS" '. + {"sessionLimits":$sl}')
    elif [[ -n "$COPILOT_MAX_AI_CREDITS" ]]; then
        params=$(echo "$params" | jq -c --argjson mac "$COPILOT_MAX_AI_CREDITS" '. + {"sessionLimits":{"maxAiCredits":$mac}}')
    fi
    if [[ -n "$COPILOT_MEMORY_ENABLED" ]]; then
        if [[ "$COPILOT_MEMORY_ENABLED" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"memory":{"enabled":true}}')
        else
            params=$(echo "$params" | jq -c '. + {"memory":{"enabled":false}}')
        fi
    fi
    if [[ -n "$COPILOT_OTLP_PROTOCOL" ]]; then
        params=$(echo "$params" | jq -c --arg op "$COPILOT_OTLP_PROTOCOL" '. + {"otlpProtocol":$op}')
    fi
    # COPILOT_ENABLE_WEB_SOCKET_RESPONSES is sent inside "capi" (see _copilot_client_add_common_session_options)
    if [[ -n "$COPILOT_EXP_ASSIGNMENTS" ]]; then
        params=$(echo "$params" | jq -c --argjson ea "$COPILOT_EXP_ASSIGNMENTS" '. + {"expAssignments":$ea}')
    fi
    # --- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk) ---
    if [[ -n "$COPILOT_REASONING_EFFORT" ]]; then
        params=$(echo "$params" | jq -c --arg re "$COPILOT_REASONING_EFFORT" '. + {"reasoningEffort":$re}')
    fi
    if [[ -n "$COPILOT_REWIND_ENABLED" ]]; then
        if [[ "$COPILOT_REWIND_ENABLED" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"rewindEnabled":true}')
        else
            params=$(echo "$params" | jq -c '. + {"rewindEnabled":false}')
        fi
    fi
    if [[ -n "$COPILOT_ADDITIONAL_DIRECTORIES" ]]; then
        params=$(echo "$params" | jq -c --argjson ad "$COPILOT_ADDITIONAL_DIRECTORIES" '. + {"additionalDirectories":$ad}')
    fi
    if [[ -n "$COPILOT_DISABLED_MCP_SERVERS" ]]; then
        params=$(echo "$params" | jq -c --argjson dms "$COPILOT_DISABLED_MCP_SERVERS" '. + {"disabledMcpServers":$dms}')
    fi
    if [[ -n "$COPILOT_GITHUB_MCP_TOOL_CONFIG" ]]; then
        params=$(echo "$params" | jq -c --argjson gmtc "$COPILOT_GITHUB_MCP_TOOL_CONFIG" '. + {"githubMcpToolConfig":$gmtc}')
    fi
    if [[ -n "$COPILOT_CANVAS_PROVIDER" ]]; then
        params=$(echo "$params" | jq -c --arg cp "$COPILOT_CANVAS_PROVIDER" '. + {"canvasProvider":$cp}')
    fi
    if [[ -n "$COPILOT_CUSTOM_AGENTS_LOCAL_ONLY" ]]; then
        if [[ "$COPILOT_CUSTOM_AGENTS_LOCAL_ONLY" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"customAgentsLocalOnly":true}')
        else
            params=$(echo "$params" | jq -c '. + {"customAgentsLocalOnly":false}')
        fi
    fi
    if [[ -n "$COPILOT_BUILTIN_PLUGIN_DIRECTORIES" ]]; then
        params=$(echo "$params" | jq -c --argjson bpd "$COPILOT_BUILTIN_PLUGIN_DIRECTORIES" '. + {"builtinPluginDirectories":$bpd}')
    fi
    if [[ -n "$COPILOT_ARGS_SCHEMA" ]]; then
        params=$(echo "$params" | jq -c --argjson as "$COPILOT_ARGS_SCHEMA" '. + {"argsSchema":$as}')
    fi
    if [[ -n "$COPILOT_DECISION_CONTEXT" ]]; then
        params=$(echo "$params" | jq -c --argjson dc "$COPILOT_DECISION_CONTEXT" '. + {"decisionContext":$dc}')
    fi
    if [[ -n "$COPILOT_TOOL_SEARCH" ]]; then
        params=$(echo "$params" | jq -c --argjson ts "$COPILOT_TOOL_SEARCH" '. + {"toolSearch":$ts}')
    fi
    if [[ -n "$COPILOT_IN_PROCESS" ]]; then
        if [[ "$COPILOT_IN_PROCESS" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"inProcess":true}')
        else
            params=$(echo "$params" | jq -c '. + {"inProcess":false}')
        fi
    fi
    if [[ -n "$COPILOT_EXPERIMENTAL_MODE" ]]; then
        if [[ "$COPILOT_EXPERIMENTAL_MODE" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"experimentalMode":true}')
        else
            params=$(echo "$params" | jq -c '. + {"experimentalMode":false}')
        fi
    fi
    if [[ -n "$COPILOT_CONTENT_EXCLUSION" ]]; then
        if [[ "$COPILOT_CONTENT_EXCLUSION" == "true" ]]; then
            params=$(echo "$params" | jq -c '. + {"contentExclusion":true}')
        else
            params=$(echo "$params" | jq -c '. + {"contentExclusion":false}')
        fi
    fi
    # User-prompt-transformed hook: signal to the runtime that a handler is registered.
    if [[ -n "$COPILOT_ON_USER_PROMPT_TRANSFORMED" ]]; then
        params=$(echo "$params" | jq -c '. + {"userPromptTransformed":true}')
    fi
    # MCP OAuth host token handler: signal to the runtime that a handler is registered.
    if [[ -n "$COPILOT_ON_MCP_AUTH_REQUEST" ]]; then
        params=$(echo "$params" | jq -c '. + {"mcpAuthHandler":true}')
    fi

    # --- v2.6.0 upstream-sync session options (create only) ---
    if [[ -n "${COPILOT_SESSION_NAME:-}" ]]; then
        params=$(echo "$params" | jq -c --arg sn "$COPILOT_SESSION_NAME" '. + {"name":$sn}')
    fi
    if [[ -n "${COPILOT_TRUST_WORKING_DIRECTORY:-}" ]]; then
        params=$(echo "$params" | jq -c --arg twd "$COPILOT_TRUST_WORKING_DIRECTORY" '. + {"trustWorkingDirectory":$twd}')
    fi
    if [[ -n "${COPILOT_REFRESH_CUSTOM_INSTRUCTIONS:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson rci "$(_copilot_json_bool "$COPILOT_REFRESH_CUSTOM_INSTRUCTIONS")" '. + {"refreshCustomInstructions":$rci}')
    fi
    # --- v2.6.0 options shared with resume ---
    params=$(_copilot_client_add_common_session_options "$params") || return 1

    # GitHub token provider: register it client-side and send its registration ID
    local gh_registration_id=""
    if [[ -n "${COPILOT_GITHUB_TOKEN_PROVIDER:-}" ]]; then
        if [[ -n "$COPILOT_GITHUB_TOKEN" ]]; then
            echo "ERROR: COPILOT_GITHUB_TOKEN and COPILOT_GITHUB_TOKEN_PROVIDER are mutually exclusive" >&2
            return 1
        fi
        _copilot_github_token_provider_register "$COPILOT_GITHUB_TOKEN_PROVIDER" ""
        gh_registration_id="$COPILOT_GITHUB_TOKEN_REGISTRATION_ID"
        params=$(echo "$params" | jq -c --arg rid "$gh_registration_id" '. + {"gitHubTokenProviderRegistrationId":$rid}')
    fi

    if ! copilot_jsonrpc_request "session.create" "$params"; then
        echo "ERROR: Failed to create session" >&2
        if [[ -n "$gh_registration_id" ]]; then
            _copilot_github_token_provider_unregister "$gh_registration_id"
        fi
        return 1
    fi

    COPILOT_SESSION_ID=$(copilot_jsonrpc_get_result_field '.sessionId')
    if [[ -z "$COPILOT_SESSION_ID" ]] || [[ "$COPILOT_SESSION_ID" == "null" ]]; then
        echo "ERROR: No sessionId in response" >&2
        if [[ -n "$gh_registration_id" ]]; then
            _copilot_github_token_provider_unregister "$gh_registration_id"
        fi
        return 1
    fi

    if [[ -n "$gh_registration_id" ]]; then
        _copilot_github_token_provider_commit "$gh_registration_id" "$COPILOT_SESSION_ID"
    fi

    return 0
}

# Resume an existing conversation session.
#
# Arguments:
#   $1 - Session ID to resume (required)
#   $2 - Optional model name (string)
#
# Sets:
#   COPILOT_SESSION_ID - The ID of the resumed session
#   COPILOT_JSONRPC_LAST_RESPONSE - The full resume session response
#
# Returns 0 on success, 1 on failure.
copilot_client_resume_session() {
    local session_id="$1"
    local model="${2:-}"

    _copilot_client_ensure_connected || return 1

    if [[ -z "$session_id" ]]; then
        echo "ERROR: session_id is required" >&2
        return 1
    fi

    local params
    if [[ -n "$model" ]]; then
        params=$(jq -c -n \
            --arg sid "$session_id" \
            --arg model "$model" \
            '{"sessionId":$sid,"model":$model}')
    else
        params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')
    fi

    # Append optional session config fields for resume
    if [[ -n "$COPILOT_INSTRUCTION_DIRECTORIES" ]]; then
        params=$(echo "$params" | jq -c --argjson id "$COPILOT_INSTRUCTION_DIRECTORIES" '. + {"instructionDirectories":$id}')
    fi
    if [[ "${COPILOT_ENABLE_SESSION_TELEMETRY:-}" == "true" ]]; then
        params=$(echo "$params" | jq -c '. + {"enableSessionTelemetry":true}')
    fi
    if [[ -n "$COPILOT_EXIT_PLAN_MODE_HANDLER" ]] && declare -f "$COPILOT_EXIT_PLAN_MODE_HANDLER" > /dev/null 2>&1; then
        params=$(echo "$params" | jq -c '. + {"requestExitPlanMode":true}')
    fi

    # --- v2.6.0 upstream-sync session options (resume only) ---
    if [[ -n "${COPILOT_ALLOW_TRANSCRIPT_RECOVERY:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson atr "$(_copilot_json_bool "$COPILOT_ALLOW_TRANSCRIPT_RECOVERY")" '. + {"allowTranscriptRecovery":$atr}')
    fi
    # --- v2.6.0 options shared with create ---
    params=$(_copilot_client_add_common_session_options "$params") || return 1

    # GitHub token provider: register it client-side and send its registration ID
    local gh_registration_id=""
    if [[ -n "${COPILOT_GITHUB_TOKEN_PROVIDER:-}" ]]; then
        if [[ -n "$COPILOT_GITHUB_TOKEN" ]]; then
            echo "ERROR: COPILOT_GITHUB_TOKEN and COPILOT_GITHUB_TOKEN_PROVIDER are mutually exclusive" >&2
            return 1
        fi
        _copilot_github_token_provider_register "$COPILOT_GITHUB_TOKEN_PROVIDER" "$session_id"
        gh_registration_id="$COPILOT_GITHUB_TOKEN_REGISTRATION_ID"
        params=$(echo "$params" | jq -c --arg rid "$gh_registration_id" '. + {"gitHubTokenProviderRegistrationId":$rid}')
    fi

    if ! copilot_jsonrpc_request "session.resume" "$params"; then
        echo "ERROR: Failed to resume session $session_id" >&2
        if [[ -n "$gh_registration_id" ]]; then
            _copilot_github_token_provider_unregister "$gh_registration_id"
        fi
        return 1
    fi

    COPILOT_SESSION_ID=$(copilot_jsonrpc_get_result_field '.sessionId')
    if [[ -z "$COPILOT_SESSION_ID" ]] || [[ "$COPILOT_SESSION_ID" == "null" ]]; then
        echo "ERROR: No sessionId in resume response" >&2
        if [[ -n "$gh_registration_id" ]]; then
            _copilot_github_token_provider_unregister "$gh_registration_id"
        fi
        return 1
    fi

    if [[ -n "$gh_registration_id" ]]; then
        _copilot_github_token_provider_commit "$gh_registration_id" "$COPILOT_SESSION_ID"
    fi

    return 0
}

# Delete a session permanently.
#
# Arguments:
#   $1 - Session ID to delete (required)
#
# Returns 0 on success, 1 on failure.
copilot_client_delete_session() {
    local session_id="$1"

    _copilot_client_ensure_connected || return 1

    if [[ -z "$session_id" ]]; then
        echo "ERROR: session_id is required" >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.delete" "$params"; then
        echo "ERROR: Failed to delete session $session_id" >&2
        return 1
    fi

    local success
    success=$(copilot_jsonrpc_get_result_field '.success')
    if [[ "$success" != "true" ]]; then
        local error_msg
        error_msg=$(copilot_jsonrpc_get_result_field '.error')
        echo "ERROR: Failed to delete session $session_id: $error_msg" >&2
        return 1
    fi

    return 0
}

# List all available models.
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full list models response
#
# Usage:
#   copilot_client_list_models
#   models=$(copilot_jsonrpc_get_result_field '.models')
#   echo "$models" | jq -r '.[].id'
#
# Returns 0 on success, 1 on failure.
copilot_client_list_models() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "models.list" "{}"; then
        echo "ERROR: Failed to list models" >&2
        return 1
    fi

    return 0
}

# Get the last session ID.
#
# Sets:
#   COPILOT_LAST_SESSION_ID - The last session ID (or empty)
#   COPILOT_JSONRPC_LAST_RESPONSE - The full response
#
# Returns 0 on success, 1 on failure.
copilot_client_get_last_session_id() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "session.getLastId" "{}"; then
        echo "ERROR: Failed to get last session ID" >&2
        return 1
    fi

    COPILOT_LAST_SESSION_ID=$(copilot_jsonrpc_get_result_field '.sessionId')
    if [[ "$COPILOT_LAST_SESSION_ID" == "null" ]]; then
        COPILOT_LAST_SESSION_ID=""
    fi

    return 0
}

# Get metadata for a session by ID.
#
# Arguments:
#   $1 - Session ID (required)
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full metadata response
#
# Returns 0 on success, 1 on failure.
copilot_client_get_session_metadata() {
    local session_id="$1"

    _copilot_client_ensure_connected || return 1

    if [[ -z "$session_id" ]]; then
        echo "ERROR: session_id is required" >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.getMetadata" "$params"; then
        echo "ERROR: Failed to get session metadata for $session_id" >&2
        return 1
    fi

    return 0
}

# Get CLI status including version and protocol information.
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full status response
#
# Returns 0 on success, 1 on failure.
copilot_client_get_status() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "status.get" "{}"; then
        echo "ERROR: Failed to get status" >&2
        return 1
    fi

    return 0
}

# Get current authentication status.
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full auth status response
#
# Returns 0 on success, 1 on failure.
copilot_client_get_auth_status() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "auth.getStatus" "{}"; then
        echo "ERROR: Failed to get auth status" >&2
        return 1
    fi

    return 0
}

# Clear the runtime's cached managed (enterprise) settings so they are re-read.
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full managedSettings.clearCache response
#
# Returns 0 on success, 1 on failure.
copilot_client_clear_managed_settings_cache() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "managedSettings.clearCache" "{}"; then
        echo "ERROR: Failed to clear managed settings cache" >&2
        return 1
    fi

    return 0
}

# List all available sessions.
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full list sessions response
#
# Usage:
#   copilot_client_list_sessions
#   sessions=$(copilot_jsonrpc_get_result_field '.sessions')
#   echo "$sessions" | jq -r '.[].sessionId'
#
# Returns 0 on success, 1 on failure.
copilot_client_list_sessions() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "session.list" "{}"; then
        echo "ERROR: Failed to list sessions" >&2
        return 1
    fi

    return 0
}

# Get the foreground session ID.
#
# Sets:
#   COPILOT_FOREGROUND_SESSION_ID - The foreground session ID (or empty)
#   COPILOT_JSONRPC_LAST_RESPONSE - The full response
#
# Returns 0 on success, 1 on failure.
copilot_client_get_foreground_session_id() {
    _copilot_client_ensure_connected || return 1

    if ! copilot_jsonrpc_request "session.getForeground" "{}"; then
        echo "ERROR: Failed to get foreground session" >&2
        return 1
    fi

    COPILOT_FOREGROUND_SESSION_ID=$(copilot_jsonrpc_get_result_field '.sessionId')
    if [[ "$COPILOT_FOREGROUND_SESSION_ID" == "null" ]]; then
        COPILOT_FOREGROUND_SESSION_ID=""
    fi

    return 0
}

# Set the foreground session ID.
#
# Arguments:
#   $1 - Session ID to set as foreground (required)
#
# Returns 0 on success, 1 on failure.
copilot_client_set_foreground_session_id() {
    local session_id="$1"

    _copilot_client_ensure_connected || return 1

    if [[ -z "$session_id" ]]; then
        echo "ERROR: session_id is required" >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.setForeground" "$params"; then
        echo "ERROR: Failed to set foreground session $session_id" >&2
        return 1
    fi

    local success
    success=$(copilot_jsonrpc_get_result_field '.success')
    if [[ "$success" != "true" ]]; then
        local error_msg
        error_msg=$(copilot_jsonrpc_get_result_field '.error')
        echo "ERROR: Failed to set foreground session $session_id: $error_msg" >&2
        return 1
    fi

    return 0
}

# --- Internal Functions ---

# Ensure the client is connected.
# Returns 0 if connected, 1 if not.
_copilot_client_ensure_connected() {
    if [[ "$COPILOT_CLIENT_STATE" != "connected" ]]; then
        echo "ERROR: Client not connected. Call copilot_client_start first." >&2
        return 1
    fi

    if ! copilot_jsonrpc_is_connected; then
        COPILOT_CLIENT_STATE="error"
        echo "ERROR: CLI process is no longer running" >&2
        return 1
    fi

    return 0
}

# Verify the protocol version matches between SDK and server.
# Returns 0 on match, 1 on mismatch.
_copilot_client_verify_protocol_version() {
    local expected_version
    expected_version=$(copilot_get_sdk_protocol_version)

    if ! copilot_client_ping; then
        echo "ERROR: Failed to ping server during protocol verification" >&2
        return 1
    fi

    local server_version
    server_version=$(copilot_jsonrpc_get_result_field '.protocolVersion')

    if [[ -z "$server_version" ]] || [[ "$server_version" == "null" ]]; then
        echo "ERROR: SDK protocol version mismatch: SDK expects version $expected_version, but server does not report a protocol version. Please update your server to ensure compatibility." >&2
        return 1
    fi

    if [[ "$server_version" != "$expected_version" ]]; then
        echo "ERROR: SDK protocol version mismatch: SDK expects version $expected_version, but server reports version $server_version. Please update your SDK or server to ensure compatibility." >&2
        return 1
    fi

    return 0
}

# Send the `connect` handshake declaring the integrating application (ClientInfo).
# Skipped when every COPILOT_CLIENT_INFO_* field is empty; a method-not-found error
# from older CLIs is ignored. Returns 0 on success/skip, 1 on other errors.
_copilot_client_send_connect() {
    local client_info
    client_info=$(jq -c -n \
        --arg aname "${COPILOT_CLIENT_INFO_APPLICATION_NAME:-}" \
        --arg aver "${COPILOT_CLIENT_INFO_APPLICATION_VERSION:-}" \
        --arg iname "${COPILOT_CLIENT_INFO_INTEGRATION_NAME:-}" \
        --arg iver "${COPILOT_CLIENT_INFO_INTEGRATION_VERSION:-}" \
        '{"editorName":$aname,"editorVersion":$aver,"extensionName":$iname,"extensionVersion":$iver} | with_entries(select(.value != ""))') || return 1

    if [[ "$client_info" == "{}" ]]; then
        return 0
    fi

    local params
    params=$(jq -c -n --argjson ci "$client_info" '{"clientInfo":$ci}') || return 1

    if copilot_jsonrpc_request "connect" "$params"; then
        return 0
    fi

    local error_code error_message
    error_code=$(echo "$COPILOT_JSONRPC_LAST_ERROR" | jq -r '.code // empty' 2>/dev/null) || error_code=""
    error_message=$(echo "$COPILOT_JSONRPC_LAST_ERROR" | jq -r '.message // empty' 2>/dev/null) || error_message=""
    if [[ "$error_code" == "-32601" ]] || [[ "$error_message" == *"Unhandled method connect"* ]]; then
        return 0
    fi

    echo "ERROR: connect handshake failed: ${error_message:-unknown error}" >&2
    return 1
}

# Append the v2.6.0 session options shared by session.create and session.resume.
#
# Arguments:
#   $1 - The params JSON object
#
# Outputs the updated params JSON object. Returns 1 if a value is not valid JSON.
_copilot_client_add_common_session_options() {
    local params="$1"

    # CAPI transport options: the runtime only accepts enableWebSocketResponses inside "capi".
    local capi='{}'
    if [[ -n "${COPILOT_AUTO_TIER:-}" ]]; then
        capi=$(echo "$capi" | jq -c --arg at "$COPILOT_AUTO_TIER" '. + {"autoTier":$at}') || return 1
    fi
    if [[ -n "${COPILOT_ENABLE_WEB_SOCKET_RESPONSES:-}" ]]; then
        capi=$(echo "$capi" | jq -c --argjson ws "$(_copilot_json_bool "$COPILOT_ENABLE_WEB_SOCKET_RESPONSES")" '. + {"enableWebSocketResponses":$ws}') || return 1
    fi
    if [[ "$capi" != "{}" ]]; then
        params=$(echo "$params" | jq -c --argjson capi "$capi" '. + {"capi":$capi}') || return 1
    fi

    if [[ -n "${COPILOT_ASK_USER_VARIANT:-}" ]]; then
        params=$(echo "$params" | jq -c --arg auv "$COPILOT_ASK_USER_VARIANT" '. + {"askUserVariant":$auv}') || return 1
    fi
    if [[ -n "${COPILOT_SANDBOX_CONFIG:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson sc "$COPILOT_SANDBOX_CONFIG" '. + {"sandboxConfig":$sc}') || return 1
    fi
    if [[ -n "${COPILOT_FEATURE_FLAGS:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson ff "$COPILOT_FEATURE_FLAGS" '. + {"featureFlags":$ff}') || return 1
    fi
    if [[ -n "${COPILOT_ALLOWED_MODELS:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson am "$COPILOT_ALLOWED_MODELS" '. + {"allowedModels":$am}') || return 1
    fi
    if [[ -n "${COPILOT_AUTH_CLIENT_ID_METADATA_URL:-}" ]]; then
        params=$(echo "$params" | jq -c --arg acmu "$COPILOT_AUTH_CLIENT_ID_METADATA_URL" '. + {"authClientIdMetadataUrl":$acmu}') || return 1
    fi
    if [[ -n "${COPILOT_AGENT_DIRECTORIES:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson agd "$COPILOT_AGENT_DIRECTORIES" '. + {"agentDirectories":$agd}') || return 1
    fi
    if [[ -n "${COPILOT_HOOK_DIRECTORIES:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson hd "$COPILOT_HOOK_DIRECTORIES" '. + {"hookDirectories":$hd}') || return 1
    fi
    if [[ -n "${COPILOT_INTERACTIVE_USER:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson iu "$(_copilot_json_bool "$COPILOT_INTERACTIVE_USER")" '. + {"interactiveUser":$iu}') || return 1
    fi
    if [[ -n "${COPILOT_EXCLUDED_MANAGED_SETTINGS:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson ems "$COPILOT_EXCLUDED_MANAGED_SETTINGS" '. + {"excludedManagedSettings":$ems}') || return 1
    fi
    if [[ -n "${COPILOT_ENFORCE_MANAGED_MODEL_DEFAULTS:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson emmd "$(_copilot_json_bool "$COPILOT_ENFORCE_MANAGED_MODEL_DEFAULTS")" '. + {"enforceManagedModelDefaults":$emmd}') || return 1
    fi
    if [[ -n "${COPILOT_IMAGE_GENERATION:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson ig "$COPILOT_IMAGE_GENERATION" '. + {"imageGeneration":$ig}') || return 1
    fi
    if [[ -n "${COPILOT_AUTOPILOT:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson ap "$COPILOT_AUTOPILOT" '. + {"autopilot":$ap}') || return 1
    fi
    if [[ -n "${COPILOT_PROVIDER:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson prov "$COPILOT_PROVIDER" '. + {"provider":$prov}') || return 1
    fi
    # Sub-agent lifecycle hooks: ask the runtime to send hooks.invoke (dispatched in session.sh)
    if [[ -n "${COPILOT_ON_SUBAGENT_START:-}" ]] || [[ -n "${COPILOT_ON_SUBAGENT_STOP:-}" ]]; then
        params=$(echo "$params" | jq -c '. + {"hooks":true}') || return 1
    fi

    echo "$params"
}

# --- GitHub token provider registry helpers ---

# Register a GitHub token provider function under a new unique registration ID
# (a UUID when uuidgen or /proc is available).
#
# Arguments:
#   $1 - Provider function name
#   $2 - Session ID the registration belongs to (may be empty until known)
#
# Sets:
#   COPILOT_GITHUB_TOKEN_REGISTRATION_ID - The new registration ID
_copilot_github_token_provider_register() {
    local provider="$1"
    local session_id="${2:-}"
    local reg_id=""

    _COPILOT_GITHUB_TOKEN_REGISTRATION_COUNTER=$(( _COPILOT_GITHUB_TOKEN_REGISTRATION_COUNTER + 1 ))
    if command -v uuidgen > /dev/null 2>&1; then
        reg_id=$(uuidgen 2>/dev/null) || reg_id=""
    elif [[ -r /proc/sys/kernel/random/uuid ]]; then
        reg_id=$(cat /proc/sys/kernel/random/uuid 2>/dev/null) || reg_id=""
    fi
    if [[ -z "$reg_id" ]]; then
        reg_id="ghtp-$$-$(date +%s)-${RANDOM}${RANDOM}-${_COPILOT_GITHUB_TOKEN_REGISTRATION_COUNTER}"
    fi

    COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY+="${reg_id}|${provider}|${session_id}"$'\n'
    COPILOT_GITHUB_TOKEN_REGISTRATION_ID="$reg_id"
    return 0
}

# Remove a registration by ID.
_copilot_github_token_provider_unregister() {
    local reg_id="$1"
    local updated="" line
    while IFS= read -r line; do
        if [[ -n "$line" ]] && [[ "${line%%|*}" != "$reg_id" ]]; then
            updated+="${line}"$'\n'
        fi
    done <<< "$COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY"
    COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY="$updated"
    return 0
}

# Remove every registration bound to a session (used when the session is destroyed).
_copilot_github_token_provider_unregister_session() {
    local session_id="$1"
    local updated="" line rid fn sid
    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi
        IFS='|' read -r rid fn sid <<< "$line"
        if [[ "$sid" != "$session_id" ]]; then
            updated+="${line}"$'\n'
        fi
    done <<< "$COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY"
    COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY="$updated"
    return 0
}

# Bind a registration to its session, replacing any earlier registration for the
# same session (for example from a previous resume).
_copilot_github_token_provider_commit() {
    local reg_id="$1"
    local session_id="$2"
    local updated="" line rid fn sid
    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi
        IFS='|' read -r rid fn sid <<< "$line"
        if [[ "$rid" == "$reg_id" ]]; then
            updated+="${rid}|${fn}|${session_id}"$'\n'
        elif [[ "$sid" != "$session_id" ]]; then
            updated+="${line}"$'\n'
        fi
    done <<< "$COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY"
    COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY="$updated"
    return 0
}

# Look up a registration by ID.
#
# Sets:
#   _COPILOT_GITHUB_TOKEN_PROVIDER_FN      - The provider function name
#   _COPILOT_GITHUB_TOKEN_PROVIDER_SESSION - The bound session ID (may be empty)
#
# Returns 0 if found, 1 if not.
_copilot_github_token_provider_lookup() {
    local reg_id="$1"
    local line rid fn sid
    _COPILOT_GITHUB_TOKEN_PROVIDER_FN=""
    _COPILOT_GITHUB_TOKEN_PROVIDER_SESSION=""
    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi
        IFS='|' read -r rid fn sid <<< "$line"
        if [[ -n "$reg_id" ]] && [[ "$rid" == "$reg_id" ]]; then
            _COPILOT_GITHUB_TOKEN_PROVIDER_FN="$fn"
            _COPILOT_GITHUB_TOKEN_PROVIDER_SESSION="$sid"
            return 0
        fi
    done <<< "$COPILOT_GITHUB_TOKEN_PROVIDER_REGISTRY"
    return 1
}

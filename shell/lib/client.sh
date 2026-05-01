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

# Excluded built-in tools (JSON array string, optional)
# Example: '["view","edit"]'
COPILOT_EXCLUDED_TOOLS=""
# Available/allowed tools (JSON array string, optional; takes precedence over excluded_tools)
# Example: '["grep","glob"]'
COPILOT_AVAILABLE_TOOLS=""
# Commands (JSON array string of {name, description} objects, optional)
# Example: '[{"name":"deploy","description":"Deploy to prod"}]'
COPILOT_COMMANDS=""

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
    if ! copilot_jsonrpc_start "$cli_path" "--log-level" "$log_level"; then
        COPILOT_CLIENT_STATE="error"
        echo "ERROR: Failed to start Copilot CLI process" >&2
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

    if ! copilot_jsonrpc_request "session.create" "$params"; then
        echo "ERROR: Failed to create session" >&2
        return 1
    fi

    COPILOT_SESSION_ID=$(copilot_jsonrpc_get_result_field '.sessionId')
    if [[ -z "$COPILOT_SESSION_ID" ]] || [[ "$COPILOT_SESSION_ID" == "null" ]]; then
        echo "ERROR: No sessionId in response" >&2
        return 1
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

    if ! copilot_jsonrpc_request "session.resume" "$params"; then
        echo "ERROR: Failed to resume session $session_id" >&2
        return 1
    fi

    COPILOT_SESSION_ID=$(copilot_jsonrpc_get_result_field '.sessionId')
    if [[ -z "$COPILOT_SESSION_ID" ]] || [[ "$COPILOT_SESSION_ID" == "null" ]]; then
        echo "ERROR: No sessionId in resume response" >&2
        return 1
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

#!/usr/bin/env bash
# Copilot Session - functions for managing individual conversation sessions.
#
# This module provides functions for sending messages, receiving responses,
# and managing the lifecycle of a conversation session with the Copilot CLI.
#
# Requires: json_rpc.sh

# --- Response Format Constants ---
# Use these with the response_format parameter of send functions.
COPILOT_RESPONSE_FORMAT_TEXT="text"
COPILOT_RESPONSE_FORMAT_IMAGE="image"
COPILOT_RESPONSE_FORMAT_JSON_OBJECT="json_object"

# --- System Prompt Section Constants ---
COPILOT_SECTION_IDENTITY="identity"
COPILOT_SECTION_TONE="tone"
COPILOT_SECTION_TOOL_EFFICIENCY="tool_efficiency"
COPILOT_SECTION_ENVIRONMENT_CONTEXT="environment_context"
COPILOT_SECTION_CODE_CHANGE_RULES="code_change_rules"
COPILOT_SECTION_GUIDELINES="guidelines"
COPILOT_SECTION_SAFETY="safety"
COPILOT_SECTION_TOOL_INSTRUCTIONS="tool_instructions"
COPILOT_SECTION_CUSTOM_INSTRUCTIONS="custom_instructions"
COPILOT_SECTION_LAST_INSTRUCTIONS="last_instructions"

# --- Section Override Action Constants ---
COPILOT_OVERRIDE_REPLACE="replace"
COPILOT_OVERRIDE_REMOVE="remove"
COPILOT_OVERRIDE_APPEND="append"
COPILOT_OVERRIDE_PREPEND="prepend"

# --- System Message Mode Constants ---
COPILOT_SYSTEM_MESSAGE_APPEND="append"
COPILOT_SYSTEM_MESSAGE_REPLACE="replace"
COPILOT_SYSTEM_MESSAGE_CUSTOMIZE="customize"

# --- Tool Defer Loading Constants (ToolDefer) ---
# Control whether a tool is eagerly pre-loaded or lazily discovered via search.
# Set a tool's "defer" field (wire key: toolDefer) to one of these:
COPILOT_TOOL_DEFER_AUTO="auto"     # lazy: discovered on demand
COPILOT_TOOL_DEFER_NEVER="never"   # eager: always pre-loaded

# --- System Message Section (extended) ---
# The identity "preamble" section, and the "preserve" action that shields an
# individually-addressable section from a group-level remove.
COPILOT_SECTION_PREAMBLE="preamble"
COPILOT_OVERRIDE_PRESERVE="preserve"

# --- GitHub Attachment Type Constants ---
# GitHub-anchored attachment variants for context attachments.
COPILOT_GITHUB_COMMIT="GitHubCommit"
COPILOT_GITHUB_RELEASE="GitHubRelease"
COPILOT_GITHUB_ACTIONS_JOB="GitHubActionsJob"
COPILOT_GITHUB_REPOSITORY="GitHubRepository"
COPILOT_GITHUB_FILE_DIFF="GitHubFileDiff"
COPILOT_GITHUB_TREE_COMPARISON="GitHubTreeComparison"
COPILOT_GITHUB_URL="GitHubUrl"
COPILOT_GITHUB_FILE="GitHubFile"
COPILOT_GITHUB_SNIPPET="GitHubSnippet"

# --- Message Send Options ---
# Set these before calling copilot_session_send / _send_and_wait to include them
# in the session.send payload (camelCase wire keys).
# Agent mode override for a single send (wire: agentMode)
COPILOT_AGENT_MODE=""
# Display prompt shown to the user instead of the real prompt (wire: displayPrompt)
COPILOT_DISPLAY_PROMPT=""
# Message source (wire: source): "user", "system", or "agent-<id>"
# (use the COPILOT_MESSAGE_SOURCE_* constants or copilot_message_source_agent)
COPILOT_MESSAGE_SOURCE=""
# JSON Schema for a structured response (JSON object string). When set, the send
# payload carries responseFormat {"type":"json_schema","jsonSchema":{"name":"response",
# "strict":true,"schema":<schema>}} in place of the legacy string response format.
COPILOT_RESPONSE_SCHEMA=""

# --- Message Source Constants (MessageSource) ---
COPILOT_MESSAGE_SOURCE_USER="user"
COPILOT_MESSAGE_SOURCE_SYSTEM="system"

# Build an agent message source ("agent-<id>").
# Usage: COPILOT_MESSAGE_SOURCE=$(copilot_message_source_agent "build-bot")
copilot_message_source_agent() {
    echo "agent-$1"
}

# --- Additional Handler Conventions (parity with @github/copilot-sdk) ---
# Like COPILOT_ELICITATION_HANDLER, set each of these to a shell function name.
#
# BYOK bearer token provider (per-session token minting). Convention:
#   copilot_bearer_token_provider <session_id>   → echoes bearer token
# (also referred to as get_bearer_token in other SDKs)
COPILOT_BEARER_TOKEN_PROVIDER=""
# HTTP request handler intercepting outbound LLM inference requests
# (CopilotRequestHandler). Convention:
#   copilot_request_handler <request_json>       → echoes (possibly mutated) request_json
COPILOT_REQUEST_HANDLER=""
# Lifecycle hooks (set to function names). Hook conventions:
#   copilot_on_pre_tool_use      <context_json>
#   copilot_on_post_tool_use     <context_json>   (post_tool_use)
#   copilot_on_pre_mcp_tool_call <context_json>   (pre_mcp_tool_call)
#   copilot_on_user_prompt_transformed <context_json>   (user_prompt_transformed)
#   copilot_on_mcp_auth_request  <context_json>
COPILOT_ON_POST_TOOL_USE=""
COPILOT_ON_PRE_MCP_TOOL_CALL=""
# Sub-agent lifecycle hooks (subagentStart / subagentStop). Setting either one makes
# create/resume send "hooks": true; hooks.invoke requests are then answered by
# _copilot_session_handle_hooks_invoke, which also dispatches COPILOT_ON_POST_TOOL_USE
# and COPILOT_ON_PRE_MCP_TOOL_CALL. Conventions (echo a JSON output object, or nothing):
#   copilot_on_subagent_start <input_json>
#       input: {transcriptPath, agentName, agentDisplayName?, agentDescription?, ...}
#       output: {"additionalContext":"..."}
#   copilot_on_subagent_stop  <input_json>
#       input: start fields + {agentId?, agentType, stopReason:"end_turn", response}
#       output: {"decision":"block"|"allow","reason":"...","modifiedResponse":"..."}
COPILOT_ON_SUBAGENT_START=""
COPILOT_ON_SUBAGENT_STOP=""

# --- Command Handler Support ---
# Command contexts are passed as JSON strings with these fields:
#   sessionId     - Session ID where the command was invoked
#   command       - Full command text (e.g. "/deploy production")
#   commandName   - Command name without leading /
#   args          - Raw argument string after the command name
#
# Command definitions are JSON objects with:
#   name          - Command name (without leading /)
#   description   - Human-readable description (optional)
#   handler       - Name of the shell function to call
#
# Command handlers are shell functions that receive a JSON context string:
#   my_command_handler() { local context="$1"; ... }

# --- Elicitation Support ---
# Elicitation contexts are passed as JSON strings with these fields:
#   sessionId         - Session that triggered the request
#   message           - Prompt message to show the user
#   requestedSchema   - JSON schema for expected input (optional)
#   mode              - Elicitation mode (optional)
#   elicitationSource - Source that initiated the request (optional)
#   url               - Optional URL for context
#
# Elicitation results are JSON objects with:
#   action   - "accept", "decline", or "cancel"
#   content  - JSON string of form values (optional, for "accept")
#
# Elicitation handlers are shell functions that receive a JSON context string
# and echo a JSON result:
#   my_elicitation_handler() { local context="$1"; echo '{"action":"accept","content":{}}'; }

# --- Handler Registration ---
# Set these to a shell function name to handle callbacks during send_and_wait.

# Elicitation handler function name (receives JSON context, echoes JSON result)
COPILOT_ELICITATION_HANDLER=""
# Command handler function name (receives JSON context, echoes JSON result)
COPILOT_COMMAND_HANDLER=""

# --- Session State ---
# The active session ID (set by copilot_client_create_session / resume)
COPILOT_SESSION_ID=""
# Last assistant message content from send_and_wait
COPILOT_SESSION_LAST_RESPONSE=""
# Last session events JSON array from get_messages
COPILOT_SESSION_MESSAGES=""
# Parsed structured result (compact JSON) from send_and_wait_structured
COPILOT_SESSION_STRUCTURED_RESPONSE=""

# --- Session Functions ---

# Send a message to the current session.
#
# Arguments:
#   $1 - The prompt text (required)
#   $2 - Optional session ID (defaults to COPILOT_SESSION_ID)
#   $3 - Optional response format (use COPILOT_RESPONSE_FORMAT_* constants)
#   $4 - Optional image options JSON string (e.g. '{"size":"1024x1024","quality":"high","style":"natural"}')
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full send response
#
# Usage:
#   copilot_session_send "What is 2+2?"
#   copilot_session_send "Generate a cat" "" "$COPILOT_RESPONSE_FORMAT_IMAGE" '{"size":"1024x1024","quality":"high","style":"natural"}'
#   message_id=$(copilot_jsonrpc_get_result_field '.messageId')
#
# Returns 0 on success, 1 on failure.
copilot_session_send() {
    local prompt="$1"
    local session_id="${2:-$COPILOT_SESSION_ID}"
    local response_format="${3:-}"
    local image_options="${4:-}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    if [[ -z "$prompt" ]]; then
        echo "ERROR: prompt is required" >&2
        return 1
    fi

    local params
    params=$(jq -c -n \
        --arg sid "$session_id" \
        --arg prompt "$prompt" \
        '{"sessionId":$sid,"prompt":$prompt}')

    # Add optional responseFormat if provided
    if [[ -n "$response_format" ]]; then
        params=$(echo "$params" | jq -c --arg rf "$response_format" '. + {"responseFormat":$rf}')
    fi

    # Add optional imageOptions if provided (expects a JSON string)
    if [[ -n "$image_options" ]]; then
        params=$(echo "$params" | jq -c --argjson io "$image_options" '. + {"imageOptions":$io}')
    fi

    # Add custom HTTP headers for outbound model requests if set
    if [[ -n "$COPILOT_REQUEST_HEADERS" ]]; then
        params=$(echo "$params" | jq -c --argjson rh "$COPILOT_REQUEST_HEADERS" '. + {"requestHeaders":$rh}')
    fi

    # Add message agent mode override if set
    if [[ -n "$COPILOT_AGENT_MODE" ]]; then
        params=$(echo "$params" | jq -c --arg am "$COPILOT_AGENT_MODE" '. + {"agentMode":$am}')
    fi

    # Add display prompt (shown to the user in place of the real prompt) if set
    if [[ -n "$COPILOT_DISPLAY_PROMPT" ]]; then
        params=$(echo "$params" | jq -c --arg dp "$COPILOT_DISPLAY_PROMPT" '. + {"displayPrompt":$dp}')
    fi

    # Add message source and structured-output schema if set
    params=$(_copilot_session_add_message_options "$params") || return 1

    if ! copilot_jsonrpc_request "session.send" "$params"; then
        echo "ERROR: Failed to send message" >&2
        return 1
    fi

    return 0
}

# Send a message and wait for the session to become idle.
#
# This function sends a prompt, then polls for events until it receives
# a session.idle event, indicating the assistant has finished responding.
#
# Arguments:
#   $1 - The prompt text (required)
#   $2 - Optional session ID (defaults to COPILOT_SESSION_ID)
#   $3 - Timeout in seconds (default: 60)
#   $4 - Poll interval in seconds (default: 1)
#   $5 - Optional response format (use COPILOT_RESPONSE_FORMAT_* constants)
#   $6 - Optional image options JSON string (e.g. '{"size":"1024x1024","quality":"high","style":"natural"}')
#
# Sets:
#   COPILOT_SESSION_LAST_RESPONSE - The last assistant message content (text)
#   COPILOT_JSONRPC_LAST_RESPONSE - The last raw JSON-RPC response
#
# Returns 0 on success (idle reached), 1 on failure or timeout.
copilot_session_send_and_wait() {
    local prompt="$1"
    local session_id="${2:-$COPILOT_SESSION_ID}"
    local timeout="${3:-60}"
    local poll_interval="${4:-1}"
    local response_format="${5:-}"
    local image_options="${6:-}"

    COPILOT_SESSION_LAST_RESPONSE=""

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    if [[ -z "$prompt" ]]; then
        echo "ERROR: prompt is required" >&2
        return 1
    fi

    # Send the message
    local params
    params=$(jq -c -n \
        --arg sid "$session_id" \
        --arg prompt "$prompt" \
        '{"sessionId":$sid,"prompt":$prompt}')

    # Add optional responseFormat if provided
    if [[ -n "$response_format" ]]; then
        params=$(echo "$params" | jq -c --arg rf "$response_format" '. + {"responseFormat":$rf}')
    fi

    # Add optional imageOptions if provided (expects a JSON string)
    if [[ -n "$image_options" ]]; then
        params=$(echo "$params" | jq -c --argjson io "$image_options" '. + {"imageOptions":$io}')
    fi

    # Add custom HTTP headers for outbound model requests if set
    if [[ -n "$COPILOT_REQUEST_HEADERS" ]]; then
        params=$(echo "$params" | jq -c --argjson rh "$COPILOT_REQUEST_HEADERS" '. + {"requestHeaders":$rh}')
    fi

    # Add message agent mode override if set
    if [[ -n "$COPILOT_AGENT_MODE" ]]; then
        params=$(echo "$params" | jq -c --arg am "$COPILOT_AGENT_MODE" '. + {"agentMode":$am}')
    fi

    # Add display prompt (shown to the user in place of the real prompt) if set
    if [[ -n "$COPILOT_DISPLAY_PROMPT" ]]; then
        params=$(echo "$params" | jq -c --arg dp "$COPILOT_DISPLAY_PROMPT" '. + {"displayPrompt":$dp}')
    fi

    # Add message source and structured-output schema if set
    params=$(_copilot_session_add_message_options "$params") || return 1

    if ! copilot_jsonrpc_request "session.send" "$params"; then
        echo "ERROR: Failed to send message" >&2
        return 1
    fi

    # Poll for events until we see session.idle
    local deadline
    deadline=$(( $(date +%s) + timeout ))
    local last_assistant_content=""

    while true; do
        local now
        now=$(date +%s)
        if [[ $now -ge $deadline ]]; then
            echo "ERROR: Timeout after ${timeout}s waiting for session.idle" >&2
            COPILOT_SESSION_LAST_RESPONSE="$last_assistant_content"
            return 1
        fi

        # Try to read next message from the stream.
        # The CLI sends notifications as events occur.
        if copilot_jsonrpc_read_message; then
            local msg="$COPILOT_JSONRPC_LAST_RESPONSE"

            # Check if this is a notification
            local method
            method=$(echo "$msg" | jq -r '.method // empty' 2>/dev/null)

            if [[ "$method" == "session.event" ]]; then
                local event_type event_id event_agent_id
                event_type=$(echo "$msg" | jq -r '.params.event.type // empty' 2>/dev/null)
                event_id=$(echo "$msg" | jq -r '.params.event.id // empty' 2>/dev/null)
                event_agent_id=$(echo "$msg" | jq -r '.params.event.agentId // empty' 2>/dev/null)

                case "$event_type" in
                    "assistant.message")
                        last_assistant_content=$(echo "$msg" | jq -r '.params.event.data.content // empty' 2>/dev/null)
                        ;;
                    "session.idle")
                        COPILOT_SESSION_LAST_RESPONSE="$last_assistant_content"
                        return 0
                        ;;
                    "session.error")
                        local error_msg
                        error_msg=$(echo "$msg" | jq -r '.params.event.data.message // "Unknown error"' 2>/dev/null)
                        echo "ERROR: Session error: $error_msg" >&2
                        COPILOT_SESSION_LAST_RESPONSE="$last_assistant_content"
                        return 1
                        ;;
                esac

            # Handle elicitation requests from the server
            elif [[ "$method" == "session.elicitationRequest" ]]; then
                local req_id
                req_id=$(echo "$msg" | jq -r '.id // empty' 2>/dev/null)
                local elicit_result='{"action":"cancel"}'

                if [[ -n "$COPILOT_ELICITATION_HANDLER" ]] && declare -f "$COPILOT_ELICITATION_HANDLER" > /dev/null 2>&1; then
                    local elicit_context
                    elicit_context=$(echo "$msg" | jq -c '.params // {}' 2>/dev/null)
                    elicit_result=$("$COPILOT_ELICITATION_HANDLER" "$elicit_context" 2>/dev/null) || elicit_result='{"action":"cancel"}'
                fi

                if [[ -n "$req_id" ]]; then
                    local response
                    response=$(jq -c -n --argjson id "$req_id" --argjson res "$elicit_result" \
                        '{"jsonrpc":"2.0","id":$id,"result":$res}')
                    copilot_jsonrpc_send_message "$response"
                fi

            # Handle command requests from the server
            elif [[ "$method" == "session.commandRequest" ]]; then
                local req_id
                req_id=$(echo "$msg" | jq -r '.id // empty' 2>/dev/null)
                local cmd_result='{}'

                if [[ -n "$COPILOT_COMMAND_HANDLER" ]] && declare -f "$COPILOT_COMMAND_HANDLER" > /dev/null 2>&1; then
                    local cmd_context
                    cmd_context=$(echo "$msg" | jq -c '.params // {}' 2>/dev/null)
                    cmd_result=$("$COPILOT_COMMAND_HANDLER" "$cmd_context" 2>/dev/null) || cmd_result='{}'
                fi

                if [[ -n "$req_id" ]]; then
                    local response
                    response=$(jq -c -n --argjson id "$req_id" --argjson res "$cmd_result" \
                        '{"jsonrpc":"2.0","id":$id,"result":$res}')
                    copilot_jsonrpc_send_message "$response"
                fi

            # Answer server -> client requests (gitHubToken.getToken, hooks.invoke)
            elif [[ "$method" == "gitHubToken.getToken" ]] || [[ "$method" == "hooks.invoke" ]]; then
                _copilot_handle_server_request "$msg" || true
            fi
        fi

        # Brief sleep to avoid busy-waiting
        sleep "$poll_interval"
    done
}

# Get all messages/events from the current session history.
#
# Arguments:
#   $1 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Sets:
#   COPILOT_SESSION_MESSAGES - JSON array of all session events
#   COPILOT_JSONRPC_LAST_RESPONSE - The full getMessages response
#
# Usage:
#   copilot_session_get_messages
#   echo "$COPILOT_SESSION_MESSAGES" | jq '.[] | select(.type == "assistant.message") | .data.content'
#
# Returns 0 on success, 1 on failure.
copilot_session_get_messages() {
    local session_id="${1:-$COPILOT_SESSION_ID}"

    COPILOT_SESSION_MESSAGES=""

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.getMessages" "$params"; then
        echo "ERROR: Failed to get messages" >&2
        return 1
    fi

    COPILOT_SESSION_MESSAGES=$(copilot_jsonrpc_get_result_field '.events')
    return 0
}

# Get metadata for the current session.
#
# Arguments:
#   $1 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The full getMetadata response
#
# Usage:
#   copilot_session_get_metadata
#   echo "$COPILOT_JSONRPC_LAST_RESPONSE" | jq '.result'
#
# Returns 0 on success, 1 on failure.
copilot_session_get_metadata() {
    local session_id="${1:-$COPILOT_SESSION_ID}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.getMetadata" "$params"; then
        echo "ERROR: Failed to get metadata" >&2
        return 1
    fi

    return 0
}

# Abort the currently processing message in the session.
#
# Arguments:
#   $1 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Returns 0 on success, 1 on failure.
copilot_session_abort() {
    local session_id="${1:-$COPILOT_SESSION_ID}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.abort" "$params"; then
        echo "ERROR: Failed to abort session" >&2
        return 1
    fi

    return 0
}

# Destroy the current session and release resources.
#
# Sends session.detach (keeping on-disk state so the session can be resumed);
# falls back to session.destroy when the CLI does not implement session.detach.
#
# Arguments:
#   $1 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Returns 0 on success, 1 on failure.
copilot_session_destroy() {
    local session_id="${1:-$COPILOT_SESSION_ID}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.detach" "$params"; then
        local error_code
        error_code=$(echo "$COPILOT_JSONRPC_LAST_ERROR" | jq -r '.code // empty' 2>/dev/null) || error_code=""
        # -32601 (method not found): older CLI without session.detach
        if [[ "$error_code" != "-32601" ]] || ! copilot_jsonrpc_request "session.destroy" "$params"; then
            echo "ERROR: Failed to destroy session" >&2
            return 1
        fi
    fi

    # Drop GitHub token provider registrations bound to this session
    if declare -f _copilot_github_token_provider_unregister_session > /dev/null 2>&1; then
        _copilot_github_token_provider_unregister_session "$session_id"
    fi

    # Clear session state if this was the active session
    if [[ "$session_id" == "$COPILOT_SESSION_ID" ]]; then
        COPILOT_SESSION_ID=""
        COPILOT_SESSION_LAST_RESPONSE=""
        COPILOT_SESSION_MESSAGES=""
    fi

    return 0
}

# Send a message with a JSON Schema and parse the structured result.
#
# Sets COPILOT_RESPONSE_SCHEMA for this call only, waits via
# copilot_session_send_and_wait, and parses the final assistant content as JSON.
#
# Arguments:
#   $1 - The prompt text (required)
#   $2 - JSON Schema (JSON object string, required)
#   $3 - Optional session ID (defaults to COPILOT_SESSION_ID)
#   $4 - Timeout in seconds (default: 60)
#
# Sets:
#   COPILOT_SESSION_STRUCTURED_RESPONSE - The parsed result (compact JSON)
#   COPILOT_SESSION_LAST_RESPONSE       - The raw assistant message content
#
# Usage:
#   copilot_session_send_and_wait_structured "What is 2+2?" \
#       '{"type":"object","properties":{"answer":{"type":"integer"}},"required":["answer"],"additionalProperties":false}'
#   echo "$COPILOT_SESSION_STRUCTURED_RESPONSE" | jq '.answer'
#
# Returns 0 on success, 1 on failure (including a missing or non-JSON result).
copilot_session_send_and_wait_structured() {
    local prompt="$1"
    local response_schema="${2:-}"
    local session_id="${3:-$COPILOT_SESSION_ID}"
    local timeout="${4:-60}"

    COPILOT_SESSION_STRUCTURED_RESPONSE=""

    if [[ -z "$response_schema" ]]; then
        echo "ERROR: response_schema is required" >&2
        return 1
    fi

    local COPILOT_RESPONSE_SCHEMA="$response_schema"
    if ! copilot_session_send_and_wait "$prompt" "$session_id" "$timeout"; then
        return 1
    fi

    if [[ -z "$COPILOT_SESSION_LAST_RESPONSE" ]]; then
        echo "ERROR: The requested run completed without a structured assistant response" >&2
        return 1
    fi

    local parsed
    if ! parsed=$(echo "$COPILOT_SESSION_LAST_RESPONSE" | jq -c . 2>/dev/null); then
        echo "ERROR: The structured assistant response is not valid JSON" >&2
        return 1
    fi
    COPILOT_SESSION_STRUCTURED_RESPONSE="$parsed"
    return 0
}

# Change the Auto routing preference (experimental) without changing the model.
#
# Arguments:
#   $1 - Tier: efficiency|balance|intelligence|fast; "" or "null" returns to
#        default Auto routing (sends JSON null)
#   $2 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The response (status, effectiveAutoTier,
#     pendingAutoTier, activatingAutoTier, supersededAutoTier)
#
# Usage:
#   copilot_session_set_auto_tier "intelligence"
#   copilot_jsonrpc_get_result_field '.status'
#
# Returns 0 on success, 1 on failure.
copilot_session_set_auto_tier() {
    local auto_tier="${1:-}"
    local session_id="${2:-$COPILOT_SESSION_ID}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    if [[ -z "$auto_tier" ]] || [[ "$auto_tier" == "null" ]]; then
        params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid,"autoTier":null}')
    else
        params=$(jq -c -n --arg sid "$session_id" --arg tier "$auto_tier" '{"sessionId":$sid,"autoTier":$tier}')
    fi

    if ! copilot_jsonrpc_request "session.model.switchAutoTier" "$params"; then
        echo "ERROR: Failed to switch auto tier" >&2
        return 1
    fi

    return 0
}

# Get the managed (enterprise) settings resolved for a session.
#
# Arguments:
#   $1 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The session.managedSettings.get response
#
# Returns 0 on success, 1 on failure.
copilot_session_get_managed_settings() {
    local session_id="${1:-$COPILOT_SESSION_ID}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.managedSettings.get" "$params"; then
        echo "ERROR: Failed to get managed settings" >&2
        return 1
    fi

    return 0
}

# Re-run the managed-settings policy helper for a session.
#
# Arguments:
#   $1 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Sets:
#   COPILOT_JSONRPC_LAST_RESPONSE - The session.managedSettings.recheckPolicyHelper response
#
# Returns 0 on success, 1 on failure.
copilot_session_recheck_policy_helper() {
    local session_id="${1:-$COPILOT_SESSION_ID}"

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" '{"sessionId":$sid}')

    if ! copilot_jsonrpc_request "session.managedSettings.recheckPolicyHelper" "$params"; then
        echo "ERROR: Failed to recheck policy helper" >&2
        return 1
    fi

    return 0
}

# --- Internal Functions ---

# Append the message source and structured-output schema (if set) to send params.
# Outputs the updated params JSON object.
_copilot_session_add_message_options() {
    local params="$1"

    if [[ -n "${COPILOT_MESSAGE_SOURCE:-}" ]]; then
        params=$(echo "$params" | jq -c --arg src "$COPILOT_MESSAGE_SOURCE" '. + {"source":$src}') || return 1
    fi
    if [[ -n "${COPILOT_RESPONSE_SCHEMA:-}" ]]; then
        params=$(echo "$params" | jq -c --argjson rs "$COPILOT_RESPONSE_SCHEMA" \
            '. + {"responseFormat":{"type":"json_schema","jsonSchema":{"name":"response","strict":true,"schema":$rs}}}') || return 1
    fi

    echo "$params"
}

# Answer a server -> client JSON-RPC request the SDK handles inline.
# Called from copilot_jsonrpc_request and copilot_session_send_and_wait.
#
# Arguments:
#   $1 - The raw JSON-RPC message
#
# Returns 0 if the request was handled, 1 otherwise.
_copilot_handle_server_request() {
    local msg="$1"
    local method req_id
    method=$(echo "$msg" | jq -r '.method // empty' 2>/dev/null) || method=""
    req_id=$(echo "$msg" | jq -c '.id // empty' 2>/dev/null) || req_id=""

    if [[ -z "$req_id" ]]; then
        return 1
    fi

    case "$method" in
        "gitHubToken.getToken")
            _copilot_session_handle_github_token_request "$msg" "$req_id"
            return 0
            ;;
        "hooks.invoke")
            _copilot_session_handle_hooks_invoke "$msg" "$req_id"
            return 0
            ;;
    esac

    return 1
}

# Answer gitHubToken.getToken by calling the registered COPILOT_GITHUB_TOKEN_PROVIDER.
_copilot_session_handle_github_token_request() {
    local msg="$1"
    local req_id="$2"
    local reg_id host session_id reason
    reg_id=$(echo "$msg" | jq -r '.params.registrationId // empty' 2>/dev/null) || reg_id=""
    host=$(echo "$msg" | jq -r '.params.host // empty' 2>/dev/null) || host=""
    session_id=$(echo "$msg" | jq -r '.params.sessionId // empty' 2>/dev/null) || session_id=""
    reason=$(echo "$msg" | jq -r '.params.reason // empty' 2>/dev/null) || reason=""

    local response
    if ! _copilot_github_token_provider_lookup "$reg_id" || \
        ! declare -f "$_COPILOT_GITHUB_TOKEN_PROVIDER_FN" > /dev/null 2>&1; then
        response=$(jq -c -n --argjson id "$req_id" \
            --arg msg "No GitHub token provider registered for registration ID \"$reg_id\"" \
            '{"jsonrpc":"2.0","id":$id,"error":{"code":-32603,"message":$msg}}')
        copilot_jsonrpc_send_message "$response"
        return 0
    fi

    if [[ -z "$session_id" ]]; then
        session_id="$_COPILOT_GITHUB_TOKEN_PROVIDER_SESSION"
    fi

    local args_json result=""
    args_json=$(jq -c -n --arg host "$host" --arg sid "$session_id" --arg reason "$reason" \
        '{"host":$host,"sessionId":(if $sid == "" then null else $sid end),"reason":$reason}')

    if ! result=$("$_COPILOT_GITHUB_TOKEN_PROVIDER_FN" "$args_json") || \
        ! echo "$result" | jq -e 'type == "object"' > /dev/null 2>&1; then
        response=$(jq -c -n --argjson id "$req_id" \
            '{"jsonrpc":"2.0","id":$id,"error":{"code":-32603,"message":"GitHub token provider failed or returned invalid JSON"}}')
        copilot_jsonrpc_send_message "$response"
        return 0
    fi

    response=$(jq -c -n --argjson id "$req_id" --argjson res "$result" \
        '{"jsonrpc":"2.0","id":$id,"result":$res}')
    copilot_jsonrpc_send_message "$response"
    return 0
}

# Answer hooks.invoke by dispatching to the configured hook function (if any).
# Unconfigured hook types are answered with a null output (default behavior).
_copilot_session_handle_hooks_invoke() {
    local msg="$1"
    local req_id="$2"
    local hook_type input handler="" output="null"
    hook_type=$(echo "$msg" | jq -r '.params.hookType // empty' 2>/dev/null) || hook_type=""
    input=$(echo "$msg" | jq -c '.params.input // {}' 2>/dev/null) || input="{}"

    case "$hook_type" in
        "subagentStart")  handler="${COPILOT_ON_SUBAGENT_START:-}" ;;
        "subagentStop")   handler="${COPILOT_ON_SUBAGENT_STOP:-}" ;;
        "postToolUse")    handler="${COPILOT_ON_POST_TOOL_USE:-}" ;;
        "preMcpToolCall") handler="${COPILOT_ON_PRE_MCP_TOOL_CALL:-}" ;;
    esac

    if [[ -n "$handler" ]] && declare -f "$handler" > /dev/null 2>&1; then
        local hook_result=""
        hook_result=$("$handler" "$input" 2>/dev/null) || hook_result=""
        if [[ -n "$hook_result" ]] && echo "$hook_result" | jq -e 'type == "object"' > /dev/null 2>&1; then
            output=$(echo "$hook_result" | jq -c .)
        fi
    fi

    local response
    response=$(jq -c -n --argjson id "$req_id" --argjson out "$output" \
        '{"jsonrpc":"2.0","id":$id,"result":{"output":$out}}')
    copilot_jsonrpc_send_message "$response"
    return 0
}

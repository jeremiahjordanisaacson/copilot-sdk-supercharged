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
# Opaque caller-chosen ID for the next turn (wire: clientTurnId), echoed on that turn's events
COPILOT_CLIENT_TURN_ID=""
# tool.shell_output handler function name. During send_and_wait each streamed chunk is passed
# to it as {toolCallId, text, sequence, stream?} (see copilot_shell_output_event_data).
COPILOT_ON_SHELL_OUTPUT=""

# --- Custom Tools ---
# Newline-separated "tool_name|handler_function" records used to answer
# external_tool.requested events during send_and_wait (see copilot_register_tool).
COPILOT_TOOL_HANDLER_REGISTRY=""

# Register a custom tool: appends its definition to COPILOT_TOOLS (sent on the next
# create/resume) and its handler to COPILOT_TOOL_HANDLER_REGISTRY.
#
# Arguments:
#   $1 - Tool name
#   $2 - Description shown to the model
#   $3 - Handler function name: my_tool <arguments_json> → echoes the result text
#        (a non-zero exit reports the call as failed)
#   $4 - Optional JSON Schema for the parameters (JSON object string)
#   $5 - Optional idempotent flag ("true"/"false")
#   $6 - Optional skip_permission flag ("true"): run without a permission prompt; otherwise
#        set COPILOT_PERMISSION_HANDLER to approve the tool
#
# Usage:
#   lookup_order() { echo "Order $(echo "$1" | jq -r '.id') shipped"; }
#   copilot_register_tool "lookup_order" "Look up an order" lookup_order \
#       '{"type":"object","properties":{"id":{"type":"string"}}}' true
copilot_register_tool() {
    local name="$1"
    local description="$2"
    local handler="$3"
    local parameters="${4:-}"
    local idempotent="${5:-}"
    local skip_permission="${6:-}"

    local definition
    definition=$(copilot_tool_definition "$name" "$description" "$parameters" "$idempotent" \
        "$skip_permission") || return 1
    COPILOT_TOOLS=$(jq -c -n --argjson tools "${COPILOT_TOOLS:-[]}" --argjson def "$definition" \
        '[$tools[] | select(.name != $def.name)] + [$def]') || return 1
    COPILOT_TOOL_HANDLER_REGISTRY+="${name}|${handler}"$'\n'
    return 0
}

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
#   requestId         - Pending request ID (optional, when the runtime sends one)
#   _meta             - Opaque metadata of negotiated extensions (optional)
#
# Elicitation results are JSON objects with:
#   action   - "accept", "decline", or "cancel"
#   content  - JSON string of form values (optional, for "accept")
#   _meta    - Opaque metadata (optional), sent back to the runtime unchanged
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
# Result JSON of the last copilot_session_set_permission_mode ({"success","mode","reason"?})
COPILOT_PERMISSION_MODE_RESULT=""

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
                    "tool.shell_output")
                        if [[ -n "${COPILOT_ON_SHELL_OUTPUT:-}" ]] && declare -f "$COPILOT_ON_SHELL_OUTPUT" > /dev/null 2>&1; then
                            local shell_output
                            shell_output=$(copilot_shell_output_event_data "$(echo "$msg" | jq -c '.params.event' 2>/dev/null)")
                            if [[ -n "$shell_output" ]]; then
                                "$COPILOT_ON_SHELL_OUTPUT" "$shell_output" || true
                            fi
                        fi
                        ;;
                    "external_tool.requested")
                        _copilot_session_handle_external_tool "$msg" "$session_id" || true
                        ;;
                    "permission.requested")
                        _copilot_session_handle_permission_requested "$msg" "$session_id" || true
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

            # Answer other server -> client requests (gitHubToken.getToken, hooks.invoke,
            # shellAuthentication.*, memoryStore.store, network.*, userInput.request)
            elif [[ -n "$method" ]]; then
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

# Change the session's permission mode.
#
# Arguments:
#   $1 - Permission mode: "manual", "assisted", or "allow-all"
#   $2 - Optional session ID (defaults to COPILOT_SESSION_ID)
#
# Sets:
#   COPILOT_PERMISSION_MODE_RESULT - The result JSON ({"success","mode","reason"?})
#   COPILOT_JSONRPC_LAST_RESPONSE  - The full session.permissions.setMode response
#
# When the runtime refuses the mode, the result has "success":false and, when known, a
# "reason" such as $COPILOT_PERMISSION_MODE_REFUSAL_MANAGED_POLICY ("managed_policy").
#
# Returns 0 when the mode was applied, 1 when it was refused or the request failed.
copilot_session_set_permission_mode() {
    local mode="$1"
    local session_id="${2:-$COPILOT_SESSION_ID}"

    COPILOT_PERMISSION_MODE_RESULT=""

    if [[ -z "$session_id" ]]; then
        echo "ERROR: No active session. Create or resume a session first." >&2
        return 1
    fi

    local params
    params=$(jq -c -n --arg sid "$session_id" --arg mode "$mode" '{"sessionId":$sid,"mode":$mode}')

    if ! copilot_jsonrpc_request "session.permissions.setMode" "$params"; then
        echo "ERROR: Failed to set permission mode" >&2
        return 1
    fi

    COPILOT_PERMISSION_MODE_RESULT=$(copilot_jsonrpc_get_result | jq -c '.' 2>/dev/null) || COPILOT_PERMISSION_MODE_RESULT=""
    if [[ "$(echo "$COPILOT_PERMISSION_MODE_RESULT" | jq -r '.success // false' 2>/dev/null)" != "true" ]]; then
        local reason
        reason=$(echo "$COPILOT_PERMISSION_MODE_RESULT" | jq -r '.reason // "unknown"' 2>/dev/null) || reason="unknown"
        echo "ERROR: Permission mode \"$mode\" was refused (reason: $reason)" >&2
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
    if [[ -n "${COPILOT_CLIENT_TURN_ID:-}" ]]; then
        params=$(echo "$params" | jq -c --arg ctid "$COPILOT_CLIENT_TURN_ID" '. + {"clientTurnId":$ctid}') || return 1
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
        "shellAuthentication.request")
            _copilot_session_handle_shell_authentication "$msg" "$req_id"
            return 0
            ;;
        "shellAuthentication.cancel")
            # Handlers run synchronously, so a cancellation only arrives after the handler
            # for that request has returned; acknowledge it.
            _copilot_send_rpc_result "$req_id" '{}'
            return 0
            ;;
        "memoryStore.store")
            _copilot_session_handle_memory_store "$msg" "$req_id"
            return 0
            ;;
        "network.resolveProxy"|"network.authenticateNegotiate"|"network.authenticateBasic")
            _copilot_handle_network_request "$method" "$msg" "$req_id"
            return 0
            ;;
        "userInput.request")
            _copilot_session_handle_user_input "$msg" "$req_id"
            return 0
            ;;
    esac

    return 1
}

# Send a JSON-RPC success response.
# Arguments:
#   $1 - Request id (JSON)
#   $2 - Result (JSON)
_copilot_send_rpc_result() {
    local response
    response=$(jq -c -n --argjson id "$1" --argjson res "$2" '{"jsonrpc":"2.0","id":$id,"result":$res}') || return 1
    copilot_jsonrpc_send_message "$response"
}

# Send a JSON-RPC error response.
# Arguments:
#   $1 - Request id (JSON)
#   $2 - Error code (integer)
#   $3 - Error message
_copilot_send_rpc_error() {
    local response
    response=$(jq -c -n --argjson id "$1" --argjson code "$2" --arg msg "$3" \
        '{"jsonrpc":"2.0","id":$id,"error":{"code":$code,"message":$msg}}') || return 1
    copilot_jsonrpc_send_message "$response"
}

# Answer shellAuthentication.request by calling COPILOT_SHELL_AUTHENTICATION_HANDLER.
# Only {"action":"accept","value":<string>} is forwarded; anything else cancels the prompt.
# The answer is never logged.
_copilot_session_handle_shell_authentication() {
    local msg="$1"
    local req_id="$2"
    local result='{"action":"cancel"}'
    local handler="${COPILOT_SHELL_AUTHENTICATION_HANDLER:-}"
    local request
    request=$(echo "$msg" | jq -c '.params // {}' 2>/dev/null) || request='{}'

    if [[ -n "$handler" ]] && declare -f "$handler" > /dev/null 2>&1 && \
        echo "$request" | jq -e '(.requestId | type) == "string" and (.program | type) == "string"
            and (.prompt | type) == "string"
            and (.inputKind == "secret" or .inputKind == "text" or .inputKind == "confirm")' \
            > /dev/null 2>&1; then
        local answer=""
        answer=$("$handler" "$request" 2>/dev/null) || answer=""
        if [[ -n "$answer" ]] && \
            echo "$answer" | jq -e '.action == "accept" and (.value | type) == "string"' > /dev/null 2>&1; then
            result=$(echo "$answer" | jq -c '{"action":"accept","value":.value}') || result='{"action":"cancel"}'
        fi
    fi

    _copilot_send_rpc_result "$req_id" "$result"
}

# Answer memoryStore.store by calling COPILOT_MEMORY_STORE_HANDLER; replies with a null result.
_copilot_session_handle_memory_store() {
    local msg="$1"
    local req_id="$2"
    local handler="${COPILOT_MEMORY_STORE_HANDLER:-}"

    if [[ -z "$handler" ]] || ! declare -f "$handler" > /dev/null 2>&1; then
        _copilot_send_rpc_error "$req_id" -32603 "No memory store is registered"
        return 0
    fi

    local memory
    memory=$(echo "$msg" | jq -c '.params | {"sessionId": .sessionId, "subject": .subject,
        "fact": .fact, "citations": .citations, "reason": .reason}' 2>/dev/null) || memory='{}'
    if ! "$handler" "$memory" >&2; then
        echo "WARN: Memory store callback failed" >&2
        _copilot_send_rpc_error "$req_id" -32603 "Memory store callback failed"
        return 0
    fi

    _copilot_send_rpc_result "$req_id" 'null'
}

# Answer userInput.request (the ask_user tool) by calling COPILOT_USER_INPUT_HANDLER with the
# request params ({"sessionId","question","choices"?,"allowFreeform"?,"requestId"?,
# "imageAttachmentConstraints"?,"_meta"?}). A JSON object output is sent as the
# UserInputResponse: answer, wasFreeform, and attachments (normalized to
# {"type":"image","data","mimeType","description"?,"displayName"?}, sent only when non-empty).
# Any other output is sent as a freeform answer. The response is piped to jq rather than
# passed as an argument, so base64 image attachments are not limited by the argument size.
_copilot_session_handle_user_input() {
    local msg="$1"
    local req_id="$2"
    local handler="${COPILOT_USER_INPUT_HANDLER:-}"

    if [[ -z "$handler" ]] || ! declare -f "$handler" > /dev/null 2>&1; then
        _copilot_send_rpc_error "$req_id" -32603 "No user input handler is registered"
        return 0
    fi

    local request
    request=$(echo "$msg" | jq -c '.params // {}' 2>/dev/null) || request='{}'

    local output=""
    if ! output=$("$handler" "$request"); then
        _copilot_send_rpc_error "$req_id" -32603 "User input handler failed"
        return 0
    fi

    local result=""
    result=$(printf '%s' "$output" | jq -c 'select(type == "object")
        | {"answer": ((.answer // "") | tostring), "wasFreeform": (.wasFreeform == true)}
          + ([(.attachments // [])[]? | select(type == "object")
              | {"type": "image", "data": .data, "mimeType": .mimeType}
                + (if .description == null then {} else {"description": .description} end)
                + (if .displayName == null then {} else {"displayName": .displayName} end)]
             | if length > 0 then {"attachments": .} else {} end)' 2>/dev/null) || result=""
    if [[ -z "$result" ]]; then
        result=$(jq -c -n --arg answer "$output" '{"answer":$answer,"wasFreeform":true}') || return 1
    fi

    local response
    response=$(printf '%s' "$result" | jq -c --argjson id "$req_id" '{"jsonrpc":"2.0","id":$id,"result":.}') || return 1
    copilot_jsonrpc_send_message "$response"
}

# Answer network.resolveProxy / network.authenticateNegotiate / network.authenticateBasic by
# calling the matching COPILOT_NETWORK_* function. Tokens and passwords are never logged.
_copilot_handle_network_request() {
    local method="$1"
    local msg="$2"
    local req_id="$3"
    local handler request

    case "$method" in
        "network.resolveProxy")
            handler="${COPILOT_NETWORK_RESOLVE_PROXY:-}"
            request=$(echo "$msg" | jq -c '{"url": .params.url}' 2>/dev/null) || request='{}'
            ;;
        "network.authenticateNegotiate")
            handler="${COPILOT_NETWORK_AUTHENTICATE_NEGOTIATE:-}"
            request=$(echo "$msg" | jq -c '{"spn": .params.spn}' 2>/dev/null) || request='{}'
            ;;
        *)
            handler="${COPILOT_NETWORK_AUTHENTICATE_BASIC:-}"
            request=$(echo "$msg" | jq -c '{"proxy": .params.proxy, "realm": .params.realm,
                "previousRejected": (.params.previousRejected == true)}' 2>/dev/null) || request='{}'
            ;;
    esac

    if [[ -z "$handler" ]] || ! declare -f "$handler" > /dev/null 2>&1; then
        _copilot_send_rpc_error "$req_id" -32603 "No callback is registered for $method"
        return 0
    fi

    local output=""
    if ! output=$("$handler" "$request"); then
        _copilot_send_rpc_error "$req_id" -32603 "The $method callback failed"
        return 0
    fi

    local result
    case "$method" in
        "network.resolveProxy")
            result=$(jq -c -n --arg v "$output" 'if $v == "" then {} else {"proxy": $v} end')
            ;;
        "network.authenticateNegotiate")
            result=$(jq -c -n --arg v "$output" 'if $v == "" then {} else {"token": $v} end')
            ;;
        *)
            # Send only the credential fields, never other members of the handler output.
            result=$(echo "${output:-null}" | jq -c 'if type == "object"
                then {"username": .username, "password": .password} | with_entries(select(.value != null))
                else {} end' 2>/dev/null) || result='{}'
            ;;
    esac

    _copilot_send_rpc_result "$req_id" "$result"
}

# Answer an external_tool.requested event by running the registered tool handler and sending
# session.tools.handlePendingToolCall without waiting for its response, so events that arrive
# meanwhile stay in the send_and_wait loop.
#
# Arguments:
#   $1 - The raw session.event notification
#   $2 - Fallback session ID
_copilot_session_handle_external_tool() {
    local msg="$1"
    local session_id="$2"
    local tool_name request_id has_handler event_session_id
    tool_name=$(echo "$msg" | jq -r '.params.event.data.toolName // empty' 2>/dev/null) || tool_name=""
    request_id=$(echo "$msg" | jq -r '.params.event.data.requestId // empty' 2>/dev/null) || request_id=""
    has_handler=$(echo "$msg" | jq -r '.params.event.data.hasHandler' 2>/dev/null) || has_handler=""
    event_session_id=$(echo "$msg" | jq -r '.params.sessionId // empty' 2>/dev/null) || event_session_id=""
    if [[ -n "$event_session_id" ]]; then
        session_id="$event_session_id"
    fi
    if [[ -z "$tool_name" ]] || [[ -z "$request_id" ]] || [[ "$has_handler" == "false" ]]; then
        return 0
    fi

    local handler="" line
    while IFS= read -r line; do
        if [[ "${line%%|*}" == "$tool_name" ]]; then
            handler="${line#*|}"
        fi
    done <<< "${COPILOT_TOOL_HANDLER_REGISTRY:-}"
    if [[ -z "$handler" ]] || ! declare -f "$handler" > /dev/null 2>&1; then
        return 0
    fi

    local arguments output="" params
    arguments=$(echo "$msg" | jq -c '.params.event.data.arguments // {}' 2>/dev/null) || arguments='{}'
    if output=$("$handler" "$arguments"); then
        params=$(jq -c -n --arg sid "$session_id" --arg rid "$request_id" --arg out "$output" \
            '{"sessionId":$sid,"requestId":$rid,"result":$out}')
    else
        params=$(jq -c -n --arg sid "$session_id" --arg rid "$request_id" --arg tool "$tool_name" \
            '{"sessionId":$sid,"requestId":$rid,"error":("Tool " + $tool + " failed")}')
    fi

    _copilot_jsonrpc_next_id
    local request
    request=$(jq -c -n --arg id "$COPILOT_JSONRPC_REQUEST_ID" --argjson params "$params" \
        '{"jsonrpc":"2.0","id":($id | tonumber),"method":"session.tools.handlePendingToolCall","params":$params}')
    copilot_jsonrpc_send_message "$request"
}

# Permission decision JSON for a COPILOT_PERMISSION_HANDLER result: "approve" / "allow" /
# "true" approve once, "approve-for-session" approves for the session, "deny" / "reject" /
# "false" reject, "user-not-available" (or empty output) reports that no user is available,
# and a JSON object with "kind" (for example {"kind":"reject","feedback":"..."}) is sent as
# given. Earlier result kinds ("approved", "denied-*") are mapped. "no-result" outputs nothing
# (no reply: the request stays pending for another client).
# Arguments:
#   $1 - Handler result
copilot_permission_decision_json() {
    local decision="${1:-}"
    local kind=""
    if [[ "$decision" == "{"* ]]; then
        kind=$(echo "$decision" | jq -r 'if type == "object" then (.kind // "") else "" end' 2>/dev/null) || kind=""
        case "$kind" in
            "no-result") return 0 ;;
            "approve-once"|"approve-for-session"|"approve-permanently"|"reject"|"user-not-available")
                echo "$decision" | jq -c '.'
                return 0
                ;;
            *) decision="$kind" ;;
        esac
    fi
    case "$decision" in
        "no-result") return 0 ;;
        "approve"|"approved"|"allow"|"approve-once"|"true"|"yes") kind="approve-once" ;;
        "approve-for-session"|"approve-permanently") kind="$decision" ;;
        ""|"user-not-available"|"denied-by-rules"|"denied-no-approval-rule-and-could-not-request-from-user"|"denied-by-content-exclusion-policy")
            kind="user-not-available"
            ;;
        *) kind="reject" ;;
    esac
    jq -c -n --arg k "$kind" '{"kind":$k}'
}

# Permission handlers for COPILOT_PERMISSION_HANDLER.
copilot_approve_all() {
    echo "approve"
}

copilot_deny_all() {
    echo "deny"
}

# Answer a permission.requested event with COPILOT_PERMISSION_HANDLER and send
# session.permissions.handlePendingPermissionRequest without waiting for its response. A
# failing handler reports that no user is available.
#
# Arguments:
#   $1 - The raw session.event notification
#   $2 - Fallback session ID
_copilot_session_handle_permission_requested() {
    local msg="$1"
    local session_id="$2"
    local handler="${COPILOT_PERMISSION_HANDLER:-}"
    if [[ -z "$handler" ]] || ! declare -f "$handler" > /dev/null 2>&1; then
        return 0
    fi
    local request_id resolved_by_hook event_session_id
    request_id=$(echo "$msg" | jq -r '.params.event.data.requestId // empty' 2>/dev/null) || request_id=""
    resolved_by_hook=$(echo "$msg" | jq -r '.params.event.data.resolvedByHook // false' 2>/dev/null) || resolved_by_hook="false"
    event_session_id=$(echo "$msg" | jq -r '.params.sessionId // empty' 2>/dev/null) || event_session_id=""
    if [[ -n "$event_session_id" ]]; then
        session_id="$event_session_id"
    fi
    if [[ -z "$request_id" ]] || [[ "$resolved_by_hook" == "true" ]]; then
        return 0
    fi

    local request decision result
    request=$(echo "$msg" | jq -c '.params.event.data.permissionRequest // {}' 2>/dev/null) || request='{}'
    if ! decision=$("$handler" "$request"); then
        decision="user-not-available"
    fi
    result=$(copilot_permission_decision_json "$decision") || result='{"kind":"user-not-available"}'
    if [[ -z "$result" ]]; then
        return 0
    fi

    _copilot_jsonrpc_next_id
    local params request_msg
    params=$(jq -c -n --arg sid "$session_id" --arg rid "$request_id" --argjson res "$result" \
        '{"sessionId":$sid,"requestId":$rid,"result":$res}')
    request_msg=$(jq -c -n --arg id "$COPILOT_JSONRPC_REQUEST_ID" --argjson params "$params" \
        '{"jsonrpc":"2.0","id":($id | tonumber),"method":"session.permissions.handlePendingPermissionRequest","params":$params}')
    copilot_jsonrpc_send_message "$request_msg"
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

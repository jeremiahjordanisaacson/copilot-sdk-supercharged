#!/usr/bin/env bash
# Copilot Types - Type constants and helpers for the Shell/Bash Copilot SDK.
#
# Defines type constants, configuration variables, and helper functions
# for the Copilot SDK types system.
#
# Copyright (c) Microsoft Corporation. All rights reserved.

# --- Connection State Constants ---
COPILOT_STATE_DISCONNECTED="disconnected"
COPILOT_STATE_CONNECTING="connecting"
COPILOT_STATE_CONNECTED="connected"
COPILOT_STATE_ERROR="error"

# --- Tool Result Type Constants ---
COPILOT_TOOL_RESULT_SUCCESS="success"
COPILOT_TOOL_RESULT_FAILURE="failure"
COPILOT_TOOL_RESULT_REJECTED="rejected"
COPILOT_TOOL_RESULT_DENIED="denied"

# --- Permission Kind Constants ---
COPILOT_PERMISSION_APPROVED="approved"
COPILOT_PERMISSION_DENIED_BY_RULES="denied-by-rules"
COPILOT_PERMISSION_DENIED_NO_APPROVAL="denied-no-approval-rule-and-could-not-request-from-user"
COPILOT_PERMISSION_DENIED_BY_USER="denied-interactively-by-user"

# --- Session Event Type Constants ---
COPILOT_EVENT_SESSION_START="session.start"
COPILOT_EVENT_SESSION_RESUME="session.resume"
COPILOT_EVENT_SESSION_ERROR="session.error"
COPILOT_EVENT_SESSION_IDLE="session.idle"
COPILOT_EVENT_SESSION_SHUTDOWN="session.shutdown"
COPILOT_EVENT_ASSISTANT_MESSAGE="assistant.message"
COPILOT_EVENT_ASSISTANT_MESSAGE_DELTA="assistant.message_delta"
COPILOT_EVENT_ASSISTANT_TURN_START="assistant.turn_start"
COPILOT_EVENT_ASSISTANT_TURN_END="assistant.turn_end"
COPILOT_EVENT_TOOL_EXECUTION_START="tool.execution_start"
COPILOT_EVENT_TOOL_EXECUTION_COMPLETE="tool.execution_complete"

# --- Slash Command Input Completion Constants ---
COPILOT_SLASH_COMMAND_INPUT_COMPLETION_DIRECTORY="directory"

# --- Slash Command Kind Constants ---
COPILOT_SLASH_COMMAND_KIND_BUILTIN="builtin"
COPILOT_SLASH_COMMAND_KIND_CLIENT="client"
COPILOT_SLASH_COMMAND_KIND_SKILL="skill"

# --- Model Picker Price Category Constants ---
COPILOT_MODEL_PICKER_PRICE_CATEGORY_HIGH="high"
COPILOT_MODEL_PICKER_PRICE_CATEGORY_LOW="low"
COPILOT_MODEL_PICKER_PRICE_CATEGORY_MEDIUM="medium"
COPILOT_MODEL_PICKER_PRICE_CATEGORY_VERY_HIGH="very_high"

# --- Model Capabilities Override ---
# Set these variables to override model capabilities for session creation.
# Example:
#   COPILOT_MODEL_CAPABILITIES='{"supports":{"vision":true},"limits":{"maxPromptTokens":4096}}'
# Already defined in client.sh: COPILOT_MODEL_CAPABILITIES

# --- Exit Plan Mode Handler ---
# Function name to call when server requests exit from plan mode.
# The function receives a JSON string with { "sessionId": "..." }
# and must echo a JSON result: { "approved": true } or { "approved": false }
COPILOT_EXIT_PLAN_MODE_HANDLER=""

# --- Trace Context Provider ---
# Function name to call to get trace context for distributed tracing.
# The function should echo a JSON object: { "traceparent": "...", "tracestate": "..." }
COPILOT_TRACE_CONTEXT_PROVIDER=""

# --- Remote Mode ---
# When set to "true", adds "--remote" flag to CLI args
COPILOT_REMOTE=""

# --- Remote Session Mode Constants (Experimental) ---
COPILOT_REMOTE_SESSION_MODE_EXPORT="export"
COPILOT_REMOTE_SESSION_MODE_OFF="off"
COPILOT_REMOTE_SESSION_MODE_ON="on"

# --- Session Telemetry ---
# When set to "true", enables session telemetry in create/resume payload
COPILOT_ENABLE_SESSION_TELEMETRY=""

# --- UserInput Types ---

# Build a UserInputRequest JSON object.
# Arguments:
#   $1 - Optional question
#   $2 - Optional choices JSON array
#   $3 - Optional allowFreeform JSON boolean
copilot_user_input_request() {
    local question="${1:-}"
    local choices="${2:-}"
    local allow_freeform="${3:-}"

    local json
    json=$(jq -c -n '{}')
    if [[ -n "$question" ]]; then
        json=$(echo "$json" | jq -c --arg question "$question" '. + {"question":$question}')
    fi
    if [[ -n "$choices" ]]; then
        json=$(echo "$json" | jq -c --argjson choices "$choices" '. + {"choices":$choices}')
    fi
    if [[ -n "$allow_freeform" ]]; then
        json=$(echo "$json" | jq -c --argjson allowFreeform "$allow_freeform" '. + {"allowFreeform":$allowFreeform}')
    fi
    echo "$json"
}

# Build a UserInputResponse JSON object.
# Arguments:
#   $1 - Required answer
#   $2 - Optional wasFreeform JSON boolean (default: false)
copilot_user_input_response() {
    local answer="$1"
    local was_freeform="${2:-false}"

    jq -c -n         --arg answer "$answer"         --argjson wasFreeform "$was_freeform"         '{"answer":$answer,"wasFreeform":$wasFreeform}'
}

# --- ResponseFormat / ImageOptions Types ---

COPILOT_RESPONSE_FORMAT_TEXT="text"
COPILOT_RESPONSE_FORMAT_IMAGE="image"
COPILOT_RESPONSE_FORMAT_JSON_OBJECT="json_object"

# Build an ImageOptions JSON object.
# Arguments:
#   $1 - Optional size
#   $2 - Optional quality
#   $3 - Optional style
copilot_image_options() {
    local size="${1:-}"
    local quality="${2:-}"
    local style="${3:-}"

    local json
    json=$(jq -c -n '{}')
    if [[ -n "$size" ]]; then
        json=$(echo "$json" | jq -c --arg size "$size" '. + {"size":$size}')
    fi
    if [[ -n "$quality" ]]; then
        json=$(echo "$json" | jq -c --arg quality "$quality" '. + {"quality":$quality}')
    fi
    if [[ -n "$style" ]]; then
        json=$(echo "$json" | jq -c --arg style "$style" '. + {"style":$style}')
    fi
    echo "$json"
}

# --- SystemMessage / SectionOverride Types ---

COPILOT_SECTION_OVERRIDE_REPLACE="replace"
COPILOT_SECTION_OVERRIDE_REMOVE="remove"
COPILOT_SECTION_OVERRIDE_APPEND="append"
COPILOT_SECTION_OVERRIDE_PREPEND="prepend"

# Build a SectionOverride JSON object.
# Arguments:
#   $1 - Required action
#   $2 - Optional content
copilot_section_override() {
    local action="$1"
    local content="${2:-}"

    local json
    json=$(jq -c -n --arg action "$action" '{"action":$action}')
    if [[ -n "$content" ]]; then
        json=$(echo "$json" | jq -c --arg content "$content" '. + {"content":$content}')
    fi
    echo "$json"
}

# Build a SystemMessageAppendConfig JSON object.
# Arguments:
#   $1 - Optional content
copilot_system_message_append_config() {
    local content="${1:-}"

    local json
    json=$(jq -c -n '{"mode":"append"}')
    if [[ -n "$content" ]]; then
        json=$(echo "$json" | jq -c --arg content "$content" '. + {"content":$content}')
    fi
    echo "$json"
}

# Build a SystemMessageReplaceConfig JSON object.
# Arguments:
#   $1 - Required content
copilot_system_message_replace_config() {
    local content="$1"

    jq -c -n --arg content "$content" '{"mode":"replace","content":$content}'
}

# Build a SystemMessageCustomizeConfig JSON object.
# Arguments:
#   $1 - Optional sections JSON object
#   $2 - Optional content
copilot_system_message_customize_config() {
    local sections="${1:-}"
    local content="${2:-}"

    local json
    json=$(jq -c -n '{"mode":"customize"}')
    if [[ -n "$sections" ]]; then
        json=$(echo "$json" | jq -c --argjson sections "$sections" '. + {"sections":$sections}')
    fi
    if [[ -n "$content" ]]; then
        json=$(echo "$json" | jq -c --arg content "$content" '. + {"content":$content}')
    fi
    echo "$json"
}

# --- CloudSession Types ---

# Build a CloudSessionRepository JSON object.
# Arguments:
#   $1 - Required owner
#   $2 - Required name
#   $3 - Optional branch
copilot_cloud_session_repository() {
    local owner="$1"
    local name="$2"
    local branch="${3:-}"

    local json
    json=$(jq -c -n --arg owner "$owner" --arg name "$name" '{"owner":$owner,"name":$name}')
    if [[ -n "$branch" ]]; then
        json=$(echo "$json" | jq -c --arg branch "$branch" '. + {"branch":$branch}')
    fi
    echo "$json"
}

# Build a CloudSessionOptions JSON object.
# Arguments:
#   $1 - Optional repository JSON object
copilot_cloud_session_options() {
    local repository="${1:-}"

    local json
    json=$(jq -c -n '{}')
    if [[ -n "$repository" ]]; then
        json=$(echo "$json" | jq -c --argjson repository "$repository" '. + {"repository":$repository}')
    fi
    echo "$json"
}

# --- Canvas Types ---

# Build a CanvasAction JSON object.
# Arguments:
#   $1 - Required name
#   $2 - Required description
#   $3 - Optional inputSchema JSON object
copilot_canvas_action() {
    local name="$1"
    local description="$2"
    local input_schema="${3:-}"

    local json
    json=$(jq -c -n --arg name "$name" --arg description "$description" '{"name":$name,"description":$description}')
    if [[ -n "$input_schema" ]]; then
        json=$(echo "$json" | jq -c --argjson inputSchema "$input_schema" '. + {"inputSchema":$inputSchema}')
    fi
    echo "$json"
}

# Build a CanvasDeclaration JSON object.
# Arguments:
#   $1 - Required id
#   $2 - Required displayName
#   $3 - Required description
#   $4 - Optional inputSchema JSON object
#   $5 - Optional actions JSON array
copilot_canvas_declaration() {
    local id="$1"
    local display_name="$2"
    local description="$3"
    local input_schema="${4:-}"
    local actions="${5:-}"

    local json
    json=$(jq -c -n         --arg id "$id"         --arg displayName "$display_name"         --arg description "$description"         '{"id":$id,"displayName":$displayName,"description":$description}')
    if [[ -n "$input_schema" ]]; then
        json=$(echo "$json" | jq -c --argjson inputSchema "$input_schema" '. + {"inputSchema":$inputSchema}')
    fi
    if [[ -n "$actions" ]]; then
        json=$(echo "$json" | jq -c --argjson actions "$actions" '. + {"actions":$actions}')
    fi
    echo "$json"
}

# Build a CanvasOpenResponse JSON object.
# Arguments:
#   $1 - Optional url
#   $2 - Optional title
#   $3 - Optional status
copilot_canvas_open_response() {
    local url="${1:-}"
    local title="${2:-}"
    local status="${3:-}"

    local json
    json=$(jq -c -n '{}')
    if [[ -n "$url" ]]; then
        json=$(echo "$json" | jq -c --arg url "$url" '. + {"url":$url}')
    fi
    if [[ -n "$title" ]]; then
        json=$(echo "$json" | jq -c --arg title "$title" '. + {"title":$title}')
    fi
    if [[ -n "$status" ]]; then
        json=$(echo "$json" | jq -c --arg status "$status" '. + {"status":$status}')
    fi
    echo "$json"
}

# Build a CanvasHostCapabilities JSON object.
# Arguments:
#   $1 - Optional canvases JSON boolean (default: false)
copilot_canvas_host_capabilities() {
    local canvases="${1:-false}"

    jq -c -n --argjson canvases "$canvases" '{"canvases":$canvases}'
}

# Build a CanvasHostContext JSON object.
# Arguments:
#   $1 - Optional capabilities JSON object
copilot_canvas_host_context() {
    local capabilities="${1:-}"

    if [[ -z "$capabilities" ]]; then
        capabilities=$(copilot_canvas_host_capabilities false)
    fi
    jq -c -n --argjson capabilities "$capabilities" '{"capabilities":$capabilities}'
}

# Build a CanvasOpenContext JSON object.
# Arguments:
#   $1 - Required sessionId
#   $2 - Required extensionId
#   $3 - Required canvasId
#   $4 - Required instanceId
#   $5 - Optional input JSON value
#   $6 - Optional host JSON object
copilot_canvas_open_context() {
    local session_id="$1"
    local extension_id="$2"
    local canvas_id="$3"
    local instance_id="$4"
    local input_json="${5:-}"
    local host_json="${6:-}"

    local json
    json=$(jq -c -n         --arg sessionId "$session_id"         --arg extensionId "$extension_id"         --arg canvasId "$canvas_id"         --arg instanceId "$instance_id"         '{"sessionId":$sessionId,"extensionId":$extensionId,"canvasId":$canvasId,"instanceId":$instanceId}')
    if [[ -n "$input_json" ]]; then
        json=$(echo "$json" | jq -c --argjson input "$input_json" '. + {"input":$input}')
    fi
    if [[ -n "$host_json" ]]; then
        json=$(echo "$json" | jq -c --argjson host "$host_json" '. + {"host":$host}')
    fi
    echo "$json"
}

# Build a CanvasActionContext JSON object.
# Arguments:
#   $1 - Required sessionId
#   $2 - Required extensionId
#   $3 - Required canvasId
#   $4 - Required instanceId
#   $5 - Required actionName
#   $6 - Optional input JSON value
#   $7 - Optional host JSON object
copilot_canvas_action_context() {
    local session_id="$1"
    local extension_id="$2"
    local canvas_id="$3"
    local instance_id="$4"
    local action_name="$5"
    local input_json="${6:-}"
    local host_json="${7:-}"

    local json
    json=$(jq -c -n         --arg sessionId "$session_id"         --arg extensionId "$extension_id"         --arg canvasId "$canvas_id"         --arg instanceId "$instance_id"         --arg actionName "$action_name"         '{"sessionId":$sessionId,"extensionId":$extensionId,"canvasId":$canvasId,"instanceId":$instanceId,"actionName":$actionName}')
    if [[ -n "$input_json" ]]; then
        json=$(echo "$json" | jq -c --argjson input "$input_json" '. + {"input":$input}')
    fi
    if [[ -n "$host_json" ]]; then
        json=$(echo "$json" | jq -c --argjson host "$host_json" '. + {"host":$host}')
    fi
    echo "$json"
}

# Build a CanvasLifecycleContext JSON object.
# Arguments:
#   $1 - Required sessionId
#   $2 - Required extensionId
#   $3 - Required canvasId
#   $4 - Required instanceId
#   $5 - Optional host JSON object
copilot_canvas_lifecycle_context() {
    local session_id="$1"
    local extension_id="$2"
    local canvas_id="$3"
    local instance_id="$4"
    local host_json="${5:-}"

    local json
    json=$(jq -c -n         --arg sessionId "$session_id"         --arg extensionId "$extension_id"         --arg canvasId "$canvas_id"         --arg instanceId "$instance_id"         '{"sessionId":$sessionId,"extensionId":$extensionId,"canvasId":$canvasId,"instanceId":$instanceId}')
    if [[ -n "$host_json" ]]; then
        json=$(echo "$json" | jq -c --argjson host "$host_json" '. + {"host":$host}')
    fi
    echo "$json"
}

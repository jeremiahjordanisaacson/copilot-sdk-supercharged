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
# Streamed output of a shell tool call (SDK protocol 4); data: {toolCallId, text, sequence,
# stream?} where stream is "stdout", "stderr", or absent. See copilot_shell_output_event_data.
COPILOT_EVENT_TOOL_SHELL_OUTPUT="tool.shell_output"
# No longer emitted by protocol 4 runtimes; still tolerated when received.
COPILOT_EVENT_TOOL_EXECUTION_PARTIAL_RESULT="tool.execution_partial_result"
# A custom tool call the SDK answers with session.tools.handlePendingToolCall
COPILOT_EVENT_EXTERNAL_TOOL_REQUESTED="external_tool.requested"

# --- Permission Mode Refusal Reason Constants ---
# Known values of the "reason" in a session.permissions.setMode result
# (see copilot_session_set_permission_mode).
COPILOT_PERMISSION_MODE_REFUSAL_MANAGED_POLICY="managed_policy"

# --- Plugin Extension Approval Types ---
# A PermissionsExtensionApproval (a user-scoped approval for one installed plugin extension;
# see copilot_client_extension_approvals_grant, _list, and _revoke) is a JSON object with:
#   extensionName     - Stable identity (for example "plugin:plugin-name:extension-name")
#   pluginName        - Plugin that contributed the extension
#   capabilities      - JSON array of the exact privileged capabilities the user approved
#   grantedAt         - RFC 3339 timestamp of the grant
#   installationKey   - Opaque installation identity; pass it back unchanged to revoke
#   installationLabel - Human-readable installation label (optional)

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
# Limits use snake_case wire keys (max_prompt_tokens, max_context_window_tokens,
# max_output_tokens). Example:
#   COPILOT_MODEL_CAPABILITIES='{"supports":{"vision":true},"limits":{"max_prompt_tokens":4096}}'
#   COPILOT_MODEL_CAPABILITIES=$(copilot_model_capabilities_limits "" "" 4096)
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

# --- JSON Helpers ---

# Echo the JSON boolean literal for a boolean string: "true" → true, anything else → false.
_copilot_json_bool() {
    if [[ "${1:-}" == "true" ]]; then
        echo "true"
    else
        echo "false"
    fi
}

# --- v2.6.0 Config Builders (parity with @github/copilot-sdk) ---

# Build a model capabilities override with a "limits" object (snake_case wire keys).
# Arguments:
#   $1 - Optional max_prompt_tokens
#   $2 - Optional max_context_window_tokens
#   $3 - Optional max_output_tokens
# Example:
#   COPILOT_MODEL_CAPABILITIES=$(copilot_model_capabilities_limits "" "" 4096)
copilot_model_capabilities_limits() {
    local max_prompt_tokens="${1:-}"
    local max_context_window_tokens="${2:-}"
    local max_output_tokens="${3:-}"

    local limits
    limits=$(jq -c -n '{}')
    if [[ -n "$max_prompt_tokens" ]]; then
        limits=$(echo "$limits" | jq -c --argjson v "$max_prompt_tokens" '. + {"max_prompt_tokens":$v}')
    fi
    if [[ -n "$max_context_window_tokens" ]]; then
        limits=$(echo "$limits" | jq -c --argjson v "$max_context_window_tokens" '. + {"max_context_window_tokens":$v}')
    fi
    if [[ -n "$max_output_tokens" ]]; then
        limits=$(echo "$limits" | jq -c --argjson v "$max_output_tokens" '. + {"max_output_tokens":$v}')
    fi
    jq -c -n --argjson limits "$limits" '{"limits":$limits}'
}

# Build a SandboxConfig JSON object (wire: sandboxConfig).
# Arguments:
#   $1 - enabled: "true"/"false" (default: "true")
#   $2 - Optional allowBypass: "true"/"false". Opt-in that lets the agent request an
#        individual command outside the sandbox through the normal permission flow.
# Optional env vars to set before calling:
#   SANDBOX_USER_POLICY  - JSON object (userPolicy)
#   SANDBOX_ADD_CWD      - "true"/"false" (addCurrentWorkingDirectory)
#   SANDBOX_MCP_SERVERS  - "true"/"false" (sandboxMcpServers)
#   SANDBOX_LSP_SERVERS  - "true"/"false" (sandboxLspServers)
#   SANDBOX_EXTENSIONS   - "true"/"false" (sandboxExtensions)
# Example:
#   COPILOT_SANDBOX_CONFIG=$(copilot_sandbox_config true true)
copilot_sandbox_config() {
    local enabled="${1:-true}"
    local allow_bypass="${2:-}"

    local json
    json=$(jq -c -n --argjson enabled "$(_copilot_json_bool "$enabled")" '{"enabled":$enabled}')
    if [[ -n "${SANDBOX_USER_POLICY:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson up "$SANDBOX_USER_POLICY" '. + {"userPolicy":$up}')
    fi
    if [[ -n "${SANDBOX_ADD_CWD:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$SANDBOX_ADD_CWD")" '. + {"addCurrentWorkingDirectory":$v}')
    fi
    if [[ -n "${SANDBOX_MCP_SERVERS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$SANDBOX_MCP_SERVERS")" '. + {"sandboxMcpServers":$v}')
    fi
    if [[ -n "${SANDBOX_LSP_SERVERS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$SANDBOX_LSP_SERVERS")" '. + {"sandboxLspServers":$v}')
    fi
    if [[ -n "${SANDBOX_EXTENSIONS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$SANDBOX_EXTENSIONS")" '. + {"sandboxExtensions":$v}')
    fi
    if [[ -n "$allow_bypass" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$allow_bypass")" '. + {"allowBypass":$v}')
    fi
    echo "$json"
}

# Build an image generation config JSON object (wire: imageGeneration).
# Arguments (each optional, "true"/"false"; empty = omitted):
#   $1 - enabled
#   $2 - watermark
#   $3 - saveToDisk
# Example:
#   COPILOT_IMAGE_GENERATION=$(copilot_image_generation_config true "" false)
copilot_image_generation_config() {
    local enabled="${1:-}"
    local watermark="${2:-}"
    local save_to_disk="${3:-}"

    local json
    json=$(jq -c -n '{}')
    if [[ -n "$enabled" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$enabled")" '. + {"enabled":$v}')
    fi
    if [[ -n "$watermark" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$watermark")" '. + {"watermark":$v}')
    fi
    if [[ -n "$save_to_disk" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$save_to_disk")" '. + {"saveToDisk":$v}')
    fi
    echo "$json"
}

# Build autopilot options JSON (wire: autopilot).
# Arguments:
#   $1 - Optional continuationEnabled ("true"/"false")
#   $2 - Optional maxContinues (integer)
# Example:
#   COPILOT_AUTOPILOT=$(copilot_autopilot_options true 5)
copilot_autopilot_options() {
    local continuation_enabled="${1:-}"
    local max_continues="${2:-}"

    local json
    json=$(jq -c -n '{}')
    if [[ -n "$continuation_enabled" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$continuation_enabled")" '. + {"continuationEnabled":$v}')
    fi
    if [[ -n "$max_continues" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$max_continues" '. + {"maxContinues":$v}')
    fi
    echo "$json"
}

# Build a BYOK provider config JSON object (wire: provider).
# Arguments:
#   $1 - Provider type (e.g. "openai", "azure", "anthropic")
#   $2 - Optional base URL
#   $3 - Optional API key
# Optional env vars to set before calling:
#   PROVIDER_MODEL_PROVIDER  - openai|anthropic|azure_openai|ollama|lm_studio|foundry_local|llama_cpp (modelProvider)
#   PROVIDER_API_KEY_COMMAND - command whose stdout is the API key (apiKeyCommand)
#   PROVIDER_WIRE_API        - wire API ("completions" or "responses") (wireApi)
#   PROVIDER_BEARER_TOKEN    - static bearer token (bearerToken)
# Example:
#   PROVIDER_MODEL_PROVIDER="ollama"
#   COPILOT_PROVIDER=$(copilot_provider_config "openai" "http://localhost:11434/v1")
copilot_provider_config() {
    local provider_type="$1"
    local base_url="${2:-}"
    local api_key="${3:-}"

    local json
    json=$(jq -c -n --arg type "$provider_type" '{"type":$type}')
    if [[ -n "$base_url" ]]; then
        json=$(echo "$json" | jq -c --arg v "$base_url" '. + {"baseUrl":$v}')
    fi
    if [[ -n "$api_key" ]]; then
        json=$(echo "$json" | jq -c --arg v "$api_key" '. + {"apiKey":$v}')
    fi
    if [[ -n "${PROVIDER_MODEL_PROVIDER:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_MODEL_PROVIDER" '. + {"modelProvider":$v}')
    fi
    if [[ -n "${PROVIDER_API_KEY_COMMAND:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_API_KEY_COMMAND" '. + {"apiKeyCommand":$v}')
    fi
    if [[ -n "${PROVIDER_WIRE_API:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_WIRE_API" '. + {"wireApi":$v}')
    fi
    if [[ -n "${PROVIDER_BEARER_TOKEN:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_BEARER_TOKEN" '. + {"bearerToken":$v}')
    fi
    echo "$json"
}

# --- v3.0.0 Config Builders (SDK protocol 4) ---

# Build a named BYOK provider connection (NamedProviderConfig) for COPILOT_PROVIDERS.
# Arguments:
#   $1 - Connection name (must not contain "/")
#   $2 - Base URL
#   $3 - Optional provider type ("openai", "azure", or "anthropic")
#   $4 - Optional API key
# Optional env vars to set before calling (same as copilot_provider_config):
#   PROVIDER_MODEL_PROVIDER, PROVIDER_API_KEY_COMMAND, PROVIDER_WIRE_API, PROVIDER_BEARER_TOKEN,
#   PROVIDER_TRANSPORT ("http" or "websockets"), PROVIDER_HEADERS (JSON object),
#   PROVIDER_AZURE_API_VERSION
# Example:
#   COPILOT_PROVIDERS="[$(copilot_named_provider_config gateway https://models.example.com/v1 openai "$KEY")]"
copilot_named_provider_config() {
    local name="$1"
    local base_url="$2"
    local provider_type="${3:-}"
    local api_key="${4:-}"

    local json
    json=$(jq -c -n --arg name "$name" --arg url "$base_url" '{"name":$name,"baseUrl":$url}')
    if [[ -n "$provider_type" ]]; then
        json=$(echo "$json" | jq -c --arg v "$provider_type" '. + {"type":$v}')
    fi
    if [[ -n "$api_key" ]]; then
        json=$(echo "$json" | jq -c --arg v "$api_key" '. + {"apiKey":$v}')
    fi
    if [[ -n "${PROVIDER_MODEL_PROVIDER:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_MODEL_PROVIDER" '. + {"modelProvider":$v}')
    fi
    if [[ -n "${PROVIDER_API_KEY_COMMAND:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_API_KEY_COMMAND" '. + {"apiKeyCommand":$v}')
    fi
    if [[ -n "${PROVIDER_WIRE_API:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_WIRE_API" '. + {"wireApi":$v}')
    fi
    if [[ -n "${PROVIDER_BEARER_TOKEN:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_BEARER_TOKEN" '. + {"bearerToken":$v}')
    fi
    if [[ -n "${PROVIDER_TRANSPORT:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_TRANSPORT" '. + {"transport":$v}')
    fi
    if [[ -n "${PROVIDER_HEADERS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$PROVIDER_HEADERS" '. + {"headers":$v}')
    fi
    if [[ -n "${PROVIDER_AZURE_API_VERSION:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$PROVIDER_AZURE_API_VERSION" '. + {"azure":{"apiVersion":$v}}')
    fi
    echo "$json"
}

# Build a BYOK model definition (ProviderModelConfig) for COPILOT_MODELS.
# Arguments:
#   $1 - Provider connection name
#   $2 - Provider-local model id (select it with "<provider>/<id>")
#   $3 - Optional modelId (well-known base model for capability lookup)
#   $4 - Optional wireModel (model or deployment name sent to the provider API)
# Optional env vars to set before calling:
#   MODEL_NAME, MODEL_MAX_PROMPT_TOKENS, MODEL_MAX_CONTEXT_WINDOW_TOKENS,
#   MODEL_MAX_OUTPUT_TOKENS, MODEL_CAPABILITIES (JSON object)
# Example:
#   COPILOT_MODELS="[$(copilot_provider_model_config gateway chat gpt-4o deployment)]"
copilot_provider_model_config() {
    local provider="$1"
    local id="$2"
    local model_id="${3:-}"
    local wire_model="${4:-}"

    local json
    json=$(jq -c -n --arg p "$provider" --arg id "$id" '{"provider":$p,"id":$id}')
    if [[ -n "$model_id" ]]; then
        json=$(echo "$json" | jq -c --arg v "$model_id" '. + {"modelId":$v}')
    fi
    if [[ -n "$wire_model" ]]; then
        json=$(echo "$json" | jq -c --arg v "$wire_model" '. + {"wireModel":$v}')
    fi
    if [[ -n "${MODEL_NAME:-}" ]]; then
        json=$(echo "$json" | jq -c --arg v "$MODEL_NAME" '. + {"name":$v}')
    fi
    if [[ -n "${MODEL_MAX_PROMPT_TOKENS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$MODEL_MAX_PROMPT_TOKENS" '. + {"maxPromptTokens":$v}')
    fi
    if [[ -n "${MODEL_MAX_CONTEXT_WINDOW_TOKENS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$MODEL_MAX_CONTEXT_WINDOW_TOKENS" '. + {"maxContextWindowTokens":$v}')
    fi
    if [[ -n "${MODEL_MAX_OUTPUT_TOKENS:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$MODEL_MAX_OUTPUT_TOKENS" '. + {"maxOutputTokens":$v}')
    fi
    if [[ -n "${MODEL_CAPABILITIES:-}" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$MODEL_CAPABILITIES" '. + {"capabilities":$v}')
    fi
    echo "$json"
}

# Build a custom tool definition (sent in tools.definitions).
# Arguments:
#   $1 - Tool name
#   $2 - Description shown to the model
#   $3 - Optional JSON Schema for the parameters (JSON object string)
#   $4 - Optional idempotent flag ("true"/"false"): repeated calls with the same arguments
#        are safe
#   $5 - Optional skip_permission flag ("true"): the runtime runs the tool without a
#        permission prompt (otherwise COPILOT_PERMISSION_HANDLER must approve it)
# Example:
#   copilot_tool_definition "lookup_order" "Look up an order" '{"type":"object"}' true
copilot_tool_definition() {
    local name="$1"
    local description="$2"
    local parameters="${3:-}"
    local idempotent="${4:-}"
    local skip_permission="${5:-}"

    local json
    json=$(jq -c -n --arg n "$name" --arg d "$description" '{"name":$n,"description":$d}')
    if [[ -n "$parameters" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$parameters" '. + {"parameters":$v}')
    fi
    if [[ -n "$idempotent" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$(_copilot_json_bool "$idempotent")" '. + {"idempotent":$v}')
    fi
    if [[ "$(_copilot_json_bool "$skip_permission")" == "true" ]]; then
        json=$(echo "$json" | jq -c '. + {"skipPermission":true}')
    fi
    echo "$json"
}

# Extract the data of a tool.shell_output event as {toolCallId, text, sequence, stream?}.
# Outputs nothing when the event is not a shell output event.
# Arguments:
#   $1 - Session event JSON object ({"type":"tool.shell_output","data":{...}})
copilot_shell_output_event_data() {
    local event="${1:-}"
    if [[ -z "$event" ]]; then
        return 0
    fi
    echo "$event" | jq -c 'select(.type == "tool.shell_output") | .data
        | {"toolCallId": .toolCallId, "text": .text, "sequence": .sequence}
          + (if .stream == null then {} else {"stream": .stream} end)' 2>/dev/null || true
}

# --- UserInput Types ---
# A userInput.request (see COPILOT_USER_INPUT_HANDLER) carries "imageAttachmentConstraints"
# ({"allowedMimeTypes","maxBytesPerImage","maxCount"}) when the runtime accepts images in the
# answer, and may carry "requestId" (the pending request ID) and "_meta" (opaque metadata).

# Build a UserInputRequest JSON object.
# Arguments:
#   $1 - Optional question
#   $2 - Optional choices JSON array
#   $3 - Optional allowFreeform JSON boolean
#   $4 - Optional imageAttachmentConstraints JSON object
#        (see copilot_user_input_image_attachment_constraints)
#   $5 - Optional requestId
#   $6 - Optional _meta JSON object
copilot_user_input_request() {
    local question="${1:-}"
    local choices="${2:-}"
    local allow_freeform="${3:-}"
    local image_attachment_constraints="${4:-}"
    local request_id="${5:-}"
    local meta="${6:-}"

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
    if [[ -n "$image_attachment_constraints" ]]; then
        json=$(echo "$json" | jq -c --argjson iac "$image_attachment_constraints" '. + {"imageAttachmentConstraints":$iac}')
    fi
    if [[ -n "$request_id" ]]; then
        json=$(echo "$json" | jq -c --arg rid "$request_id" '. + {"requestId":$rid}')
    fi
    if [[ -n "$meta" ]]; then
        json=$(echo "$json" | jq -c --argjson meta "$meta" '. + {"_meta":$meta}')
    fi
    echo "$json"
}

# Build a UserInputImageAttachmentConstraints JSON object (the image support a
# userInput.request advertises; absent when the runtime does not accept images).
# Arguments:
#   $1 - Required allowedMimeTypes JSON array (for example '["image/png","image/jpeg"]')
#   $2 - Optional maxBytesPerImage integer
#   $3 - Optional maxCount integer
copilot_user_input_image_attachment_constraints() {
    local allowed_mime_types="${1:-[]}"
    local max_bytes_per_image="${2:-}"
    local max_count="${3:-}"

    local json
    json=$(jq -c -n --argjson amt "$allowed_mime_types" '{"allowedMimeTypes":$amt}')
    if [[ -n "$max_bytes_per_image" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$max_bytes_per_image" '. + {"maxBytesPerImage":$v}')
    fi
    if [[ -n "$max_count" ]]; then
        json=$(echo "$json" | jq -c --argjson v "$max_count" '. + {"maxCount":$v}')
    fi
    echo "$json"
}

# Build a UserInputImageAttachment JSON object: an image attached to an ask_user answer. Only
# valid when the request advertised imageAttachmentConstraints; the runtime enforces the MIME
# types, size, and count. The wire "type" is always "image". Line breaks in the base64 data
# (for example from wrapped base64 output) are removed. The data is piped to jq (not passed as
# an argument), so large images are not limited by the argument size.
# Arguments:
#   $1 - Required base64-encoded image bytes
#   $2 - Required MIME type (for example "image/png")
#   $3 - Optional description
#   $4 - Optional display name
# Example:
#   image=$(copilot_user_input_image_attachment "$(base64 < shot.png)" image/png "" shot.png)
copilot_user_input_image_attachment() {
    local data="$1"
    local mime_type="$2"
    local description="${3:-}"
    local display_name="${4:-}"

    printf '%s' "$data" | tr -d '\r\n' | jq -c -R -s --arg mimeType "$mime_type" \
        --arg description "$description" --arg displayName "$display_name" \
        '{"type":"image","data":.,"mimeType":$mimeType}
         + (if $description == "" then {} else {"description":$description} end)
         + (if $displayName == "" then {} else {"displayName":$displayName} end)'
}

# Build a UserInputResponse JSON object.
# Arguments:
#   $1 - Required answer (use "" with wasFreeform true for an image-only answer)
#   $2 - Optional wasFreeform JSON boolean (default: false)
#   $3 - Optional attachments JSON array of UserInputImageAttachment objects (see
#        copilot_user_input_image_attachment); sent only when non-empty
copilot_user_input_response() {
    local answer="$1"
    local was_freeform="${2:-false}"
    local attachments="${3:-}"

    if [[ -z "$attachments" ]]; then
        jq -c -n --arg answer "$answer" --argjson wasFreeform "$was_freeform" \
            '{"answer":$answer,"wasFreeform":$wasFreeform}'
        return
    fi

    printf '%s' "$attachments" | jq -c --arg answer "$answer" --argjson wasFreeform "$was_freeform" \
        '{"answer":$answer,"wasFreeform":$wasFreeform}
         + (if length > 0 then {"attachments":.} else {} end)'
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

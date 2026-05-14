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

# --- Session Telemetry ---
# When set to "true", enables session telemetry in create/resume payload
COPILOT_ENABLE_SESSION_TELEMETRY=""

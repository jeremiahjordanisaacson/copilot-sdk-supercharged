/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file session.c
 * @brief CopilotSession supplementary implementation.
 *
 * The core session implementation lives in client.c because the session struct
 * is defined there alongside the client (for mutual access). This file provides
 * supplementary session utility functions that may be extended in the future.
 *
 * Currently this module provides:
 *   - Session event type string constants
 *   - Helper functions for working with session events
 */

#include "copilot/copilot.h"
#include <string.h>
#include <stdlib.h>

/* ============================================================================
 * Well-known session event type constants
 * ============================================================================ */

/* These match the event type strings sent by the Copilot CLI server */

const char *const COPILOT_EVENT_ASSISTANT_MESSAGE       = "assistant.message";
const char *const COPILOT_EVENT_ASSISTANT_MESSAGE_DELTA = "assistant.message_delta";
const char *const COPILOT_EVENT_ASSISTANT_REASONING_DELTA = "assistant.reasoning_delta";
const char *const COPILOT_EVENT_SESSION_IDLE            = "session.idle";
const char *const COPILOT_EVENT_SESSION_ERROR           = "session.error";
const char *const COPILOT_EVENT_SESSION_RESUME          = "session.resume";
const char *const COPILOT_EVENT_TOOL_EXECUTING          = "tool.executing";
const char *const COPILOT_EVENT_TOOL_EXECUTED           = "tool.executed";
const char *const COPILOT_EVENT_TOOL_CALLED             = "tool.called";
const char *const COPILOT_EVENT_USER_MESSAGE            = "user.message";
const char *const COPILOT_EVENT_COMPACTION_START        = "compaction.start";
const char *const COPILOT_EVENT_COMPACTION_COMPLETE     = "compaction.complete";

/* ============================================================================
 * Session event helpers
 * ============================================================================ */

/**
 * Checks if a session event is of the given type.
 *
 * @param event  The event to check.
 * @param type   The type string to compare against.
 * @return true if the event type matches.
 */
bool copilot_session_event_is_type(const copilot_session_event_t *event, const char *type)
{
    if (!event || !event->type || !type) return false;
    return strcmp(event->type, type) == 0;
}

/**
 * Checks if a session event is an assistant message.
 */
bool copilot_session_event_is_assistant_message(const copilot_session_event_t *event)
{
    return copilot_session_event_is_type(event, COPILOT_EVENT_ASSISTANT_MESSAGE);
}

/**
 * Checks if a session event indicates the session is idle.
 */
bool copilot_session_event_is_idle(const copilot_session_event_t *event)
{
    return copilot_session_event_is_type(event, COPILOT_EVENT_SESSION_IDLE);
}

/**
 * Checks if a session event is an error.
 */
bool copilot_session_event_is_error(const copilot_session_event_t *event)
{
    return copilot_session_event_is_type(event, COPILOT_EVENT_SESSION_ERROR);
}

/**
 * Checks if a session event is a streaming message delta.
 */
bool copilot_session_event_is_delta(const copilot_session_event_t *event)
{
    return copilot_session_event_is_type(event, COPILOT_EVENT_ASSISTANT_MESSAGE_DELTA);
}

// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Core type definitions

module copilot

import json

// ConnectionState tracks whether the client is connected to the CLI.
pub enum ConnectionState {
	disconnected
	connecting
	connected
	error
}

// CopilotClientOptions configures the CopilotClient.
pub struct CopilotClientOptions {
pub mut:
	cli_path  string // path to the Copilot CLI binary
	cli_url   string // URL of an already-running CLI server (host:port)
	log_level string = 'error' // log level: error, warn, info, debug
}

// SessionConfig defines how to create a new conversation session.
pub struct SessionConfig {
pub mut:
	model                 string // model name, e.g. "gpt-4" or "gpt-5"
	system_message        string // custom system prompt
	tools                 []Tool // tools available to the model
	streaming             bool = true // receive streaming delta events
	agent                 string // agent name for scoped behaviour
	history               []HistoryEntry // conversation history to seed the session
	on_permission_request fn (PermissionRequest) PermissionResponse = unsafe { nil }
}

// HistoryEntry is a single turn in a conversation.
pub struct HistoryEntry {
pub:
	role    string [json: 'role']    // "user" or "assistant"
	content string [json: 'content'] // message text
}

// SendOptions describes a message to send into a session.
pub struct SendOptions {
pub mut:
	prompt      string            // the user message text
	attachments []Attachment      // optional file attachments
	mode        string            // optional mode override
}

// Attachment represents a file attached to a message.
pub struct Attachment {
pub:
	file_path string [json: 'filePath']
	language  string [json: 'language']
	content   string [json: 'content']
}

// SessionEvent is the envelope for every event received from the server.
pub struct SessionEvent {
pub:
	event_type string [json: 'type'] // e.g. "assistant.message", "assistant.message_delta"
	raw_data   string [json: 'data'] // raw JSON of the data payload
}

// MessageData holds a completed assistant message.
pub struct MessageData {
pub:
	content string [json: 'content']
	role    string [json: 'role']
}

// DeltaData holds a streaming chunk of assistant output.
pub struct DeltaData {
pub:
	content string [json: 'content']
}

// ToolCallRequest is sent by the server when it wants to invoke a tool.
pub struct ToolCallRequest {
pub:
	id        string [json: 'id']
	name      string [json: 'name']
	arguments string [json: 'arguments'] // JSON-encoded arguments
}

// ToolCallResponse is sent back to the server after executing a tool.
pub struct ToolCallResponse {
pub:
	id     string [json: 'id']
	result string [json: 'result']
	error  string [json: 'error']
}

// PermissionRequest is emitted when the model requests elevated permissions.
pub struct PermissionRequest {
pub:
	resource    string [json: 'resource']
	action      string [json: 'action']
	description string [json: 'description']
}

// PermissionResponse is the answer to a permission request.
pub struct PermissionResponse {
pub:
	approved bool   [json: 'approved']
	reason   string [json: 'reason']
}

// ModelInfo describes a model the server can use.
pub struct ModelInfo {
pub:
	name    string [json: 'name']
	version string [json: 'version']
}

// SessionMetadata holds server-assigned identifiers for a session.
pub struct SessionMetadata {
pub:
	session_id string [json: 'sessionId']
	model      string [json: 'model']
}

// CopilotError is a typed error returned by SDK operations.
pub struct CopilotError {
	Error
pub:
	kind    ErrorKind
	message string
	source  string
}

pub fn (e CopilotError) msg() string {
	return '${e.kind}: ${e.message} (${e.source})'
}

// ErrorKind classifies errors.
pub enum ErrorKind {
	connection_failed
	protocol_error
	timeout
	session_error
	tool_error
	permission_denied
	unknown
}

// approve_all is a convenience permission handler that approves everything.
pub fn approve_all(req PermissionRequest) PermissionResponse {
	return PermissionResponse{
		approved: true
		reason: 'auto-approved'
	}
}

// deny_all is a convenience permission handler that denies everything.
pub fn deny_all(req PermissionRequest) PermissionResponse {
	return PermissionResponse{
		approved: false
		reason: 'auto-denied'
	}
}

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
	cli_path                      string // path to the Copilot CLI binary
	cli_url                       string // URL of an already-running CLI server (host:port)
	log_level                     string = 'error' // log level: error, warn, info, debug
	github_token                  string // GitHub token for authentication
	use_logged_in_user            bool = true // use the currently logged-in user
	session_idle_timeout_seconds  int    // idle timeout in seconds
	session_fs                    SessionFsConfig // session filesystem config
	copilot_home                  string // override path to the Copilot home directory
	tcp_connection_token          string // token for TCP connection authentication
	remote                        bool   // connect to remote CLI server
	on_get_trace_context          fn () TraceContext = unsafe { nil }
}

// SessionConfig defines how to create a new conversation session.
pub struct SessionConfig {
pub mut:
	model                               string // model name, e.g. "gpt-4" or "gpt-5"
	system_message                      string // custom system prompt
	tools                               []Tool // tools available to the model
	streaming                           bool = true // receive streaming delta events
	agent                               string // agent name for scoped behaviour
	history                             []HistoryEntry // conversation history to seed the session
	on_permission_request               fn (PermissionRequest) PermissionResponse = unsafe { nil }
	excluded_tools                      []string // tools to exclude
	mcp_servers                         map[string]McpServerConfig // MCP server configs
	model_capabilities                  map[string]string // model capabilities override
	enable_config_discovery             bool // auto-discover config
	include_sub_agent_streaming_events  bool // include sub-agent streaming events
	commands                            []CommandDefinition // command definitions
	skill_directories                   []string // skill directories
	disabled_skills                     []string // disabled skills
	working_directory                   string // working directory
	github_token                        string // per-session auth token
	reasoning_effort                    string // reasoning effort level
	response_format                     string // response format: "text", "image", "json_object"
	request_headers                     map[string]string // additional request headers
	elicitation_handler                 fn (map[string]string) map[string]string = unsafe { nil }
	idle_timeout                        int    // session idle timeout in seconds
	instruction_directories             []string // directories to search for instruction files
	enable_session_telemetry            bool // enable session telemetry
	exit_plan_mode_handler              fn (ExitPlanModeRequest) ExitPlanModeResponse = unsafe { nil }
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
	event_type string [json: 'type']
	raw_data   string [json: 'data']
	id         string [json: 'id']
	timestamp  string [json: 'timestamp']
	parent_id  string [json: 'parentId']
	agent_id   string [json: 'agentId']
	ephemeral  bool   [json: 'ephemeral']
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

// SlashCommandInputCompletion is the completion type for slash command inputs.
pub enum SlashCommandInputCompletion {
	directory
}

// SlashCommandKind is the kind of slash command.
pub enum SlashCommandKind {
	builtin
	client
	skill
}

// ModelPickerPriceCategory is the price category for model picker.
pub enum ModelPickerPriceCategory {
	high
	low
	medium
	very_high
}

// SlashCommandInput defines the input for a slash command.
pub struct SlashCommandInput {
pub:
	hint       string                       [json: 'hint']
	completion SlashCommandInputCompletion   [json: 'completion']
}

// SlashCommandInfo describes a slash command.
pub struct SlashCommandInfo {
pub:
	allow_during_agent_execution bool                       [json: 'allowDuringAgentExecution']
	description                  string                     [json: 'description']
	kind                         SlashCommandKind           [json: 'kind']
	name                         string                     [json: 'name']
	aliases                      []string                   [json: 'aliases']
	experimental                 bool                       [json: 'experimental']
	input                        SlashCommandInput          [json: 'input']
}

// CommandsInvokeRequest is a request to invoke a command.
pub struct CommandsInvokeRequest {
pub:
	name  string [json: 'name']
	input string [json: 'input']
}

// CommandsListRequest is a request to list available commands.
pub struct CommandsListRequest {
pub:
	include_builtins         bool [json: 'includeBuiltins']
	include_client_commands  bool [json: 'includeClientCommands']
	include_skills           bool [json: 'includeSkills']
}

// ModelBillingTokenPrices holds token pricing information.
pub struct ModelBillingTokenPrices {
pub:
	batch_size   int [json: 'batchSize']
	cache_price  int [json: 'cachePrice']
	input_price  int [json: 'inputPrice']
	output_price int [json: 'outputPrice']
}

// Experimental: SkillsLoadDiagnostics holds diagnostics from loading skills.
pub struct SkillsLoadDiagnostics {
pub:
	errors   []string [json: 'errors']
	warnings []string [json: 'warnings']
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

// SessionFsConfig describes the session filesystem provider.
pub struct SessionFsConfig {
pub:
	initial_cwd        string [json: 'initialCwd']
	session_state_path string [json: 'sessionStatePath']
	conventions        string [json: 'conventions'] // "windows" or "posix"
}

// McpServerType indicates how to connect to an MCP server.
pub enum McpServerType {
	stdio
	http
}

// McpServerConfig describes an MCP server connection.
pub struct McpServerConfig {
pub:
	server_type McpServerType     [json: 'type']
	command     string            [json: 'command']
	args        []string          [json: 'args']
	url         string            [json: 'url']
	env         map[string]string [json: 'env']
	headers     map[string]string [json: 'headers']
}

// CommandDefinition describes a command that can be invoked.
pub struct CommandDefinition {
pub:
	name        string [json: 'name']
	description string [json: 'description']
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

// ExitPlanModeRequest is sent when the model requests to exit plan mode.
pub struct ExitPlanModeRequest {
pub:
	session_id string [json: 'sessionId']
}

// ExitPlanModeResponse is the answer to an exit-plan-mode request.
pub struct ExitPlanModeResponse {
pub:
	approved bool [json: 'approved']
}

// TraceContext provides W3C Trace Context headers for distributed tracing.
pub struct TraceContext {
pub:
	traceparent string [json: 'traceparent']
	tracestate  string [json: 'tracestate']
}

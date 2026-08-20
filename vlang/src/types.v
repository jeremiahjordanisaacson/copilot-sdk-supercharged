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
	builtin_plugin_directories    []string // built-in plugin directories (wire: builtinPluginDirectories)
	in_process                    bool   // use in-process FFI transport (wire: inProcess)
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
	// --- Upstream-sync session configuration (parity with @github/copilot-sdk) ---
	enable_citations                    bool // emit inline source citations (wire: enableCitations)
	excluded_builtin_agents             []string // built-in agents to exclude (wire: excludedBuiltinAgents)
	session_limits                      SessionLimitsConfig // per-session spend/credit limits (wire: sessionLimits)
	memory                              MemoryConfiguration // persistent session memory (wire: memory)
	otlp_protocol                       string // OTLP telemetry protocol, e.g. "grpc" (wire: otlpProtocol)
	enable_web_socket_responses         bool // stream responses over a WebSocket (wire: enableWebSocketResponses)
	exp_assignments                     map[string]string // experiment assignment overrides (wire: expAssignments)
	on_mcp_auth_request                 McpAuthHandler = unsafe { nil } // MCP OAuth token handler
	bearer_token_provider               BearerTokenProvider = unsafe { nil } // BYOK bearer token provider
	on_post_tool_use                    PostToolUseHandler = unsafe { nil } // post-tool-use hook
	on_pre_mcp_tool_call                PreMcpToolCallHandler = unsafe { nil } // pre-MCP-tool-call hook
	request_handler                     CopilotRequestHandler = unsafe { nil } // custom HTTP request handler
	// --- Additional upstream-sync session configuration (2026-08) ---
	rewind_enabled                      bool // enable session rewind (wire: rewindEnabled)
	additional_directories              []string // extra session directories (wire: additionalDirectories)
	disabled_mcp_servers                []string // disabled MCP servers (wire: disabledMcpServers)
	github_mcp_tool_config              map[string]string // GitHub MCP tool config (wire: githubMcpToolConfig)
	canvas_provider                     map[string]string // canvas provider config (wire: canvasProvider)
	custom_agents_local_only            bool // restrict custom agents to local (wire: customAgentsLocalOnly)
	tool_search                         map[string]string // tool search configuration (wire: toolSearch)
	experimental_mode                   bool // enable experimental mode (wire: experimentalMode)
	content_exclusion                   bool // enable content exclusion (wire: contentExclusion)
	on_user_prompt_transformed          UserPromptTransformedHandler = unsafe { nil } // user-prompt-transformed hook (wire: userPromptTransformed)
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
	prompt          string            // the user message text
	attachments     []Attachment      // optional file attachments
	mode            string            // optional mode override
	agent_mode      string            // agent mode override for this message (wire: agentMode)
	display_prompt  string            // alternate prompt shown in the UI (wire: displayPrompt)
	request_headers map[string]string // per-message request headers (wire: requestHeaders)
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
	approved         bool              [json: 'approved']
	reason           string            [json: 'reason']
	decision_context map[string]string [json: 'decisionContext'] // opaque decision context
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

// ---------------------------------------------------------------------------
// Upstream-sync feature types & constants (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

// SessionLimitsConfig caps resource usage for a single session.
pub struct SessionLimitsConfig {
pub:
	max_ai_credits f64 [json: 'maxAiCredits'] // maximum AI credits to spend
	max_requests   int [json: 'maxRequests']  // maximum number of model requests
	max_tokens     int [json: 'maxTokens']    // maximum number of tokens
}

// MemoryConfiguration controls persistent, cross-turn session memory.
pub struct MemoryConfiguration {
pub:
	enabled     bool   [json: 'enabled']   // enable persistent session memory
	max_entries int    [json: 'maxEntries'] // maximum retained memory entries
	directory   string [json: 'directory'] // directory used for memory storage
}

// ProviderTokenArgs is passed to a BearerTokenProvider (BYOK) callback.
pub struct ProviderTokenArgs {
pub:
	provider string [json: 'provider'] // provider identifier, e.g. "azure-openai"
	endpoint string [json: 'endpoint'] // provider endpoint URL
	scope    string [json: 'scope']    // requested token scope
}

// McpAuthRequest is delivered to the MCP OAuth token handler.
pub struct McpAuthRequest {
pub:
	server_name string   [json: 'serverName'] // MCP server requesting authorization
	auth_url    string   [json: 'authUrl']    // OAuth authorization URL
	scopes      []string [json: 'scopes']     // requested OAuth scopes
}

// McpAuthResponse is returned by the MCP OAuth token handler.
pub struct McpAuthResponse {
pub:
	token string [json: 'token'] // bearer token to present to the MCP server
}

// PostToolUsePayload is delivered to the post-tool-use hook.
pub struct PostToolUsePayload {
pub:
	tool_name string [json: 'toolName'] // name of the tool that ran
	result    string [json: 'result']   // JSON-encoded tool result
}

// PreMcpToolCallPayload is delivered to the pre-MCP-tool-call hook.
pub struct PreMcpToolCallPayload {
pub:
	server_name string [json: 'serverName'] // MCP server name
	tool_name   string [json: 'toolName']   // tool about to be invoked
	arguments   string [json: 'arguments']  // JSON-encoded call arguments
}

// UserPromptTransformedPayload is delivered to the user-prompt-transformed hook.
pub struct UserPromptTransformedPayload {
pub:
	original_prompt    string [json: 'originalPrompt']    // the user's original prompt
	transformed_prompt string [json: 'transformedPrompt'] // the transformed prompt
}

// CopilotHttpRequest is passed to a custom HTTP request handler.
pub struct CopilotHttpRequest {
pub:
	method  string            [json: 'method']  // HTTP method
	url     string            [json: 'url']     // request URL
	headers map[string]string [json: 'headers'] // request headers
	body    string            [json: 'body']    // request body
}

// CopilotHttpResponse is returned by a custom HTTP request handler.
pub struct CopilotHttpResponse {
pub:
	status  int               [json: 'status']  // HTTP status code
	headers map[string]string [json: 'headers'] // response headers
	body    string            [json: 'body']    // response body
}

// GitHubAttachment references GitHub content attached to a message.
pub struct GitHubAttachment {
pub:
	kind string [json: 'kind'] // one of the github_*_attachment variants below
	ref  string [json: 'ref']  // the reference value (SHA, owner/repo, number, ...)
}

// SystemMessageSection is a named, individually-configurable part of the
// system message (e.g. a preamble that is preserved across compaction).
pub struct SystemMessageSection {
pub:
	name     string [json: 'name']     // section name, e.g. "preamble"
	content  string [json: 'content']  // section content
	preserve bool   [json: 'preserve'] // preserve this section across compaction
}

// McpAuthHandler resolves an OAuth bearer token for an MCP server (MCP OAuth).
pub type McpAuthHandler = fn (McpAuthRequest) McpAuthResponse

// BearerTokenProvider supplies a bearer token for BYOK model providers.
pub type BearerTokenProvider = fn (ProviderTokenArgs) string

// PostToolUseHandler is invoked after a tool has finished executing.
pub type PostToolUseHandler = fn (PostToolUsePayload)

// PreMcpToolCallHandler is invoked before an MCP tool call is dispatched.
pub type PreMcpToolCallHandler = fn (PreMcpToolCallPayload)

// UserPromptTransformedHandler is invoked after the user prompt is transformed.
pub type UserPromptTransformedHandler = fn (UserPromptTransformedPayload)

// CopilotRequestHandler intercepts outbound Copilot HTTP requests.
pub type CopilotRequestHandler = fn (CopilotHttpRequest) CopilotHttpResponse

// AgentFactoryOptions holds authoring options for a programmatic agent factory.
pub struct AgentFactoryOptions {
pub:
	args_schema map[string]string [json: 'argsSchema'] // JSON schema for factory arguments
}

// System message section name constants.
pub const system_message_preamble = 'preamble'
pub const system_message_preserve = 'preserve'

// Tool defer-loading strategy constants.
pub const tool_defer_eager = 'eager'
pub const tool_defer_lazy = 'lazy'

// GitHub attachment variant identifiers.
pub const github_commit_attachment = 'GitHubCommit'
pub const github_repository_attachment = 'GitHubRepository'
pub const github_pull_request_attachment = 'GitHubPullRequest'
pub const github_issue_attachment = 'GitHubIssue'

// OTLP telemetry protocol constants.
pub const otlp_protocol_grpc = 'grpc'
pub const otlp_protocol_http = 'http/protobuf'

// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Core type definitions

module copilot

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
	mode                          ClientMode // .coding_agent (default) or .empty
	client_info                   ClientInfo // integrating application identity (wire: clientInfo on connect)
	network                       NetworkOptions // proxy callbacks registered with network.setProvider
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
	exit_plan_mode_handler              ExitPlanModeHandler = unsafe { nil } // answers exit-plan-mode requests
	// --- v3.0.0 / SDK protocol 4 session options ---
	available_tools                     ?[]string // tool allowlist; required on every session in .empty client mode
	ask_user_variant                    string // 'legacy' or 'elicitation' (wire: interaction.askUserVariant)
	github_token_provider               GitHubTokenProvider = unsafe { nil } // answers gitHubToken.getToken; exclusive with github_token
	sandbox                             ?SandboxConfig // wire: permissions.sandbox
	feature_flags                       map[string]bool // runtime feature flag overrides (wire: experiments.flags)
	allowed_models                      []string // model allowlist (wire: policy.allowedModels)
	auth_client_id_metadata_url         string // MCP OAuth client metadata URL (wire: mcp.authClientIdMetadataUrl)
	model_capabilities_override         ?ModelCapabilitiesOverride // typed capabilities override (wire: inference.capabilities)
	agent_directories                   []string // wire: agents.directories
	hook_directories                    []string // wire: hooks.directories
	trust_working_directory             string // trust_working_directory_session or _user; create only (wire: filesystem.trustWorkingDirectory)
	image_generation                    ?ImageGenerationConfig // wire: tools.imageGeneration
	autopilot                           ?AutopilotConfig // wire: execution.autopilot
	hooks                               map[string]HookHandler // hook handlers keyed by hook type
	provider                            ?ProviderConfig // legacy single BYOK provider
	providers                           []NamedProviderConfig // wire: inference.providers
	models                              []ProviderModelConfig // wire: inference.models
	default_provider                    string // wire: inference.defaultProvider
	capi                                ?CapiConfig // wire: github.capi
	on_shell_authentication_request     ShellAuthenticationHandler = unsafe { nil } // answers shellAuthentication.request
	on_store_memory                     StoreMemoryHandler = unsafe { nil } // answers memoryStore.store
	on_user_input_request               UserInputHandler = unsafe { nil } // answers userInput.request, including image answers (wire: interaction.requestUserInput)
}

// ResumeSessionOptions configures `resume_session_with`.
pub struct ResumeSessionOptions {
pub mut:
	session_id                string // session to resume
	config                    SessionConfig // options re-supplied on resume (create-only options are not sent)
	disable_resume            bool // suppress the resume event (wire: suppressResumeEvent)
	allow_transcript_recovery bool // allow recovery from the transcript (wire: allowTranscriptRecovery)
	clear_default_provider    bool // send an explicit null default provider
}

// HistoryEntry is a single turn in a conversation.
pub struct HistoryEntry {
pub:
	role    string @[json: 'role']    // "user" or "assistant"
	content string @[json: 'content'] // message text
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
	source          string            // message source: message_source_user, message_source_system, or agent_message_source(id)
	client_turn_id  string            // opaque caller turn id (wire: clientTurnId)
	response_schema string            // JSON Schema (raw JSON) for structured output (wire: responseFormat)
}

// Attachment represents a file attached to a message.
pub struct Attachment {
pub:
	file_path string @[json: 'filePath']
	language  string @[json: 'language']
	content   string @[json: 'content']
}

// SessionEvent is the envelope for every event received from the server.
pub struct SessionEvent {
pub:
	event_type string @[json: 'type']
	raw_data   string @[json: 'data']
	id         string @[json: 'id']
	timestamp  string @[json: 'timestamp']
	parent_id  string @[json: 'parentId']
	agent_id   string @[json: 'agentId']
	ephemeral  bool   @[json: 'ephemeral']
}

// MessageData holds a completed assistant message.
pub struct MessageData {
pub:
	content string @[json: 'content']
	role    string @[json: 'role']
}

// DeltaData holds a streaming chunk of assistant output.
pub struct DeltaData {
pub:
	content string @[json: 'content']
}

// ToolCallRequest is sent by the server when it wants to invoke a tool.
pub struct ToolCallRequest {
pub:
	id        string @[json: 'id']
	name      string @[json: 'name']
	arguments string @[json: 'arguments'] // JSON-encoded arguments
}

// ToolCallResponse is sent back to the server after executing a tool.
pub struct ToolCallResponse {
pub:
	id     string @[json: 'id']
	result string @[json: 'result']
	error  string @[json: 'error']
}

// PermissionRequest is emitted when the model requests elevated permissions.
pub struct PermissionRequest {
pub:
	resource    string @[json: 'resource']
	action      string @[json: 'action']
	description string @[json: 'description']
}

// PermissionResponse is the answer to a permission request.
pub struct PermissionResponse {
pub:
	approved         bool              @[json: 'approved']
	reason           string            @[json: 'reason']
	decision_context map[string]string @[json: 'decisionContext'] // opaque decision context
}

// ModelInfo describes a model the server can use.
pub struct ModelInfo {
pub:
	name    string @[json: 'name']
	version string @[json: 'version']
}

// SessionMetadata holds server-assigned identifiers for a session.
pub struct SessionMetadata {
pub:
	session_id string @[json: 'sessionId']
	model      string @[json: 'model']
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
	initial_cwd        string @[json: 'initialCwd']
	session_state_path string @[json: 'sessionStatePath']
	conventions        string @[json: 'conventions'] // "windows" or "posix"
}

// McpServerType indicates how to connect to an MCP server.
pub enum McpServerType {
	stdio
	http
}

// McpServerConfig describes an MCP server connection.
pub struct McpServerConfig {
pub:
	server_type McpServerType     @[json: 'type']
	command     string            @[json: 'command']
	args        []string          @[json: 'args']
	url         string            @[json: 'url']
	env         map[string]string @[json: 'env']
	headers     map[string]string @[json: 'headers']
}

// CommandDefinition describes a command that can be invoked.
pub struct CommandDefinition {
pub:
	name        string @[json: 'name']
	description string @[json: 'description']
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
	max_ai_credits f64 @[json: 'maxAiCredits'] // maximum AI credits to spend
	max_requests   int @[json: 'maxRequests']  // maximum number of model requests
	max_tokens     int @[json: 'maxTokens']    // maximum number of tokens
}

// MemoryConfiguration controls persistent, cross-turn session memory.
pub struct MemoryConfiguration {
pub:
	enabled     bool   @[json: 'enabled']   // enable persistent session memory
	max_entries int    @[json: 'maxEntries'] // maximum retained memory entries
	directory   string @[json: 'directory'] // directory used for memory storage
}

// ProviderTokenArgs is passed to a BearerTokenProvider (BYOK) callback.
pub struct ProviderTokenArgs {
pub:
	provider string @[json: 'provider'] // provider identifier, e.g. "azure-openai"
	endpoint string @[json: 'endpoint'] // provider endpoint URL
	scope    string @[json: 'scope']    // requested token scope
}

// McpAuthRequest is delivered to the MCP OAuth token handler.
pub struct McpAuthRequest {
pub:
	server_name string   @[json: 'serverName'] // MCP server requesting authorization
	auth_url    string   @[json: 'authUrl']    // OAuth authorization URL
	scopes      []string @[json: 'scopes']     // requested OAuth scopes
}

// McpAuthResponse is returned by the MCP OAuth token handler.
pub struct McpAuthResponse {
pub:
	token string @[json: 'token'] // bearer token to present to the MCP server
}

// PostToolUsePayload is delivered to the post-tool-use hook.
pub struct PostToolUsePayload {
pub:
	tool_name string @[json: 'toolName'] // name of the tool that ran
	result    string @[json: 'result']   // JSON-encoded tool result
}

// PreMcpToolCallPayload is delivered to the pre-MCP-tool-call hook.
pub struct PreMcpToolCallPayload {
pub:
	server_name string @[json: 'serverName'] // MCP server name
	tool_name   string @[json: 'toolName']   // tool about to be invoked
	arguments   string @[json: 'arguments']  // JSON-encoded call arguments
}

// UserPromptTransformedPayload is delivered to the user-prompt-transformed hook.
pub struct UserPromptTransformedPayload {
pub:
	original_prompt    string @[json: 'originalPrompt']    // the user's original prompt
	transformed_prompt string @[json: 'transformedPrompt'] // the transformed prompt
}

// CopilotHttpRequest is passed to a custom HTTP request handler.
pub struct CopilotHttpRequest {
pub:
	method  string            @[json: 'method']  // HTTP method
	url     string            @[json: 'url']     // request URL
	headers map[string]string @[json: 'headers'] // request headers
	body    string            @[json: 'body']    // request body
}

// CopilotHttpResponse is returned by a custom HTTP request handler.
pub struct CopilotHttpResponse {
pub:
	status  int               @[json: 'status']  // HTTP status code
	headers map[string]string @[json: 'headers'] // response headers
	body    string            @[json: 'body']    // response body
}

// GitHubAttachment references GitHub content attached to a message.
pub struct GitHubAttachment {
pub:
	kind string @[json: 'kind'] // one of the github_*_attachment variants below
	ref  string @[json: 'ref']  // the reference value (SHA, owner/repo, number, ...)
}

// SystemMessageSection is a named, individually-configurable part of the
// system message (e.g. a preamble that is preserved across compaction).
pub struct SystemMessageSection {
pub:
	name     string @[json: 'name']     // section name, e.g. "preamble"
	content  string @[json: 'content']  // section content
	preserve bool   @[json: 'preserve'] // preserve this section across compaction
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

// ExitPlanModeRequest asks the client to approve leaving plan mode.
pub struct ExitPlanModeRequest {
pub:
	session_id string @[json: 'sessionId']
}

// ExitPlanModeResponse answers an ExitPlanModeRequest.
pub struct ExitPlanModeResponse {
pub:
	approved bool   @[json: 'approved']
	feedback string @[json: 'feedback']
}

// ExitPlanModeHandler decides whether to leave plan mode.
pub type ExitPlanModeHandler = fn (ExitPlanModeRequest) ExitPlanModeResponse

// AgentFactoryOptions holds authoring options for a programmatic agent factory.
pub struct AgentFactoryOptions {
pub:
	args_schema map[string]string @[json: 'argsSchema'] // JSON schema for factory arguments
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

// ---------------------------------------------------------------------------
// SDK protocol 4 / v3.0.0 feature types
// ---------------------------------------------------------------------------

// ClientMode selects the client behavior profile.
pub enum ClientMode {
	coding_agent // default: the 'coding-agent' behavior
	empty        // requires available_tools on every session and applies conservative defaults
}

// wire_name returns the upstream mode name ('coding-agent' or 'empty').
pub fn (m ClientMode) wire_name() string {
	return match m {
		.coding_agent { 'coding-agent' }
		.empty { 'empty' }
	}
}

// parse_client_mode parses a mode name. 'copilot-cli' is an alias of 'coding-agent'.
pub fn parse_client_mode(name string) !ClientMode {
	return match name {
		'', 'coding-agent', 'copilot-cli' { ClientMode.coding_agent }
		'empty' { ClientMode.empty }
		else { error('unknown client mode: ${name}') }
	}
}

// ClientInfo identifies the integrating application. It is sent as
// `clientInfo` on the `connect` handshake; empty fields are dropped.
pub struct ClientInfo {
pub mut:
	editor_name       string
	editor_version    string
	extension_name    string
	extension_version string
}

// Auto routing tiers for `CopilotSession.switch_auto_tier`.
pub const auto_tier_fast = 'fast'
pub const auto_tier_efficiency = 'efficiency'
pub const auto_tier_balance = 'balance'
pub const auto_tier_intelligence = 'intelligence'

// Ask-user variants for `SessionConfig.ask_user_variant`.
pub const ask_user_variant_legacy = 'legacy'
pub const ask_user_variant_elicitation = 'elicitation'

// Working directory trust scopes for `SessionConfig.trust_working_directory`.
pub const trust_working_directory_session = 'session'
pub const trust_working_directory_user = 'user'

// MessageSource values for `SendOptions.source`.
pub const message_source_user = 'user'
pub const message_source_system = 'system'

// agent_message_source returns the message source for the agent with the given id.
pub fn agent_message_source(agent_id string) string {
	return 'agent-${agent_id}'
}

// Hook types for the subagent lifecycle hooks.
pub const hook_subagent_start = 'subagentStart'
pub const hook_subagent_stop = 'subagentStop'

// permission_mode_refusal_managed_policy is the known `PermissionModeResult.reason`.
pub const permission_mode_refusal_managed_policy = 'managed_policy'

// event_tool_shell_output is the event type for live shell output chunks.
pub const event_tool_shell_output = 'tool.shell_output'

// GitHubTokenRequest is a `gitHubToken.getToken` request from the runtime.
pub struct GitHubTokenRequest {
pub:
	registration_id string
	host            string
	session_id      string
	reason          string
}

// GitHubTokenResult answers a GitHubTokenRequest. Leave `access_token` empty
// (or set `cancelled`) to decline.
pub struct GitHubTokenResult {
pub:
	access_token string
	token_type   string
	expires_in   int // remaining lifetime in seconds
	cancelled    bool
}

// GitHubTokenProvider supplies GitHub tokens on demand.
pub type GitHubTokenProvider = fn (GitHubTokenRequest) GitHubTokenResult

// SandboxConfig is sent as `permissions.sandbox`.
pub struct SandboxConfig {
pub mut:
	enabled                       bool
	allow_bypass                  bool // let the agent ask to run a command outside the sandbox
	add_current_working_directory ?bool
	sandbox_mcp_servers           ?bool
	user_policy                   string // optional raw JSON policy fragment
}

// ModelCapabilitiesOverride is sent as `inference.capabilities`.
pub struct ModelCapabilitiesOverride {
pub mut:
	supports_vision           ?bool
	supports_tool_calls       ?bool
	supports_reasoning_effort ?bool
	max_prompt_tokens         int
	max_context_window_tokens int
	max_output_tokens         int // sent as limits.max_output_tokens
}

// ImageGenerationConfig is sent as `tools.imageGeneration`.
pub struct ImageGenerationConfig {
pub mut:
	enabled      ?bool
	watermark    ?bool
	save_to_disk ?bool
}

// AutopilotConfig is sent as `execution.autopilot`.
pub struct AutopilotConfig {
pub mut:
	continuation_enabled ?bool
	max_continues        ?int
}

// AzureProviderOptions holds Azure-specific provider settings.
pub struct AzureProviderOptions {
pub mut:
	api_version string
}

// ProviderConfig is the legacy single BYOK provider. On the protocol 4 wire
// it becomes the named "default" provider with Copilot models disabled.
pub struct ProviderConfig {
pub mut:
	provider_type             string // wire: type
	base_url                  string
	api_key                   string
	api_key_command           string // command run before each request to get a fresh key
	bearer_token              string
	wire_api                  string
	transport                 string
	azure                     AzureProviderOptions
	headers                   map[string]string
	model_provider            string
	model_id                  string
	wire_model                string
	max_prompt_tokens         int
	max_context_window_tokens int
	max_output_tokens         int
}

// NamedProviderConfig is a named BYOK provider connection (`inference.providers`).
pub struct NamedProviderConfig {
pub mut:
	name            string
	provider_type   string // wire: type
	base_url        string
	api_key         string
	api_key_command string
	bearer_token    string
	wire_api        string
	transport       string
	azure           AzureProviderOptions
	headers         map[string]string
	model_provider  string
}

// ProviderModelConfig is a BYOK model served by a named provider
// (`inference.models`). Select it with the model id 'provider/id'.
pub struct ProviderModelConfig {
pub mut:
	provider                  string
	id                        string
	model_id                  string
	wire_model                string
	name                      string
	max_prompt_tokens         int
	max_context_window_tokens int
	max_output_tokens         int
	capabilities              ?ModelCapabilitiesOverride
}

// CapiConfig controls Copilot model availability (`github.capi`).
pub struct CapiConfig {
pub mut:
	enabled ?bool
}

// ShellAuthenticationRequest is a password or other answer a shell command asks for.
pub struct ShellAuthenticationRequest {
pub:
	session_id string
	request_id string
	program    string
	prompt     string
	input_kind string // 'secret', 'text', or 'confirm'
	command    string
	cwd        string
}

// ShellAuthenticationResponse answers a request; leave `accept` false to cancel.
pub struct ShellAuthenticationResponse {
pub:
	accept bool
	value  string
}

// ShellAuthenticationHandler answers shellAuthentication.request.
pub type ShellAuthenticationHandler = fn (ShellAuthenticationRequest) ShellAuthenticationResponse

// StoredMemory is a memory the agent asked to store (memoryStore.store).
pub struct StoredMemory {
pub:
	session_id string
	subject    string
	fact       string
	citations  string
	reason     string
}

// StoreMemoryHandler receives memories to store.
pub type StoreMemoryHandler = fn (StoredMemory)

// HookHandler handles one hook type. It receives the raw JSON hook input and
// returns the raw JSON hook output, or '' for no output.
pub type HookHandler = fn (input string) string

// NetworkBasicAuthRequest is a proxy Basic authentication challenge.
pub struct NetworkBasicAuthRequest {
pub:
	proxy             string
	realm             string
	previous_rejected bool
}

// NetworkBasicCredentials answers a Basic challenge. Never log them.
pub struct NetworkBasicCredentials {
pub:
	username string
	password string
}

// ProxyResolver returns a PAC-style proxy list for a URL, or none.
pub type ProxyResolver = fn (url string) ?string

// NegotiateAuthenticator returns a base64 SPNEGO token for a service principal, or none.
pub type NegotiateAuthenticator = fn (spn string) ?string

// BasicAuthenticator returns proxy credentials, or none.
pub type BasicAuthenticator = fn (NetworkBasicAuthRequest) ?NetworkBasicCredentials

// NetworkOptions holds callbacks registered with network.setProvider after connecting.
pub struct NetworkOptions {
pub mut:
	scope                  string // 'connection' or 'runtime'; empty uses the runtime default
	resolve_proxy          ProxyResolver          = unsafe { nil }
	authenticate_negotiate NegotiateAuthenticator = unsafe { nil }
	authenticate_basic     BasicAuthenticator     = unsafe { nil }
}

// PolicyHelperFailure is a structured Policy Helper failure reported in RPC
// error data (`error.data.policyHelperFailure`). Treat the text as untrusted.
pub struct PolicyHelperFailure {
pub:
	category                 string
	failure_mode             string
	message                  string
	previous_policy_retained bool
	reason                   string
	authority                string
	code                     string
	diagnostic               string
	details                  string // raw JSON
	help_link                string // raw JSON
}

// PermissionModeResult is the result of `CopilotSession.set_permission_mode`.
pub struct PermissionModeResult {
pub:
	success bool
	mode    string
	reason  string // for example permission_mode_refusal_managed_policy
}

// ShellOutputData is the data of a `tool.shell_output` event.
pub struct ShellOutputData {
pub:
	tool_call_id string
	text         string
	sequence     int
	stream       string // 'stdout', 'stderr', or empty
}

// ---------------------------------------------------------------------------
// v3.1.0 feature types (parity with the Microsoft Copilot SDK 2.0.3-preview.0)
// ---------------------------------------------------------------------------

// UserInputImageAttachmentConstraints are the image limits a user input request
// advertises (`imageAttachmentConstraints`) when the runtime accepts images.
pub struct UserInputImageAttachmentConstraints {
pub mut:
	allowed_mime_types  []string // accepted MIME types, for example 'image/png'
	max_bytes_per_image int
	max_count           int // most images accepted in one answer
}

// UserInputRequest is a question from the agent's ask_user tool (`userInput.request`).
pub struct UserInputRequest {
pub mut:
	session_id                   string
	question                     string
	choices                      []string
	allow_freeform               bool = true // a freeform answer is allowed in addition to the choices
	request_id                   string // runtime id of the pending request, when sent
	image_attachment_constraints ?UserInputImageAttachmentConstraints // none when images are not accepted
	meta                         string // raw JSON of the opaque `_meta` object ('' when absent)
}

// UserInputImageAttachment is an image sent with a UserInputResponse (the wire
// `type` is always "image"). Only valid when the request advertised
// `image_attachment_constraints`; the runtime enforces the limits.
pub struct UserInputImageAttachment {
pub mut:
	data         string // base64-encoded image bytes
	mime_type    string
	description  string
	display_name string
}

// UserInputResponse answers a UserInputRequest. For an image-only answer, use an
// empty `answer` with `was_freeform` true.
pub struct UserInputResponse {
pub mut:
	answer       string
	was_freeform bool // true when the user typed the answer instead of picking a choice
	attachments  []UserInputImageAttachment // sent as `attachments` only when non-empty
}

// UserInputHandler answers the agent's ask_user questions (userInput.request).
pub type UserInputHandler = fn (UserInputRequest) UserInputResponse

// McpSecretsSaveResult is the result of `CopilotClient.mcp_secrets_save`.
pub struct McpSecretsSaveResult {
pub:
	used_keychain bool // true when the OS keychain stored the credential, false for the file backend
}

// PermissionsExtensionApproval is a user-scoped approval for one installed
// plugin extension (`permissions.extensionApprovals.*`).
pub struct PermissionsExtensionApproval {
pub:
	extension_name     string // stable identity, for example 'plugin:plugin-name:extension-name'
	plugin_name        string
	capabilities       []string // exact privileged capabilities the user approved
	granted_at         string   // RFC 3339 timestamp
	installation_key   string   // opaque; pass it back unchanged to revoke
	installation_label string
}

// PermissionsExtensionApprovalsGrantResult is the result of
// `CopilotClient.extension_approvals_grant`.
pub struct PermissionsExtensionApprovalsGrantResult {
pub:
	granted  bool // whether the pending request was eligible and persisted
	approval ?PermissionsExtensionApproval // the persisted approval, when granted
}

// PermissionsExtensionApprovalsListResult is the result of
// `CopilotClient.extension_approvals_list`.
pub struct PermissionsExtensionApprovalsListResult {
pub:
	approvals []PermissionsExtensionApproval
}

// PermissionsExtensionApprovalsRevokeResult is the result of
// `CopilotClient.extension_approvals_revoke`.
pub struct PermissionsExtensionApprovalsRevokeResult {
pub:
	revoked bool // whether a matching approval was revoked
}

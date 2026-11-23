// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - CopilotClient: manages CLI process and sessions

module copilot

import io
import json
import os
import sync
import time
import net
import x.json2

// CopilotClient is the main entry point for the Copilot SDK.
// It manages the CLI process lifecycle and creates conversation sessions.
pub struct CopilotClient {
mut:
	options    CopilotClientOptions
	state      ConnectionState = .disconnected
	process    &os.Process     = unsafe { nil }
	transport  &RpcTransport   = unsafe { nil }
	sessions   shared []&CopilotSession
	mu         &sync.Mutex  = sync.new_mutex()
	tcp_conn   &net.TcpConn = unsafe { nil }
	dispatcher &Dispatcher  = unsafe { nil }
}

// new_client creates a CopilotClient with the given options.
pub fn new_client(opts CopilotClientOptions) &CopilotClient {
	return &CopilotClient{
		options:    opts
		mu:         sync.new_mutex()
		dispatcher: &Dispatcher{
			network: opts.network
		}
	}
}

// state returns the current connection state.
pub fn (c &CopilotClient) state() ConnectionState {
	return c.state
}

// start launches the Copilot CLI process (or connects to an existing one),
// performs the handshake, and verifies SDK protocol 4.
pub fn (mut c CopilotClient) start() ! {
	if c.state == .connected {
		return
	}
	c.state = .connecting

	if c.options.cli_url.len > 0 {
		c.connect_to_server()!
	} else {
		c.spawn_cli_process()!
	}
	c.attach_dispatcher()

	c.handshake()!
	c.register_network_provider()!
	c.state = .connected

	// Set up session filesystem if configured
	if c.options.session_fs.initial_cwd.len > 0 {
		c.set_session_fs_provider(c.options.session_fs) or {}
	}
}

// stop shuts down the CLI process and all active sessions.
pub fn (mut c CopilotClient) stop() ! {
	mut active := []&CopilotSession{}
	rlock c.sessions {
		active = c.sessions.clone()
	}
	for mut session in active {
		session.disconnect() or {}
	}

	if !isnil(c.tcp_conn) {
		c.tcp_conn.close() or {}
	}
	if !isnil(c.process) {
		c.process.signal_kill()
		c.process.close()
	}

	c.state = .disconnected
}

// create_session starts a new conversation session on the connected server.
pub fn (mut c CopilotClient) create_session(config SessionConfig) !&CopilotSession {
	if c.state != .connected {
		return error('client is not connected -- call start() first')
	}
	c.require_available_tools(config)!

	mut params := build_session_params(config)
	registration_id := c.prepare_token_provider(config, mut params)!
	mut grouped := group_session_params(params, false)
	if c.options.mode == .empty {
		grouped.apply_empty_mode_defaults()
	}

	result := c.transport.send_request('session.create', grouped.to_json()) or {
		c.dispatcher.unregister_token_provider(registration_id)
		return err
	}

	metadata := json.decode(SessionMetadata, result) or {
		c.dispatcher.unregister_token_provider(registration_id)
		return error('failed to decode session metadata: ${err}')
	}

	mut session := new_session(c.transport, config, metadata)
	if registration_id.len > 0 {
		mut d := c.dispatcher
		session.on_close = fn [mut d, registration_id] () {
			d.unregister_token_provider(registration_id)
		}
	}
	c.track_session(session)
	return session
}

// resume_session_with resumes a session with options. Session options such as
// tools, hooks, providers, and handlers are re-supplied through `opts.config`.
pub fn (mut c CopilotClient) resume_session_with(opts ResumeSessionOptions) !&CopilotSession {
	if c.state != .connected {
		return error('client is not connected -- call start() first')
	}
	c.require_available_tools(opts.config)!

	mut params := build_session_params(opts.config)
	params['sessionId'] = json_str(opts.session_id)
	if opts.disable_resume {
		params['disableResume'] = 'true'
	}
	if opts.allow_transcript_recovery {
		params['allowTranscriptRecovery'] = 'true'
	}
	if opts.clear_default_provider && opts.config.default_provider.len == 0 {
		params['defaultProvider'] = 'null'
	}
	registration_id := c.prepare_token_provider(opts.config, mut params)!
	mut grouped := group_session_params(params, true)
	if c.options.mode == .empty {
		grouped.apply_empty_mode_defaults()
	}

	result := c.transport.send_request('session.resume', grouped.to_json()) or {
		c.dispatcher.unregister_token_provider(registration_id)
		return err
	}

	decoded := json.decode(SessionMetadata, result) or { SessionMetadata{} }
	metadata := SessionMetadata{
		session_id: if decoded.session_id.len > 0 { decoded.session_id } else { opts.session_id }
		model:      decoded.model
	}
	mut session := new_session(c.transport, opts.config, metadata)
	if registration_id.len > 0 {
		mut d := c.dispatcher
		session.on_close = fn [mut d, registration_id] () {
			d.unregister_token_provider(registration_id)
		}
	}
	c.track_session(session)
	return session
}

// get_status retrieves the server status.
pub fn (mut c CopilotClient) get_status() !string {
	raw := c.transport.send_request('status.get', '{}')!
	return parse_response_result(raw)
}

// get_models lists available models.
pub fn (mut c CopilotClient) get_models() ![]ModelInfo {
	raw := c.transport.send_request('models.list', '{}')!
	result := parse_response_result(raw)!
	return json.decode([]ModelInfo, result)
}

// get_foreground_session_id retrieves the current foreground session ID.
pub fn (mut c CopilotClient) get_foreground_session_id() !string {
	raw := c.transport.send_request('session.getForeground', '{}')!
	result := parse_response_result(raw)!
	decoded := json.decode(map[string]string, result) or {
		return error('failed to decode foreground session response: ${err}')
	}
	session_id := decoded['sessionId'] or { return error('no sessionId in response') }
	return session_id
}

// set_foreground_session_id sets the foreground session to the given ID.
pub fn (mut c CopilotClient) set_foreground_session_id(session_id string) ! {
	mut params := map[string]string{}
	params['"sessionId"'] = json_str(session_id)
	raw := c.transport.send_request('session.setForeground', build_params(params))!
	result := parse_response_result(raw)!
	if !result.contains('"success":true') && !result.contains('"success": true') {
		return error('Failed to set foreground session: ${result}')
	}
}

// get_last_session_id retrieves the last session ID, or an empty string if none.
pub fn (mut c CopilotClient) get_last_session_id() !string {
	raw := c.transport.send_request('session.getLastId', '{}')!
	result := parse_response_result(raw)!
	decoded := json.decode(map[string]string, result) or { return '' }
	session_id := decoded['sessionId'] or { return '' }
	return session_id
}

// get_session_metadata retrieves metadata for a session by ID.
pub fn (mut c CopilotClient) get_session_metadata(session_id string) !string {
	mut params := map[string]string{}
	params['"sessionId"'] = json_str(session_id)
	raw := c.transport.send_request('session.getMetadata', build_params(params))!
	return parse_response_result(raw)
}

// ping sends a health-check ping to the server.
pub fn (mut c CopilotClient) ping(message string) !string {
	msg := if message.len > 0 { message } else { 'ping' }
	mut params := map[string]string{}
	params['"message"'] = json_str(msg)
	raw := c.transport.send_request('ping', build_params(params))!
	return parse_response_result(raw)
}

// get_auth_status retrieves the authentication status.
pub fn (mut c CopilotClient) get_auth_status() !string {
	raw := c.transport.send_request('auth.getStatus', '{}')!
	return parse_response_result(raw)
}

// resume_session resumes a previously created session by ID and returns the
// raw result. Use `resume_session_with` to re-supply session options.
pub fn (mut c CopilotClient) resume_session(session_id string) !string {
	mut params := map[string]string{}
	params['sessionId'] = json_str(session_id)
	grouped := group_session_params(params, true)
	raw := c.transport.send_request('session.resume', grouped.to_json())!
	return parse_response_result(raw)
}

// delete_session deletes a session by ID.
pub fn (mut c CopilotClient) delete_session(session_id string) ! {
	mut params := map[string]string{}
	params['"sessionId"'] = json_str(session_id)
	raw := c.transport.send_request('session.delete', build_params(params))!
	_ := parse_response_result(raw)!
	c.dispatcher.remove_session(session_id)
}

// list_sessions returns a list of all known sessions.
pub fn (mut c CopilotClient) list_sessions() !string {
	raw := c.transport.send_request('session.list', '{}')!
	return parse_response_result(raw)
}

// clear_managed_settings_cache clears the runtime's cached managed settings
// (`managedSettings.clearCache`).
pub fn (mut c CopilotClient) clear_managed_settings_cache() ! {
	c.transport.send_request('managedSettings.clearCache', '{}')!
}

// set_session_fs_provider registers a session filesystem provider with the CLI server.
pub fn (mut c CopilotClient) set_session_fs_provider(config SessionFsConfig) !string {
	mut params := map[string]string{}
	params['"initialCwd"'] = json_str(config.initial_cwd)
	params['"sessionStatePath"'] = json_str(config.session_state_path)
	params['"conventions"'] = json_str(config.conventions)
	raw := c.transport.send_request('sessionFs.setProvider', build_params(params))!
	return parse_response_result(raw)
}

// --- private helpers ---

// build_session_params builds the flat session payload shared by create and
// resume. `group_session_params` converts it to the protocol 4 shape.
fn build_session_params(config SessionConfig) map[string]string {
	mut params := map[string]string{}
	if config.model.len > 0 {
		params['model'] = json_str(config.model)
	}
	if config.working_directory.len > 0 {
		params['workingDirectory'] = json_str(config.working_directory)
	}
	if config.streaming {
		params['streaming'] = 'true'
	}
	if config.agent.len > 0 {
		params['agent'] = json_str(config.agent)
	}
	if config.system_message.len > 0 {
		params['systemMessage'] = '{"mode":"append","content":${json_str(config.system_message)}}'
	}
	if config.github_token.len > 0 {
		params['gitHubToken'] = json_str(config.github_token)
	}
	if config.tools.len > 0 {
		mut tools_json := []string{}
		for t in config.tools {
			tools_json << tool_definition_json(&t)
		}
		params['tools'] = '[${tools_json.join(',')}]'
	}
	if config.history.len > 0 {
		params['history'] = json.encode(config.history)
	}
	if config.excluded_tools.len > 0 {
		params['excludedTools'] = json.encode(config.excluded_tools)
	}
	if config.enable_config_discovery {
		params['enableConfigDiscovery'] = 'true'
	}
	if config.include_sub_agent_streaming_events {
		params['includeSubAgentStreamingEvents'] = 'true'
	}
	if config.skill_directories.len > 0 {
		params['skillDirectories'] = json.encode(config.skill_directories)
	}
	if config.disabled_skills.len > 0 {
		params['disabledSkills'] = json.encode(config.disabled_skills)
	}
	if config.instruction_directories.len > 0 {
		params['instructionDirectories'] = json.encode(config.instruction_directories)
	}
	// --- Upstream-sync session configuration passthroughs ---
	if config.enable_citations {
		params['enableCitations'] = 'true'
	}
	if config.excluded_builtin_agents.len > 0 {
		params['excludedBuiltinAgents'] = json.encode(config.excluded_builtin_agents)
	}
	if config.session_limits.max_ai_credits != 0 {
		// Protocol 4 limits accept only maxAiCredits; max_requests and max_tokens are not sent.
		params['sessionLimits'] = '{"maxAiCredits":${config.session_limits.max_ai_credits}}'
	}
	if config.memory.enabled {
		params['memory'] = '{"enabled":true}'
	}
	if config.otlp_protocol.len > 0 {
		params['otlpProtocol'] = json_str(config.otlp_protocol)
	}
	if config.enable_web_socket_responses {
		params['enableWebSocketResponses'] = 'true'
	}
	if config.exp_assignments.len > 0 {
		params['expAssignments'] = json.encode(config.exp_assignments)
	}
	if !isnil(config.on_mcp_auth_request) {
		params['mcpAuthHandler'] = 'true'
	}
	if config.reasoning_effort.len > 0 {
		params['reasoningEffort'] = json_str(config.reasoning_effort)
	}
	if config.rewind_enabled {
		params['rewindEnabled'] = 'true'
	}
	if config.additional_directories.len > 0 {
		params['additionalDirectories'] = json.encode(config.additional_directories)
	}
	if config.disabled_mcp_servers.len > 0 {
		params['disabledMcpServers'] = json.encode(config.disabled_mcp_servers)
	}
	if config.github_mcp_tool_config.len > 0 {
		params['githubMcpToolConfig'] = json.encode(config.github_mcp_tool_config)
	}
	if config.canvas_provider.len > 0 {
		params['canvasProvider'] = json.encode(config.canvas_provider)
	}
	if config.custom_agents_local_only {
		params['customAgentsLocalOnly'] = 'true'
	}
	if config.tool_search.len > 0 {
		params['toolSearch'] = json.encode(config.tool_search)
	}
	if config.experimental_mode {
		params['enableExperimentalMode'] = 'true'
	}
	if config.content_exclusion {
		params['contentExclusion'] = 'true'
	}
	if !isnil(config.on_user_prompt_transformed) {
		params['userPromptTransformed'] = 'true'
	}
	// --- v3.0.0 / SDK protocol 4 session options ---
	if tools := config.available_tools {
		params['availableTools'] = json.encode(tools)
	}
	if config.ask_user_variant.len > 0 {
		params['askUserVariant'] = json_str(config.ask_user_variant)
	}
	if sandbox := config.sandbox {
		params['sandboxConfig'] = sandbox.to_json()
	}
	if config.feature_flags.len > 0 {
		params['featureFlags'] = json.encode(config.feature_flags)
	}
	if config.allowed_models.len > 0 {
		params['allowedModels'] = json.encode(config.allowed_models)
	}
	if config.auth_client_id_metadata_url.len > 0 {
		params['authClientIdMetadataUrl'] = json_str(config.auth_client_id_metadata_url)
	}
	if caps := config.model_capabilities_override {
		params['modelCapabilities'] = caps.to_json()
	}
	if config.agent_directories.len > 0 {
		params['agentDirectories'] = json.encode(config.agent_directories)
	}
	if config.hook_directories.len > 0 {
		params['hookDirectories'] = json.encode(config.hook_directories)
	}
	if config.trust_working_directory.len > 0 {
		params['trustWorkingDirectory'] = json_str(config.trust_working_directory)
	}
	if image_generation := config.image_generation {
		params['imageGeneration'] = image_generation.to_json()
	}
	if autopilot := config.autopilot {
		params['autopilot'] = autopilot.to_json()
	}
	if config.hooks.len > 0 {
		params['hooks'] = 'true'
	}
	if provider := config.provider {
		params['provider'] = provider.to_json()
	}
	if config.providers.len > 0 {
		params['providers'] = '[${config.providers.map(it.to_json()).join(',')}]'
	}
	if config.models.len > 0 {
		params['models'] = '[${config.models.map(it.to_json()).join(',')}]'
	}
	if config.default_provider.len > 0 {
		params['defaultProvider'] = json_str(config.default_provider)
	}
	if capi := config.capi {
		params['capi'] = capi.to_json()
	}
	if !isnil(config.on_shell_authentication_request) {
		params['requestShellAuthentication'] = 'true'
	}
	if !isnil(config.on_store_memory) {
		params['hasMemoryStore'] = 'true'
	}
	return params
}

// require_available_tools enforces the 'empty' client mode contract.
fn (c &CopilotClient) require_available_tools(config SessionConfig) ! {
	if c.options.mode != .empty {
		return
	}
	if _ := config.available_tools {
		return
	}
	return error('client mode "empty" requires available_tools on every session')
}

// prepare_token_provider registers the session's GitHub token provider and
// adds its registration id to the flat payload. Returns '' when unset.
fn (mut c CopilotClient) prepare_token_provider(config SessionConfig, mut params map[string]string) !string {
	if isnil(config.github_token_provider) {
		return ''
	}
	if config.github_token.len > 0 {
		return error('github_token_provider and github_token are mutually exclusive')
	}
	id := c.dispatcher.register_token_provider(config.github_token_provider)
	params['gitHubTokenProviderRegistrationId'] = json_str(id)
	return id
}

fn (mut c CopilotClient) track_session(session &CopilotSession) {
	c.dispatcher.add_session(session)
	lock c.sessions {
		c.sessions << session
	}
}

// attach_dispatcher routes server requests and notifications to the client
// and starts the background reader.
fn (mut c CopilotClient) attach_dispatcher() {
	mut d := c.dispatcher
	c.transport.request_handler = fn [mut d] (method string, params string) !string {
		return d.handle_request(method, params)
	}
	c.transport.notification_handler = fn [mut d] (method string, params string) {
		d.handle_notification(method, params)
	}
	mut t := c.transport
	spawn t.run()
}

fn (mut c CopilotClient) connect_to_server() ! {
	// Parse host:port from cli_url
	url := c.options.cli_url.replace('http://', '').replace('https://', '')
	parts := url.split(':')
	host := if parts.len > 0 { parts[0] } else { '127.0.0.1' }
	port := if parts.len > 1 { parts[1].int() } else { 3000 }

	mut conn := net.dial_tcp('${host}:${port}') or {
		c.state = .error
		return error('failed to connect to CLI server at ${host}:${port}: ${err}')
	}
	c.tcp_conn = conn

	// The TCP connection implements io.Reader and io.Writer
	c.transport = new_rpc_transport(conn, conn)
}

fn (mut c CopilotClient) spawn_cli_process() ! {
	cli_path := c.resolve_cli_path()!
	mut args := ['--headless', '--no-auto-update', '--stdio']

	// A `.js` entry point (for example npm-loader.js) runs under node; any other
	// path (a native binary or the `.bin/copilot` shim) is executed directly.
	mut executable := cli_path
	if cli_path.to_lower().ends_with('.js') {
		executable = os.find_abs_path_of_executable('node') or {
			return error('node is required to run ${cli_path}: ${err}')
		}
		args.prepend(cli_path)
	}

	mut proc := os.new_process(executable)
	proc.set_args(args)
	proc.set_redirect_stdio()
	proc.run()

	if proc.status != .running && proc.status != .exited {
		c.state = .error
		return error('CLI process failed to start')
	}
	c.process = proc
	reader := &ProcessReader{
		process: proc
	}
	writer := &ProcessWriter{
		process: proc
	}
	c.transport = new_rpc_transport(reader, writer)
}

fn (c &CopilotClient) resolve_cli_path() !string {
	// 1. Explicit path in options
	if c.options.cli_path.len > 0 {
		if os.exists(c.options.cli_path) {
			return c.options.cli_path
		}
		return error('cli_path does not exist: ${c.options.cli_path}')
	}

	// 2. Environment variable
	env_path := os.getenv('COPILOT_CLI_PATH')
	if env_path.len > 0 && os.exists(env_path) {
		return env_path
	}

	// 3. Search PATH
	for name in ['copilot', 'copilot-cli'] {
		found := os.find_abs_path_of_executable(name) or { continue }
		return found
	}

	return error('copilot-cli not found. Set cli_path, COPILOT_CLI_PATH, or add to PATH.')
}

// handshake sends `connect` (only when client_info is set, falling back to
// `ping` when the server does not know `connect`) or `ping`, then checks that
// the server speaks SDK protocol 4.
fn (mut c CopilotClient) handshake() ! {
	mut result := ''
	info := client_info_json(c.options.client_info)
	if info.len > 0 {
		result = c.transport.send_request('connect', '{"clientInfo":${info}}') or {
			if err is RpcError {
				if err.error_code != -32601 && !err.message.contains('Unhandled method connect') {
					c.state = .error
					return error('handshake failed: ${err}')
				}
			} else {
				c.state = .error
				return error('handshake failed: ${err}')
			}
			''
		}
	}
	if result.len == 0 {
		result = c.transport.send_request('ping', '{}') or {
			c.state = .error
			return error('handshake failed: ${err}')
		}
	}
	verify_protocol_version(result) or {
		c.state = .error
		return err
	}
}

// register_network_provider calls network.setProvider when any network
// callback is configured (an empty object marks each registered callback).
fn (mut c CopilotClient) register_network_provider() ! {
	network := c.options.network
	if isnil(network.resolve_proxy) && isnil(network.authenticate_negotiate)
		&& isnil(network.authenticate_basic) {
		return
	}
	mut params := map[string]string{}
	if network.scope.len > 0 {
		params['scope'] = json_str(network.scope)
	}
	if !isnil(network.resolve_proxy) {
		params['resolveProxy'] = '{}'
	}
	if !isnil(network.authenticate_negotiate) {
		params['authenticateNegotiate'] = '{}'
	}
	if !isnil(network.authenticate_basic) {
		params['authenticateBasic'] = '{}'
	}
	c.transport.send_request('network.setProvider', build_params(params))!
}

// wait_for_ready blocks until the client is connected or timeout elapses.
pub fn (mut c CopilotClient) wait_for_ready(timeout_ms int) ! {
	start := time.now()
	for {
		if c.state == .connected {
			return
		}
		if c.state == .error {
			return error('client entered error state')
		}
		elapsed := time.since(start)
		if elapsed.milliseconds() > timeout_ms {
			return error('timed out waiting for client to be ready')
		}
		time.sleep(50 * time.millisecond)
	}
}

// with_session is a convenience that creates a session, runs a closure,
// and disconnects the session when done.
pub fn (mut c CopilotClient) with_session(config SessionConfig, callback fn (mut CopilotSession) !) ! {
	mut session := c.create_session(config)!
	defer {
		session.disconnect() or {}
	}
	callback(mut session)!
}

// --- process stdio adapters ---

// ProcessReader adapts the CLI process stdout to io.Reader.
struct ProcessReader {
mut:
	process &os.Process = unsafe { nil }
	pending []u8
}

fn (mut r ProcessReader) read(mut buf []u8) !int {
	for r.pending.len == 0 {
		chunk := r.process.stdout_read()
		if chunk.len > 0 {
			r.pending << chunk.bytes()
		} else if !r.process.is_alive() {
			return io.Eof{}
		} else {
			time.sleep(5 * time.millisecond)
		}
	}
	n := if r.pending.len < buf.len { r.pending.len } else { buf.len }
	for i in 0 .. n {
		buf[i] = r.pending[i]
	}
	r.pending = r.pending[n..].clone()
	return n
}

// ProcessWriter adapts the CLI process stdin to io.Writer.
struct ProcessWriter {
mut:
	process &os.Process = unsafe { nil }
}

fn (mut w ProcessWriter) write(buf []u8) !int {
	w.process.stdin_write(buf.bytestr())
	return buf.len
}

// --- server request and notification dispatch ---

// Dispatcher answers server-to-client requests and routes notifications to
// sessions. It runs on the transport's reader thread.
@[heap]
pub struct Dispatcher {
mut:
	mu              &sync.Mutex = sync.new_mutex()
	sessions        map[string]&CopilotSession
	token_providers map[string]GitHubTokenProvider
	network         NetworkOptions
}

fn (mut d Dispatcher) add_session(session &CopilotSession) {
	d.mu.lock()
	d.sessions[session.id()] = session
	d.mu.unlock()
}

fn (mut d Dispatcher) remove_session(session_id string) {
	d.mu.lock()
	d.sessions.delete(session_id)
	d.mu.unlock()
}

fn (mut d Dispatcher) find_session(session_id string) ?&CopilotSession {
	d.mu.lock()
	defer {
		d.mu.unlock()
	}
	return d.sessions[session_id] or { return none }
}

fn (mut d Dispatcher) register_token_provider(provider GitHubTokenProvider) string {
	id := new_registration_id()
	d.mu.lock()
	d.token_providers[id] = provider
	d.mu.unlock()
	return id
}

fn (mut d Dispatcher) unregister_token_provider(registration_id string) {
	if registration_id.len == 0 {
		return
	}
	d.mu.lock()
	d.token_providers.delete(registration_id)
	d.mu.unlock()
}

fn (mut d Dispatcher) find_token_provider(registration_id string) ?GitHubTokenProvider {
	d.mu.lock()
	defer {
		d.mu.unlock()
	}
	return d.token_providers[registration_id] or { return none }
}

// handle_request answers one server-to-client request with a raw JSON result.
fn (mut d Dispatcher) handle_request(method string, params string) !string {
	obj := decode_object(params)
	session_id := any_str(obj, 'sessionId')
	match method {
		'hooks.invoke' {
			session := d.find_session(session_id) or {
				return error('Session not found: ${session_id}')
			}
			hook_type := any_str(obj, 'hookType')
			handler := session.config.hooks[hook_type] or { return '{}' }
			if isnil(handler) {
				return '{}'
			}
			input := any_raw(obj, 'input')
			output := handler(if input.len > 0 { input } else { 'null' })
			if output.trim_space().len == 0 {
				return '{}'
			}
			return '{"output":${output}}'
		}
		'gitHubToken.getToken' {
			registration_id := any_str(obj, 'registrationId')
			provider := d.find_token_provider(registration_id) or {
				return error('No GitHub token provider is registered for this request')
			}
			answer := provider(GitHubTokenRequest{
				registration_id: registration_id
				host:            any_str(obj, 'host')
				session_id:      session_id
				reason:          any_str(obj, 'reason')
			})
			return answer.to_json()
		}
		'shellAuthentication.request' {
			session := d.find_session(session_id) or { return '{"action":"cancel"}' }
			if isnil(session.config.on_shell_authentication_request) {
				return '{"action":"cancel"}'
			}
			answer := session.config.on_shell_authentication_request(ShellAuthenticationRequest{
				session_id: session_id
				request_id: any_str(obj, 'requestId')
				program:    any_str(obj, 'program')
				prompt:     any_str(obj, 'prompt')
				input_kind: any_str(obj, 'inputKind')
				command:    any_str(obj, 'command')
				cwd:        any_str(obj, 'cwd')
			})
			if answer.accept {
				return '{"action":"accept","value":${json_str(answer.value)}}'
			}
			return '{"action":"cancel"}'
		}
		'shellAuthentication.cancel' {
			// Requests are answered synchronously on the reader thread, so a
			// cancellation always refers to a request that was already answered.
			return '{}'
		}
		'memoryStore.store' {
			session := d.find_session(session_id) or {
				return error('Memory store owner is unavailable')
			}
			if isnil(session.config.on_store_memory) {
				return error('Memory store owner is unavailable')
			}
			session.config.on_store_memory(StoredMemory{
				session_id: session_id
				subject:    any_str(obj, 'subject')
				fact:       any_str(obj, 'fact')
				citations:  any_str(obj, 'citations')
				reason:     any_str(obj, 'reason')
			})
			return 'null'
		}
		'network.resolveProxy' {
			if isnil(d.network.resolve_proxy) {
				return RpcError{
					error_code: -32601
					message:    'No network.resolveProxy callback registered'
				}
			}
			proxy := d.network.resolve_proxy(any_str(obj, 'url')) or { return '{}' }
			if proxy.len == 0 {
				return '{}'
			}
			return '{"proxy":${json_str(proxy)}}'
		}
		'network.authenticateNegotiate' {
			if isnil(d.network.authenticate_negotiate) {
				return RpcError{
					error_code: -32601
					message:    'No network.authenticateNegotiate callback registered'
				}
			}
			token := d.network.authenticate_negotiate(any_str(obj, 'spn')) or { return '{}' }
			if token.len == 0 {
				return '{}'
			}
			return '{"token":${json_str(token)}}'
		}
		'network.authenticateBasic' {
			if isnil(d.network.authenticate_basic) {
				return RpcError{
					error_code: -32601
					message:    'No network.authenticateBasic callback registered'
				}
			}
			credentials := d.network.authenticate_basic(NetworkBasicAuthRequest{
				proxy:             any_str(obj, 'proxy')
				realm:             any_str(obj, 'realm')
				previous_rejected: any_bool(obj, 'previousRejected')
			}) or { return '{}' }
			mut parts := map[string]string{}
			if credentials.username.len > 0 {
				parts['username'] = json_str(credentials.username)
			}
			if credentials.password.len > 0 {
				parts['password'] = json_str(credentials.password)
			}
			return build_params(parts)
		}
		else {
			return RpcError{
				error_code: -32601
				message:    'Method not found: ${method}'
			}
		}
	}
}

// handle_notification routes `session.event` (and legacy per-method)
// notifications to the owning session. Unknown event types pass through.
fn (mut d Dispatcher) handle_notification(method string, params string) {
	obj := decode_object(params)
	session_id := any_str(obj, 'sessionId')
	if session_id.len == 0 {
		return
	}
	mut session := d.find_session(session_id) or { return }
	mut ev := SessionEvent{
		event_type: method
		raw_data:   params
	}
	if method == 'session.event' {
		if raw_event := obj['event'] {
			if raw_event is map[string]json2.Any {
				ev = SessionEvent{
					event_type: any_str(raw_event, 'type')
					raw_data:   any_raw(raw_event, 'data')
					id:         any_str(raw_event, 'id')
					timestamp:  any_str(raw_event, 'timestamp')
					parent_id:  any_str(raw_event, 'parentId')
					agent_id:   any_str(raw_event, 'agentId')
					ephemeral:  any_bool(raw_event, 'ephemeral')
				}
			}
		}
	}
	session.dispatch_event(ev)
}

// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - CopilotClient: manages CLI process and sessions

module copilot

import json
import os
import sync
import time
import net

// CopilotClient is the main entry point for the Copilot SDK.
// It manages the CLI process lifecycle and creates conversation sessions.
pub struct CopilotClient {
mut:
	options   CopilotClientOptions
	state     ConnectionState = .disconnected
	process   os.Process
	transport &RpcTransport = unsafe { nil }
	sessions  shared []&CopilotSession
	mu        sync.Mutex
	tcp_conn  &net.TcpConn = unsafe { nil }
}

// new_client creates a CopilotClient with the given options.
pub fn new_client(opts CopilotClientOptions) &CopilotClient {
	return &CopilotClient{
		options: opts
		mu: sync.new_mutex()
	}
}

// state returns the current connection state.
pub fn (c &CopilotClient) state() ConnectionState {
	return c.state
}

// start launches the Copilot CLI process (or connects to an existing one)
// and performs the initial handshake.
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

	c.handshake()!
	c.state = .connected

	// Set up session filesystem if configured
	if c.options.session_fs.initial_cwd.len > 0 {
		c.set_session_fs_provider(c.options.session_fs) or {}
	}
}

// stop shuts down the CLI process and all active sessions.
pub fn (mut c CopilotClient) stop() ! {
	rlock c.sessions {
		for mut session in c.sessions {
			session.disconnect() or {}
		}
	}

	if !isnil(c.tcp_conn) {
		c.tcp_conn.close() or {}
	}

	c.state = .disconnected
}

// create_session starts a new conversation session on the connected server.
pub fn (mut c CopilotClient) create_session(config SessionConfig) !&CopilotSession {
	if c.state != .connected {
		return error('client is not connected -- call start() first')
	}

	// Build the session/create request params
	mut params := map[string]string{}
	params['"protocolVersion"'] = '${protocol_version}'
	if config.model.len > 0 {
		params['"model"'] = '"${config.model}"'
	}
	if config.streaming {
		params['"streaming"'] = 'true'
	}
	if config.agent.len > 0 {
		params['"agent"'] = '"${config.agent}"'
	}
	if config.system_message.len > 0 {
		params['"systemMessage"'] = '"${config.system_message}"'
	}
	if config.tools.len > 0 {
		mut tools_json := []string{}
		for t in config.tools {
			schema := tool_to_json_schema(&t)
			tool_obj := '{"name":"${t.name}","description":"${t.description}","parameters":${json.encode(schema)}}'
			tools_json << tool_obj
		}
		params['"tools"'] = '[${tools_json.join(",")}]'
	}
	if config.history.len > 0 {
		params['"history"'] = json.encode(config.history)
	}

	raw_result := c.transport.send_request('session.create', build_params(params))!
	result := parse_response_result(raw_result)!

	metadata := json.decode(SessionMetadata, result) or {
		return error('failed to decode session metadata: ${err}')
	}

	mut session := new_session(c.transport, config, metadata)
	session.start_event_loop()

	lock c.sessions {
		c.sessions << session
	}

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
	params['"sessionId"'] = '"${session_id}"'
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
	decoded := json.decode(map[string]string, result) or {
		return ''
	}
	session_id := decoded['sessionId'] or { return '' }
	return session_id
}

// get_session_metadata retrieves metadata for a session by ID.
pub fn (mut c CopilotClient) get_session_metadata(session_id string) !string {
	mut params := map[string]string{}
	params['"sessionId"'] = '"${session_id}"'
	raw := c.transport.send_request('session.getMetadata', build_params(params))!
	return parse_response_result(raw)
}

// ping sends a health-check ping to the server.
pub fn (mut c CopilotClient) ping(message string) !string {
	msg := if message.len > 0 { message } else { 'ping' }
	mut params := map[string]string{}
	params['"message"'] = '"${msg}"'
	raw := c.transport.send_request('ping', build_params(params))!
	return parse_response_result(raw)
}

// get_auth_status retrieves the authentication status.
pub fn (mut c CopilotClient) get_auth_status() !string {
	raw := c.transport.send_request('auth.getStatus', '{}')!
	return parse_response_result(raw)
}

// resume_session resumes a previously created session by ID.
pub fn (mut c CopilotClient) resume_session(session_id string) !string {
	mut params := map[string]string{}
	params['"sessionId"'] = '"${session_id}"'
	raw := c.transport.send_request('session.resume', build_params(params))!
	return parse_response_result(raw)
}

// delete_session deletes a session by ID.
pub fn (mut c CopilotClient) delete_session(session_id string) ! {
	mut params := map[string]string{}
	params['"sessionId"'] = '"${session_id}"'
	raw := c.transport.send_request('session.delete', build_params(params))!
	_ := parse_response_result(raw)!
}

// list_sessions returns a list of all known sessions.
pub fn (mut c CopilotClient) list_sessions() !string {
	raw := c.transport.send_request('session.list', '{}')!
	return parse_response_result(raw)
}

// set_session_fs_provider registers a session filesystem provider with the CLI server.
pub fn (mut c CopilotClient) set_session_fs_provider(config SessionFsConfig) !string {
	mut params := map[string]string{}
	params['"initialCwd"'] = '"${config.initial_cwd}"'
	params['"sessionStatePath"'] = '"${config.session_state_path}"'
	params['"conventions"'] = '"${config.conventions}"'
	raw := c.transport.send_request('sessionFs.setProvider', build_params(params))!
	return parse_response_result(raw)
}

// --- private helpers ---

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

	mut cmd := os.Command{
		path: '${cli_path} --stdio'
		redirect_stdout: true
	}
	cmd.start() or {
		c.state = .error
		return error('failed to spawn CLI process at ${cli_path}: ${err}')
	}

	// For stdio-based transport we rely on the process stdin/stdout.
	// In V, os.Command gives limited pipe access, so we open a socket
	// connection if the CLI prints a port. This is a simplified model.
	// For production, you would pipe stdin/stdout directly.
	c.process = os.Process{
		filename: cli_path
		args: ['--stdio']
	}
	c.process.set_redirect_stdio()
	c.process.run()

	if c.process.status != .running && c.process.status != .exited {
		c.state = .error
		return error('CLI process failed to start')
	}
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
	which_result := os.execute('which copilot-cli 2>/dev/null || where copilot-cli 2>nul')
	if which_result.exit_code == 0 {
		found := which_result.output.trim_space()
		if found.len > 0 {
			return found
		}
	}

	return error('copilot-cli not found. Set cli_path, COPILOT_CLI_PATH, or add to PATH.')
}

fn (mut c CopilotClient) handshake() ! {
	mut params := map[string]string{}
	params['"protocolVersion"'] = '${protocol_version}'
	params['"sdkName"'] = '"${sdk_name}"'
	params['"sdkVersion"'] = '"${sdk_version}"'

	raw := c.transport.send_request('initialize', build_params(params))!
	result := parse_response_result(raw) or {
		c.state = .error
		return error('handshake failed: ${err}')
	}

	// Verify protocol version compatibility
	if result.contains('"protocolVersion"') {
		// Simple check -- a full implementation would decode and compare
		_ = result
	}
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

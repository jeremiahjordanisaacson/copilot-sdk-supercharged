// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Unit tests for client, types, tools, jsonrpc, and session

module copilot

import json

// --- version tests ---

fn test_sdk_version_is_set() {
	assert sdk_version.len > 0
	assert sdk_version == '0.1.0'
}

fn test_protocol_version_value() {
	assert protocol_version == 3
	assert min_protocol_version == 3
}

fn test_sdk_name() {
	assert sdk_name == 'copilot-sdk-vlang'
}

// --- types tests ---

fn test_approve_all() {
	req := PermissionRequest{
		resource: '/file'
		action: 'read'
		description: 'read a file'
	}
	resp := approve_all(req)
	assert resp.approved == true
	assert resp.reason == 'auto-approved'
}

fn test_deny_all() {
	req := PermissionRequest{
		resource: '/secret'
		action: 'write'
		description: 'write secret'
	}
	resp := deny_all(req)
	assert resp.approved == false
	assert resp.reason == 'auto-denied'
}

fn test_history_entry_json_roundtrip() {
	entry := HistoryEntry{
		role: 'user'
		content: 'Hello'
	}
	encoded := json.encode(entry)
	assert encoded.contains('"role"')
	assert encoded.contains('"content"')

	decoded := json.decode(HistoryEntry, encoded) or {
		assert false, 'decode failed'
		return
	}
	assert decoded.role == 'user'
	assert decoded.content == 'Hello'
}

fn test_send_options_defaults() {
	opts := SendOptions{}
	assert opts.prompt == ''
	assert opts.attachments.len == 0
	assert opts.mode == ''
}

fn test_session_event_decode() {
	raw := '{"type":"assistant.message","data":"{\\"content\\":\\"hi\\"}"}'
	ev := json.decode(SessionEvent, raw) or {
		assert false, 'decode failed'
		return
	}
	assert ev.event_type == 'assistant.message'
}

fn test_message_data_decode() {
	raw := '{"content":"Hello world","role":"assistant"}'
	data := json.decode(MessageData, raw) or {
		assert false, 'decode failed'
		return
	}
	assert data.content == 'Hello world'
	assert data.role == 'assistant'
}

fn test_copilot_error_message() {
	e := CopilotError{
		kind: .connection_failed
		message: 'cannot reach server'
		source: 'start'
	}
	assert e.msg().contains('connection_failed')
	assert e.msg().contains('cannot reach server')
}

fn test_error_kind_values() {
	assert ErrorKind.connection_failed != ErrorKind.timeout
	assert ErrorKind.protocol_error != ErrorKind.unknown
}

fn test_client_options_defaults() {
	opts := CopilotClientOptions{}
	assert opts.log_level == 'error'
	assert opts.cli_path == ''
	assert opts.cli_url == ''
}

fn test_session_config_defaults() {
	config := SessionConfig{}
	assert config.streaming == true
	assert config.model == ''
	assert config.tools.len == 0
}

// --- tool tests ---

fn test_define_tool() {
	handler := fn (args map[string]string) !string {
		city := args['city'] or { return error('missing city') }
		return '{"temp":"72F","city":"${city}"}'
	}

	mut params := map[string]ToolParam{}
	params['city'] = ToolParam{
		param_type: 'string'
		description: 'City name'
		required: true
	}

	tool := define_tool('get_weather', 'Get the weather', params, handler)
	assert tool.name == 'get_weather'
	assert tool.description == 'Get the weather'
	assert tool.parameters.len == 1
}

fn test_execute_tool_success() {
	handler := fn (args map[string]string) !string {
		return 'result-ok'
	}

	tool := Tool{
		name: 'echo'
		description: 'echo tool'
		handler: handler
	}

	result := execute_tool(&tool, map[string]string{}) or {
		assert false, 'should not fail'
		return
	}
	assert result == 'result-ok'
}

fn test_execute_tool_nil_handler() {
	tool := Tool{
		name: 'broken'
		description: 'no handler'
	}

	execute_tool(&tool, map[string]string{}) or {
		assert err.msg().contains('no handler')
		return
	}
	assert false, 'expected error for nil handler'
}

fn test_tool_to_json_schema() {
	mut params := map[string]ToolParam{}
	params['city'] = ToolParam{
		param_type: 'string'
		description: 'The city'
		required: true
	}
	params['unit'] = ToolParam{
		param_type: 'string'
		description: 'Temperature unit'
		required: false
	}

	tool := Tool{
		name: 'weather'
		description: 'Get weather'
		parameters: params
	}

	schema := tool_to_json_schema(&tool)
	encoded := json.encode(schema)
	assert encoded.contains('"type"')
	assert encoded.contains('"properties"')
	assert encoded.contains('"city"')
}

// --- jsonrpc tests ---

fn test_build_params_empty() {
	result := build_params(map[string]string{})
	assert result == '{}'
}

fn test_build_params_single() {
	mut pairs := map[string]string{}
	pairs['"key"'] = '"value"'
	result := build_params(pairs)
	assert result.contains('"key"')
	assert result.contains('"value"')
}

fn test_build_params_multiple() {
	mut pairs := map[string]string{}
	pairs['"a"'] = '1'
	pairs['"b"'] = '"two"'
	result := build_params(pairs)
	assert result.starts_with('{')
	assert result.ends_with('}')
}

fn test_parse_response_result_success() {
	result := parse_response_result('{"ok":true}') or {
		assert false, 'should not fail'
		return
	}
	assert result == '{"ok":true}'
}

fn test_parse_response_result_error() {
	parse_response_result('ERROR:42:something broke') or {
		assert err.msg().contains('42')
		assert err.msg().contains('something broke')
		return
	}
	assert false, 'expected error'
}

fn test_jsonrpc_request_encode() {
	req := JsonRpcRequest{
		jsonrpc: '2.0'
		id: 1
		method: 'test/method'
		params: '{}'
	}
	encoded := json.encode(req)
	assert encoded.contains('"jsonrpc":"2.0"')
	assert encoded.contains('"method":"test/method"')
}

fn test_jsonrpc_notification_encode() {
	notif := JsonRpcNotification{
		jsonrpc: '2.0'
		method: 'session/event'
		params: '{"type":"done"}'
	}
	encoded := json.encode(notif)
	assert encoded.contains('"method":"session/event"')
}

fn test_jsonrpc_error_struct() {
	err_obj := JsonRpcError{
		code: -32600
		message: 'Invalid Request'
		data: ''
	}
	assert err_obj.code == -32600
	assert err_obj.message == 'Invalid Request'
}

// --- client tests (unit-level, no process spawn) ---

fn test_new_client_defaults() {
	client := new_client(CopilotClientOptions{})
	assert client.state() == .disconnected
}

fn test_new_client_with_url() {
	client := new_client(CopilotClientOptions{
		cli_url: 'localhost:3000'
	})
	assert client.options.cli_url == 'localhost:3000'
	assert client.state() == .disconnected
}

fn test_new_client_with_path() {
	client := new_client(CopilotClientOptions{
		cli_path: '/usr/local/bin/copilot-cli'
	})
	assert client.options.cli_path == '/usr/local/bin/copilot-cli'
}

fn test_create_session_before_start() {
	mut client := new_client(CopilotClientOptions{})
	client.create_session(SessionConfig{}) or {
		assert err.msg().contains('not connected')
		return
	}
	assert false, 'expected error when creating session before start'
}

// --- session tests (unit-level) ---

fn test_session_metadata() {
	metadata := SessionMetadata{
		session_id: 'abc-123'
		model: 'gpt-4'
	}
	assert metadata.session_id == 'abc-123'
	assert metadata.model == 'gpt-4'
}

fn test_model_info_decode() {
	raw := '{"name":"gpt-5","version":"2025-01"}'
	info := json.decode(ModelInfo, raw) or {
		assert false, 'decode failed'
		return
	}
	assert info.name == 'gpt-5'
	assert info.version == '2025-01'
}

fn test_tool_call_request_decode() {
	raw := '{"id":"tc-1","name":"get_weather","arguments":"{\\"city\\":\\"Seattle\\"}"}'
	req := json.decode(ToolCallRequest, raw) or {
		assert false, 'decode failed'
		return
	}
	assert req.id == 'tc-1'
	assert req.name == 'get_weather'
}

fn test_tool_call_response_encode() {
	resp := ToolCallResponse{
		id: 'tc-1'
		result: '{"temp":"72F"}'
		error: ''
	}
	encoded := json.encode(resp)
	assert encoded.contains('"id":"tc-1"')
	assert encoded.contains('"result"')
}

fn test_permission_request_decode() {
	raw := '{"resource":"/etc/passwd","action":"read","description":"read system file"}'
	req := json.decode(PermissionRequest, raw) or {
		assert false, 'decode failed'
		return
	}
	assert req.resource == '/etc/passwd'
	assert req.action == 'read'
}

fn test_attachment_json_roundtrip() {
	att := Attachment{
		file_path: 'main.v'
		language: 'vlang'
		content: 'fn main() {}'
	}
	encoded := json.encode(att)
	decoded := json.decode(Attachment, encoded) or {
		assert false, 'decode failed'
		return
	}
	assert decoded.file_path == 'main.v'
	assert decoded.language == 'vlang'
}

fn test_connection_state_enum() {
	s := ConnectionState.disconnected
	assert s == .disconnected

	match s {
		.disconnected { assert true }
		else { assert false, 'unexpected state' }
	}
}

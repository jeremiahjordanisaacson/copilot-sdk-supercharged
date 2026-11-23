// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - JSON helpers for SDK protocol 4 wire payloads

module copilot

import rand
import x.json2

fn bool_json(value bool) string {
	return if value { 'true' } else { 'false' }
}

// decode_object parses raw JSON and returns it as a map (empty when not an object).
fn decode_object(raw string) map[string]json2.Any {
	value := json2.decode[json2.Any](raw) or { return map[string]json2.Any{} }
	if value is map[string]json2.Any {
		return value
	}
	return map[string]json2.Any{}
}

fn any_str(obj map[string]json2.Any, key string) string {
	value := obj[key] or { return '' }
	if value is string {
		return value
	}
	return ''
}

fn any_bool(obj map[string]json2.Any, key string) bool {
	value := obj[key] or { return false }
	if value is bool {
		return value
	}
	return false
}

fn any_int(obj map[string]json2.Any, key string) int {
	value := obj[key] or { return 0 }
	if value is json2.Null || value is string || value is bool {
		return 0
	}
	return value.int()
}

// any_raw returns the raw JSON of a member, or '' when absent or null.
fn any_raw(obj map[string]json2.Any, key string) string {
	value := obj[key] or { return '' }
	if value is json2.Null {
		return ''
	}
	return value.json_str()
}

fn new_registration_id() string {
	return rand.uuid_v4()
}

// client_info_json renders `clientInfo` for `connect`, or '' when every field is empty.
pub fn client_info_json(info ClientInfo) string {
	mut parts := map[string]string{}
	if info.editor_name.len > 0 {
		parts['editorName'] = json_str(info.editor_name)
	}
	if info.editor_version.len > 0 {
		parts['editorVersion'] = json_str(info.editor_version)
	}
	if info.extension_name.len > 0 {
		parts['extensionName'] = json_str(info.extension_name)
	}
	if info.extension_version.len > 0 {
		parts['extensionVersion'] = json_str(info.extension_version)
	}
	if parts.len == 0 {
		return ''
	}
	return build_params(parts)
}

// to_json renders the `gitHubToken.getToken` answer.
pub fn (r GitHubTokenResult) to_json() string {
	if r.cancelled || r.access_token.len == 0 {
		return '{"kind":"cancelled"}'
	}
	mut parts := map[string]string{}
	parts['kind'] = '"token"'
	parts['accessToken'] = json_str(r.access_token)
	if r.token_type.len > 0 {
		parts['tokenType'] = json_str(r.token_type)
	}
	parts['expiresIn'] = '${r.expires_in}'
	return build_params(parts)
}

// to_json renders `permissions.sandbox`.
pub fn (s SandboxConfig) to_json() string {
	mut parts := map[string]string{}
	parts['enabled'] = bool_json(s.enabled)
	if s.allow_bypass {
		parts['allowBypass'] = 'true'
	}
	if value := s.add_current_working_directory {
		parts['addCurrentWorkingDirectory'] = bool_json(value)
	}
	if value := s.sandbox_mcp_servers {
		parts['sandboxMcpServers'] = bool_json(value)
	}
	if s.user_policy.len > 0 {
		parts['userPolicy'] = s.user_policy
	}
	return build_params(parts)
}

// to_json renders `inference.capabilities` (limits use snake_case wire keys).
pub fn (m ModelCapabilitiesOverride) to_json() string {
	mut supports := map[string]string{}
	if value := m.supports_vision {
		supports['vision'] = bool_json(value)
	}
	if value := m.supports_tool_calls {
		supports['toolCalls'] = bool_json(value)
	}
	if value := m.supports_reasoning_effort {
		supports['reasoningEffort'] = bool_json(value)
	}
	mut limits := map[string]string{}
	if m.max_prompt_tokens > 0 {
		limits['max_prompt_tokens'] = '${m.max_prompt_tokens}'
	}
	if m.max_output_tokens > 0 {
		limits['max_output_tokens'] = '${m.max_output_tokens}'
	}
	if m.max_context_window_tokens > 0 {
		limits['max_context_window_tokens'] = '${m.max_context_window_tokens}'
	}
	mut parts := map[string]string{}
	if supports.len > 0 {
		parts['supports'] = build_params(supports)
	}
	if limits.len > 0 {
		parts['limits'] = build_params(limits)
	}
	return build_params(parts)
}

// to_json renders `tools.imageGeneration`.
pub fn (i ImageGenerationConfig) to_json() string {
	mut parts := map[string]string{}
	if value := i.enabled {
		parts['enabled'] = bool_json(value)
	}
	if value := i.watermark {
		parts['watermark'] = bool_json(value)
	}
	if value := i.save_to_disk {
		parts['saveToDisk'] = bool_json(value)
	}
	return build_params(parts)
}

// to_json renders `execution.autopilot`.
pub fn (a AutopilotConfig) to_json() string {
	mut parts := map[string]string{}
	if value := a.continuation_enabled {
		parts['continuationEnabled'] = bool_json(value)
	}
	if value := a.max_continues {
		parts['maxContinues'] = '${value}'
	}
	return build_params(parts)
}

fn add_connection_fields(mut parts map[string]string, provider_type string, base_url string, api_key string, api_key_command string, bearer_token string, wire_api string, transport string, model_provider string, azure AzureProviderOptions, headers map[string]string) {
	if provider_type.len > 0 {
		parts['type'] = json_str(provider_type)
	}
	if base_url.len > 0 {
		parts['baseUrl'] = json_str(base_url)
	}
	if api_key.len > 0 {
		parts['apiKey'] = json_str(api_key)
	}
	if api_key_command.len > 0 {
		parts['apiKeyCommand'] = json_str(api_key_command)
	}
	if bearer_token.len > 0 {
		parts['bearerToken'] = json_str(bearer_token)
	}
	if wire_api.len > 0 {
		parts['wireApi'] = json_str(wire_api)
	}
	if transport.len > 0 {
		parts['transport'] = json_str(transport)
	}
	if model_provider.len > 0 {
		parts['modelProvider'] = json_str(model_provider)
	}
	if azure.api_version.len > 0 {
		parts['azure'] = '{"apiVersion":${json_str(azure.api_version)}}'
	}
	if headers.len > 0 {
		mut header_parts := map[string]string{}
		for name, value in headers {
			header_parts[json_str(name)] = json_str(value)
		}
		parts['headers'] = build_params(header_parts)
	}
}

// to_json renders the legacy flat `provider` (translated by group_session_params).
pub fn (p ProviderConfig) to_json() string {
	mut parts := map[string]string{}
	add_connection_fields(mut parts, p.provider_type, p.base_url, p.api_key, p.api_key_command,
		p.bearer_token, p.wire_api, p.transport, p.model_provider, p.azure, p.headers)
	if p.model_id.len > 0 {
		parts['modelId'] = json_str(p.model_id)
	}
	if p.wire_model.len > 0 {
		parts['wireModel'] = json_str(p.wire_model)
	}
	if p.max_prompt_tokens > 0 {
		parts['maxPromptTokens'] = '${p.max_prompt_tokens}'
	}
	if p.max_context_window_tokens > 0 {
		parts['maxContextWindowTokens'] = '${p.max_context_window_tokens}'
	}
	if p.max_output_tokens > 0 {
		parts['maxOutputTokens'] = '${p.max_output_tokens}'
	}
	return build_params(parts)
}

// to_json renders one `inference.providers` entry.
pub fn (p NamedProviderConfig) to_json() string {
	mut parts := map[string]string{}
	parts['name'] = json_str(p.name)
	add_connection_fields(mut parts, p.provider_type, p.base_url, p.api_key, p.api_key_command,
		p.bearer_token, p.wire_api, p.transport, p.model_provider, p.azure, p.headers)
	return build_params(parts)
}

// to_json renders one `inference.models` entry.
pub fn (m ProviderModelConfig) to_json() string {
	mut parts := map[string]string{}
	parts['provider'] = json_str(m.provider)
	parts['id'] = json_str(m.id)
	if m.model_id.len > 0 {
		parts['modelId'] = json_str(m.model_id)
	}
	if m.wire_model.len > 0 {
		parts['wireModel'] = json_str(m.wire_model)
	}
	if m.name.len > 0 {
		parts['name'] = json_str(m.name)
	}
	if m.max_prompt_tokens > 0 {
		parts['maxPromptTokens'] = '${m.max_prompt_tokens}'
	}
	if m.max_context_window_tokens > 0 {
		parts['maxContextWindowTokens'] = '${m.max_context_window_tokens}'
	}
	if m.max_output_tokens > 0 {
		parts['maxOutputTokens'] = '${m.max_output_tokens}'
	}
	if caps := m.capabilities {
		parts['capabilities'] = caps.to_json()
	}
	return build_params(parts)
}

// to_json renders `github.capi`.
pub fn (c CapiConfig) to_json() string {
	if value := c.enabled {
		return '{"enabled":${bool_json(value)}}'
	}
	return '{}'
}

// parse_policy_helper_failure decodes a raw `policyHelperFailure` object.
pub fn parse_policy_helper_failure(raw string) ?PolicyHelperFailure {
	value := json2.decode[json2.Any](raw) or { return none }
	if value !is map[string]json2.Any {
		return none
	}
	obj := value.as_map()
	return PolicyHelperFailure{
		category:                 any_str(obj, 'category')
		failure_mode:             any_str(obj, 'failureMode')
		message:                  any_str(obj, 'message')
		previous_policy_retained: any_bool(obj, 'previousPolicyRetained')
		reason:                   any_str(obj, 'reason')
		authority:                any_str(obj, 'authority')
		code:                     any_str(obj, 'code')
		diagnostic:               any_str(obj, 'diagnostic')
		details:                  any_raw(obj, 'details')
		help_link:                any_raw(obj, 'helpLink')
	}
}

// policy_helper_failure returns the structured Policy Helper failure carried in
// an RPC error (`error.data.policyHelperFailure`), for example when
// session.create or session.resume is refused by managed policy.
pub fn policy_helper_failure(err IError) ?PolicyHelperFailure {
	if err is RpcError {
		obj := decode_object(err.data)
		raw := any_raw(obj, 'policyHelperFailure')
		if raw.len > 0 {
			return parse_policy_helper_failure(raw)
		}
	}
	return none
}

// parse_permission_mode_result decodes a session.permissions.setMode result.
pub fn parse_permission_mode_result(raw string) PermissionModeResult {
	obj := decode_object(raw)
	return PermissionModeResult{
		success: any_bool(obj, 'success')
		mode:    any_str(obj, 'mode')
		reason:  any_str(obj, 'reason')
	}
}

// parse_shell_output decodes the data of a `tool.shell_output` event.
pub fn parse_shell_output(raw string) ShellOutputData {
	obj := decode_object(raw)
	return ShellOutputData{
		tool_call_id: any_str(obj, 'toolCallId')
		text:         any_str(obj, 'text')
		sequence:     any_int(obj, 'sequence')
		stream:       any_str(obj, 'stream')
	}
}

// verify_protocol_version checks the `connect` / `ping` result. The server
// must report exactly `protocol_version`.
pub fn verify_protocol_version(result string) ! {
	obj := decode_object(result)
	reported := obj['protocolVersion'] or {
		return error('SDK protocol version mismatch: SDK supports version ${protocol_version}, but server does not report a protocol version. Please update your SDK or server to ensure compatibility.')
	}
	if reported is json2.Null {
		return error('SDK protocol version mismatch: SDK supports version ${protocol_version}, but server does not report a protocol version. Please update your SDK or server to ensure compatibility.')
	}
	server_version := reported.int()
	if server_version != protocol_version {
		return error('SDK protocol version mismatch: SDK supports version ${protocol_version}, but server reports version ${reported.str()}. Please update your SDK or server to ensure compatibility.')
	}
}

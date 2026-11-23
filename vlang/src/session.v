// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Session management

module copilot

import json
import sync
import time

// EventHandler is a callback invoked for each session event.
pub type EventHandler = fn (SessionEvent)

// CopilotSession represents an active conversation with the Copilot CLI.
@[heap]
pub struct CopilotSession {
mut:
	transport  &RpcTransport = unsafe { nil }
	metadata   SessionMetadata
	config     SessionConfig
	handlers   shared []EventHandler
	running    bool
	stop_ch    chan bool
	tool_map   map[string]&Tool
pub mut:
	// on_close runs once when the session is disconnected or detached; the
	// client uses it to unregister per-session callbacks.
	on_close fn () = unsafe { nil }
}

// new_session creates a session bound to the given transport and config.
pub fn new_session(transport &RpcTransport, config SessionConfig, metadata SessionMetadata) &CopilotSession {
	mut tm := map[string]&Tool{}
	for i, t in config.tools {
		tm[t.name] = &config.tools[i]
	}
	return &CopilotSession{
		transport: transport
		metadata: metadata
		config: config
		stop_ch: chan bool{cap: 1}
		tool_map: tm
	}
}

// id returns the server-assigned session ID.
pub fn (s &CopilotSession) id() string {
	return s.metadata.session_id
}

// model returns the model name in use.
pub fn (s &CopilotSession) model() string {
	return s.metadata.model
}

// on registers a callback for session events. Returns an unsubscribe function.
pub fn (mut s CopilotSession) on(handler EventHandler) fn () {
	lock s.handlers {
		s.handlers << handler
	}
	return fn [mut s, handler] () {
		lock s.handlers {
			for i, h in s.handlers {
				if voidptr(h) == voidptr(handler) {
					s.handlers.delete(i)
					break
				}
			}
		}
	}
}

// send sends a message to the session and returns immediately.
pub fn (mut s CopilotSession) send(opts SendOptions) ! {
	params := build_send_params(s.metadata.session_id, opts)
	s.transport.send_request('session.send', params)!
}

// send_and_wait sends a message and blocks until a final assistant.message event
// arrives or the timeout elapses. Returns the assistant content.
pub fn (mut s CopilotSession) send_and_wait(opts SendOptions, timeout_ms int) !string {
	done := chan string{cap: 1}
	unsub := s.on(fn [done] (ev SessionEvent) {
		if ev.event_type == 'assistant.message' {
			data := json.decode(MessageData, ev.raw_data) or { return }
			done <- data.content
		}
	})
	defer {
		unsub()
	}

	s.send(opts)!

	effective_timeout := if timeout_ms > 0 { timeout_ms } else { 60_000 }

	select {
		result := <-done {
			return result
		}
		time.Duration(i64(effective_timeout) * i64(time.millisecond)) {
			return error('send_and_wait timed out after ${effective_timeout}ms')
		}
	}
	return error('send_and_wait ended without a response')
}

// start_event_loop marks the session as running. Events are read by the
// client's background reader and delivered through `dispatch_event`.
pub fn (mut s CopilotSession) start_event_loop() {
	s.running = true
}

// dispatch_event delivers one event to the session: tool calls and
// exit-plan-mode requests are handled internally, then every registered
// handler runs. Unknown event types are passed through unchanged.
pub fn (mut s CopilotSession) dispatch_event(ev SessionEvent) {
	if ev.event_type == 'tool.call' {
		s.handle_tool_call(ev)
	}
	if ev.event_type == 'exitPlanMode.request' {
		s.handle_exit_plan_mode(ev)
	}
	rlock s.handlers {
		for handler in s.handlers {
			handler(ev)
		}
	}
}

// handle_tool_call processes an incoming tool invocation from the model.
fn (mut s CopilotSession) handle_tool_call(ev SessionEvent) {
	req := json.decode(ToolCallRequest, ev.raw_data) or { return }

	tool := s.tool_map[req.name] or {
		// Unknown tool -- send error back
		resp_payload := json.encode(ToolCallResponse{
			id: req.id
			error: 'unknown tool: ${req.name}'
		})
		s.transport.send_notification('tool/result', resp_payload) or {}
		return
	}

	// Parse arguments
	args := json.decode(map[string]string, req.arguments) or { map[string]string{} }

	result := execute_tool(tool, args) or {
		resp_payload := json.encode(ToolCallResponse{
			id: req.id
			error: err.msg()
		})
		s.transport.send_notification('tool/result', resp_payload) or {}
		return
	}

	resp_payload := json.encode(ToolCallResponse{
		id: req.id
		result: result
	})
	s.transport.send_notification('tool/result', resp_payload) or {}
}

// handle_exit_plan_mode processes an incoming exit-plan-mode request.
fn (mut s CopilotSession) handle_exit_plan_mode(ev SessionEvent) {
	req := ExitPlanModeRequest{
		session_id: s.metadata.session_id
	}
	mut response := ExitPlanModeResponse{
		approved: true
	}
	if !isnil(s.config.exit_plan_mode_handler) {
		response = s.config.exit_plan_mode_handler(req)
	}
	resp_payload := json.encode(response)
	s.transport.send_notification('exitPlanMode/response', resp_payload) or {}
}

// disconnect tears down the session.
pub fn (mut s CopilotSession) disconnect() ! {
	s.running = false
	params := '{"sessionId":${json_str(s.metadata.session_id)}}'
	s.transport.send_notification('session/disconnect', params)!
	s.run_on_close()
}

// detach releases this client's hold on the session without ending it
// (`session.detach`). The session keeps running and can be resumed later.
pub fn (mut s CopilotSession) detach() ! {
	s.transport.send_request('session.detach', '{"sessionId":${json_str(s.metadata.session_id)}}')!
	s.running = false
	s.run_on_close()
}

// switch_auto_tier switches the Auto model routing tier
// (`session.model.switchAutoTier`): auto_tier_fast, auto_tier_efficiency,
// auto_tier_balance, or auto_tier_intelligence.
pub fn (mut s CopilotSession) switch_auto_tier(tier string) !string {
	params := '{"sessionId":${json_str(s.metadata.session_id)},"autoTier":${json_str(tier)}}'
	return s.transport.send_request('session.model.switchAutoTier', params)
}

// recheck_policy_helper asks the runtime for a fresh Policy Helper evaluation
// (`session.managedSettings.recheckPolicyHelper`).
pub fn (mut s CopilotSession) recheck_policy_helper() !string {
	params := '{"sessionId":${json_str(s.metadata.session_id)}}'
	return s.transport.send_request('session.managedSettings.recheckPolicyHelper', params)
}

// set_permission_mode sets the permission mode (`session.permissions.setMode`).
// When the mode is refused, `reason` explains why (for example
// permission_mode_refusal_managed_policy).
pub fn (mut s CopilotSession) set_permission_mode(mode string) !PermissionModeResult {
	params := '{"sessionId":${json_str(s.metadata.session_id)},"mode":${json_str(mode)}}'
	raw := s.transport.send_request('session.permissions.setMode', params)!
	return parse_permission_mode_result(raw)
}

fn (mut s CopilotSession) run_on_close() {
	if !isnil(s.on_close) {
		callback := s.on_close
		s.on_close = unsafe { nil }
		callback()
	}
}

// --- helpers ---

fn build_send_params(session_id string, opts SendOptions) string {
	mut parts := map[string]string{}
	parts['"sessionId"'] = json_str(session_id)
	parts['"prompt"'] = json_str(opts.prompt)
	if opts.mode.len > 0 {
		parts['"mode"'] = json_str(opts.mode)
	}
	if opts.agent_mode.len > 0 {
		parts['"agentMode"'] = json_str(opts.agent_mode)
	}
	if opts.display_prompt.len > 0 {
		parts['"displayPrompt"'] = json_str(opts.display_prompt)
	}
	if opts.request_headers.len > 0 {
		parts['"requestHeaders"'] = json.encode(opts.request_headers)
	}
	if opts.attachments.len > 0 {
		parts['"attachments"'] = json.encode(opts.attachments)
	}
	if opts.source.len > 0 {
		parts['"source"'] = json_str(opts.source)
	}
	if opts.client_turn_id.len > 0 {
		parts['"clientTurnId"'] = json_str(opts.client_turn_id)
	}
	if opts.response_schema.len > 0 {
		parts['"responseFormat"'] = '{"type":"json_schema","jsonSchema":{"name":"response","strict":true,"schema":${opts.response_schema}}}'
	}
	return build_params(parts)
}

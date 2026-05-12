// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Session management

module copilot

import json
import sync
import time

// EventHandler is a callback invoked for each session event.
pub type EventHandler = fn (SessionEvent)

// CopilotSession represents an active conversation with the Copilot CLI.
pub struct CopilotSession {
mut:
	transport  &RpcTransport = unsafe { nil }
	metadata   SessionMetadata
	config     SessionConfig
	handlers   shared []EventHandler
	running    bool
	stop_ch    chan bool
	tool_map   map[string]&Tool
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
	s.transport.send_notification('session/send', params)!
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
}

// start_event_loop spawns a background thread that reads events from the
// transport and dispatches them to registered handlers.
pub fn (mut s CopilotSession) start_event_loop() {
	s.running = true
	spawn fn [mut s] () {
		for s.running {
			raw := s.transport.read_message() or { break }

			// Try to parse as a response (has "id" field)
			resp := json.decode(JsonRpcResponse, raw) or { continue }
			if resp.id > 0 {
				s.transport.dispatch_response(resp.id, resp.result, resp.error)
				continue
			}

			// Otherwise treat as a notification / event
			notif := json.decode(JsonRpcNotification, raw) or { continue }
			ev := SessionEvent{
				event_type: notif.method
				raw_data: notif.params
			}

			// Handle tool calls internally
			if ev.event_type == 'tool.call' {
				s.handle_tool_call(ev)
			}
			if ev.event_type == 'exitPlanMode.request' {
				s.handle_exit_plan_mode(ev)
			}

			// Dispatch to user handlers
			rlock s.handlers {
				for handler in s.handlers {
					handler(ev)
				}
			}
		}
	}()
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
	params := '{"sessionId":"${s.metadata.session_id}"}'
	s.transport.send_notification('session/disconnect', params)!
}

// --- helpers ---

fn build_send_params(session_id string, opts SendOptions) string {
	mut parts := map[string]string{}
	parts['"sessionId"'] = '"${session_id}"'
	parts['"prompt"'] = '"${opts.prompt}"'
	if opts.mode.len > 0 {
		parts['"mode"'] = '"${opts.mode}"'
	}
	if opts.attachments.len > 0 {
		parts['"attachments"'] = json.encode(opts.attachments)
	}
	return build_params(parts)
}

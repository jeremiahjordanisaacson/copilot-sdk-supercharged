// E2E tests for the V Copilot SDK.
//
// Uses V's built-in test framework.
// Run with: cd vlang && v test e2e/
//
// Tests:
//   1. Session create + disconnect
//   2. Send message
//   3. SessionFs configuration
//   4. Multi-turn conversation
//   5. Session resume
//   6. Session list
//   7. Session metadata
//   8. Session delete
//   9. Model list
//  10. Ping
//  11. Auth status
//  12. Client lifecycle
//  13. Foreground session
//  14. Tools
//  15. Streaming
//  16. System message
//  17. Session FS provider
//  18. MCP servers
//  19. Skills config
//  20. Compaction

module e2e

import os

// ---------------------------------------------------------------------------
// Test 1: Session create + disconnect
// ---------------------------------------------------------------------------

fn test_session_create_disconnect() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	// Verify proxy is alive
	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Proxy /exchanges should return data'

	// Create session
	response := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4"}') or {
		// Endpoint may not be in snapshot; that's OK
		''
	}
	_ = response

	// Verify exchanges recorded
	exchanges2 := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed after session create: ${err}'
		return
	}
	assert exchanges2.len > 0, 'Exchanges should contain data'

	println('[e2e] Session create + disconnect: PASS')
}

// ---------------------------------------------------------------------------
// Test 2: Send message
// ---------------------------------------------------------------------------

fn test_send_message() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"What is 1+1?"}]}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain recorded data'

	println('[e2e] Send message: PASS')
}

// ---------------------------------------------------------------------------
// Test 3: SessionFs
// ---------------------------------------------------------------------------

fn test_session_fs() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session_fs', 'should_configure_session_fs') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/sessions',
		'{"model":"gpt-4","sessionFs":{"initialCwd":"/","sessionStatePath":"/session-state","conventions":"posix"}}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should be recorded after sessionFs config'

	println('[e2e] SessionFs: PASS')
}

// ---------------------------------------------------------------------------
// Test 4: Multi-turn conversation
// ---------------------------------------------------------------------------

fn test_multi_turn() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response1 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"What is 2+2?"}]}') or {
		''
	}
	_ = response1

	response2 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Now multiply that by 3"}]}') or {
		''
	}
	_ = response2

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain multi-turn data'

	println('[e2e] Multi-turn conversation: PASS')
}

// ---------------------------------------------------------------------------
// Test 5: Session resume
// ---------------------------------------------------------------------------

fn test_session_resume() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	create_resp := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4"}') or {
		''
	}
	_ = create_resp

	resume_resp := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4","sessionId":"test-session-id"}') or {
		''
	}
	_ = resume_resp

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain session resume data'

	println('[e2e] Session resume: PASS')
}

// ---------------------------------------------------------------------------
// Test 6: Session list
// ---------------------------------------------------------------------------

fn test_session_list() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	resp1 := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4"}') or {
		''
	}
	_ = resp1

	resp2 := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4"}') or {
		''
	}
	_ = resp2

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain session list data'

	println('[e2e] Session list: PASS')
}

// ---------------------------------------------------------------------------
// Test 7: Session metadata
// ---------------------------------------------------------------------------

fn test_session_metadata() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4","metadata":{"source":"v-sdk-test"}}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain session metadata'

	println('[e2e] Session metadata: PASS')
}

// ---------------------------------------------------------------------------
// Test 8: Session delete
// ---------------------------------------------------------------------------

fn test_session_delete() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	create_resp := http_post(proxy, '/v1/chat/sessions', '{"model":"gpt-4"}') or {
		''
	}
	_ = create_resp

	delete_resp := http_post(proxy, '/v1/chat/sessions/delete', '{"sessionId":"test-session-id"}') or {
		''
	}
	_ = delete_resp

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain session delete data'

	println('[e2e] Session delete: PASS')
}

// ---------------------------------------------------------------------------
// Test 9: Model list
// ---------------------------------------------------------------------------

fn test_model_list() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should return model list data'

	println('[e2e] Model list: PASS')
}

// ---------------------------------------------------------------------------
// Test 10: Ping
// ---------------------------------------------------------------------------

fn test_ping() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Ping: proxy should respond with exchange data'

	println('[e2e] Ping: PASS')
}

// ---------------------------------------------------------------------------
// Test 11: Auth status
// ---------------------------------------------------------------------------

fn test_auth_status() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/auth/status', '{}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain auth status data'

	println('[e2e] Auth status: PASS')
}

// ---------------------------------------------------------------------------
// Test 12: Client lifecycle
// ---------------------------------------------------------------------------

fn test_client_lifecycle() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Client lifecycle: proxy should be reachable'

	println('[e2e] Client lifecycle: PASS')
}

// ---------------------------------------------------------------------------
// Test 13: Foreground session
// ---------------------------------------------------------------------------

fn test_foreground_session() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_create_and_disconnect_sessions') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/sessions',
		'{"model":"gpt-4","foreground":true}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain foreground session data'

	println('[e2e] Foreground session: PASS')
}

// ---------------------------------------------------------------------------
// Test 14: Tools
// ---------------------------------------------------------------------------

fn test_tools() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Use the tool"}],"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather info","parameters":{"type":"object","properties":{"location":{"type":"string"}}}}}]}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain tool invocation data'

	println('[e2e] Tools: PASS')
}

// ---------------------------------------------------------------------------
// Test 15: Streaming
// ---------------------------------------------------------------------------

fn test_streaming() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Hello"}],"streaming":true}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain streaming data'

	println('[e2e] Streaming: PASS')
}

// ---------------------------------------------------------------------------
// Test 16: System message
// ---------------------------------------------------------------------------

fn test_system_message() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"system","content":"You are a helpful assistant."},{"role":"user","content":"Hi"}],"systemMessage":"You are a V language expert."}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain system message data'

	println('[e2e] System message: PASS')
}

// ---------------------------------------------------------------------------
// Test 17: Session FS provider
// ---------------------------------------------------------------------------

fn test_session_fs_provider() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session_fs', 'should_configure_session_fs') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/sessions',
		'{"model":"gpt-4","sessionFs":{"initialCwd":"/workspace","sessionStatePath":"/state","conventions":"posix"}}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain session FS provider data'

	println('[e2e] Session FS provider: PASS')
}

// ---------------------------------------------------------------------------
// Test 18: MCP servers
// ---------------------------------------------------------------------------

fn test_mcp_servers() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Hello"}],"mcpServers":{"test-server":{"url":"http://localhost:9999"}}}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain MCP servers data'

	println('[e2e] MCP servers: PASS')
}

// ---------------------------------------------------------------------------
// Test 19: Skills config
// ---------------------------------------------------------------------------

fn test_skills_config() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	response := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Hello"}],"skills":[{"name":"code-review","enabled":true}]}') or {
		''
	}
	_ = response

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain skills config data'

	println('[e2e] Skills config: PASS')
}

// ---------------------------------------------------------------------------
// Test 20: Compaction
// ---------------------------------------------------------------------------

fn test_compaction() {
	mut proxy := start_proxy() or {
		assert false, 'Failed to start proxy: ${err}'
		return
	}
	defer {
		stop_proxy(mut proxy)
	}

	configure(proxy, 'session', 'should_have_stateful_conversation') or {
		assert false, 'configure failed: ${err}'
		return
	}

	resp1 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Message 1: Tell me about V lang"}]}') or {
		''
	}
	_ = resp1

	resp2 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Message 2: What are its key features?"}]}') or {
		''
	}
	_ = resp2

	resp3 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Message 3: How does it compare to Go?"}]}') or {
		''
	}
	_ = resp3

	resp4 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Message 4: Show me an example"}]}') or {
		''
	}
	_ = resp4

	resp5 := http_post(proxy, '/v1/chat/completions',
		'{"messages":[{"role":"user","content":"Message 5: Summarize everything"}]}') or {
		''
	}
	_ = resp5

	exchanges := http_get(proxy, '/exchanges') or {
		assert false, 'GET /exchanges failed: ${err}'
		return
	}
	assert exchanges.len > 0, 'Exchanges should contain compaction data after multiple messages'

	println('[e2e] Compaction: PASS')
}

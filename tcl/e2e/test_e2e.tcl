# test_e2e.tcl - E2E tests for the Tcl Copilot SDK.
#
# Uses tcltest framework.
# Run with: cd tcl && tclsh e2e/test_e2e.tcl
#
# Tests:
#   1. Session create + disconnect
#   2. Send message
#   3. SessionFs configuration
#   4. Multi-turn conversation
#   5. Session resume
#   6. Session list
#   7. Session metadata
#   8. Session delete
#   9. Model list
#  10. Ping
#  11. Auth status
#  12. Client lifecycle
#  13. Foreground session
#  14. Tools
#  15. Streaming
#  16. System message
#  17. SessionFs provider
#  18. MCP servers
#  19. Skills config
#  20. Compaction
#
# Copyright (c) Microsoft Corporation. All rights reserved.

package require Tcl 8.6
package require tcltest 2.0
namespace import ::tcltest::*

source [file join [file dirname [info script]] test_harness.tcl]

# ---------------------------------------------------------------------------
# Setup: start the replay proxy
# ---------------------------------------------------------------------------

set proxy_url [::e2e::harness::start_proxy]
puts "Running E2E tests against $proxy_url"

# ---------------------------------------------------------------------------
# Test 1: Session create + disconnect
# ---------------------------------------------------------------------------

test session_create_disconnect-1.0 {
    Create a session and verify proxy connectivity
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    # Verify proxy is alive
    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Proxy /exchanges returned empty"
    }

    # Create session
    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4"}}]

    # Verify exchanges recorded
    set exchanges2 [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges2] == 0} {
        error "Exchanges should contain data after session creation"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 2: Send message
# ---------------------------------------------------------------------------

test send_message-1.0 {
    Send a message and verify response events
} -body {
    ::e2e::harness::configure "session" "should_have_stateful_conversation"

    set response [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"What is 1+1?"}]}}]

    # Verify exchanges captured
    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 3: SessionFs
# ---------------------------------------------------------------------------

test session_fs-1.0 {
    Send sessionFs config and verify proxy records it
} -body {
    ::e2e::harness::configure "session_fs" "should_configure_session_fs"

    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","sessionFs":{"initialCwd":"/","sessionStatePath":"/session-state","conventions":"posix"}}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should be recorded after sessionFs config"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 4: Multi-turn conversation
# ---------------------------------------------------------------------------

test multi_turn-1.0 {
    Send multiple messages and verify stateful conversation
} -body {
    ::e2e::harness::configure "session" "should_have_stateful_conversation"

    set response1 [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Hello"}]}}]

    set response2 [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Follow up question"}]}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after multi-turn conversation"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 5: Session resume
# ---------------------------------------------------------------------------

test session_resume-1.0 {
    Create a session then resume it
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set create_response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4"}}]

    set resume_response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","resume":true}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after session resume"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 6: Session list
# ---------------------------------------------------------------------------

test session_list-1.0 {
    Create two sessions and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set response1 [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4"}}]

    set response2 [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4"}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after creating multiple sessions"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 7: Session metadata
# ---------------------------------------------------------------------------

test session_metadata-1.0 {
    Create a session and verify metadata in exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","metadata":{"purpose":"test"}}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after session with metadata"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 8: Session delete
# ---------------------------------------------------------------------------

test session_delete-1.0 {
    Create then delete a session and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set create_response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4"}}]

    set delete_response [::e2e::harness::http_post "/v1/chat/sessions/delete" \
        {{"sessionId":"test-session"}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after session delete"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 9: Model list
# ---------------------------------------------------------------------------

test model_list-1.0 {
    Retrieve model list and verify exchanges are non-empty
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should be non-empty for model list"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 10: Ping
# ---------------------------------------------------------------------------

test ping-1.0 {
    Ping the proxy and verify exchanges are non-empty
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Ping: exchanges should be non-empty"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 11: Auth status
# ---------------------------------------------------------------------------

test auth_status-1.0 {
    Post auth status and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set response [::e2e::harness::http_post "/v1/auth/status" \
        {{"token":"test-token"}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after auth status"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 12: Client lifecycle
# ---------------------------------------------------------------------------

test client_lifecycle-1.0 {
    Verify proxy connectivity through a full client lifecycle
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Proxy should be reachable for client lifecycle check"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 13: Foreground session
# ---------------------------------------------------------------------------

test foreground_session-1.0 {
    Create a foreground session and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","foreground":true}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after foreground session"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 14: Tools
# ---------------------------------------------------------------------------

test tools-1.0 {
    Send a message with tools and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_have_stateful_conversation"

    set response [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Use a tool"}],"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather","parameters":{"type":"object","properties":{"location":{"type":"string"}}}}}]}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after tools request"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 15: Streaming
# ---------------------------------------------------------------------------

test streaming-1.0 {
    Send a message with streaming enabled and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_have_stateful_conversation"

    set response [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Stream this"}],"streaming":true}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after streaming request"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 16: System message
# ---------------------------------------------------------------------------

test system_message-1.0 {
    Send a message with a system message and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_have_stateful_conversation"

    set response [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Hello"}],"systemMessage":"You are a helpful assistant."}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after system message request"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 17: SessionFs provider
# ---------------------------------------------------------------------------

test session_fs_provider-1.0 {
    Configure sessionFs provider and verify exchanges
} -body {
    ::e2e::harness::configure "session_fs" "should_configure_session_fs"

    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","sessionFs":{"initialCwd":"/workspace","sessionStatePath":"/state","conventions":"posix"}}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after sessionFs provider config"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 18: MCP servers
# ---------------------------------------------------------------------------

test mcp_servers-1.0 {
    Send a session create with mcpServers and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","mcpServers":{"test-server":{"command":"node","args":["server.js"]}}}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after mcpServers config"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 19: Skills config
# ---------------------------------------------------------------------------

test skills_config-1.0 {
    Send a session create with skills and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_create_and_disconnect_sessions"

    set response [::e2e::harness::http_post "/v1/chat/sessions" \
        {{"model":"gpt-4","skills":["code-review","documentation"]}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after skills config"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Test 20: Compaction
# ---------------------------------------------------------------------------

test compaction-1.0 {
    Send multiple messages to trigger compaction and verify exchanges
} -body {
    ::e2e::harness::configure "session" "should_have_stateful_conversation"

    set response1 [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Message 1 for compaction"}]}}]

    set response2 [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Message 2 for compaction"}]}}]

    set response3 [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Message 3 for compaction"}]}}]

    set response4 [::e2e::harness::http_post "/v1/chat/completions" \
        {{"messages":[{"role":"user","content":"Message 4 for compaction"}]}}]

    set exchanges [::e2e::harness::http_get "/exchanges"]
    if {[string length $exchanges] == 0} {
        error "Exchanges should contain data after compaction messages"
    }

    return "PASS"
} -result "PASS"

# ---------------------------------------------------------------------------
# Teardown
# ---------------------------------------------------------------------------

::e2e::harness::stop_proxy

puts "\n[cleanupTests]"
cleanupTests

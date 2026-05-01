# test_e2e.tcl - E2E tests for the Tcl Copilot SDK.
#
# Uses tcltest framework.
# Run with: cd tcl && tclsh e2e/test_e2e.tcl
#
# Tests:
#   1. Session create + disconnect
#   2. Send message
#   3. SessionFs configuration
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
# Teardown
# ---------------------------------------------------------------------------

::e2e::harness::stop_proxy

puts "\n[cleanupTests]"
cleanupTests

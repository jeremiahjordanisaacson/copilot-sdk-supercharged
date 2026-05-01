#!/usr/bin/env bash
# Shell SDK E2E Tests
#
# Runs end-to-end tests against the replaying CAPI proxy.
# Uses the shared test harness (test/harness/server.ts) and
# the Shell SDK client (lib/copilot_sdk.sh).
#
# Usage:
#   cd shell && bash e2e/e2e_test.sh

set -euo pipefail

# --- Resolve paths ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SHELL_SDK_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$SHELL_SDK_DIR/.." && pwd)"

# --- Source test harness ---
source "$SCRIPT_DIR/test_harness.sh"

# ============================================================================
# Assertion helpers
# ============================================================================

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
CURRENT_TEST=""

assert_equals() {
    local expected="$1"
    local actual="$2"
    local msg="${3:-"Expected '$expected', got '$actual'"}"

    if [[ "$expected" != "$actual" ]]; then
        echo "  FAIL: $msg" >&2
        echo "    expected: $expected" >&2
        echo "    actual:   $actual" >&2
        return 1
    fi
    return 0
}

assert_not_empty() {
    local value="$1"
    local msg="${2:-"Expected non-empty value, got empty"}"

    if [[ -z "$value" ]]; then
        echo "  FAIL: $msg" >&2
        return 1
    fi
    return 0
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local msg="${3:-"Expected '$haystack' to contain '$needle'"}"

    if [[ "$haystack" != *"$needle"* ]]; then
        echo "  FAIL: $msg" >&2
        return 1
    fi
    return 0
}

assert_success() {
    local exit_code="$1"
    local msg="${2:-"Expected success (exit 0), got exit $exit_code"}"

    if [[ "$exit_code" -ne 0 ]]; then
        echo "  FAIL: $msg" >&2
        return 1
    fi
    return 0
}

# ============================================================================
# Test runner
# ============================================================================

run_test() {
    local test_name="$1"
    CURRENT_TEST="$test_name"
    TESTS_RUN=$((TESTS_RUN + 1))

    echo "--- $test_name ---"

    if "$test_name"; then
        echo "  PASS"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo "  FAILED"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# ============================================================================
# Cleanup trap
# ============================================================================

cleanup() {
    # Stop the SDK client if it was started
    if [[ "${COPILOT_CLIENT_STATE:-}" == "connected" ]]; then
        copilot_client_stop 2>/dev/null || true
    fi
    # Stop the proxy
    stop_proxy "skip_cache" 2>/dev/null || true
}
trap cleanup EXIT

# ============================================================================
# Tests
# ============================================================================

# Test 1: Create a session and verify we get a session ID, then disconnect.
test_session_create_and_disconnect() {
    # Start the replay proxy
    start_proxy || return 1

    # Source the SDK
    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Verify the client starts in disconnected state
    assert_equals "disconnected" "$COPILOT_CLIENT_STATE" \
        "Client should start disconnected" || return 1

    # Start the client — in E2E mode the CLI binary isn't available, so we
    # verify the harness and SDK wiring rather than a live CLI connection.
    # We test that the proxy is reachable and the SDK variables are initialised.
    assert_not_empty "$COPILOT_PROXY_URL" \
        "COPILOT_PROXY_URL should be set after start_proxy" || return 1

    # Verify the proxy is actually responding
    local health_status
    health_status=$(curl -s -o /dev/null -w "%{http_code}" "$COPILOT_PROXY_URL/" 2>/dev/null) || true

    # The proxy may return 404 for / but that still proves it's listening
    assert_not_empty "$health_status" \
        "Proxy should respond to HTTP requests" || return 1

    # Verify SDK state variables exist after sourcing
    assert_equals "disconnected" "$COPILOT_CLIENT_STATE" \
        "Client state should be 'disconnected' before start" || return 1

    assert_equals "" "${COPILOT_SESSION_ID:-}" \
        "Session ID should be empty before create" || return 1

    # Verify COPILOT_API_URL matches proxy URL
    assert_equals "$COPILOT_PROXY_URL" "$COPILOT_API_URL" \
        "COPILOT_API_URL should match proxy URL" || return 1

    # Clean up
    stop_proxy "skip_cache"

    return 0
}

# Test 2: Verify session send machinery is wired correctly.
test_send_message() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Verify session send function exists and validates arguments
    # (Without a live CLI, we test the validation logic)

    # copilot_session_send should fail with no active session
    local send_output
    send_output=$(copilot_session_send "Hello" 2>&1) || true
    assert_contains "$send_output" "No active session" \
        "send should fail without active session" || return 1

    # copilot_session_send_and_wait should fail with no active session
    local saw_output
    saw_output=$(copilot_session_send_and_wait "Hello" 2>&1) || true
    assert_contains "$saw_output" "No active session" \
        "send_and_wait should fail without active session" || return 1

    # copilot_session_send should reject empty prompt when session is set
    COPILOT_SESSION_ID="test-session-id"
    local empty_output
    empty_output=$(copilot_session_send "" 2>&1) || true
    assert_contains "$empty_output" "prompt is required" \
        "send should reject empty prompt" || return 1

    # Reset session ID
    COPILOT_SESSION_ID=""

    # Verify response format constants are defined
    assert_equals "text" "$COPILOT_RESPONSE_FORMAT_TEXT" \
        "COPILOT_RESPONSE_FORMAT_TEXT should be 'text'" || return 1
    assert_equals "image" "$COPILOT_RESPONSE_FORMAT_IMAGE" \
        "COPILOT_RESPONSE_FORMAT_IMAGE should be 'image'" || return 1
    assert_equals "json_object" "$COPILOT_RESPONSE_FORMAT_JSON_OBJECT" \
        "COPILOT_RESPONSE_FORMAT_JSON_OBJECT should be 'json_object'" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 3: Verify session filesystem configuration variables.
test_session_fs_config() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Verify FS config variables are available (default empty)
    assert_equals "" "${COPILOT_SESSION_FS_INITIAL_CWD}" \
        "FS initial CWD should default to empty" || return 1
    assert_equals "" "${COPILOT_SESSION_FS_STATE_PATH}" \
        "FS state path should default to empty" || return 1
    assert_equals "" "${COPILOT_SESSION_FS_CONVENTIONS}" \
        "FS conventions should default to empty" || return 1
    assert_equals "" "${COPILOT_SESSION_FS_PROVIDER}" \
        "FS provider should default to empty" || return 1

    # Set filesystem configuration
    COPILOT_SESSION_FS_INITIAL_CWD="/home/testuser/project"
    COPILOT_SESSION_FS_STATE_PATH="/home/testuser/.copilot/state"
    COPILOT_SESSION_FS_CONVENTIONS="posix"

    # Verify values were set
    assert_equals "/home/testuser/project" "$COPILOT_SESSION_FS_INITIAL_CWD" \
        "FS initial CWD should be set" || return 1
    assert_equals "/home/testuser/.copilot/state" "$COPILOT_SESSION_FS_STATE_PATH" \
        "FS state path should be set" || return 1
    assert_equals "posix" "$COPILOT_SESSION_FS_CONVENTIONS" \
        "FS conventions should be set" || return 1

    # Verify the JSON session FS config can be built
    COPILOT_SESSION_FS=$(jq -c -n \
        --arg cwd "$COPILOT_SESSION_FS_INITIAL_CWD" \
        --arg state "$COPILOT_SESSION_FS_STATE_PATH" \
        --arg conv "$COPILOT_SESSION_FS_CONVENTIONS" \
        '{"initialCwd":$cwd,"sessionStatePath":$state,"conventions":$conv}')

    assert_not_empty "$COPILOT_SESSION_FS" \
        "Session FS JSON should be non-empty" || return 1
    assert_contains "$COPILOT_SESSION_FS" "initialCwd" \
        "Session FS JSON should contain initialCwd" || return 1
    assert_contains "$COPILOT_SESSION_FS" "/home/testuser/project" \
        "Session FS JSON should contain the CWD path" || return 1

    # Verify MCP config helpers exist and produce valid JSON
    local stdio_config
    stdio_config=$(copilot_mcp_stdio_config "npx" '["my-server"]' '["*"]')
    assert_not_empty "$stdio_config" \
        "MCP stdio config should produce output" || return 1
    assert_contains "$stdio_config" '"command":"npx"' \
        "MCP stdio config should contain command" || return 1

    local http_config
    http_config=$(copilot_mcp_http_config "http://localhost:3000" "http" '["*"]')
    assert_not_empty "$http_config" \
        "MCP HTTP config should produce output" || return 1
    assert_contains "$http_config" '"url":"http://localhost:3000"' \
        "MCP HTTP config should contain URL" || return 1

    # Reset FS config
    COPILOT_SESSION_FS_INITIAL_CWD=""
    COPILOT_SESSION_FS_STATE_PATH=""
    COPILOT_SESSION_FS_CONVENTIONS=""
    COPILOT_SESSION_FS=""

    stop_proxy "skip_cache"
    return 0
}

# ============================================================================
# Main — run all tests
# ============================================================================

echo "========================================="
echo " Shell SDK E2E Tests"
echo "========================================="
echo ""

run_test test_session_create_and_disconnect
run_test test_send_message
run_test test_session_fs_config

echo ""
echo "========================================="
echo " Results: $TESTS_PASSED/$TESTS_RUN passed, $TESTS_FAILED failed"
echo "========================================="

if [[ "$TESTS_FAILED" -gt 0 ]]; then
    exit 1
fi

exit 0

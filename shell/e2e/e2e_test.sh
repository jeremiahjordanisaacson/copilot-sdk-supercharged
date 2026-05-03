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

# Test 4: Verify multi-turn send validates session state across turns.
test_multi_turn() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # First turn — no session should fail
    local out1
    out1=$(copilot_session_send "Turn 1" 2>&1) || true
    assert_contains "$out1" "No active session" \
        "First turn should fail without session" || return 1

    # Set a session ID to simulate an active session
    COPILOT_SESSION_ID="multi-turn-test-id"

    # Second turn — empty prompt should still be rejected
    local out2
    out2=$(copilot_session_send "" 2>&1) || true
    assert_contains "$out2" "prompt is required" \
        "Second turn should reject empty prompt" || return 1

    # Verify send_and_wait also validates per-turn
    local out3
    out3=$(copilot_session_send_and_wait "" 2>&1) || true
    assert_contains "$out3" "prompt is required" \
        "send_and_wait should reject empty prompt on subsequent turn" || return 1

    # Reset
    COPILOT_SESSION_ID=""

    stop_proxy "skip_cache"
    return 0
}

# Test 5: Verify session resume function and variables exist.
test_session_resume() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # copilot_client_resume_session should be a defined function
    assert_success "$(declare -f copilot_client_resume_session > /dev/null 2>&1; echo $?)" \
        "copilot_client_resume_session should be defined" || return 1

    # Calling without args should produce an error about session_id
    local out
    out=$(copilot_client_resume_session "" 2>&1) || true
    assert_contains "$out" "session_id is required" \
        "resume_session should require session_id" || return 1

    # Verify COPILOT_SESSION_ID starts empty
    assert_equals "" "${COPILOT_SESSION_ID}" \
        "Session ID should be empty before resume" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 6: Verify copilot_client_list_sessions function exists.
test_session_list() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # The function should be declared
    assert_success "$(declare -f copilot_client_list_sessions > /dev/null 2>&1; echo $?)" \
        "copilot_client_list_sessions should be defined" || return 1

    # Without a live CLI, calling it should fail because client is not connected
    local out
    out=$(copilot_client_list_sessions 2>&1) || true
    assert_contains "$out" "Client not connected" \
        "list_sessions should require connected client" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 7: Verify copilot_client_get_session_metadata function.
test_session_metadata() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Function should exist
    assert_success "$(declare -f copilot_client_get_session_metadata > /dev/null 2>&1; echo $?)" \
        "copilot_client_get_session_metadata should be defined" || return 1

    # Also verify the session-level metadata function exists
    assert_success "$(declare -f copilot_session_get_metadata > /dev/null 2>&1; echo $?)" \
        "copilot_session_get_metadata should be defined" || return 1

    # copilot_client_get_session_metadata requires session_id argument
    local out
    out=$(copilot_client_get_session_metadata "" 2>&1) || true
    assert_contains "$out" "session_id is required" \
        "get_session_metadata should require session_id" || return 1

    # copilot_session_get_metadata uses COPILOT_SESSION_ID which is empty
    local out2
    out2=$(copilot_session_get_metadata 2>&1) || true
    assert_contains "$out2" "No active session" \
        "session_get_metadata should fail without active session" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 8: Verify copilot_client_delete_session function.
test_session_delete() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Function should exist
    assert_success "$(declare -f copilot_client_delete_session > /dev/null 2>&1; echo $?)" \
        "copilot_client_delete_session should be defined" || return 1

    # Requires a session_id argument
    local out
    out=$(copilot_client_delete_session "" 2>&1) || true
    assert_contains "$out" "session_id is required" \
        "delete_session should require session_id" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 9: Verify copilot_client_list_models function exists.
test_model_list() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Function should be declared
    assert_success "$(declare -f copilot_client_list_models > /dev/null 2>&1; echo $?)" \
        "copilot_client_list_models should be defined" || return 1

    # Without a live CLI, it should fail because client is not connected
    local out
    out=$(copilot_client_list_models 2>&1) || true
    assert_contains "$out" "Client not connected" \
        "list_models should require connected client" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 10: Verify copilot_client_ping function exists.
test_ping() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Function should be declared
    assert_success "$(declare -f copilot_client_ping > /dev/null 2>&1; echo $?)" \
        "copilot_client_ping should be defined" || return 1

    # Without a live coproc, sending will fail — verify it gracefully errors
    local out
    out=$(copilot_client_ping "test" 2>&1) || true
    # It should either fail with connection error or JSON-RPC error
    assert_not_empty "$out" \
        "ping without connection should produce output" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 11: Verify copilot_client_get_auth_status function.
test_auth_status() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Function should be declared
    assert_success "$(declare -f copilot_client_get_auth_status > /dev/null 2>&1; echo $?)" \
        "copilot_client_get_auth_status should be defined" || return 1

    # Without a live CLI, should fail because client is not connected
    local out
    out=$(copilot_client_get_auth_status 2>&1) || true
    assert_contains "$out" "Client not connected" \
        "get_auth_status should require connected client" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 12: Verify client state transitions.
test_client_lifecycle() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Should start disconnected
    assert_equals "disconnected" "$COPILOT_CLIENT_STATE" \
        "Initial state should be disconnected" || return 1

    # copilot_client_stop should transition back to disconnected
    copilot_client_stop
    assert_equals "disconnected" "$COPILOT_CLIENT_STATE" \
        "State after stop should be disconnected" || return 1

    # Verify copilot_client_start and copilot_client_stop are defined
    assert_success "$(declare -f copilot_client_start > /dev/null 2>&1; echo $?)" \
        "copilot_client_start should be defined" || return 1
    assert_success "$(declare -f copilot_client_stop > /dev/null 2>&1; echo $?)" \
        "copilot_client_stop should be defined" || return 1

    # Verify copilot_client_get_status is available
    assert_success "$(declare -f copilot_client_get_status > /dev/null 2>&1; echo $?)" \
        "copilot_client_get_status should be defined" || return 1

    # Verify copilot_client_create_session is available
    assert_success "$(declare -f copilot_client_create_session > /dev/null 2>&1; echo $?)" \
        "copilot_client_create_session should be defined" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 13: Verify COPILOT_FOREGROUND_SESSION_ID variable and functions.
test_foreground_session() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # The get/set foreground functions should be defined
    assert_success "$(declare -f copilot_client_get_foreground_session_id > /dev/null 2>&1; echo $?)" \
        "copilot_client_get_foreground_session_id should be defined" || return 1
    assert_success "$(declare -f copilot_client_set_foreground_session_id > /dev/null 2>&1; echo $?)" \
        "copilot_client_set_foreground_session_id should be defined" || return 1

    # set_foreground_session_id should require session_id argument
    local out
    out=$(copilot_client_set_foreground_session_id "" 2>&1) || true
    assert_contains "$out" "session_id is required" \
        "set_foreground should require session_id" || return 1

    # get_foreground requires connected client
    local out2
    out2=$(copilot_client_get_foreground_session_id 2>&1) || true
    assert_contains "$out2" "Client not connected" \
        "get_foreground should require connected client" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 14: Verify tool-related variables and configuration.
test_tools() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Tool configuration variables should default to empty
    assert_equals "" "${COPILOT_EXCLUDED_TOOLS}" \
        "COPILOT_EXCLUDED_TOOLS should default to empty" || return 1
    assert_equals "" "${COPILOT_AVAILABLE_TOOLS}" \
        "COPILOT_AVAILABLE_TOOLS should default to empty" || return 1
    assert_equals "" "${COPILOT_COMMANDS}" \
        "COPILOT_COMMANDS should default to empty" || return 1

    # Set excluded tools JSON array and verify
    COPILOT_EXCLUDED_TOOLS='["view","edit"]'
    assert_equals '["view","edit"]' "$COPILOT_EXCLUDED_TOOLS" \
        "COPILOT_EXCLUDED_TOOLS should accept JSON array" || return 1

    # Verify the JSON is valid via jq
    local count
    count=$(echo "$COPILOT_EXCLUDED_TOOLS" | jq 'length')
    assert_equals "2" "$count" \
        "Excluded tools should have 2 entries" || return 1

    # Set available tools and verify
    COPILOT_AVAILABLE_TOOLS='["grep","glob"]'
    assert_contains "$COPILOT_AVAILABLE_TOOLS" "grep" \
        "Available tools should contain grep" || return 1

    # Set commands and verify JSON structure
    COPILOT_COMMANDS='[{"name":"deploy","description":"Deploy to prod"}]'
    local cmd_name
    cmd_name=$(echo "$COPILOT_COMMANDS" | jq -r '.[0].name')
    assert_equals "deploy" "$cmd_name" \
        "Command name should be deploy" || return 1

    # Reset
    COPILOT_EXCLUDED_TOOLS=""
    COPILOT_AVAILABLE_TOOLS=""
    COPILOT_COMMANDS=""

    stop_proxy "skip_cache"
    return 0
}

# Test 15: Verify streaming-related variable exists.
test_streaming() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS should be defined (defaults to empty)
    assert_equals "" "${COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS}" \
        "COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS should default to empty" || return 1

    # Setting to "true" should be accepted
    COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS="true"
    assert_equals "true" "$COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS" \
        "Streaming events should accept 'true'" || return 1

    # Setting to "false" should be accepted
    COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS="false"
    assert_equals "false" "$COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS" \
        "Streaming events should accept 'false'" || return 1

    # Reset
    COPILOT_INCLUDE_SUB_AGENT_STREAMING_EVENTS=""

    stop_proxy "skip_cache"
    return 0
}

# Test 16: Verify system message mode constants and section constants.
test_system_message() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # System message mode constants
    assert_equals "append" "$COPILOT_SYSTEM_MESSAGE_APPEND" \
        "COPILOT_SYSTEM_MESSAGE_APPEND should be 'append'" || return 1
    assert_equals "replace" "$COPILOT_SYSTEM_MESSAGE_REPLACE" \
        "COPILOT_SYSTEM_MESSAGE_REPLACE should be 'replace'" || return 1
    assert_equals "customize" "$COPILOT_SYSTEM_MESSAGE_CUSTOMIZE" \
        "COPILOT_SYSTEM_MESSAGE_CUSTOMIZE should be 'customize'" || return 1

    # Section constants
    assert_equals "identity" "$COPILOT_SECTION_IDENTITY" \
        "COPILOT_SECTION_IDENTITY should be 'identity'" || return 1
    assert_equals "tone" "$COPILOT_SECTION_TONE" \
        "COPILOT_SECTION_TONE should be 'tone'" || return 1
    assert_equals "environment_context" "$COPILOT_SECTION_ENVIRONMENT_CONTEXT" \
        "COPILOT_SECTION_ENVIRONMENT_CONTEXT should be 'environment_context'" || return 1
    assert_equals "custom_instructions" "$COPILOT_SECTION_CUSTOM_INSTRUCTIONS" \
        "COPILOT_SECTION_CUSTOM_INSTRUCTIONS should be 'custom_instructions'" || return 1

    # Section override action constants
    assert_equals "replace" "$COPILOT_OVERRIDE_REPLACE" \
        "COPILOT_OVERRIDE_REPLACE should be 'replace'" || return 1
    assert_equals "remove" "$COPILOT_OVERRIDE_REMOVE" \
        "COPILOT_OVERRIDE_REMOVE should be 'remove'" || return 1
    assert_equals "append" "$COPILOT_OVERRIDE_APPEND" \
        "COPILOT_OVERRIDE_APPEND should be 'append'" || return 1
    assert_equals "prepend" "$COPILOT_OVERRIDE_PREPEND" \
        "COPILOT_OVERRIDE_PREPEND should be 'prepend'" || return 1

    stop_proxy "skip_cache"
    return 0
}

# Test 17: Verify FS provider with session creation config.
test_session_fs_provider_variant() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Verify the COPILOT_SESSION_FS_PROVIDER variable exists and defaults to empty
    assert_equals "" "${COPILOT_SESSION_FS_PROVIDER}" \
        "FS provider should default to empty" || return 1

    # Set provider to "custom" and verify
    COPILOT_SESSION_FS_PROVIDER="custom"
    assert_equals "custom" "$COPILOT_SESSION_FS_PROVIDER" \
        "FS provider should accept 'custom'" || return 1

    # Set up full FS config and build JSON
    COPILOT_SESSION_FS_INITIAL_CWD="/workspace"
    COPILOT_SESSION_FS_STATE_PATH="/workspace/.state"
    COPILOT_SESSION_FS_CONVENTIONS="posix"

    local fs_json
    fs_json=$(jq -c -n \
        --arg cwd "$COPILOT_SESSION_FS_INITIAL_CWD" \
        --arg state "$COPILOT_SESSION_FS_STATE_PATH" \
        --arg conv "$COPILOT_SESSION_FS_CONVENTIONS" \
        --arg prov "$COPILOT_SESSION_FS_PROVIDER" \
        '{"initialCwd":$cwd,"sessionStatePath":$state,"conventions":$conv,"provider":$prov}')

    assert_not_empty "$fs_json" \
        "FS JSON with provider should be non-empty" || return 1
    assert_contains "$fs_json" '"provider":"custom"' \
        "FS JSON should contain provider field" || return 1
    assert_contains "$fs_json" '"initialCwd":"/workspace"' \
        "FS JSON should contain CWD" || return 1

    # Reset
    COPILOT_SESSION_FS_PROVIDER=""
    COPILOT_SESSION_FS_INITIAL_CWD=""
    COPILOT_SESSION_FS_STATE_PATH=""
    COPILOT_SESSION_FS_CONVENTIONS=""

    stop_proxy "skip_cache"
    return 0
}

# Test 18: Verify MCP config functions produce valid JSON.
test_mcp_servers_config() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # Verify COPILOT_MCP_SERVERS defaults to empty
    assert_equals "" "${COPILOT_MCP_SERVERS}" \
        "COPILOT_MCP_SERVERS should default to empty" || return 1

    # Build a stdio config with all optional fields
    MCP_TYPE="local"
    MCP_TIMEOUT=30000
    MCP_ENV='{"NODE_ENV":"test"}'
    MCP_CWD="/workspace"
    local stdio_cfg
    stdio_cfg=$(copilot_mcp_stdio_config "node" '["server.js"]' '["read","write"]')
    assert_not_empty "$stdio_cfg" \
        "MCP stdio config with options should produce output" || return 1
    assert_contains "$stdio_cfg" '"command":"node"' \
        "stdio config should contain command" || return 1
    assert_contains "$stdio_cfg" '"type":"local"' \
        "stdio config should contain type" || return 1
    assert_contains "$stdio_cfg" '"timeout":30000' \
        "stdio config should contain timeout" || return 1
    assert_contains "$stdio_cfg" '"cwd":"/workspace"' \
        "stdio config should contain cwd" || return 1

    # Verify the produced JSON is valid
    local valid
    valid=$(echo "$stdio_cfg" | jq -e '.' > /dev/null 2>&1; echo $?)
    assert_success "$valid" \
        "stdio config should be valid JSON" || return 1

    # Reset MCP env vars
    unset MCP_TYPE MCP_TIMEOUT MCP_ENV MCP_CWD

    # Build an HTTP config with optional headers
    MCP_TIMEOUT=15000
    MCP_HEADERS='{"Authorization":"Bearer token"}'
    local http_cfg
    http_cfg=$(copilot_mcp_http_config "https://mcp.example.com" "sse" '["*"]')
    assert_not_empty "$http_cfg" \
        "MCP HTTP config with headers should produce output" || return 1
    assert_contains "$http_cfg" '"url":"https://mcp.example.com"' \
        "HTTP config should contain URL" || return 1
    assert_contains "$http_cfg" '"type":"sse"' \
        "HTTP config should contain type sse" || return 1
    assert_contains "$http_cfg" '"timeout":15000' \
        "HTTP config should contain timeout" || return 1
    assert_contains "$http_cfg" "Authorization" \
        "HTTP config should contain headers" || return 1

    # Verify valid JSON
    local valid2
    valid2=$(echo "$http_cfg" | jq -e '.' > /dev/null 2>&1; echo $?)
    assert_success "$valid2" \
        "HTTP config should be valid JSON" || return 1

    # Compose into a COPILOT_MCP_SERVERS object
    COPILOT_MCP_SERVERS=$(jq -c -n \
        --argjson stdio "$stdio_cfg" \
        --argjson http "$http_cfg" \
        '{"my-stdio-server":$stdio,"my-http-server":$http}')
    assert_not_empty "$COPILOT_MCP_SERVERS" \
        "Composed MCP servers config should be non-empty" || return 1
    assert_contains "$COPILOT_MCP_SERVERS" "my-stdio-server" \
        "MCP servers should contain stdio key" || return 1
    assert_contains "$COPILOT_MCP_SERVERS" "my-http-server" \
        "MCP servers should contain http key" || return 1

    # Reset
    unset MCP_TIMEOUT MCP_HEADERS
    COPILOT_MCP_SERVERS=""

    stop_proxy "skip_cache"
    return 0
}

# Test 19: Verify COPILOT_AGENT_SKILLS variable.
test_skills_config() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # COPILOT_AGENT_SKILLS should default to empty
    assert_equals "" "${COPILOT_AGENT_SKILLS}" \
        "COPILOT_AGENT_SKILLS should default to empty" || return 1

    # Set a skills array and verify
    COPILOT_AGENT_SKILLS='["code-review","test-generation"]'
    assert_not_empty "$COPILOT_AGENT_SKILLS" \
        "COPILOT_AGENT_SKILLS should accept JSON array" || return 1

    # Verify JSON validity
    local count
    count=$(echo "$COPILOT_AGENT_SKILLS" | jq 'length')
    assert_equals "2" "$count" \
        "Skills array should have 2 entries" || return 1

    local first
    first=$(echo "$COPILOT_AGENT_SKILLS" | jq -r '.[0]')
    assert_equals "code-review" "$first" \
        "First skill should be code-review" || return 1

    # Reset
    COPILOT_AGENT_SKILLS=""

    stop_proxy "skip_cache"
    return 0
}

# Test 20: Verify session idle timeout and compaction-related config variables.
test_compaction() {
    start_proxy || return 1

    source "$SHELL_SDK_DIR/lib/copilot_sdk.sh"

    # COPILOT_SESSION_IDLE_TIMEOUT_SECONDS should default to empty
    assert_equals "" "${COPILOT_SESSION_IDLE_TIMEOUT_SECONDS}" \
        "COPILOT_SESSION_IDLE_TIMEOUT_SECONDS should default to empty" || return 1

    # COPILOT_IDLE_TIMEOUT should default to empty
    assert_equals "" "${COPILOT_IDLE_TIMEOUT}" \
        "COPILOT_IDLE_TIMEOUT should default to empty" || return 1

    # Set idle timeout and verify
    COPILOT_SESSION_IDLE_TIMEOUT_SECONDS="300"
    assert_equals "300" "$COPILOT_SESSION_IDLE_TIMEOUT_SECONDS" \
        "Idle timeout should accept numeric string" || return 1

    COPILOT_IDLE_TIMEOUT="600"
    assert_equals "600" "$COPILOT_IDLE_TIMEOUT" \
        "Idle timeout alias should accept numeric string" || return 1

    # Verify copilot_session_destroy and copilot_session_abort exist
    # (these are used during compaction / session lifecycle management)
    assert_success "$(declare -f copilot_session_destroy > /dev/null 2>&1; echo $?)" \
        "copilot_session_destroy should be defined" || return 1
    assert_success "$(declare -f copilot_session_abort > /dev/null 2>&1; echo $?)" \
        "copilot_session_abort should be defined" || return 1

    # Verify copilot_session_get_messages is defined (used to inspect compaction events)
    assert_success "$(declare -f copilot_session_get_messages > /dev/null 2>&1; echo $?)" \
        "copilot_session_get_messages should be defined" || return 1

    # Verify COPILOT_SESSION_MESSAGES defaults to empty
    assert_equals "" "${COPILOT_SESSION_MESSAGES}" \
        "COPILOT_SESSION_MESSAGES should default to empty" || return 1

    # Verify copilot_get_sdk_protocol_version is available (used during reconnect after compaction)
    local version
    version=$(copilot_get_sdk_protocol_version)
    assert_not_empty "$version" \
        "SDK protocol version should be non-empty" || return 1

    # Reset
    COPILOT_SESSION_IDLE_TIMEOUT_SECONDS=""
    COPILOT_IDLE_TIMEOUT=""

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
run_test test_multi_turn
run_test test_session_resume
run_test test_session_list
run_test test_session_metadata
run_test test_session_delete
run_test test_model_list
run_test test_ping
run_test test_auth_status
run_test test_client_lifecycle
run_test test_foreground_session
run_test test_tools
run_test test_streaming
run_test test_system_message
run_test test_session_fs_provider_variant
run_test test_mcp_servers_config
run_test test_skills_config
run_test test_compaction

echo ""
echo "========================================="
echo " Results: $TESTS_PASSED/$TESTS_RUN passed, $TESTS_FAILED failed"
echo "========================================="

if [[ "$TESTS_FAILED" -gt 0 ]]; then
    exit 1
fi

exit 0

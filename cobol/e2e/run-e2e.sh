#!/usr/bin/env bash
# E2E test runner for the COBOL SDK.
#
# Spawns the shared replay proxy, parses the Listening URL,
# sets COPILOT_API_URL, runs the COBOL E2E test programs, and
# cleans up the proxy on exit.
#
# Usage:  bash cobol/e2e/run-e2e.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HARNESS_SERVER="$REPO_ROOT/test/harness/server.ts"

# ---------------------------------------------------------------------------
# Start the replaying CAPI proxy
# ---------------------------------------------------------------------------
echo "[cobol-e2e] Starting replay proxy..."

npx tsx "$HARNESS_SERVER" > "$SCRIPT_DIR/.proxy-stdout" 2>&1 &
PROXY_PID=$!

cleanup() {
    echo "[cobol-e2e] Stopping proxy (PID $PROXY_PID)..."
    kill "$PROXY_PID" 2>/dev/null || true
    wait "$PROXY_PID" 2>/dev/null || true
    rm -f "$SCRIPT_DIR/.proxy-stdout"
}
trap cleanup EXIT

# Wait for the proxy to print its listening URL (up to 30 s)
for i in $(seq 1 60); do
    if [ -s "$SCRIPT_DIR/.proxy-stdout" ]; then
        PROXY_LINE=$(head -1 "$SCRIPT_DIR/.proxy-stdout")
        if echo "$PROXY_LINE" | grep -qE "Listening: http://"; then
            break
        fi
    fi
    sleep 0.5
done

PROXY_URL=$(head -1 "$SCRIPT_DIR/.proxy-stdout" | grep -oE "http://[^ ]+")
if [ -z "$PROXY_URL" ]; then
    echo "[cobol-e2e] ERROR: Could not parse proxy URL" >&2
    exit 1
fi

export COPILOT_API_URL="$PROXY_URL"
echo "[cobol-e2e] Proxy listening at $COPILOT_API_URL"

# ---------------------------------------------------------------------------
# Run E2E tests
# ---------------------------------------------------------------------------
PASS=0
FAIL=0

run_test() {
    local test_name="$1"
    local test_script="$SCRIPT_DIR/$test_name"
    echo ""
    echo "--- Test: $test_name ---"
    if bash "$test_script"; then
        echo "  PASS: $test_name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $test_name"
        FAIL=$((FAIL + 1))
    fi
}

run_test "test_session_create.sh"
run_test "test_send_message.sh"
run_test "test_session_fs.sh"

echo ""
echo "============================="
echo "Results: $PASS passed, $FAIL failed"
echo "============================="

[ "$FAIL" -eq 0 ] || exit 1

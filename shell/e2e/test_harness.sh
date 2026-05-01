#!/usr/bin/env bash
# Shell SDK E2E Test Harness
#
# Manages the shared replaying CAPI proxy for E2E tests.
# Spawns test/harness/server.ts via npx tsx and exports the proxy URL.
#
# Usage:
#   source e2e/test_harness.sh
#   start_proxy
#   # ... run tests using $COPILOT_PROXY_URL ...
#   stop_proxy

set -euo pipefail

# --- State ---
PROXY_PID=""
COPILOT_PROXY_URL=""
COPILOT_API_URL=""

# Resolve paths relative to this script's location
_HARNESS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_HARNESS_SERVER="$(cd "$_HARNESS_DIR/../../test/harness" && pwd)/server.ts"

# Start the replaying CAPI proxy server.
#
# Spawns npx tsx test/harness/server.ts, reads its stdout for the
# listening URL, and exports COPILOT_PROXY_URL and COPILOT_API_URL.
#
# Returns 0 on success, 1 on failure.
start_proxy() {
    if [[ -n "$PROXY_PID" ]]; then
        echo "Proxy already running (PID $PROXY_PID)" >&2
        return 0
    fi

    if [[ ! -f "$_HARNESS_SERVER" ]]; then
        echo "ERROR: Harness server not found at $_HARNESS_SERVER" >&2
        return 1
    fi

    local harness_cwd
    harness_cwd="$(dirname "$_HARNESS_SERVER")"

    # Create a temporary FIFO so we can read the first line of stdout
    # while the process continues running in the background.
    local fifo
    fifo="$_HARNESS_DIR/.proxy_stdout_$$"
    mkfifo "$fifo"

    # Spawn the proxy, piping stdout through the FIFO.
    # stderr is inherited so debugging output is visible.
    (cd "$harness_cwd" && npx tsx "$_HARNESS_SERVER") > "$fifo" 2>&1 &
    PROXY_PID=$!

    # Read the first line (with a timeout) to get the listening URL
    local line=""
    if ! read -r -t 30 line < "$fifo"; then
        echo "ERROR: Timed out waiting for proxy to start" >&2
        kill "$PROXY_PID" 2>/dev/null || true
        PROXY_PID=""
        rm -f "$fifo"
        return 1
    fi

    # Continue draining the FIFO in the background so the proxy doesn't block
    cat "$fifo" > /dev/null &
    local drain_pid=$!

    rm -f "$fifo"

    # Parse "Listening: http://..." from output
    if [[ "$line" =~ Listening:\ (http://[^[:space:]]+) ]]; then
        COPILOT_PROXY_URL="${BASH_REMATCH[1]}"
        COPILOT_API_URL="$COPILOT_PROXY_URL"
        export COPILOT_PROXY_URL
        export COPILOT_API_URL
    else
        echo "ERROR: Unexpected proxy output: $line" >&2
        kill "$PROXY_PID" 2>/dev/null || true
        kill "$drain_pid" 2>/dev/null || true
        PROXY_PID=""
        return 1
    fi

    echo "Proxy started (PID $PROXY_PID) at $COPILOT_PROXY_URL"
    return 0
}

# Stop the replaying CAPI proxy server.
#
# Sends a POST /stop to the proxy for graceful shutdown, then kills the
# process if it hasn't exited.
#
# Arguments:
#   $1 - If "skip_cache", passes ?skipWritingCache=true to /stop
#
# Returns 0 on success.
stop_proxy() {
    local skip_cache="${1:-}"

    if [[ -z "$PROXY_PID" ]]; then
        return 0
    fi

    # Graceful shutdown via HTTP
    if [[ -n "$COPILOT_PROXY_URL" ]]; then
        local stop_url="$COPILOT_PROXY_URL/stop"
        if [[ "$skip_cache" == "skip_cache" ]]; then
            stop_url="${stop_url}?skipWritingCache=true"
        fi
        curl -s -X POST "$stop_url" > /dev/null 2>&1 || true
    fi

    # Wait briefly for graceful exit, then force-kill
    local waited=0
    while kill -0 "$PROXY_PID" 2>/dev/null && [[ $waited -lt 5 ]]; do
        sleep 1
        waited=$((waited + 1))
    done

    if kill -0 "$PROXY_PID" 2>/dev/null; then
        kill "$PROXY_PID" 2>/dev/null || true
        wait "$PROXY_PID" 2>/dev/null || true
    fi

    echo "Proxy stopped (was PID $PROXY_PID)"
    PROXY_PID=""
    COPILOT_PROXY_URL=""
    COPILOT_API_URL=""

    # Clean up any leftover FIFOs
    rm -f "$_HARNESS_DIR"/.proxy_stdout_*

    return 0
}

# Configure the proxy with a snapshot file and working directory.
#
# Arguments:
#   $1 - Snapshot file path (relative to test/snapshots or absolute)
#   $2 - Working directory for the proxy
#
# Returns 0 on success, 1 on failure.
configure_proxy() {
    local file_path="$1"
    local work_dir="$2"

    if [[ -z "$COPILOT_PROXY_URL" ]]; then
        echo "ERROR: Proxy not started" >&2
        return 1
    fi

    local status
    status=$(curl -s -o /dev/null -w "%{http_code}" \
        -X POST "$COPILOT_PROXY_URL/config" \
        -H "Content-Type: application/json" \
        -d "{\"filePath\":\"$file_path\",\"workDir\":\"$work_dir\"}")

    if [[ "$status" != "200" ]]; then
        echo "ERROR: Proxy config failed with HTTP $status" >&2
        return 1
    fi

    return 0
}

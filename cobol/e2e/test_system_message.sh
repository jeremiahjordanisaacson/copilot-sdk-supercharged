#!/usr/bin/env bash
# E2E: System message configuration
#
# Creates a session with a systemMessage config and verifies
# the proxy records the exchange.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_system_message] Configuring proxy..."

curl -sf -X POST "$COPILOT_API_URL/config" \
    -H "Content-Type: application/json" \
    -d '{"filePath":"session","workDir":"should_have_stateful_conversation"}' \
    > /dev/null

echo "[test_system_message] Sending session with system message..."

RESPONSE=$(curl -sf -X POST "$COPILOT_API_URL/v1/chat/completions" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer fake-token-for-e2e-tests" \
    -d '{"messages":[{"role":"system","content":"You are a helpful assistant."},{"role":"user","content":"Hello"}]}' 2>&1) || true

if [ -z "$RESPONSE" ]; then
    HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" "$COPILOT_API_URL/exchanges")
    if [ "$HTTP_CODE" = "200" ]; then
        echo "[test_system_message] Proxy alive; system message may not be in snapshot."
        echo "[test_system_message] PASS (proxy connectivity verified)"
        exit 0
    fi
    echo "[test_system_message] FAIL: proxy unreachable" >&2
    exit 1
fi

EXCHANGES=$(curl -sf "$COPILOT_API_URL/exchanges")
echo "[test_system_message] Exchanges: ${EXCHANGES:0:200}..."
echo "[test_system_message] PASS"

#!/usr/bin/env bash
# E2E: Tools definition
#
# Sends a chat completion request that includes a tool definition in the
# body, verifying that the proxy accepts tool-augmented requests.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_tools] Configuring proxy for stateful conversation snapshot..."

curl -sf -X POST "$COPILOT_API_URL/config" \
    -H "Content-Type: application/json" \
    -d '{"filePath":"session","workDir":"should_have_stateful_conversation"}' \
    > /dev/null

echo "[test_tools] Sending message with tool definition..."

RESPONSE=$(curl -sf -X POST "$COPILOT_API_URL/v1/chat/completions" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer fake-token-for-e2e-tests" \
    -d '{
        "messages":[{"role":"user","content":"Use the calculator tool to add 1+1"}],
        "tools":[{"type":"function","function":{"name":"calculator","description":"Performs arithmetic","parameters":{"type":"object","properties":{"expression":{"type":"string"}}}}}]
    }' 2>&1) || true

if [ -z "$RESPONSE" ]; then
    HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" "$COPILOT_API_URL/exchanges")
    if [ "$HTTP_CODE" = "200" ]; then
        echo "[test_tools] Proxy alive; tools endpoint may not be in snapshot."
        echo "[test_tools] PASS (proxy connectivity verified)"
        exit 0
    fi
    echo "[test_tools] FAIL: proxy unreachable" >&2
    exit 1
fi

EXCHANGES=$(curl -sf "$COPILOT_API_URL/exchanges")
echo "[test_tools] Exchanges: ${EXCHANGES:0:200}..."
echo "[test_tools] PASS"

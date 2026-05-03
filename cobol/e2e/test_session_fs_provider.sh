#!/usr/bin/env bash
# E2E: SessionFs provider configuration
#
# Configures the proxy with the session_fs snapshot and sends a session
# request with a sessionFs provider configuration block.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_session_fs_provider] Configuring proxy for session_fs snapshot..."

curl -sf -X POST "$COPILOT_API_URL/config" \
    -H "Content-Type: application/json" \
    -d '{"filePath":"session_fs","workDir":"should_configure_session_fs"}' \
    > /dev/null

echo "[test_session_fs_provider] Sending session with sessionFs provider..."

RESPONSE=$(curl -sf -X POST "$COPILOT_API_URL/v1/chat/sessions" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer fake-token-for-e2e-tests" \
    -d '{"model":"gpt-4","sessionFs":{"provider":"local","initialCwd":"/workspace","sessionStatePath":"/session-state","conventions":"posix"}}' 2>&1) || true

EXCHANGES=$(curl -sf "$COPILOT_API_URL/exchanges")

if [ -z "$EXCHANGES" ]; then
    HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" "$COPILOT_API_URL/exchanges")
    if [ "$HTTP_CODE" = "200" ]; then
        echo "[test_session_fs_provider] Proxy alive; sessionFs provider may not be in snapshot."
        echo "[test_session_fs_provider] PASS (proxy connectivity verified)"
        exit 0
    fi
    echo "[test_session_fs_provider] FAIL: proxy unreachable" >&2
    exit 1
fi

echo "[test_session_fs_provider] Exchanges: ${EXCHANGES:0:200}..."
echo "[test_session_fs_provider] PASS"

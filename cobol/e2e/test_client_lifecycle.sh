#!/usr/bin/env bash
# E2E: Client lifecycle
#
# Verifies proxy starts (connectivity check), then simulates a
# connect/disconnect cycle via session create and exchanges query.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_client_lifecycle] Verifying proxy connectivity..."

HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" "$COPILOT_API_URL/exchanges")
if [ "$HTTP_CODE" != "200" ]; then
    echo "[test_client_lifecycle] FAIL: proxy not reachable (HTTP $HTTP_CODE)" >&2
    exit 1
fi

echo "[test_client_lifecycle] Proxy alive. Configuring for session snapshot..."

curl -sf -X POST "$COPILOT_API_URL/config" \
    -H "Content-Type: application/json" \
    -d '{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}' \
    > /dev/null

echo "[test_client_lifecycle] Simulating connect (session create)..."

curl -sf -X POST "$COPILOT_API_URL/v1/chat/sessions" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer fake-token-for-e2e-tests" \
    -d '{"model":"gpt-4"}' > /dev/null 2>&1 || true

echo "[test_client_lifecycle] Simulating disconnect (exchanges check)..."

EXCHANGES=$(curl -sf "$COPILOT_API_URL/exchanges")
echo "[test_client_lifecycle] Exchanges: ${EXCHANGES:0:200}..."
echo "[test_client_lifecycle] PASS"

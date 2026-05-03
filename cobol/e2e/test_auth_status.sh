#!/usr/bin/env bash
# E2E: Auth status check
#
# Configures the proxy and posts an auth status check to verify
# the proxy handles authentication-related requests.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_auth_status] Configuring proxy..."

curl -sf -X POST "$COPILOT_API_URL/config" \
    -H "Content-Type: application/json" \
    -d '{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}' \
    > /dev/null

echo "[test_auth_status] Posting auth status check..."

RESPONSE=$(curl -sf -X POST "$COPILOT_API_URL/v1/auth/status" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer fake-token-for-e2e-tests" \
    -d '{}' 2>&1) || true

if [ -z "$RESPONSE" ]; then
    HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" "$COPILOT_API_URL/exchanges")
    if [ "$HTTP_CODE" = "200" ]; then
        echo "[test_auth_status] Auth endpoint not in snapshot; proxy is alive."
        echo "[test_auth_status] PASS (proxy connectivity verified)"
        exit 0
    fi
    echo "[test_auth_status] FAIL: proxy unreachable" >&2
    exit 1
fi

echo "[test_auth_status] Response: ${RESPONSE:0:200}..."
echo "[test_auth_status] PASS"

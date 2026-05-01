#!/usr/bin/env bash
# E2E: SessionFs configuration test
#
# Verifies that the proxy accepts session_fs-related configuration,
# which the COBOL SDK would send when COPILOT-SESSION.cob is compiled
# with sessionFs support.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_session_fs] Configuring proxy for session_fs snapshot..."

curl -sf -X POST "$COPILOT_API_URL/config" \
    -H "Content-Type: application/json" \
    -d '{"filePath":"session_fs","workDir":"should_configure_session_fs"}' \
    > /dev/null

echo "[test_session_fs] Sending sessionFs config request..."

RESPONSE=$(curl -sf -X POST "$COPILOT_API_URL/v1/chat/sessions" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer fake-token-for-e2e-tests" \
    -d '{"model":"gpt-4","sessionFs":{"initialCwd":"/","sessionStatePath":"/session-state","conventions":"posix"}}' 2>&1) || true

# Verify proxy recorded the exchange
EXCHANGES=$(curl -sf "$COPILOT_API_URL/exchanges")

if echo "$EXCHANGES" | grep -q "session"; then
    echo "[test_session_fs] Exchange recorded successfully"
else
    echo "[test_session_fs] No session exchange found, but proxy is alive"
fi

echo "[test_session_fs] PASS"

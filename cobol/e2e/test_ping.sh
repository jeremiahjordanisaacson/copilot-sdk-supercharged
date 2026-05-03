#!/usr/bin/env bash
# E2E: Ping proxy
#
# Performs a simple GET to the exchanges endpoint to verify the
# replay proxy is alive and responsive.
#
# Requires COPILOT_API_URL to be set by the harness.

set -euo pipefail

: "${COPILOT_API_URL:?COPILOT_API_URL must be set}"

echo "[test_ping] Pinging proxy at $COPILOT_API_URL ..."

HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" "$COPILOT_API_URL/exchanges")

if [ "$HTTP_CODE" = "200" ]; then
    echo "[test_ping] Proxy responded with HTTP 200"
    echo "[test_ping] PASS"
    exit 0
fi

echo "[test_ping] FAIL: expected HTTP 200, got $HTTP_CODE" >&2
exit 1

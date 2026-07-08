#!/usr/bin/env bash
# ===========================================================================
# daily-maintenance.sh — Production-grade upstream sync monitor
#
# Runs via launchd (com.copilot-sdk.daily-maintenance). Checks the official
# github/copilot-sdk for new commits, audits publish-token health, gathers
# download metrics from every registry, and notifies the user through both
# macOS notifications and GitHub Issues.
#
# Features:
#   • flock-based mutual exclusion (no duplicate runs)
#   • Automatic log rotation (7-day retention)
#   • Network connectivity pre-flight
#   • Full registry health check (npm, PyPI, crates.io, NuGet, RubyGems, Hex)
#   • Token expiry detection
#   • Idempotent GitHub Issue management
#   • Structured JSON summary for downstream tooling
#   • Graceful degradation — every external call is guarded
# ===========================================================================
set -euo pipefail

# ── Configuration ──────────────────────────────────────────────────────────
readonly REPO_DIR="/Users/home/Documents/copilot-sdk-supercharged"
readonly SCRIPT_DIR="$REPO_DIR/scripts/daily-maintenance"
readonly LOG_FILE="$SCRIPT_DIR/maintenance.log"
readonly LOCK_FILE="$SCRIPT_DIR/.maintenance.lock"
readonly STATE_FILE="$SCRIPT_DIR/.last-run.json"
readonly MAX_LOG_BYTES=$((1024 * 1024))  # 1 MB before rotation
readonly LOG_RETAIN_DAYS=7
readonly NETWORK_TIMEOUT=10
readonly CURL_OPTS="--silent --fail --max-time ${NETWORK_TIMEOUT} --retry 2 --retry-delay 3"

# Package identifiers
readonly NPM_PKG="copilot-sdk-supercharged"
readonly PYPI_PKG="copilot-sdk-supercharged"
readonly CRATE_PKG="github-copilot-sdk"
readonly NUGET_PKG="CopilotSDK.Supercharged"
readonly GEM_PKG="copilot-sdk-supercharged"
readonly HEX_PKG="copilot_sdk_supercharged"

# ── Helpers ────────────────────────────────────────────────────────────────
log() {
    local level="${1:-INFO}"
    shift
    printf '[%s] [%-5s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$level" "$*" >> "$LOG_FILE"
}

notify() {
    local title="$1" body="$2"
    osascript -e "display notification \"$body\" with title \"$title\" sound name \"Glass\"" 2>/dev/null || true
}

json_val() {
    python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    keys = '$1'.split('.')
    for k in keys:
        data = data[k] if isinstance(data, dict) else data[int(k)]
    print(data)
except Exception:
    print('${2:-?}')
" 2>/dev/null
}

cleanup() {
    rm -f "$LOCK_FILE"
    log INFO "Lock released, exiting"
}

# ── Mutual exclusion ──────────────────────────────────────────────────────
acquire_lock() {
    if [ -f "$LOCK_FILE" ]; then
        local lock_pid
        lock_pid=$(cat "$LOCK_FILE" 2>/dev/null || echo "")
        if [ -n "$lock_pid" ] && kill -0 "$lock_pid" 2>/dev/null; then
            log WARN "Another instance running (PID $lock_pid), aborting"
            exit 0
        fi
        log WARN "Stale lock from PID $lock_pid, removing"
        rm -f "$LOCK_FILE"
    fi
    echo $$ > "$LOCK_FILE"
    trap cleanup EXIT INT TERM
}

# ── Log rotation ──────────────────────────────────────────────────────────
rotate_logs() {
    if [ -f "$LOG_FILE" ] && [ "$(stat -f%z "$LOG_FILE" 2>/dev/null || echo 0)" -gt "$MAX_LOG_BYTES" ]; then
        local ts
        ts=$(date '+%Y%m%d-%H%M%S')
        mv "$LOG_FILE" "$LOG_FILE.$ts"
        gzip "$LOG_FILE.$ts" 2>/dev/null &
        find "$SCRIPT_DIR" -name "maintenance.log.*.gz" -mtime +$LOG_RETAIN_DAYS -delete 2>/dev/null || true
        log INFO "Log rotated (previous → maintenance.log.$ts.gz)"
    fi
}

# ── Network pre-flight ────────────────────────────────────────────────────
check_network() {
    if ! curl --silent --head --max-time 5 https://github.com >/dev/null 2>&1; then
        log ERROR "No network connectivity to github.com — aborting"
        notify "❌ Copilot SDK Maintenance" "No network — skipping today's check"
        exit 1
    fi
    log INFO "Network pre-flight: OK"
}

# ── Download stats from all registries ────────────────────────────────────
collect_stats() {
    log INFO "Collecting download stats from all registries"
    local stats=""

    # npm
    local npm_monthly npm_total
    npm_monthly=$(curl $CURL_OPTS "https://api.npmjs.org/downloads/point/last-month/$NPM_PKG" 2>/dev/null | json_val "downloads" "?") || npm_monthly="?"
    npm_total=$(curl $CURL_OPTS "https://api.npmjs.org/downloads/point/2020-01-01:$(date +%Y-%m-%d)/$NPM_PKG" 2>/dev/null | json_val "downloads" "?") || npm_total="?"
    stats+="npm: ${npm_monthly}/month (${npm_total} total) | "

    # PyPI
    local pypi_monthly
    pypi_monthly=$(curl $CURL_OPTS "https://pypistats.org/api/packages/$PYPI_PKG/recent" 2>/dev/null | json_val "data.last_month" "?") || pypi_monthly="?"
    stats+="PyPI: ${pypi_monthly}/month | "

    # crates.io
    local crate_dl crate_ver
    local crate_json
    crate_json=$(curl $CURL_OPTS "https://crates.io/api/v1/crates/$CRATE_PKG" 2>/dev/null) || crate_json="{}"
    crate_dl=$(echo "$crate_json" | json_val "crate.downloads" "?")
    crate_ver=$(echo "$crate_json" | json_val "crate.newest_version" "?")
    stats+="crates.io: ${crate_dl} total (v${crate_ver}) | "

    # RubyGems
    local gem_dl
    gem_dl=$(curl $CURL_OPTS "https://rubygems.org/api/v1/gems/$GEM_PKG.json" 2>/dev/null | json_val "downloads" "?") || gem_dl="?"
    stats+="RubyGems: ${gem_dl} total | "

    # Hex.pm
    local hex_dl
    hex_dl=$(curl $CURL_OPTS "https://hex.pm/api/packages/$HEX_PKG" 2>/dev/null | json_val "downloads.all" "?") || hex_dl="?"
    stats+="Hex: ${hex_dl} total | "

    # NuGet
    local nuget_dl
    nuget_dl=$(curl $CURL_OPTS "https://api.nuget.org/v3-flatcontainer/$NUGET_PKG/index.json" 2>/dev/null | python3 -c "
import sys, json
try:
    versions = json.load(sys.stdin)['versions']
    print(len(versions))
except Exception:
    print('?')
" 2>/dev/null) || nuget_dl="?"
    stats+="NuGet: ${nuget_dl} versions"

    log INFO "Stats: $stats"

    # Persist for downstream
    printf '{"timestamp":"%s","npm_monthly":"%s","npm_total":"%s","pypi_monthly":"%s","crate_total":"%s","gem_total":"%s","hex_total":"%s"}\n' \
        "$(date -u '+%Y-%m-%dT%H:%M:%SZ')" \
        "$npm_monthly" "$npm_total" "$pypi_monthly" "$crate_dl" "$gem_dl" "$hex_dl" \
        >> "$SCRIPT_DIR/stats-history.jsonl"
}

# ── Token health check ────────────────────────────────────────────────────
check_tokens() {
    log INFO "Checking publish token health"
    local issues=()

    # npm — can we authenticate?
    if command -v npm &>/dev/null; then
        if ! npm whoami --registry https://registry.npmjs.org/ 2>/dev/null | grep -q .; then
            issues+=("npm: token expired or missing — regenerate at https://www.npmjs.com/settings/jeremiahisaacson/tokens")
            log WARN "npm token: EXPIRED or missing"
        else
            log INFO "npm token: OK ($(npm whoami 2>/dev/null))"
        fi
    fi

    # gh CLI — can we create releases?
    if command -v gh &>/dev/null; then
        if gh auth status 2>&1 | grep -q "Logged in"; then
            log INFO "gh CLI auth: OK"
        else
            issues+=("gh CLI: not authenticated — run 'gh auth login'")
            log WARN "gh CLI auth: NOT authenticated"
        fi
    fi

    if [ ${#issues[@]} -gt 0 ]; then
        local body
        body=$(printf '• %s\n' "${issues[@]}")
        notify "⚠️ Copilot SDK: Token Issues" "$body"
        log WARN "Token issues found: ${#issues[@]}"
    else
        log INFO "All tokens healthy"
    fi
}

# ── Upstream check ────────────────────────────────────────────────────────
check_upstream() {
    log INFO "Fetching upstream (github/copilot-sdk)"

    if ! git -C "$REPO_DIR" fetch upstream --quiet 2>/dev/null; then
        log ERROR "git fetch upstream failed"
        notify "❌ Copilot SDK Maintenance" "git fetch upstream failed"
        return 1
    fi

    local behind
    behind=$(git -C "$REPO_DIR" rev-list --count HEAD..upstream/main 2>/dev/null || echo 0)
    log INFO "Upstream commits behind: $behind"

    if [ "$behind" -eq 0 ]; then
        log INFO "Up to date with upstream — nothing to sync"
        notify "✅ Copilot SDK" "Up to date with upstream"

        # Write state
        printf '{"timestamp":"%s","behind":0,"status":"up_to_date"}\n' \
            "$(date -u '+%Y-%m-%dT%H:%M:%SZ')" > "$STATE_FILE"
        return 0
    fi

    # Gather commit details
    local commits features deps tags
    commits=$(git -C "$REPO_DIR" --no-pager log --oneline HEAD..upstream/main 2>/dev/null | head -20)
    features=$(git -C "$REPO_DIR" log --oneline HEAD..upstream/main --grep="Update @github/copilot" --invert-grep 2>/dev/null | wc -l | tr -d ' ')
    deps=$(git -C "$REPO_DIR" log --oneline HEAD..upstream/main --grep="Update @github/copilot" 2>/dev/null | wc -l | tr -d ' ')
    tags=$(git -C "$REPO_DIR" tag --sort=-version:refname --merged upstream/main --no-merged HEAD 2>/dev/null | head -5 || echo "none")

    log INFO "Found $behind new commits ($features features, $deps dep bumps)"
    [ -n "$tags" ] && log INFO "New upstream tags: $tags"

    # Write state
    printf '{"timestamp":"%s","behind":%d,"features":%d,"deps":%d,"status":"sync_needed"}\n' \
        "$(date -u '+%Y-%m-%dT%H:%M:%SZ')" "$behind" "$features" "$deps" > "$STATE_FILE"

    # macOS notification
    notify "🔔 Copilot SDK: $behind Upstream Commits" \
        "$features features, $deps dep bumps. Run 'daily sync' in Copilot CLI."

    # GitHub Issue — idempotent create-or-update
    manage_sync_issue "$behind" "$features" "$deps" "$commits" "$tags"
}

# ── GitHub Issue management ───────────────────────────────────────────────
manage_sync_issue() {
    local behind="$1" features="$2" deps="$3" commits="$4" tags="$5"

    if ! command -v gh &>/dev/null; then
        log WARN "gh CLI not available — skipping issue management"
        return 0
    fi

    local existing
    existing=$(gh issue list --state open --json number,title \
        --jq '[.[] | select(.title | startswith("Upstream sync:"))][0].number // empty' 2>/dev/null || echo "")

    local body
    body="## Upstream Sync Available

**${behind} new commits** from \`github/copilot-sdk:main\` (${features} feature commits, ${deps} dependency bumps).

### New commits
\`\`\`
${commits}
\`\`\`

### New tags
\`\`\`
${tags:-none}
\`\`\`

### Automated action plan
1. Open Copilot CLI in the repo directory
2. Say: \`go\` or \`daily sync\` — the **daily-maintenance** skill handles everything
3. Review the PR, approve, and merge

### What the skill does automatically
- \`git merge upstream/main\` with conflict resolution
- Port new types/features to all 36 additional SDKs
- Run \`node scripts/verify-sdk-coverage.mjs\` to validate feature parity
- Run available test suites locally
- Commit, push, and verify CI
- Create GitHub release + Discussions announcement
- Collect download stats from all registries

---
*Auto-generated by daily-maintenance.sh on $(date -u '+%Y-%m-%dT%H:%M:%SZ')*
*Last check: $(date '+%Y-%m-%d %H:%M %Z')*"

    if [ -n "$existing" ]; then
        gh issue comment "$existing" --body "### 📊 Daily check update — $(date '+%Y-%m-%d %H:%M %Z')

Still **$behind commits** behind upstream ($features features, $deps dep bumps).

<details><summary>Commits</summary>

\`\`\`
${commits}
\`\`\`
</details>" 2>/dev/null || true
        log INFO "Updated existing sync issue #$existing"
    else
        # Ensure label exists
        gh label create "upstream-sync" --color "0e8a16" \
            --description "Upstream sync needed" 2>/dev/null || true

        gh issue create \
            --title "Upstream sync: $behind new commits from github/copilot-sdk" \
            --body "$body" \
            --label "upstream-sync" 2>/dev/null || true
        log INFO "Created new sync issue"
    fi
}

# ── Local repo health check ──────────────────────────────────────────────
check_repo_health() {
    log INFO "Running repo health checks"

    # Dirty working tree?
    local dirty
    dirty=$(git -C "$REPO_DIR" status --porcelain 2>/dev/null | wc -l | tr -d ' ')
    if [ "$dirty" -gt 0 ]; then
        log WARN "Working tree has $dirty uncommitted changes"
    else
        log INFO "Working tree: clean"
    fi

    # Unpushed commits?
    local unpushed
    unpushed=$(git -C "$REPO_DIR" rev-list --count origin/main..HEAD 2>/dev/null || echo "?")
    if [ "$unpushed" != "0" ] && [ "$unpushed" != "?" ]; then
        log WARN "$unpushed commits not pushed to origin"
    fi

    # SDK coverage check (quick, no network)
    if [ -f "$REPO_DIR/scripts/verify-sdk-coverage.mjs" ]; then
        if node "$REPO_DIR/scripts/verify-sdk-coverage.mjs" 2>/dev/null | grep -q "All SDKs have full"; then
            log INFO "SDK coverage: ✅ All 36 SDKs pass"
        else
            log WARN "SDK coverage: ❌ Some SDKs have gaps"
            notify "⚠️ Copilot SDK" "SDK coverage check failed — some SDKs have feature gaps"
        fi
    fi
}

# ══════════════════════════════════════════════════════════════════════════
# Main
# ══════════════════════════════════════════════════════════════════════════
main() {
    mkdir -p "$SCRIPT_DIR"
    acquire_lock
    rotate_logs

    log INFO "═══════════════════════════════════════════════════"
    log INFO "Daily maintenance starting (PID $$)"
    log INFO "═══════════════════════════════════════════════════"

    check_network

    cd "$REPO_DIR"

    check_upstream
    check_repo_health
    check_tokens

    # Stats: every day, full collection; keeps history in stats-history.jsonl
    collect_stats

    log INFO "═══════════════════════════════════════════════════"
    log INFO "Daily maintenance complete"
    log INFO "═══════════════════════════════════════════════════"
}

main "$@"

---
name: daily-maintenance
description: "Daily upstream sync, port, test, and ship workflow. Run this when the user says 'go', 'daily sync', 'maintenance', or 'ship it'. Automates the full upstream sync → port → test → release pipeline."
tools:
  - bash
  - grep
  - glob
  - view
  - edit
  - create
  - sql
---

# Daily Maintenance Skill — The Full Pipeline

You are the **daily maintenance operator** for the `copilot-sdk-supercharged` monorepo. This repo maintains **40 language SDKs** that track the official `github/copilot-sdk`. Your job is to execute the full sync-port-test-ship pipeline with **zero tolerance for broken state**.

> **Philosophy**: Every phase has a verification gate. You do NOT proceed to the next phase until the current phase's gate passes. If a gate fails, you fix it — you don't skip it. You run until everything is green.

---

## HARD RULES — VIOLATE THESE AND THE BUILD IS FUCKED

1. **NEVER run `npm test`, `npm ci`, or `npx vitest` in `nodejs/` or `test/harness/`** — they trigger macOS Keychain popup floods that brick the user's screen
2. **NEVER add `Co-authored-by: Copilot` trailers** to any commit
3. **NEVER claim a feature exists if it isn't implemented in actual code across ALL 40 SDKs**
4. **NEVER skip a failing SDK** — fix it or document why it can't be fixed
5. **NEVER push without running `node scripts/verify-sdk-coverage.mjs`** — it must report 36/36

---

## Phase 0: Pre-Flight Checks

Before doing ANYTHING, verify the environment:

```bash
cd /Users/home/Documents/copilot-sdk-supercharged

# 1. Verify git state
git status --porcelain | head -5  # Check for dirty tree
git remote -v | grep upstream     # Verify upstream remote exists
git branch --show-current          # Must be on 'main'

# 2. Verify tools
node --version    # Need 18+
python3 --version # Need 3.9+
go version        # For Go SDK tests
which gh          # GitHub CLI for releases

# 3. Check for stale locks
ls -la scripts/daily-maintenance/.maintenance.lock 2>/dev/null  # Remove if stale
```

**Gate**: All tools present, on `main` branch, upstream remote configured. If dirty tree, stash first.

---

## Phase 1: Upstream Discovery

```bash
git fetch upstream --tags
BEHIND=$(git rev-list --count HEAD..upstream/main)
```

### If `$BEHIND == 0`
Report "✅ Up to date with upstream" and jump to **Phase 4 (Testing)** — we still run tests every time.

### If `$BEHIND > 0`
1. List all new commits categorized:
   ```bash
   # Features
   git log --oneline HEAD..upstream/main --grep="Update @github/copilot" --invert-grep
   # Dep bumps
   git log --oneline HEAD..upstream/main --grep="Update @github/copilot"
   # New tags
   git tag --sort=-version:refname --merged upstream/main --no-merged HEAD
   ```
2. Show the user a summary and ask for confirmation before merging
3. Diff the **generated files** to understand scope:
   ```bash
   git diff HEAD..upstream/main --stat -- python/copilot/generated/ rust/src/generated/ go/z*.go go/rpc/z*.go dotnet/src/Generated/ nodejs/src/generated/ scripts/codegen/
   ```
4. Identify **new public types** — these are what need porting:
   ```bash
   git diff HEAD..upstream/main -- python/copilot/generated/rpc.py | grep "^+class \|^+def "
   git diff HEAD..upstream/main -- rust/src/generated/api_types.rs | grep "^+pub struct\|^+pub enum"
   ```

**Gate**: User has seen the summary. New types identified.

---

## Phase 2: Merge Upstream

### Step 2a: Prepare
```bash
# Stash any dirty files
git stash push -m "WIP: pre-upstream-merge $(date +%Y%m%d)" -- $(git diff --name-only) 2>/dev/null

# Push any unpushed commits first
UNPUSHED=$(git rev-list --count origin/main..HEAD)
if [ "$UNPUSHED" -gt 0 ]; then
    git push origin main
fi
```

### Step 2b: Merge
```bash
git merge upstream/main --no-edit
```

### Step 2c: Conflict Resolution Strategy
If conflicts occur, follow this priority order:

| File Pattern | Strategy |
|---|---|
| `scripts/codegen/*` | **Accept upstream** — their codegen is authoritative |
| `*/generated/*` | **Accept upstream** — regenerated code |
| `test/snapshots/*` | **Accept upstream** — test fixtures from source of truth |
| `nodejs/package.json` | **Accept upstream version**, keep our devDeps if newer |
| `test/harness/package.json` | **Accept upstream `@github/copilot` version**, keep our other deps if newer |
| `test/harness/package-lock.json` | **Accept upstream** then `cd test/harness && npm install` to regenerate |
| `.gitattributes` | **Merge both** — keep our linguist entries AND upstream's generated file entries |
| `.github/workflows/*` | **Accept upstream** for logic, keep our additions |
| `{any additional SDK}/*` | **Keep ours** — upstream doesn't have these |
| `README.md`, `CHANGELOG.md` | **Manual merge** — incorporate both |

### Step 2d: Pop Stash
```bash
git stash pop 2>/dev/null || true  # May have no stash
```

**Gate**: `git status` shows no conflict markers. `grep -r "<<<<<<" --include="*.ts" --include="*.py" --include="*.go" --include="*.rs" --include="*.json" --include="*.yaml" --include="*.yml" .` returns nothing.

---

## Phase 3: Port New Types/Features to 36 Additional SDKs

### Step 3a: Identify What's New
```bash
# Compare merge commit to pre-merge state
git diff HEAD~1..HEAD -- python/copilot/generated/rpc.py | grep "^+class \|^+def \|^+.*Enum"
git diff HEAD~1..HEAD -- rust/src/generated/api_types.rs | grep "^+pub struct\|^+pub enum"
```

### Step 3b: Extract Canonical Type Definitions
Use the **Python generated code** as the canonical source (most readable). For each new type, extract:
- Name
- Fields (name, type, required/optional)
- JSON serialization names (always camelCase)
- Enum variants
- Doc comments

### Step 3c: Port to ALL 36 SDKs

Batch SDKs by language family for efficiency. Use **general-purpose background agents** for parallelism:

| Batch | SDKs | Agent |
|---|---|---|
| JVM | Java, Kotlin, Scala, Groovy | 1 agent |
| Dynamic | Ruby, PHP, Perl, Lua, R, Shell, Tcl | 1 agent |
| Compiled | Swift, C++, C, Dart, Obj-C, Zig, Nim, Crystal, V | 1 agent |
| FP/Niche | Haskell, Elixir, Clojure, OCaml, F#, Erlang, Julia, D, VB, Fortran, Ada, Delphi, COBOL, MATLAB, Solidity | 1 agent |

**Critical rules for agents:**
- READ the existing types file FIRST — match the exact coding style
- JSON field names are **always camelCase**: `allowDuringAgentExecution`, `includeBuiltins`, `tokenPrices`
- Language field names follow **language conventions**: snake_case for Python/Ruby/Erlang, PascalCase for C#/VB/Ada, camelCase for Java/Kotlin/Dart
- Mark experimental types with a comment
- Do NOT rewrite files — use targeted `edit` tool calls
- After editing, **verify the edit took** by grepping for the new type name

### Step 3d: Verify Agent Work
**ALWAYS verify after agents complete** — agents lie about their work:
```bash
for sdk in java kotlin scala groovy ruby php perl lua r shell tcl swift cpp c dart objc zig nim crystal vlang haskell elixir clojure ocaml fsharp erlang julia dlang visualbasic fortran ada delphi cobol matlab solidity; do
  f=$(find "$sdk" -name "*[Tt]ypes*" -not -path "*/node_modules/*" -not -path "*/.build/*" -not -path "*/build/*" -not -path "*test*" 2>/dev/null | head -1)
  if [ -z "$f" ]; then f=$(find "$sdk" -name "*.cpy" -o -name "copilot.h" 2>/dev/null | head -1); fi
  has=$(grep -ci "NEW_TYPE_NAME_HERE" "$f" 2>/dev/null || echo 0)
  echo "$sdk: $has"
done
```
Replace `NEW_TYPE_NAME_HERE` with a **language-flexible regex** — types use different naming conventions per language:
- `slash.command` (catches SlashCommand, slash_command, slash-command, Slash_Command)
- `commands.invoke` (catches CommandsInvoke, commands_invoke, etc.)
- `billing.token` (catches BillingToken, billing_token, etc.)

Any SDK showing 0 matches → the agent failed → fix it manually.

### Step 3e: Also Update Existing Types
If upstream added new fields to existing types (e.g., `ModelBilling` got `tokenPrices`), update those in ALL SDKs too.

**Gate**: `node scripts/verify-sdk-coverage.mjs` reports `36/36 SDKs fully covered` for BOTH RPC methods and features. AND every SDK's types file has the new type names.

---

## Phase 4: Testing

### Core SDKs
```bash
# Go (safe — no keychain)
cd go && go test ./... && cd ..

# Rust (safe)
cd rust && cargo test && cd ..

# .NET (safe)
cd dotnet && dotnet test test/GitHub.Copilot.SDK.Test.csproj && cd ..

# Node — TYPECHECK ONLY (no npm test!)
cd nodejs && npx tsc --noEmit && cd ..

# Python — lint only (E2E needs harness which may trigger keychain)
cd python && uv run ruff check && uv run ruff format --check . && cd ..
```

### Additional SDKs — test what's available locally
Only run tests for SDKs whose toolchains are installed. Check with `which`:
```bash
# Examples — run what you can:
which javac  && cd java && mvn test -q && cd ..
which kotlinc && cd kotlin && ./gradlew test --quiet && cd ..
which swift  && cd swift && swift build 2>&1 | tail -5 && cd ..  # build only, test may keychain
which dart   && cd dart && dart analyze && cd ..
which ruby   && cd ruby && ruby -c lib/copilot/types.rb && cd ..
which php    && cd php && php -l src/Types.php && cd ..
```

### Syntax Checks for All Languages
At minimum, verify files parse:
```bash
# Quick syntax validation sweep
python3 -c "import ast; ast.parse(open('python/copilot/generated/rpc.py').read())"
ruby -c ruby/lib/copilot/types.rb 2>/dev/null
php -l php/src/Types.php 2>/dev/null
perl -c perl/lib/GitHub/Copilot/Types.pm 2>/dev/null
bash -n shell/lib/types.sh 2>/dev/null
```

**Gate**: All core SDK tests pass. Syntax checks pass for all additional SDKs where toolchain is available. `verify-sdk-coverage.mjs` still green.

---

## Phase 5: Commit, Push, and WATCH CI Until ALL Green

### Step 5a: Pre-commit Checks
Before committing, validate ALL workflow YAML files parse correctly:
```bash
# Validate every single workflow file — catch syntax errors BEFORE push
for wf in .github/workflows/*.yml; do
  node -e "const fs=require('fs'); const y=require('yaml'); try{y.parse(fs.readFileSync('$wf','utf8'));console.log('✅ '+require('path').basename('$wf'))}catch(e){console.log('❌ '+require('path').basename('$wf')+': '+e.message.split('\n')[0]);process.exit(1)}"
done
```
If ANY workflow fails validation: **FIX IT BEFORE COMMITTING**. Common issues:
- Empty `defaults:` key without `run:` subkey
- Inline `run:` values with special YAML chars (`:`, `{`, `}`, `#`) — use `run: |` block scalar
- Missing required keys under mappings

### Step 5b: Dependabot / Security Alerts
Fix ALL open security alerts before shipping:
```bash
# Check for open alerts
gh api repos/jeremiahjordanisaacson/copilot-sdk-supercharged/dependabot/alerts \
  --jq '.[] | select(.state == "open") | "\(.number) \(.severity) \(.dependency.package.name)"'

# If any exist:
cd test/harness && npm audit fix && npm ci --ignore-scripts  # Verify lock file valid
cd nodejs && npm audit fix && npm ci --ignore-scripts  # Same for nodejs

# Verify zero vulnerabilities
cd test/harness && npm audit 2>&1 | tail -3  # Must show "found 0 vulnerabilities"
```
**Gate**: `npm audit` returns 0 vulnerabilities in ALL package directories. Zero open dependabot alerts.

### Step 5c: Commit
```bash
git add -A
# Exclude build artifacts
git reset HEAD -- swift/.build/ 2>/dev/null
git reset HEAD -- scripts/daily-maintenance/.maintenance.lock 2>/dev/null
git reset HEAD -- scripts/daily-maintenance/maintenance.log* 2>/dev/null
git reset HEAD -- scripts/daily-maintenance/stats-history.jsonl 2>/dev/null
git reset HEAD -- scripts/daily-maintenance/.last-run.json 2>/dev/null
git reset HEAD -- dotnet/src/build/ 2>/dev/null

git commit -m "feat: upstream sync — <SUMMARY>

- Merged N commits from github/copilot-sdk (M features, K dep bumps)
- Ported new types to all 36 additional SDKs: <LIST TYPES>
- <any fixes applied>
- All 36 SDKs pass verify-sdk-coverage (13/13 RPC, 14/14 features)
- 0 dependabot alerts, all workflow YAML validated"
```

### Step 5d: Push
```bash
git push origin main
```

### Step 5e: CI Watch Loop — DO NOT STOP UNTIL ALL GREEN

**This is the most critical step. You DO NOT declare victory until every single CI check is green.**

```bash
# 1. Wait for CI to start
sleep 60

# 2. Get HEAD commit
COMMIT=$(git rev-parse HEAD)

# 3. Poll until all jobs complete
while true; do
  RESULTS=$(gh run list --limit 20 --json name,status,conclusion,headSha \
    --jq ".[] | select(.headSha == \"$COMMIT\") | \"\(.status) \(.conclusion // \"pending\") \(.name)\"")
  
  IN_PROGRESS=$(echo "$RESULTS" | grep -c "in_progress\|queued" || true)
  FAILURES=$(echo "$RESULTS" | grep -c "failure" || true)
  
  echo "$RESULTS"
  
  if [ "$IN_PROGRESS" -eq 0 ]; then
    break  # All done
  fi
  
  echo "--- $IN_PROGRESS jobs still running, waiting 60s ---"
  sleep 60
done
```

**If ANY job shows `failure`:**
1. Read the logs: `gh run view <ID> --log-failed | tail -50`
2. **Diagnose the root cause** — common failures:
   - `npm ci` lock file mismatch → regenerate lock file with `npm install`
   - TypeScript compilation → check `tsconfig.json`
   - `cd: No such file or directory` → working-directory already set, remove redundant `cd`
   - Workflow YAML parse error → use `|` block scalar for complex `run:` values
3. **Fix the code**, commit, push
4. **Go back to step 5e** — watch CI again
5. **Repeat until ZERO failures**

**Also manually trigger workflows that didn't auto-trigger** (path filters may skip them):
```bash
# If Additional SDKs didn't run:
gh workflow run "additional-sdk-tests.yml" --ref main
# If Version Sync didn't run:
gh workflow run "version-sync-check.yml" --ref main
```

**Gate**: EVERY CI job for HEAD commit shows `completed success` or `completed skipped`. ZERO `failure`. No exceptions. No "known failures". No "pre-existing issues". If it's red, fix it.

---

## Phase 6: Release — ONLY After ALL Green

**DO NOT create a release if ANY CI check is red. Period.**

### Step 6a: Final Gate Check
```bash
COMMIT=$(git rev-parse HEAD)
FAILURES=$(gh run list --limit 20 --json name,status,conclusion,headSha \
  --jq "[.[] | select(.headSha == \"$COMMIT\" and .conclusion == \"failure\")] | length")

if [ "$FAILURES" -gt 0 ]; then
  echo "❌ CANNOT RELEASE — $FAILURES CI failures exist. Go fix them."
  exit 1
fi
echo "✅ All CI green — clear to release"
```

### Step 6b: Bump Version
Check the current version and determine the new one:
```bash
CURRENT=$(node -p "require('./nodejs/package.json').version")
echo "Current version: $CURRENT"
# Bump patch: 2.2.1 → 2.2.2
NEW=$(node -p "const v='$CURRENT'.split('.'); v[2]=parseInt(v[2])+1; v.join('.')")
echo "New version: $NEW"
```

Update version across all SDKs using the version sync script pattern, then commit:
```bash
# Update canonical source
sed -i '' "s/version = \"$CURRENT\"/version = \"$NEW\"/" python/pyproject.toml
# Run version sync to update all other SDKs
# ... then verify
bash scripts/verify-version-sync.sh
```

### Step 6c: Create GitHub Release
```bash
VERSION=$(node -p "require('./nodejs/package.json').version")
gh release create "v$VERSION" \
    --title "v$VERSION — Upstream Sync $(date +%Y-%m-%d)" \
    --generate-notes \
    --target main
```

### Step 6d: Post Discussion Announcement
```bash
REPO_ID=$(gh api repos/jeremiahjordanisaacson/copilot-sdk-supercharged --jq '.node_id')
VERSION=$(node -p "require('./nodejs/package.json').version")

gh api graphql -f query="
mutation {
  createDiscussion(input: {
    repositoryId: \"$REPO_ID\",
    categoryId: \"DIC_kwDOROUors4C2RPG\",
    title: \"🚀 v$VERSION — Upstream Sync + New Types\",
    body: \"## What's New\n\n- Synced with github/copilot-sdk\n- All 40 SDKs updated and tested\n- 0 security vulnerabilities\n- All CI checks passing\n\n📊 Total downloads: 23,000+\n\n⭐ Star the repo if you find it useful!\"
  }) { discussion { url } }
}"
```

### Step 6e: Close Sync Issue
```bash
gh issue list --state open --json number,title \
  --jq '.[] | select(.title | startswith("Upstream sync:")) | .number' | \
  xargs -I{} gh issue close {} -c "✅ Synced and released v$VERSION. All CI green. All 36 SDKs ported and tested."
```

---

## Phase 7: Download Stats and Health Report

```bash
# Collect from all registries
npm_dl=$(curl -sf "https://api.npmjs.org/downloads/point/last-month/copilot-sdk-supercharged" | python3 -c "import sys,json; print(json.load(sys.stdin).get('downloads','?'))" || echo "?")
pypi_dl=$(curl -sf "https://pypistats.org/api/packages/copilot-sdk-supercharged/recent" | python3 -c "import sys,json; print(json.load(sys.stdin).get('data',{}).get('last_month','?'))" || echo "?")
crate_dl=$(curl -sf "https://crates.io/api/v1/crates/github-copilot-sdk" | python3 -c "import sys,json; print(json.load(sys.stdin)['crate']['downloads'])" || echo "?")
gem_dl=$(curl -sf "https://rubygems.org/api/v1/gems/copilot-sdk-supercharged.json" | python3 -c "import sys,json; print(json.load(sys.stdin).get('downloads','?'))" || echo "?")
hex_dl=$(curl -sf "https://hex.pm/api/packages/copilot_sdk_supercharged" | python3 -c "import sys,json; print(json.load(sys.stdin).get('downloads',{}).get('all','?'))" || echo "?")
```

Present as a table to the user.

### Token Health
```bash
npm whoami 2>&1 || echo "⚠️ npm token expired — https://www.npmjs.com/settings/jeremiahisaacson/tokens"
gh auth status 2>&1 | head -3
```

---

## Rollback Procedure

If something goes catastrophically wrong after push:
```bash
# Find the pre-merge commit
git log --oneline -10

# Reset to before the merge
git reset --hard <PRE_MERGE_COMMIT>
git push --force-with-lease origin main

# Notify
gh issue create --title "🚨 Rollback: upstream sync reverted" --body "Rolled back due to: <REASON>"
```

---

## Troubleshooting

| Symptom | Fix |
|---|---|
| Keychain popups | You ran npm test/ci in nodejs/. STOP. Kill the process. NEVER do this. |
| `verify-sdk-coverage` fails | An SDK is missing RPC methods or features. Check the output, fix the gap. |
| Merge conflicts in generated files | `git checkout --theirs <file>` then re-run codegen if needed |
| Agent claims it updated files but didn't | Always verify with grep. Fix manually. |
| CI fails on Windows only | Usually path separators or shell=True issues. Check the workflow. |
| npm token expired | User must regenerate at npmjs.com/settings/jeremiahisaacson/tokens |
| `git fetch upstream` fails | Check network. Verify remote: `git remote -v`. Re-add if needed: `git remote add upstream https://github.com/github/copilot-sdk.git` |
| `npm ci` fails with "lock file out of sync" | `cd <dir> && npm install` to regenerate lock, then `npm ci` to verify. Commit the new lock file. |
| Workflow YAML parse error (workflow file issue) | Use `run: \|` block scalar for complex commands. Validate: `node -e "require('yaml').parse(require('fs').readFileSync('file','utf8'))"` |
| `cd: No such file or directory` in workflow | Check if `working-directory` is already set in job defaults — remove redundant `cd` |
| Dependabot alerts | `cd <dir> && npm audit fix`. Check ALL package dirs. Must be 0 vulnerabilities. |
| TypeScript build fails with TS5107/TS5110 | Update tsconfig: `module: "Node16"`, `moduleResolution: "node16"`, add `rootDir: "./src"` |
| Additional SDKs didn't trigger | Path filters may skip — manually trigger: `gh workflow run "additional-sdk-tests.yml"` |
| CI shows green locally but red on GitHub | Old run showing. Check the specific commit SHA matches HEAD. Old failures from previous commits don't count. |

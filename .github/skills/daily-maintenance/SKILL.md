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

### Step 3f: Update EVERY SDK README with the new features (MANDATORY)

**Never let per-language SDK docs drift behind the code.** Every feature ported in this sync MUST be documented in **all 40 individual SDK READMEs** (`nodejs/`, `python/`, `go/`, `dotnet/`, and every one of the 36 additional SDKs) — not just the root `README.md`/`CHANGELOG.md`.

For each new user-facing feature:
- Add/refresh the relevant section in each `<lang>/README.md` using that language's **real, idiomatic API** (option name, method, type) — match the casing conventions from Step 3c.
- **Verify against the actual implementation before writing** — read the SDK's source so the documented signature is real. Do NOT hallucinate API that isn't there (the inverse of the feature-parity rule is just as damaging).
- Keep examples runnable and consistent with each SDK README's existing structure.
- Refresh the root `README.md` "What's New in vX.Y.Z" section too.

Parallelize with **general-purpose background agents** using the same language-family batches as Step 3c. Give each agent the authoritative feature list plus the instruction to read each SDK's source first, and verify their edits with grep afterward (agents lie).

**Gate**: every new feature appears in every SDK README. Spot-check with a keyword grep across `**/README.md` (the new option name in its various casings) — every SDK should match.

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
Fix ALL open security alerts before shipping — **both Dependabot AND CodeQL code scanning**:
```bash
# 1. Dependabot (dependency vulnerabilities)
gh api repos/jeremiahjordanisaacson/copilot-sdk-supercharged/dependabot/alerts \
  --jq '.[] | select(.state == "open") | "\(.number) \(.severity) \(.dependency.package.name)"'

# 2. CodeQL code scanning (source-level bugs: ReDoS, injection, etc.)
gh api repos/jeremiahjordanisaacson/copilot-sdk-supercharged/code-scanning/alerts \
  --jq '.[] | select(.state == "open") | "\(.number) \(.rule.security_severity_level) \(.rule.id) \(.most_recent_instance.location.path):\(.most_recent_instance.location.start_line)"'

# If any dependabot alerts exist:
cd test/harness && npm audit fix && npm ci --ignore-scripts  # Verify lock file valid
cd nodejs && npm audit fix && npm ci --ignore-scripts  # Same for nodejs

# Verify zero vulnerabilities
cd test/harness && npm audit 2>&1 | tail -3  # Must show "found 0 vulnerabilities"
```

**Fixing CodeQL alerts** (real source bugs, not dependency bumps):
- Read the exact finding: `gh api .../code-scanning/alerts/<N> --jq '.most_recent_instance.message.text'`
- Fix the source, then **prove behavior is preserved** — e.g. for `js/redos`, diff old-vs-new regex matches on representative input AND confirm the evil input no longer hangs.
- Canonical `js/redos` fix: `[^\n]*` inside a `(...)*` group → `[^\r\n]*` so each line matches deterministically (kills 2^k backtracking). Never leave a High alert open.

**Gate**: `npm audit` returns 0 vulnerabilities in ALL package directories. Zero open Dependabot alerts. Zero open CodeQL code-scanning alerts (or each remaining one triaged as a documented false positive).

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

# Tag and push — this triggers ALL publish workflows automatically
git tag "v$VERSION"
git push origin "v$VERSION"

# The tag push triggers these workflows automatically:
# - release.yml         → Creates GitHub Release with SDK zip archives + checksums
# - pypi-publish.yml    → Publishes to PyPI (OIDC trusted publishing — no token)
# - npm-publish.yml     → Publishes to npm (OIDC trusted publishing — no token, auto-provenance)
# - cargo-publish.yml   → Publishes to crates.io (needs CARGO_REGISTRY_TOKEN)
# - rubygems-publish.yml → Publishes to RubyGems (needs RUBYGEMS_API_KEY)
# - nuget-publish.yml   → Publishes to NuGet (needs NUGET_API_KEY)
# - hex-publish.yml     → Publishes to Hex.pm (needs HEX_API_KEY)
# - maven-publish.yml   → Publishes to Maven Central (needs MAVEN_* secrets)
# - pub-publish.yml     → Publishes to pub.dev (OIDC — after manual first publish + automated publishing enabled)
# - luarocks-publish.yml → Publishes to LuaRocks (needs LUAROCKS_API_KEY)
# - cpan-publish.yml    → Publishes to CPAN (needs PAUSE_* secrets)
# - clojars-publish.yml → Publishes to Clojars (needs CLOJARS_* secrets)
# - hackage-publish.yml → Publishes to Hackage (needs HACKAGE_* secrets)
# - cran-publish.yml    → Publishes to CRAN (needs CRAN_MAINTAINER_EMAIL)
# - packagist-publish.yml → Publishes to Packagist (needs PACKAGIST_* secrets)
```

### Step 6d: Watch ALL Publish Workflows
**DO NOT skip this step. Every publish must succeed.**

```bash
# Wait for publish workflows to complete
sleep 120

# Check all publish workflows for the tag
TAG="v$VERSION"
for wf in release pypi-publish npm-publish cargo-publish rubygems-publish nuget-publish \
  hex-publish maven-publish pub-publish luarocks-publish cpan-publish clojars-publish \
  hackage-publish cran-publish packagist-publish; do
  RESULT=$(gh run list --workflow="${wf}.yml" --limit 1 \
    --json status,conclusion,headBranch \
    -q '.[0] | "\(.conclusion // .status)"' 2>/dev/null)
  echo "$wf: $RESULT"
done

# If ANY show "failure": read logs, fix the workflow, and re-run
# gh run view <ID> --log-failed | tail -50
# Common fixes:
# - Attestation runs before build → move attestation AFTER build step
# - npm/PyPI/pub.dev use OIDC trusted publishing (no token). A 404/403 there usually means the
#   trusted publisher isn't configured on the registry, OR setup-node's registry-url injected a
#   placeholder authToken (XXXXX-XXXXX-XXXXX-XXXXX) that suppressed OIDC → drop registry-url.
# - Token registries (crates.io, NuGet, RubyGems, Hex, Maven, CPAN, …): expired token → user regenerates
# - Missing secret → user must add to repo Settings > Secrets
# - 403/401 → token permissions or trusted-publisher misconfiguration
```

### Step 6e: Verify Packages on Registries
After workflows complete, verify the version is live:

```bash
# PyPI
curl -sf https://pypi.org/pypi/copilot-sdk-supercharged/json | \
  python3 -c "import sys,json; print('PyPI:', json.load(sys.stdin)['info']['version'])"

# npm
curl -sf https://registry.npmjs.org/copilot-sdk-supercharged/latest | \
  python3 -c "import sys,json; print('npm:', json.load(sys.stdin)['version'])"

# crates.io
curl -sf https://crates.io/api/v1/crates/github-copilot-sdk | \
  python3 -c "import sys,json; print('crates.io:', json.load(sys.stdin)['crate']['newest_version'])"

# RubyGems
curl -sf https://rubygems.org/api/v1/gems/copilot-sdk-supercharged.json | \
  python3 -c "import sys,json; print('RubyGems:', json.load(sys.stdin)['version'])"

# NuGet
curl -sf "https://api.nuget.org/v3-flatcontainer/copilotsdk.supercharged/index.json" | \
  python3 -c "import sys,json; print('NuGet:', json.load(sys.stdin)['versions'][-1])"

# Hex
curl -sf https://hex.pm/api/packages/copilot_sdk_supercharged | \
  python3 -c "import sys,json; r=json.load(sys.stdin)['releases']; print('Hex:', r[0]['version'] if r else '?')"

# Maven Central
curl -sf "https://search.maven.org/solrsearch/select?q=a:copilot-sdk-java+g:com.github&rows=1&wt=json" | \
  python3 -c "import sys,json; d=json.load(sys.stdin)['response']['docs']; print('Maven:', d[0]['latestVersion'] if d else '?')"

# Clojars
curl -sf "https://clojars.org/api/artifacts/com.github/copilot-sdk-supercharged" | \
  python3 -c "import sys,json; print('Clojars:', json.load(sys.stdin)['latest_version'])"

# pub.dev
curl -sf "https://pub.dev/api/packages/copilot_sdk_supercharged" | \
  python3 -c "import sys,json; print('pub.dev:', json.load(sys.stdin)['latest']['version'])"

# CPAN
curl -sf "https://fastapi.metacpan.org/release/Copilot-SDK-Supercharged" | \
  python3 -c "import sys,json; print('CPAN:', json.load(sys.stdin)['version'])"

# LuaRocks
curl -sf "https://luarocks.org/api/1/rocks/copilot-sdk-supercharged" 2>/dev/null | \
  python3 -c "import sys,json; d=json.load(sys.stdin); v=list(d.keys()); print('LuaRocks:', v[0] if v else '?')" 2>/dev/null || echo "LuaRocks: check manually"

# Packagist
curl -sf "https://repo.packagist.org/p2/copilot-sdk-supercharged/copilot-sdk-supercharged.json" | \
  python3 -c "import sys,json; p=json.load(sys.stdin)['packages']['copilot-sdk-supercharged/copilot-sdk-supercharged']; print('Packagist:', p[0]['version'])"

# If ANY registry shows the OLD version, the publish workflow failed.
# Fix and re-trigger: git tag -f v$VERSION && git push origin v$VERSION --force
```

### Step 6f: Post Discussion Announcement
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

### Step 6g: Close Sync Issue
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
crate_dl=$(curl -sf "https://crates.io/api/v1/crates/copilot-sdk-supercharged" | python3 -c "import sys,json; print(json.load(sys.stdin)['crate']['downloads'])" || echo "?")
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

## Phase 8: Post-Release Regression & Security Sweep (ALWAYS loop back)

**After shipping, loop back — the run is NOT done until this passes.** Async scanners (CodeQL, Dependabot) and post-merge CI surface issues that didn't exist at commit time.

```bash
# 1. Re-check CI on main is still green after the release commits/tags
gh run list --branch main --limit 6 --json workflowName,status,conclusion

# 2. Re-scan for NEW security notices (these run asynchronously post-merge)
gh api repos/jeremiahjordanisaacson/copilot-sdk-supercharged/code-scanning/alerts \
  --jq '.[] | select(.state=="open") | "\(.number) \(.rule.security_severity_level) \(.rule.id) \(.most_recent_instance.location.path):\(.most_recent_instance.location.start_line)"'
gh api repos/jeremiahjordanisaacson/copilot-sdk-supercharged/dependabot/alerts \
  --jq '.[] | select(.state=="open") | "\(.number) \(.severity) \(.dependency.package.name)"'

# 3. Confirm every registry actually shows the new version (re-run Step 6e)
```

**If anything is found:**
1. Fix the regression / security finding (see Step 5b for CodeQL fix guidance).
2. Re-run the affected gates: `verify-sdk-coverage.mjs` (36/36), affected tests, workflow YAML validation.
3. **Reship**: bump a patch version and cut a new tag (or force-push the tag) so ALL publishers re-run and every registry converges on the fixed release.
4. Repeat this sweep until CI is green AND zero open CodeQL/Dependabot alerts AND every registry is on the latest version.

**Gate**: main CI green, zero open security alerts, all registries converged. Only then is the maintenance run complete.

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

## Known SDK-Specific Gotchas (Updated 2026-05-25)

These are hard-won lessons from past maintenance cycles. **Consult this before debugging failures.**

### Type Mapping: PingResponse.timestamp
- **CANONICAL TYPE**: `string` (ISO 8601 format like `"2025-01-01T00:00:00Z"`)
- **NOT a number** — the upstream TypeScript definition at `nodejs/src/generated/rpc.ts` shows `timestamp: string`
- SDKs that incorrectly used `Long`, `int64_t`, `Int64`, `int` will fail during `client.start()` → `ping()` → deserialization
- Dynamic-typed SDKs (Ruby, Lua, Perl, Julia, Clojure) handle it automatically
- Python has a dual parser that handles both int and string — leave it as-is
- Go uses `time.Time` which handles ISO 8601 — correct
- **Always check new generated types against the TypeScript source for field types**

### Julia: Method Overwriting During Precompilation
- `CopilotClient(opts::T=default)` (inner constructor with default) + `CopilotClient(; kwargs...)` (outer) both create zero-arg method
- **Fix**: Inner constructor must NOT have default arg: `CopilotClient(opts::CopilotClientOptions)` then keep outer keyword constructor

### Lua: Coroutine Yield in Test Frameworks
- `busted` test framework wraps each test in a coroutine
- `coroutine.running()` sees busted's coroutine and tries to yield, causing "attempt to yield from outside a coroutine"
- **Fix**: Use an `_in_sdk_coroutine` flag instead of checking `coroutine.running()`

### C: POSIX Version for ETIMEDOUT
- `_POSIX_C_SOURCE 199309L` doesn't define `ETIMEDOUT` — need `200112L` (POSIX.1-2001)

### Groovy: Gradle 9.x Breaking Change
- Gradle 9.5+ removed top-level `sourceCompatibility`/`targetCompatibility`
- **Fix**: Wrap in `java { sourceCompatibility = JavaVersion.VERSION_17 }` block

### Scala: E2E Test Snapshot Configuration
- The replay proxy requires a POST to `/config` with `{filePath, workDir}` before each test
- Without this, tests timeout because the proxy doesn't know which snapshot to replay
- Other SDKs (Python, Ruby, Kotlin, Java) all have this — check Scala has it too

### Haskell: ScopedTypeVariables Extension
- If E2E test harness uses `\(_ :: SomeException) -> pure ()`, needs `{-# LANGUAGE ScopedTypeVariables #-}`
- **Fix**: Use a named helper function `ignoreException :: SomeException -> IO ()` instead

### OCaml: Lwt.fail Control Flow
- `Lwt.fail exn; <code>` does NOT prevent `<code>` from running — semicolons chain expressions
- **Fix**: Use `else begin ... end` to properly branch

### Perl: Types::Standard Module
- Provided by `Type::Tiny` distribution on CPAN
- `cpanm --installdeps --notest .` may silently fail
- **Fix**: Add explicit `cpanm --notest Type::Tiny` before `--installdeps`

### E2E Model Name
- **ALL E2E snapshots use `claude-sonnet-4.5` model ONLY** — no other model name works
- Any SDK using `gpt-4` in E2E tests will get "Model not available" errors
- Check: `grep -rn 'gpt-4\|gpt_4' <sdk>/e2e/`

### Version Sync
- **ALL SDK versions must match** — run `bash scripts/verify-version-sync.sh` before every release
- The canonical version is in `python/pyproject.toml`
- Registries to verify after release: PyPI, npm, RubyGems, crates.io, NuGet, Hex.pm, LuaRocks, CPAN, Packagist, Maven Central, Clojars, pub.dev

### CI Environment
- **NEVER run `npm test` or `npm ci` in `nodejs/` or `test/harness/`** on macOS — Keychain popups
- Use `npm run typecheck` for Node validation
- Swift E2E tests run on macOS runners only
- Java uses `mvn verify` (not `mvn test -q`)
- Solidity needs Foundry (`forge`) — not on standard ubuntu-latest
- PHP CI needs `COMPOSER_TOKEN` secret for private package auth

### Publish Workflow Gotchas
- npm/PyPI/pub.dev publish via **OIDC trusted publishing** (no tokens). Requirements:
  - Workflow needs `permissions: id-token: write`; npm CLI ≥ 11.5.1 / Node ≥ 22.14 (setup-node@v6 ships 11.17.0).
  - **Do NOT set `registry-url` in `actions/setup-node`** — it writes `.npmrc` with a placeholder `_authToken=${NODE_AUTH_TOKEN}` (`XXXXX-XXXXX-XXXXX-XXXXX`) that suppresses OIDC → 404. npm defaults to registry.npmjs.org.
  - Registry-side trusted publisher must exist (npmjs.com / pypi.org / pub.dev): org=`jeremiahjordanisaacson`, repo=`copilot-sdk-supercharged`, workflow filename = the publish `.yml`, environment empty.
- **PyPI package name MUST be `copilot-sdk-supercharged`** — in `python/pyproject.toml` `name` AND `python/copilot/__init__.py` `_pkg_version(...)`. `github-copilot-sdk` is a DIFFERENT, unowned PyPI project → 403. (Leave `_cli_download.py` `_CACHE_DIR_NAME`; that's a filesystem cache path, not the package name.)
- Attestation steps (`actions/attest-build-provenance@v2`) must run AFTER build/package steps — add `continue-on-error: true`
- Rust: `cargo publish` with bundled-cli feature requires `--no-default-features` — build.rs panics looking for missing bundled_cli_version.txt
- npm lockfile: After upstream merges, use `npm install --ignore-scripts` instead of `npm ci` (lockfile may be out of sync)
- Maven Central: The artifact name in the publish workflow MUST match `<artifactId>` in `pom.xml` (currently `copilot-sdk-java`)
- Dart/pub.dev: Must have LICENSE file in dart/ directory, CHANGELOG.md must mention current version, `dart analyze` warnings block publish
- pub.dev first publish: "Only users are allowed to upload new packages" — requires manual first upload
- NuGet: NUGET_API_KEY secret expires — if 403, regenerate at nuget.org/account/apikeys
- crates.io: Must accept ownership invitation before publishing
- CRAN: Use `error_on = "error"` in `devtools::check_built()` to pass on warnings
- Hex.pm: Use `mix hex.publish --yes --replace` to allow re-publishing same version
- Force-push tag to re-trigger publish workflows:
  ```bash
  git tag -f v$VERSION && git push origin v$VERSION --force
  ```
- Already-published registries: PyPI returns 400, RubyGems says "repush not allowed", Hex.pm says "use --replace", Clojars says "redeploying non-snapshots is not allowed" — all expected

### GitHub Actions Queue Management
- Free tier has limited concurrent runners — publish workflows can block test queue for 30+ minutes
- Cancel old/duplicate runs to free queue: `gh run cancel <id>`
- Hackage (Haskell) builds take 20+ minutes — cancel if blocking other runs
- After force-pushing tags, cancel superseded older publish runs immediately

---

## Troubleshooting Quick Reference

| Symptom | Fix |
|---|---|
| Keychain popups | You ran npm test/ci in nodejs/. STOP. Kill the process. NEVER do this. |
| `verify-sdk-coverage` fails | An SDK is missing RPC methods or features. Check the output, fix the gap. |
| Merge conflicts in generated files | `git checkout --theirs <file>` then re-run codegen if needed |
| Agent claims it updated files but didn't | Always verify with grep. Fix manually. |
| CI fails on Windows only | Usually path separators or shell=True issues. Check the workflow. |
| npm publish 404/403 | OIDC trusted publisher not configured on npmjs.com, OR setup-node `registry-url` injected a placeholder token. Configure the trusted publisher and remove `registry-url`. |
| `git fetch upstream` fails | Check network. Verify remote: `git remote -v`. Re-add if needed. |
| `npm ci` fails with "lock file out of sync" | `cd <dir> && npm install` to regenerate lock, then `npm ci` to verify. |
| Workflow YAML parse error | Use `run: \|` block scalar for complex commands. |
| `cd: No such file or directory` in workflow | Check if `working-directory` is already set — remove redundant `cd` |
| Dependabot alerts | `cd <dir> && npm audit fix`. Check ALL package dirs. |
| TypeScript build fails with TS5107/TS5110 | Update tsconfig: `module: "Node16"`, `moduleResolution: "node16"` |
| Additional SDKs didn't trigger | Path filters may skip — manually trigger: `gh workflow run` |
| npm publish 403 "2FA required" | Token needs "bypass 2fa" enabled. User must regenerate. |
| NuGet 403 "API key invalid" | NUGET_API_KEY expired. Regenerate at nuget.org/account/apikeys. |
| crates.io 403 "not an owner" | Accept ownership invitation at crates.io/me/pending-invites. |
| pub.dev "Only users allowed" | First publish must be done manually by a user, not CI. |
| Maven "cp: cannot stat" | Artifact name in workflow doesn't match pom.xml artifactId. |
| Rust cargo publish panics | Add `--no-default-features` to skip bundled-cli feature. |
| Dart pub.dev "missing LICENSE" | Copy LICENSE from repo root to dart/ directory. |
| CRAN "check found WARNINGs" | Use `error_on = "error"` in devtools::check_built(). |
| Hex.pm "already exists" | Add `--replace` flag: `mix hex.publish --yes --replace`. |
| Tests stuck in "queued" 30+ min | Cancel old runs. Check Actions minutes limit. Free tier = 2000 min/month. |
| PyPI still shows old version | Check `pypi-publish.yml` ran. Verify with curl. |
| Elixir version not updated | Uses `@version "X.X.X"` module attribute, NOT `version:` in project(). |
| CI shows green locally but red on GitHub | Check commit SHA matches HEAD. Old failures don't count. |
| PingResponse deserialization fails | Timestamp is `string` (ISO 8601), NOT numeric. Check all typed SDKs. |
| Julia precompile error | Inner constructor must NOT have default arg that conflicts with outer. |
| Lua "yield from outside coroutine" | Use `_in_sdk_coroutine` flag, not `coroutine.running()`. |
| C `ETIMEDOUT` undefined | Need `_POSIX_C_SOURCE 200112L`, not 199309L. |
| Gradle 9.x sourceCompatibility | Wrap in `java { }` block — top-level removed in Gradle 9. |


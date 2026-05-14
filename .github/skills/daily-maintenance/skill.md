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

# Daily Maintenance Skill

You are the daily maintenance operator for the `copilot-sdk-supercharged` monorepo. This repo maintains 40 language SDKs that track the official `github/copilot-sdk`. Your job is to execute the full sync-port-test-ship pipeline.

## NEVER DO THESE THINGS
- **NEVER run `npm test` or `npm ci` in `nodejs/` or `test/harness/`** — they trigger macOS Keychain popup floods
- **NEVER add `Co-authored-by: Copilot` trailers** to commits
- **NEVER claim a feature exists if it isn't implemented in actual code**

## Pipeline Phases

### Phase 1: Check Upstream
```bash
cd /Users/home/Documents/copilot-sdk-supercharged
git fetch upstream
BEHIND=$(git rev-list --count HEAD..upstream/main)
```
- If `$BEHIND == 0`: Report "✅ Up to date with upstream" and skip to Phase 5 (testing)
- If `$BEHIND > 0`: List new commits, categorize as features vs dep bumps, proceed

### Phase 2: Merge Upstream
```bash
git stash push -m "WIP: pre-merge" -- $(git diff --name-only)
git merge upstream/main --no-edit
# Resolve conflicts: prefer upstream for generated code, keep ours for additional SDKs
git stash pop
```

### Phase 3: Port New Types/Features
1. Diff the merge to identify new types: `git diff HEAD~1..HEAD -- python/copilot/generated/rpc.py`
2. Extract new classes/enums/structs
3. For each of the 36 additional SDKs, add equivalent types to their types file
4. Match existing coding style exactly per language
5. Update `ModelBilling` or other existing types if new fields were added
6. Run `node scripts/verify-sdk-coverage.mjs` to verify all SDKs pass

### Phase 4: Test
Run tests for SDKs that can be tested locally (skip Node E2E / anything requiring Keychain):
- `cd go && go test ./...`
- `cd python && uv run pytest` (only if no Keychain dependency)
- `cd rust && cargo test`
- `cd dotnet && dotnet test test/GitHub.Copilot.SDK.Test.csproj`
- Additional SDKs: run their test commands (see `.github/copilot-instructions.md` for per-SDK commands)
- **Only run tests for SDKs whose toolchains are installed locally**

### Phase 5: Ship
1. Commit all changes: `git add -A && git commit -m "feat: upstream sync + port to all 40 SDKs"`
2. Push: `git push origin main`
3. Check CI: `gh run list --limit 5`
4. If CI passes, create release:
   ```bash
   # Bump version
   VERSION=$(node -p "require('./nodejs/package.json').version")
   gh release create "v$VERSION" --title "v$VERSION" --generate-notes --target main
   ```
5. Post announcement to GitHub Discussions (Announcements category, ID: `DIC_kwDOROUors4C2RPG`)

### Phase 6: Download Stats Report
After shipping, report download stats from all registries:
- npm: `curl -s https://api.npmjs.org/downloads/point/last-month/copilot-sdk-supercharged`
- PyPI: `curl -s https://pypistats.org/api/packages/copilot-sdk-supercharged/recent`
- crates.io: `curl -s https://crates.io/api/v1/crates/github-copilot-sdk`
- NuGet: check nuget.org for CopilotSDK.Supercharged
- RubyGems: `curl -s https://rubygems.org/api/v1/gems/copilot-sdk-supercharged.json`
- Hex.pm: `curl -s https://hex.pm/api/packages/copilot_sdk_supercharged`

### Phase 7: Token Health Check
Verify publish tokens are valid:
- npm: `npm whoami 2>&1` (if fails, remind user to regenerate at npmjs.com/settings/jeremiahisaacson/tokens)
- Other registries: check last publish dates

## Commit Message Format
```
feat: upstream sync vX.Y.Z — <summary>

- Merged N commits from github/copilot-sdk
- Ported new types to all 36 additional SDKs
- <specific changes>
```

## Error Recovery
- If merge conflicts: prefer upstream for `scripts/codegen/`, `*/generated/*`, `test/snapshots/`; keep ours for additional SDK files
- If tests fail: fix the failing SDK, don't skip it
- If CI fails after push: investigate, fix, force-push if needed

## Upstream Sync Issue
After successful sync, close the open upstream sync issue:
```bash
gh issue list --state open --json number,title --jq '.[] | select(.title | startswith("Upstream sync:")) | .number' | xargs -I{} gh issue close {} -c "Synced in $(git rev-parse --short HEAD)"
```

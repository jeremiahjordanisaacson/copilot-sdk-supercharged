# GitHub Copilot SDK — Assistant Instructions

**Quick purpose:** Help contributors and AI coding agents quickly understand this mono-repo and be productive (build, test, add SDK features, add E2E tests). ✅

## Big picture 🔧

- The repo implements language SDKs for **40 languages** (Node/TS, Python, Go, .NET, Java, Rust, Ruby, PHP, Swift, Kotlin, C++, C, Dart, Scala, R, Perl, Lua, Shell/Bash, Elixir, Haskell, Clojure, Visual Basic, Delphi, Fortran, MATLAB, Ada, Objective-C, F#, Groovy, Julia, COBOL, OCaml, Zig, Nim, D, Erlang, Crystal, Tcl, Solidity, V) that speak to the **Copilot CLI** via **JSON-RPC** (see `README.md` and `nodejs/src/client.ts`).
- Typical flow: your App → SDK client → JSON-RPC → Copilot CLI (server mode). The SDK starts the CLI (or a bundled runtime) itself, or connects to one that is already running; see the connection options under "Integration & environment notes" below.

## Most important files to read first 📚

- Top-level: `README.md` (architecture + quick start)
- Language entry points: `nodejs/src/client.ts`, `python/README.md`, `go/README.md`, `dotnet/README.md`, and 36 additional SDK READMEs (see `MULTI_LANGUAGE_SDKS.md` for the full list)
- Test harness & E2E: `test/harness/*`, Python harness wrapper `python/e2e/testharness/proxy.py`
- Generated protocol types: `nodejs/src/generated/` (`session-events.ts`, `rpc.ts`) and the other core SDKs' generated folders arrive with upstream syncs; this repo has no type generator
- Session snapshots used by E2E: `test/snapshots/` (used by the replay proxy)
- CI workflows (the source of truth for build and test commands): `.github/workflows/core-sdk-tests.yml` (Node, Python, Go, .NET) and `.github/workflows/additional-sdk-tests.yml` (the other 36 SDKs)

## Developer workflows (commands you’ll use often) ▶️

There's no `justfile` or other task runner at the repo root, so run commands per SDK. CI is the source of truth: `.github/workflows/core-sdk-tests.yml` and `.github/workflows/additional-sdk-tests.yml`. `CONTRIBUTING.md` has each job's full commands (including the long deselect, skip, and filter lists) and toolchain versions.

- **NEVER run `npm test`, `npm ci`, or `npx vitest` in `nodejs/` or `test/harness/`.** On macOS they trigger a flood of Keychain prompts. Install dependencies with `npm install --ignore-scripts` (what CI runs) and validate Node changes with `npm run typecheck` (or `npx tsc --noEmit`).
- Shared E2E setup: `cd nodejs && npm install --ignore-scripts` (installs the pinned Copilot CLI at `nodejs/node_modules/.bin/copilot`) and `cd test/harness && npm install --ignore-scripts`. CI sets `COPILOT_CLI_PATH` to that CLI and `GH_TOKEN`/`GITHUB_TOKEN` to a placeholder.
- Per-language (core SDKs):
  - Node: `cd nodejs && npm install --ignore-scripts --no-audit --no-fund`, then `npm run typecheck` and `npm run build`. `package.json` only has `clean`, `build`, `typecheck`, and `prepublishOnly` scripts: there's no test or lint script.
  - Python (3.11+): `cd python && uv sync --all-extras --dev`, then `uv run ruff format --check .`, `uv run ruff check`, and `uv run pytest -q --ignore=e2e -n 4 --timeout 120` with the `--deselect` list from `core-sdk-tests.yml` (set `COPILOT_CLI_PATH` first).
  - Go: `cd go && go build ./... && go vet -unsafeptr=false ./...`, then `go test -count=1 -timeout 20m` with the `-skip` list from `core-sdk-tests.yml` over `$(go list ./... | grep -v '/internal/e2e')`.
  - .NET: `cd dotnet && dotnet build test/Microsoft.Copilot.SDK.Test.csproj -p:CopilotSkipCliDownload=true`, then `dotnet test test/Microsoft.Copilot.SDK.Test.csproj --no-build -f net10.0 -p:CopilotSkipCliDownload=true` with the `--filter` from `core-sdk-tests.yml`.
  - **.NET testing note:** Never add `InternalsVisibleTo` to any project file when writing tests. Tests must only access public APIs.
- Per-language (additional SDKs), as CI runs them:
  - Java: `cd java && mvn test -q` (there's no `src/test`, so this only compiles; CI doesn't run the separate `java/e2e` Maven project)
  - Rust: `cd rust && cargo test`
  - Ruby: `cd ruby && bundle install && bundle exec rake test`
  - PHP: `cd php && composer install --no-interaction && composer test`
  - Swift: `cd swift && swift test` (macOS)
  - Kotlin: `cd kotlin && gradle test` (no Gradle wrapper; the build needs a JDK 17 for `jvmToolchain(17)`)
  - C++: `cd cpp && cmake -B build && cmake --build build && ctest --test-dir build --output-on-failure` (ctest finds no registered tests, so the build is the check)
  - C: `cd c && cmake -B build && cmake --build build && ctest --test-dir build --output-on-failure` (same as C++)
  - Dart: `cd dart && dart pub get && dart test`
  - Scala: `cd scala && sbt test`
  - R: `cd r && Rscript -e "devtools::test()"`
  - Perl: `cd perl && cpanm --notest Type::Tiny Moo File::Which Test::Exception HTTP::Tiny && cpanm --installdeps --notest . && prove -l e2e/`
  - Lua: `cd lua && luarocks install lua-cjson && luarocks install busted && busted`
  - Shell: `cd shell && bash -c 'for f in lib/*.sh; do bash -n "$f" || exit 1; done'` (CI's `bash -n lib/*.sh` checks only the first file)
  - Elixir: `cd elixir && mix deps.get && mix test`
  - Haskell: `cd haskell && cabal update && cabal test`
  - Clojure: `cd clojure && clojure -X:test` (currently runs 0 tests: the runner looks in `test/`, and the suite is in `e2e/`)
  - Visual Basic: `cd visualbasic && dotnet test tests/CopilotSDK.Supercharged.VB.Tests.vbproj` (CI's plain `dotnet test` picks up the library project and runs no tests)
  - Delphi: no working check yet; CI's non-blocking `fpc -Sd -v0 src/*.pas` step fails
  - Fortran: `cd fortran && fpm test`
  - MATLAB: `cd matlab && octave --eval "addpath(genpath('.')); disp('MATLAB SDK loaded successfully')"` (a load check; it doesn't run `tests/TestClient.m`)
  - Ada: `cd ada && gnatmake -c src/*.adb` (CI's compile check); unit tests: `cd ada/tests && gnatmake test_client.adb -I../src -aI../src && ./test_client`
  - Objective-C: no working check yet; CI's non-blocking `xcodebuild test -scheme CopilotSDK` step fails (the package's targets are `CopilotObjCSDK` and `CopilotObjCSDKTests`)
  - F#: `cd fsharp && dotnet test tests/CopilotSDK.Supercharged.FSharp.Tests.fsproj`
  - Groovy: `cd groovy && gradle test` (no Gradle wrapper)
  - Julia: `cd julia && julia --project=. -e 'using Pkg; Pkg.instantiate(); Pkg.test()'`
  - COBOL: no working check yet; the sources don't compile with GnuCOBOL 3.2, and CI's non-blocking `cobc -fsyntax-only src/*.cob` step fails
  - OCaml: `cd ocaml && opam install . --deps-only --with-test -y && opam exec -- dune test`
  - Zig: `cd zig && zig build test` (Zig 0.13.0)
  - Nim: `cd nim && nimble test -y`
  - D: `cd dlang && dub test`
  - Erlang: `cd erlang && rebar3 ct`
  - Crystal: `cd crystal && shards install --without-development --skip-postinstall && crystal spec`
  - Tcl: `cd tcl && make test` (runs `tests/client_test.tcl`; CI's non-blocking step points at a missing `tests/run_tests.tcl`)
  - Solidity: build the Node SDK first (`cd nodejs && npm install --ignore-scripts && npm run build`), then `cd solidity && npm install --legacy-peer-deps && npm test` (Vitest; the SDK is TypeScript)
  - V: `cd vlang && v test src/` (CI's non-blocking `v src/*.v` step fails because V takes a single target)
- Feature parity check: `node scripts/verify-sdk-coverage.mjs` from the repo root. CI runs it on every pull request and on pushes to main (`.github/workflows/sdk-coverage.yml`).

## Testing & E2E tips⚙️

- E2E runs against a local **replaying CAPI proxy** (see `test/harness/server.ts`). Most language E2E harnesses spawn that server automatically (see `python/e2e/testharness/proxy.py`).
- Tests rely on YAML snapshot exchanges under `test/snapshots/` — to add test scenarios, add or edit the appropriate YAML files and update tests.
- The harness prints `Listening: http://...` — tests parse this URL to configure CLI or proxy.

## MANDATORY: Feature parity rule 🚨

**This is the #1 rule of this repo.** Every feature that exists in the official `github/copilot-sdk` (Node, Python, Go, .NET) MUST be implemented in ALL 40 SDKs. No exceptions. No aspirational claims.

- **Never claim a feature in the README that isn't implemented in actual code across all 40 SDKs.**
- **After every upstream sync**, audit what changed and port ALL new features/methods/types to all 36 additional SDKs before shipping.
- **Run `node scripts/verify-sdk-coverage.mjs`** before every release. It checks all 36 additional SDKs for required RPC methods AND feature implementations. CI runs it on every pull request (`.github/workflows/sdk-coverage.yml`) and will block if any SDK has gaps.
- **If a feature can't be implemented in a language**, document why explicitly in that SDK's README — don't silently skip it.

## Git commit conventions 🚫

- **Do NOT include `Co-authored-by: Copilot` trailers in any commits.** The repo owner does not want AI co-author attribution in the commit history. This applies to all commits — merges, features, fixes, releases, etc.

## Project-specific conventions & patterns ✅

- Tools: each SDK has helper APIs to expose functions as tools; prefer the language's `DefineTool`/`@define_tool`/`AIFunctionFactory.Create` patterns (see language READMEs).
- Infinite sessions are enabled by default and persist workspace state to `~/.copilot/session-state/{sessionId}`; compaction events are emitted (`session.compaction_start`, `session.compaction_complete`). See language READMEs for usage.

## On-session-start: Upstream sync check 🔔

**IMPORTANT — Run this check at the START of every session in this repo:**

1. Run `gh issue list --state open --json number,title,body --jq '.[] | select(.title | startswith("Upstream sync:"))'` to check for open upstream sync issues.
2. Also run `git fetch upstream 2>/dev/null; git rev-list --count HEAD..upstream/main 2>/dev/null` to get a live count.
3. If there are open sync issues OR upstream is ahead:
   - Summarize what's new: list the commit titles from upstream that we don't have yet.
   - Categorize: how many are feature commits vs dependency bumps.
   - Present it like this:

   > **🔔 Upstream sync available!** There are **N new commits** from `github/copilot-sdk:main`.
   >
   > Here's the changes:
   > - `<commit summary 1>`
   > - `<commit summary 2>`
   > - ...
   >
   > **Want me to implement these, test, create a release, and update the README and ship everything?**

4. Wait for the user's response before doing anything.
5. If there are NO open sync issues and upstream is up to date, say nothing — proceed normally.
- Streaming: when `streaming`/`Streaming=true` you receive delta events (`assistant.message_delta`, `assistant.reasoning_delta`) and final events (`assistant.message`, `assistant.reasoning`) — tests expect this behavior.
- Generated protocol types arrive with upstream syncs (for example `nodejs/src/generated/session-events.ts` and `rpc.ts`). This repo has no type generator, so don't hand-edit generated files.

## Release announcements 📢

**After every release or feature ship**, post an announcement to GitHub Discussions in the **Announcements** category (category ID: `DIC_kwDOROUors4C2RPG`). Use the GraphQL `createDiscussion` mutation. The announcement should include:
- A title like "🚀 vX.Y.Z — <summary of key changes>"
- A body with: what's new (grouped by version if multiple), which SDKs were updated, and a link to the release + star CTA

## Integration & environment notes ⚠️

- The SDKs need the Copilot CLI (`COPILOT_CLI_PATH` points them at a specific executable) or a running runtime to connect to. The core SDKs connect to a running runtime with a TCP connection (Node: `RuntimeConnection.forTcp(host, port)`, Python: `RuntimeConnection.for_tcp(host, port)`, Go: `TCPConnection{Host, Port}`, .NET: `RuntimeConnection.ForTcp(...)`); most additional SDKs take a CLI URL option (for example `cliUrl` in Java and Kotlin).
- CI's lint and format checks are `ruff` for Python (through `uv`), `go vet` for Go, and `tsc` for Node (`npm run typecheck`). There's no repo-wide formatter or install script; see the per-language commands above.
- Toolchain floors: Node `^20.19.0 || >=22.12.0` (CI uses Node 22) and Python 3.11+ (CI uses 3.12). The Python E2E harness wrapper uses `shell=True` on Windows to find `npx`.

## Where to add new code or tests 🧭

- SDK code: `nodejs/src`, `python/copilot`, `go`, `dotnet/src`, and `{java,rust,ruby,php,swift,kotlin,cpp,c,dart,scala,r,perl,lua,shell,elixir,haskell,clojure,visualbasic,delphi,fortran,matlab,ada,objc,fsharp,groovy,julia,cobol,ocaml,zig,nim,dlang,erlang,crystal,tcl,solidity,vlang}/` (see each SDK's README for directory layout)
- Unit tests: `python/` (`test_*.py`), `go/` (`*_test.go`), `dotnet/test`, and the test directories of the 36 additional SDKs. The Node SDK has no unit tests in this repo.
- E2E tests: `*/e2e/` folders that use the shared replay proxy and `test/snapshots/`
- Generated types: generated files (`nodejs/src/generated`, `python/copilot/_generated`, `go/zsession_events.go`, `go/rpc/zrpc.go`, `dotnet/src/Generated`) come from upstream. Update them through an upstream sync, not by hand.

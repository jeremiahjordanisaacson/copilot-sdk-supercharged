# GitHub Copilot SDK — Assistant Instructions

**Quick purpose:** Help contributors and AI coding agents quickly understand this mono-repo and be productive (build, test, add SDK features, add E2E tests). ✅

## Big picture 🔧

- The repo implements language SDKs for **40 languages** (Node/TS, Python, Go, .NET, Java, Rust, Ruby, PHP, Swift, Kotlin, C++, C, Dart, Scala, R, Perl, Lua, Shell/Bash, Elixir, Haskell, Clojure, Visual Basic, Delphi, Fortran, MATLAB, Ada, Objective-C, F#, Groovy, Julia, COBOL, OCaml, Zig, Nim, D, Erlang, Crystal, Tcl, Solidity, V) that speak to the **Copilot CLI** via **JSON-RPC** (see `README.md` and `nodejs/src/client.ts`).
- Typical flow: your App → SDK client → JSON-RPC → Copilot CLI (server mode). The CLI must be installed or you can connect to an external CLI server via the `CLI URL option (language-specific casing)` (Node: `cliUrl`, Go: `CLIUrl`, .NET: `CliUrl`, Python: `cli_url`).

## Most important files to read first 📚

- Top-level: `README.md` (architecture + quick start)
- Language entry points: `nodejs/src/client.ts`, `python/README.md`, `go/README.md`, `dotnet/README.md`, and 36 additional SDK READMEs (see `MULTI_LANGUAGE_SDKS.md` for the full list)
- Test harness & E2E: `test/harness/*`, Python harness wrapper `python/e2e/testharness/proxy.py`
- Schemas & type generation: `nodejs/scripts/generate-session-types.ts`
- Session snapshots used by E2E: `test/snapshots/` (used by the replay proxy)

## Developer workflows (commands you’ll use often) ▶️

- Monorepo helpers: use `just` tasks from repo root:
  - Install deps: `just install` (runs npm ci, uv pip install -e, go mod download, dotnet restore)
  - Format all: `just format` | Lint all: `just lint` | Test all: `just test`
- Per-language (core SDKs):
  - Node: `cd nodejs && npm ci` → `npm test` (Vitest), `npm run generate:session-types` to regenerate session-event types
  - Python: `cd python && uv pip install -e ".[dev]"` → `uv run pytest` (E2E tests use the test harness)
  - Go: `cd go && go test ./...`
  - .NET: `cd dotnet && dotnet test test/GitHub.Copilot.SDK.Test.csproj`
  - **.NET testing note:** Never add `InternalsVisibleTo` to any project file when writing tests. Tests must only access public APIs.
- Per-language (additional SDKs):
  - Java: `cd java && mvn test`
  - Rust: `cd rust && cargo test`
  - Ruby: `cd ruby && bundle exec rake test`
  - PHP: `cd php && composer test`
  - Swift: `cd swift && swift test`
  - Kotlin: `cd kotlin && ./gradlew test`
  - C++: `cd cpp && cmake -B build && cmake --build build && ctest --test-dir build`
  - C: `cd c && cmake -B build && cmake --build build && ctest --test-dir build`
  - Dart: `cd dart && dart test`
  - Scala: `cd scala && sbt test`
  - R: `cd r && Rscript -e "devtools::test()"`
  - Perl: `cd perl && prove -l t/`
  - Lua: `cd lua && busted`
  - Shell: `cd shell && bash -n lib/*.sh`
  - Elixir: `cd elixir && mix test`
  - Haskell: `cd haskell && cabal test`
  - Clojure: `cd clojure && clj -M:test`
  - Visual Basic: `cd visualbasic && dotnet test`
  - Delphi: `cd delphi && lazbuild tests/TestRunner.lpi && ./tests/TestRunner`
  - Fortran: `cd fortran && fpm test`
  - MATLAB: `cd matlab && matlab -batch "runtests('tests')"`
  - Ada: `cd ada && alr build && alr run copilot_sdk_tests`
  - Objective-C: `cd objc && xcodebuild test -scheme CopilotSDK`
  - F#: `cd fsharp && dotnet test`
  - Groovy: `cd groovy && ./gradlew test`
  - Julia: `cd julia && julia --project=. -e 'using Pkg; Pkg.test()'`
  - COBOL: `cd cobol && make test`
  - OCaml: `cd ocaml && dune runtest`
  - Zig: `cd zig && zig build test`
  - Nim: `cd nim && nimble test`
  - D: `cd dlang && dub test`
  - Erlang: `cd erlang && rebar3 ct`
  - Crystal: `cd crystal && crystal spec`
  - Tcl: `cd tcl && make test`
  - Solidity: `cd solidity && forge test`
  - V: `cd vlang && v test src/`

## Testing & E2E tips⚙️

- E2E runs against a local **replaying CAPI proxy** (see `test/harness/server.ts`). Most language E2E harnesses spawn that server automatically (see `python/e2e/testharness/proxy.py`).
- Tests rely on YAML snapshot exchanges under `test/snapshots/` — to add test scenarios, add or edit the appropriate YAML files and update tests.
- The harness prints `Listening: http://...` — tests parse this URL to configure CLI or proxy.

## MANDATORY: Feature parity rule 🚨

**This is the #1 rule of this repo.** Every feature that exists in the official `github/copilot-sdk` (Node, Python, Go, .NET) MUST be implemented in ALL 40 SDKs. No exceptions. No aspirational claims.

- **Never claim a feature in the README that isn't implemented in actual code across all 40 SDKs.**
- **After every upstream sync**, audit what changed and port ALL new features/methods/types to all 36 additional SDKs before shipping.
- **Run `node scripts/verify-sdk-coverage.mjs`** before every release. It checks all 36 additional SDKs for required RPC methods AND feature implementations. CI will block if any SDK has gaps.
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
- Type generation is centralized in `nodejs/scripts/generate-session-types.ts` and requires the `@github/copilot` schema to be present (often via `npm link` or installed package).

## Release announcements 📢

**After every release or feature ship**, post an announcement to GitHub Discussions in the **Announcements** category (category ID: `DIC_kwDOROUors4C2RPG`). Use the GraphQL `createDiscussion` mutation. The announcement should include:
- A title like "🚀 vX.Y.Z — <summary of key changes>"
- A body with: what's new (grouped by version if multiple), which SDKs were updated, and a link to the release + star CTA

## Integration & environment notes ⚠️

- The SDK requires a Copilot CLI installation or an external server reachable via the `CLI URL option (language-specific casing)` (Node: `cliUrl`, Go: `CLIUrl`, .NET: `CliUrl`, Python: `cli_url`) or `COPILOT_CLI_PATH`.
- Some scripts (typegen, formatting) call external tools: `gofmt`, `dotnet format`, `tsx` (available via npm), `quicktype`/`quicktype-core` (used by the Node typegen script), and `prettier` (provided as an npm devDependency). Most of these are available through the repo's package scripts or devDependencies—run `just install` (and `cd nodejs && npm ci`) to install them. Ensure the required tools are available in CI / developer machines.
- Tests may assume `node >= 18`, `python >= 3.9`, platform differences handled (Windows uses `shell=True` for npx in harness).

## Where to add new code or tests 🧭

- SDK code: `nodejs/src`, `python/copilot`, `go`, `dotnet/src`, `rust/src`, and `{java,ruby,php,swift,kotlin,cpp,c,dart,scala,r,perl,lua,shell,elixir,haskell,clojure,visualbasic,delphi,fortran,matlab,ada,objc,fsharp,groovy,julia,cobol,ocaml,zig,nim,dlang,erlang,crystal,tcl,solidity,vlang}/` (see each SDK's README for directory layout)
- Unit tests: `nodejs/test`, `python/*`, `go/*`, `dotnet/test`, `rust/tests`, and test directories within each of the 36 additional SDK directories
- E2E tests: `*/e2e/` folders that use the shared replay proxy and `test/snapshots/`
- Generated types: update schema in `@github/copilot` then run `cd nodejs && npm run generate:session-types` and commit generated files in `src/generated` or language generated location.

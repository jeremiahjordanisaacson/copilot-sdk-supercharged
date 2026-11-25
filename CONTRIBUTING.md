# Contributing

[fork]: https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged/fork
[pr]: https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged/compare

Thanks for your interest in contributing!

This repository contains GitHub Copilot SDK Supercharged: SDKs for 40 languages that control the GitHub Copilot CLI over JSON-RPC. The Node.js/TypeScript, Python, Go, and .NET SDKs are synced from the upstream Copilot SDK, and the other 36 SDKs are maintained in this repository.

Contributions to this project are [released](https://help.github.com/articles/github-terms-of-service/#6-contributions-under-repository-license) to the public under the [project's open source license](LICENSE).

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Before You Submit a PR

**Please discuss any feature work with us before writing code.**

The team already has a committed product roadmap, and features must be maintained in sync across all supported languages. Pull requests that introduce features not previously aligned with the team are unlikely to be accepted, regardless of their quality or scope.

If you submit a PR, **be sure to link to an associated issue describing the bug or agreed feature**. No PRs without context :)

**Want an SDK for a language we don't support yet?** Open an issue and request it! We currently support 40 languages and are always looking to expand. Community requests are how we prioritize which languages to add next.

## What We're Looking For

We welcome:

- Bug fixes with clear reproduction steps
- Improvements to documentation
- Making the SDKs more idiomatic and nice to use for each supported language
- Bug reports and feature suggestions on [our issue tracker](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged/issues), especially for bugs with repro steps

We are generally **not** looking for:

- New features, capabilities, or UX changes that haven't been discussed and agreed with the team
- Refactors or architectural changes
- Integrations with external tools or services
- Additional documentation
- **SDKs for other languages** — if you want to create a Copilot SDK for another language, we'd love to hear from you and may offer to link to your SDK from our repo. However we do not plan to add further language-specific SDKs to this repo in the short term, since we need to retain our maintenance capacity for moving forwards quickly with the existing language set. For other languages, please consider running your own external project.

## Prerequisites for Running and Testing Code

This is a multi-language SDK repository. Install the toolchain for the SDK you plan to work on. CI is
the source of truth for versions and commands: the four core SDKs are tested by
[`core-sdk-tests.yml`](.github/workflows/core-sdk-tests.yml) and the other 36 by
[`additional-sdk-tests.yml`](.github/workflows/additional-sdk-tests.yml). The versions below are the
ones CI installs, or the minimum the SDK's manifest declares.

| SDK | Toolchain |
| --- | --- |
| Node.js/TypeScript | Node.js `^20.19.0` or `>=22.12.0` (CI uses Node 22) |
| Python | Python 3.11+ (CI uses 3.12) and [uv](https://github.com/astral-sh/uv) |
| Go | The Go version in `go/go.mod` (1.26) |
| .NET | .NET SDK 10 (CI installs 8 and 10; unit tests run on `net10.0`) |
| Java | JDK 17+ (CI uses Temurin 21) and Maven |
| Rust | Stable Rust through [rustup](https://rustup.rs/) |
| Ruby | Ruby 3.1+ (CI uses 3.3) and Bundler |
| PHP | PHP 8.1+ (CI uses 8.3) and Composer |
| Swift | Swift 5.9+ (CI runs on macOS) |
| Kotlin | JDK 17 (the build uses `jvmToolchain(17)`) and a local Gradle install; there's no Gradle wrapper |
| C++ | A C++17 compiler, CMake 3.16+, and network access (CMake fetches nlohmann/json) |
| C | A C11 compiler, CMake 3.14+, and network access (CMake fetches cJSON) |
| Dart | Dart SDK 3.0+ |
| Scala | A JDK (CI uses Temurin 21) and sbt |
| R | R and the `devtools` package |
| Perl | Perl 5.20+ (CI uses 5.38) and cpanm |
| Lua | Lua 5.1+ and LuaRocks |
| Shell/Bash | Bash and [jq](https://jqlang.github.io/jq/download/) |
| Elixir | Elixir 1.15+ (CI uses 1.17 with OTP 27) |
| Haskell | GHC and cabal (CI uses GHC 9.6.6 and cabal 3.12.1.0) |
| Clojure | A JDK and the [Clojure CLI](https://clojure.org/guides/install_clojure) |
| Visual Basic | .NET 8 (the projects target `net8.0`; CI installs the .NET 8 SDK) |
| Delphi | Free Pascal (CI installs `fp-compiler`) |
| Fortran | A Fortran compiler and [fpm](https://fpm.fortran-lang.org/) (CI uses fpm 0.10.1) |
| MATLAB | MATLAB, or GNU Octave for the load check that CI runs |
| Ada | GNAT |
| Objective-C | Xcode (macOS) |
| F# | .NET 8 (the projects target `net8.0`; CI installs the .NET 8 SDK) |
| Groovy | JDK 17+ (CI uses Temurin 21) and a local Gradle install; there's no Gradle wrapper |
| Julia | Julia 1.x |
| COBOL | GnuCOBOL 3.x |
| OCaml | OCaml 5.2 and opam |
| Zig | Zig 0.13.0 |
| Nim | Nim and nimble |
| D | A D compiler and dub |
| Erlang | Erlang/OTP 27 and rebar3 |
| Crystal | Crystal and shards |
| Tcl | Tcl 8.6 (`tclsh`) and make |
| Solidity | Node.js 20+ (CI uses 22); this SDK is written in TypeScript and tested with Vitest |
| V | The V compiler |

### Shared test harness and Copilot CLI

The end-to-end tests in every language run the real Copilot CLI against a replaying proxy in
`test/harness`, which serves recorded responses from `test/snapshots`. Before you run tests that need
them, install the Node SDK's dependencies (they include the pinned Copilot CLI) and the harness, the
same way CI does. From the repository root:

```bash
cd nodejs && npm install --ignore-scripts && cd ..
cd test/harness && npm install --ignore-scripts && cd ../..
```

CI then sets `COPILOT_CLI_PATH` to `nodejs/node_modules/.bin/copilot` (the Go job uses the platform
binary in `nodejs/node_modules/@github/copilot-<platform>/`) and sets `GH_TOKEN` and `GITHUB_TOKEN` to
a placeholder (`fake-token-for-e2e-tests`).

On macOS, don't run `npm ci` or `npm test` in `nodejs/` or `test/harness/`: they can trigger a flood
of Keychain prompts. Use the `npm install --ignore-scripts` commands above, and validate Node.js
changes with `npm run typecheck`.

## Submitting a Pull Request

1. Fork and clone the repository
1. Install the toolchain and dependencies for the SDK(s) you're modifying (see above)
1. Make sure the tests and linters pass on your machine (see the commands below)
1. Create a new branch: `git checkout -b my-branch-name`
1. Make your change, add tests, and make sure the tests and linters still pass
1. If you changed an SDK's features, run `node scripts/verify-sdk-coverage.mjs` from the repository root. CI runs it on every pull request ([`sdk-coverage.yml`](.github/workflows/sdk-coverage.yml)) and fails if one of the 36 additional SDKs is missing a required RPC method or feature, or declares a different SDK protocol version
1. Push to your fork and [submit a pull request][pr]
1. Pat yourself on the back and wait for your pull request to be reviewed and merged.

### Running Tests and Linters

There's no task runner (such as `just`) at the repository root. The commands below match what CI runs
for each SDK. Where CI's step is broken or runs no tests, the comment says so and the command shown is
one that works. Every block and every line starts from the repository root.

#### Core SDKs

Node.js (there's no unit test script; CI type-checks and builds):

```bash
cd nodejs
npm install --ignore-scripts --no-audit --no-fund
npm run typecheck
npm run build
```

Python (install the `nodejs/` dependencies first; the tests use the CLI they pin):

```bash
cd python
export COPILOT_CLI_PATH="$PWD/../nodejs/node_modules/.bin/copilot"
uv sync --all-extras --dev
uv run ruff format --check .
uv run ruff check
uv run pytest -q --ignore=e2e -n 4 --timeout 120 \
  --deselect test_client.py::test_internal_test_options_override_only_owned_runtime \
  --deselect test_client.py::test_telemetry_configures_only_owned_runtime \
  --deselect test_client.py::test_default_connection \
  --deselect test_client.py::test_explicit_connection_ignores_invalid_default_override \
  --deselect test_client.py::test_inprocess_ui_thread_rpc_waits_for_scheduler \
  --deselect test_client.py::test_inprocess_ui_thread_rpc_rejects_out_of_range_action_ids \
  --deselect test_client.py::test_inprocess_forwards_integration_id_to_native_host \
  --deselect test_client.py::test_inprocess_scopes_environment_with_platform_casing \
  --deselect test_stdio_shutdown.py
```

Go (install the `nodejs/` dependencies first; CI points `COPILOT_CLI_PATH` at the Linux x64 binary, so
use the `copilot-<platform>` folder for your machine, such as `copilot-darwin-arm64`):

```bash
cd go
export COPILOT_CLI_PATH="$PWD/../nodejs/node_modules/@github/copilot-linux-x64/copilot"
go build ./...
go vet -unsafeptr=false ./...
go test -count=1 -timeout 20m \
  -skip 'TestDirectShutdownDrainsAfterChildExit|TestSpawnedJavaScriptUsesChildPath|TestOwnedStdioShutdown|TestOwnedTCPShutdown|TestSettings' \
  $(go list ./... | grep -v '/internal/e2e')
```

.NET:

```bash
cd dotnet
dotnet build test/Microsoft.Copilot.SDK.Test.csproj -p:CopilotSkipCliDownload=true
dotnet test test/Microsoft.Copilot.SDK.Test.csproj --no-build -f net10.0 \
  -p:CopilotSkipCliDownload=true \
  --filter "FullyQualifiedName!~.E2E.&FullyQualifiedName!~ReplayProxyTests&FullyQualifiedName!~E2ETestFixtureTests&FullyQualifiedName!~ConnectionToken&FullyQualifiedName!~StdioShutdownTests&FullyQualifiedName!~ClientSessionLifetimeTests.Stdio_&FullyQualifiedName!~FfiRuntimeHostLifetimeTests"
```

The deselected Python tests, skipped Go tests, and filtered .NET tests need runtime pieces that
upstream doesn't publish (replay-proxy scripts, a downloaded in-process runtime, or a newer runtime
than the pinned one). CI doesn't run golangci-lint or `dotnet format`.

#### Additional SDKs

Install the shared harness and CLI first (see above), and set `COPILOT_CLI_PATH` the way CI does.
Each line runs from the repository root.

```bash
# Java (there's no src/test, so `mvn test` only compiles; the end-to-end suite is the separate Maven
# project in java/e2e, which CI doesn't run. The second command builds the release artifacts, as maven-publish.yml does)
cd java && mvn test -q && mvn -q -B package source:jar javadoc:jar -DskipTests -Dcheckstyle.skip=true

# Rust
cd rust && cargo test

# Ruby
cd ruby && bundle install && bundle exec rake test

# PHP
cd php && composer install --no-interaction && composer test

# Swift (macOS)
cd swift && swift test

# Kotlin (local Gradle; there's no ./gradlew)
cd kotlin && gradle test

# C++ (ctest currently finds no registered tests, so the build is the check)
cd cpp && cmake -B build && cmake --build build && ctest --test-dir build --output-on-failure

# C (ctest currently finds no registered tests, so the build is the check)
cd c && cmake -B build && cmake --build build && ctest --test-dir build --output-on-failure

# Dart
cd dart && dart pub get && dart test

# Scala
cd scala && sbt test

# R (installs devtools and the DESCRIPTION dependencies, runs the tests, then runs R CMD check as cran-publish.yml does)
cd r && Rscript -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); install.packages("devtools"); devtools::install_deps(dependencies = TRUE)' && Rscript -e "devtools::test()" && R CMD build . && R CMD check copilot.sdk.supercharged_*.tar.gz --as-cran --no-manual

# Perl
cd perl && cpanm --notest Type::Tiny Moo File::Which Test::Exception HTTP::Tiny && cpanm --installdeps --notest . && prove -l e2e/

# Lua
cd lua && luarocks install lua-cjson && luarocks install busted && busted

# Shell/Bash (checks every file; CI's `bash -n lib/*.sh` checks only the first one)
cd shell && bash -c 'for f in lib/*.sh; do bash -n "$f" || exit 1; done'

# Elixir
cd elixir && mix deps.get && mix test

# Haskell
cd haskell && cabal update && cabal test

# Clojure (CI currently reports 0 tests: the test runner looks in test/, and the suite is in e2e/)
cd clojure && clojure -X:test

# Visual Basic (CI's plain `dotnet test` picks up the library project and runs no tests; this runs the test project)
cd visualbasic && dotnet test tests/CopilotSDK.Supercharged.VB.Tests.vbproj

# Fortran
cd fortran && fpm test

# MATLAB (a load check through GNU Octave; it doesn't run tests/TestClient.m)
cd matlab && octave --eval "addpath(genpath('.')); disp('MATLAB SDK loaded successfully')"

# Ada (CI only compiles the sources; the rest of the line builds and runs the unit tests)
cd ada && gnatmake -c src/*.adb && cd tests && gnatmake test_client.adb -I../src -aI../src && ./test_client

# F#
cd fsharp && dotnet test tests/CopilotSDK.Supercharged.FSharp.Tests.fsproj

# Groovy (local Gradle; there's no ./gradlew)
cd groovy && gradle test

# Julia
cd julia && julia --project=. -e 'using Pkg; Pkg.instantiate(); Pkg.test()'

# OCaml
cd ocaml && opam install . --deps-only --with-test -y && opam exec -- dune test

# Zig
cd zig && zig build test

# Nim
cd nim && nimble test -y

# D
cd dlang && dub test

# Erlang
cd erlang && rebar3 ct

# Crystal
cd crystal && shards install --without-development --skip-postinstall && crystal spec

# Tcl (runs tests/client_test.tcl; CI's step points at tests/run_tests.tcl, which doesn't exist)
cd tcl && make test

# Solidity (builds the local Node SDK first, because the Solidity SDK depends on it)
cd nodejs && npm install --ignore-scripts && npm run build && cd ../solidity && npm install --legacy-peer-deps && npm test

# V (CI only attempts `v src/*.v`, which V rejects; this runs the unit tests)
cd vlang && v test src/
```

These SDKs have no working automated check yet. CI runs a step for each, but the step can't fail the
build, and it currently fails:

- **COBOL**: `cobc -fsyntax-only src/*.cob` can't find the copybooks, and the sources don't compile with GnuCOBOL 3.2 even with `-I src`, so `make check` and `make test` fail too.
- **Delphi**: the Free Pascal syntax check (`fpc -Sd -v0 src/*.pas`) fails, and the tests are a DUnitX unit (`tests/TestClient.pas`) with no runner project.
- **Objective-C**: `xcodebuild test -scheme CopilotSDK` fails because the package has no `CopilotSDK` scheme (its targets are `CopilotObjCSDK` and `CopilotObjCSDKTests`).

Here are a few things you can do that will increase the likelihood of your pull request being accepted:

- Write tests.
- Keep your change as focused as possible. If there are multiple changes you would like to make that are not dependent upon each other, consider submitting them as separate pull requests.
- Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

## Resources

- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
- [Using Pull Requests](https://help.github.com/articles/about-pull-requests/)
- [GitHub Help](https://help.github.com)

## Contributing

[fork]: https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged/fork
[pr]: https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged/compare

Hi there! We're thrilled that you'd like to contribute to this project. Your help is essential for keeping it great.

Contributions to this project are [released](https://help.github.com/articles/github-terms-of-service/#6-contributions-under-repository-license) to the public under the [project's open source license](LICENSE).

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## What kinds of contributions we're looking for

We'd love your help with:

 * Fixing any bugs in the existing feature set
 * Making the SDKs more idiomatic and nice to use for each supported language
 * Improving documentation

If you have ideas for entirely new features, please post an issue or start a discussion. We're very open to new features but need to make sure they align with the direction of the underlying Copilot CLI and can be maintained in sync across all our supported languages.

**Want an SDK for a language we don't support yet?** Open an issue and request it! We currently support 21 languages and are always looking to expand. Community requests are how we prioritize which languages to add next.

## Prerequisites for running and testing code

This is a multi-language SDK repository. Install the tools for the SDK(s) you plan to work on:

### All SDKs
1. (Optional) Install [just](https://github.com/casey/just) command runner for convenience

### Node.js/TypeScript SDK
1. Install [Node.js](https://nodejs.org/) (v18+)
1. Install dependencies: `cd nodejs && npm ci`

### Python SDK
1. Install [Python 3.8+](https://www.python.org/downloads/)
1. Install [uv](https://github.com/astral-sh/uv)
1. Install dependencies: `cd python && uv pip install -e ".[dev]"`

### Go SDK
1. Install [Go 1.24+](https://go.dev/doc/install)
1. Install [golangci-lint](https://golangci-lint.run/welcome/install/#local-installation)
1. Install dependencies: `cd go && go mod download`

### .NET SDK
1. Install [.NET 8.0+](https://dotnet.microsoft.com/download)
1. Install [Node.js](https://nodejs.org/) (v18+) (the .NET tests depend on a TypeScript-based test harness)
1. Install npm dependencies (from the repository root):
   ```bash
   cd nodejs && npm ci
   cd test/harness && npm ci
   ```
1. Install .NET dependencies: `cd dotnet && dotnet restore`

### Java SDK
1. Install [Java 17+](https://adoptium.net/) (JDK)
1. Install [Maven 3.8+](https://maven.apache.org/download.cgi)
1. Install dependencies: `cd java && mvn dependency:resolve`

### Rust SDK
1. Install [Rust toolchain](https://rustup.rs/) via rustup
1. Install dependencies: `cd rust && cargo fetch`

### Ruby SDK
1. Install [Ruby 3.1+](https://www.ruby-lang.org/en/downloads/)
1. Install [Bundler](https://bundler.io/): `gem install bundler`
1. Install dependencies: `cd ruby && bundle install`

### PHP SDK
1. Install [PHP 8.1+](https://www.php.net/downloads)
1. Install [Composer](https://getcomposer.org/download/)
1. Install dependencies: `cd php && composer install`

### Swift SDK
1. Install [Swift 5.9+](https://www.swift.org/install/) (Xcode on macOS, or the Swift toolchain on Linux)
1. Resolve dependencies: `cd swift && swift package resolve`

### Kotlin SDK
1. Install [JDK 17+](https://adoptium.net/)
1. Install [Kotlin 1.9+](https://kotlinlang.org/docs/getting-started.html) (bundled with Gradle)
1. Install dependencies: `cd kotlin && ./gradlew dependencies`

### C++ SDK
1. Install a C++17-compatible compiler (gcc 9+, clang 10+, or MSVC 2019+)
1. Install [CMake 3.16+](https://cmake.org/download/)
1. Configure: `cd cpp && cmake -B build`

### C SDK
1. Install a C11-compatible compiler (gcc, clang, or MSVC)
1. Install [CMake 3.16+](https://cmake.org/download/)
1. Configure: `cd c && cmake -B build`

### Dart SDK
1. Install [Dart SDK 3.0+](https://dart.dev/get-dart)
1. Install dependencies: `cd dart && dart pub get`

### Scala SDK
1. Install [JDK 17+](https://adoptium.net/)
1. Install [sbt](https://www.scala-sbt.org/download.html) (Scala 3.4+ is resolved by sbt)
1. Install dependencies: `cd scala && sbt update`

### R SDK
1. Install [R 4.0+](https://cran.r-project.org/)
1. Install required packages: `Rscript -e "install.packages(c('jsonlite', 'R6', 'processx', 'devtools'))"`

### Perl SDK
1. Install [Perl 5.32+](https://www.perl.org/get.html)
1. Install required modules: `cpanm Moo JSON::PP IPC::Open3`

### Lua SDK
1. Install [Lua 5.1+](https://www.lua.org/download.html)
1. Install [LuaRocks](https://luarocks.org/)
1. Install dependencies: `cd lua && luarocks install lua-cjson && luarocks install busted`

### Shell/Bash SDK
1. Ensure [Bash 4+](https://www.gnu.org/software/bash/) is installed
1. Install [jq](https://stedolan.github.io/jq/download/)

### Elixir SDK
1. Install [Elixir 1.15+](https://elixir-lang.org/install.html) (includes Erlang/OTP)
1. Install dependencies: `cd elixir && mix deps.get`

### Haskell SDK
1. Install [GHC 9.4+](https://www.haskell.org/ghcup/) via ghcup
1. Install [Cabal](https://www.haskell.org/cabal/) (included with ghcup)
1. Install dependencies: `cd haskell && cabal update && cabal build --only-dependencies`

### Clojure SDK
1. Install [Java 17+](https://adoptium.net/) (JDK)
1. Install [Clojure 1.12+](https://clojure.org/guides/install_clojure)
1. Download dependencies: `cd clojure && clj -P`

## Submitting a pull request

1. [Fork][fork] and clone the repository
1. Install dependencies for the SDK(s) you're modifying (see above)
1. Make sure the tests pass on your machine (see commands below)
1. Make sure linter passes on your machine (see commands below)
1. Create a new branch: `git checkout -b my-branch-name`
1. Make your change, add tests, and make sure the tests and linter still pass
1. Push to your fork and [submit a pull request][pr]
1. Pat yourself on the back and wait for your pull request to be reviewed and merged.

### Running tests and linters

If you installed `just`, you can use it to run tests and linters for the core SDKs (Node.js, Python, Go, .NET):

```bash
# Core SDKs via just
just test          # Run all core SDK tests
just lint          # Run all core SDK linters
just format        # Format all core SDK code

# Individual core SDKs
just test-nodejs   # Node.js tests
just test-python   # Python tests
just test-go       # Go tests
just test-dotnet   # .NET tests

just lint-nodejs   # Node.js linting
just lint-python   # Python linting
just lint-go       # Go linting
just lint-dotnet   # .NET linting
```

For the remaining SDKs, run commands directly in each SDK directory. Below are the test and lint commands for all 21 SDKs:

```bash
# Node.js
cd nodejs && npm test && npm run lint

# Python
cd python && uv run pytest && uv run ruff check .

# Go
cd go && go test ./... && golangci-lint run ./...

# .NET
cd dotnet && dotnet test test/GitHub.Copilot.SDK.Test.csproj

# Java
cd java && mvn test
cd java && mvn compile

# Rust
cd rust && cargo test
cd rust && cargo clippy

# Ruby
cd ruby && bundle exec rake test
cd ruby && bundle exec rubocop

# PHP
cd php && composer test
cd php && composer lint

# Swift
cd swift && swift build && swift test

# Kotlin
cd kotlin && ./gradlew test
cd kotlin && ./gradlew check

# C++
cd cpp && cmake -B build && cmake --build build && ctest --test-dir build

# C
cd c && cmake -B build && cmake --build build && ctest --test-dir build

# Dart
cd dart && dart test
cd dart && dart analyze

# Scala
cd scala && sbt test
cd scala && sbt compile

# R
cd r && Rscript -e "devtools::test()"

# Perl
cd perl && prove -l t/

# Lua
cd lua && busted

# Shell/Bash
cd shell && bash -n lib/*.sh

# Elixir
cd elixir && mix test
cd elixir && mix format --check-formatted

# Haskell
cd haskell && cabal test
cd haskell && cabal build

# Clojure
cd clojure && clj -M:test
```

Here are a few things you can do that will increase the likelihood of your pull request being accepted:

- Write tests.
- Keep your change as focused as possible. If there are multiple changes you would like to make that are not dependent upon each other, consider submitting them as separate pull requests.
- Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

## Resources

- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
- [Using Pull Requests](https://help.github.com/articles/about-pull-requests/)
- [GitHub Help](https://help.github.com)

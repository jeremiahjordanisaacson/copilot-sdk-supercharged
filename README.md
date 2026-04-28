# GitHub Copilot SDK - Supercharged

![GitHub Copilot SDK SUPERCHARGED](./assets/SUPERCHARGED!!!.gif)

[![npm](https://img.shields.io/npm/v/copilot-sdk-supercharged?logo=npm&label=npm&color=CB3837)](https://www.npmjs.com/package/copilot-sdk-supercharged)
[![PyPI](https://img.shields.io/pypi/v/copilot-sdk-supercharged?logo=python&label=PyPI&color=3775A9)](https://pypi.org/project/copilot-sdk-supercharged/)
[![crates.io](https://img.shields.io/crates/v/copilot-sdk-supercharged?logo=rust&label=crates.io&color=dea584)](https://crates.io/crates/copilot-sdk-supercharged)
[![NuGet](https://img.shields.io/nuget/v/CopilotSDK.Supercharged?logo=nuget&label=NuGet&color=004880)](https://www.nuget.org/packages/CopilotSDK.Supercharged)
[![Gem](https://img.shields.io/gem/v/copilot-sdk-supercharged?logo=rubygems&label=RubyGems&color=E9573F)](https://rubygems.org/gems/copilot-sdk-supercharged)
[![Hex](https://img.shields.io/hexpm/v/copilot_sdk_supercharged?logo=elixir&label=Hex&color=6e4a7e)](https://hex.pm/packages/copilot_sdk_supercharged)
[![Clojars](https://img.shields.io/clojars/v/net.clojars.jeremiahisaacson/copilot-sdk-supercharged?logo=clojure&label=Clojars&color=63B132)](https://clojars.org/net.clojars.jeremiahisaacson/copilot-sdk-supercharged)
[![LuaRocks](https://img.shields.io/badge/LuaRocks-latest-2C2D72?logo=lua)](https://luarocks.org/modules/jeremiahisaacson/copilot-sdk-supercharged)
[![GitHub Stars](https://img.shields.io/github/stars/jeremiahjordanisaacson/copilot-sdk-supercharged?style=flat&logo=github&color=238636)](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged)
[![Registries](https://img.shields.io/badge/registries-8-f0883e?style=flat)](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged#available-sdks)
[![Languages](https://img.shields.io/badge/languages-21-58a6ff?style=flat)](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged#available-sdks)

**21 languages. One SDK.** Agents for every app.

Embed Copilot's agentic workflows in your application—now available in public preview as a programmable SDK for 21 languages.

The GitHub Copilot SDK exposes the same engine behind Copilot CLI: a production-tested agent runtime you can invoke programmatically. No need to build your own orchestration—you define agent behavior, Copilot handles planning, tool invocation, file edits, and more.

## Available SDKs

| SDK | Location | Cookbook | Installation |
|-----|----------|---------|-------------|
| **Node.js / TypeScript** | [`nodejs/`](./nodejs/README.md) | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/nodejs/README.md) | `npm install copilot-sdk-supercharged` |
| **Python** | [`python/`](./python/README.md) | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/python/README.md) | `pip install copilot-sdk-supercharged` |
| **Go** | [`go/`](./go/README.md) | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/go/README.md) | `go get github.com/github/copilot-sdk/go` |
| **.NET / C#** | [`dotnet/`](./dotnet/README.md) | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/dotnet/README.md) | `dotnet add package CopilotSDK.Supercharged` |
| **Java** | [`java/`](./java/README.md) | [Cookbook](./java/cookbook/README.md) | `io.github.jeremiahjordanisaacson:copilot-sdk-supercharged` |
| **Rust** | [`rust/`](./rust/README.md) | [Cookbook](./rust/cookbook/README.md) | `cargo add copilot-sdk-supercharged` |
| **Ruby** | [`ruby/`](./ruby/README.md) | [Cookbook](./ruby/cookbook/README.md) | `gem install copilot-sdk-supercharged` |
| **PHP** | [`php/`](./php/README.md) | [Cookbook](./php/cookbook/README.md) | Composer |
| **Swift** | [`swift/`](./swift/README.md) | [Cookbook](./swift/cookbook/README.md) | Swift Package Manager |
| **Kotlin** | [`kotlin/`](./kotlin/README.md) | [Cookbook](./kotlin/cookbook/README.md) | Gradle |
| **C++** | [`cpp/`](./cpp/README.md) | [Cookbook](./cpp/cookbook/README.md) | CMake |
| **C** | [`c/`](./c/README.md) | [Cookbook](./c/cookbook/README.md) | CMake |
| **Dart** | [`dart/`](./dart/README.md) | [Cookbook](./dart/cookbook/README.md) | pub |
| **Scala** | [`scala/`](./scala/README.md) | [Cookbook](./scala/cookbook/README.md) | sbt |
| **R** | [`r/`](./r/README.md) | [Cookbook](./r/cookbook/README.md) | CRAN |
| **Perl** | [`perl/`](./perl/README.md) | [Cookbook](./perl/cookbook/README.md) | CPAN |
| **Lua** | [`lua/`](./lua/README.md) | [Cookbook](./lua/cookbook/README.md) | `luarocks install copilot-sdk-supercharged` |
| **Shell / Bash** | [`shell/`](./shell/README.md) | [Cookbook](./shell/cookbook/README.md) | Source |
| **Elixir** | [`elixir/`](./elixir/README.md) | [Cookbook](./elixir/cookbook/README.md) | `{:copilot_sdk_supercharged, "~> 2.0"}` |
| **Haskell** | [`haskell/`](./haskell/README.md) | [Cookbook](./haskell/cookbook/README.md) | Cabal |
| **Clojure** | [`clojure/`](./clojure/README.md) | [Cookbook](./clojure/cookbook/README.md) | `net.clojars.jeremiahisaacson/copilot-sdk-supercharged` |

See the individual SDK READMEs for usage examples and API reference. For architecture details, see [`MULTI_LANGUAGE_SDKS.md`](./MULTI_LANGUAGE_SDKS.md).

## What's New in v2.0.0

Synced with upstream `github/copilot-sdk` v0.3.0 (public preview) and ported all features to all 21 SDKs:

- **Per-session GitHub authentication** – Scope auth tokens per session for multi-tenant apps
- **Session idle timeout** – Auto-cleanup inactive sessions with configurable timeout
- **SessionFs** – Session-scoped filesystem provider with 10 I/O operations
- **Commands & UI Elicitation** – Register slash commands and interactive prompts/dialogs
- **System prompt customization** – Fine-grained control with append/replace/customize modes
- **Per-agent skills** – Preload skill content into agent context at startup
- **Per-agent tool visibility** – `excludedTools` to control tool access per agent
- **Runtime request headers** – Custom HTTP headers per message turn
- **Model capabilities override** – Deep-merge overrides for model feature flags
- **Config discovery** – Auto-detect MCP servers and skill directories from workspace
- **Sub-agent streaming events** – Control streaming event forwarding from sub-agents
- **session.getMetadata()** – Retrieve session metadata via RPC
- **MCP server config refactoring** – Separate stdio/HTTP server config types
- **Image generation** – Response format and image options across all SDKs

## Getting Started

For a complete walkthrough, see the **[Getting Started Guide](./docs/getting-started.md)**.

Quick steps:

1. **(Optional) Install the Copilot CLI**

For Node.js, Python, and .NET SDKs, the Copilot CLI is bundled automatically and no separate installation is required.
For the Go SDK, [install the CLI manually](https://github.com/features/copilot/cli) or ensure `copilot` is available in your PATH.

2. **Install your preferred SDK** using the commands above.

3. **See the SDK README** for usage examples and API documentation.

## Architecture

All SDKs communicate with the Copilot CLI server via JSON-RPC:

```
Your Application
       ↓
  SDK Client
       ↓ JSON-RPC
  Copilot CLI (server mode)
```

The SDK manages the CLI process lifecycle automatically. You can also connect to an external CLI server. See the [Getting Started Guide](./docs/getting-started.md#connecting-to-an-external-cli-server) for details on running the CLI in server mode.

## FAQ

### Do I need a GitHub Copilot subscription to use the SDK?

Yes, a GitHub Copilot subscription is required to use the GitHub Copilot SDK, **unless you are using BYOK (Bring Your Own Key)**. With BYOK, you can use the SDK without GitHub authentication by configuring your own API keys from supported LLM providers. For standard usage (non-BYOK), refer to the [GitHub Copilot pricing page](https://github.com/features/copilot#pricing), which includes a free tier with limited usage.

### How does billing work for SDK usage?

Billing for the GitHub Copilot SDK is based on the same model as the Copilot CLI, with each prompt being counted towards your premium request quota. For more information on premium requests, see [Requests in GitHub Copilot](https://docs.github.com/en/copilot/concepts/billing/copilot-requests).

### Does it support BYOK (Bring Your Own Key)?

Yes, the GitHub Copilot SDK supports BYOK (Bring Your Own Key). You can configure the SDK to use your own API keys from supported LLM providers (e.g. OpenAI, Azure AI Foundry, Anthropic) to access models through those providers. See the **[BYOK documentation](./docs/auth/byok.md)** for setup instructions and examples.

**Note:** BYOK uses key-based authentication only. Microsoft Entra ID (Azure AD), managed identities, and third-party identity providers are not supported.

### What authentication methods are supported?

The SDK supports multiple authentication methods:

- **GitHub signed-in user** - Uses stored OAuth credentials from `copilot` CLI login
- **OAuth GitHub App** - Pass user tokens from your GitHub OAuth app
- **Environment variables** - `COPILOT_GITHUB_TOKEN`, `GH_TOKEN`, `GITHUB_TOKEN`
- **BYOK** - Use your own API keys (no GitHub auth required)

See the **[Authentication documentation](./docs/auth/index.md)** for details on each method.

### Do I need to install the Copilot CLI separately?

No — for Node.js, Python, and .NET SDKs, the Copilot CLI is bundled automatically as a dependency. You do not need to install it separately.

For Go SDK, you may still need to install the CLI manually.

Advanced: You can override the bundled CLI using `cliPath` or `cliUrl` if you want to use a custom CLI binary or connect to an external server.

### What tools are enabled by default?

By default, the SDK will operate the Copilot CLI in the equivalent of `--allow-all` being passed to the CLI, enabling all first-party tools, which means that the agents can perform a wide range of actions, including file system operations, Git operations, and web requests. You can customize tool availability by configuring the SDK client options to enable and disable specific tools. Refer to the individual SDK documentation for details on tool configuration and Copilot CLI for the list of tools available.

### Can I use custom agents, skills or tools?

Yes, the GitHub Copilot SDK allows you to define custom agents, skills, and tools. You can extend the functionality of the agents by implementing your own logic and integrating additional tools as needed. Refer to the SDK documentation of your preferred language for more details.

### Are there instructions for Copilot to speed up development with the SDK?

Yes, check out the custom instructions for each SDK:

- **[Node.js / TypeScript](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-nodejs.instructions.md)**
- **[Python](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-python.instructions.md)**
- **[.NET](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-csharp.instructions.md)**
- **[Go](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-go.instructions.md)**
- **[Java](https://github.com/github/copilot-sdk-java/blob/main/instructions/copilot-sdk-java.instructions.md)**

### What models are supported?

All models available via Copilot CLI are supported in the SDK. The SDK also exposes a method which will return the models available so they can be accessed at runtime.

### Is the SDK production-ready?

The GitHub Copilot SDK is currently in Public Preview. While it is functional and can be used for development and testing, it may not yet be suitable for production use.

### How do I report issues or request features?

Please use the [GitHub Issues](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged/issues) page to report bugs or request new features. We welcome your feedback to help improve the SDK.

## Quick Links

- **[Documentation](./docs/index.md)** – Full documentation index
- **[Getting Started](./docs/getting-started.md)** – Tutorial to get up and running
- **[Setup Guides](./docs/setup/index.md)** – Architecture, deployment, and scaling
- **[Authentication](./docs/auth/index.md)** – GitHub OAuth, BYOK, and more
- **[Features](./docs/features/index.md)** – Hooks, custom agents, MCP, skills, and more
- **[Architecture](./MULTI_LANGUAGE_SDKS.md)** – How all 21 SDKs work under the hood
- **[Troubleshooting](./docs/troubleshooting/debugging.md)** – Common issues and solutions
- **[Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk)** – Practical recipes for common tasks across all languages
- **[More Resources](https://github.com/github/awesome-copilot/blob/main/collections/copilot-sdk.md)** – Additional examples, tutorials, and community resources
- **[Contributing](./CONTRIBUTING.md)** – How to contribute (new languages welcome!)
- **[Website](https://jeremiahjordanisaacson.github.io/copilot-sdk-supercharged/)** – Landing page with examples in every language

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) for contribution guidelines.

## License

MIT

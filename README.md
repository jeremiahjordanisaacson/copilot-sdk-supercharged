# Microsoft Copilot SDK

> [!IMPORTANT]
> **GitHub Copilot SDK is becoming Microsoft Copilot SDK**, the programmable interface for the Microsoft Copilot harness, the shared agent foundation used across Microsoft and GitHub products.
>
> The SDK repository will move to the Microsoft organization as part of a phased transition. Existing package names, namespaces, and some documentation may continue to reference GitHub Copilot SDK during the transition. **No immediate changes are required for existing users.** [Learn more about the transition](https://aka.ms/copilot-harness/announce).
> 
> Support for the Copilot SDK remains the same. For more information: [SUPPORT.md](SUPPORT.md)

[![NPM Downloads](https://img.shields.io/npm/dm/%40microsoft%2Fcopilot-sdk?label=npm)](https://www.npmjs.com/package/@microsoft/copilot-sdk)
[![PyPI - Downloads](https://img.shields.io/pypi/dm/microsoft-copilot-sdk?label=PyPI)](https://pypi.org/project/microsoft-copilot-sdk/)
[![NuGet Downloads](https://img.shields.io/nuget/dt/Microsoft.Copilot.SDK?label=NuGet)](https://www.nuget.org/packages/Microsoft.Copilot.SDK)
[![Go Reference](https://img.shields.io/badge/Go-Reference-00ADD8?logo=go&logoColor=white)](https://pkg.go.dev/github.com/microsoft/copilot-sdk/go/v2)
[![crates.io](https://img.shields.io/crates/v/copilot-sdk?label=crates.io)](https://crates.io/crates/copilot-sdk)
[![Maven Central](https://img.shields.io/maven-central/v/com.microsoft/copilot-sdk?label=Maven%20Central)](https://central.sonatype.com/artifact/com.microsoft/copilot-sdk)

Agents for every app.

Embed Copilot's agentic workflows in your application with the Microsoft Copilot SDK for Python, TypeScript, Go, .NET, Java, and Rust.

The Microsoft Copilot SDK exposes the same engine behind Copilot CLI: a production-tested agent runtime you can invoke programmatically. No need to build your own orchestration—you define agent behavior, Copilot handles planning, tool invocation, file edits, and more.

## Available SDKs

| SDK                      | Location                                                                | Cookbook                                                                                              | Installation                                                                                                                                                                                                                               | API docs                                                                       |
| ------------------------ | ----------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------ |
| **Node.js / TypeScript** | [`nodejs/`](./nodejs/)                                                  | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/nodejs/README.md) | `npm install @microsoft/copilot-sdk`                                                                                                                                                                                                          |                                                                                |
| **Python**               | [`python/`](./python/)                                                  | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/python/README.md) | `pip install microsoft-copilot-sdk`                                                                                                                                                                                                           |                                                                                |
| **Go**                   | [`go/`](./go/)                                                          | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/go/README.md)     | `go get github.com/microsoft/copilot-sdk/go/v2`                                                                                                                                                                                                  | [API docs](https://pkg.go.dev/github.com/microsoft/copilot-sdk/go/v2#readme-api-reference) |
| **.NET**                 | [`dotnet/`](./dotnet/)                                                  | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/dotnet/README.md) | `dotnet add package Microsoft.Copilot.SDK`                                                                                                                                                                                                    |                                                                                |
| **Rust**                 | [`rust/`](./rust/)                                                      | —                                                                                                     | `cargo add copilot-sdk`                                                                                                                                                                                                             | [API docs](https://docs.rs/copilot-sdk/latest/copilot_sdk/#api-reference) |
| **Java**                 | [`java/`](./java/) | [Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk/java/README.md)                                                                                                   | Maven coordinates<br>`com.microsoft:copilot-sdk`<br>See instructions for [Maven](./java/README.md#maven) and [Gradle](./java/README.md#gradle) | [API docs](https://javadoc.io/doc/com.microsoft/copilot-sdk/latest/) |

See the individual SDK READMEs for installation, usage examples, and API reference.

## Getting Started

For a complete walkthrough, see the **[Getting Started Guide](./docs/getting-started.md)**.

Quick steps:

1. **(Optional) Install the Copilot CLI**

A separate CLI installation is not required.
Node.js and .NET include platform runtime artifacts, Python downloads them, and Rust bundles them with its default features. Go applications must run the bundler or provide a compatible runtime package; Java applications must add their platform runtime classifier. Follow the language README before starting a client.

2. **Install your preferred SDK** using the commands above.

3. **See the SDK README** for usage examples and API documentation.

## Architecture

All SDKs drive the Copilot agent runtime over protocol-4 JSON-RPC. The runtime is a standalone component, separate from the Copilot CLI. By default, SDK v2 loads the native Rust runtime into your application through a C ABI and carries framed JSON-RPC messages over an in-memory connection. It does not embed a Node.js interpreter or start a separate runtime process:

```
Your Application
       ↓
  SDK Client
       ↓ JSON-RPC (in memory, C ABI)
  Native Copilot runtime
```

The SDK manages its runtime host, but the native library remains loaded for the application process lifetime. In-process hosting provides neither crash isolation nor a sandbox. See [Run the Copilot runtime in process](./docs/setup/in-process-runtime.md) for requirements and v2 migration options.

You can also run the runtime outside your application process:

- **Spawned child process**: select a spawned stdio or TCP connection (`RuntimeConnection.forSpawnedStdio()` or `RuntimeConnection.forSpawnedTcp()` in TypeScript and Java; each SDK has an equivalent). The SDK starts the runtime as a child process and manages its lifecycle. Use it when you need crash isolation or per-client process configuration.
- **Standalone runtime server**: run `copilot-runtime --server` yourself and attach to it over TCP with `RuntimeConnection.forTcp(host, port)`. One multi-tenant runtime server can serve many SDK clients and concurrent sessions. See [Connecting to a standalone runtime server](./docs/getting-started.md#connecting-to-a-standalone-runtime-server).

### Portable elicitation schemas

For form elicitation callbacks, the portable schema contract is an object form
with modeled `properties` and optional `required` fields (`type: "object"` on
the wire; an SDK may represent the object type implicitly). Applications that
support multiple SDK languages must use the modeled property definitions and
must not depend on additional schema metadata (such as a top-level `title`) or
on the insertion order of `properties`.

Node.js and Rust pass through the wire schema, including additional metadata
and property order. Python, Go, .NET, and Java reconstruct the callback schema
from the modeled fields and do not offer that same preservation guarantee.
Treat wire-schema preservation as a language-specific capability, not a
cross-language requirement.

### Agent Finder Skill installation APIs

Runtimes that grant `skill-confirmed-installation` expose
`skills.planInstall`, `skills.applyInstall`, `skills.planUninstall`,
`skills.applyUninstall`, `skills.installations.list/recover/status/cancel`,
and `skills.installations.setEnabled`. Skill install reviews include the full
verified `SKILL.md` content up to 256 KiB, catalogue identity, file manifest,
and non-absolute personal target location. Summaries report `ownershipState`,
`configuredEnabled`, `sessionState`, retained catalogue identity and
`installedAt`.

## FAQ

### Do I need a GitHub Copilot subscription to use the SDK?

Yes, a GitHub Copilot subscription is required to use the Microsoft Copilot SDK, **unless you are using BYOK (Bring Your Own Key)**. With BYOK, you can use the SDK without GitHub authentication by configuring your own API keys from supported LLM providers. For standard usage (non-BYOK), refer to the [GitHub Copilot pricing page](https://github.com/features/copilot#pricing), which includes a free tier with limited usage.

### How does billing work for SDK usage?

Billing for the Microsoft Copilot SDK is based on the same model as the Copilot CLI, with each prompt being counted towards your usage allowance. For more information on Copilot usage billing, see [Usage in GitHub Copilot](https://docs.github.com/en/copilot/reference/copilot-billing/models-and-pricing).

### Does it support BYOK (Bring Your Own Key)?

Yes, the Microsoft Copilot SDK supports BYOK (Bring Your Own Key). You can configure the SDK to use your own API keys from supported LLM providers (e.g. OpenAI, Microsoft Foundry, Anthropic) to access models through those providers. See the **[BYOK documentation](./docs/auth/byok.md)** for setup instructions and examples.

**Note:** BYOK uses key-based authentication only. Microsoft Entra ID (Azure AD), managed identities, and third-party identity providers are not supported.

### What authentication methods are supported?

The SDK supports multiple authentication methods:

- **GitHub signed-in user** - Uses stored OAuth credentials from `copilot` CLI login
- **OAuth GitHub App** - Pass user tokens from your GitHub OAuth app
- **Environment variables** - `COPILOT_GITHUB_TOKEN`, `GH_TOKEN`, `GITHUB_TOKEN`
- **BYOK** - Use your own API keys (no GitHub auth required)

See the **[Authentication documentation](./docs/auth/README.md)** for details on each method.

### Do I need to install the Copilot CLI separately?

No. The default needs a compatible native runtime, not a separately running CLI. Node.js, .NET, Python, and Rust provide acquisition or bundling; Go needs application-level bundling or an explicit runtime package, and Java needs the platform classifier dependency.

Advanced: You can run a runtime binary you supply as a child process, or connect to a standalone runtime server. See the individual SDK README for language-specific options.

### What tools are enabled by default?

By default, the SDK exposes the Copilot CLI's first-party tools, similar to running the CLI with `--allow-all`. Tool execution is still governed by each SDK's permission handler, so applications can approve, deny, or customize tool calls. You can customize tool availability by configuring the SDK client options to enable and disable specific tools. Refer to the individual SDK documentation for details on tool configuration and to the Copilot CLI documentation for the list of available tools.

### Can I use custom agents, skills or tools?

Yes, the Microsoft Copilot SDK allows you to define custom agents, skills, and tools. You can extend the functionality of the agents by implementing your own logic and integrating additional tools as needed. Refer to the SDK documentation of your preferred language for more details.

### Are there instructions or SDK guidance for Copilot to speed up development?

Yes, check out the custom instructions and SDK-specific guidance:

- **[Node.js / TypeScript](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-nodejs.instructions.md)**
- **[Python](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-python.instructions.md)**
- **[.NET](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-csharp.instructions.md)**
- **[Go](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-go.instructions.md)**
- **[Rust](./rust/README.md)** (SDK guidance; custom instructions not yet published)
- **[Java](https://github.com/github/awesome-copilot/blob/main/instructions/copilot-sdk-java.instructions.md)**

### What models are supported?

All models available via Copilot CLI are supported in the SDK. The SDK also exposes a method which will return the models available so they can be accessed at runtime.

### Is the SDK production-ready?

The Microsoft Copilot SDK is generally available and follows semantic versioning. See [CHANGELOG.md](./CHANGELOG.md) for release notes.

### How do I report issues or request features?

Please use the [GitHub Issues](https://github.com/github/copilot-sdk/issues) page to report bugs or request new features. We welcome your feedback to help improve the SDK.

## Quick Links

- **[Documentation](./docs/README.md)** – Full documentation index
- **[Getting Started](./docs/getting-started.md)** – Tutorial to get up and running
- **[Setup Guides](./docs/setup/README.md)** – Architecture, deployment, and scaling
- **[Authentication](./docs/auth/README.md)** – GitHub OAuth, BYOK, and more
- **[Features](./docs/features/README.md)** – Hooks, custom agents, MCP, skills, and more
- **[Troubleshooting](./docs/troubleshooting/debugging.md)** – Common issues and solutions
- **[Cookbook](https://github.com/github/awesome-copilot/blob/main/cookbook/copilot-sdk)** – Practical recipes for common tasks across all languages
- **[More Resources](https://github.com/github/awesome-copilot/blob/main/plugins/copilot-sdk/README.md)** – SDK instructions for C#, Go, Node.js/TypeScript, and Python, plus a skill

## Unofficial, Community-maintained SDKs

⚠️ Disclaimer: These are unofficial, community-driven SDKs and they are not supported by GitHub. Use at your own risk.

| SDK         | Location                                                 |
| ----------- | -------------------------------------------------------- |
| **Clojure** | [copilot-community-sdk/copilot-sdk-clojure][sdk-clojure] |
| **C++**     | [0xeb/copilot-sdk-cpp][sdk-cpp]                          |

[sdk-cpp]: https://github.com/0xeb/copilot-sdk-cpp
[sdk-clojure]: https://github.com/copilot-community-sdk/copilot-sdk-clojure

## License

MIT

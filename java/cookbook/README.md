# GitHub Copilot SDK Cookbook - Java

Short, practical recipes for the Java SDK (`io.github.jeremiahjordanisaacson:copilot-sdk-supercharged`).
Every sample uses the SDK's public API in the `com.github.copilot` package (the error-handling recipe
also catches `com.github.copilot.internal.JsonRpcClient.JsonRpcException`) and compiles with Java 17.

## Before you start

- Java 17 or newer, and the SDK dependency from [Installation](../README.md#installation).
- The Copilot CLI on your `PATH`, or `COPILOT_CLI_PATH` set to the CLI executable.
- A signed-in GitHub account with Copilot access.

Samples with a `main` method are complete programs. The snippets in
[Advanced Features](advanced-features.md) are fragments; that page lists what they assume.

To keep the samples short, they approve every permission request with
`(request, sessionId) -> new PermissionRequestResult("approve-once")`. Replace that handler with
your own policy before you ship.

## Recipes

- [Error Handling](error-handling.md): Handle startup failures, RPC errors, timeouts, and aborts, and shut down cleanly.
- [Multiple Sessions](multiple-sessions.md): Run independent conversations side by side and in parallel.
- [Persisting Sessions](persisting-sessions.md): Use your own session IDs, resume sessions after a restart, and manage saved sessions.
- [Tools and Skills](tools-and-skills.md): Define custom tools, load skill directories, add custom agents, and watch tool and sub-agent events.
- [Advanced Features](advanced-features.md): Per-session auth, idle timeout, system prompts, tool visibility, request headers, model capabilities, MCP servers, streaming, metadata, and structured output.

## Contributing

Add a new recipe by creating a markdown file in this folder and linking it above. Compile every
sample against the current SDK before you submit it.

# GitHub Copilot SDK Cookbook - Kotlin

Short, practical recipes for the Kotlin SDK (`io.github.jeremiahjordanisaacson:copilot-sdk-supercharged-kotlin`).
Every sample uses the SDK's public API in the `com.github.copilot` package and compiles with Kotlin 1.9.

## Before you start

- JDK 17 or newer, Kotlin 1.9 or newer, and the SDK built and installed as described in
  [Installation](../README.md#installation). Your build also needs `kotlinx-coroutines-core` and
  `kotlinx-serialization-json`, because the SDK's API uses their types.
- The Copilot CLI on your `PATH`, or `COPILOT_CLI_PATH` set to the CLI executable.
- A signed-in GitHub account with Copilot access.

All SDK calls that talk to the CLI are `suspend` functions, so the programs run inside
`runBlocking`. Samples with a `main` function are complete programs. The snippets in
[Advanced Features](advanced-features.md) are fragments; that page lists what they assume.

Event data is a `kotlinx.serialization.json.JsonObject`. Read a text field with
`event.data?.get("content")?.jsonPrimitive?.contentOrNull`.

To keep the samples short, they approve every permission request with
`{ _, _ -> PermissionRequestResult(kind = "approve-once") }`. Replace that handler with your own
policy before you ship.

## Recipes

- [Error Handling](error-handling.md): Handle startup failures, RPC errors, timeouts, and aborts, and shut down cleanly.
- [Multiple Sessions](multiple-sessions.md): Run independent conversations side by side and concurrently.
- [Persisting Sessions](persisting-sessions.md): Use your own session IDs, resume sessions after a restart, and manage saved sessions.
- [Tools and Skills](tools-and-skills.md): Define custom tools, load skill directories, add custom agents, and watch tool and sub-agent events.
- [Advanced Features](advanced-features.md): Per-session auth, system prompts, tool visibility, request headers, model capabilities, MCP servers, streaming, metadata, and structured output.

## Contributing

Add a new recipe by creating a markdown file in this folder and linking it above. Compile every
sample against the current SDK before you submit it.

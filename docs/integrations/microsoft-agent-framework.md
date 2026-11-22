# Microsoft agent framework integration

The [Microsoft Agent Framework](https://devblogs.microsoft.com/semantic-kernel/build-ai-agents-with-github-copilot-sdk-and-microsoft-agent-framework/) (MAF) composes multi-agent workflows across providers such as Azure OpenAI and Anthropic.

## Overview

The Microsoft Agent Framework is the unified successor to Semantic Kernel and AutoGen. It provides a standard interface for building, orchestrating, and deploying AI agents.

| Concept | Description |
|---------|-------------|
| **Microsoft Agent Framework** | Open-source framework for single- and multi-agent orchestration in .NET and Python |
| **Agent provider** | A backend that powers an agent (Copilot, Azure OpenAI, Anthropic, etc.) |
| **Orchestrator** | A MAF component that coordinates agents in sequential, concurrent, or handoff workflows |
| **A2A protocol** | Agent-to-Agent communication standard supported by the framework |

> [!WARNING]
> **SDK V2:** The .NET and Python MAF adapters are not yet compatible with Microsoft Copilot SDK. `Microsoft.Agents.AI.GitHub.Copilot` depends on `GitHub.Copilot.SDK`, and `agent-framework-github-copilot` requires `github-copilot-sdk<2.0`.
>
> Do not install these adapters alongside V2. The .NET adapter requires the old SDK types; the Python distributions share the `copilot` import and install overlapping files. MAF installation and usage examples are unavailable until compatible adapters are published. The examples below use the standalone SDK, not MAF.

## Prerequisites

Before you begin, ensure you have:

* A working [Copilot SDK setup](../getting-started.md) in your language of choice
* A GitHub Copilot subscription (Individual, Business, or Enterprise)
* The Copilot runtime, which the SDK bundles or downloads for you (see [Getting started](../getting-started.md))

## Installation

Install the standalone Copilot SDK using the [getting started guide](../getting-started.md). For the Java examples below:

<details>
<summary><strong>Java</strong></summary>

> [!NOTE]
> The Java SDK does not have a dedicated MAF integration package. Use the standard Copilot SDK directly—it provides tool calling, streaming, and custom agents out of the box.

```xml
<!-- Maven -->
<!-- Set copilot.sdk.version to the version published in java/README.md / Maven Central -->
<dependency>
    <groupId>com.microsoft</groupId>
    <artifactId>copilot-sdk</artifactId>
    <version>${copilot.sdk.version}</version>
</dependency>
```

</details>

## Basic usage

Use the Copilot SDK client directly to create a session and send a prompt:

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;

var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT));
client
        .start()
        .get();

var session = client
        .createSession(new CreateSessionOptions()
                .setModel("auto")
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
        .get();

var response = session
        .sendAndWait(new MessageOptions()
                .setPrompt("Explain how dependency injection works in Spring Boot"))
        .get();
System.out
        .println(response
                .getData()
                .content());

client
        .stop()
        .get();
```

</details>

## Adding custom tools

Extend your Copilot agent with custom function tools using the standalone SDK:

<details open>
<summary><strong>Node.js / TypeScript (standalone SDK)</strong></summary>

<!-- docs-validate: strict -->
```typescript
import { CopilotClient, defineTool } from "@microsoft/copilot-sdk";

const getWeather = defineTool("GetWeather", {
    description: "Get the current weather for a given location.",
    parameters: {
        type: "object",
        properties: {
            location: { type: "string", description: "City name" },
        },
        required: ["location"],
    },
    handler: async ({ location }: { location: string }) =>
        `The weather in ${location} is sunny, 25°C.`,
});

const client = new CopilotClient({ mode: "coding-agent" });
const session = await client.createSession({
    model: "auto",
    tools: {
        definitions: [getWeather],
    },
    permissions: {
        onRequest: async () => ({ kind: "approve-once" }),
    },
});

await session.sendAndWait({ prompt: "What's the weather like in Seattle?" });
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

var getWeather = ToolDefinition
        .create("GetWeather", "Get the current weather for a given location.", Map
                .of("type", "object", "properties", Map
                        .of("location", Map
                                .of("type", "string", "description", "City name")),
                        "required", List
                                .of("location")),
                invocation -> {
                    var location = (String) invocation
                            .getArguments()
                            .get("location");
                    return CompletableFuture
                            .completedFuture("The weather in " + location + " is sunny, 25°C.");
                });

try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
    client
            .start()
            .get();

    var session = client
            .createSession(new CreateSessionOptions()
                    .setModel("auto")
                    .setTools(new SessionToolOptions()
                            .setDefinitions(List
                                    .of(getWeather)))
                    .setPermissions(new SessionPermissionOptions()
                            .setOnRequest(PermissionHandler.APPROVE_ALL)))
            .get();

    session
            .sendAndWait(new MessageOptions()
                    .setPrompt("What's the weather like in Seattle?"))
            .get();
}
```

</details>

## Multi-agent workflows

The standalone SDK can coordinate sessions sequentially or concurrently without MAF.

### Sequential workflow

Run agents one after another, passing output from one to the next:

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;

// Java uses the standard SDK directly — no MAF orchestrator needed
var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT));
client
        .start()
        .get();

// Step 1: Code review session
var reviewer = client
        .createSession(new CreateSessionOptions()
                .setModel("auto")
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
        .get();

var review = reviewer
        .sendAndWait(new MessageOptions()
                .setPrompt("Review this PR for bugs, security issues, and best practices: " + "added retry logic to the HTTP client"))
        .get();

// Step 2: Documentation session using review output
var documentor = client
        .createSession(new CreateSessionOptions()
                .setModel("auto")
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
        .get();

var docs = documentor
        .sendAndWait(new MessageOptions()
                .setPrompt("Write documentation for these changes: " + review
                        .getData()
                        .content()))
        .get();
System.out
        .println(docs
                .getData()
                .content());

client
        .stop()
        .get();
```

</details>

### Concurrent workflow

Run multiple agents in parallel and aggregate their results:

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;
import java.util.concurrent.CompletableFuture;

// Java uses CompletableFuture for concurrent execution
var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT));
client
        .start()
        .get();

var securitySession = client
        .createSession(new CreateSessionOptions()
                .setModel("auto")
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
        .get();

var perfSession = client
        .createSession(new CreateSessionOptions()
                .setModel("auto")
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
        .get();

// Run both reviews concurrently
var securityFuture = securitySession
        .sendAndWait(new MessageOptions()
                .setPrompt("Focus on security vulnerabilities in this database query module"));
var perfFuture = perfSession
        .sendAndWait(new MessageOptions()
                .setPrompt("Focus on performance bottlenecks in this database query module"));

CompletableFuture
        .allOf(securityFuture, perfFuture)
        .get();

System.out
        .println("Security: " + securityFuture
                .get()
                .getData()
                .content());
System.out
        .println("Performance: " + perfFuture
                .get()
                .getData()
                .content());

client
        .stop()
        .get();
```

</details>

## Streaming responses

When building interactive applications, stream agent responses directly through the Copilot SDK to show real-time output:

<details open>
<summary><strong>Node.js / TypeScript (standalone SDK)</strong></summary>

<!-- docs-validate: strict -->
```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";

const client = new CopilotClient({ mode: "coding-agent" });
const session = await client.createSession({
    model: "auto",
    events: {
        streaming: true,
    },
    permissions: {
        onRequest: async () => ({ kind: "approve-once" }),
    },
});

session.on("assistant.message_delta", (event) => {
    process.stdout.write(event.data.deltaContent ?? "");
});

await session.sendAndWait({ prompt: "Write a quicksort implementation in TypeScript" });
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.generated.AssistantMessageDeltaEvent;
import com.microsoft.copilot.rpc.*;

var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT));
client
        .start()
        .get();

var session = client
        .createSession(new CreateSessionOptions()
                .setModel("auto")
                .setEvents(new SessionEventOptions()
                        .setStreaming(true))
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
        .get();

session
        .on(AssistantMessageDeltaEvent.class, event -> {
            System.out
                    .print(event
                            .getData()
                            .deltaContent());
        });

session
        .sendAndWait(new MessageOptions()
                .setPrompt("Write a quicksort implementation in Java"))
        .get();
System.out
        .println();

client
        .stop()
        .get();
```

</details>

## Configuration reference

For standalone SDK client and session configuration, see the [getting started guide](../getting-started.md).

## Best practices

### Choose the right level of integration

Until compatible MAF adapters are available, use the standalone SDK directly:

<!-- docs-validate: strict -->
```typescript
// Standalone SDK — full control, simpler setup
import { CopilotClient } from "@microsoft/copilot-sdk";

const client = new CopilotClient({ mode: "coding-agent" });
const session = await client.createSession({
    model: "auto",
    permissions: {
        onRequest: async () => ({ kind: "approve-once" }),
    },
});
const response = await session.sendAndWait({ prompt: "Explain this code" });
```

### Keep agents focused

When building multi-agent workflows, give each agent a specific role with clear instructions. Avoid overlapping responsibilities:

<!-- docs-validate: strict -->
```typescript
// ❌ Too vague — overlapping roles
const vagueAgents = [
    { instructions: "Help with code" },
    { instructions: "Assist with programming" },
];

// ✅ Focused — clear separation of concerns
const focusedAgents = [
    { instructions: "Review code for security vulnerabilities. Flag SQL injection, XSS, and auth issues." },
    { instructions: "Optimize code performance. Focus on algorithmic complexity and memory usage." },
];
```

## See also

* [Getting Started](../getting-started.md): initial Copilot SDK setup
* [Custom Agents](../features/custom-agents.md): define specialized sub-agents within the SDK
* [Custom Skills](../features/skills.md): reusable prompt modules
* [Microsoft Agent Framework documentation](https://learn.microsoft.com/en-us/agent-framework/agents/providers/github-copilot): official MAF docs for the legacy Copilot adapter
* [Blog: Build AI Agents with GitHub Copilot SDK and Microsoft Agent Framework](https://devblogs.microsoft.com/semantic-kernel/build-ai-agents-with-github-copilot-sdk-and-microsoft-agent-framework/)
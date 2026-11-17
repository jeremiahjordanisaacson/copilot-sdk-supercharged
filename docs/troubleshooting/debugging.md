# Debugging guide

This guide covers common issues and debugging techniques for the Copilot SDK across all supported languages.

## Table of contents

- [Enable Debug Logging](#enable-debug-logging)
- [Common Issues](#common-issues)
- [MCP Server Debugging](#mcp-server-debugging)
- [Connection Issues](#connection-issues)
- [Tool Execution Issues](#tool-execution-issues)
- [Platform-Specific Issues](#platform-specific-issues)

## Enable debug logging

The first step in debugging is enabling verbose logging to see what's happening under the hood.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    observability: {
        logLevel: "debug",
    },
});
```

</details>

<details>
<summary><strong>Python</strong></summary>

```python
from copilot import CopilotClient

client = CopilotClient(mode="coding-agent", observability={"log_level": "debug"})
```

</details>

<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: hidden -->

```go
package main

import copilot "github.com/microsoft/copilot-sdk/go/v2"

func main() {
    client := copilot.NewClient(&copilot.ClientOptions{
        Mode: copilot.ModeCodingAgent,
        Observability: copilot.ClientObservabilityOptions{
            LogLevel: "debug",
        },
    })
    _ = client
}
```

<!-- /docs-validate: hidden -->

```go
import copilot "github.com/microsoft/copilot-sdk/go/v2"


client := copilot.NewClient(&copilot.ClientOptions{
    Mode: copilot.ModeCodingAgent,
    Observability: copilot.ClientObservabilityOptions{
        LogLevel: "debug",
    },
})

```

</details>

<details>
<summary><strong>.NET</strong></summary>

```csharp
using Microsoft.Copilot;
using Microsoft.Extensions.Logging;

// Using ILogger (AddConsole comes from the Microsoft.Extensions.Logging.Console package)
var loggerFactory = LoggerFactory.Create(builder =>
{
    builder.SetMinimumLevel(LogLevel.Debug);
    builder.AddConsole();
});
var client = new CopilotClient(new CopilotClientOptions
{
    Mode = CopilotClientMode.CodingAgent,
    Observability = new ClientObservabilityOptions
    {
        LogLevel = CopilotLogLevel.Debug,
        Logger = loggerFactory.CreateLogger<CopilotClient>()
    }
});
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;

var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
        .setObservability(new ClientObservabilityOptions()
                .setLogLevel("debug")));
```

</details>

### Log directory

The SDK-managed out-of-process runtime writes redacted
`process-<timestamp>-<pid>.log` files under the Copilot home's `logs` directory
by default in `coding-agent` mode. SDKs in `empty` mode do not set the logging
opt-in, but an inherited setting can still enable it; session events remain
available to `collectLogs`. An explicitly selected older runtime executable may
ignore this setting. You can specify a custom log location with `--log-dir`;
`collectLogs` searches the configured directory. In-process connections do
not start a separate logging process.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forSpawnedStdio({ args: ["--log-dir", "/path/to/logs"] }),
});
```

</details>

<details>
<summary><strong>Python</strong></summary>

```python
from copilot import CopilotClient, RuntimeConnection

client = CopilotClient(
    mode="coding-agent",
    runtime=RuntimeConnection.for_spawned_stdio(args=["--log-dir", "/path/to/logs"])
)
```

</details>

<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: hidden -->

```go
package main

import copilot "github.com/microsoft/copilot-sdk/go/v2"

func main() {
	client := copilot.NewClient(&copilot.ClientOptions{
		Mode: copilot.ModeCodingAgent,
		Runtime: copilot.SpawnedStdioConnection{
			Args: []string{"--log-dir", "/path/to/logs"},
		},
	})
	_ = client
}
```

<!-- /docs-validate: hidden -->

```go
client := copilot.NewClient(&copilot.ClientOptions{
    Mode: copilot.ModeCodingAgent,
    Runtime: copilot.SpawnedStdioConnection{
        Args: []string{"--log-dir", "/path/to/logs"},
    },
})

```

</details>

<details>
<summary><strong>.NET</strong></summary>

```csharp
var client = new CopilotClient(new CopilotClientOptions
{
    Mode = CopilotClientMode.CodingAgent,
    Runtime = RuntimeConnection.ForSpawnedStdio(args: new[] { "--log-dir", "/path/to/logs" })
});
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import java.util.List;

var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
    .setRuntime(RuntimeConnection.forSpawnedStdio().setArgs(List.of("--log-dir", "/path/to/logs"))));
```

</details>

## Common issues

### "Copilot CLI not found" / runtime executable not found

**Cause:** A spawned connection (`forSpawnedStdio` or `forSpawnedTcp`) could not find the Copilot runtime executable (`copilot-runtime`), or the path you configured for it is invalid. The SDK does not need a separate Copilot CLI installation. The default in-process connection loads the native runtime library instead of starting an executable; if that library is missing or doesn't match your platform, startup fails without falling back to a subprocess. See [Runtime library resolution](../setup/in-process-runtime.md#runtime-library-resolution).

**Solution:**

1. Make sure the runtime is available. The Node.js SDK installs it with an optional `@microsoft/copilot-sdk-<platform>` package, the .NET SDK downloads it for your build's runtime identifier, and the Rust SDK bundles it by default, so reinstall or rebuild if it is missing. For Python, run `python -m copilot download-runtime`; for Go, run the [bundler tool](../../go/README.md#distributing-your-application-with-an-embedded-github-copilot-cli); for Java, add `com.microsoft:copilot-sdk-runtime` with the classifier for your platform. If you supply the executable yourself, use the `github-copilot-<version>-<platform>.tgz` asset from the [Copilot CLI releases](https://github.com/github/copilot-cli/releases) for the runtime version that matches your SDK, and keep the package layout intact: `copilot-runtime` loads `runtime.node` from its own directory and finds its bundled assets in the package root. The examples below assume the archive's `package` directory was extracted to `/opt/copilot`.

1. Specify the full path to the runtime executable:

    <details open>
    <summary><strong>Node.js</strong></summary>

    ```typescript
    import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

    const client = new CopilotClient({
        mode: "coding-agent",
        runtime: RuntimeConnection.forSpawnedStdio({ path: "/opt/copilot/prebuilds/linux-x64/copilot-runtime" }),
    });
    ```

    </details>

    <details>
    <summary><strong>Python</strong></summary>

    ```python
    from copilot import CopilotClient, RuntimeConnection

    client = CopilotClient(
        mode="coding-agent",
        runtime=RuntimeConnection.for_spawned_stdio(path="/opt/copilot/prebuilds/linux-x64/copilot-runtime")
    )
    ```

    </details>

    <details>
    <summary><strong>Go</strong></summary>

    <!-- docs-validate: hidden -->

    ```go
    package main

    import copilot "github.com/microsoft/copilot-sdk/go/v2"

    func main() {
        client := copilot.NewClient(&copilot.ClientOptions{
            Mode: copilot.ModeCodingAgent,
            Runtime: copilot.SpawnedStdioConnection{Path: "/opt/copilot/prebuilds/linux-x64/copilot-runtime"},
        })
        _ = client
    }
    ```

    <!-- /docs-validate: hidden -->

    ```go
    client := copilot.NewClient(&copilot.ClientOptions{
        Mode: copilot.ModeCodingAgent,
        Runtime: copilot.SpawnedStdioConnection{Path: "/opt/copilot/prebuilds/linux-x64/copilot-runtime"},
    })

    ```

    </details>

    <details>
    <summary><strong>.NET</strong></summary>

    ```csharp
    var client = new CopilotClient(new CopilotClientOptions
    {
        Mode = CopilotClientMode.CodingAgent,
        Runtime = RuntimeConnection.ForSpawnedStdio(path: "/opt/copilot/prebuilds/linux-x64/copilot-runtime"),
    });
    ```

    </details>

    <details>
    <summary><strong>Java</strong></summary>

    ```java
    var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
        .setRuntime(RuntimeConnection.forSpawnedStdio("/opt/copilot/prebuilds/linux-x64/copilot-runtime"))
    );
    ```

    </details>

### "Not authenticated"

**Cause:** The runtime has no GitHub credentials to use.

**Solution:**

1. Sign in once with the [Copilot CLI](https://docs.github.com/en/copilot/how-tos/set-up/install-copilot-cli); the runtime can use the stored credentials:

    ```bash
    copilot login
    ```

1. Or provide a token programmatically:

    <details open>
    <summary><strong>Node.js</strong></summary>

    ```typescript
    import { CopilotClient } from "@microsoft/copilot-sdk";

    const client = new CopilotClient({
        mode: "coding-agent",
        github: {
            token: process.env.GITHUB_TOKEN,
        },
    });
    ```

    </details>

    <details>
    <summary><strong>Python</strong></summary>

    ```python
    import os
    from copilot import CopilotClient

    client = CopilotClient(mode="coding-agent", github={"token": os.environ.get("GITHUB_TOKEN")})
    ```

    </details>

    <details>
    <summary><strong>Go</strong></summary>

    <!-- docs-validate: hidden -->

    ```go
    package main

    import (
        copilot "github.com/microsoft/copilot-sdk/go/v2"
        "os"
    )

    func main() {
        client := copilot.NewClient(&copilot.ClientOptions{
            Mode: copilot.ModeCodingAgent,
            GitHub: copilot.ClientGitHubOptions{
                Token: os.Getenv("GITHUB_TOKEN"),
            },
        })
        _ = client
    }
    ```

    <!-- /docs-validate: hidden -->

    ```go
    client := copilot.NewClient(&copilot.ClientOptions{
        Mode: copilot.ModeCodingAgent,
        GitHub: copilot.ClientGitHubOptions{
            Token: os.Getenv("GITHUB_TOKEN"),
        },
    })

    ```

    </details>

    <details>
    <summary><strong>.NET</strong></summary>

    ```csharp
    var client = new CopilotClient(new CopilotClientOptions
    {
        Mode = CopilotClientMode.CodingAgent,
        GitHub = new ClientGitHubOptions
        {
            Token = Environment.GetEnvironmentVariable("GITHUB_TOKEN")
        }
    });
    ```

    </details>

    <details>
    <summary><strong>Java</strong></summary>

    ```java
    var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
            .setGitHub(new ClientGitHubOptions()
                    .setToken(System
                            .getenv("GITHUB_TOKEN"))));
    ```

    </details>

### "Session not found"

**Cause:** Attempting to use a session that was destroyed or doesn't exist.

**Solution:**

1. Ensure you're not calling methods after `disconnect()`:

    ```typescript
    await session.disconnect();
    // Don't use session after this!
    ```

1. For resuming sessions, verify the session ID exists:
    ```typescript
    const sessions = await client.listSessions();
    console.log("Available sessions:", sessions);
    ```

### "Connection refused" / "ECONNREFUSED"

**Cause:** The runtime process crashed or failed to start, or no runtime server is listening at the address you connected to.

**Solution:**

1. Check if the runtime starts correctly on its own. It prints `CLI server listening on port <port>` when it is ready; press Ctrl+C to stop it:

    ```bash
    copilot-runtime --server
    ```

1. Check for port conflicts if using TCP mode:

    ```typescript
    import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

    const client = new CopilotClient({
        mode: "coding-agent",
        runtime: RuntimeConnection.forSpawnedTcp({ port: 0 }), // Use random available port
    });
    ```

## MCP server debugging

MCP (Model Context Protocol) servers can be tricky to debug. For comprehensive MCP debugging guidance, see the dedicated **[MCP Debugging Guide](./mcp-debugging.md)**.

### Quick MCP checklist

- [ ] MCP server executable exists and runs independently
- [ ] Command path is correct (use absolute paths)
- [ ] Tools are enabled: `tools: ["*"]`
- [ ] Server responds to `initialize` request correctly
- [ ] Working directory (`workingDirectory`) is set if needed

### Test your MCP server

Before integrating with the SDK, verify your MCP server works:

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | /path/to/your/mcp-server
```

See [MCP Debugging Guide](./mcp-debugging.md) for detailed troubleshooting.

## Connection issues

### Transport modes

Choose the connection type appropriate to your deployment:

| Mode                     | Description                                                        | Use Case                                                 |
| ------------------------ | ------------------------------------------------------------------ | -------------------------------------------------------- |
| **In-process** (default) | Native runtime in the application process, JSON-RPC over the C ABI | Applications with matching native runtime assets         |
| **Stdio**                | Runtime runs as a subprocess, communicates via pipes               | Subprocess isolation or per-client process configuration |
| **TCP**                  | Runtime listens on a TCP socket, spawned by the SDK or separately  | Multiple clients, shared or remote runtime               |

**Stdio mode:**

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forSpawnedStdio(),
});
```

**TCP mode:**

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forSpawnedTcp({ port: 8080 }), // Or 0 for random port
});
```

**Connect to a running runtime server:**

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forTcp("localhost", 8080), // Connect to a running runtime server
});
```

The default in-process transport runs the runtime inside your application process, so there is no child process or socket to check; see [In-process runtime](../setup/in-process-runtime.md).

### Diagnosing connection failures

1. **Check the connection:**

    ```typescript
    const response = await client.ping();
    console.log("Server responded at", response.timestamp);
    // Starts the client if needed; surfaces startup and connection errors
    ```

1. **For spawned connections, verify the runtime process is running:**
    ```bash
    # Check for copilot-runtime processes
    ps aux | grep copilot-runtime
    ```

## Tool execution issues

### Custom tool not being called

1. **Verify tool registration:**
    <!-- docs-validate: hidden -->

    ```typescript
    import type { Tool } from "@microsoft/copilot-sdk";

    declare const myTool: Tool<{ location: string }>;

    const session = await client.createSession({
        tools: {
            definitions: [myTool],
        },
    });
    ```

    <!-- /docs-validate: hidden -->

    ```typescript
    const session = await client.createSession({
        tools: {
            definitions: [myTool],
        },
    });
    ```

1. **Check tool schema is valid JSON Schema:**

    ```typescript
    const myTool = {
        name: "get_weather",
        description: "Get weather for a location",
        parameters: {
            type: "object",
            properties: {
                location: { type: "string", description: "City name" },
            },
            required: ["location"],
        },
        handler: async (args: { location: string }) => {
            return { temperature: 72 };
        },
    };
    ```

1. **Ensure handler returns valid result:**

    ```typescript
    handler: async (args: { location: string }) => {
        // Must return something JSON-serializable
        return { success: true, data: "result" };

        // Don't return undefined or non-serializable objects
    };
    ```

### Tool errors not surfacing

A failed tool call arrives as `tool.execution_complete` with `success: false`, and session errors arrive as `session.error`:

```typescript
session.on("tool.execution_complete", (event) => {
    if (!event.data.success) {
        console.error("Tool error:", event.data.error);
    }
});

session.on("session.error", (event) => {
    console.error("Session error:", event.data);
});
```

## Platform-specific issues

### Windows

1. **Path separators:** Use raw strings or forward slashes:

    ```csharp
    var options = new CopilotClientOptions

    {
        Mode = CopilotClientMode.CodingAgent,
        // Verbatim string
        Runtime = RuntimeConnection.ForSpawnedStdio(path: @"C:\Program Files\Copilot\prebuilds\win32-x64\copilot-runtime.exe"),
    };
    // or forward slashes
    options.Runtime = RuntimeConnection.ForSpawnedStdio(path: "C:/Program Files/Copilot/prebuilds/win32-x64/copilot-runtime.exe");
    ```

1. **PATHEXT resolution:** The SDK handles this automatically, but if issues persist:

    ```csharp
    var server = new McpStdioServerConfig
    {
        // Explicitly specify .exe
        Command = "myserver.exe",  // Not just "myserver"
    };
    ```

1. **Console encoding:** Ensure UTF-8 for proper JSON handling:
    ```csharp
    Console.OutputEncoding = System.Text.Encoding.UTF8;
    ```

### macOS

1. **Gatekeeper issues:** If the runtime executable is blocked:

    ```bash
    xattr -d com.apple.quarantine /path/to/copilot-runtime
    ```

1. **PATH issues in GUI apps:** GUI applications may not inherit shell PATH:

    ```typescript
    import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

    const client = new CopilotClient({
        mode: "coding-agent",
        runtime: RuntimeConnection.forSpawnedStdio({ path: "/opt/copilot/prebuilds/darwin-arm64/copilot-runtime" }), // Full path
    });
    ```

### Linux

1. **Permission issues:**

    ```bash
    chmod +x /path/to/copilot-runtime
    ```

1. **Missing libraries:** Check for required shared libraries:
    ```bash
    ldd /path/to/copilot-runtime
    ```

## Getting help

If you're still stuck:

1. **Collect debug information:**
    - SDK version
    - Runtime version (`version` from `client.getStatus()`)
    - Operating system
    - Debug logs
    - Minimal reproduction code

1. **Search existing issues:** [GitHub Issues](https://github.com/github/copilot-sdk/issues)

1. **Open a new issue** with the collected information

## See also

- [Getting Started Guide](../getting-started.md)
- [MCP Overview](../features/mcp.md) - MCP configuration and setup
- [MCP Debugging Guide](./mcp-debugging.md) - Detailed MCP troubleshooting
- [API Reference](https://github.com/github/copilot-sdk)

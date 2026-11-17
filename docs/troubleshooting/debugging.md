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
available to `collectLogs`. An explicitly selected older CLI executable may
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

### "CLI not found" / "Copilot: command not found"

**Cause:** The Copilot CLI is missing or its configured path is invalid.

**Solution:**

1. Install the CLI: [Installation guide](https://docs.github.com/en/copilot/how-tos/set-up/install-copilot-cli)

1. Verify installation:

    ```bash
    copilot --version
    ```

1. Specify the full path:

    <details open>
    <summary><strong>Node.js</strong></summary>

    ```typescript
    import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

    const client = new CopilotClient({
        mode: "coding-agent",
        runtime: RuntimeConnection.forSpawnedStdio({ path: "/usr/local/bin/copilot" }),
    });
    ```

    </details>

    <details>
    <summary><strong>Python</strong></summary>

    ```python
    from copilot import CopilotClient, RuntimeConnection

    client = CopilotClient(
        mode="coding-agent",
        runtime=RuntimeConnection.for_spawned_stdio(path="/usr/local/bin/copilot")
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
            Runtime: copilot.SpawnedStdioConnection{Path: "/usr/local/bin/copilot"},
        })
        _ = client
    }
    ```

    <!-- /docs-validate: hidden -->

    ```go
    client := copilot.NewClient(&copilot.ClientOptions{
        Mode: copilot.ModeCodingAgent,
        Runtime: copilot.SpawnedStdioConnection{Path: "/usr/local/bin/copilot"},
    })

    ```

    </details>

    <details>
    <summary><strong>.NET</strong></summary>

    ```csharp
    var client = new CopilotClient(new CopilotClientOptions
    {
        Mode = CopilotClientMode.CodingAgent,
        Runtime = RuntimeConnection.ForSpawnedStdio(path: "/usr/local/bin/copilot"),
    });
    ```

    </details>

    <details>
    <summary><strong>Java</strong></summary>

    ```java
    var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
        .setRuntime(RuntimeConnection.forSpawnedStdio("/usr/local/bin/copilot"))
    );
    ```

    </details>

### "Not authenticated"

**Cause:** The CLI is not authenticated with GitHub.

**Solution:**

1. Authenticate the CLI:

    ```bash
    copilot auth login
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

**Cause:** The CLI server process crashed or failed to start.

**Solution:**

1. Check if the CLI runs correctly standalone:

    ```bash
    copilot --server --stdio
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
| **Stdio**                | CLI runs as subprocess, communicates via pipes                     | Subprocess isolation or per-client process configuration |
| **TCP**                  | CLI runs separately, communicates via TCP socket                   | Multiple clients, remote CLI                             |

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

**Connect to existing server:**

```typescript
import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forTcp("localhost", 8080), // Connect to running server
});
```

The experimental in-process transport loads the runtime into your process instead, so there is no child process or socket to check; see [In-process runtime](../setup/in-process-runtime.md).

### Diagnosing connection failures

1. **Check the connection:**

    ```typescript
    const response = await client.ping();
    console.log("Server responded at", response.timestamp);
    // Starts the client if needed; surfaces startup and connection errors
    ```

1. **Verify CLI process is running:**
    ```bash
    # Check for copilot processes
    ps aux | grep copilot
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
        Runtime = RuntimeConnection.ForSpawnedStdio(path: @"C:\Program Files\GitHub\copilot.exe"),
    };
    // or forward slashes
    options.Runtime = RuntimeConnection.ForSpawnedStdio(path: "C:/Program Files/GitHub/copilot.exe");
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

1. **Gatekeeper issues:** If CLI is blocked:

    ```bash
    xattr -d com.apple.quarantine /path/to/copilot
    ```

1. **PATH issues in GUI apps:** GUI applications may not inherit shell PATH:

    ```typescript
    import { CopilotClient, RuntimeConnection } from "@microsoft/copilot-sdk";

    const client = new CopilotClient({
        mode: "coding-agent",
        runtime: RuntimeConnection.forSpawnedStdio({ path: "/opt/homebrew/bin/copilot" }), // Full path
    });
    ```

### Linux

1. **Permission issues:**

    ```bash
    chmod +x /path/to/copilot
    ```

1. **Missing libraries:** Check for required shared libraries:
    ```bash
    ldd /path/to/copilot
    ```

## Getting help

If you're still stuck:

1. **Collect debug information:**
    - SDK version
    - CLI version (`copilot --version`)
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

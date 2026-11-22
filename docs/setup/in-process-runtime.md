# Run the Copilot runtime in process

SDK v2 hosts the native Copilot runtime in your application process by default instead of starting a separate runtime process. Sessions, events, tools, hooks, and JSON-RPC framing remain the same.

> [!WARNING]
> In-process hosting is not a sandbox and provides no crash isolation from the runtime. A native crash can terminate your application. Test startup, model turns, and shutdown on every operating system and architecture that you deploy.

## When to use in-process hosting

In-process hosting is a good fit when:

- Your application must run without a separate runtime process.
- You want the SDK to own the runtime lifecycle.
- You can ship a native library for each deployment platform.
- Process-wide environment and working-directory settings are acceptable.

Select an explicit stdio or TCP subprocess connection when crash isolation or per-client process configuration is required. Use a [backend service](./backend-services.md) when multiple application instances must connect to a shared runtime over TCP.

## How it works

The SDK loads the Copilot runtime native library and binds its fixed C ABI. All SDK methods continue to use the existing `Content-Length`-framed JSON-RPC protocol over an in-memory connection.

```mermaid
flowchart LR
    App["Your application"]
    SDK["Copilot SDK client"]
    FFI["C ABI connection"]
    Runtime["Native Copilot runtime"]

    App --> SDK
    SDK -- "JSON-RPC frames" --> FFI
    FFI --> Runtime
    Runtime -- "Events and callbacks" --> FFI
    FFI --> SDK
```

The runtime:

- Runs native Rust in the application process, not in an embedded Node.js interpreter. The SDK connection needs no child process, TCP port, or connection token; tools can still launch subprocesses.
- Supports the same sessions, streaming events, tools, hooks, permissions, and server-to-client requests as other transports.
- Can invoke SDK callbacks from native worker threads. The SDK handles thread marshalling and callback lifetime.
- Keeps the loaded native library and its worker pool available for the lifetime of the application process.

## SDK requirements

All SDKs expose an explicit in-process connection option. Some languages require additional build or package configuration.

| SDK        | Connection option                                       | Additional requirement                                                                                                  |
| ---------- | ------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| TypeScript | `RuntimeConnection.forInProcess()`                      | None when the package includes a compatible runtime bundle                                                              |
| Python     | `RuntimeConnection.for_inprocess()`                     | Pre-download with `python -m copilot download-runtime --in-process` when runtime download is unavailable during startup |
| Go         | `copilot.InProcessConnection{}`                         | Run the application bundler or supply a compatible runtime package; no build tag is needed                              |
| .NET       | `RuntimeConnection.ForInProcess()`                      | Include the runtime assets acquired for the deployment RID                                                              |
| Rust       | `RuntimeConnection::for_in_process(Default::default())` | Included by the default `bundled-in-process` Cargo feature                                                              |
| Java       | `RuntimeConnection.forInProcess()`                      | Add the matching platform runtime classifier; JNA is a transitive SDK dependency                                        |

The native runtime bundle must match the host operating system, CPU architecture, and, on Linux, C library. Unsupported hosts fail during runtime resolution or startup instead of falling back to a child process.

## Configure an in-process connection

Ordinary clients use in-process hosting without a connection option. Set it explicitly to ignore the environment override:

<details open>
<summary><strong>TypeScript</strong></summary>

<!-- docs-validate: strict -->

```typescript
import { CopilotClient, RuntimeConnection } from "copilot-sdk-supercharged";

const client = new CopilotClient({
    mode: "coding-agent",
    runtime: RuntimeConnection.forInProcess(),
});

await client.start();
```

</details>
<details>
<summary><strong>Python</strong></summary>

```python
from copilot import CopilotClient, RuntimeConnection

client = CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_inprocess())

await client.start()
```

</details>
<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: hidden -->

```go
package main

import (
	"context"
	"log"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
)

func main() {
	client := copilot.NewClient(&copilot.ClientOptions{
		Mode: copilot.ModeCodingAgent,
		Runtime: copilot.InProcessConnection{},
	})

	if err := client.Start(context.Background()); err != nil {
		log.Fatal(err)
	}
	defer client.Stop()
}
```

<!-- /docs-validate: hidden -->

```go
client := copilot.NewClient(&copilot.ClientOptions{
	Mode: copilot.ModeCodingAgent,
	Runtime: copilot.InProcessConnection{},
})

if err := client.Start(context.Background()); err != nil {
	log.Fatal(err)
}
defer client.Stop()

```

</details>
<details>
<summary><strong>.NET</strong></summary>

```csharp
var client = new CopilotClient(new CopilotClientOptions
{
    Mode = CopilotClientMode.CodingAgent,
    Runtime = RuntimeConnection.ForInProcess()
});
await client.StartAsync();
```

</details>
<details>
<summary><strong>Rust</strong></summary>

```rust
let options = ClientOptions::new(copilot_sdk::ClientMode::CodingAgent).with_runtime(RuntimeConnection::for_in_process(
    InProcessRuntimeOptions::default(),
));

let client = Client::start(options).await?;
```

</details>
<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;

public class Example {
    public void run() throws Exception {
        CopilotClientOptions options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection
                        .forInProcess());

        CopilotClient client = new CopilotClient(options);
        client
                .start()
                .join();
    }
}
```

</details>

`COPILOT_SDK_DEFAULT_CONNECTION` accepts `inprocess` or `stdio`, case-insensitively. Unset or empty selects in-process. An explicit connection wins, even over an invalid override; otherwise invalid values fail. Java configures subprocesses through `RuntimeConnection.forSpawnedStdio()` or `RuntimeConnection.forSpawnedTcp()`.

Prefer explicit client configuration in application code. Use the environment variable when deployment configuration must select the transport without changing the application.

## Migrate to the v2 default

Clients that omit a connection now load native code into the application process. To retain an owned subprocess, select it explicitly:

| SDK        | Subprocess selection                                                                                                                    |
| ---------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| TypeScript | `new CopilotClient({ mode: "coding-agent", runtime: RuntimeConnection.forSpawnedStdio() })`                                             |
| Python     | `CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio())`                                                     |
| Go         | `copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent, Runtime: copilot.SpawnedStdioConnection{}})`                   |
| .NET       | `new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent, Runtime = RuntimeConnection.ForSpawnedStdio() })`   |
| Rust       | `Client::start(ClientOptions::new(ClientMode::CodingAgent).with_runtime(RuntimeConnection::SpawnedStdio(Default::default()))).await?`   |
| Java       | `new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(RuntimeConnection.forSpawnedStdio()))` |

Alternatively, set `COPILOT_SDK_DEFAULT_CONNECTION=stdio` before starting the application. Existing explicit TCP and external-server connections retain their behavior. A missing native library or unsupported option never triggers an automatic subprocess fallback.

Environment maps and process working directories belong only to spawned connections. Clients using those options must select a subprocess or move process-wide configuration to the host before startup. Supported typed settings such as authentication, telemetry, and base directory still work. Session working directories are separate and remain supported.

Rust applications disabling default features must enable `in-process` (or `local-runtime` with an application-supplied runtime) to use the new default. Subprocess-only builds can retain `bundled-cli` or `runtime` and select `RuntimeConnection::for_spawned_stdio(...)` explicitly.

Java GraalVM native-image cannot use JNA callback upcalls. Select `RuntimeConnection.forSpawnedStdio()` or `RuntimeConnection.forTcp(host, port)` explicitly for those deployments.

## Configure the runtime

The SDK converts supported typed client options into native runtime arguments and host-scoped environment values. Depending on the SDK, these options include:

- Authentication token and logged-in-user fallback.
- Copilot base directory.
- Log level.
- Session idle timeout.
- Remote session mode.

The in-process runtime receives a host environment snapshot plus supported typed overrides. It does not mutate the host environment.

Configure working directories per session. Set genuinely process-wide values before creating the first in-process client.

## Runtime library resolution

Each SDK first looks for a compatible bundled or cached runtime library. You can set `COPILOT_CLI_PATH` to point into a compatible Copilot runtime package when you need to provide the runtime separately.

Only one native runtime library path and version can normally be loaded in a process. Starting another client with the same loaded library is supported, but attempting to load a different runtime library fails.

For production deployments:

1. Build and test the application for each target platform.
1. Ensure that the matching native runtime artifact is included in the deployed package or available through the SDK's runtime download mechanism.
1. Start at least one session and complete a model turn in a deployment smoke test.
1. Stop clients gracefully before the application exits.

## Lifecycle behavior

Starting an in-process client loads the native library, creates a runtime host, opens an in-memory connection, and performs the normal SDK protocol-version handshake.

During graceful shutdown, the SDK:

1. Closes active sessions.
1. Requests normal runtime shutdown over JSON-RPC.
1. Closes the JSON-RPC and native connections.
1. Releases the runtime host.

The native library can remain loaded until the application process exits. Do not depend on unloading and replacing the runtime library after first use.

## Limitations

In-process hosting has these current constraints:

- **Shared process state**: all clients share the host process environment, current working directory, native library, and runtime worker pool.
- **Restricted process options**: arbitrary environment maps, client working directories, executable paths, and extra runtime arguments are rejected where applicable.
- **No per-client working directory**: set working directories per session instead.
- **One runtime version per process**: loading another native library path or version is not supported.
- **macOS library validation**: hardened hosts must permit loading the runtime's signed native library. Verifying its signature alone does not establish host/library Team ID compatibility.
- **Java native-image**: JNA callback upcalls are unavailable; use an explicit subprocess or external-server connection.
- **Platform maturity varies**: some SDK and platform combinations have reduced model-turn or shutdown coverage. Validate the exact combination that you deploy.

## Further reading

- [Choosing a setup path](./choosing-a-setup-path.md): compare in-process hosting with other deployment models
- [Default setup](./bundled-cli.md): acquire and run the bundled native runtime
- [Backend services](./backend-services.md): connect applications to a shared runtime over TCP
- [Session lifecycle](../hooks/session-lifecycle.md): handle session start and end events

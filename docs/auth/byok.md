# BYOK (bring your own key)

BYOK uses named `inference.providers` connections and per-model `inference.models` rows. GitHub identity remains independent: disable Copilot inference with `github.capi.enabled: false`, without removing identity needed for GitHub services.

## Supported providers

| Provider                         | Type Value              | Notes                                                                      |
| -------------------------------- | ----------------------- | -------------------------------------------------------------------------- |
| OpenAI                           | `"openai"`              | OpenAI API and OpenAI-compatible endpoints                                 |
| Microsoft Foundry / Azure OpenAI | `"openai"` or `"azure"` | Use `"openai"` for `/openai/v1/`; use `"azure"` for native Azure endpoints |
| Anthropic                        | `"anthropic"`           | Claude models                                                              |
| Google                           | `"google"`              | Gemini models via Google's native Gemini Interactions API (experimental)   |
| Ollama                           | `"openai"`              | Local models via OpenAI-compatible API                                     |
| Microsoft Foundry Local          | `"openai"`              | Run AI models locally on your device via OpenAI-compatible API             |
| Other OpenAI-compatible          | `"openai"`              | vLLM, LiteLLM, etc.                                                        |

## Model identity and editing tools

Select models directly with session `model`, usually `provider/id`. Optional `inference.defaultProvider` routes unqualified selections through a named connection and requires CAPI disabled. Omission creates no default or preserves it on resume; explicit `null` clears it. It never selects the initial model. Unregistered pass-through aliases use inferred metadata plus session processing, without provider-wide model defaults.

Selection, behavior, and inference identities are separate: `models[].id` identifies the entry within the provider, `models[].modelId` selects model behavior (defaulting to `id`), and `models[].wireModel` controls the inference request's model (also defaulting to `id`). Choosing an editing policy does not change the provider endpoint or wire model, and does not bypass permission checks or explicit tool filters.

## Quick start: Microsoft Foundry

Microsoft Foundry is a common BYOK deployment target for enterprises. Here's a complete example:

<details open>
<summary><strong>Python</strong></summary>

```python
import asyncio
import os
from copilot import CopilotClient
from copilot.session import PermissionHandler

FOUNDRY_MODEL_URL = "https://<resource-name>.openai.azure.com/openai/v1/"
# Set FOUNDRY_API_KEY environment variable


async def main():
    client = CopilotClient(mode="coding-agent")
    await client.start()

    session = await client.create_session(
        model="gpt-5.2-codex",
        permissions={"on_request": PermissionHandler.approve_all},
        github={"capi": {"enabled": False}},
        inference={
            "default_provider": "byok",
            "providers": [
                {
                    "name": "byok",
                    "type": "openai",
                    "base_url": FOUNDRY_MODEL_URL,
                    "wire_api": "responses",  # Use "completions" for older models
                    "api_key": os.environ["FOUNDRY_API_KEY"],
                }
            ],
        },
    )

    done = asyncio.Event()

    def on_event(event):
        if event.type.value == "assistant.message":
            print(event.data.content)
        elif event.type.value == "session.idle":
            done.set()

    session.on(on_event)
    await session.send("What is 2+2?")
    await done.wait()

    await session.disconnect()
    await client.stop()


asyncio.run(main())
```

</details>

<details>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";

const FOUNDRY_MODEL_URL = "https://<resource-name>.openai.azure.com/openai/v1/";

const client = new CopilotClient({ mode: "coding-agent" });
const session = await client.createSession({
    model: "gpt-5.2-codex",
    github: { capi: { enabled: false } },
    inference: {
        // Your deployment name
        defaultProvider: "byok",
        providers: [
            {
                name: "byok",
                type: "openai",
                baseUrl: FOUNDRY_MODEL_URL,
                wireApi: "responses", // Use "completions" for older models
                apiKey: process.env.FOUNDRY_API_KEY,
            },
        ],
    },
});

session.on("assistant.message", (event) => {
    console.log(event.data.content);
});

await session.sendAndWait({ prompt: "What is 2+2?" });
await client.stop();
```

</details>

<details>
<summary><strong>Go</strong></summary>

```go
package main

import (
	"context"
	"fmt"
	copilot "github.com/microsoft/copilot-sdk/go/v2"
	"os"
)

func main() {
	ctx := context.Background()
	client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent})
	if err := client.Start(ctx); err != nil {
		panic(err)
	}
	defer client.Stop()

	defaultProvider := copilot.String("byok")
	session, err := client.CreateSession(ctx, &copilot.CreateSessionOptions{GitHub: copilot.SessionGitHubOptions{Capi: &copilot.CapiSessionOptions{Enabled: copilot.Bool(false)}},
		Model: "gpt-5.2-codex",
		Inference: // Your deployment name
		copilot.SessionInferenceOptions{
			DefaultProvider: &defaultProvider, Providers: []copilot.NamedProviderConfig{{Name: "byok",
				Type:    "openai",
				BaseURL: "https://<resource-name>.openai.azure.com/openai/v1/",
				WireAPI: "responses", // Use "completions" for older models
				APIKey:  os.Getenv("FOUNDRY_API_KEY"),
			}},
		},
	})
	if err != nil {
		panic(err)
	}

	response, err := session.SendAndWait(ctx, copilot.MessageOptions{

		Prompt: "What is 2+2?",
	})
	if err != nil {
		panic(err)
	}

	if d, ok := response.Data.(*copilot.AssistantMessageData); ok {
		fmt.Println(d.Content)
	}
}
```

</details>

<details>
<summary><strong>.NET</strong></summary>

```csharp
using Microsoft.Copilot;
#pragma warning disable MCPSDK001 // Named providers are experimental.
await using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CodingAgent });
await using var session = await client.CreateSessionAsync(new CreateSessionOptions
{
    Model = "gpt-5.2-codex",
    GitHub = new SessionGitHubOptions { Capi = new CapiSessionOptions { Enabled = false } },
    Inference = new SessionInferenceOptions
    {
        DefaultProvider = "byok", Providers = [new NamedProviderConfig { Name = "byok",
            Type = "openai",
            BaseUrl = "https://<resource-name>.openai.azure.com/openai/v1/",
            WireApi = "responses", // Use "completions" for older models
            ApiKey = Environment.GetEnvironmentVariable("FOUNDRY_API_KEY"),
        }]
    }
});
var response = await session.SendAndWaitAsync(new MessageOptions
{
    Prompt = "What is 2+2?",
});
Console.WriteLine(response?.Data.Content);
```

</details>

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
                .setModel("gpt-5.2-codex")
                .setPermissions(new SessionPermissionOptions()
                        .setOnRequest(PermissionHandler.APPROVE_ALL))
                .setInference(new SessionInferenceOptions()
                        .setDefaultProvider("byok").setProviders(java.util.List.of(new NamedProviderConfig().setName("byok")
                                .setType("openai")
                                .setBaseUrl("https://<resource-name>.openai.azure.com/openai/v1/")
                                .setWireApi("responses") // Use "completions" for older models
                                .setApiKey(System
                                        .getenv("FOUNDRY_API_KEY"))))).setGitHub(new SessionGitHubOptions().setCapi(new CapiSessionOptions().setEnabled(false))))
        .get();

var response = session
        .sendAndWait(new MessageOptions()
                .setPrompt("What is 2+2?"))
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

## Provider configuration reference

### NamedProviderConfig fields

| Field                                           | Type                                                   | Description                                                                                                                                                                                                                                                                                                                                   |
| ----------------------------------------------- | ------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `name`                                          | string                                                 | **Required.** Connection name, without `/`, referenced by model rows and bearer callbacks.                                                                                                                                                                                                                                                    |
| `type`                                          | `"openai"` \| `"azure"` \| `"anthropic"` \| `"google"` | Provider type (default: `"openai"`). `"google"` (experimental) uses Google's native Gemini Interactions API.                                                                                                                                                                                                                                  |
| `baseUrl` / `base_url`                          | string                                                 | **Required.** API endpoint URL                                                                                                                                                                                                                                                                                                                |
| `apiKey` / `api_key`                            | string                                                 | API key (optional for local providers like Ollama)                                                                                                                                                                                                                                                                                            |
| `apiKeyCommand` / `api_key_command`             | string                                                 | Command run by the runtime before each provider request to obtain a fresh API key. Also exposed as `ApiKeyCommand` in .NET and `APIKeyCommand` in Go. Credential-command failures fail the turn before sending a model request.                                                                                                               |
| `bearerToken` / `bearer_token`                  | string                                                 | Bearer token auth (takes precedence over apiKey)                                                                                                                                                                                                                                                                                              |
| `bearerTokenProvider` / `bearer_token_provider` | callback                                               | Returns a bearer token on demand (takes precedence over `apiKey`, `apiKeyCommand`, and `bearerToken`; the command does not run)                                                                                                                                                                                                               |
| `wireApi` / `wire_api`                          | `"completions"` \| `"responses"`                       | Select `"completions"` for broad model compatibility (the Chat Completions API); select `"responses"` for multi-turn state management, tool namespacing, and reasoning support (the Responses API). Anthropic models always use the Messages API and `"google"` providers always use the Gemini Interactions API, regardless of this setting. |
| `transport`                                     | `"http"` \| `"websockets"`                             | WebSockets require OpenAI Responses.                                                                                                                                                                                                                                                                                                          |
| `azure.apiVersion` / `azure.api_version`        | string                                                 | Azure API version. When set, the runtime uses the versioned deployment route; when omitted, it uses the GA versionless `v1` route.                                                                                                                                                                                                            |

### Wire API format

The `wireApi` setting determines which OpenAI API format to use:

- **`"completions"`** (default) - Chat Completions API (`/chat/completions`) for broad model compatibility.
- **`"responses"`** - Responses API for multi-turn state management, tool namespacing, and reasoning support.

Anthropic models always use the Anthropic Messages API regardless of this setting.

### Type-specific notes

**OpenAI (`type: "openai"`)**

- Works with OpenAI API and any OpenAI-compatible endpoint
- `baseUrl` should include the full path (e.g., `https://api.openai.com/v1`)

**Azure (`type: "azure"`)**

- Use for native Azure OpenAI endpoints
- `baseUrl` / `base_url` accepts a resource host or a full project URL, such as `https://<host>/api/projects/hosted-agents-ncus`, with or without a trailing slash.
- The runtime preserves the project prefix when constructing Azure API paths. For `wireApi: "responses"` with `azure.apiVersion` omitted, the project URL above produces `https://<host>/api/projects/hosted-agents-ncus/openai/v1/responses`. Project URLs require an updated Copilot runtime.

**Anthropic (`type: "anthropic"`)**

- For direct Anthropic API access
- Uses Claude-specific API format

**Google (`type: "google"`, experimental)**

- Experimental: behavior and configuration may change in future releases
- For direct access to Google's Gemini API
- Always uses Google's native Gemini Interactions API (`{baseUrl}/interactions`); `wireApi` is ignored
- `baseUrl` should include the API version (e.g., `https://generativelanguage.googleapis.com/v1beta`)
- `apiKey` is sent in the `x-goog-api-key` header; `bearerToken` is sent as an `Authorization` bearer token and takes precedence, so the API key isn't sent when both are set
- Requests set `store: false`, so Google keeps no conversation state; each request carries the full history

## Example configurations

### OpenAI direct

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    { name: "byok", type: "openai", baseUrl: "https://api.openai.com/v1", apiKey: process.env.OPENAI_API_KEY },
];
```

### Azure OpenAI (native Azure endpoint)

Use `type: "azure"` for endpoints at `*.openai.azure.com`:

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    {
        name: "byok",
        type: "azure",
        baseUrl: "https://my-resource.openai.azure.com", // Resource host or full project URL
        apiKey: process.env.AZURE_OPENAI_KEY,
        azure: {
            apiVersion: "2024-10-21",
        },
    },
];
```

### Microsoft Foundry (OpenAI-compatible endpoint)

For Microsoft Foundry deployments with `/openai/v1/` endpoints, use `type: "openai"`:

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    {
        name: "byok",
        type: "openai",
        baseUrl: "https://<resource-name>.openai.azure.com/openai/v1/",
        apiKey: process.env.FOUNDRY_API_KEY,
        wireApi: "responses", // For GPT-5 series models
    },
];
```

### Ollama (local)

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    {
        name: "byok",
        type: "openai",
        baseUrl: "http://localhost:11434/v1",
        // No apiKey needed for local Ollama
    },
];
```

### Microsoft Foundry Local

[Microsoft Foundry Local](https://foundrylocal.ai) lets you run AI models locally on your own device with an OpenAI-compatible API. Install it via the Foundry Local CLI, then point the SDK at your local endpoint:

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    {
        name: "byok",
        type: "openai",
        baseUrl: "http://localhost:<PORT>/v1",
        // No apiKey needed for local Foundry Local
    },
];
```

> [!NOTE]
> Foundry Local starts on a **dynamic port**—the port is not fixed. Use `foundry service status` to confirm the port the service is currently listening on, then use that port in your `baseUrl`.

To get started with Foundry Local:

```bash
# Windows: Install Foundry Local CLI (requires winget)
winget install Microsoft.FoundryLocal

# macOS / Linux: see https://foundrylocal.ai for installation instructions
# List available models
foundry model list

# Run a model (starts the local server automatically)
foundry model run phi-4-mini

# Check the port the service is running on
foundry service status
```

### Anthropic

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    { name: "byok", type: "anthropic", baseUrl: "https://api.anthropic.com", apiKey: process.env.ANTHROPIC_API_KEY },
];
```

### Google (Gemini Interactions API, experimental)

```typescript
provider: {
    type: "google",
    baseUrl: "https://generativelanguage.googleapis.com/v1beta",
    apiKey: process.env.GEMINI_API_KEY,
}
```

### Bearer token authentication

Supply static `bearerToken` or client-side `bearerTokenProvider` for bearer authentication. Callbacks receive the configured connection name, manage caching/refresh, and take precedence over static credentials and `apiKeyCommand`. Callback errors fail the request; they never fall back to static credentials.

Use `bearerToken` when your application already has a token:

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    {
        name: "byok",
        type: "openai",
        baseUrl: "https://<resource-name>.openai.azure.com/openai/v1/",
        bearerToken: process.env.MY_BEARER_TOKEN, // Sets Authorization header
    },
];
```

> [!NOTE]
> The `bearerToken` option accepts a **static token string** only. The SDK does not refresh this token automatically. If your token expires, requests will fail and you'll need to create a new session with a fresh token.

Use `bearerTokenProvider` to acquire tokens on demand:

<!-- docs-validate: skip -->

```typescript
const providers: import("@microsoft/copilot-sdk").NamedProviderConfig[] = [
    {
        name: "byok",
        type: "openai",
        baseUrl: "https://my-custom-endpoint.example.com/v1",
        bearerTokenProvider: async () => {
            return await acquireBearerToken();
        },
    },
];
```

For more details about acquiring and refreshing Microsoft Entra bearer tokens, see [Azure Managed Identity with BYOK](../setup/azure-managed-identity.md).

## Custom model listing

When using BYOK, the runtime may not know which models your provider supports. You can supply a custom `onListModels` handler at the client level so that `client.listModels()` returns your provider's models in the generated `Model` format. This lets downstream consumers discover available models without querying the runtime.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";
import type { Model } from "@microsoft/copilot-sdk";

const client = new CopilotClient({
    mode: "coding-agent",
    models: {
        onListModels: (): Model[] => [
            {
                id: "my-custom-model",
                name: "My Custom Model",
                capabilities: {
                    supports: { vision: false, reasoningEffort: false },
                    limits: { max_context_window_tokens: 128000 },
                },
            },
        ],
    },
});
```

</details>

<details>
<summary><strong>Python</strong></summary>

```python
from copilot import CopilotClient, Model
from copilot.rpc import ModelCapabilities, ModelCapabilitiesSupports, ModelCapabilitiesLimits

client = CopilotClient(
    mode="coding-agent",
    models={
        "on_list_models": lambda: [
            Model(
                id="my-custom-model",
                name="My Custom Model",
                capabilities=ModelCapabilities(
                    supports=ModelCapabilitiesSupports(vision=False, reasoning_effort=False),
                    limits=ModelCapabilitiesLimits(max_context_window_tokens=128000),
                ),
            )
        ]
    }
)
```

</details>

<details>
<summary><strong>Go</strong></summary>

```go
package main

import (
	"context"
	copilot "github.com/microsoft/copilot-sdk/go/v2"
)

func main() {
    client := copilot.NewClient(&copilot.ClientOptions{
        Mode: copilot.ModeCodingAgent,
        Models: copilot.ClientModelOptions{
            OnListModels: func(ctx context.Context) ([]copilot.Model, error) {
                return []copilot.Model{
                    {
                        ID:   "my-custom-model",
                        Name: "My Custom Model",
                        Capabilities: copilot.ModelCapabilities{
                            Supports: &copilot.ModelCapabilitiesSupports{Vision: copilot.Bool(false), ReasoningEffort: copilot.Bool(false)},
                            Limits:   &copilot.ModelCapabilitiesLimits{MaxContextWindowTokens: new(int64(128000))},
                        },
                    },
                }, nil
            },
        },
    })
    _ = client
}
```

</details>

<details>
<summary><strong>.NET</strong></summary>

```csharp
using Microsoft.Copilot; using Microsoft.Copilot.Rpc;

var client = new CopilotClient(new CopilotClientOptions
{
    Mode = CopilotClientMode.CodingAgent,
    Models = new ClientModelOptions
    {
        OnListModels = (ct) => Task.FromResult<IList<Model>>(new List<Model>
        {
            new()
            {
                Id = "my-custom-model",
                Name = "My Custom Model",
                Capabilities = new ModelCapabilities
                {
                    Supports = new ModelCapabilitiesSupports
                    {
                        Vision = false,
                        ReasoningEffort = false
                    },
                    Limits = new ModelCapabilitiesLimits
                    {
                        MaxContextWindowTokens = 128000
                    }
                }
            }
        })
    }
});
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;
import com.microsoft.copilot.generated.rpc.Model;
import com.microsoft.copilot.generated.rpc.ModelCapabilities;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesSupports;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesLimits;
import java.util.List;
import java.util.concurrent.CompletableFuture;

var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
    .setModels(new ClientModelOptions().setOnListModels(() -> CompletableFuture.completedFuture(List.of(
        Model.builder()
            .id("my-custom-model")
            .name("My Custom Model")
            .capabilities(ModelCapabilities.builder()
                .supports(ModelCapabilitiesSupports.builder().vision(false).reasoningEffort(false).build())
                .limits(ModelCapabilitiesLimits.builder().maxContextWindowTokens(128000L).build())
                .build())
            .build()
    ))))
);
```

</details>

Results are cached after the first call, just like the default behavior. The handler completely replaces the runtime's `models.list` RPC—no fallback to the runtime occurs.

## Limitations

### Feature limitations

Some Copilot features may behave differently with BYOK:

- **Model availability** - Only models supported by your provider are available
- **Rate limiting** - Subject to your provider's rate limits, not Copilot's
- **Usage tracking** - Usage is tracked by your provider, not GitHub Copilot
- **Premium requests** - Do not count against Copilot premium request quotas

### Provider-specific limitations

| Provider                                           | Limitations                                                                    |
| -------------------------------------------------- | ------------------------------------------------------------------------------ |
| [Microsoft Foundry Local](https://foundrylocal.ai) | Local only; model availability depends on device hardware; no API key required |
| Ollama                                             | No API key; local only; model support varies                                   |
| OpenAI                                             | Subject to OpenAI rate limits and quotas                                       |

## Troubleshooting

### "Model not specified" error

When using BYOK, the `model` parameter is **required**:

```typescript
// ❌ Error: Model required with custom provider
const missingModelSession = await client.createSession({
    github: { capi: { enabled: false } },
    inference: {
        defaultProvider: "byok",
        providers: [{ name: "byok", type: "openai", baseUrl: "..." }],
    },
});

// ✅ Correct: Model specified
const session = await client.createSession({
    model: "gpt-4",
    github: { capi: { enabled: false } },
    inference: {
        // Required!
        defaultProvider: "byok",
        providers: [{ name: "byok", type: "openai", baseUrl: "..." }],
    },
});
```

### Azure endpoint type confusion

For Azure OpenAI endpoints (`*.openai.azure.com`), use the correct type:

<!-- docs-validate: hidden -->

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";

const client = new CopilotClient({ mode: "coding-agent" });
const session = await client.createSession({
    model: "gpt-5.4",
    github: { capi: { enabled: false } },
    inference: {
        defaultProvider: "byok",
        providers: [
            {
                name: "byok",
                type: "azure",
                baseUrl: "https://my-resource.openai.azure.com",
            },
        ],
    },
});
```

<!-- /docs-validate: hidden -->

```typescript
// ✅ Correct: Using "azure" type
defaultProvider: "byok",
providers: [
    {
        name: "byok",
        type: "azure",
        baseUrl: "https://my-resource.openai.azure.com",
    },
],
```

However, if your Microsoft Foundry deployment provides an OpenAI-compatible endpoint path (for example, `/openai/v1/`), use `type: "openai"`:

<!-- docs-validate: hidden -->

```typescript
import { CopilotClient } from "@microsoft/copilot-sdk";

const client = new CopilotClient({ mode: "coding-agent" });
const session = await client.createSession({
    model: "gpt-5.4",
    github: { capi: { enabled: false } },
    inference: {
        defaultProvider: "byok",
        providers: [
            {
                name: "byok",
                type: "openai",
                baseUrl: "https://your-resource.openai.azure.com/openai/v1/",
            },
        ],
    },
});
```

<!-- /docs-validate: hidden -->

```typescript
// ✅ Correct: OpenAI-compatible Microsoft Foundry endpoint
defaultProvider: "byok",
providers: [
    {
        name: "byok",
        type: "openai",
        baseUrl: "https://your-resource.openai.azure.com/openai/v1/",
    },
],
```

### Connection refused (Ollama)

Ensure Ollama is running and accessible:

```bash
# Check Ollama is running
curl http://localhost:11434/v1/models

# Start Ollama if not running
ollama serve
```

### Connection refused (Foundry Local)

Foundry Local uses a dynamic port that may change between restarts. Confirm the active port:

```bash
# Check the service status and port
foundry service status
```

Update your `baseUrl` to match the port shown in the output. If the service is not running, start a model to launch it:

```bash
foundry model run phi-4-mini
```

### Authentication failed

1. Verify your API key is correct and not expired
1. Check the `baseUrl` matches your provider's expected format
1. For bearer tokens, ensure the full token is provided (not just a prefix)

## Next steps

- [Authentication Overview](./README.md) - Learn about all authentication methods
- [Getting Started Guide](../getting-started.md) - Build your first Copilot-powered app

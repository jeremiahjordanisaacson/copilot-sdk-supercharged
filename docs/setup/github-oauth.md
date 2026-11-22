# GitHub OAuth Setup

Let users authenticate with their GitHub accounts to use Copilot through your application. This supports individual accounts, organization memberships, and enterprise identities.

**Best for:** Multi-user apps, internal tools with org access control, SaaS products, apps where users have GitHub accounts.

## How It Works

You create a GitHub OAuth App (or GitHub App), users authorize it, and you pass their access token to the SDK. Copilot requests are made on behalf of each authenticated user, using their Copilot subscription.

```mermaid
sequenceDiagram
    participant User
    participant App as Your App
    participant GH as GitHub
    participant SDK as SDK Client
    participant Runtime as Copilot runtime
    participant API as Copilot API

    User->>App: Click "Sign in with GitHub"
    App->>GH: Redirect to OAuth authorize
    GH->>User: "Authorize this app?"
    User->>GH: Approve
    GH->>App: Authorization code
    App->>GH: Exchange code for token
    GH-->>App: Access token (gho_xxx)

    App->>SDK: Create client with token
    SDK->>Runtime: Start with github.token
    Runtime->>API: Request (as user)
    API-->>Runtime: Response
    Runtime-->>SDK: Result
    SDK-->>App: Display to user
```

**Key characteristics:**
- Each user authenticates with their own GitHub account
- Copilot usage is billed to each user's subscription
- Supports GitHub organizations and enterprise accounts
- Your app never handles model API keys — GitHub manages everything

## Architecture

```mermaid
flowchart TB
    subgraph Users["Users"]
        U1["👤 User A<br/>(Org Member)"]
        U2["👤 User B<br/>(Enterprise)"]
        U3["👤 User C<br/>(Personal)"]
    end

    subgraph App["Your Application"]
        OAuth["OAuth Flow"]
        TokenStore["Token Store"]
        SDK["SDK Client(s)"]
    end

    subgraph Runtime["Copilot runtime"]
        RPC["JSON-RPC"]
    end

    U1 --> OAuth
    U2 --> OAuth
    U3 --> OAuth
    OAuth --> TokenStore
    TokenStore --> SDK
    SDK --> RPC
    RPC --> Copilot["☁️ GitHub Copilot"]

    style Users fill:#161b22,stroke:#8b949e,color:#c9d1d9
    style App fill:#0d1117,stroke:#58a6ff,color:#c9d1d9
    style Runtime fill:#0d1117,stroke:#3fb950,color:#c9d1d9
```

## Step 1: Create a GitHub OAuth App

1. Go to **GitHub Settings → Developer Settings → OAuth Apps → New OAuth App**
   (or for organizations: **Organization Settings → Developer Settings**)

2. Fill in:
   - **Application name**: Your app's name
   - **Homepage URL**: Your app's URL
   - **Authorization callback URL**: Your OAuth callback endpoint (e.g., `https://yourapp.com/auth/callback`)

3. Note your **Client ID** and generate a **Client Secret**

> **GitHub App vs OAuth App:** Both work. GitHub Apps offer finer-grained permissions and are recommended for new projects. OAuth Apps are simpler to set up. The token flow is the same from the SDK's perspective.

## Step 2: Implement the OAuth Flow

Your application handles the standard GitHub OAuth flow. Here's the server-side token exchange:

<!-- docs-validate: strict -->
```typescript
// Server-side: Exchange authorization code for user token
async function handleOAuthCallback(code: string): Promise<string> {
    const response = await fetch("https://github.com/login/oauth/access_token", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            Accept: "application/json",
        },
        body: JSON.stringify({
            client_id: process.env.GITHUB_CLIENT_ID,
            client_secret: process.env.GITHUB_CLIENT_SECRET,
            code,
        }),
    });

    const data = (await response.json()) as { access_token: string };
    return data.access_token; // gho_xxxx or ghu_xxxx
}
```

## Step 3: Pass the Token to the SDK

Create a SDK client for each authenticated user, passing their token:

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient } from "copilot-sdk-supercharged";

// Create a client for an authenticated user
function createClientForUser(userToken: string): CopilotClient {
    return new CopilotClient({
        mode: "coding-agent",
        github: {
            token: userToken,
            useLoggedInUser: false,
        },
    });
}

// Usage
const client = createClientForUser("gho_user_access_token");
const session = await client.createSession({
    sessionId: `user-${userId}-session`,
    model: "auto",
});

const response = await session.sendAndWait({ prompt: "Hello!" });
```

</details>

<details>
<summary><strong>Python</strong></summary>

```python
from copilot import CopilotClient
from copilot.session import PermissionHandler


def create_client_for_user(user_token: str) -> CopilotClient:
    return CopilotClient(mode="coding-agent", github={"token": user_token, "use_logged_in_user": False})


# Usage
user_id = "123"
client = create_client_for_user("gho_user_access_token")
await client.start()

session = await client.create_session(
    model="auto",
    session_id=f"user-{user_id}-session",
    permissions={"on_request": PermissionHandler.approve_all},
)

response = await session.send_and_wait("Hello!")
```

</details>

<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: hidden -->
```go
package main

import (
	"context"
	"fmt"
	copilot "github.com/microsoft/copilot-sdk/go/v2"
)

func createClientForUser(userToken string) *copilot.Client {
	return copilot.NewClient(&copilot.ClientOptions{
		Mode: copilot.ModeCodingAgent,
		GitHub: copilot.ClientGitHubOptions{
			Token:           userToken,
			UseLoggedInUser: copilot.Bool(false),
		},
	})
}

func main() {
	ctx := context.Background()
	userID := "user1"

	client := createClientForUser("gho_user_access_token")
	client.Start(ctx)
	defer client.Stop()

	session, _ := client.CreateSession(ctx, &copilot.CreateSessionOptions{
		SessionID: fmt.Sprintf("user-%s-session", userID),
		Model:     "auto",
	})
	response, _ := session.SendAndWait(ctx, copilot.MessageOptions{
		Prompt: "Hello!",
	})
	_ = response
}
```
<!-- /docs-validate: hidden -->

```go
func createClientForUser(userToken string) *copilot.Client {
	return copilot.NewClient(&copilot.ClientOptions{
		Mode: copilot.ModeCodingAgent,
		GitHub: copilot.ClientGitHubOptions{
			Token:           userToken,
			UseLoggedInUser: copilot.Bool(false),
		},
	})
}

// Usage
client := createClientForUser("gho_user_access_token")
client.Start(ctx)
defer client.Stop()

session, _ := client.CreateSession(ctx, &copilot.CreateSessionOptions{
	SessionID: fmt.Sprintf("user-%s-session", userID),
	Model:     "auto",
})
response, _ := session.SendAndWait(ctx, copilot.MessageOptions{
	Prompt: "Hello!",
})

```

</details>

<details>
<summary><strong>.NET</strong></summary>

<!-- docs-validate: hidden -->
```csharp
using Microsoft.Copilot;

CopilotClient CreateClientForUser(string userToken) => new CopilotClient(new CopilotClientOptions
{
    Mode = CopilotClientMode.CodingAgent,
    GitHub = new ClientGitHubOptions
    {
        Token = userToken,
        UseLoggedInUser = false
    }
});
var userId = "user1";
await using var client = CreateClientForUser("gho_user_access_token");
await using var session = await client.CreateSessionAsync(new CreateSessionOptions
{
    SessionId = $"user-{userId}-session",
    Model = "auto"
});
var response = await session.SendAndWaitAsync(new MessageOptions
{
    Prompt = "Hello!"
});
```
<!-- /docs-validate: hidden -->

```csharp
CopilotClient CreateClientForUser(string userToken) => new CopilotClient(new CopilotClientOptions
{
    Mode = CopilotClientMode.CodingAgent,
    GitHub = new ClientGitHubOptions
    {
        Token = userToken,
        UseLoggedInUser = false
    }
});

// Usage
await using var client = CreateClientForUser("gho_user_access_token");
await using var session = await client.CreateSessionAsync(new CreateSessionOptions
{
    SessionId = $"user-{userId}-session",
    Model = "auto"
});
var response = await session.SendAndWaitAsync(new MessageOptions
{
    Prompt = "Hello!"
});
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;

public class UserClientExample {
    static CopilotClient createClientForUser(String userToken) throws Exception {
        var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setGitHub(new ClientGitHubOptions()
                        .setToken(userToken)
                        .setUseLoggedInUser(false)));
        client
                .start()
                .get();
        return client;
    }

    public static void main(String[] args) throws Exception {
        // Usage — use try-with-resources to ensure cleanup
        var userId = "user1";
        try (var client = createClientForUser("gho_user_access_token")) {
            var session = client
                    .createSession(new CreateSessionOptions()
                            .setSessionId(String
                                    .format("user-%s-session", userId))
                            .setModel("auto")
                            .setPermissions(new SessionPermissionOptions()
                                    .setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            var response = session
                    .sendAndWait(new MessageOptions()
                            .setPrompt("Hello!"))
                    .get();
        }
    }
}
```
<!-- /docs-validate: hidden -->
```java
import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.rpc.*;

CopilotClient createClientForUser(String userToken) throws Exception {
    var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
            .setGitHub(new ClientGitHubOptions()
                    .setToken(userToken)
                    .setUseLoggedInUser(false)));
    client
            .start()
            .get();
    return client;
}

// Usage — use try-with-resources to ensure cleanup
var userId = "user1";
try (var client = createClientForUser("gho_user_access_token")) {
    var session = client
            .createSession(new CreateSessionOptions()
                    .setSessionId(String
                            .format("user-%s-session", userId))
                    .setModel("auto")
                    .setPermissions(new SessionPermissionOptions()
                            .setOnRequest(PermissionHandler.APPROVE_ALL)))
            .get();

    var response = session
            .sendAndWait(new MessageOptions()
                    .setPrompt("Hello!"))
            .get();
}
```

</details>

> **40 languages supported.** See the [full SDK list](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged#available-sdks) with cookbooks for Objective-C, F#, Groovy, Julia, COBOL, OCaml, Zig, Nim, D, Erlang, Crystal, Tcl, Solidity, V, and 18 more.

## Enterprise & Organization Access

GitHub OAuth naturally supports enterprise scenarios. When users authenticate with GitHub, their org memberships and enterprise associations come along.

```mermaid
flowchart TB
    subgraph Enterprise["GitHub Enterprise"]
        Org1["Org: Engineering"]
        Org2["Org: Data Science"]
    end

    subgraph Users
        U1["👤 Alice<br/>(Engineering)"]
        U2["👤 Bob<br/>(Data Science)"]
    end

    U1 -.->|member| Org1
    U2 -.->|member| Org2

    subgraph App["Your Internal App"]
        OAuth["OAuth + Org Check"]
        SDK["SDK Client"]
    end

    U1 --> OAuth
    U2 --> OAuth
    OAuth -->|"Verify org membership"| GH["GitHub API"]
    OAuth --> SDK

    style Enterprise fill:#161b22,stroke:#f0883e,color:#c9d1d9
    style App fill:#0d1117,stroke:#58a6ff,color:#c9d1d9
```

### Verify Organization Membership

After OAuth, check that the user belongs to your organization:

<!-- docs-validate: hidden -->
<!-- docs-validate: strict -->
```typescript
import type { CopilotClient } from "copilot-sdk-supercharged";

declare const code: string;
declare function handleOAuthCallback(code: string): Promise<string>;
declare function createClientForUser(userToken: string): CopilotClient;

async function verifyOrgMembership(
    token: string,
    requiredOrg: string
): Promise<boolean> {
    const response = await fetch("https://api.github.com/user/orgs", {
        headers: { Authorization: `Bearer ${token}` },
    });
    const orgs = (await response.json()) as { login: string }[];
    return orgs.some((org) => org.login === requiredOrg);
}

// In your auth flow
const token = await handleOAuthCallback(code);
if (!await verifyOrgMembership(token, "my-company")) {
    throw new Error("User is not a member of the required organization");
}
const client = createClientForUser(token);
```
<!-- /docs-validate: hidden -->
```typescript
async function verifyOrgMembership(
    token: string,
    requiredOrg: string
): Promise<boolean> {
    const response = await fetch("https://api.github.com/user/orgs", {
        headers: { Authorization: `Bearer ${token}` },
    });
    const orgs = (await response.json()) as { login: string }[];
    return orgs.some((org) => org.login === requiredOrg);
}

// In your auth flow
const token = await handleOAuthCallback(code);
if (!await verifyOrgMembership(token, "my-company")) {
    throw new Error("User is not a member of the required organization");
}
const client = createClientForUser(token);
```

### Enterprise Managed Users (EMU)

For GitHub Enterprise Managed Users, the flow is identical — EMU users authenticate through GitHub OAuth like any other user. Their enterprise policies (IP restrictions, SAML SSO) are enforced by GitHub automatically.

```typescript
// No special SDK configuration needed for EMU
// Enterprise policies are enforced server-side by GitHub
const client = new CopilotClient({
    mode: "coding-agent",
    github: {
        token: emuUserToken,
        // Works the same as regular tokens
        useLoggedInUser: false,
    },
});
```

## Supported Token Types

| Token Prefix | Source | Works? |
|-------------|--------|--------|
| `gho_` | OAuth user access token | ✅ |
| `ghu_` | GitHub App user access token | ✅ |
| `github_pat_` | Fine-grained personal access token | ✅ |
| `ghp_` | Classic personal access token | ❌ (deprecated) |

## Token Lifecycle

```mermaid
flowchart LR
    A["User authorizes"] --> B["Token issued<br/>(gho_xxx)"]
    B --> C{"Token valid?"}
    C -->|Yes| D["SDK uses token"]
    C -->|No| E["Refresh or<br/>re-authorize"]
    E --> B
    D --> F{"User revokes<br/>or token expires?"}
    F -->|Yes| E
    F -->|No| D

    style A fill:#0d1117,stroke:#3fb950,color:#c9d1d9
    style E fill:#0d1117,stroke:#f0883e,color:#c9d1d9
```

**Important:** Your application is responsible for token storage, refresh, and expiration handling. The SDK uses whatever token you provide — it doesn't manage the OAuth lifecycle.

### Token Refresh Pattern

```typescript
async function getOrRefreshToken(userId: string): Promise<string> {
    const stored = await tokenStore.get(userId);

    if (stored && !isExpired(stored)) {
        return stored.accessToken;
    }

    if (stored?.refreshToken) {
        const refreshed = await refreshGitHubToken(stored.refreshToken);
        await tokenStore.set(userId, refreshed);
        return refreshed.accessToken;
    }

    throw new Error("User must re-authenticate");
}
```

## Multi-User Patterns

### One Client Per User (Recommended)

Each user gets their own SDK client with their own token. By default, each client hosts its runtime in your application process (see [In-process runtime](./in-process-runtime.md)); to isolate users in separate runtime processes, give each client a spawned connection such as `RuntimeConnection.forSpawnedStdio()`.

```typescript
const clients = new Map<string, CopilotClient>();

function getClientForUser(userId: string, token: string): CopilotClient {
    if (!clients.has(userId)) {
        clients.set(
            userId,
            new CopilotClient({
                mode: "coding-agent",
                github: {
                    token: token,
                    useLoggedInUser: false,
                },
            }),
        );
    }
    return clients.get(userId)!;
}
```

### Shared runtime with per-session tokens

For a lighter resource footprint, you can run a single standalone runtime server and pass tokens per session. A client attached to the server with `RuntimeConnection.forTcp` can't set the client-level `github` token options shown above, so pass each user's token on the session instead (`github: { token }` in `createSession`). See [Backend Services](./backend-services.md) for this pattern.

## Limitations

| Limitation | Details |
|------------|---------|
| **Copilot subscription required** | Each user needs an active Copilot subscription |
| **Token management is your responsibility** | Store, refresh, and handle expiration |
| **GitHub account required** | Users must have GitHub accounts |
| **Rate limits per user** | Subject to each user's Copilot rate limits |

## When to Move On

| Need | Next Guide |
|------|-----------|
| Users without GitHub accounts | [BYOK](../auth/byok.md) |
| Run the SDK on servers | [Backend Services](./backend-services.md) |
| Handle many concurrent users | [Scaling & Multi-Tenancy](./scaling.md) |

## Next Steps

- **[Authentication docs](../auth/index.md)** — Full auth method reference
- **[Backend Services](./backend-services.md)** — Run the SDK server-side
- **[Scaling & Multi-Tenancy](./scaling.md)** — Handle many users at scale

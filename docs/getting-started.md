# Build Your First Copilot-Powered App

In this tutorial, you'll use the Copilot SDK to build a command-line assistant. You'll start with the basics, add streaming responses, then add custom tools - giving Copilot the ability to call your code.

The Copilot SDK Supercharged supports **21 languages**. This guide covers the core SDKs in detail, with links to all language SDKs.

**What you'll build:**

```
You: What's the weather like in Seattle?
Copilot: Let me check the weather for Seattle...
         Currently 62°F and cloudy with a chance of rain.
         Typical Seattle weather!

You: How about Tokyo?
Copilot: In Tokyo it's 75°F and sunny. Great day to be outside!
```

## Prerequisites

Before you begin, make sure you have:

- **GitHub Copilot CLI** installed and authenticated ([Installation guide](https://docs.github.com/en/copilot/how-tos/set-up/install-copilot-cli))
- Your preferred language runtime:
  - **Node.js** 18+ or **Python** 3.11+ or **Go** 1.21+ or **Java** 17+ or **.NET** 8.0+
  - Additional languages: Rust, Ruby 3.1+, PHP 8.1+, Swift 5.9+, Kotlin 1.9+, C++17, C11, Dart 3.0+, Scala 3.4+, R 4.0+, Perl 5.32+, Lua 5.1+, Bash 4+, Elixir 1.15+, Haskell (GHC 9.4+), Clojure 1.12+

Verify the CLI is working:

```bash
copilot --version
```

## Step 1: Install the SDK

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

First, create a new directory and initialize your project:

```bash
mkdir copilot-demo && cd copilot-demo
npm init -y --init-type module
```

Then install the SDK and TypeScript runner:

```bash
npm install copilot-sdk-supercharged tsx
```

</details>

<details>
<summary><strong>Python</strong></summary>

```bash
pip install github-copilot-sdk
```

</details>

<details>
<summary><strong>Go</strong></summary>

First, create a new directory and initialize your module:

```bash
mkdir copilot-demo && cd copilot-demo
go mod init copilot-demo
```

Then install the SDK:

```bash
go get github.com/github/copilot-sdk/go
```

</details>

<details>
<summary><strong>.NET</strong></summary>

First, create a new console project:

```bash
dotnet new console -n CopilotDemo && cd CopilotDemo
```

Then add the SDK:

```bash
dotnet add package GitHub.Copilot.SDK
```

</details>

<details>
<summary><strong>Java</strong></summary>

First, create a new directory and initialize your project.

**Maven** — add to your `pom.xml`:

```xml
<dependency>
    <groupId>com.github</groupId>
    <artifactId>copilot-sdk-java</artifactId>
    <version>${copilot.sdk.version}</version>
</dependency>
```

**Gradle** — add to your `build.gradle`:

```groovy
implementation 'com.github:copilot-sdk-java:${copilotSdkVersion}'
```

</details>


<details>
<summary><strong>Rust</strong></summary>

Add to your `Cargo.toml`:

```toml
[dependencies]
copilot-sdk-supercharged = { path = "../rust" }
```

Or reference the SDK directory from this repo.

</details>

<details>
<summary><strong>Ruby</strong></summary>

```bash
gem install copilot-sdk-supercharged
```

Or add to your `Gemfile`:

```ruby
gem 'copilot-sdk-supercharged', path: '../ruby'
```

</details>

<details>
<summary><strong>PHP</strong></summary>

```bash
composer require github/copilot-sdk-supercharged
```

</details>

<details>
<summary><strong>Swift</strong></summary>

Add to your `Package.swift`:

```swift
.package(path: "../swift")
```

</details>

<details>
<summary><strong>Kotlin</strong></summary>

Add the SDK as a local dependency in `build.gradle.kts`:

```kotlin
implementation(project(":copilot-sdk-supercharged-kotlin"))
```

</details>

<details>
<summary><strong>Other Languages</strong></summary>

See the individual SDK READMEs for installation instructions:

| Language | README |
|----------|--------|
| C++ | [`cpp/README.md`](../cpp/README.md) |
| C | [`c/README.md`](../c/README.md) |
| Dart | [`dart/README.md`](../dart/README.md) |
| Scala | [`scala/README.md`](../scala/README.md) |
| R | [`r/README.md`](../r/README.md) |
| Perl | [`perl/README.md`](../perl/README.md) |
| Lua | [`lua/README.md`](../lua/README.md) |
| Shell/Bash | [`shell/README.md`](../shell/README.md) |
| Elixir | [`elixir/README.md`](../elixir/README.md) |
| Haskell | [`haskell/README.md`](../haskell/README.md) |
| Clojure | [`clojure/README.md`](../clojure/README.md) |

</details>

## Step 2: Send Your First Message

Create a new file and add the following code. This is the simplest way to use the SDK—about 5 lines of code.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

Create `index.ts`:

```typescript
import { CopilotClient } from "copilot-sdk-supercharged";

const client = new CopilotClient();
const session = await client.createSession({ model: "gpt-4.1" });

const response = await session.sendAndWait({ prompt: "What is 2 + 2?" });
console.log(response?.data.content);

await client.stop();
process.exit(0);
```

Run it:

```bash
npx tsx index.ts
```

</details>

<details>
<summary><strong>Python</strong></summary>

Create `main.py`:

```python
import asyncio
from copilot import CopilotClient
from copilot.session import PermissionHandler

async def main():
    client = CopilotClient()
    await client.start()

    session = await client.create_session(on_permission_request=PermissionHandler.approve_all, model="gpt-4.1")
    response = await session.send_and_wait("What is 2 + 2?")
    print(response.data.content)

    await client.stop()

asyncio.run(main())
```

Run it:

```bash
python main.py
```

</details>

<details>
<summary><strong>Go</strong></summary>

Create `main.go`:

```go
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	copilot "github.com/github/copilot-sdk/go"
)

func main() {
	ctx := context.Background()
	client := copilot.NewClient(nil)
	if err := client.Start(ctx); err != nil {
		log.Fatal(err)
	}
	defer client.Stop()

	session, err := client.CreateSession(ctx, &copilot.SessionConfig{Model: "gpt-4.1"})
	if err != nil {
		log.Fatal(err)
	}

	response, err := session.SendAndWait(ctx, copilot.MessageOptions{Prompt: "What is 2 + 2?"})
	if err != nil {
		log.Fatal(err)
	}

	if d, ok := response.Data.(*copilot.AssistantMessageData); ok {
		fmt.Println(d.Content)
	}
	os.Exit(0)
}
```

Run it:

```bash
go run main.go
```

</details>

<details>
<summary><strong>.NET</strong></summary>

Create a new console project and add this to `Program.cs`:

```csharp
using GitHub.Copilot.SDK;

await using var client = new CopilotClient();
await using var session = await client.CreateSessionAsync(new SessionConfig
{
    Model = "gpt-4.1",
    OnPermissionRequest = PermissionHandler.ApproveAll
});

var response = await session.SendAndWaitAsync(new MessageOptions { Prompt = "What is 2 + 2?" });
Console.WriteLine(response?.Data.Content);
```

Run it:

```bash
dotnet run
```

</details>

<details>
<summary><strong>Java</strong></summary>

Create `HelloCopilot.java`:

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.events.*;
import com.github.copilot.sdk.json.*;

public class HelloCopilot {
    public static void main(String[] args) throws Exception {
        try (var client = new CopilotClient()) {
            client.start().get();

            var session = client.createSession(
                new SessionConfig()
                    .setModel("gpt-4.1")
                    .setOnPermissionRequest(PermissionHandler.APPROVE_ALL)
            ).get();

            var response = session.sendAndWait(
                new MessageOptions().setPrompt("What is 2 + 2?")
            ).get();

            System.out.println(response.getData().content());

            client.stop().get();
        }
    }
}
```

Run it:

```bash
javac -cp copilot-sdk.jar HelloCopilot.java && java -cp .:copilot-sdk.jar HelloCopilot
```

</details>

<details>
<summary><strong>Rust</strong></summary>

Create `src/main.rs`:

```rust
use copilot_sdk::{CopilotClient, SessionConfig, MessageOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(None);
    client.start().await?;

    let session = client.create_session(&SessionConfig {
        model: "gpt-4.1".into(),
        ..Default::default()
    }).await?;

    let response = session.send_and_wait(&MessageOptions {
        prompt: "What is 2 + 2?".into(),
        ..Default::default()
    }).await?;

    if let Some(resp) = response {
        println!("{}", resp.data.content);
    }

    client.stop().await?;
    Ok(())
}
```

Run it:

```bash
cargo run
```

</details>

<details>
<summary><strong>Ruby</strong></summary>

Create `main.rb`:

```ruby
require "copilot_sdk"

client = CopilotSDK::Client.new
client.start

session = client.create_session(model: "gpt-4.1")

response = session.send_and_wait(prompt: "What is 2 + 2?")
puts response.data.content

client.stop
```

Run it:

```bash
ruby main.rb
```

</details>

<details>
<summary><strong>PHP</strong></summary>

Create `main.php`:

```php
<?php
require_once 'vendor/autoload.php';

use CopilotSDK\CopilotClient;

$client = new CopilotClient();
$client->start();

$session = $client->createSession(['model' => 'gpt-4.1']);

$response = $session->sendAndWait(['prompt' => 'What is 2 + 2?']);
echo $response->data->content . "\n";

$client->stop();
```

Run it:

```bash
php main.php
```

</details>

<details>
<summary><strong>Swift</strong></summary>

Create `main.swift`:

```swift
import CopilotSDK

let client = CopilotClient()
try await client.start()

let session = try await client.createSession(config: SessionConfig(
    model: "gpt-4.1"
))

let response = try await session.sendAndWait(prompt: "What is 2 + 2?")
if let content = response?.data.content {
    print(content)
}

try await client.stop()
```

Run it:

```bash
swift run
```

</details>

<details>
<summary><strong>Kotlin</strong></summary>

Create `HelloCopilot.kt`:

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.MessageOptions

suspend fun main() {
    val client = CopilotClient()
    client.start()

    val session = client.createSession(SessionConfig(model = "gpt-4.1"))

    val response = session.sendAndWait(MessageOptions(prompt = "What is 2 + 2?"))
    println(response?.data?.content)

    client.stop()
}
```

Run it:

```bash
kotlinc -cp copilot-sdk.jar HelloCopilot.kt -include-runtime -d hello.jar && java -jar hello.jar
```

</details>

<details>
<summary><strong>C++</strong></summary>

Create `main.cpp`:

```cpp
#include <copilot/client.h>
#include <iostream>

int main() {
    copilot::CopilotClient client;
    client.start();

    auto session = client.createSession({
        .model = "gpt-4.1"
    });

    auto response = session.sendAndWait({
        .prompt = "What is 2 + 2?"
    });

    if (response) {
        std::cout << response->data.content << std::endl;
    }

    client.stop();
    return 0;
}
```

Build and run:

```bash
cmake -B build && cmake --build build && ./build/main
```

</details>

**You should see:**

```
4
```

Congratulations! You just built your first Copilot-powered app.

## Step 3: Add Streaming Responses

Right now, you wait for the complete response before seeing anything. Let's make it interactive by streaming the response as it's generated.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

Update `index.ts`:

```typescript
import { CopilotClient } from "copilot-sdk-supercharged";

const client = new CopilotClient();
const session = await client.createSession({
    model: "gpt-4.1",
    streaming: true,
});

// Listen for response chunks
session.on("assistant.message_delta", (event) => {
    process.stdout.write(event.data.deltaContent);
});
session.on("session.idle", () => {
    console.log(); // New line when done
});

await session.sendAndWait({ prompt: "Tell me a short joke" });

await client.stop();
process.exit(0);
```

</details>

<details>
<summary><strong>Python</strong></summary>

Update `main.py`:

```python
import asyncio
import sys
from copilot import CopilotClient
from copilot.session import PermissionHandler
from copilot.generated.session_events import SessionEventType

async def main():
    client = CopilotClient()
    await client.start()

    session = await client.create_session(on_permission_request=PermissionHandler.approve_all, model="gpt-4.1", streaming=True)

    # Listen for response chunks
    def handle_event(event):
        if event.type == SessionEventType.ASSISTANT_MESSAGE_DELTA:
            sys.stdout.write(event.data.delta_content)
            sys.stdout.flush()
        if event.type == SessionEventType.SESSION_IDLE:
            print()  # New line when done

    session.on(handle_event)

    await session.send_and_wait("Tell me a short joke")

    await client.stop()

asyncio.run(main())
```

</details>

<details>
<summary><strong>Go</strong></summary>

Update `main.go`:

```go
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	copilot "github.com/github/copilot-sdk/go"
)

func main() {
	ctx := context.Background()
	client := copilot.NewClient(nil)
	if err := client.Start(ctx); err != nil {
		log.Fatal(err)
	}
	defer client.Stop()

	session, err := client.CreateSession(ctx, &copilot.SessionConfig{
		Model:     "gpt-4.1",
		Streaming: true,
	})
	if err != nil {
		log.Fatal(err)
	}

	// Listen for response chunks
	session.On(func(event copilot.SessionEvent) {
		switch d := event.Data.(type) {
		case *copilot.AssistantMessageDeltaData:
			fmt.Print(d.DeltaContent)
		case *copilot.SessionIdleData:
			_ = d
			fmt.Println()
		}
	})

	_, err = session.SendAndWait(ctx, copilot.MessageOptions{Prompt: "Tell me a short joke"})
	if err != nil {
		log.Fatal(err)
	}
	os.Exit(0)
}
```

</details>

<details>
<summary><strong>.NET</strong></summary>

Update `Program.cs`:

```csharp
using GitHub.Copilot.SDK;

await using var client = new CopilotClient();
await using var session = await client.CreateSessionAsync(new SessionConfig
{
    Model = "gpt-4.1",
    OnPermissionRequest = PermissionHandler.ApproveAll,
    Streaming = true,
});

// Listen for response chunks
session.On(ev =>
{
    if (ev is AssistantMessageDeltaEvent deltaEvent)
    {
        Console.Write(deltaEvent.Data.DeltaContent);
    }
    if (ev is SessionIdleEvent)
    {
        Console.WriteLine();
    }
});

await session.SendAndWaitAsync(new MessageOptions { Prompt = "Tell me a short joke" });
```

</details>

<details>
<summary><strong>Java</strong></summary>

Update `HelloCopilot.java`:

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.events.*;
import com.github.copilot.sdk.json.*;

public class HelloCopilot {
    public static void main(String[] args) throws Exception {
        try (var client = new CopilotClient()) {
            client.start().get();

            var session = client.createSession(
                new SessionConfig()
                    .setModel("gpt-4.1")
                    .setStreaming(true)
                    .setOnPermissionRequest(PermissionHandler.APPROVE_ALL)
            ).get();

            // Listen for response chunks
            session.on(AssistantMessageDeltaEvent.class, delta -> {
                System.out.print(delta.getData().deltaContent());
            });
            session.on(SessionIdleEvent.class, idle -> {
                System.out.println(); // New line when done
            });

            session.sendAndWait(
                new MessageOptions().setPrompt("Tell me a short joke")
            ).get();

            client.stop().get();
        }
    }
}
```

</details>

<details>
<summary><strong>Rust</strong></summary>

Update `src/main.rs`:

```rust
use copilot_sdk::{CopilotClient, SessionConfig, MessageOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(None);
    client.start().await?;

    let session = client.create_session(&SessionConfig {
        model: "gpt-4.1".into(),
        streaming: true,
        ..Default::default()
    }).await?;

    // Listen for response chunks
    session.on(|event| {
        match event.event_type.as_str() {
            "assistant.message_delta" => {
                print!("{}", event.data.delta_content);
            }
            "session.idle" => {
                println!();
            }
            _ => {}
        }
    });

    session.send_and_wait(&MessageOptions {
        prompt: "Tell me a short joke".into(),
        ..Default::default()
    }).await?;

    client.stop().await?;
    Ok(())
}
```

</details>

<details>
<summary><strong>Ruby</strong></summary>

Update `main.rb`:

```ruby
require "copilot_sdk"

client = CopilotSDK::Client.new
client.start

session = client.create_session(model: "gpt-4.1", streaming: true)

# Listen for response chunks
session.on do |event|
  case event.type
  when "assistant.message_delta"
    $stdout.write(event.data.delta_content)
  when "session.idle"
    puts
  end
end

session.send_and_wait(prompt: "Tell me a short joke")

client.stop
```

</details>

<details>
<summary><strong>PHP</strong></summary>

Update `main.php`:

```php
<?php
require_once 'vendor/autoload.php';

use CopilotSDK\CopilotClient;

$client = new CopilotClient();
$client->start();

$session = $client->createSession([
    'model' => 'gpt-4.1',
    'streaming' => true,
]);

// Listen for response chunks
$session->on(function ($event) {
    if ($event->type === 'assistant.message_delta') {
        echo $event->data->deltaContent;
    }
    if ($event->type === 'session.idle') {
        echo "\n";
    }
});

$session->sendAndWait(['prompt' => 'Tell me a short joke']);

$client->stop();
```

</details>

<details>
<summary><strong>Swift</strong></summary>

Update `main.swift`:

```swift
import CopilotSDK

let client = CopilotClient()
try await client.start()

let session = try await client.createSession(config: SessionConfig(
    model: "gpt-4.1",
    streaming: true
))

// Listen for response chunks
session.on { event in
    if let delta = event as? AssistantMessageDeltaEvent {
        print(delta.data.deltaContent, terminator: "")
    }
    if event is SessionIdleEvent {
        print()
    }
}

try await session.sendAndWait(prompt: "Tell me a short joke")

try await client.stop()
```

</details>

<details>
<summary><strong>Kotlin</strong></summary>

Update `HelloCopilot.kt`:

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.MessageOptions
import com.github.copilot.sdk.events.*

suspend fun main() {
    val client = CopilotClient()
    client.start()

    val session = client.createSession(SessionConfig(
        model = "gpt-4.1",
        streaming = true
    ))

    // Listen for response chunks
    session.on { event ->
        when (event) {
            is AssistantMessageDeltaEvent -> print(event.data.deltaContent)
            is SessionIdleEvent -> println()
        }
    }

    session.sendAndWait(MessageOptions(prompt = "Tell me a short joke"))

    client.stop()
}
```

</details>

<details>
<summary><strong>C++</strong></summary>

Update `main.cpp`:

```cpp
#include <copilot/client.h>
#include <iostream>

int main() {
    copilot::CopilotClient client;
    client.start();

    auto session = client.createSession({
        .model = "gpt-4.1",
        .streaming = true
    });

    // Listen for response chunks
    session.on([](const auto& event) {
        if (event.type == "assistant.message_delta") {
            std::cout << event.data.deltaContent;
        }
        if (event.type == "session.idle") {
            std::cout << std::endl;
        }
    });

    session.sendAndWait({.prompt = "Tell me a short joke"});

    client.stop();
    return 0;
}
```

</details>

Run the code again. You'll see the response appear word by word.

### Event Subscription Methods

The SDK provides methods for subscribing to session events:

| Method | Description |
|--------|-------------|
| `on(handler)` | Subscribe to all events; returns unsubscribe function |
| `on(eventType, handler)` | Subscribe to specific event type (Node.js/TypeScript only); returns unsubscribe function |

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
// Subscribe to all events
const unsubscribeAll = session.on((event) => {
    console.log("Event:", event.type);
});

// Subscribe to specific event type
const unsubscribeIdle = session.on("session.idle", (event) => {
    console.log("Session is idle");
});

// Later, to unsubscribe:
unsubscribeAll();
unsubscribeIdle();
```

</details>

<details>
<summary><strong>Python</strong></summary>

<!-- docs-validate: hidden -->
```python
from copilot import CopilotClient
from copilot.generated.session_events import SessionEvent, SessionEventType
from copilot.session import PermissionRequestResult

client = CopilotClient()

session = await client.create_session(on_permission_request=lambda req, inv: PermissionRequestResult(kind="approved"))

# Subscribe to all events
unsubscribe = session.on(lambda event: print(f"Event: {event.type}"))

# Filter by event type in your handler
def handle_event(event: SessionEvent) -> None:
    if event.type == SessionEventType.SESSION_IDLE:
        print("Session is idle")
    elif event.type == SessionEventType.ASSISTANT_MESSAGE:
        print(f"Message: {event.data.content}")

unsubscribe = session.on(handle_event)

# Later, to unsubscribe:
unsubscribe()
```
<!-- /docs-validate: hidden -->

```python
# Subscribe to all events
unsubscribe = session.on(lambda event: print(f"Event: {event.type}"))

# Filter by event type in your handler
def handle_event(event):
    if event.type == SessionEventType.SESSION_IDLE:
        print("Session is idle")
    elif event.type == SessionEventType.ASSISTANT_MESSAGE:
        print(f"Message: {event.data.content}")

unsubscribe = session.on(handle_event)

# Later, to unsubscribe:
unsubscribe()
```

</details>

<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: hidden -->
```go
package main

import (
	"fmt"

	copilot "github.com/github/copilot-sdk/go"
)

func main() {
	session := &copilot.Session{}

	// Subscribe to all events
	unsubscribe := session.On(func(event copilot.SessionEvent) {
		fmt.Println("Event:", event.Type)
	})

	// Filter by event type in your handler
	session.On(func(event copilot.SessionEvent) {
		switch d := event.Data.(type) {
		case *copilot.SessionIdleData:
			_ = d
			fmt.Println("Session is idle")
		case *copilot.AssistantMessageData:
			fmt.Println("Message:", d.Content)
		}
	})

	// Later, to unsubscribe:
	unsubscribe()
}
```
<!-- /docs-validate: hidden -->

```go
// Subscribe to all events
unsubscribe := session.On(func(event copilot.SessionEvent) {
    fmt.Println("Event:", event.Type)
})

// Filter by event type in your handler
session.On(func(event copilot.SessionEvent) {
    switch d := event.Data.(type) {
    case *copilot.SessionIdleData:
        _ = d
        fmt.Println("Session is idle")
    case *copilot.AssistantMessageData:
        fmt.Println("Message:", d.Content)
    }
})

// Later, to unsubscribe:
unsubscribe()
```

</details>

<details>
<summary><strong>.NET</strong></summary>

<!-- docs-validate: hidden -->
```csharp
using GitHub.Copilot.SDK;

public static class EventSubscriptionExample
{
    public static void Example(CopilotSession session)
    {
        // Subscribe to all events
        var unsubscribe = session.On(ev => Console.WriteLine($"Event: {ev.Type}"));

        // Filter by event type using pattern matching
        session.On(ev =>
        {
            switch (ev)
            {
                case SessionIdleEvent:
                    Console.WriteLine("Session is idle");
                    break;
                case AssistantMessageEvent msg:
                    Console.WriteLine($"Message: {msg.Data.Content}");
                    break;
            }
        });

        // Later, to unsubscribe:
        unsubscribe.Dispose();
    }
}
```
<!-- /docs-validate: hidden -->

```csharp
// Subscribe to all events
var unsubscribe = session.On(ev => Console.WriteLine($"Event: {ev.Type}"));

// Filter by event type using pattern matching
session.On(ev =>
{
    switch (ev)
    {
        case SessionIdleEvent:
            Console.WriteLine("Session is idle");
            break;
        case AssistantMessageEvent msg:
            Console.WriteLine($"Message: {msg.Data.Content}");
            break;
    }
});

// Later, to unsubscribe:
unsubscribe.Dispose();
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
// Subscribe to all events
var unsubscribe = session.on(event -> {
    System.out.println("Event: " + event.getType());
});

// Subscribe to a specific event type
session.on(AssistantMessageEvent.class, msg -> {
    System.out.println("Message: " + msg.getData().content());
});

session.on(SessionIdleEvent.class, idle -> {
    System.out.println("Session is idle");
});

// Later, to unsubscribe:
unsubscribe.close();
```

</details>

<details>
<summary><strong>Rust</strong></summary>

```rust
// Subscribe to all events
let unsubscribe = session.on(|event| {
    println!("Event: {}", event.event_type);
});

// Filter by event type in your handler
session.on(|event| {
    match event.event_type.as_str() {
        "session.idle" => println!("Session is idle"),
        "assistant.message" => {
            println!("Message: {}", event.data.content);
        }
        _ => {}
    }
});

// Later, to unsubscribe:
unsubscribe();
```

</details>

<details>
<summary><strong>Ruby</strong></summary>

```ruby
# Subscribe to all events
unsubscribe = session.on { |event| puts "Event: #{event.type}" }

# Filter by event type in your handler
session.on do |event|
  case event.type
  when "session.idle"
    puts "Session is idle"
  when "assistant.message"
    puts "Message: #{event.data.content}"
  end
end

# Later, to unsubscribe:
unsubscribe.call
```

</details>

<details>
<summary><strong>PHP</strong></summary>

```php
// Subscribe to all events
$unsubscribe = $session->on(function ($event) {
    echo "Event: " . $event->type . "\n";
});

// Filter by event type in your handler
$session->on(function ($event) {
    if ($event->type === 'session.idle') {
        echo "Session is idle\n";
    } elseif ($event->type === 'assistant.message') {
        echo "Message: " . $event->data->content . "\n";
    }
});

// Later, to unsubscribe:
$unsubscribe();
```

</details>

<details>
<summary><strong>Swift</strong></summary>

```swift
// Subscribe to all events
let unsubscribe = session.on { event in
    print("Event: \(event.type)")
}

// Filter by event type in your handler
session.on { event in
    if event is SessionIdleEvent {
        print("Session is idle")
    }
    if let msg = event as? AssistantMessageEvent {
        print("Message: \(msg.data.content)")
    }
}

// Later, to unsubscribe:
unsubscribe()
```

</details>

<details>
<summary><strong>Kotlin</strong></summary>

```kotlin
// Subscribe to all events
val unsubscribe = session.on { event ->
    println("Event: ${event.type}")
}

// Filter by event type in your handler
session.on { event ->
    when (event) {
        is SessionIdleEvent -> println("Session is idle")
        is AssistantMessageEvent -> println("Message: ${event.data.content}")
    }
}

// Later, to unsubscribe:
unsubscribe()
```

</details>

<details>
<summary><strong>C++</strong></summary>

```cpp
// Subscribe to all events
auto unsubscribe = session.on([](const auto& event) {
    std::cout << "Event: " << event.type << std::endl;
});

// Filter by event type in your handler
session.on([](const auto& event) {
    if (event.type == "session.idle") {
        std::cout << "Session is idle" << std::endl;
    } else if (event.type == "assistant.message") {
        std::cout << "Message: " << event.data.content << std::endl;
    }
});

// Later, to unsubscribe:
unsubscribe();
```

</details>

## Step 4: Add a Custom Tool

Now for the powerful part. Let's give Copilot the ability to call your code by defining a custom tool. We'll create a simple weather lookup tool.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

Update `index.ts`:

```typescript
import { CopilotClient, defineTool } from "copilot-sdk-supercharged";

// Define a tool that Copilot can call
const getWeather = defineTool("get_weather", {
    description: "Get the current weather for a city",
    parameters: {
        type: "object",
        properties: {
            city: { type: "string", description: "The city name" },
        },
        required: ["city"],
    },
    handler: async (args: { city: string }) => {
        const { city } = args;
        // In a real app, you'd call a weather API here
        const conditions = ["sunny", "cloudy", "rainy", "partly cloudy"];
        const temp = Math.floor(Math.random() * 30) + 50;
        const condition = conditions[Math.floor(Math.random() * conditions.length)];
        return { city, temperature: `${temp}°F`, condition };
    },
});

const client = new CopilotClient();
const session = await client.createSession({
    model: "gpt-4.1",
    streaming: true,
    tools: [getWeather],
});

session.on("assistant.message_delta", (event) => {
    process.stdout.write(event.data.deltaContent);
});

session.on("session.idle", () => {
    console.log(); // New line when done
});

await session.sendAndWait({
    prompt: "What's the weather like in Seattle and Tokyo?",
});

await client.stop();
process.exit(0);
```

</details>

<details>
<summary><strong>Python</strong></summary>

Update `main.py`:

```python
import asyncio
import random
import sys
from copilot import CopilotClient
from copilot.session import PermissionHandler
from copilot.tools import define_tool
from copilot.generated.session_events import SessionEventType
from pydantic import BaseModel, Field

# Define the parameters for the tool using Pydantic
class GetWeatherParams(BaseModel):
    city: str = Field(description="The name of the city to get weather for")

# Define a tool that Copilot can call
@define_tool(description="Get the current weather for a city")
async def get_weather(params: GetWeatherParams) -> dict:
    city = params.city
    # In a real app, you'd call a weather API here
    conditions = ["sunny", "cloudy", "rainy", "partly cloudy"]
    temp = random.randint(50, 80)
    condition = random.choice(conditions)
    return {"city": city, "temperature": f"{temp}°F", "condition": condition}

async def main():
    client = CopilotClient()
    await client.start()

    session = await client.create_session(on_permission_request=PermissionHandler.approve_all, model="gpt-4.1", streaming=True, tools=[get_weather])

    def handle_event(event):
        if event.type == SessionEventType.ASSISTANT_MESSAGE_DELTA:
            sys.stdout.write(event.data.delta_content)
            sys.stdout.flush()
        if event.type == SessionEventType.SESSION_IDLE:
            print()

    session.on(handle_event)

    await session.send_and_wait("What's the weather like in Seattle and Tokyo?")

    await client.stop()

asyncio.run(main())
```

</details>

<details>
<summary><strong>Go</strong></summary>

Update `main.go`:

```go
package main

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"os"

	copilot "github.com/github/copilot-sdk/go"
)

// Define the parameter type
type WeatherParams struct {
	City string `json:"city" jsonschema:"The city name"`
}

// Define the return type
type WeatherResult struct {
	City        string `json:"city"`
	Temperature string `json:"temperature"`
	Condition   string `json:"condition"`
}

func main() {
	ctx := context.Background()

	// Define a tool that Copilot can call
	getWeather := copilot.DefineTool(
		"get_weather",
		"Get the current weather for a city",
		func(params WeatherParams, inv copilot.ToolInvocation) (WeatherResult, error) {
			// In a real app, you'd call a weather API here
			conditions := []string{"sunny", "cloudy", "rainy", "partly cloudy"}
			temp := rand.Intn(30) + 50
			condition := conditions[rand.Intn(len(conditions))]
			return WeatherResult{
				City:        params.City,
				Temperature: fmt.Sprintf("%d°F", temp),
				Condition:   condition,
			}, nil
		},
	)

	client := copilot.NewClient(nil)
	if err := client.Start(ctx); err != nil {
		log.Fatal(err)
	}
	defer client.Stop()

	session, err := client.CreateSession(ctx, &copilot.SessionConfig{
		Model:     "gpt-4.1",
		Streaming: true,
		Tools:     []copilot.Tool{getWeather},
	})
	if err != nil {
		log.Fatal(err)
	}

	session.On(func(event copilot.SessionEvent) {
		switch d := event.Data.(type) {
		case *copilot.AssistantMessageDeltaData:
			fmt.Print(d.DeltaContent)
		case *copilot.SessionIdleData:
			_ = d
			fmt.Println()
		}
	})

	_, err = session.SendAndWait(ctx, copilot.MessageOptions{
		Prompt: "What's the weather like in Seattle and Tokyo?",
	})
	if err != nil {
		log.Fatal(err)
	}
	os.Exit(0)
}
```

</details>

<details>
<summary><strong>.NET</strong></summary>

Update `Program.cs`:

```csharp
using GitHub.Copilot.SDK;
using Microsoft.Extensions.AI;
using System.ComponentModel;

await using var client = new CopilotClient();

// Define a tool that Copilot can call
var getWeather = AIFunctionFactory.Create(
    ([Description("The city name")] string city) =>
    {
        // In a real app, you'd call a weather API here
        var conditions = new[] { "sunny", "cloudy", "rainy", "partly cloudy" };
        var temp = Random.Shared.Next(50, 80);
        var condition = conditions[Random.Shared.Next(conditions.Length)];
        return new { city, temperature = $"{temp}°F", condition };
    },
    "get_weather",
    "Get the current weather for a city"
);

await using var session = await client.CreateSessionAsync(new SessionConfig
{
    Model = "gpt-4.1",
    OnPermissionRequest = PermissionHandler.ApproveAll,
    Streaming = true,
    Tools = [getWeather],
});

session.On(ev =>
{
    if (ev is AssistantMessageDeltaEvent deltaEvent)
    {
        Console.Write(deltaEvent.Data.DeltaContent);
    }
    if (ev is SessionIdleEvent)
    {
        Console.WriteLine();
    }
});

await session.SendAndWaitAsync(new MessageOptions
{
    Prompt = "What's the weather like in Seattle and Tokyo?",
});
```

</details>

<details>
<summary><strong>Java</strong></summary>

Update `HelloCopilot.java`:

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.events.*;
import com.github.copilot.sdk.json.*;

import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.CompletableFuture;

public class HelloCopilot {
    public static void main(String[] args) throws Exception {
        var random = new Random();
        var conditions = List.of("sunny", "cloudy", "rainy", "partly cloudy");

        // Define a tool that Copilot can call
        var getWeather = ToolDefinition.create(
            "get_weather",
            "Get the current weather for a city",
            Map.of(
                "type", "object",
                "properties", Map.of(
                    "city", Map.of("type", "string", "description", "The city name")
                ),
                "required", List.of("city")
            ),
            invocation -> {
                var city = (String) invocation.getArguments().get("city");
                var temp = random.nextInt(30) + 50;
                var condition = conditions.get(random.nextInt(conditions.size()));
                return CompletableFuture.completedFuture(Map.of(
                    "city", city,
                    "temperature", temp + "°F",
                    "condition", condition
                ));
            }
        );

        try (var client = new CopilotClient()) {
            client.start().get();

            var session = client.createSession(
                new SessionConfig()
                    .setModel("gpt-4.1")
                    .setStreaming(true)
                    .setTools(List.of(getWeather))
                    .setOnPermissionRequest(PermissionHandler.APPROVE_ALL)
            ).get();

            session.on(AssistantMessageDeltaEvent.class, delta -> {
                System.out.print(delta.getData().deltaContent());
            });
            session.on(SessionIdleEvent.class, idle -> {
                System.out.println();
            });

            session.sendAndWait(
                new MessageOptions().setPrompt("What's the weather like in Seattle and Tokyo?")
            ).get();

            client.stop().get();
        }
    }
}
```

</details>

<details>
<summary><strong>Rust</strong></summary>

Update `src/main.rs`:

```rust
use copilot_sdk::{CopilotClient, SessionConfig, MessageOptions, ToolDefinition};
use serde::{Deserialize, Serialize};
use rand::Rng;

#[derive(Deserialize)]
struct WeatherParams {
    city: String,
}

#[derive(Serialize)]
struct WeatherResult {
    city: String,
    temperature: String,
    condition: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Define a tool that Copilot can call
    let get_weather = ToolDefinition::new(
        "get_weather",
        "Get the current weather for a city",
        |params: WeatherParams| async move {
            let mut rng = rand::thread_rng();
            let conditions = ["sunny", "cloudy", "rainy", "partly cloudy"];
            let temp = rng.gen_range(50..80);
            let condition = conditions[rng.gen_range(0..conditions.len())];
            Ok(WeatherResult {
                city: params.city,
                temperature: format!("{}F", temp),
                condition: condition.to_string(),
            })
        },
    );

    let client = CopilotClient::new(None);
    client.start().await?;

    let session = client.create_session(&SessionConfig {
        model: "gpt-4.1".into(),
        streaming: true,
        tools: vec![get_weather],
        ..Default::default()
    }).await?;

    session.on(|event| {
        match event.event_type.as_str() {
            "assistant.message_delta" => print!("{}", event.data.delta_content),
            "session.idle" => println!(),
            _ => {}
        }
    });

    session.send_and_wait(&MessageOptions {
        prompt: "What's the weather like in Seattle and Tokyo?".into(),
        ..Default::default()
    }).await?;

    client.stop().await?;
    Ok(())
}
```

</details>

<details>
<summary><strong>Ruby</strong></summary>

Update `main.rb`:

```ruby
require "copilot_sdk"

# Define a tool that Copilot can call
get_weather = CopilotSDK.define_tool(
  "get_weather",
  description: "Get the current weather for a city",
  parameters: {
    type: "object",
    properties: {
      city: { type: "string", description: "The city name" }
    },
    required: ["city"]
  }
) do |params|
  conditions = ["sunny", "cloudy", "rainy", "partly cloudy"]
  temp = rand(50..80)
  condition = conditions.sample
  { city: params["city"], temperature: "#{temp}F", condition: condition }
end

client = CopilotSDK::Client.new
client.start

session = client.create_session(
  model: "gpt-4.1",
  streaming: true,
  tools: [get_weather]
)

session.on do |event|
  case event.type
  when "assistant.message_delta"
    $stdout.write(event.data.delta_content)
  when "session.idle"
    puts
  end
end

session.send_and_wait(prompt: "What's the weather like in Seattle and Tokyo?")

client.stop
```

</details>

<details>
<summary><strong>PHP</strong></summary>

Update `main.php`:

```php
<?php
require_once 'vendor/autoload.php';

use CopilotSDK\CopilotClient;
use CopilotSDK\ToolDefinition;

// Define a tool that Copilot can call
$getWeather = ToolDefinition::create(
    'get_weather',
    'Get the current weather for a city',
    [
        'type' => 'object',
        'properties' => [
            'city' => ['type' => 'string', 'description' => 'The city name'],
        ],
        'required' => ['city'],
    ],
    function ($params) {
        $conditions = ['sunny', 'cloudy', 'rainy', 'partly cloudy'];
        $temp = rand(50, 80);
        $condition = $conditions[array_rand($conditions)];
        return [
            'city' => $params['city'],
            'temperature' => "{$temp}F",
            'condition' => $condition,
        ];
    }
);

$client = new CopilotClient();
$client->start();

$session = $client->createSession([
    'model' => 'gpt-4.1',
    'streaming' => true,
    'tools' => [$getWeather],
]);

$session->on(function ($event) {
    if ($event->type === 'assistant.message_delta') {
        echo $event->data->deltaContent;
    }
    if ($event->type === 'session.idle') {
        echo "\n";
    }
});

$session->sendAndWait(['prompt' => "What's the weather like in Seattle and Tokyo?"]);

$client->stop();
```

</details>

<details>
<summary><strong>Swift</strong></summary>

Update `main.swift`:

```swift
import CopilotSDK
import Foundation

// Define a tool that Copilot can call
let getWeather = ToolDefinition(
    name: "get_weather",
    description: "Get the current weather for a city",
    parameters: [
        "type": "object",
        "properties": [
            "city": ["type": "string", "description": "The city name"]
        ],
        "required": ["city"]
    ]
) { params in
    let city = params["city"] as! String
    let conditions = ["sunny", "cloudy", "rainy", "partly cloudy"]
    let temp = Int.random(in: 50...80)
    let condition = conditions.randomElement()!
    return ["city": city, "temperature": "\(temp)F", "condition": condition]
}

let client = CopilotClient()
try await client.start()

let session = try await client.createSession(config: SessionConfig(
    model: "gpt-4.1",
    streaming: true,
    tools: [getWeather]
))

session.on { event in
    if let delta = event as? AssistantMessageDeltaEvent {
        print(delta.data.deltaContent, terminator: "")
    }
    if event is SessionIdleEvent {
        print()
    }
}

try await session.sendAndWait(prompt: "What's the weather like in Seattle and Tokyo?")

try await client.stop()
```

</details>

<details>
<summary><strong>Kotlin</strong></summary>

Update `HelloCopilot.kt`:

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.MessageOptions
import com.github.copilot.sdk.ToolDefinition
import com.github.copilot.sdk.events.*

suspend fun main() {
    val conditions = listOf("sunny", "cloudy", "rainy", "partly cloudy")

    // Define a tool that Copilot can call
    val getWeather = ToolDefinition.create(
        name = "get_weather",
        description = "Get the current weather for a city",
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf(
                "city" to mapOf("type" to "string", "description" to "The city name")
            ),
            "required" to listOf("city")
        )
    ) { params ->
        val city = params["city"] as String
        val temp = (50..80).random()
        val condition = conditions.random()
        mapOf("city" to city, "temperature" to "${temp}F", "condition" to condition)
    }

    val client = CopilotClient()
    client.start()

    val session = client.createSession(SessionConfig(
        model = "gpt-4.1",
        streaming = true,
        tools = listOf(getWeather)
    ))

    session.on { event ->
        when (event) {
            is AssistantMessageDeltaEvent -> print(event.data.deltaContent)
            is SessionIdleEvent -> println()
        }
    }

    session.sendAndWait(MessageOptions(
        prompt = "What's the weather like in Seattle and Tokyo?"
    ))

    client.stop()
}
```

</details>

<details>
<summary><strong>C++</strong></summary>

Update `main.cpp`:

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <iostream>
#include <random>

int main() {
    std::mt19937 rng(std::random_device{}());
    std::vector<std::string> conditions = {"sunny", "cloudy", "rainy", "partly cloudy"};

    // Define a tool that Copilot can call
    auto getWeather = copilot::defineTool(
        "get_weather",
        "Get the current weather for a city",
        {{"type", "object"},
         {"properties", {{"city", {{"type", "string"}, {"description", "The city name"}}}}},
         {"required", {"city"}}},
        [&](const auto& params) {
            auto city = params["city"].get<std::string>();
            int temp = std::uniform_int_distribution<>(50, 80)(rng);
            auto condition = conditions[std::uniform_int_distribution<>(0, conditions.size() - 1)(rng)];
            return copilot::json{
                {"city", city}, {"temperature", std::to_string(temp) + "F"}, {"condition", condition}
            };
        }
    );

    copilot::CopilotClient client;
    client.start();

    auto session = client.createSession({
        .model = "gpt-4.1",
        .streaming = true,
        .tools = {getWeather}
    });

    session.on([](const auto& event) {
        if (event.type == "assistant.message_delta") {
            std::cout << event.data.deltaContent;
        }
        if (event.type == "session.idle") {
            std::cout << std::endl;
        }
    });

    session.sendAndWait({.prompt = "What's the weather like in Seattle and Tokyo?"});

    client.stop();
    return 0;
}
```

</details>

Run it and you'll see Copilot call your tool to get weather data, then respond with the results!

## Step 5: Build an Interactive Assistant

Let's put it all together into a useful interactive assistant:

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient, defineTool } from "copilot-sdk-supercharged";
import * as readline from "readline";

const getWeather = defineTool("get_weather", {
    description: "Get the current weather for a city",
    parameters: {
        type: "object",
        properties: {
            city: { type: "string", description: "The city name" },
        },
        required: ["city"],
    },
    handler: async ({ city }) => {
        const conditions = ["sunny", "cloudy", "rainy", "partly cloudy"];
        const temp = Math.floor(Math.random() * 30) + 50;
        const condition = conditions[Math.floor(Math.random() * conditions.length)];
        return { city, temperature: `${temp}°F`, condition };
    },
});

const client = new CopilotClient();
const session = await client.createSession({
    model: "gpt-4.1",
    streaming: true,
    tools: [getWeather],
});

session.on("assistant.message_delta", (event) => {
    process.stdout.write(event.data.deltaContent);
});

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
});

console.log("🌤️  Weather Assistant (type 'exit' to quit)");
console.log("   Try: 'What's the weather in Paris?'\n");

const prompt = () => {
    rl.question("You: ", async (input) => {
        if (input.toLowerCase() === "exit") {
            await client.stop();
            rl.close();
            return;
        }

        process.stdout.write("Assistant: ");
        await session.sendAndWait({ prompt: input });
        console.log("\n");
        prompt();
    });
};

prompt();
```

Run with:

```bash
npx tsx weather-assistant.ts
```

</details>

<details>
<summary><strong>Python</strong></summary>

Create `weather_assistant.py`:

```python
import asyncio
import random
import sys
from copilot import CopilotClient
from copilot.session import PermissionHandler
from copilot.tools import define_tool
from copilot.generated.session_events import SessionEventType
from pydantic import BaseModel, Field

class GetWeatherParams(BaseModel):
    city: str = Field(description="The name of the city to get weather for")

@define_tool(description="Get the current weather for a city")
async def get_weather(params: GetWeatherParams) -> dict:
    city = params.city
    conditions = ["sunny", "cloudy", "rainy", "partly cloudy"]
    temp = random.randint(50, 80)
    condition = random.choice(conditions)
    return {"city": city, "temperature": f"{temp}°F", "condition": condition}

async def main():
    client = CopilotClient()
    await client.start()

    session = await client.create_session(on_permission_request=PermissionHandler.approve_all, model="gpt-4.1", streaming=True, tools=[get_weather])

    def handle_event(event):
        if event.type == SessionEventType.ASSISTANT_MESSAGE_DELTA:
            sys.stdout.write(event.data.delta_content)
            sys.stdout.flush()

    session.on(handle_event)

    print("🌤️  Weather Assistant (type 'exit' to quit)")
    print("   Try: 'What's the weather in Paris?' or 'Compare weather in NYC and LA'\n")

    while True:
        try:
            user_input = input("You: ")
        except EOFError:
            break

        if user_input.lower() == "exit":
            break

        sys.stdout.write("Assistant: ")
        await session.send_and_wait(user_input)
        print("\n")

    await client.stop()

asyncio.run(main())
```

Run with:

```bash
python weather_assistant.py
```

</details>

<details>
<summary><strong>Go</strong></summary>

Create `weather-assistant.go`:

```go
package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"math/rand"
	"os"
	"strings"

	copilot "github.com/github/copilot-sdk/go"
)

type WeatherParams struct {
	City string `json:"city" jsonschema:"The city name"`
}

type WeatherResult struct {
	City        string `json:"city"`
	Temperature string `json:"temperature"`
	Condition   string `json:"condition"`
}

func main() {
	ctx := context.Background()

	getWeather := copilot.DefineTool(
		"get_weather",
		"Get the current weather for a city",
		func(params WeatherParams, inv copilot.ToolInvocation) (WeatherResult, error) {
			conditions := []string{"sunny", "cloudy", "rainy", "partly cloudy"}
			temp := rand.Intn(30) + 50
			condition := conditions[rand.Intn(len(conditions))]
			return WeatherResult{
				City:        params.City,
				Temperature: fmt.Sprintf("%d°F", temp),
				Condition:   condition,
			}, nil
		},
	)

	client := copilot.NewClient(nil)
	if err := client.Start(ctx); err != nil {
		log.Fatal(err)
	}
	defer client.Stop()

	session, err := client.CreateSession(ctx, &copilot.SessionConfig{
		Model:     "gpt-4.1",
		Streaming: true,
		Tools:     []copilot.Tool{getWeather},
	})
	if err != nil {
		log.Fatal(err)
	}

	session.On(func(event copilot.SessionEvent) {
		switch d := event.Data.(type) {
		case *copilot.AssistantMessageDeltaData:
			fmt.Print(d.DeltaContent)
		case *copilot.SessionIdleData:
			_ = d
			fmt.Println()
		}
	})

	fmt.Println("🌤️  Weather Assistant (type 'exit' to quit)")
	fmt.Println("   Try: 'What's the weather in Paris?' or 'Compare weather in NYC and LA'\n")

	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("You: ")
		if !scanner.Scan() {
			break
		}
		input := scanner.Text()
		if strings.ToLower(input) == "exit" {
			break
		}

		fmt.Print("Assistant: ")
		_, err = session.SendAndWait(ctx, copilot.MessageOptions{Prompt: input})
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			break
		}
		fmt.Println()
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Input error: %v\n", err)
	}
}
```

Run with:

```bash
go run weather-assistant.go
```

</details>

<details>
<summary><strong>.NET</strong></summary>

Create a new console project and update `Program.cs`:

```csharp
using GitHub.Copilot.SDK;
using Microsoft.Extensions.AI;
using System.ComponentModel;

// Define the weather tool using AIFunctionFactory
var getWeather = AIFunctionFactory.Create(
    ([Description("The city name")] string city) =>
    {
        var conditions = new[] { "sunny", "cloudy", "rainy", "partly cloudy" };
        var temp = Random.Shared.Next(50, 80);
        var condition = conditions[Random.Shared.Next(conditions.Length)];
        return new { city, temperature = $"{temp}°F", condition };
    },
    "get_weather",
    "Get the current weather for a city");

await using var client = new CopilotClient();
await using var session = await client.CreateSessionAsync(new SessionConfig
{
    Model = "gpt-4.1",
    OnPermissionRequest = PermissionHandler.ApproveAll,
    Streaming = true,
    Tools = [getWeather]
});

// Listen for response chunks
session.On(ev =>
{
    if (ev is AssistantMessageDeltaEvent deltaEvent)
    {
        Console.Write(deltaEvent.Data.DeltaContent);
    }
    if (ev is SessionIdleEvent)
    {
        Console.WriteLine();
    }
});

Console.WriteLine("🌤️  Weather Assistant (type 'exit' to quit)");
Console.WriteLine("   Try: 'What's the weather in Paris?' or 'Compare weather in NYC and LA'\n");

while (true)
{
    Console.Write("You: ");
    var input = Console.ReadLine();

    if (string.IsNullOrEmpty(input) || input.Equals("exit", StringComparison.OrdinalIgnoreCase))
    {
        break;
    }

    Console.Write("Assistant: ");
    await session.SendAndWaitAsync(new MessageOptions { Prompt = input });
    Console.WriteLine("\n");
}
```

Run with:

```bash
dotnet run
```

</details>

<details>
<summary><strong>Java</strong></summary>

Create `WeatherAssistant.java`:

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.events.*;
import com.github.copilot.sdk.json.*;

import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.CompletableFuture;

public class WeatherAssistant {
    public static void main(String[] args) throws Exception {
        var random = new Random();
        var conditions = List.of("sunny", "cloudy", "rainy", "partly cloudy");

        var getWeather = ToolDefinition.create(
            "get_weather",
            "Get the current weather for a city",
            Map.of(
                "type", "object",
                "properties", Map.of(
                    "city", Map.of("type", "string", "description", "The city name")
                ),
                "required", List.of("city")
            ),
            invocation -> {
                var city = (String) invocation.getArguments().get("city");
                var temp = random.nextInt(30) + 50;
                var condition = conditions.get(random.nextInt(conditions.size()));
                return CompletableFuture.completedFuture(Map.of(
                    "city", city,
                    "temperature", temp + "°F",
                    "condition", condition
                ));
            }
        );

        try (var client = new CopilotClient()) {
            client.start().get();

            var session = client.createSession(
                new SessionConfig()
                    .setModel("gpt-4.1")
                    .setStreaming(true)
                    .setOnPermissionRequest(request ->
                        CompletableFuture.completedFuture(PermissionDecision.allow())
                    )
                    .setTools(List.of(getWeather))
            ).get();

            session.on(AssistantMessageDeltaEvent.class, delta -> {
                System.out.print(delta.getData().deltaContent());
            });
            session.on(SessionIdleEvent.class, idle -> {
                System.out.println();
            });

            System.out.println("🌤️  Weather Assistant (type 'exit' to quit)");
            System.out.println("   Try: 'What's the weather in Paris?' or 'Compare weather in NYC and LA'\n");

            var scanner = new Scanner(System.in);
            while (true) {
                System.out.print("You: ");
                if (!scanner.hasNextLine()) break;
                var input = scanner.nextLine();
                if (input.equalsIgnoreCase("exit")) break;

                System.out.print("Assistant: ");
                session.sendAndWait(
                    new MessageOptions().setPrompt(input)
                ).get();
                System.out.println("\n");
            }

            client.stop().get();
        }
    }
}
```

Run with:

```bash
javac -cp copilot-sdk.jar WeatherAssistant.java && java -cp .:copilot-sdk.jar WeatherAssistant
```

</details>

<details>
<summary><strong>Rust</strong></summary>

Create `src/main.rs`:

```rust
use copilot_sdk::{CopilotClient, SessionConfig, MessageOptions, ToolDefinition};
use serde::{Deserialize, Serialize};
use rand::Rng;
use std::io::{self, BufRead, Write};

#[derive(Deserialize)]
struct WeatherParams {
    city: String,
}

#[derive(Serialize)]
struct WeatherResult {
    city: String,
    temperature: String,
    condition: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let get_weather = ToolDefinition::new(
        "get_weather",
        "Get the current weather for a city",
        |params: WeatherParams| async move {
            let mut rng = rand::thread_rng();
            let conditions = ["sunny", "cloudy", "rainy", "partly cloudy"];
            let temp = rng.gen_range(50..80);
            let condition = conditions[rng.gen_range(0..conditions.len())];
            Ok(WeatherResult {
                city: params.city,
                temperature: format!("{}F", temp),
                condition: condition.to_string(),
            })
        },
    );

    let client = CopilotClient::new(None);
    client.start().await?;

    let session = client.create_session(&SessionConfig {
        model: "gpt-4.1".into(),
        streaming: true,
        tools: vec![get_weather],
        ..Default::default()
    }).await?;

    session.on(|event| {
        match event.event_type.as_str() {
            "assistant.message_delta" => print!("{}", event.data.delta_content),
            "session.idle" => println!(),
            _ => {}
        }
    });

    println!("Weather Assistant (type 'exit' to quit)");
    println!("   Try: 'What's the weather in Paris?'\n");

    let stdin = io::stdin();
    loop {
        print!("You: ");
        io::stdout().flush()?;

        let mut input = String::new();
        if stdin.lock().read_line(&mut input)? == 0 {
            break;
        }
        let input = input.trim();
        if input.eq_ignore_ascii_case("exit") {
            break;
        }

        print!("Assistant: ");
        session.send_and_wait(&MessageOptions {
            prompt: input.to_string(),
            ..Default::default()
        }).await?;
        println!("\n");
    }

    client.stop().await?;
    Ok(())
}
```

Run with:

```bash
cargo run
```

</details>

<details>
<summary><strong>Ruby</strong></summary>

Create `weather_assistant.rb`:

```ruby
require "copilot_sdk"

get_weather = CopilotSDK.define_tool(
  "get_weather",
  description: "Get the current weather for a city",
  parameters: {
    type: "object",
    properties: {
      city: { type: "string", description: "The city name" }
    },
    required: ["city"]
  }
) do |params|
  conditions = ["sunny", "cloudy", "rainy", "partly cloudy"]
  temp = rand(50..80)
  condition = conditions.sample
  { city: params["city"], temperature: "#{temp}F", condition: condition }
end

client = CopilotSDK::Client.new
client.start

session = client.create_session(
  model: "gpt-4.1",
  streaming: true,
  tools: [get_weather]
)

session.on do |event|
  case event.type
  when "assistant.message_delta"
    $stdout.write(event.data.delta_content)
  when "session.idle"
    puts
  end
end

puts "Weather Assistant (type 'exit' to quit)"
puts "   Try: 'What's the weather in Paris?'\n\n"

loop do
  print "You: "
  input = $stdin.gets&.chomp
  break if input.nil? || input.downcase == "exit"

  $stdout.write "Assistant: "
  session.send_and_wait(prompt: input)
  puts "\n"
end

client.stop
```

Run with:

```bash
ruby weather_assistant.rb
```

</details>

<details>
<summary><strong>PHP</strong></summary>

Create `weather_assistant.php`:

```php
<?php
require_once 'vendor/autoload.php';

use CopilotSDK\CopilotClient;
use CopilotSDK\ToolDefinition;

$getWeather = ToolDefinition::create(
    'get_weather',
    'Get the current weather for a city',
    [
        'type' => 'object',
        'properties' => [
            'city' => ['type' => 'string', 'description' => 'The city name'],
        ],
        'required' => ['city'],
    ],
    function ($params) {
        $conditions = ['sunny', 'cloudy', 'rainy', 'partly cloudy'];
        $temp = rand(50, 80);
        $condition = $conditions[array_rand($conditions)];
        return [
            'city' => $params['city'],
            'temperature' => "{$temp}F",
            'condition' => $condition,
        ];
    }
);

$client = new CopilotClient();
$client->start();

$session = $client->createSession([
    'model' => 'gpt-4.1',
    'streaming' => true,
    'tools' => [$getWeather],
]);

$session->on(function ($event) {
    if ($event->type === 'assistant.message_delta') {
        echo $event->data->deltaContent;
    }
    if ($event->type === 'session.idle') {
        echo "\n";
    }
});

echo "Weather Assistant (type 'exit' to quit)\n";
echo "   Try: 'What's the weather in Paris?'\n\n";

while (true) {
    echo "You: ";
    $input = trim(fgets(STDIN));
    if ($input === false || strtolower($input) === 'exit') {
        break;
    }

    echo "Assistant: ";
    $session->sendAndWait(['prompt' => $input]);
    echo "\n\n";
}

$client->stop();
```

Run with:

```bash
php weather_assistant.php
```

</details>

<details>
<summary><strong>Swift</strong></summary>

Create `WeatherAssistant.swift`:

```swift
import CopilotSDK
import Foundation

let getWeather = ToolDefinition(
    name: "get_weather",
    description: "Get the current weather for a city",
    parameters: [
        "type": "object",
        "properties": [
            "city": ["type": "string", "description": "The city name"]
        ],
        "required": ["city"]
    ]
) { params in
    let city = params["city"] as! String
    let conditions = ["sunny", "cloudy", "rainy", "partly cloudy"]
    let temp = Int.random(in: 50...80)
    let condition = conditions.randomElement()!
    return ["city": city, "temperature": "\(temp)F", "condition": condition]
}

let client = CopilotClient()
try await client.start()

let session = try await client.createSession(config: SessionConfig(
    model: "gpt-4.1",
    streaming: true,
    tools: [getWeather]
))

session.on { event in
    if let delta = event as? AssistantMessageDeltaEvent {
        print(delta.data.deltaContent, terminator: "")
    }
    if event is SessionIdleEvent {
        print()
    }
}

print("Weather Assistant (type 'exit' to quit)")
print("   Try: 'What's the weather in Paris?'\n")

while true {
    print("You: ", terminator: "")
    guard let input = readLine(), !input.isEmpty else { break }
    if input.lowercased() == "exit" { break }

    print("Assistant: ", terminator: "")
    try await session.sendAndWait(prompt: input)
    print("\n")
}

try await client.stop()
```

Run with:

```bash
swift run
```

</details>

<details>
<summary><strong>Kotlin</strong></summary>

Create `WeatherAssistant.kt`:

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.MessageOptions
import com.github.copilot.sdk.ToolDefinition
import com.github.copilot.sdk.events.*

suspend fun main() {
    val conditions = listOf("sunny", "cloudy", "rainy", "partly cloudy")

    val getWeather = ToolDefinition.create(
        name = "get_weather",
        description = "Get the current weather for a city",
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf(
                "city" to mapOf("type" to "string", "description" to "The city name")
            ),
            "required" to listOf("city")
        )
    ) { params ->
        val city = params["city"] as String
        val temp = (50..80).random()
        val condition = conditions.random()
        mapOf("city" to city, "temperature" to "${temp}F", "condition" to condition)
    }

    val client = CopilotClient()
    client.start()

    val session = client.createSession(SessionConfig(
        model = "gpt-4.1",
        streaming = true,
        tools = listOf(getWeather)
    ))

    session.on { event ->
        when (event) {
            is AssistantMessageDeltaEvent -> print(event.data.deltaContent)
            is SessionIdleEvent -> println()
        }
    }

    println("Weather Assistant (type 'exit' to quit)")
    println("   Try: 'What's the weather in Paris?'\n")

    while (true) {
        print("You: ")
        val input = readLine() ?: break
        if (input.equals("exit", ignoreCase = true)) break

        print("Assistant: ")
        session.sendAndWait(MessageOptions(prompt = input))
        println("\n")
    }

    client.stop()
}
```

Run with:

```bash
kotlinc -cp copilot-sdk.jar WeatherAssistant.kt -include-runtime -d weather.jar && java -jar weather.jar
```

</details>

<details>
<summary><strong>C++</strong></summary>

Create `weather_assistant.cpp`:

```cpp
#include <copilot/client.h>
#include <copilot/tools.h>
#include <iostream>
#include <random>
#include <string>

int main() {
    std::mt19937 rng(std::random_device{}());
    std::vector<std::string> conditions = {"sunny", "cloudy", "rainy", "partly cloudy"};

    auto getWeather = copilot::defineTool(
        "get_weather",
        "Get the current weather for a city",
        {{"type", "object"},
         {"properties", {{"city", {{"type", "string"}, {"description", "The city name"}}}}},
         {"required", {"city"}}},
        [&](const auto& params) {
            auto city = params["city"].get<std::string>();
            int temp = std::uniform_int_distribution<>(50, 80)(rng);
            auto condition = conditions[std::uniform_int_distribution<>(0, conditions.size() - 1)(rng)];
            return copilot::json{
                {"city", city}, {"temperature", std::to_string(temp) + "F"}, {"condition", condition}
            };
        }
    );

    copilot::CopilotClient client;
    client.start();

    auto session = client.createSession({
        .model = "gpt-4.1",
        .streaming = true,
        .tools = {getWeather}
    });

    session.on([](const auto& event) {
        if (event.type == "assistant.message_delta") {
            std::cout << event.data.deltaContent;
        }
        if (event.type == "session.idle") {
            std::cout << std::endl;
        }
    });

    std::cout << "Weather Assistant (type 'exit' to quit)" << std::endl;
    std::cout << "   Try: 'What's the weather in Paris?'\n" << std::endl;

    std::string input;
    while (true) {
        std::cout << "You: ";
        if (!std::getline(std::cin, input) || input == "exit") {
            break;
        }

        std::cout << "Assistant: ";
        session.sendAndWait({.prompt = input});
        std::cout << "\n" << std::endl;
    }

    client.stop();
    return 0;
}
```

Build and run:

```bash
cmake -B build && cmake --build build && ./build/weather_assistant
```

</details>


**Example session:**

```
🌤️  Weather Assistant (type 'exit' to quit)
   Try: 'What's the weather in Paris?' or 'Compare weather in NYC and LA'

You: What's the weather in Seattle?
Assistant: Let me check the weather for Seattle...
It's currently 62°F and cloudy in Seattle.

You: How about Tokyo and London?
Assistant: I'll check both cities for you:
- Tokyo: 75°F and sunny
- London: 58°F and rainy

You: exit
```

You've built an assistant with a custom tool that Copilot can call!

---

## How Tools Work

When you define a tool, you're telling Copilot:
1. **What the tool does** (description)
2. **What parameters it needs** (schema)
3. **What code to run** (handler)

Copilot decides when to call your tool based on the user's question. When it does:
1. Copilot sends a tool call request with the parameters
2. The SDK runs your handler function
3. The result is sent back to Copilot
4. Copilot incorporates the result into its response

---

## What's Next?

Now that you've got the basics, here are more powerful features to explore:

### Connect to MCP Servers

MCP (Model Context Protocol) servers provide pre-built tools. Connect to GitHub's MCP server to give Copilot access to repositories, issues, and pull requests:

```typescript
const session = await client.createSession({
    mcpServers: {
        github: {
            type: "http",
            url: "https://api.githubcopilot.com/mcp/",
        },
    },
});
```

📖 **[Full MCP documentation →](./features/mcp.md)** - Learn about local vs remote servers, all configuration options, and troubleshooting.

### Create Custom Agents

Define specialized AI personas for specific tasks:

```typescript
const session = await client.createSession({
    customAgents: [{
        name: "pr-reviewer",
        displayName: "PR Reviewer",
        description: "Reviews pull requests for best practices",
        prompt: "You are an expert code reviewer. Focus on security, performance, and maintainability.",
    }],
});
```

> **Tip:** You can also set `agent: "pr-reviewer"` in the session config to pre-select this agent from the start. See the [Custom Agents guide](./features/custom-agents.md#selecting-an-agent-at-session-creation) for details.

### Customize the System Message

Control the AI's behavior and personality by appending instructions:

```typescript
const session = await client.createSession({
    systemMessage: {
        content: "You are a helpful assistant for our engineering team. Always be concise.",
    },
});
```

For more fine-grained control, use `mode: "customize"` to override individual sections of the system prompt while preserving the rest:

```typescript
const session = await client.createSession({
    systemMessage: {
        mode: "customize",
        sections: {
            tone: { action: "replace", content: "Respond in a warm, professional tone. Be thorough in explanations." },
            code_change_rules: { action: "remove" },
            guidelines: { action: "append", content: "\n* Always cite data sources" },
        },
        content: "Focus on financial analysis and reporting.",
    },
});
```

Available section IDs: `identity`, `tone`, `tool_efficiency`, `environment_context`, `code_change_rules`, `guidelines`, `safety`, `tool_instructions`, `custom_instructions`, `last_instructions`.

Each override supports four actions: `replace`, `remove`, `append`, and `prepend`. Unknown section IDs are handled gracefully — content is appended to additional instructions and a warning is emitted; `remove` on unknown sections is silently ignored.

See the language-specific SDK READMEs for examples in [TypeScript](../nodejs/README.md), [Python](../python/README.md), [Go](../go/README.md), [Java](../java/README.md), and [C#](../dotnet/README.md).

---

## Connecting to an External CLI Server

By default, the SDK automatically manages the Copilot CLI process lifecycle, starting and stopping the CLI as needed. However, you can also run the CLI in server mode separately and have the SDK connect to it. This can be useful for:

- **Debugging**: Keep the CLI running between SDK restarts to inspect logs
- **Resource sharing**: Multiple SDK clients can connect to the same CLI server
- **Development**: Run the CLI with custom settings or in a different environment

### Running the CLI in Server Mode

Start the CLI in server mode using the `--headless` flag and optionally specify a port:

```bash
copilot --headless --port 4321
```

If you don't specify a port, the CLI will choose a random available port.

### Connecting the SDK to the External Server

Once the CLI is running in server mode, configure your SDK client to connect to it using the "cli url" option:

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

```typescript
import { CopilotClient } from "copilot-sdk-supercharged";

const client = new CopilotClient({
    cliUrl: "localhost:4321"
});

// Use the client normally
const session = await client.createSession({ onPermissionRequest: approveAll });
// ...
```

</details>

<details>
<summary><strong>Python</strong></summary>

```python
from copilot import CopilotClient
from copilot.session import PermissionHandler

client = CopilotClient({
    "cli_url": "localhost:4321"
})
await client.start()

# Use the client normally
session = await client.create_session(on_permission_request=PermissionHandler.approve_all)
# ...
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

	copilot "github.com/github/copilot-sdk/go"
)

func main() {
	ctx := context.Background()

	client := copilot.NewClient(&copilot.ClientOptions{
		CLIUrl: "localhost:4321",
	})

	if err := client.Start(ctx); err != nil {
		log.Fatal(err)
	}
	defer client.Stop()

	// Use the client normally
	_, _ = client.CreateSession(ctx, &copilot.SessionConfig{
		OnPermissionRequest: copilot.PermissionHandler.ApproveAll,
	})
}
```
<!-- /docs-validate: hidden -->

```go
import copilot "github.com/github/copilot-sdk/go"

client := copilot.NewClient(&copilot.ClientOptions{
    CLIUrl: "localhost:4321",
})

if err := client.Start(ctx); err != nil {
    log.Fatal(err)
}
defer client.Stop()

// Use the client normally
session, err := client.CreateSession(ctx, &copilot.SessionConfig{
    OnPermissionRequest: copilot.PermissionHandler.ApproveAll,
})
// ...
```

</details>

<details>
<summary><strong>.NET</strong></summary>

```csharp
using GitHub.Copilot.SDK;

using var client = new CopilotClient(new CopilotClientOptions
{
    CliUrl = "localhost:4321",
    UseStdio = false
});

// Use the client normally
await using var session = await client.CreateSessionAsync(new()
{
    OnPermissionRequest = PermissionHandler.ApproveAll
});
// ...
```

</details>

<details>
<summary><strong>Java</strong></summary>

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.json.*;

var client = new CopilotClient(
    new CopilotClientOptions().setCliUrl("localhost:4321")
);
client.start().get();

// Use the client normally
var session = client.createSession(
    new SessionConfig().setOnPermissionRequest(PermissionHandler.APPROVE_ALL)
).get();
// ...
```

</details>

**Note:** When `cli_url` / `cliUrl` / `CLIUrl` is provided, the SDK will not spawn or manage a CLI process - it will only connect to the existing server at the specified URL.

---

## Telemetry & Observability

The Copilot SDK supports [OpenTelemetry](https://opentelemetry.io/) for distributed tracing. Provide a `telemetry` configuration to the client to enable trace export from the CLI process and automatic [W3C Trace Context](https://www.w3.org/TR/trace-context/) propagation between the SDK and CLI.

### Enabling Telemetry

Pass a `telemetry` (or `Telemetry`) config when creating the client. This is the opt-in — no separate "enabled" flag is needed.

<details open>
<summary><strong>Node.js / TypeScript</strong></summary>

<!-- docs-validate: skip -->
```typescript
import { CopilotClient } from "@github/copilot-sdk";

const client = new CopilotClient({
  telemetry: {
    otlpEndpoint: "http://localhost:4318",
  },
});
```

Optional peer dependency: `@opentelemetry/api`

</details>

<details>
<summary><strong>Python</strong></summary>

<!-- docs-validate: skip -->
```python
from copilot import CopilotClient, SubprocessConfig

client = CopilotClient(SubprocessConfig(
    telemetry={
        "otlp_endpoint": "http://localhost:4318",
    },
))
```

Install with telemetry extras: `pip install copilot-sdk[telemetry]` (provides `opentelemetry-api`)

</details>

<details>
<summary><strong>Go</strong></summary>

<!-- docs-validate: skip -->
```go
client, err := copilot.NewClient(copilot.ClientOptions{
    Telemetry: &copilot.TelemetryConfig{
        OTLPEndpoint: "http://localhost:4318",
    },
})
```

Dependency: `go.opentelemetry.io/otel`

</details>

<details>
<summary><strong>.NET</strong></summary>

<!-- docs-validate: skip -->
```csharp
var client = new CopilotClient(new CopilotClientOptions
{
    Telemetry = new TelemetryConfig
    {
        OtlpEndpoint = "http://localhost:4318",
    },
});
```

No extra dependencies — uses built-in `System.Diagnostics.Activity`.

</details>

<details>
<summary><strong>Java</strong></summary>

<!-- docs-validate: skip -->
```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.json.*;

var client = new CopilotClient(new CopilotClientOptions()
    .setTelemetry(new TelemetryConfig()
        .setOtlpEndpoint("http://localhost:4318")));
```

Dependency: `io.opentelemetry:opentelemetry-api`

</details>

### TelemetryConfig Options

| Option | Node.js | Python | Go | Java | .NET | Description |
|---|---|---|---|---|---|---|
| OTLP endpoint | `otlpEndpoint` | `otlp_endpoint` | `OTLPEndpoint` | `otlpEndpoint` | `OtlpEndpoint` | OTLP HTTP endpoint URL |
| File path | `filePath` | `file_path` | `FilePath` | `filePath` | `FilePath` | File path for JSON-lines trace output |
| Exporter type | `exporterType` | `exporter_type` | `ExporterType` | `exporterType` | `ExporterType` | `"otlp-http"` or `"file"` |
| Source name | `sourceName` | `source_name` | `SourceName` | `sourceName` | `SourceName` | Instrumentation scope name |
| Capture content | `captureContent` | `capture_content` | `CaptureContent` | `captureContent` | `CaptureContent` | Whether to capture message content |

### File Export

To write traces to a local file instead of an OTLP endpoint:

<!-- docs-validate: skip -->
```typescript
const client = new CopilotClient({
  telemetry: {
    filePath: "./traces.jsonl",
    exporterType: "file",
  },
});
```

### Trace Context Propagation

Trace context is propagated automatically — no manual instrumentation is needed:

- **SDK → CLI**: `traceparent` and `tracestate` headers from the current span/activity are included in `session.create`, `session.resume`, and `session.send` RPC calls.
- **CLI → SDK**: When the CLI invokes tool handlers, the trace context from the CLI's span is propagated so your tool code runs under the correct parent span.

📖 **[OpenTelemetry Instrumentation Guide →](./observability/opentelemetry.md)** — TelemetryConfig options, trace context propagation, and per-language dependencies.

---

## Learn More

### All SDK References
- [Node.js](../nodejs/README.md) | [Python](../python/README.md) | [Go](../go/README.md) | [.NET](../dotnet/README.md) | [Java](../java/README.md) | [Rust](../rust/README.md) | [Ruby](../ruby/README.md)
- [PHP](../php/README.md) | [Swift](../swift/README.md) | [Kotlin](../kotlin/README.md) | [C++](../cpp/README.md) | [C](../c/README.md) | [Dart](../dart/README.md) | [Scala](../scala/README.md)
- [R](../r/README.md) | [Perl](../perl/README.md) | [Lua](../lua/README.md) | [Shell/Bash](../shell/README.md) | [Elixir](../elixir/README.md) | [Haskell](../haskell/README.md) | [Clojure](../clojure/README.md)

### Guides
- [Authentication Guide](./auth/index.md) - GitHub OAuth, environment variables, and BYOK
- [BYOK (Bring Your Own Key)](./auth/byok.md) - Use your own API keys from Azure AI Foundry, OpenAI, etc.
- [Using MCP Servers](./features/mcp.md) - Integrate external tools via Model Context Protocol
- [GitHub MCP Server Documentation](https://github.com/github/github-mcp-server)
- [MCP Servers Directory](https://github.com/modelcontextprotocol/servers) - Explore more MCP servers
- [Multi-Language SDK Documentation](../MULTI_LANGUAGE_SDKS.md) - Architecture and protocol details
- [OpenTelemetry Instrumentation](./observability/opentelemetry.md) - TelemetryConfig, trace context propagation, and per-language dependencies

---

**You did it!** You've learned the core concepts of the GitHub Copilot SDK:
- Creating a client and session
- Sending messages and receiving responses
- Streaming for real-time output
- Defining custom tools that Copilot can call

Now go build something amazing in any of our **20 supported languages**!

# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Java.

## Basic Error Handling

Wrap SDK calls in try-catch blocks to handle common failure modes.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.CopilotSession;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

CopilotClient client = new CopilotClient();

try {
    client.start();
    CopilotSession session = client.createSession(new SessionConfig());
    var response = session.sendAndWait(new SendOptions("Explain Java streams"));
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("SDK error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Handling Specific Error Types

Differentiate between connection errors, timeout errors, and request failures.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.CopilotSession;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import java.net.ConnectException;
import java.util.concurrent.TimeoutException;

CopilotClient client = new CopilotClient();

try {
    client.start();
    CopilotSession session = client.createSession(new SessionConfig());
    var response = session.sendAndWait(new SendOptions("Hello"));
    System.out.println(response.getMessage());
} catch (ConnectException e) {
    System.err.println("Connection failed. Is the Copilot CLI running? " + e.getMessage());
} catch (TimeoutException e) {
    System.err.println("Request timed out: " + e.getMessage());
} catch (Exception e) {
    System.err.println("Unexpected error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Timeout Handling

Set a timeout on requests to prevent indefinite blocking.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import java.time.Duration;

CopilotClient client = new CopilotClient();

try {
    client.start();
    var session = client.createSession(new SessionConfig());

    SendOptions options = new SendOptions("Summarize this large codebase");
    options.setTimeout(Duration.ofSeconds(30));

    var response = session.sendAndWait(options);
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Aborting a Request

Cancel a long-running request using an abort signal.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CancellationException;

CopilotClient client = new CopilotClient();

try {
    client.start();
    var session = client.createSession(new SessionConfig());

    SendOptions options = new SendOptions("Write a long essay");
    CompletableFuture<?> future = session.sendAsync(options);

    // Cancel after 5 seconds
    Thread.sleep(5000);
    future.cancel(true);
} catch (CancellationException e) {
    System.out.println("Request was cancelled.");
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Graceful Shutdown

Always stop the client in a shutdown hook so resources are released.

```java
import com.github.copilot.sdk.CopilotClient;

CopilotClient client = new CopilotClient();

Runtime.getRuntime().addShutdownHook(new Thread(() -> {
    System.out.println("Shutting down...");
    client.stop();
}));

try {
    client.start();
    var session = client.createSession(new com.github.copilot.sdk.SessionConfig());
    var response = session.sendAndWait(
        new com.github.copilot.sdk.SendOptions("Hello from Java")
    );
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
}
```

## Best Practices

- Always call `client.stop()` in a `finally` block or shutdown hook to clean up resources.
- Catch specific exception types before catching the generic `Exception` class.
- Use timeouts on all production requests to prevent indefinite hangs.
- Log errors with enough context (session ID, prompt snippet) for debugging.
- Use `CompletableFuture.cancel()` for aborting in-flight requests.

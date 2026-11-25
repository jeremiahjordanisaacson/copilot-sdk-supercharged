# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Java.

## Basic Error Handling

`start()`, `createSession()`, and `sendAndWait()` throw checked exceptions. `stop()` doesn't throw: it
returns a list of cleanup errors, so it's safe to call in a `finally` block.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class BasicErrorHandling {
    public static void main(String[] args) {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            // Returns the last assistant.message event of the turn, or null if there was none
            SessionEvent reply = session.sendAndWait(new MessageOptions("Explain Java streams"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } catch (Exception e) {
            System.err.println("SDK error: " + e.getMessage());
        } finally {
            client.stop();
        }
    }
}
```

## Handling Specific Error Types

| Failure | What the SDK throws |
| --- | --- |
| The CLI can't be launched (not installed, wrong `COPILOT_CLI_PATH`) | `IOException` from `start()` |
| The server at `cliUrl` isn't reachable | `java.net.ConnectException` (an `IOException`) |
| The CLI rejects a request | `ExecutionException` whose cause is `com.github.copilot.internal.JsonRpcClient.JsonRpcException`, which carries the JSON-RPC error `code` |
| The CLI doesn't answer a request within 30 seconds | `java.util.concurrent.TimeoutException` |
| `sendAndWait` times out, or the session reports `session.error` | `RuntimeException` |

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import com.github.copilot.internal.JsonRpcClient.JsonRpcException;
import java.io.IOException;
import java.net.ConnectException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

public class SpecificErrors {
    public static void main(String[] args) {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            SessionEvent reply = session.sendAndWait(new MessageOptions("Hello"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } catch (ConnectException e) {
            System.err.println("Could not reach the CLI server: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Could not start the Copilot CLI. Is it on PATH or in COPILOT_CLI_PATH? " + e.getMessage());
        } catch (ExecutionException e) {
            if (e.getCause() instanceof JsonRpcException rpcError) {
                System.err.println("The CLI rejected the request (code " + rpcError.code + "): " + rpcError.getMessage());
            } else {
                System.err.println("Request failed: " + e.getCause());
            }
        } catch (TimeoutException e) {
            System.err.println("The CLI did not answer in time: " + e.getMessage());
        } catch (RuntimeException e) {
            System.err.println("The turn failed or timed out: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("Unexpected error: " + e);
        } finally {
            client.stop();
        }
    }
}
```

## Timeout Handling

`sendAndWait` waits 60 seconds for the session to go idle. Pass a timeout in milliseconds to change
that. When the wait times out, the turn can still be running in the CLI, so abort it.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class TimeoutHandling {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            try {
                SessionEvent reply = session.sendAndWait(new MessageOptions("Summarize this repository"), 30_000);
                if (reply != null) {
                    System.out.println(reply.data.get("content"));
                }
            } catch (RuntimeException e) {
                System.err.println(e.getMessage());
                session.abort(); // stop the turn if it is still running
            }
        } finally {
            client.stop();
        }
    }
}
```

## Aborting a Request

`send()` returns as soon as the message is queued. Call `abort()` to stop the turn; the session
then goes idle and accepts new messages.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class AbortRequest {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            CountDownLatch idle = new CountDownLatch(1);
            session.on("abort", event -> System.out.println("Aborted: " + event.data.get("reason")));
            session.on("session.idle", event -> idle.countDown());

            session.send(new MessageOptions("Write a long essay about the history of the JVM"));

            Thread.sleep(5_000);
            session.abort();

            if (!idle.await(30, TimeUnit.SECONDS)) {
                System.err.println("The session did not go idle after the abort");
            }
        } finally {
            client.stop();
        }
    }
}
```

## Graceful Shutdown

Stop the client from a shutdown hook so the CLI process it started is cleaned up, even when the
program is interrupted.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class GracefulShutdown {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();

        // Runs on normal exit, System.exit, and Ctrl+C
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("Shutting down...");
            client.stop();
        }));

        client.start();
        CopilotSession session = client.createSession(new SessionConfig()
            .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

        SessionEvent reply = session.sendAndWait(new MessageOptions("Hello from Java"));
        if (reply != null) {
            System.out.println(reply.data.get("content"));
        }
    }
}
```

## Best Practices

- Call `client.stop()` in a `finally` block or a shutdown hook. It detaches every session the client created and stops the CLI process.
- Check `sendAndWait` results for `null`; a turn can finish without an assistant message.
- Catch specific exception types before the generic `Exception`.
- Pass a timeout to `sendAndWait` for long tasks, and call `abort()` when you stop waiting.
- Log the session ID (`session.getSessionId()`) with errors so you can find the session later.

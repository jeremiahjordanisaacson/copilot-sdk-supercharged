# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in Java.

## Creating Multiple Sessions

Each session maintains its own conversation history, so you can run independent tasks in parallel.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.CopilotSession;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

CopilotClient client = new CopilotClient();

try {
    client.start();

    // Create two independent sessions
    CopilotSession codeReview = client.createSession(new SessionConfig());
    CopilotSession documentation = client.createSession(new SessionConfig());

    // Each session tracks its own context
    var reviewResponse = codeReview.sendAndWait(
        new SendOptions("Review this function for bugs")
    );
    System.out.println("Review: " + reviewResponse.getMessage());

    var docsResponse = documentation.sendAndWait(
        new SendOptions("Write API docs for UserService")
    );
    System.out.println("Docs: " + docsResponse.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Independent Conversations

Sessions do not share context. A follow-up in one session has no effect on the other.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.CopilotSession;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

CopilotClient client = new CopilotClient();

try {
    client.start();

    CopilotSession session1 = client.createSession(new SessionConfig());
    CopilotSession session2 = client.createSession(new SessionConfig());

    // Session 1 conversation
    session1.sendAndWait(new SendOptions("I'm working on a REST API in Java"));
    var followUp1 = session1.sendAndWait(new SendOptions("How should I handle auth?"));
    // This reply understands the REST API context from session1
    System.out.println("Session 1: " + followUp1.getMessage());

    // Session 2 conversation (knows nothing about session1)
    session2.sendAndWait(new SendOptions("I'm building a CLI tool in Java"));
    var followUp2 = session2.sendAndWait(new SendOptions("How should I handle auth?"));
    // This reply is about CLI auth, not REST API auth
    System.out.println("Session 2: " + followUp2.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Parallel Requests with CompletableFuture

Run multiple sessions concurrently using `CompletableFuture`.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import java.util.concurrent.CompletableFuture;

CopilotClient client = new CopilotClient();

try {
    client.start();

    var session1 = client.createSession(new SessionConfig());
    var session2 = client.createSession(new SessionConfig());
    var session3 = client.createSession(new SessionConfig());

    // Fire all requests at once
    CompletableFuture<String> f1 = CompletableFuture.supplyAsync(() -> {
        try {
            return session1.sendAndWait(new SendOptions("Explain generics")).getMessage();
        } catch (Exception e) { return "Error: " + e.getMessage(); }
    });

    CompletableFuture<String> f2 = CompletableFuture.supplyAsync(() -> {
        try {
            return session2.sendAndWait(new SendOptions("Explain streams")).getMessage();
        } catch (Exception e) { return "Error: " + e.getMessage(); }
    });

    CompletableFuture<String> f3 = CompletableFuture.supplyAsync(() -> {
        try {
            return session3.sendAndWait(new SendOptions("Explain records")).getMessage();
        } catch (Exception e) { return "Error: " + e.getMessage(); }
    });

    // Wait for all results
    CompletableFuture.allOf(f1, f2, f3).join();
    System.out.println("Generics: " + f1.get());
    System.out.println("Streams: " + f2.get());
    System.out.println("Records: " + f3.get());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Session Lifecycle

Sessions remain active until the client is stopped. Clean up sessions you no longer need.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

CopilotClient client = new CopilotClient();

try {
    client.start();

    // Short-lived session for a one-off task
    var tempSession = client.createSession(new SessionConfig());
    var result = tempSession.sendAndWait(new SendOptions("Format this JSON"));
    System.out.println(result.getMessage());
    // tempSession is cleaned up when the client stops

    // Long-lived session for interactive use
    var mainSession = client.createSession(new SessionConfig());
    mainSession.sendAndWait(new SendOptions("Let's build a REST API"));
    mainSession.sendAndWait(new SendOptions("Add a /users endpoint"));
    mainSession.sendAndWait(new SendOptions("Add pagination support"));
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop(); // All sessions are cleaned up
}
```

## Best Practices

- Use separate sessions for tasks that should not share conversational context.
- Use `CompletableFuture` to run independent sessions concurrently.
- Keep session references accessible so you can send follow-up messages to the right context.
- Call `client.stop()` once when done; this cleans up all active sessions.

# Persisting Sessions

Save and resume sessions across restarts using the GitHub Copilot SDK in Java.

## Custom Session IDs

Provide a custom session ID to resume a conversation later.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

CopilotClient client = new CopilotClient();

try {
    client.start();

    // Create a session with a custom ID
    SessionConfig config = new SessionConfig();
    config.setSessionId("project-alpha-session-001");

    var session = client.createSession(config);
    var response = session.sendAndWait(new SendOptions("Let's plan the auth module"));
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Resuming a Session

Use the same session ID to pick up where you left off after a restart.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

// First run: start a session
CopilotClient client1 = new CopilotClient();
try {
    client1.start();
    SessionConfig config = new SessionConfig();
    config.setSessionId("persistent-session-42");
    var session = client1.createSession(config);
    session.sendAndWait(new SendOptions("We are building a payment gateway"));
} finally {
    client1.stop();
}

// Later run: resume the same session
CopilotClient client2 = new CopilotClient();
try {
    client2.start();
    SessionConfig config = new SessionConfig();
    config.setSessionId("persistent-session-42");
    var session = client2.createSession(config);

    // The session remembers previous context
    var response = session.sendAndWait(new SendOptions("What were we building?"));
    System.out.println(response.getMessage()); // Refers to the payment gateway
} finally {
    client2.stop();
}
```

## Infinite Sessions with Compaction

Enable infinite sessions for long-running conversations. The SDK automatically compacts
history when it grows too large, emitting compaction events.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;

CopilotClient client = new CopilotClient();

try {
    client.start();

    SessionConfig config = new SessionConfig();
    config.setSessionId("long-running-agent");
    config.setInfiniteSession(true); // Enable compaction for long conversations

    var session = client.createSession(config);

    // Listen for compaction events
    session.onEvent("session.compaction_start", event -> {
        System.out.println("Compaction started: conversation history is being summarized...");
    });
    session.onEvent("session.compaction_complete", event -> {
        System.out.println("Compaction complete: history has been summarized.");
    });

    // Long-running loop that may trigger compaction
    String[] tasks = {
        "Analyze the auth module",
        "Review the database schema",
        "Check the API endpoints",
        "Suggest performance improvements",
        "Write unit tests for the service layer"
    };

    for (String task : tasks) {
        var response = session.sendAndWait(new SendOptions(task));
        System.out.println("Task: " + task);
        System.out.println("Response: " + response.getMessage());
        System.out.println();
    }
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Session State Location

By default, session state is persisted to `~/.copilot/session-state/{sessionId}`.
This allows automatic resumption without any extra configuration.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;

CopilotClient client = new CopilotClient();

try {
    client.start();

    // The session state is saved at:
    // ~/.copilot/session-state/my-app-session/
    SessionConfig config = new SessionConfig();
    config.setSessionId("my-app-session");

    var session = client.createSession(config);
    System.out.println("Session ID: " + config.getSessionId());
    System.out.println("State stored at: ~/.copilot/session-state/my-app-session/");
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Best Practices

- Use descriptive, deterministic session IDs (e.g., `"user-123-project-alpha"`) for easy resumption.
- Enable infinite sessions (`setInfiniteSession(true)`) for long-running agents or interactive tools.
- Listen for compaction events to provide user feedback during context summarization.
- Session state is stored in `~/.copilot/session-state/{sessionId}` by default.
- Avoid reusing session IDs across unrelated conversations to prevent context pollution.

# Multiple Sessions

Manage multiple independent conversations at the same time with the GitHub Copilot SDK in Java.

## Creating Multiple Sessions

One client can host many sessions. Each session keeps its own conversation history.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class MultipleSessions {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            SessionConfig config = new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once"));

            CopilotSession codeReview = client.createSession(config);
            CopilotSession documentation = client.createSession(config);

            SessionEvent review = codeReview.sendAndWait(
                new MessageOptions("Review this method for bugs: int add(int a, int b) { return a - b; }"));
            System.out.println("Review: " + contentOf(review));

            SessionEvent docs = documentation.sendAndWait(
                new MessageOptions("Write a Javadoc comment for a method that adds two ints"));
            System.out.println("Docs: " + contentOf(docs));
        } finally {
            client.stop();
        }
    }

    static Object contentOf(SessionEvent event) {
        return event == null ? "(no reply)" : event.data.get("content");
    }
}
```

## Independent Conversations

Sessions don't share context. A follow-up in one session has no effect on the other.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class IndependentConversations {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            SessionConfig config = new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once"));

            CopilotSession api = client.createSession(config);
            CopilotSession cli = client.createSession(config);

            api.sendAndWait(new MessageOptions("I'm building a REST API in Java"));
            SessionEvent apiAnswer = api.sendAndWait(new MessageOptions("How should I handle auth?"));
            System.out.println("REST API session: " + contentOf(apiAnswer));

            // This session knows nothing about the REST API conversation
            cli.sendAndWait(new MessageOptions("I'm building a command-line tool in Java"));
            SessionEvent cliAnswer = cli.sendAndWait(new MessageOptions("How should I handle auth?"));
            System.out.println("CLI session: " + contentOf(cliAnswer));
        } finally {
            client.stop();
        }
    }

    static Object contentOf(SessionEvent event) {
        return event == null ? "(no reply)" : event.data.get("content");
    }
}
```

## Parallel Requests with CompletableFuture

`sendAndWait` blocks the calling thread, so give each session its own task to run them concurrently.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

public class ParallelSessions {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            SessionConfig config = new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once"));

            CopilotSession generics = client.createSession(config);
            CopilotSession streams = client.createSession(config);
            CopilotSession records = client.createSession(config);

            CompletableFuture<String> f1 = CompletableFuture.supplyAsync(() -> ask(generics, "Explain Java generics in two sentences"));
            CompletableFuture<String> f2 = CompletableFuture.supplyAsync(() -> ask(streams, "Explain Java streams in two sentences"));
            CompletableFuture<String> f3 = CompletableFuture.supplyAsync(() -> ask(records, "Explain Java records in two sentences"));

            CompletableFuture.allOf(f1, f2, f3).join();
            System.out.println("Generics: " + f1.join());
            System.out.println("Streams: " + f2.join());
            System.out.println("Records: " + f3.join());
        } finally {
            client.stop();
        }
    }

    static String ask(CopilotSession session, String prompt) {
        try {
            SessionEvent reply = session.sendAndWait(new MessageOptions(prompt), 120_000);
            return reply == null ? "(no reply)" : String.valueOf(reply.data.get("content"));
        } catch (Exception e) {
            throw new CompletionException(e);
        }
    }
}
```

## Session Lifecycle

`destroy()` releases a session you no longer need and keeps its saved state, so you can resume it
later. `deleteSession()` removes the saved state for good. `client.stop()` destroys every session the
client still holds.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class SessionLifecycle {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            SessionConfig config = new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once"));

            // A short-lived session for a one-off task
            CopilotSession scratch = client.createSession(config);
            SessionEvent formatted = scratch.sendAndWait(new MessageOptions("Format this JSON: {\"a\":1,\"b\":[1,2]}"));
            System.out.println(formatted == null ? "(no reply)" : formatted.data.get("content"));
            scratch.destroy();
            client.deleteSession(scratch.getSessionId());

            // A long-lived session for a multi-step task
            CopilotSession project = client.createSession(config);
            project.sendAndWait(new MessageOptions("Let's design a REST API for a todo app"));
            project.sendAndWait(new MessageOptions("Add a /users endpoint"));
            SessionEvent last = project.sendAndWait(new MessageOptions("Add pagination to both endpoints"));
            System.out.println(last == null ? "(no reply)" : last.data.get("content"));
        } finally {
            client.stop();
        }
    }
}
```

## Best Practices

- Use a separate session for each task that shouldn't share conversational context.
- Reuse one `CopilotClient` for all sessions; each client starts its own CLI process.
- Run independent sessions concurrently with `CompletableFuture`, one session per task.
- Keep session references (or IDs) so follow-up messages go to the right conversation.
- Call `destroy()` on sessions you're done with, and `client.stop()` once at the end.

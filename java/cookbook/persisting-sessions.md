# Persisting Sessions

Save and resume sessions across restarts with the GitHub Copilot SDK in Java.

## Custom Session IDs

Give a session your own ID so you can find and resume it later. To continue a saved conversation,
use `resumeSession`: `createSession` with an ID that's already in use starts a new conversation
without the saved history.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class CustomSessionId {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .sessionId("project-alpha-session-001")
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));
            System.out.println("Session ID: " + session.getSessionId());

            SessionEvent reply = session.sendAndWait(new MessageOptions("Let's plan the auth module"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Resuming a Session

Sessions are saved as you go. `client.stop()` (or `session.destroy()`) detaches a session and keeps
its state on disk, and `resumeSession` picks it up again, even from another process. Handlers such as
`onPermissionRequest` and tools aren't saved with the session, so pass them again when you resume.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class ResumeSession {
    static final String SESSION_ID = "persistent-session-42";

    public static void main(String[] args) throws Exception {
        // First run: start the conversation
        CopilotClient first = new CopilotClient();
        try {
            first.start();
            CopilotSession session = first.createSession(new SessionConfig()
                .sessionId(SESSION_ID)
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));
            session.sendAndWait(new MessageOptions("We are building a payment gateway in Java"));
        } finally {
            first.stop();
        }

        // Later run: resume the conversation by ID
        CopilotClient later = new CopilotClient();
        try {
            later.start();
            CopilotSession session = later.resumeSession(SESSION_ID, new SessionConfig()
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            SessionEvent reply = session.sendAndWait(new MessageOptions("What are we building?"));
            if (reply != null) {
                System.out.println(reply.data.get("content")); // mentions the payment gateway
            }
        } finally {
            later.stop();
        }
    }
}
```

## Listing and Deleting Saved Sessions

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.Types.SessionMetadata;

public class ListSavedSessions {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            for (SessionMetadata meta : client.listSessions()) {
                System.out.println(meta.sessionId + "  modified " + meta.modifiedTime + "  " + meta.summary);
            }

            String lastId = client.getLastSessionId();
            if (lastId != null) {
                System.out.println("Most recent session: " + lastId);
            }

            // Removes a saved session and its state for good
            // client.deleteSession("project-alpha-session-001");
        } finally {
            client.stop();
        }
    }
}
```

## Infinite Sessions with Compaction

Infinite sessions are on by default: when the conversation nears the model's context limit, the
runtime compacts older history in the background and emits compaction events. Pass an
`InfiniteSessionConfig` only to change the thresholds or to turn the feature off
(`enabled = false`).

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.InfiniteSessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;

public class InfiniteSessions {
    public static void main(String[] args) throws Exception {
        InfiniteSessionConfig infinite = new InfiniteSessionConfig();
        infinite.enabled = true;
        infinite.backgroundCompactionThreshold = 0.80; // start compacting at 80% of the context window
        infinite.bufferExhaustionThreshold = 0.95;     // pause new work at 95% until compaction finishes

        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .sessionId("long-running-agent")
                .infiniteSessions(infinite)
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            session.on("session.compaction_start", event ->
                System.out.println("Compacting conversation history..."));
            session.on("session.compaction_complete", event ->
                System.out.println("Compaction finished, success=" + event.data.get("success")));

            String[] tasks = {
                "Analyze the auth module",
                "Review the database schema",
                "Check the API endpoints",
                "Suggest performance improvements",
                "Write unit tests for the service layer"
            };
            for (String task : tasks) {
                SessionEvent reply = session.sendAndWait(new MessageOptions(task), 300_000);
                System.out.println("Task: " + task);
                System.out.println("Response: " + (reply == null ? "(no reply)" : reply.data.get("content")));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Session State Location

With infinite sessions on, each session has a workspace directory for its checkpoints, plan, and
files. By default it's `~/.copilot/session-state/{sessionId}`. `getWorkspacePath()` returns the
actual path, or `null` when infinite sessions are off.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;

public class SessionStateLocation {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .sessionId("my-app-session")
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            System.out.println("Session ID: " + session.getSessionId());
            System.out.println("Workspace: " + session.getWorkspacePath());
        } finally {
            client.stop();
        }
    }
}
```

## Best Practices

- Use descriptive, deterministic session IDs (for example `"user-123-project-alpha"`) so you can resume them.
- Resume with `resumeSession`; `createSession` with an existing ID doesn't load the saved history.
- Register handlers and tools again on every resume.
- Listen for compaction events to tell users when history is being summarized.
- Delete sessions you no longer need with `deleteSession`.

# Tools and Skills

Define custom tools, load skills, add custom agents, and watch tool and sub-agent activity with the
GitHub Copilot SDK in Java.

## Defining a Simple Tool

Build a tool with `DefineTool.create(name)`: give it a description, a JSON Schema for its parameters,
and a handler. The handler receives the arguments as a `Map` and returns the result for the model: a
`String`, a `ToolResultObject`, or any object Jackson can serialize (it's sent as JSON).

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.DefineTool;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import com.github.copilot.Types.Tool;
import java.util.List;
import java.util.Map;

public class SimpleTool {
    public static void main(String[] args) throws Exception {
        Tool weatherTool = DefineTool.create("get_weather")
            .description("Get the current weather for a city")
            .parameters(Map.of(
                "type", "object",
                "properties", Map.of(
                    "city", Map.of("type", "string", "description", "City name")),
                "required", List.of("city")))
            .handler((arguments, invocation) -> {
                String city = (String) ((Map<?, ?>) arguments).get("city");
                // Call a real weather API here
                return Map.of("city", city, "temperatureF", 72, "condition", "sunny");
            })
            .build();

        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .tools(List.of(weatherTool))
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            SessionEvent reply = session.sendAndWait(new MessageOptions("What is the weather in Seattle?"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Required and Optional Parameters

List required parameters in the schema's `required` array and give optional ones a default in the
handler. JSON numbers arrive as `Integer`, `Long`, or `Double`, so read them through `Number`.

```java
import com.github.copilot.DefineTool;
import com.github.copilot.Types.Tool;
import java.util.List;
import java.util.Map;

public class SearchDocsTool {
    static Tool create() {
        return DefineTool.create("search_docs")
            .description("Search the documentation for a topic")
            .parameters(Map.of(
                "type", "object",
                "properties", Map.of(
                    "query", Map.of("type", "string", "description", "The search query"),
                    "limit", Map.of("type", "integer", "description", "Maximum number of results (default 10)")),
                "required", List.of("query")))
            .idempotent(true) // repeating a call with the same arguments has no extra effect
            .handler((arguments, invocation) -> {
                Map<?, ?> input = (Map<?, ?>) arguments;
                String query = (String) input.get("query");
                int limit = input.get("limit") instanceof Number n ? n.intValue() : 10;
                return "Top " + limit + " results for '" + query + "': ...";
            })
            .build();
    }
}
```

## Reporting Tool Failures

If a handler throws, the call fails. The runtime logs the exception message but the model only sees
a generic failure, so when the model should know why (to fix its arguments, for example), return a
`ToolResultObject` with `resultType` set to `"failure"`.

```java
import com.github.copilot.DefineTool;
import com.github.copilot.Types.Tool;
import com.github.copilot.Types.ToolResultObject;
import java.util.List;
import java.util.Map;

public class LookupIssueTool {
    static final Map<String, String> ISSUES = Map.of("42", "Login fails when the password contains a space");

    static Tool create() {
        return DefineTool.create("lookup_issue")
            .description("Fetch issue details from the tracker")
            .parameters(Map.of(
                "type", "object",
                "properties", Map.of("id", Map.of("type", "string", "description", "Issue ID")),
                "required", List.of("id")))
            .handler((arguments, invocation) -> {
                String id = (String) ((Map<?, ?>) arguments).get("id");
                String issue = ISSUES.get(id);
                if (issue == null) {
                    return new ToolResultObject("Issue " + id + " was not found. Check the ID and try again.", "failure", null);
                }
                return issue;
            })
            .build();
    }
}
```

## Multiple Tools

Register several tools on one session. Avoid the names of the CLI's built-in tools (such as `view`,
`edit`, or `grep`).

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.DefineTool;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import com.github.copilot.Types.Tool;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MultipleTools {
    public static void main(String[] args) throws Exception {
        Tool readFile = DefineTool.create("read_project_file")
            .description("Read a UTF-8 text file from the project")
            .parameters(Map.of(
                "type", "object",
                "properties", Map.of("path", Map.of("type", "string", "description", "File path")),
                "required", List.of("path")))
            .handler((arguments, invocation) ->
                Files.readString(Path.of((String) ((Map<?, ?>) arguments).get("path"))))
            .build();

        Tool listFiles = DefineTool.create("list_project_files")
            .description("List the entries in a project directory")
            .parameters(Map.of(
                "type", "object",
                "properties", Map.of("directory", Map.of("type", "string", "description", "Directory path")),
                "required", List.of("directory")))
            .handler((arguments, invocation) -> {
                Path dir = Path.of((String) ((Map<?, ?>) arguments).get("directory"));
                try (Stream<Path> entries = Files.list(dir)) {
                    return entries.map(p -> p.getFileName().toString()).sorted().collect(Collectors.joining("\n"));
                }
            })
            .build();

        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .tools(List.of(readFile, listFiles))
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            SessionEvent reply = session.sendAndWait(
                new MessageOptions("List the files in ./src, then read the first one and summarize it"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Skills

A skill is a directory with a `SKILL.md` file: YAML front matter (`name`, `description`) followed by
instructions. Point the session at the parent directory, and turn off individual skills by name.

```text
skills/
  code-review/
    SKILL.md
  release-notes/
    SKILL.md
```

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import java.util.List;

public class SkillDirectories {
    public static void main(String[] args) throws Exception {
        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .skillDirectories(List.of("./skills"))
                .disabledSkills(List.of("release-notes"))
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            SessionEvent reply = session.sendAndWait(
                new MessageOptions("Use the code-review skill to review src/Main.java"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Custom Agents

Custom agents are sub-agents with their own prompt and tool list. The main agent can delegate work
to them. `tools` limits the agent to the listed tools (`null` allows all of them), and `skills`
preloads skills from the session's skill directories into the agent's context.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.CustomAgentConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import java.util.List;

public class CustomAgents {
    public static void main(String[] args) throws Exception {
        CustomAgentConfig reviewer = new CustomAgentConfig();
        reviewer.name = "security-reviewer";
        reviewer.displayName = "Security Reviewer";
        reviewer.description = "Reviews code for security problems";
        reviewer.prompt = "You review code for injection, authentication, and secret-handling problems. "
            + "Report each finding with its file and line.";
        reviewer.tools = List.of("view", "grep", "glob"); // read-only tools
        reviewer.skills = List.of("code-review");

        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .skillDirectories(List.of("./skills"))
                .customAgents(List.of(reviewer))
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            SessionEvent reply = session.sendAndWait(
                new MessageOptions("Ask the security reviewer to check the src folder"), 300_000);
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Watching Tool and Sub-Agent Events

Tool calls emit `tool.execution_start` and `tool.execution_complete`. Sub-agents emit
`subagent.started`, then `subagent.completed` or `subagent.failed`.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.DefineTool;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;
import com.github.copilot.Types.SessionEvent;
import com.github.copilot.Types.Tool;
import java.util.List;
import java.util.Map;

public class ToolAndAgentEvents {
    public static void main(String[] args) throws Exception {
        Tool analyzer = DefineTool.create("analyze_code_quality")
            .description("Analyze code quality for a directory")
            .parameters(Map.of(
                "type", "object",
                "properties", Map.of("directory", Map.of("type", "string")),
                "required", List.of("directory")))
            .handler((arguments, invocation) -> Map.of("issues", 3, "severity", "medium"))
            .build();

        CopilotClient client = new CopilotClient();
        try {
            client.start();
            CopilotSession session = client.createSession(new SessionConfig()
                .tools(List.of(analyzer))
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            session.on("tool.execution_start", event ->
                System.out.println("Tool started: " + event.data.get("toolName")));
            session.on("tool.execution_complete", event ->
                System.out.println("Tool finished, success=" + event.data.get("success")));
            session.on("subagent.started", event ->
                System.out.println("Sub-agent started: " + event.data.get("agentDisplayName")));
            session.on("subagent.completed", event ->
                System.out.println("Sub-agent completed: " + event.data.get("agentDisplayName")));
            session.on("subagent.failed", event ->
                System.out.println("Sub-agent failed: " + event.data.get("agentDisplayName")
                    + ": " + event.data.get("error")));

            SessionEvent reply = session.sendAndWait(
                new MessageOptions("Analyze the code quality of ./src and summarize the result"));
            if (reply != null) {
                System.out.println(reply.data.get("content"));
            }
        } finally {
            client.stop();
        }
    }
}
```

## Best Practices

- Give tools clear names and descriptions so the model knows when to use them.
- Describe every parameter in the JSON Schema and list the required ones.
- Keep handlers fast; the model waits for the result before it continues.
- Return a failure `ToolResultObject` when the model needs to know why a call failed.
- Use skills and custom agents to specialize behavior without writing more tools.
- Log `tool.execution_start` and `tool.execution_complete` events to monitor tool use.

# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in Java.

## Defining a Simple Tool

Use `DefineTool.create` to expose a function that the model can call.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import com.github.copilot.sdk.tools.DefineTool;

CopilotClient client = new CopilotClient();

try {
    client.start();

    // Define a tool the model can call
    DefineTool weatherTool = DefineTool.create(
        "get_weather",
        "Get the current weather for a city",
        args -> {
            String city = args.getString("city");
            // In production, call a real weather API
            return String.format("{\"city\": \"%s\", \"temp\": \"72F\", \"condition\": \"sunny\"}", city);
        }
    );

    SessionConfig config = new SessionConfig();
    config.setTools(java.util.List.of(weatherTool));

    var session = client.createSession(config);
    var response = session.sendAndWait(new SendOptions("What is the weather in Seattle?"));
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Tool with Parameters

Define tools with typed parameters using a JSON Schema description.

```java
import com.github.copilot.sdk.tools.DefineTool;
import com.github.copilot.sdk.tools.ToolParameter;

DefineTool searchTool = DefineTool.create(
    "search_docs",
    "Search the documentation for a topic",
    java.util.List.of(
        new ToolParameter("query", "string", "The search query", true),
        new ToolParameter("limit", "integer", "Max results to return", false)
    ),
    args -> {
        String query = args.getString("query");
        int limit = args.optInt("limit", 10);
        // Perform the search
        return String.format("[{\"title\": \"Result for '%s'\", \"count\": %d}]", query, limit);
    }
);
```

## Multiple Tools

Register multiple tools on a single session.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import com.github.copilot.sdk.tools.DefineTool;

CopilotClient client = new CopilotClient();

try {
    client.start();

    DefineTool readFile = DefineTool.create("read_file", "Read a file from disk", args -> {
        String path = args.getString("path");
        return java.nio.file.Files.readString(java.nio.file.Path.of(path));
    });

    DefineTool listFiles = DefineTool.create("list_files", "List files in a directory", args -> {
        String dir = args.getString("directory");
        return String.join("\n", java.nio.file.Files.list(java.nio.file.Path.of(dir))
            .map(p -> p.getFileName().toString())
            .toList());
    });

    SessionConfig config = new SessionConfig();
    config.setTools(java.util.List.of(readFile, listFiles));

    var session = client.createSession(config);
    var response = session.sendAndWait(new SendOptions("List the files in /src and read main.java"));
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Skills Configuration

Pass skill definitions in the session config to enable specialized behaviors.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import com.github.copilot.sdk.skills.Skill;

CopilotClient client = new CopilotClient();

try {
    client.start();

    Skill codeReviewSkill = new Skill("code-review", "Review code for bugs and improvements");

    SessionConfig config = new SessionConfig();
    config.setSkills(java.util.List.of(codeReviewSkill));

    var session = client.createSession(config);
    var response = session.sendAndWait(
        new SendOptions("Review this pull request for security issues")
    );
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Listening for Sub-Agent Events

When the model spawns sub-agents, listen for their lifecycle events.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;
import com.github.copilot.sdk.SendOptions;
import com.github.copilot.sdk.tools.DefineTool;

CopilotClient client = new CopilotClient();

try {
    client.start();

    DefineTool analyzerTool = DefineTool.create("analyze_code", "Analyze code quality", args -> {
        return "{\"issues\": 3, \"severity\": \"medium\"}";
    });

    SessionConfig config = new SessionConfig();
    config.setTools(java.util.List.of(analyzerTool));

    var session = client.createSession(config);

    // Listen for sub-agent events
    session.onEvent("assistant.tool_call", event -> {
        System.out.println("Tool called: " + event.getToolName());
    });
    session.onEvent("assistant.tool_result", event -> {
        System.out.println("Tool result received");
    });

    var response = session.sendAndWait(
        new SendOptions("Analyze the code in /src for quality issues")
    );
    System.out.println(response.getMessage());
} catch (Exception e) {
    System.err.println("Error: " + e.getMessage());
} finally {
    client.stop();
}
```

## Best Practices

- Give tools clear, descriptive names and descriptions so the model knows when to use them.
- Mark parameters as required or optional with sensible defaults.
- Keep tool handlers fast; the model waits for the result before continuing.
- Return structured JSON from tool handlers for easier model interpretation.
- Use skills to specialize session behavior without adding custom tools.
- Listen for tool call events to log and monitor tool usage in production.

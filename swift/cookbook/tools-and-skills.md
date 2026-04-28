# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in Swift.

## Defining a Simple Tool

Use `DefineTool` to expose a function that the model can call.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    // Define a tool the model can call
    let weatherTool = DefineTool(
        name: "get_weather",
        description: "Get the current weather for a city"
    ) { args in
        let city = args["city"] as? String ?? "unknown"
        // In production, call a real weather API
        return #"{"city": "\#(city)", "temp": "72F", "condition": "sunny"}"#
    }

    var config = SessionConfig()
    config.tools = [weatherTool]

    let session = try await client.createSession(config: config)
    let response = try await session.sendAndWait(
        options: SendOptions(message: "What is the weather in Seattle?")
    )
    print(response.message)
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Tool with Parameters

Define tools with typed parameters.

```swift
import CopilotSDK

let searchTool = DefineTool(
    name: "search_docs",
    description: "Search the documentation for a topic",
    parameters: [
        ToolParameter(name: "query", type: .string, description: "The search query", required: true),
        ToolParameter(name: "limit", type: .integer, description: "Max results to return", required: false),
    ]
) { args in
    let query = args["query"] as? String ?? ""
    let limit = args["limit"] as? Int ?? 10
    return #"[{"title": "Result for '\#(query)'", "count": \#(limit)}]"#
}
```

## Multiple Tools

Register multiple tools on a single session.

```swift
import CopilotSDK
import Foundation

let client = CopilotClient()

do {
    try await client.start()

    let readFile = DefineTool(name: "read_file", description: "Read a file from disk") { args in
        let path = args["path"] as? String ?? ""
        return try String(contentsOfFile: path, encoding: .utf8)
    }

    let listFiles = DefineTool(name: "list_files", description: "List files in a directory") { args in
        let dir = args["directory"] as? String ?? "."
        let files = try FileManager.default.contentsOfDirectory(atPath: dir)
        return files.joined(separator: "\n")
    }

    var config = SessionConfig()
    config.tools = [readFile, listFiles]

    let session = try await client.createSession(config: config)
    let response = try await session.sendAndWait(
        options: SendOptions(message: "List the files in /src and read main.swift")
    )
    print(response.message)
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Skills Configuration

Pass skill definitions in the session config to enable specialized behaviors.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    let codeReviewSkill = Skill(name: "code-review", description: "Review code for bugs and improvements")

    var config = SessionConfig()
    config.skills = [codeReviewSkill]

    let session = try await client.createSession(config: config)
    let response = try await session.sendAndWait(
        options: SendOptions(message: "Review this pull request for security issues")
    )
    print(response.message)
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Listening for Sub-Agent Events

When the model spawns sub-agents, listen for their lifecycle events.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    let analyzer = DefineTool(
        name: "analyze_code",
        description: "Analyze code quality"
    ) { _ in
        return #"{"issues": 3, "severity": "medium"}"#
    }

    var config = SessionConfig()
    config.tools = [analyzer]

    let session = try await client.createSession(config: config)

    // Listen for sub-agent events
    session.onEvent("assistant.tool_call") { event in
        print("Tool called: \(event.toolName)")
    }
    session.onEvent("assistant.tool_result") { _ in
        print("Tool result received")
    }

    let response = try await session.sendAndWait(
        options: SendOptions(message: "Analyze the code in /src for quality issues")
    )
    print(response.message)
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Best Practices

- Give tools clear, descriptive names and descriptions so the model knows when to use them.
- Use `ToolParameter` with `required: true/false` for typed parameter definitions.
- Return JSON strings from tool handlers for easier model interpretation.
- Keep tool handlers fast; the model waits for the result before continuing.
- Use skills to specialize session behavior without adding custom tools.
- Listen for tool call events to log and monitor tool usage in production.

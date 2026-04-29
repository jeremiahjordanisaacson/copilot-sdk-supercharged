# Tools and Skills

Register custom tools that the Copilot agent can invoke during a session.

## Defining a Tool

```zig
const copilot = @import("copilot-sdk");

// Define the tool schema
const weather_tool = copilot.defineTool(
    "get_weather",
    "Get the current weather for a location",
    \\{"type":"object","properties":{"location":{"type":"string","description":"City name"}},"required":["location"]}
);

// Implement the handler
fn getWeatherHandler(invocation: copilot.ToolInvocation) anyerror!copilot.ToolResult {
    // Parse invocation.arguments_json to extract parameters
    _ = invocation;
    return .{
        .text_result_for_llm = "{\"location\":\"Seattle\",\"temp\":72,\"condition\":\"sunny\"}",
        .result_type = .success,
    };
}
```

## Registering Tools on a Session

```zig
var session = try client.createSession(.{});
try session.registerTool(weather_tool, getWeatherHandler);
```

## Using the ToolRegistry

For applications with many tools, use the registry for cleaner dispatch:

```zig
var registry = copilot.ToolRegistry.init(allocator);
defer registry.deinit();

try registry.register(weather_tool, getWeatherHandler);
try registry.register(search_tool, searchHandler);

// Dispatch an invocation
const result = try registry.dispatch(invocation);
```

## Tool Result Types

- `.success` -- the tool completed successfully; the LLM sees the text result
- `.failure` -- the tool failed; the LLM sees the error message

## Skills

Register skill directories and disable specific skills:

```zig
var session = try client.createSession(.{
    .skill_directories = &.{"./skills"},
    .disabled_skills = &.{"test-skill"},
    .include_sub_agent_streaming_events = true,
});
```

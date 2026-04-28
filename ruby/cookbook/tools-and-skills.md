# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in Ruby.

## Defining a Simple Tool

Use `Copilot::DefineTool.new` to expose a function that the model can call.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  # Define a tool the model can call
  weather_tool = Copilot::DefineTool.new("get_weather", "Get the current weather for a city") do |args|
    city = args["city"]
    # In production, call a real weather API
    { city: city, temp: "72F", condition: "sunny" }.to_json
  end

  config = Copilot::SessionConfig.new(tools: [weather_tool])
  session = client.create_session(config)

  response = session.send_and_wait(
    Copilot::SendOptions.new(message: "What is the weather in Seattle?")
  )
  puts response.message
ensure
  client.stop
end
```

## Tool with Parameters

Define tools with typed parameters.

```ruby
require "copilot_sdk"

search_tool = Copilot::DefineTool.new(
  "search_docs",
  "Search the documentation for a topic",
  parameters: [
    { name: "query", type: "string", description: "The search query", required: true },
    { name: "limit", type: "integer", description: "Max results to return", required: false }
  ]
) do |args|
  query = args["query"]
  limit = args.fetch("limit", 10)
  [{ title: "Result for '#{query}'", count: limit }].to_json
end
```

## Multiple Tools

Register multiple tools on a single session.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  read_file = Copilot::DefineTool.new("read_file", "Read a file from disk") do |args|
    File.read(args["path"])
  end

  list_files = Copilot::DefineTool.new("list_files", "List files in a directory") do |args|
    Dir.entries(args["directory"]).reject { |f| f.start_with?(".") }.join("\n")
  end

  config = Copilot::SessionConfig.new(tools: [read_file, list_files])
  session = client.create_session(config)

  response = session.send_and_wait(
    Copilot::SendOptions.new(message: "List the files in /src and read main.rb")
  )
  puts response.message
ensure
  client.stop
end
```

## Skills Configuration

Pass skill definitions in the session config to enable specialized behaviors.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  code_review_skill = Copilot::Skill.new("code-review", "Review code for bugs and improvements")

  config = Copilot::SessionConfig.new(skills: [code_review_skill])
  session = client.create_session(config)

  response = session.send_and_wait(
    Copilot::SendOptions.new(message: "Review this pull request for security issues")
  )
  puts response.message
ensure
  client.stop
end
```

## Listening for Sub-Agent Events

When the model spawns sub-agents, listen for their lifecycle events.

```ruby
require "copilot_sdk"

client = Copilot::Client.new

begin
  client.start

  analyzer = Copilot::DefineTool.new("analyze_code", "Analyze code quality") do |_args|
    { issues: 3, severity: "medium" }.to_json
  end

  config = Copilot::SessionConfig.new(tools: [analyzer])
  session = client.create_session(config)

  # Listen for sub-agent events
  session.on_event("assistant.tool_call") do |event|
    puts "Tool called: #{event.tool_name}"
  end
  session.on_event("assistant.tool_result") do |_event|
    puts "Tool result received"
  end

  response = session.send_and_wait(
    Copilot::SendOptions.new(message: "Analyze the code in /src for quality issues")
  )
  puts response.message
ensure
  client.stop
end
```

## Best Practices

- Give tools clear, descriptive names and descriptions so the model knows when to use them.
- Use blocks for concise tool handlers.
- Return JSON strings from tool handlers for easier model interpretation.
- Keep tool handlers fast; the model waits for the result before continuing.
- Use skills to specialize session behavior without adding custom tools.
- Listen for tool call events to log and monitor tool usage in production.

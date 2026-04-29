# Tools and Skills

Defining and composing tools that the Copilot assistant can invoke.

## Basic Tool Definition

```crystal
require "copilot_sdk"

client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)
client.start

session = client.create_session(CopilotSDK::SessionConfig.new)

# Use the builder DSL for convenience
session.define_tool("calculate") do |t|
  t.description = "Evaluate a math expression"
  t.parameter("expression", "string", "The math expression to evaluate", required: true)
  t.handler do |args, _invocation|
    expr = args["expression"].as_s
    # Simple evaluation (for demo purposes)
    JSON::Any.new("Result: #{expr}")
  end
end
```

## Tool with Multiple Parameters

```crystal
session.define_tool("search_files") do |t|
  t.description = "Search for files matching a pattern"
  t.parameter("directory", "string", "Directory to search in", required: true)
  t.parameter("pattern", "string", "Glob pattern to match", required: true)
  t.parameter("recursive", "boolean", "Search recursively")
  t.handler do |args, _invocation|
    dir = args["directory"].as_s
    pattern = args["pattern"].as_s
    recursive = args["recursive"]?.try(&.as_bool) || false

    glob = recursive ? "#{dir}/**/#{pattern}" : "#{dir}/#{pattern}"
    files = Dir.glob(glob).first(20)
    JSON::Any.new(files.join("\n"))
  end
end
```

## Manual Tool Registration

```crystal
# Build the definition and handler separately
definition = CopilotSDK::ToolDefinition.new(
  name: "get_env",
  description: "Get an environment variable",
  parameters: JSON.parse(%({
    "type": "object",
    "properties": {
      "name": {"type": "string", "description": "Variable name"}
    },
    "required": ["name"]
  }))
)

session.add_tool(definition) do |args, _invocation|
  name = args["name"].as_s
  value = ENV[name]? || "(not set)"
  JSON::Any.new(value)
end
```

## Composing Tools with a Registry

```crystal
class ToolRegistry
  @entries : Array(Tuple(CopilotSDK::ToolDefinition, CopilotSDK::ToolHandler))

  def initialize
    @entries = [] of Tuple(CopilotSDK::ToolDefinition, CopilotSDK::ToolHandler)
  end

  def register(name : String, &block : CopilotSDK::ToolBuilder ->)
    defn, handler = CopilotSDK.define_tool(name) { |t| yield t }
    @entries << {defn, handler}
  end

  def apply(session : CopilotSDK::CopilotSession) : Nil
    @entries.each do |defn, handler|
      session.add_tool(defn, &handler)
    end
  end
end

# Build a reusable registry
file_tools = ToolRegistry.new
file_tools.register("read_file") do |t|
  t.description = "Read a file"
  t.parameter("path", "string", "File path", required: true)
  t.handler { |args, _| JSON::Any.new(File.read(args["path"].as_s)) }
end

file_tools.register("list_dir") do |t|
  t.description = "List a directory"
  t.parameter("path", "string", "Directory path", required: true)
  t.handler { |args, _| JSON::Any.new(Dir.children(args["path"].as_s).join("\n")) }
end

# Apply to any session
file_tools.apply(session)
```

# Advanced Features

Crystal-specific patterns for agent mode, model selection, and streaming.

## Agent Mode

```crystal
require "copilot_sdk"

client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)
client.start

# Enable agent mode in the session config
session = client.create_session(
  CopilotSDK::SessionConfig.new(
    model: "gpt-4",
    agent_mode: "full",
    streaming: true
  )
)

# Agent mode sessions can use tools iteratively
session.define_tool("read_file") do |t|
  t.description = "Read a file from disk"
  t.parameter("path", "string", "File path to read", required: true)
  t.handler do |args, _invocation|
    path = args["path"].as_s
    if File.exists?(path)
      JSON::Any.new(File.read(path))
    else
      JSON::Any.new("File not found: #{path}")
    end
  end
end

session.on_permission do |_req, _sid|
  CopilotSDK::PermissionRequestResult.new(allowed: true)
end

session.send(CopilotSDK::MessageOptions.new(
  prompt: "Read the file ./README.md and summarize it."
))

session.destroy
client.stop
```

## Model Selection

```crystal
# List available models
models = client.list_models
models.each do |m|
  puts "#{m.id} - #{m.display_name || "no display name"}"
end

# Create a session with a specific model
session = client.create_session(
  CopilotSDK::SessionConfig.new(model: models.first.id)
)
```

## Streaming with Fibers

```crystal
# Use a channel to collect streaming output
output_channel = Channel(String).new(100)

session.on("assistant.message_delta") do |event|
  if delta = event.delta_text
    output_channel.send(delta)
  end
end

session.on("turn.end") do |_event|
  output_channel.close
end

# Process deltas in a separate fiber
spawn do
  while delta = output_channel.receive?
    print delta
    STDOUT.flush
  end
  puts
end

session.send(CopilotSDK::MessageOptions.new(prompt: "Tell me a joke"))
```

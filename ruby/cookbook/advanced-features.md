# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```ruby
require "copilot_sdk"

client = CopilotSDK::Client.new

session = client.create_session(
  github_token: "ghu_tenant_abc_token_here"
)

# This session authenticates as a specific tenant user
session.send("Hello from tenant ABC")
```

## Session Idle Timeout

Auto-cleanup inactive sessions with a configurable timeout:

```ruby
session = client.create_session(
  session_idle_timeout_seconds: 600 # 10 minutes
)

# Session automatically closes after 600 seconds of inactivity
```

## SessionFs (Session Filesystem)

Provide a session-scoped filesystem with 10 I/O operations (readFile, writeFile, appendFile, exists, stat, mkdir, readdir, readdirWithTypes, rm, rename):

```ruby
require "fileutils"

fs_provider = CopilotSDK::SessionFsProvider.new do |fs|
  base_path = "/workspace"

  fs.read_file do |path|
    File.read(File.join(base_path, path))
  end

  fs.write_file do |path, content|
    File.write(File.join(base_path, path), content)
  end

  fs.append_file do |path, content|
    File.open(File.join(base_path, path), "a") { |f| f.write(content) }
  end

  fs.exists do |path|
    File.exist?(File.join(base_path, path))
  end

  fs.stat do |path|
    s = File.stat(File.join(base_path, path))
    { size: s.size, is_directory: s.directory? }
  end

  fs.mkdir do |path|
    FileUtils.mkdir_p(File.join(base_path, path))
  end

  fs.readdir do |path|
    Dir.children(File.join(base_path, path))
  end

  fs.readdir_with_types do |path|
    Dir.children(File.join(base_path, path)).map do |name|
      full = File.join(base_path, path, name)
      { name: name, is_directory: File.directory?(full) }
    end
  end

  fs.rm do |path|
    FileUtils.rm_rf(File.join(base_path, path))
  end

  fs.rename do |old_path, new_path|
    File.rename(File.join(base_path, old_path), File.join(base_path, new_path))
  end
end

session = client.create_session(session_fs: fs_provider)
```

## Commands and UI Elicitation

Register slash commands and handle interactive prompts/dialogs:

```ruby
commands = [
  { name: "deploy", description: "Deploy the current project" },
  { name: "test", description: "Run the test suite" }
]

session = client.create_session(
  commands: commands,
  on_elicitation_request: ->(request) {
    # Present the dialog to the user and collect responses
    {
      "environment" => "staging",
      "confirmed" => true
    }
  }
)
```

## System Prompt Customization

Fine-grained control with append, replace, and customize modes:

### Replace mode

```ruby
session = client.create_session(
  system_prompt: "You are a Ruby code review assistant. Focus on idiomatic patterns and performance.",
  system_prompt_mode: "replace"
)
```

### Customize mode with sections

```ruby
sections = [
  { key: "role", content: "You are an expert Ruby developer." },
  { key: "constraints", content: "Always suggest modern Ruby 3.x patterns." },
  { key: "output", content: "Format responses as Markdown with code blocks." }
]

session = client.create_session(
  system_prompt_sections: sections,
  system_prompt_mode: "customize"
)
```

## Per-Agent Skills

Preload skill content into agent context at startup:

```ruby
session = client.create_session(
  skill_directories: [
    "/workspace/.copilot/skills",
    "/workspace/custom-skills"
  ],
  disabled_skills: ["deprecated-skill"]
)
```

## Per-Agent Tool Visibility

Control tool access per agent with excludedTools:

```ruby
session = client.create_session(
  default_agent: {
    excluded_tools: [
      "shell_exec",
      "file_delete",
      "network_request"
    ]
  }
)
```

## Runtime Request Headers

Send custom HTTP headers per message turn:

```ruby
require "securerandom"

session.send("Analyze this code",
  request_headers: {
    "X-Request-Id" => SecureRandom.uuid,
    "X-Tenant-Id" => "tenant-abc-123"
  }
)
```

## Model Capabilities Override

Deep-merge overrides for model feature flags:

```ruby
session = client.create_session(
  model_capabilities: {
    "streaming" => true,
    "functionCalling" => true,
    "vision" => false,
    "maxTokens" => 8192
  }
)
```

## Config Discovery

Auto-detect MCP servers and skill directories from workspace:

```ruby
session = client.create_session(
  enable_config_discovery: true
)

# MCP servers and skills from .copilot/ in the workspace are loaded automatically
```

## Sub-Agent Streaming Events

Control streaming event forwarding from sub-agents:

```ruby
session = client.create_session(
  include_sub_agent_streaming_events: true
)

session.on_event("assistant.message_delta") do |event|
  # Receives deltas from both the primary agent and sub-agents
  print event.delta
end
```

## Session Metadata

Retrieve session metadata via RPC:

```ruby
session = client.create_session
session.send("Hello")

metadata = session.get_session_metadata
puts "Session ID: #{metadata.session_id}"
puts "Created at: #{metadata.created_at}"
puts "Turn count: #{metadata.turn_count}"
```

## MCP Server Configuration

Configure MCP servers with stdio and HTTP transport types:

```ruby
mcp_servers = [
  # Stdio transport
  {
    name: "local-tools",
    transport_type: "stdio",
    command: "node",
    args: ["./mcp-server/index.js"]
  },
  # HTTP transport
  {
    name: "remote-tools",
    transport_type: "http",
    url: "https://mcp.example.com/api",
    headers: { "Authorization" => "Bearer #{token}" }
  }
]

session = client.create_session(mcp_servers: mcp_servers)
```

## Image Generation

Configure response format and image options:

```ruby
session = client.create_session(response_format: "image")

response = session.send("Generate a UML class diagram for a user auth system")
# Response contains image data
File.binwrite("diagram.png", response.image_data)
```

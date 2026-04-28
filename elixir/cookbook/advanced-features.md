# Advanced v2.0 Features - Elixir

Recipes for v2.0 SDK features in Elixir: per-session auth, SessionFs, commands, system prompts, skills, config discovery, image generation, and more.

## Per-Session Authentication

**Scenario:** Provide a GitHub token on each session for user-scoped auth instead of a global token.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session_a} = CopilotClient.create_session(client,
  github_token: System.get_env("GITHUB_TOKEN_USER_A"),
  system_prompt: "You are a helpful assistant."
)

{:ok, response} = CopilotClient.send_and_wait(session_a, %{
  message: "Summarize my recent pull requests."
})

IO.puts("Response: #{response.message}")

# Create a second session with a different user token
{:ok, session_b} = CopilotClient.create_session(client,
  github_token: System.get_env("GITHUB_TOKEN_USER_B"),
  system_prompt: "You are a code reviewer."
)

CopilotClient.stop(client)
```

## Session Idle Timeout

**Scenario:** Automatically expire sessions after a period of inactivity.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  session_idle_timeout_seconds: 300,
  system_prompt: "You are a helpful assistant."
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Hello!"
})

IO.puts("Response: #{response.message}")

# Session automatically expires after 300s of inactivity.
# Sending a message after timeout returns {:error, :session_expired}.

CopilotClient.stop(client)
```

## SessionFs (Session Filesystem)

**Scenario:** Configure a session filesystem provider with I/O operations for file-based context.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

# Define a SessionFs provider with 10 I/O operations
session_fs_provider = %{
  read_file: fn path ->
    case File.read(path) do
      {:ok, content} -> {:ok, content}
      {:error, reason} -> {:error, "File not found: #{reason}"}
    end
  end,
  write_file: fn path, content ->
    File.write!(path, content)
    {:ok, %{success: true}}
  end,
  list_directory: fn path ->
    case File.ls(path) do
      {:ok, entries} -> {:ok, entries}
      {:error, reason} -> {:error, "Cannot list directory: #{reason}"}
    end
  end,
  create_directory: fn path ->
    File.mkdir_p!(path)
    {:ok, %{success: true}}
  end,
  delete_file: fn path ->
    File.rm!(path)
    {:ok, %{success: true}}
  end,
  file_exists: fn path ->
    {:ok, File.exists?(path)}
  end,
  get_file_info: fn path ->
    case File.stat(path) do
      {:ok, stat} -> {:ok, %{size: stat.size, modified: stat.mtime}}
      {:error, reason} -> {:error, "Cannot stat file: #{reason}"}
    end
  end,
  copy_file: fn source, destination ->
    File.cp!(source, destination)
    {:ok, %{success: true}}
  end,
  move_file: fn source, destination ->
    File.rename!(source, destination)
    {:ok, %{success: true}}
  end,
  search_files: fn path, pattern ->
    results =
      Path.wildcard(Path.join(path, "**/*"))
      |> Enum.filter(fn file ->
        case File.read(file) do
          {:ok, content} -> String.contains?(content, pattern)
          _ -> false
        end
      end)
    {:ok, results}
  end
}

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a file management assistant.",
  session_fs: %{
    provider: session_fs_provider,
    root_path: "/workspace/project",
    writable: true
  }
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "List all files in the current project directory."
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Commands and UI Elicitation

**Scenario:** Register commands and handle UI elicitation requests from the model.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

commands = [
  %{
    name: "deploy",
    description: "Deploy the application to a target environment",
    parameters: [
      %{name: "environment", type: "string", required: true},
      %{name: "version", type: "string", required: false}
    ]
  },
  %{
    name: "rollback",
    description: "Roll back the most recent deployment",
    parameters: [
      %{name: "environment", type: "string", required: true}
    ]
  }
]

# Handle elicitation: the model asks the user for input
elicitation_handler = fn request ->
  IO.puts("Model asks: #{request.message}")

  request.options
  |> Enum.with_index(1)
  |> Enum.each(fn {option, idx} ->
    IO.puts("  #{idx}) #{option}")
  end)

  answer = IO.gets("Your choice: ") |> String.trim()
  {:ok, %{response: answer}}
end

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a deployment assistant.",
  commands: commands,
  elicitation_handler: elicitation_handler
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "/deploy"
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## System Prompt Customization

**Scenario:** Use replace and customize modes with sections to control the system prompt.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

# Replace mode: completely replace the default system prompt
{:ok, session_replace} = CopilotClient.create_session(client,
  system_prompt: %{
    mode: "replace",
    content: "You are an Elixir programming expert. Only answer Elixir questions."
  }
)

# Customize mode: add sections before/after the default prompt
{:ok, session_custom} = CopilotClient.create_session(client,
  system_prompt: %{
    mode: "customize",
    sections: [
      %{
        position: "before",
        content: "You are assisting a senior Elixir developer."
      },
      %{
        position: "after",
        content: """
        Always prefer OTP patterns and supervision trees.
        Use pattern matching over conditional branching.
        Cite the Elixir hexdocs when relevant.
        """
      }
    ]
  }
)

{:ok, response} = CopilotClient.send_and_wait(session_custom, %{
  message: "How do I implement a GenServer with state recovery?"
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Per-Agent Skills

**Scenario:** Configure skill directories and disable specific skills per agent.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a code assistant with limited skills.",
  skill_directories: [
    Path.expand("~/.copilot/skills"),
    "/project/.copilot-skills"
  ],
  disabled_skills: [
    "web-search",
    "image-generation"
  ]
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Refactor this module to use a supervision tree."
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Per-Agent Tool Visibility

**Scenario:** Hide specific tools from the model for a given session.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a read-only code reviewer.",
  excluded_tools: [
    "file_write",
    "shell_execute",
    "git_push"
  ]
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Review this module and suggest improvements."
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Runtime Request Headers

**Scenario:** Attach custom headers to individual send requests for tracing or routing.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a helpful assistant."
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Explain GenStage and Flow in Elixir.",
  request_headers: %{
    "X-Request-Id" => "req-abc-123",
    "X-Trace-Parent" => "00-traceid-spanid-01",
    "X-Custom-Routing" => "priority-queue"
  }
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Model Capabilities Override

**Scenario:** Override model capabilities for a session, such as vision or function calling.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are an assistant with extended capabilities.",
  model_capabilities: %{
    vision: true,
    function_calling: true,
    json_output: true,
    max_tokens: 8192
  }
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Analyze this architecture diagram."
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Config Discovery

**Scenario:** Enable automatic discovery of project-level configuration files.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link(
  enable_config_discovery: true
)

# The SDK automatically scans for:
#   .copilot/config.exs
#   .copilot/config.json
#   .github/copilot-config.yml
# in the workspace and its parents.

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a helpful assistant."
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "What configuration is active for this project?"
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Sub-Agent Streaming Events

**Scenario:** Subscribe to streaming events from sub-agents during orchestration.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are an orchestrator that delegates to sub-agents.",
  include_sub_agent_streaming_events: true
)

CopilotClient.send(session, %{
  message: "Research Elixir OTP patterns and write a tutorial.",
  streaming: true,
  on_event: fn event ->
    case event.type do
      "assistant.message_delta" ->
        IO.write(event.delta)

      "sub_agent.start" ->
        IO.puts("\n[Sub-agent started: #{event.agent_name}]")

      "sub_agent.message_delta" ->
        IO.write(event.delta)

      "sub_agent.end" ->
        IO.puts("\n[Sub-agent finished: #{event.agent_name}]")

      _ ->
        :ok
    end
  end
})

CopilotClient.stop(client)
```

## Session Metadata

**Scenario:** Retrieve metadata about an active session.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a helpful assistant."
)

{:ok, _response} = CopilotClient.send_and_wait(session, %{message: "Hello!"})

{:ok, metadata} = CopilotClient.get_session_metadata(session)

IO.puts("Session ID: #{metadata.session_id}")
IO.puts("Created at: #{metadata.created_at}")
IO.puts("Turn count: #{metadata.turn_count}")
IO.puts("Model:      #{metadata.model}")
IO.puts("Token usage: #{inspect(metadata.token_usage)}")

CopilotClient.stop(client)
```

## MCP Server Configuration

**Scenario:** Configure MCP (Model Context Protocol) servers using stdio and HTTP transports.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

# Stdio transport: launch a local MCP server as a child process
{:ok, session_stdio} = CopilotClient.create_session(client,
  system_prompt: "You are an assistant with MCP tools.",
  mcp_servers: [
    %{
      name: "local-tools",
      transport: "stdio",
      command: "node",
      args: ["./mcp-server/index.js"],
      env: %{"MCP_LOG_LEVEL" => "info"}
    }
  ]
)

# HTTP transport: connect to a remote MCP server
{:ok, session_http} = CopilotClient.create_session(client,
  system_prompt: "You are an assistant with remote tools.",
  mcp_servers: [
    %{
      name: "remote-tools",
      transport: "http",
      url: "https://mcp.example.com/v1",
      headers: %{
        "Authorization" => "Bearer #{System.get_env("MCP_API_KEY")}"
      }
    }
  ]
)

{:ok, response} = CopilotClient.send_and_wait(session_stdio, %{
  message: "Use the local tools to analyze the project."
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Image Generation

**Scenario:** Configure the response format for image generation tasks.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client,
  system_prompt: "You are a creative assistant that generates images.",
  model_capabilities: %{
    vision: true,
    image_generation: true
  }
)

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Generate an image of an Elixir phoenix rising from purple flames.",
  response_format: %{
    type: "image",
    size: "1024x1024",
    quality: "high"
  }
})

case response.images do
  images when is_list(images) and length(images) > 0 ->
    images
    |> Enum.with_index(1)
    |> Enum.each(fn {img, idx} ->
      IO.puts("Image #{idx} URL: #{img.url}")

      # Save base64 image data if returned inline
      if img.base64_data do
        path = "generated_#{idx}.png"
        File.write!(path, Base.decode64!(img.base64_data))
        IO.puts("Image #{idx} saved to #{path}")
      end
    end)

  _ ->
    IO.puts("Response: #{response.message}")
end

CopilotClient.stop(client)
```

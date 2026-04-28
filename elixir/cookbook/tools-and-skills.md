# Tools and Skills - Elixir

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Elixir.

## Defining a Simple Tool

**Scenario:** Expose an Elixir function as a tool that the model can invoke during a conversation.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

CopilotClient.define_tool("get_current_time", "Returns the current UTC date and time", fn _params ->
  DateTime.utc_now() |> DateTime.to_iso8601()
end)

{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a helpful assistant with access to tools."
})

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "What is the current time?"
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Tool with Parameters

**Scenario:** Define a tool that accepts structured parameters from the model.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

CopilotClient.define_tool("calculate", "Performs arithmetic operations", fn params ->
  a = params["a"]
  b = params["b"]
  op = params["operation"]

  result =
    case op do
      "add" -> {:ok, a + b}
      "subtract" -> {:ok, a - b}
      "multiply" -> {:ok, a * b}
      "divide" when b != 0 -> {:ok, a / b}
      "divide" -> {:error, "Division by zero"}
      _ -> {:error, "Unknown operation: #{op}"}
    end

  case result do
    {:ok, value} -> Jason.encode!(%{result: value})
    {:error, msg} -> Jason.encode!(%{error: msg})
  end
end)

{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a calculator. Use the calculate tool for math."
})

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "What is 42 multiplied by 17?"
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## File System Tools

**Scenario:** Give the model the ability to read files and list directories.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

CopilotClient.define_tool("read_file", "Reads the contents of a file", fn params ->
  path = params["path"]

  case File.read(path) do
    {:ok, content} ->
      truncated = String.slice(content, 0, 10_000)
      Jason.encode!(%{path: path, content: truncated, size: byte_size(content)})

    {:error, reason} ->
      Jason.encode!(%{error: "Cannot read #{path}: #{inspect(reason)}"})
  end
end)

CopilotClient.define_tool("list_directory", "Lists files in a directory", fn params ->
  dir = Map.get(params, "directory", ".")

  case File.ls(dir) do
    {:ok, entries} ->
      Jason.encode!(%{directory: dir, files: Enum.sort(entries)})

    {:error, reason} ->
      Jason.encode!(%{error: "Cannot list #{dir}: #{inspect(reason)}"})
  end
end)

CopilotClient.define_tool("file_info", "Returns metadata about a file", fn params ->
  path = params["path"]

  case File.stat(path) do
    {:ok, stat} ->
      Jason.encode!(%{
        path: path,
        size: stat.size,
        type: stat.type,
        modified: NaiveDateTime.to_iso8601(stat.mtime)
      })

    {:error, reason} ->
      Jason.encode!(%{error: "Cannot stat #{path}: #{inspect(reason)}"})
  end
end)

{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a file explorer. Use your tools to navigate and read files."
})

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "List the files in the current directory and show me the contents of mix.exs."
})

IO.puts(response.message)

CopilotClient.stop(client)
```

## Tool with State Using Agent

**Scenario:** Create tools that share mutable state via an Elixir Agent.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

# Start an Agent to hold our key-value store
{:ok, store} = Agent.start_link(fn -> %{} end)

CopilotClient.define_tool("kv_set", "Stores a value under a key", fn params ->
  Agent.update(store, &Map.put(&1, params["key"], params["value"]))
  Jason.encode!(%{status: "ok", key: params["key"]})
end)

CopilotClient.define_tool("kv_get", "Gets a value by key", fn params ->
  case Agent.get(store, &Map.get(&1, params["key"])) do
    nil -> Jason.encode!(%{error: "Key not found: #{params["key"]}"})
    value -> Jason.encode!(%{key: params["key"], value: value})
  end
end)

CopilotClient.define_tool("kv_list", "Lists all keys", fn _params ->
  keys = Agent.get(store, &Map.keys/1)
  Jason.encode!(%{keys: keys})
end)

CopilotClient.define_tool("kv_delete", "Deletes a key", fn params ->
  Agent.update(store, &Map.delete(&1, params["key"]))
  Jason.encode!(%{status: "deleted", key: params["key"]})
end)

{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a key-value store assistant. Use the kv_ tools to manage data."
})

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "Store my name as Alice and my role as Developer, then list all keys."
})

IO.puts(response.message)

Agent.stop(store)
CopilotClient.stop(client)
```

## Orchestrating Sub-Agents

**Scenario:** Use tool functions to delegate work to specialized sessions acting as sub-agents.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

# Create specialized sub-agent sessions
{:ok, code_agent} = CopilotClient.create_session(client, %{
  system_prompt: "You generate Elixir code. Output only code, no explanations."
})

{:ok, review_agent} = CopilotClient.create_session(client, %{
  system_prompt: "You review Elixir code for bugs and style issues. Be concise."
})

{:ok, test_agent} = CopilotClient.create_session(client, %{
  system_prompt: "You write ExUnit tests for Elixir code. Output only test code."
})

# Define tools that delegate to sub-agents
CopilotClient.define_tool("generate_code", "Generates Elixir code from a description", fn params ->
  {:ok, resp} = CopilotClient.send_and_wait(code_agent, %{message: params["request"]})
  resp.message
end)

CopilotClient.define_tool("review_code", "Reviews Elixir code for issues", fn params ->
  {:ok, resp} = CopilotClient.send_and_wait(review_agent, %{
    message: "Review this code:\n#{params["code"]}"
  })
  resp.message
end)

CopilotClient.define_tool("generate_tests", "Generates ExUnit tests for code", fn params ->
  {:ok, resp} = CopilotClient.send_and_wait(test_agent, %{
    message: "Write tests for:\n#{params["code"]}"
  })
  resp.message
end)

# Orchestrator session coordinates the sub-agents
{:ok, orchestrator} = CopilotClient.create_session(client, %{
  system_prompt: """
  You orchestrate development tasks. Use these tools in sequence:
  1. generate_code to create code
  2. review_code to review it
  3. generate_tests to create tests
  """
})

{:ok, response} = CopilotClient.send_and_wait(orchestrator, %{
  message: "Build a module that validates email addresses with a regex."
})

IO.puts(response.message)

CopilotClient.stop(client)
```

## Best Practices

1. **Return JSON from tool handlers** using `Jason.encode!/1` for structured, parseable output.
2. **Use pattern matching** in tool handlers to validate parameters and handle edge cases.
3. **Use Agents** for tools that need mutable shared state across invocations.
4. **Limit output size** with `String.slice/3` to stay within token limits.
5. **Keep tool names descriptive** using a `namespace_action` pattern (e.g., `kv_set`, `kv_get`).
6. **Use sub-agent orchestration** to break complex workflows into specialized, composable sessions.
7. **Handle errors inside tool handlers** and return error JSON rather than raising exceptions.

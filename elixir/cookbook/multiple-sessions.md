# Multiple Sessions - Elixir

Patterns for managing multiple independent Copilot conversations simultaneously in Elixir.

## Basic Multi-Session Setup

**Scenario:** Run multiple independent conversations from a single client, each with its own system prompt.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

{:ok, code_session} = CopilotClient.create_session(client, %{
  system_prompt: "You are an expert Elixir programmer. Provide concise code."
})

{:ok, docs_session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a technical writer for Elixir libraries."
})

{:ok, code_resp} = CopilotClient.send_and_wait(code_session, %{
  message: "Write a GenServer that caches HTTP responses."
})
IO.puts("Code:\n#{code_resp.message}")

{:ok, docs_resp} = CopilotClient.send_and_wait(docs_session, %{
  message: "Write hexdocs for an HTTP cache GenServer."
})
IO.puts("Docs:\n#{docs_resp.message}")

CopilotClient.stop(client)
```

## Session Registry with GenServer

**Scenario:** Manage a dynamic pool of named sessions using a dedicated GenServer process.

```elixir
defmodule SessionRegistry do
  use GenServer

  # Client API

  def start_link(copilot_client) do
    GenServer.start_link(__MODULE__, copilot_client, name: __MODULE__)
  end

  def get_or_create(name, config) do
    GenServer.call(__MODULE__, {:get_or_create, name, config})
  end

  def send_message(name, message) do
    GenServer.call(__MODULE__, {:send, name, message}, 60_000)
  end

  def remove(name) do
    GenServer.cast(__MODULE__, {:remove, name})
  end

  def list_sessions do
    GenServer.call(__MODULE__, :list)
  end

  # Server callbacks

  @impl true
  def init(copilot_client) do
    {:ok, %{client: copilot_client, sessions: %{}}}
  end

  @impl true
  def handle_call({:get_or_create, name, config}, _from, state) do
    case Map.get(state.sessions, name) do
      nil ->
        case CopilotSDK.CopilotClient.create_session(state.client, config) do
          {:ok, session} ->
            new_sessions = Map.put(state.sessions, name, session)
            {:reply, {:ok, name}, %{state | sessions: new_sessions}}

          {:error, _} = error ->
            {:reply, error, state}
        end

      _session ->
        {:reply, {:ok, name}, state}
    end
  end

  @impl true
  def handle_call({:send, name, message}, _from, state) do
    case Map.get(state.sessions, name) do
      nil ->
        {:reply, {:error, :session_not_found}, state}

      session ->
        result = CopilotSDK.CopilotClient.send_and_wait(session, %{message: message})
        {:reply, result, state}
    end
  end

  @impl true
  def handle_call(:list, _from, state) do
    {:reply, Map.keys(state.sessions), state}
  end

  @impl true
  def handle_cast({:remove, name}, state) do
    {:noreply, %{state | sessions: Map.delete(state.sessions, name)}}
  end
end

# Usage
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()
{:ok, _pid} = SessionRegistry.start_link(client)

SessionRegistry.get_or_create("frontend", %{system_prompt: "You are a frontend expert."})
SessionRegistry.get_or_create("backend", %{system_prompt: "You are a backend expert."})

{:ok, resp} = SessionRegistry.send_message("frontend", "How do I use LiveView?")
IO.puts(resp.message)

IO.inspect(SessionRegistry.list_sessions(), label: "Active sessions")

CopilotClient.stop(client)
```

## Concurrent Requests with Task.async

**Scenario:** Send messages to multiple sessions concurrently and collect all responses.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

sessions =
  Enum.map(
    [
      {"concise", "Give answers in under 50 words."},
      {"detailed", "Give thorough, detailed explanations."},
      {"creative", "Use analogies and metaphors in your answers."}
    ],
    fn {name, prompt} ->
      {:ok, session} = CopilotClient.create_session(client, %{system_prompt: prompt})
      {name, session}
    end
  )

question = "What is pattern matching?"

# Send to all sessions concurrently
tasks =
  Enum.map(sessions, fn {name, session} ->
    Task.async(fn ->
      case CopilotClient.send_and_wait(session, %{message: question}) do
        {:ok, response} -> {name, response.message}
        {:error, reason} -> {name, "Error: #{inspect(reason)}"}
      end
    end)
  end)

# Collect all results (30-second timeout per task)
results = Task.await_many(tasks, 30_000)

Enum.each(results, fn {name, message} ->
  IO.puts("=== #{name} ===")
  IO.puts(message)
  IO.puts("")
end)

CopilotClient.stop(client)
```

## Per-User Sessions with Agent

**Scenario:** Use an Agent to maintain a map of user-to-session mappings for a multi-user application.

```elixir
alias CopilotSDK.CopilotClient

defmodule UserSessions do
  def start_link(copilot_client) do
    Agent.start_link(fn -> %{client: copilot_client, users: %{}} end, name: __MODULE__)
  end

  def chat(user_id, message) do
    session = get_or_create_session(user_id)
    CopilotClient.send_and_wait(session, %{message: message})
  end

  def end_session(user_id) do
    Agent.update(__MODULE__, fn state ->
      %{state | users: Map.delete(state.users, user_id)}
    end)
  end

  def active_users do
    Agent.get(__MODULE__, fn state -> Map.keys(state.users) end)
  end

  defp get_or_create_session(user_id) do
    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.users, user_id) do
        nil ->
          {:ok, session} =
            CopilotClient.create_session(state.client, %{
              system_prompt: "You are a helpful assistant for user #{user_id}."
            })

          new_state = %{state | users: Map.put(state.users, user_id, session)}
          {session, new_state}

        session ->
          {session, state}
      end
    end)
  end
end

# Usage
{:ok, client} = CopilotClient.start_link()
{:ok, _} = UserSessions.start_link(client)

{:ok, resp1} = UserSessions.chat("alice", "How do I use Ecto?")
IO.puts("Alice: #{resp1.message}")

{:ok, resp2} = UserSessions.chat("bob", "How do I use Phoenix channels?")
IO.puts("Bob: #{resp2.message}")

IO.inspect(UserSessions.active_users(), label: "Active users")

UserSessions.end_session("alice")

CopilotClient.stop(client)
```

## Best Practices

1. **Use GenServer or Agent** to manage session state in a concurrent-safe way.
2. **Use `Task.async` / `Task.await_many`** to send messages to multiple sessions concurrently.
3. **Share a single client** across all sessions to reuse the underlying connection.
4. **Supervise session processes** in production to handle crashes gracefully.
5. **Set appropriate timeouts** on `GenServer.call` and `Task.await` to prevent indefinite blocking.
6. **Clean up sessions** by removing them from the registry when a user disconnects or a task completes.

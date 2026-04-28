# Persisting Sessions - Elixir

Patterns for saving and resuming Copilot sessions across application restarts in Elixir.

## Basic Save and Load with JSON

**Scenario:** Save session state to a JSON file and restore it later.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

# Create a session and have a conversation
{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a helpful assistant."
})

{:ok, _resp} = CopilotClient.send_and_wait(session, %{
  message: "Remember that my project is called Phoenix Tracker."
})

# Save session state to file
state = CopilotClient.get_session_state(session)
File.write!("session_state.json", Jason.encode!(state))
IO.puts("Session saved.")

CopilotClient.stop(client)
```

## Resuming a Saved Session

**Scenario:** Load a previously saved session state and continue the conversation.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

session =
  case File.read("session_state.json") do
    {:ok, content} ->
      saved_state = Jason.decode!(content)
      IO.puts("Resuming saved session...")

      {:ok, session} = CopilotClient.create_session(client, %{
        system_prompt: "You are a helpful assistant.",
        state: saved_state
      })
      session

    {:error, :enoent} ->
      IO.puts("Starting fresh session...")

      {:ok, session} = CopilotClient.create_session(client, %{
        system_prompt: "You are a helpful assistant."
      })
      session
  end

{:ok, response} = CopilotClient.send_and_wait(session, %{
  message: "What is my project called?"
})

IO.puts("Response: #{response.message}")

CopilotClient.stop(client)
```

## Session Store GenServer

**Scenario:** Manage multiple named sessions with persistence using a GenServer.

```elixir
defmodule SessionStore do
  use GenServer

  def start_link(opts) do
    directory = Keyword.get(opts, :directory, "./sessions")
    GenServer.start_link(__MODULE__, directory, name: __MODULE__)
  end

  def save(name, session) do
    GenServer.call(__MODULE__, {:save, name, session})
  end

  def load(name) do
    GenServer.call(__MODULE__, {:load, name})
  end

  def delete(name) do
    GenServer.call(__MODULE__, {:delete, name})
  end

  def list do
    GenServer.call(__MODULE__, :list)
  end

  # Server implementation

  @impl true
  def init(directory) do
    File.mkdir_p!(directory)
    {:ok, %{directory: directory}}
  end

  @impl true
  def handle_call({:save, name, session}, _from, state) do
    session_state = CopilotSDK.CopilotClient.get_session_state(session)
    path = Path.join(state.directory, "#{name}.json")
    File.write!(path, Jason.encode!(session_state))
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:load, name}, _from, state) do
    path = Path.join(state.directory, "#{name}.json")

    result =
      case File.read(path) do
        {:ok, content} -> {:ok, Jason.decode!(content)}
        {:error, :enoent} -> {:error, :not_found}
      end

    {:reply, result, state}
  end

  @impl true
  def handle_call({:delete, name}, _from, state) do
    path = Path.join(state.directory, "#{name}.json")
    File.rm(path)
    {:reply, :ok, state}
  end

  @impl true
  def handle_call(:list, _from, state) do
    names =
      state.directory
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".json"))
      |> Enum.map(&String.replace_suffix(&1, ".json", ""))

    {:reply, names, state}
  end
end

# Usage
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()
{:ok, _} = SessionStore.start_link(directory: "./my_sessions")

# Create and save
{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a project planning assistant."
})

{:ok, _} = CopilotClient.send_and_wait(session, %{
  message: "We are building a real-time dashboard."
})

SessionStore.save("dashboard-project", session)

# Later, restore
case SessionStore.load("dashboard-project") do
  {:ok, saved_state} ->
    {:ok, restored} = CopilotClient.create_session(client, %{
      system_prompt: "You are a project planning assistant.",
      state: saved_state
    })

    {:ok, resp} = CopilotClient.send_and_wait(restored, %{
      message: "What are we building?"
    })
    IO.puts(resp.message)

  {:error, :not_found} ->
    IO.puts("No saved session found.")
end

IO.inspect(SessionStore.list(), label: "Saved sessions")

CopilotClient.stop(client)
```

## Auto-Save After Every Turn

**Scenario:** Automatically persist session state after every message exchange.

```elixir
alias CopilotSDK.CopilotClient

defmodule AutoSaveSession do
  def send_and_save(session, message, save_path) do
    case CopilotClient.send_and_wait(session, %{message: message}) do
      {:ok, response} ->
        state = CopilotClient.get_session_state(session)
        File.write!(save_path, Jason.encode!(state))
        {:ok, response}

      {:error, _} = error ->
        error
    end
  end
end

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a helpful assistant."
})

save_path = "./autosave_session.json"

{:ok, _} = AutoSaveSession.send_and_save(session, "Hello!", save_path)
{:ok, resp} = AutoSaveSession.send_and_save(session, "What did I just say?", save_path)

IO.puts("Response: #{resp.message}")
IO.puts("Session auto-saved to #{save_path}")

CopilotClient.stop(client)
```

## Session State with Metadata

**Scenario:** Store metadata (timestamps, description) alongside the session state.

```elixir
alias CopilotSDK.CopilotClient

defmodule SessionEnvelope do
  def save(session, path, metadata \\ %{}) do
    envelope = %{
      metadata: metadata,
      state: CopilotClient.get_session_state(session),
      saved_at: DateTime.utc_now() |> DateTime.to_iso8601()
    }

    File.write!(path, Jason.encode!(envelope, pretty: true))
  end

  def load(path) do
    case File.read(path) do
      {:ok, content} -> {:ok, Jason.decode!(content)}
      {:error, reason} -> {:error, reason}
    end
  end
end

{:ok, client} = CopilotClient.start_link()

{:ok, session} = CopilotClient.create_session(client, %{
  system_prompt: "You are a helpful assistant."
})

{:ok, _} = CopilotClient.send_and_wait(session, %{message: "Hello!"})

SessionEnvelope.save(session, "session_with_meta.json", %{
  description: "Cookbook example session",
  created_by: "cookbook",
  turn_count: 1
})

case SessionEnvelope.load("session_with_meta.json") do
  {:ok, envelope} ->
    IO.puts("Saved at: #{envelope["saved_at"]}")
    IO.puts("Description: #{envelope["metadata"]["description"]}")

  {:error, reason} ->
    IO.puts(:stderr, "Failed to load: #{inspect(reason)}")
end

CopilotClient.stop(client)
```

## Best Practices

1. **Use `Jason`** for JSON encoding/decoding (add `{:jason, "~> 1.4"}` to your `mix.exs` dependencies).
2. **Use a GenServer** for concurrent-safe session store operations.
3. **Auto-save after each turn** in production to minimize data loss on crashes.
4. **Store metadata** alongside state for easier management and debugging.
5. **Handle `:enoent` errors** gracefully when loading sessions that may not exist.
6. **Use `File.mkdir_p!/1`** to ensure the storage directory exists before writing.
7. **Supervise the session store** GenServer so it restarts automatically on failure.

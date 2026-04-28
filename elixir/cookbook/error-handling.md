# Error Handling - Elixir

Patterns for handling errors gracefully in the Copilot SDK for Elixir, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with Pattern Matching

**Scenario:** Use Elixir's `{:ok, result}` / `{:error, reason}` tuples to handle SDK responses.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link(cli_path: System.get_env("COPILOT_CLI_PATH"))

case CopilotClient.create_session(client, %{
  system_prompt: "You are a helpful assistant."
}) do
  {:ok, session} ->
    case CopilotClient.send_and_wait(session, %{message: "Hello!"}) do
      {:ok, response} ->
        IO.puts("Response: #{response.message}")

      {:error, reason} ->
        IO.puts(:stderr, "Failed to send message: #{inspect(reason)}")
    end

  {:error, reason} ->
    IO.puts(:stderr, "Failed to create session: #{inspect(reason)}")
end

CopilotClient.stop(client)
```

## Using `with` for Multi-Step Pipelines

**Scenario:** Chain multiple SDK calls cleanly, short-circuiting on the first error.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

result =
  with {:ok, session} <- CopilotClient.create_session(client, %{
         system_prompt: "You are a helpful assistant."
       }),
       {:ok, response} <- CopilotClient.send_and_wait(session, %{
         message: "Explain pattern matching in Elixir."
       }) do
    {:ok, response.message}
  end

case result do
  {:ok, message} ->
    IO.puts("Got response: #{message}")

  {:error, reason} ->
    IO.puts(:stderr, "Pipeline failed: #{inspect(reason)}")
end

CopilotClient.stop(client)
```

## Retry with Exponential Backoff

**Scenario:** Retry transient failures with increasing delays between attempts.

```elixir
alias CopilotSDK.CopilotClient

defmodule Retry do
  def with_backoff(fun, opts \\ []) do
    max_attempts = Keyword.get(opts, :max_attempts, 3)
    base_delay_ms = Keyword.get(opts, :base_delay_ms, 1_000)

    do_retry(fun, 1, max_attempts, base_delay_ms, nil)
  end

  defp do_retry(_fun, attempt, max, _delay, last_error) when attempt > max do
    {:error, {:max_retries_exceeded, last_error}}
  end

  defp do_retry(fun, attempt, max, base_delay, _last_error) do
    case fun.() do
      {:ok, result} ->
        {:ok, result}

      {:error, reason} ->
        if attempt < max do
          delay = base_delay * :math.pow(2, attempt - 1) |> round()
          IO.puts(:stderr, "Attempt #{attempt}/#{max} failed: #{inspect(reason)}. Retrying in #{delay}ms...")
          Process.sleep(delay)
        end

        do_retry(fun, attempt + 1, max, base_delay, reason)
    end
  end
end

{:ok, client} = CopilotClient.start_link()
{:ok, session} = CopilotClient.create_session(client, %{system_prompt: "You are a helpful assistant."})

case Retry.with_backoff(fn ->
  CopilotClient.send_and_wait(session, %{message: "Hello!", timeout: 30_000})
end, max_attempts: 3, base_delay_ms: 1_000) do
  {:ok, response} ->
    IO.puts("Response: #{response.message}")

  {:error, {:max_retries_exceeded, reason}} ->
    IO.puts(:stderr, "All retries exhausted. Last error: #{inspect(reason)}")
end

CopilotClient.stop(client)
```

## Supervised Client with Automatic Restart

**Scenario:** Use an OTP Supervisor so the client restarts automatically if it crashes.

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {CopilotSDK.CopilotClient, name: MyApp.Copilot, cli_path: System.get_env("COPILOT_CLI_PATH")}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# Now the client is supervised and restarts on failure
# Use it anywhere in your application:
alias CopilotSDK.CopilotClient

case CopilotClient.create_session(MyApp.Copilot, %{
  system_prompt: "You are a helpful assistant."
}) do
  {:ok, session} ->
    {:ok, response} = CopilotClient.send_and_wait(session, %{message: "Hello!"})
    IO.puts(response.message)

  {:error, reason} ->
    IO.puts(:stderr, "Session creation failed: #{inspect(reason)}")
end
```

## Custom Error Types

**Scenario:** Classify SDK errors into domain-specific error types for targeted handling.

```elixir
alias CopilotSDK.CopilotClient

defmodule CopilotErrors do
  def classify({:error, %{type: "timeout"}}), do: {:timeout, "Request timed out"}
  def classify({:error, %{type: "connection_refused"}}), do: {:connection, "Cannot connect to CLI"}
  def classify({:error, %{type: "auth_failed"}}), do: {:auth, "Authentication failed"}
  def classify({:error, reason}) when is_binary(reason) do
    cond do
      String.contains?(reason, "timeout") -> {:timeout, reason}
      String.contains?(reason, "connection") -> {:connection, reason}
      String.contains?(reason, "auth") -> {:auth, reason}
      true -> {:unknown, reason}
    end
  end
  def classify({:error, reason}), do: {:unknown, inspect(reason)}

  def handle_classified({:timeout, msg}) do
    IO.puts(:stderr, "Timeout: #{msg}. Consider increasing the timeout value.")
  end

  def handle_classified({:connection, msg}) do
    IO.puts(:stderr, "Connection error: #{msg}. Is the Copilot CLI running?")
  end

  def handle_classified({:auth, msg}) do
    IO.puts(:stderr, "Auth error: #{msg}. Check your credentials.")
  end

  def handle_classified({:unknown, msg}) do
    IO.puts(:stderr, "Unexpected error: #{msg}")
  end
end

{:ok, client} = CopilotClient.start_link()

case CopilotClient.create_session(client, %{system_prompt: "You are a helpful assistant."}) do
  {:ok, session} ->
    case CopilotClient.send_and_wait(session, %{message: "Hello!"}) do
      {:ok, response} -> IO.puts(response.message)
      error -> error |> CopilotErrors.classify() |> CopilotErrors.handle_classified()
    end

  error ->
    error |> CopilotErrors.classify() |> CopilotErrors.handle_classified()
end

CopilotClient.stop(client)
```

## Cleanup with try/after

**Scenario:** Ensure the client is always stopped, even when an exception is raised.

```elixir
alias CopilotSDK.CopilotClient

{:ok, client} = CopilotClient.start_link()

try do
  {:ok, session} = CopilotClient.create_session(client, %{
    system_prompt: "You are a helpful assistant."
  })

  {:ok, response} = CopilotClient.send_and_wait(session, %{
    message: "Explain try/after in Elixir."
  })

  IO.puts("Response: #{response.message}")
after
  # Always runs, even if an exception was raised
  CopilotClient.stop(client)
  IO.puts(:stderr, "Client stopped.")
end
```

## Best Practices

1. **Use `with` chains** for multi-step SDK pipelines that should short-circuit on failure.
2. **Pattern match on `{:ok, _}` and `{:error, _}`** tuples rather than using try/rescue for expected errors.
3. **Use OTP Supervisors** in production to restart the client process on crashes.
4. **Implement retry with backoff** for transient network errors.
5. **Classify errors** into domain types (timeout, connection, auth) for targeted recovery.
6. **Use `try/after`** to guarantee resource cleanup when exceptions may occur.
7. **Log to `:stderr`** with `IO.puts(:stderr, ...)` to keep stdout clean for program output.

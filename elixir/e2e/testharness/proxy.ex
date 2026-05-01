defmodule CopilotE2E.TestHarness.Proxy do
  @moduledoc """
  Replaying CAPI proxy for E2E tests.

  Manages a child process that acts as a replaying proxy to AI endpoints.
  Spawns the shared test harness server from `test/harness/server.ts`.
  """

  use GenServer

  require Logger

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc "Start the proxy GenServer (does not yet launch the node process)."
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @doc """
  Launch the replay proxy server and return its URL.

  Spawns `npx tsx test/harness/server.ts` and waits for the
  `Listening: http://...` line on stdout.
  """
  @spec start_proxy(GenServer.server()) :: {:ok, String.t()} | {:error, any()}
  def start_proxy(server) do
    GenServer.call(server, :start_proxy, 30_000)
  end

  @doc """
  Gracefully shut down the replay proxy.

  Sends a POST to `/stop` and waits for the OS process to exit.
  """
  @spec stop_proxy(GenServer.server(), keyword()) :: :ok
  def stop_proxy(server, opts \\ []) do
    GenServer.call(server, {:stop_proxy, opts}, 15_000)
  catch
    :exit, _ -> :ok
  end

  @doc "Return the proxy URL (nil if not started)."
  @spec url(GenServer.server()) :: String.t() | nil
  def url(server) do
    GenServer.call(server, :url)
  end

  @doc """
  Configure the proxy for a specific test snapshot.

  Sends a POST to `/config` with the snapshot file path and working directory.
  """
  @spec configure(GenServer.server(), String.t(), String.t()) :: :ok | {:error, any()}
  def configure(server, file_path, work_dir) do
    GenServer.call(server, {:configure, file_path, work_dir}, 10_000)
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  defmodule State do
    @moduledoc false
    defstruct port: nil, proxy_url: nil, os_pid: nil
  end

  @impl true
  def init(:ok) do
    {:ok, %State{}}
  end

  @impl true
  def handle_call(:start_proxy, _from, %State{proxy_url: url} = state) when url != nil do
    {:reply, {:ok, url}, state}
  end

  def handle_call(:start_proxy, _from, %State{proxy_url: nil} = state) do
    case launch_proxy() do
      {:ok, port, proxy_url, os_pid} ->
        new_state = %State{state | port: port, proxy_url: proxy_url, os_pid: os_pid}
        {:reply, {:ok, proxy_url}, new_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:stop_proxy, opts}, _from, state) do
    new_state = do_stop(state, opts)
    {:reply, :ok, new_state}
  end

  def handle_call(:url, _from, state) do
    {:reply, state.proxy_url, state}
  end

  def handle_call({:configure, file_path, work_dir}, _from, state) do
    if state.proxy_url do
      body = Jason.encode!(%{"filePath" => file_path, "workDir" => work_dir})
      url = "#{state.proxy_url}/config"

      case http_post(url, body) do
        {:ok, 200, _body} -> {:reply, :ok, state}
        {:ok, status, _body} -> {:reply, {:error, "config failed with status #{status}"}, state}
        {:error, reason} -> {:reply, {:error, reason}, state}
      end
    else
      {:reply, {:error, "proxy not started"}, state}
    end
  end

  @impl true
  def handle_info({port, {:data, _data}}, %State{port: port} = state) do
    # Drain stdout/stderr from the proxy process after startup
    {:noreply, state}
  end

  def handle_info({port, {:exit_status, _status}}, %State{port: port} = state) do
    Logger.debug("Proxy process exited")
    {:noreply, %State{state | port: nil, proxy_url: nil, os_pid: nil}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    do_stop(state, [])
    :ok
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp launch_proxy do
    server_path = server_ts_path()

    unless File.exists?(server_path) do
      raise "Test harness server not found at #{server_path}"
    end

    harness_dir = Path.dirname(server_path)

    # On Windows, spawn npx via cmd.exe so that npx.cmd is resolved
    {executable, args} =
      case :os.type() do
        {:win32, _} ->
          {System.find_executable("cmd") || "cmd.exe",
           ["/c", "npx", "tsx", server_path]}

        _ ->
          npx = System.find_executable("npx") || "npx"
          {npx, ["tsx", server_path]}
      end

    port_opts = [
      :binary,
      :exit_status,
      :use_stdio,
      :stderr_to_stdout,
      {:args, args},
      {:cd, String.to_charlist(harness_dir)}
    ]

    port = Port.open({:spawn_executable, String.to_charlist(executable)}, port_opts)

    case read_listening_url(port) do
      {:ok, proxy_url} ->
        # Extract OS PID from port info for cleanup
        os_pid =
          case Port.info(port, :os_pid) do
            {:os_pid, pid} -> pid
            _ -> nil
          end

        {:ok, port, proxy_url, os_pid}

      {:error, reason} ->
        safe_close_port(port)
        {:error, reason}
    end
  end

  defp read_listening_url(port) do
    read_listening_url(port, "", 15_000)
  end

  defp read_listening_url(_port, _buffer, timeout) when timeout <= 0 do
    {:error, "timed out waiting for proxy to start"}
  end

  defp read_listening_url(port, buffer, timeout) do
    start = System.monotonic_time(:millisecond)

    receive do
      {^port, {:data, data}} ->
        new_buffer = buffer <> data

        case Regex.run(~r/Listening: (http:\/\/[^\s]+)/, new_buffer) do
          [_, url] ->
            {:ok, String.trim(url)}

          nil ->
            elapsed = System.monotonic_time(:millisecond) - start
            read_listening_url(port, new_buffer, timeout - elapsed)
        end

      {^port, {:exit_status, status}} ->
        {:error, "proxy exited with status #{status} before printing URL. Output: #{buffer}"}
    after
      timeout ->
        {:error, "timed out waiting for proxy to start. Output so far: #{buffer}"}
    end
  end

  defp do_stop(%State{proxy_url: nil} = state, _opts), do: state

  defp do_stop(%State{} = state, opts) do
    skip_cache = Keyword.get(opts, :skip_writing_cache, false)

    # Send graceful stop request
    if state.proxy_url do
      stop_url =
        if skip_cache,
          do: "#{state.proxy_url}/stop?skipWritingCache=true",
          else: "#{state.proxy_url}/stop"

      try do
        http_post(stop_url, "")
      rescue
        _ -> :ok
      catch
        _, _ -> :ok
      end
    end

    # Wait briefly for the process to exit, then force-close the port
    if state.port do
      receive do
        {port, {:exit_status, _}} when port == state.port -> :ok
      after
        3_000 -> safe_close_port(state.port)
      end
    end

    %State{state | port: nil, proxy_url: nil, os_pid: nil}
  end

  defp safe_close_port(port) do
    try do
      Port.close(port)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp server_ts_path do
    # From elixir/e2e/testharness/ go up to repo root: ../../../test/harness/server.ts
    Path.expand("../../../test/harness/server.ts", __DIR__)
  end

  # Minimal HTTP POST using Erlang's built-in :httpc client (no extra deps)
  defp http_post(url, body) do
    :inets.start()
    :ssl.start()

    headers = [
      {~c"content-type", ~c"application/json"}
    ]

    request = {String.to_charlist(url), headers, ~c"application/json", String.to_charlist(body)}

    case :httpc.request(:post, request, [{:timeout, 5_000}], []) do
      {:ok, {{_, status, _}, _headers, resp_body}} ->
        {:ok, status, List.to_string(resp_body)}

      {:error, reason} ->
        {:error, reason}
    end
  end
end

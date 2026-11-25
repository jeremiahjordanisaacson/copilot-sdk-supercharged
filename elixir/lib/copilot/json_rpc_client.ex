defmodule Copilot.JsonRpcClient do
  @moduledoc ~S"""
  GenServer-based JSON-RPC 2.0 client over the stdio of the Copilot CLI.

  The client opens the CLI port itself (see `start_link/2`), so it is the port
  owner and receives everything the CLI writes to stdout, from the first byte.
  The port runs in stream mode, and the CLI's stderr is not read (it goes to
  the stderr of the Erlang VM), so only JSON-RPC frames arrive.

  Messages are framed with `Content-Length` headers following the LSP base protocol:

      Content-Length: <byte-length>\r\n
      \r\n
      <JSON body>

  Frames are read like vscode-jsonrpc's `StreamMessageReader`, which the CLI
  uses: the header block ends at the first `\r\n\r\n`, the `Content-Length`
  header name is case-insensitive, other headers are ignored, the length counts
  bytes, and a header block without a valid `Content-Length` is dropped. A
  frame may arrive in several chunks, and one chunk may carry several frames.

  The client supports:
  - Outgoing requests (with responses matched by id)
  - Outgoing notifications (no response expected)
  - Incoming notifications from the server
  - Incoming requests from the server (tool.call, hooks.invoke, etc.), each
    handled in its own process; this client writes the response
  """

  use GenServer
  require Logger

  @type request_id :: String.t()

  @typedoc """
  The CLI to talk to: `{:spawn_executable, executable, port_settings}` for a
  port this client opens, or an already open port.
  """
  @type target :: {:spawn_executable, :file.name_all(), list()} | port()

  @closed %{code: -1, message: "CLI process exited"}
  @not_running %{code: -1, message: "JSON-RPC client is not running"}

  defmodule State do
    @moduledoc false
    @type t :: %__MODULE__{
            port: port() | nil,
            buffer: binary(),
            next_id: non_neg_integer(),
            pending: %{String.t() => GenServer.from()},
            notification_handler: (String.t(), map() -> :ok) | nil,
            request_handlers: %{String.t() => (map() -> map())},
            os_pid: non_neg_integer() | nil
          }
    defstruct port: nil,
              buffer: <<>>,
              next_id: 1,
              pending: %{},
              notification_handler: nil,
              request_handlers: %{},
              os_pid: nil
  end

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Start the JSON-RPC client linked to the calling process.

  With `{:spawn_executable, executable, port_settings}` the client opens the
  port itself: binary stdio in stream mode with exit status reporting, plus
  `port_settings` such as `{:args, args}`, `{:env, env}` and `{:cd, dir}`. If
  the executable cannot be started, this returns
  `{:error, {:spawn_failed, reason}}`.

  An already open `port` is also accepted. It is connected to the new client
  with `Port.connect/2`, but any data the port delivered before that stays in
  the mailbox of its previous owner.
  """
  @spec start_link(target(), keyword()) :: GenServer.on_start()
  def start_link(target, opts \\ [])

  def start_link({:spawn_executable, executable, port_settings}, opts) do
    with {:ok, pid} <- GenServer.start_link(__MODULE__, nil, opts) do
      case GenServer.call(pid, {:open_port, executable, port_settings}, 30_000) do
        :ok ->
          {:ok, pid}

        {:error, _} = error ->
          stop(pid)
          error
      end
    end
  end

  def start_link(port, opts) when is_port(port) do
    GenServer.start_link(__MODULE__, port, opts)
  end

  @doc """
  Send a JSON-RPC request and wait for the response.

  Returns `{:ok, result}`, or `{:error, error}` with the server's error object.
  Returns `{:error, :timeout}` when no response arrives within `timeout`, and
  `{:error, %{code: -1, message: message}}` when the CLI exits or this client
  is not running.
  """
  @spec request(GenServer.server(), String.t(), map(), timeout()) ::
          {:ok, any()} | {:error, any()}
  def request(client, method, params \\ %{}, timeout \\ 30_000) do
    GenServer.call(client, {:request, method, params}, timeout)
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
    :exit, _ -> {:error, @not_running}
  end

  @doc """
  Send a JSON-RPC notification (no response expected).
  """
  @spec notify(GenServer.server(), String.t(), map()) :: :ok
  def notify(client, method, params \\ %{}) do
    GenServer.cast(client, {:notify, method, params})
  end

  @doc """
  Register a handler for incoming notifications from the server.

  The handler receives `(method, params)` and is called in the GenServer process.
  """
  @spec set_notification_handler(GenServer.server(), (String.t(), map() -> :ok) | nil) :: :ok
  def set_notification_handler(client, handler) do
    GenServer.cast(client, {:set_notification_handler, handler})
  end

  @doc """
  Register a handler for an incoming request method (e.g. `"tool.call"`).

  The handler receives `params` and must return a result map. It runs in its
  own process.
  """
  @spec set_request_handler(GenServer.server(), String.t(), (map() -> map()) | nil) :: :ok
  def set_request_handler(client, method, handler) do
    GenServer.cast(client, {:set_request_handler, method, handler})
  end

  @doc "Stop the client and close the CLI port."
  @spec stop(GenServer.server()) :: :ok
  def stop(client) do
    GenServer.stop(client, :normal)
  catch
    :exit, _ -> :ok
  end

  # ---------------------------------------------------------------------------
  # GenServer Callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(port) do
    # Port failures arrive as messages, and terminate/2 closes the port when
    # the owning process stops.
    Process.flag(:trap_exit, true)

    if is_port(port) do
      true = Port.connect(port, self())
      {:ok, %State{port: port, os_pid: os_pid(port)}}
    else
      {:ok, %State{}}
    end
  end

  @impl true
  def handle_call({:open_port, executable, port_settings}, _from, %State{port: nil} = state) do
    settings = [:binary, :stream, :exit_status, :use_stdio, :hide | port_settings]

    try do
      port = Port.open({:spawn_executable, executable}, settings)
      {:reply, :ok, %{state | port: port, os_pid: os_pid(port)}}
    catch
      :error, reason -> {:reply, {:error, {:spawn_failed, reason}}, state}
    end
  end

  def handle_call({:open_port, _executable, _port_settings}, _from, state) do
    {:reply, {:error, :already_open}, state}
  end

  def handle_call({:request, method, params}, from, %State{} = state) do
    id = Integer.to_string(state.next_id)

    message = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "method" => method,
      "params" => params
    }

    state = %{state | next_id: state.next_id + 1}

    case send_message(state.port, message) do
      :ok -> {:noreply, %{state | pending: Map.put(state.pending, id, from)}}
      {:error, _} = error -> {:reply, error, state}
    end
  end

  @impl true
  def handle_cast({:notify, method, params}, %State{} = state) do
    _ = send_message(state.port, %{"jsonrpc" => "2.0", "method" => method, "params" => params})
    {:noreply, state}
  end

  # Responses to server requests, encoded by the handler processes.
  def handle_cast({:send_frame, frame}, %State{} = state) do
    _ = send_frame(state.port, frame)
    {:noreply, state}
  end

  def handle_cast({:set_notification_handler, handler}, state) do
    {:noreply, %{state | notification_handler: handler}}
  end

  def handle_cast({:set_request_handler, method, handler}, state) do
    handlers =
      if handler do
        Map.put(state.request_handlers, method, handler)
      else
        Map.delete(state.request_handlers, method)
      end

    {:noreply, %{state | request_handlers: handlers}}
  end

  @impl true
  def handle_info({port, {:data, data}}, %State{port: port} = state) do
    {frames, rest} = parse_frames(state.buffer <> data)
    {:noreply, Enum.reduce(frames, %{state | buffer: rest}, &handle_frame/2)}
  end

  def handle_info({port, {:exit_status, status}}, %State{port: port} = state) do
    Logger.debug("Copilot CLI exited with status #{status}")
    {:stop, :normal, fail_pending(%{state | port: nil})}
  end

  def handle_info({:EXIT, port, reason}, %State{port: port} = state) do
    Logger.debug("Copilot CLI port closed: #{inspect(reason)}")
    {:stop, :normal, fail_pending(%{state | port: nil})}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %State{port: port} = state) do
    _ = fail_pending(state)

    if is_port(port) do
      try do
        Port.close(port)
      rescue
        ArgumentError -> :ok
      end
    end

    :ok
  end

  # Fail every pending request (the CLI exited or the client is stopping).
  defp fail_pending(%State{pending: pending} = state) do
    for {_id, from} <- pending do
      GenServer.reply(from, {:error, @closed})
    end

    %{state | pending: %{}}
  end

  defp os_pid(port) do
    case Port.info(port, :os_pid) do
      {:os_pid, pid} -> pid
      _ -> nil
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Message framing
  # ---------------------------------------------------------------------------

  defp send_message(port, message) do
    with {:ok, frame} <- encode_frame(message), do: send_frame(port, frame)
  end

  defp encode_frame(message) do
    case Jason.encode(message) do
      {:ok, json} ->
        {:ok, ["Content-Length: ", Integer.to_string(byte_size(json)), "\r\n\r\n", json]}

      {:error, error} ->
        {:error, {:invalid_json, Exception.message(error)}}
    end
  rescue
    error -> {:error, {:invalid_json, Exception.message(error)}}
  end

  defp send_frame(nil, _frame), do: {:error, @closed}

  defp send_frame(port, frame) do
    Port.command(port, frame)
    :ok
  rescue
    ArgumentError -> {:error, @closed}
  end

  @doc false
  # Splits buffered stdout into the bodies of its complete frames and the
  # unconsumed rest (a partial frame, or <<>>), as vscode-jsonrpc's
  # StreamMessageReader does (see the module documentation).
  @spec parse_frames(binary()) :: {[binary()], binary()}
  def parse_frames(buffer), do: parse_frames(buffer, [])

  defp parse_frames(buffer, acc) do
    case :binary.match(buffer, "\r\n\r\n") do
      :nomatch ->
        {Enum.reverse(acc), buffer}

      {header_size, 4} ->
        <<header::binary-size(header_size), _separator::binary-size(4), rest::binary>> = buffer

        case content_length(header) do
          {:ok, size} when byte_size(rest) >= size ->
            <<body::binary-size(size), tail::binary>> = rest
            parse_frames(tail, [body | acc])

          {:ok, _size} ->
            {Enum.reverse(acc), buffer}

          :error ->
            parse_frames(rest, acc)
        end
    end
  end

  defp content_length(header) do
    header
    |> :binary.split(["\r\n", "\n"], [:global])
    |> Enum.find_value(:error, fn line ->
      case :binary.split(line, ":") do
        [name, value] ->
          if ascii_downcase(ascii_trim(name)) == "content-length",
            do: parse_size(ascii_trim(value))

        _ ->
          nil
      end
    end)
  end

  defp parse_size(value) do
    case Integer.parse(value) do
      {size, ""} when size >= 0 -> {:ok, size}
      _ -> :error
    end
  end

  # Header lines are ASCII; these helpers accept any bytes.
  defp ascii_downcase(bin), do: for(<<c <- bin>>, into: "", do: <<ascii_lower(c)>>)

  defp ascii_lower(c) when c in ?A..?Z, do: c + 32
  defp ascii_lower(c), do: c

  defp ascii_trim(bin), do: bin |> trim_leading() |> trim_trailing()

  defp trim_leading(<<c, rest::binary>>) when c in [?\s, ?\t, ?\r], do: trim_leading(rest)
  defp trim_leading(bin), do: bin

  defp trim_trailing(bin) do
    size = byte_size(bin)

    if size > 0 and :binary.last(bin) in [?\s, ?\t, ?\r] do
      trim_trailing(binary_part(bin, 0, size - 1))
    else
      bin
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: Message dispatch
  # ---------------------------------------------------------------------------

  defp handle_frame(body, state) do
    case Jason.decode(body) do
      {:ok, message} when is_map(message) ->
        dispatch_message(message, state)

      {:ok, _other} ->
        state

      {:error, reason} ->
        Logger.warning("JSON-RPC: failed to decode message: #{inspect(reason)}")
        state
    end
  end

  # Incoming request from the server (has both "id" and "method")
  defp dispatch_message(%{"id" => id, "method" => method} = message, state)
       when is_binary(method) do
    handle_incoming_request(id, method, message["params"] || %{}, state)
  end

  # Incoming notification from the server (has "method" but no "id")
  defp dispatch_message(%{"method" => method} = message, state) when is_binary(method) do
    if state.notification_handler do
      try do
        state.notification_handler.(method, message["params"] || %{})
      rescue
        e -> Logger.warning("Notification handler error: #{inspect(e)}")
      end
    end

    state
  end

  # Response to one of this client's requests
  defp dispatch_message(%{"id" => id, "error" => error}, state) do
    reply(state, id, {:error, error})
  end

  defp dispatch_message(%{"id" => id} = message, state) do
    reply(state, id, {:ok, message["result"]})
  end

  defp dispatch_message(_message, state), do: state

  defp reply(state, id, result) do
    case Map.pop(state.pending, response_key(id)) do
      {nil, _pending} ->
        state

      {from, pending} ->
        GenServer.reply(from, result)
        %{state | pending: pending}
    end
  end

  defp response_key(id) when is_integer(id), do: Integer.to_string(id)
  defp response_key(id), do: id

  # Each handler runs in its own process, so a slow handler (a prompt waiting
  # for a person) never blocks reading; this process writes the response.
  defp handle_incoming_request(id, method, params, state) do
    case Map.get(state.request_handlers, method) do
      nil ->
        _ = send_message(state.port, error_response(id, -32601, "Method not found: #{method}"))

      handler ->
        client = self()

        Task.start(fn ->
          GenServer.cast(client, {:send_frame, handler_response(handler, id, params)})
        end)
    end

    state
  end

  # Runs a request handler and encodes its response frame. A handler returns
  # :null to answer with a JSON null result; a failing handler, or a result
  # that is not valid JSON, is answered with an error.
  defp handler_response(handler, id, params) do
    response =
      try do
        result =
          case handler.(params) do
            :null -> nil
            nil -> %{}
            other -> other
          end

        %{"jsonrpc" => "2.0", "id" => id, "result" => result}
      rescue
        e -> error_response(id, -32603, Exception.message(e))
      catch
        kind, reason -> error_response(id, -32603, Exception.format_banner(kind, reason))
      end

    case encode_frame(response) do
      {:ok, frame} ->
        frame

      {:error, _} ->
        {:ok, frame} = encode_frame(error_response(id, -32603, "Handler result is not valid JSON"))
        frame
    end
  end

  defp error_response(id, code, message) do
    %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{"code" => code, "message" => message}
    }
  end
end

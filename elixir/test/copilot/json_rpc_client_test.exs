defmodule Copilot.JsonRpcClientTest do
  use ExUnit.Case, async: true

  alias Copilot.JsonRpcClient

  defp frame(body), do: "Content-Length: #{byte_size(body)}\r\n\r\n" <> body

  describe "parse_frames/1" do
    test "returns every complete frame of a chunk and keeps the partial rest" do
      a = ~s({"jsonrpc":"2.0","method":"a"})
      b = ~s({"jsonrpc":"2.0","method":"b"})
      c = frame(~s({"jsonrpc":"2.0","method":"c"}))
      partial = binary_part(c, 0, byte_size(c) - 3)

      assert JsonRpcClient.parse_frames(frame(a) <> frame(b) <> partial) == {[a, b], partial}
      assert JsonRpcClient.parse_frames("") == {[], ""}
    end

    test "reassembles frames split at any byte, counting the length in bytes" do
      bodies = [~s({"id":"1","result":{"text":"héllo ☃"}}), ~s({"method":"m","params":{}})]
      stream = Enum.map_join(bodies, &frame/1)

      for split <- 0..byte_size(stream) do
        <<first::binary-size(split), second::binary>> = stream
        {early, rest} = JsonRpcClient.parse_frames(first)
        assert {late, ""} = JsonRpcClient.parse_frames(rest <> second)
        assert early ++ late == bodies
      end
    end

    test "matches Content-Length case-insensitively and ignores other headers" do
      body = ~s({"method":"m"})

      chunk =
        "content-length: #{byte_size(body)}\r\n" <>
          "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n" <> body

      assert JsonRpcClient.parse_frames(chunk) == {[body], ""}
    end

    test "drops a header block without a valid Content-Length" do
      body = ~s({"method":"m"})

      assert JsonRpcClient.parse_frames("X-Other: 1\r\n\r\n" <> frame(body)) == {[body], ""}

      assert JsonRpcClient.parse_frames("Content-Length: abc\r\n\r\n" <> frame(body)) ==
               {[body], ""}
    end
  end
end

defmodule Copilot.JsonRpcClientPortTest do
  # cat and sh stand in for the CLI.
  use ExUnit.Case, async: true

  alias Copilot.Client
  alias Copilot.JsonRpcClient
  alias Copilot.Types.CopilotClientOptions

  if match?({:win32, _}, :os.type()) do
    @moduletag skip: "runs cat and sh"
  end

  defp frame(body), do: "Content-Length: #{byte_size(body)}\r\n\r\n" <> body

  defp start_rpc(command, args) do
    JsonRpcClient.start_link(
      {:spawn_executable, System.find_executable(command), [{:args, args}]}
    )
  end

  test "reads what the CLI writes: requests, responses and notifications" do
    # cat echoes every frame, so each request comes back as a server request,
    # its handler's answer comes back as the response, and a notification
    # comes back as a server notification.
    {:ok, rpc} = start_rpc("cat", [])
    test_pid = self()

    JsonRpcClient.set_request_handler(rpc, "echo.request", fn params -> params end)

    JsonRpcClient.set_notification_handler(rpc, fn method, params ->
      send(test_pid, {:notification, method, params})
      :ok
    end)

    # About 256 KiB, so the frames arrive split across several reads.
    payload = %{"text" => String.duplicate("é", 128 * 1024)}
    assert JsonRpcClient.request(rpc, "echo.request", payload) == {:ok, payload}

    assert {:error, %{"code" => -32601, "message" => "Method not found: missing.method"}} =
             JsonRpcClient.request(rpc, "missing.method", %{})

    :ok = JsonRpcClient.notify(rpc, "session.event", %{"n" => 1})
    assert_receive {:notification, "session.event", %{"n" => 1}}, 5_000

    assert JsonRpcClient.stop(rpc) == :ok
  end

  test "a CLI exit fails pending requests and stops the client normally" do
    # sh exits once the first line of a request arrives.
    {:ok, rpc} = start_rpc("sh", ["-c", "read line; exit 4"])
    ref = Process.monitor(rpc)

    assert JsonRpcClient.request(rpc, "ping", %{}) ==
             {:error, %{code: -1, message: "CLI process exited"}}

    assert_receive {:DOWN, ^ref, :process, ^rpc, :normal}, 5_000

    assert JsonRpcClient.request(rpc, "ping", %{}) ==
             {:error, %{code: -1, message: "JSON-RPC client is not running"}}
  end

  test "start_link returns an error when the executable cannot be started" do
    assert {:error, {:spawn_failed, _reason}} =
             JsonRpcClient.start_link({:spawn_executable, "/nonexistent/copilot", []})
  end

  test "Client.start returns an error when the CLI cannot be started" do
    {:ok, client} =
      Client.start_link(%CopilotClientOptions{cli_path: "/nonexistent/copilot", auto_start: false})

    assert {:error, message} = Client.start(client)
    assert message =~ "Failed to start the Copilot CLI (/nonexistent/copilot)"
    assert Client.get_state(client) == :error
  end

  test "Client connects to a fake CLI and is disconnected when the CLI exits" do
    pong =
      ~s({"jsonrpc":"2.0","id":"1","result":{"message":"pong","timestamp":0,"protocolVersion":4}})

    # Reads the two header lines of the version check ping (request 1) and
    # answers it, then exits when the next request arrives.
    script = ~S(read a; read b; printf '%s' "$1"; read c; exit 3)

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: System.find_executable("sh"),
        cli_args: ["-c", script, "fake-cli", frame(pong)],
        auto_start: false
      })

    assert Client.start(client) == :ok
    assert Client.get_state(client) == :connected

    assert Client.ping(client, "bye") == {:error, %{code: -1, message: "CLI process exited"}}
    assert eventually(fn -> Client.get_state(client) == :disconnected end)

    assert Client.ping(client) ==
             {:error, %{code: -1, message: "JSON-RPC client is not running"}}
  end

  defp eventually(fun, attempts \\ 100) do
    cond do
      fun.() ->
        true

      attempts == 0 ->
        false

      true ->
        Process.sleep(20)
        eventually(fun, attempts - 1)
    end
  end
end

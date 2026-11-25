defmodule Copilot.CliE2ETest.Harness do
  @moduledoc false
  # The shared replaying CAPI proxy (test/harness/server.ts) and the CLI
  # environment, as in python/e2e/testharness (proxy.py and context.py).

  @repo_root Path.expand("../../..", __DIR__)
  @token "fake-token-for-e2e-tests"
  @proxy_start_timeout 120_000

  def repo_root, do: @repo_root
  def token, do: @token

  defp harness_dir, do: Path.join([@repo_root, "test", "harness"])

  # nil when everything the test needs is present, otherwise what is missing.
  def missing_prerequisite do
    cond do
      not File.regular?(Path.join(harness_dir(), "server.ts")) ->
        "test/harness/server.ts was not found"

      System.find_executable("npx") == nil ->
        "npx is not on PATH"

      cli_path() == nil ->
        "no Copilot CLI (set COPILOT_CLI_PATH or run npm install in nodejs/)"

      not File.dir?(Path.join(harness_dir(), "node_modules")) ->
        "run npm install in test/harness/"

      true ->
        nil
    end
  end

  # COPILOT_CLI_PATH (CI points it at nodejs/node_modules/.bin/copilot), then
  # the CLI installed in nodejs/node_modules.
  def cli_path do
    modules = Path.join([@repo_root, "nodejs", "node_modules"])

    [
      System.get_env("COPILOT_CLI_PATH"),
      Path.join([modules, ".bin", "copilot"]),
      Path.join([modules, "@github", "copilot", "npm-loader.js"])
    ]
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.map(&Path.expand/1)
    |> Enum.find(&File.regular?/1)
  end

  # Isolated directories outside the repository, as the other SDKs' E2E
  # harnesses use.
  def temp_root do
    base =
      case System.get_env("RUNNER_TEMP") do
        dir when dir in [nil, ""] -> System.tmp_dir!()
        dir -> dir
      end

    Path.join(base, "copilot-elixir-e2e-#{System.pid()}-#{System.unique_integer([:positive])}")
  end

  # Environment of the CLI process, as in python/e2e/testharness (proxy.py
  # get_proxy_env and context.py get_env). The token reaches the CLI as
  # COPILOT_SDK_AUTH_TOKEN through the client's github_token option, and an
  # empty value unsets a variable.
  def cli_env(%{url: url, connect_proxy_url: connect_proxy, ca_file: ca_file}, home) do
    no_proxy = "127.0.0.1,localhost,::1"

    [
      {"HTTP_PROXY", connect_proxy},
      {"HTTPS_PROXY", connect_proxy},
      {"http_proxy", connect_proxy},
      {"https_proxy", connect_proxy},
      {"NO_PROXY", no_proxy},
      {"no_proxy", no_proxy},
      {"NODE_EXTRA_CA_CERTS", ca_file},
      {"SSL_CERT_FILE", ca_file},
      {"REQUESTS_CA_BUNDLE", ca_file},
      {"CURL_CA_BUNDLE", ca_file},
      {"GIT_SSL_CAINFO", ca_file},
      {"GH_ENTERPRISE_TOKEN", ""},
      {"GITHUB_ENTERPRISE_TOKEN", ""},
      {"COPILOT_API_URL", url},
      {"COPILOT_DEBUG_GITHUB_API_URL", url},
      {"COPILOT_HOME", home},
      {"COPILOT_CACHE_HOME", Path.join(home, "cache")},
      {"COPILOT_HMAC_KEY", ""},
      {"CAPI_HMAC_KEY", ""},
      {"GH_CONFIG_DIR", home},
      {"XDG_CONFIG_HOME", home},
      {"XDG_STATE_HOME", home},
      {"GH_TOKEN", @token},
      {"GITHUB_TOKEN", @token},
      {"COPILOT_MCP_APPS", "true"},
      {"MCP_APPS", "true"}
    ]
  end

  # The proxy runs under a dedicated process that owns its port, so it
  # outlives setup_all. It prints `Listening: <url> <json>` on stdout, where
  # the JSON carries the CONNECT proxy URL and CA file.
  def start_proxy do
    parent = self()
    ref = make_ref()
    pid = spawn(fn -> proxy_init(parent, ref) end)

    receive do
      {^ref, {:ok, proxy}} ->
        {:ok, Map.put(proxy, :pid, pid)}

      {^ref, {:error, _} = error} ->
        error
    after
      @proxy_start_timeout ->
        Process.exit(pid, :kill)
        {:error, :proxy_start_timeout}
    end
  end

  def stop_proxy(%{pid: pid, url: url}) do
    # The proxy exits after answering /stop; snapshots are never rewritten.
    _ = post_json(url <> "/stop?skipWritingCache=true", %{})
    ref = Process.monitor(pid)
    send(pid, :stop)

    receive do
      {:DOWN, ^ref, :process, ^pid, _reason} -> :ok
    after
      10_000 ->
        Process.exit(pid, :kill)
        :ok
    end
  end

  # Registers the /copilot_internal/user response for the token the CLI uses,
  # with the proxy as the Copilot API endpoint.
  def register_copilot_user(%{url: url}) do
    user = %{
      "login" => "e2e-test-user",
      "id" => 12345,
      "copilot_plan" => "individual_pro",
      "endpoints" => %{
        "api" => url,
        "telemetry" => "https://sdk-hydro-collector.example.com"
      },
      "analytics_tracking_id" => "e2e-test-tracking-id"
    }

    {:ok, 200} = post_json(url <> "/copilot-user-config", %{"token" => @token, "response" => user})
    :ok
  end

  # Points the proxy at a snapshot for the next test.
  def configure(%{url: url}, snapshot, work_dir) do
    {:ok, 200} = post_json(url <> "/config", %{"filePath" => snapshot, "workDir" => work_dir})
    :ok
  end

  defp proxy_init(parent, ref) do
    settings = [
      {:args, ["tsx", Path.join(harness_dir(), "server.ts")]},
      {:cd, String.to_charlist(harness_dir())},
      {:line, 65_536},
      :binary,
      :use_stdio,
      :exit_status,
      :hide
    ]

    case open_port(System.find_executable("npx"), settings) do
      {:ok, port} ->
        case await_listening(port, "") do
          {:ok, proxy} ->
            send(parent, {ref, {:ok, proxy}})
            proxy_loop(port)

          {:error, _} = error ->
            close_port(port)
            send(parent, {ref, error})
        end

      {:error, _} = error ->
        send(parent, {ref, error})
    end
  end

  defp open_port(executable, settings) do
    {:ok, Port.open({:spawn_executable, executable}, settings)}
  catch
    :error, reason -> {:error, {:spawn_failed, reason}}
  end

  defp close_port(port) do
    Port.close(port)
  rescue
    ArgumentError -> :ok
  end

  defp await_listening(port, partial) do
    receive do
      {^port, {:data, {:noeol, chunk}}} ->
        await_listening(port, partial <> chunk)

      {^port, {:data, {:eol, chunk}}} ->
        case parse_listening(partial <> chunk) do
          :nomatch -> await_listening(port, "")
          result -> result
        end

      {^port, {:exit_status, status}} ->
        {:error, {:proxy_exited, status}}
    after
      @proxy_start_timeout -> {:error, :proxy_start_timeout}
    end
  end

  defp parse_listening(line) do
    case Regex.run(~r/Listening: (http:\/\/\S+)\s+(\{.*\})/, line, capture: :all_but_first) do
      [url, json] ->
        case Jason.decode(json) do
          {:ok, %{"connectProxyUrl" => connect_proxy, "caFilePath" => ca_file}} ->
            {:ok, %{url: url, connect_proxy_url: connect_proxy, ca_file: ca_file}}

          other ->
            {:error, {:missing_connect_proxy_metadata, other}}
        end

      nil ->
        :nomatch
    end
  end

  defp proxy_loop(port) do
    receive do
      {^port, {:data, _}} -> proxy_loop(port)
      {^port, {:exit_status, _}} -> :ok
      :stop -> close_port(port)
    end
  end

  # POSTs a JSON body and returns {:ok, status}. Plain :gen_tcp, so the test
  # needs no HTTP client application.
  defp post_json(url, body) do
    %URI{host: host, port: port, path: path, query: query} = URI.parse(url)
    target = if query, do: "#{path}?#{query}", else: path
    json = Jason.encode!(body)

    request = [
      "POST ",
      target,
      " HTTP/1.1\r\nHost: ",
      host,
      ":",
      Integer.to_string(port),
      "\r\nContent-Type: application/json\r\nContent-Length: ",
      Integer.to_string(byte_size(json)),
      "\r\nConnection: close\r\n\r\n",
      json
    ]

    case :gen_tcp.connect(String.to_charlist(host), port, [:binary, active: false], 10_000) do
      {:ok, socket} ->
        try do
          with :ok <- :gen_tcp.send(socket, request),
               {:ok, response} <- recv_all(socket, "") do
            case Regex.run(~r/\AHTTP\/1\.[01] (\d{3})/, response, capture: :all_but_first) do
              [status] -> {:ok, String.to_integer(status)}
              nil -> {:error, {:bad_response, response}}
            end
          end
        after
          :gen_tcp.close(socket)
        end

      {:error, _} = error ->
        error
    end
  end

  defp recv_all(socket, acc) do
    case :gen_tcp.recv(socket, 0, 60_000) do
      {:ok, data} -> recv_all(socket, acc <> data)
      {:error, :closed} -> {:ok, acc}
      {:error, _} = error -> error
    end
  end
end

defmodule Copilot.CliE2ETest do
  # The Elixir client spawns the real Copilot CLI (COPILOT_CLI_PATH, or the
  # CLI installed in nodejs/node_modules) over stdio, and the CLI talks to the
  # shared replaying CAPI proxy (test/harness/server.ts) with a snapshot from
  # test/snapshots/. Needs Node.js and `npm install` in nodejs/ and
  # test/harness/. Without them the test is skipped, except on GitHub
  # Actions, where it fails instead.
  use ExUnit.Case, async: false

  alias Copilot.CliE2ETest.Harness
  alias Copilot.{Client, Session}

  alias Copilot.Types.{
    ClientInfo,
    CopilotClientOptions,
    MessageOptions,
    PingResponse,
    SessionConfig
  }

  @moduletag timeout: 300_000

  @missing Harness.missing_prerequisite()

  if @missing && System.get_env("GITHUB_ACTIONS") != "true" do
    @moduletag skip: "real CLI E2E prerequisites missing: #{@missing}"
  end

  @snapshot Path.join([
              Harness.repo_root(),
              "test",
              "snapshots",
              "session",
              "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml"
            ])

  setup_all do
    if missing = Harness.missing_prerequisite() do
      flunk("real CLI E2E prerequisites missing: #{missing}")
    end

    temp_root = Harness.temp_root()
    {:ok, proxy} = Harness.start_proxy()

    on_exit(fn ->
      Harness.stop_proxy(proxy)
      File.rm_rf(temp_root)
    end)

    :ok = Harness.register_copilot_user(proxy)

    %{proxy: proxy, home: Path.join(temp_root, "home"), work: Path.join(temp_root, "work")}
  end

  setup %{proxy: proxy, home: home, work: work} do
    for dir <- [home, work] do
      File.rm_rf!(dir)
      File.mkdir_p!(dir)
    end

    assert File.regular?(@snapshot)
    :ok = Harness.configure(proxy, @snapshot, work)
    :ok
  end

  test "starts the real CLI, pings with protocol 4, and gets the reply to a message", context do
    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: Harness.cli_path(),
        cwd: context.work,
        env: Harness.cli_env(context.proxy, context.home),
        github_token: Harness.token(),
        auto_start: false,
        # Sends the connect handshake before the version check ping.
        client_info: %ClientInfo{editor_name: "copilot-elixir-e2e", editor_version: "1.0.0"}
      })

    try do
      assert Client.start(client) == :ok
      assert Client.get_state(client) == :connected
      assert {:ok, %PingResponse{protocol_version: 4}} = Client.ping(client, "elixir-e2e")

      assert {:ok, session} =
               Client.create_session(client, %SessionConfig{model: "claude-sonnet-4.5"})

      test_pid = self()
      Session.on(session, fn event -> send(test_pid, {:session_event, event["type"]}) end)

      reply = Session.send_and_wait(session, %MessageOptions{prompt: "What is 2+2?"}, 120_000)
      types = received_event_types()

      content =
        case reply do
          {:ok, %{"data" => %{"content" => content}}} when is_binary(content) ->
            content

          other ->
            flunk("no assistant reply: #{inspect(other)}, events: #{inspect(types)}")
        end

      assert content =~ "4"
      assert "assistant.message" in types
      assert "session.idle" in types
    after
      Client.stop(client)
    end
  end

  # Session.on/2 handlers run before send_and_wait/3 replies, so every event
  # of the turn is already in the mailbox.
  defp received_event_types do
    receive do
      {:session_event, type} -> [type | received_event_types()]
    after
      0 -> []
    end
  end
end

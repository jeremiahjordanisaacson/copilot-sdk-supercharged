ExUnit.start()

defmodule CopilotE2E.E2ETest do
  @moduledoc """
  End-to-end tests for the Elixir Copilot SDK.

  These tests spawn a replaying CAPI proxy (shared test harness) and exercise
  the SDK client through real JSON-RPC communication with the Copilot CLI.
  """

  use ExUnit.Case, async: false

  alias Copilot.Client
  alias Copilot.Session
  alias Copilot.Types.{CopilotClientOptions, SessionConfig, MessageOptions, SessionFsConfig}

  # Load the proxy module (it lives outside the normal mix compilation path)
  Code.require_file("testharness/proxy.ex", __DIR__)

  # ---------------------------------------------------------------------------
  # Shared setup — start and stop the replay proxy once for all tests
  # ---------------------------------------------------------------------------

  setup_all do
    {:ok, proxy} = CopilotE2E.TestHarness.Proxy.start_link()
    {:ok, proxy_url} = CopilotE2E.TestHarness.Proxy.start_proxy(proxy)

    # Derive host:port from proxy URL for cli env configuration
    %URI{host: host, port: port} = URI.parse(proxy_url)
    _cli_url = "#{host}:#{port}"

    # Find the CLI path — prefer COPILOT_CLI_PATH env, fall back to node_modules
    cli_path = find_cli_path!()

    # Create isolated temp directories for the test run
    home_dir = create_temp_dir("copilot-elixir-e2e-home-")
    work_dir = create_temp_dir("copilot-elixir-e2e-work-")

    # Build environment variables that redirect API calls to the proxy
    test_env = build_test_env(proxy_url, home_dir)

    on_exit(fn ->
      CopilotE2E.TestHarness.Proxy.stop_proxy(proxy, skip_writing_cache: false)

      File.rm_rf!(home_dir)
      File.rm_rf!(work_dir)
    end)

    %{
      proxy: proxy,
      proxy_url: proxy_url,
      cli_path: cli_path,
      home_dir: home_dir,
      work_dir: work_dir,
      test_env: test_env
    }
  end

  # ---------------------------------------------------------------------------
  # Test: session create and disconnect
  # ---------------------------------------------------------------------------

  test "session create and disconnect", ctx do
    configure_proxy!(ctx, "session", "should_create_and_disconnect_sessions")

    # Start the client pointing at the replay proxy via env vars
    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    # Create a session
    {:ok, session} = Client.create_session(client, %SessionConfig{})

    # The session should have a valid session ID
    session_id = Session.session_id(session)
    assert is_binary(session_id)
    assert String.length(session_id) > 0

    # Fetch messages — at minimum the session.start event should exist
    {:ok, messages} = Session.get_messages(session)
    assert is_list(messages)
    assert length(messages) > 0

    first = List.first(messages)
    assert first["type"] == "session.start"
    assert first["data"]["sessionId"] == session_id

    # Destroy the session and stop the client
    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: send message and receive response
  # ---------------------------------------------------------------------------

  test "send message and receive response", ctx do
    configure_proxy!(ctx, "session", "should_have_stateful_conversation")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session} = Client.create_session(client, %SessionConfig{})
    assert is_binary(Session.session_id(session))

    # Send a message and wait for the assistant's reply
    {:ok, response} =
      Session.send_and_wait(session, %MessageOptions{prompt: "What is 1+1?"})

    # The response should be an assistant.message event containing "2"
    assert response != nil
    assert is_map(response)
    assert response["type"] == "assistant.message"
    assert is_binary(response["data"]["content"])
    assert String.contains?(response["data"]["content"], "2")

    # Clean up
    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: sessionFs configuration
  # ---------------------------------------------------------------------------

  test "sessionFs configuration", ctx do
    configure_proxy!(ctx, "session_fs", "should_route_file_operations_through_the_session_fs_provider")

    session_fs_config = %SessionFsConfig{
      initial_cwd: "/",
      session_state_path: "/session-state",
      conventions: "posix"
    }

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests",
        session_fs: session_fs_config
      })

    :ok = Client.start(client)

    # Creating a session with a session_fs-enabled client should succeed
    {:ok, session} = Client.create_session(client, %SessionConfig{})
    session_id = Session.session_id(session)
    assert is_binary(session_id)
    assert String.length(session_id) > 0

    # Verify the session_fs config was accepted by checking the client started OK
    {:ok, status} = Client.get_status(client)
    assert is_map(status) or is_struct(status)

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp find_cli_path! do
    case System.get_env("COPILOT_CLI_PATH") do
      path when is_binary(path) and path != "" ->
        if File.exists?(path), do: Path.expand(path), else: raise("COPILOT_CLI_PATH not found: #{path}")

      _ ->
        # Look in the sibling nodejs directory
        base = Path.expand("../../nodejs/node_modules/@github/copilot/index.js", __DIR__)

        if File.exists?(base) do
          base
        else
          raise """
          CLI not found for E2E tests.
          Set COPILOT_CLI_PATH or run `npm install` in the nodejs/ directory.
          Looked at: #{base}
          """
        end
    end
  end

  defp build_test_env(proxy_url, home_dir) do
    [
      {"COPILOT_API_URL", proxy_url},
      {"COPILOT_HOME", home_dir},
      {"XDG_CONFIG_HOME", home_dir},
      {"XDG_STATE_HOME", home_dir}
    ]
  end

  defp create_temp_dir(prefix) do
    base = System.tmp_dir!()
    dir = Path.join(base, prefix <> random_hex(8))
    File.mkdir_p!(dir)
    dir
  end

  defp random_hex(bytes) do
    :crypto.strong_rand_bytes(bytes) |> Base.encode16(case: :lower)
  end

  defp configure_proxy!(ctx, test_file, test_name) do
    snapshots_dir =
      Path.expand("../../test/snapshots", __DIR__)

    sanitized_name =
      test_name
      |> String.replace(~r/[^a-zA-Z0-9]/, "_")
      |> String.downcase()

    snapshot_path = Path.join([snapshots_dir, test_file, "#{sanitized_name}.yaml"])
    abs_snapshot_path = Path.expand(snapshot_path)

    :ok = CopilotE2E.TestHarness.Proxy.configure(ctx.proxy, abs_snapshot_path, ctx.work_dir)
  end
end

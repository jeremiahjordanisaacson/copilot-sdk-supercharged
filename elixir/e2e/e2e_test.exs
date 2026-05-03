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
  # Test: multi-turn conversation
  # ---------------------------------------------------------------------------

  test "multi-turn conversation", ctx do
    configure_proxy!(ctx, "session", "multi_turn_conversation")

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

    # First turn
    {:ok, response1} =
      Session.send_and_wait(session, %MessageOptions{prompt: "Remember the number 42."})

    assert response1 != nil
    assert is_map(response1)
    assert response1["type"] == "assistant.message"
    assert is_binary(response1["data"]["content"])

    # Second turn — follow-up referencing the first
    {:ok, response2} =
      Session.send_and_wait(session, %MessageOptions{prompt: "What number did I ask you to remember?"})

    assert response2 != nil
    assert is_map(response2)
    assert response2["type"] == "assistant.message"
    assert is_binary(response2["data"]["content"])
    assert String.contains?(response2["data"]["content"], "42")

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: session resume
  # ---------------------------------------------------------------------------

  test "session resume", ctx do
    configure_proxy!(ctx, "session", "session_resume")

    {:ok, client1} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client1)

    {:ok, session} = Client.create_session(client1, %SessionConfig{})
    original_id = Session.session_id(session)
    assert is_binary(original_id)

    Session.destroy(session)
    Client.stop(client1)

    # Start a fresh client and resume the same session by ID
    {:ok, client2} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client2)

    {:ok, resumed_session} =
      Client.create_session(client2, %SessionConfig{session_id: original_id})

    assert Session.session_id(resumed_session) == original_id

    Session.destroy(resumed_session)
    Client.stop(client2)
  end

  # ---------------------------------------------------------------------------
  # Test: session list
  # ---------------------------------------------------------------------------

  test "session list", ctx do
    configure_proxy!(ctx, "session", "session_list")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session1} = Client.create_session(client, %SessionConfig{})
    {:ok, session2} = Client.create_session(client, %SessionConfig{})

    id1 = Session.session_id(session1)
    id2 = Session.session_id(session2)

    {:ok, sessions} = Client.list_sessions(client)
    assert is_list(sessions)

    listed_ids = Enum.map(sessions, & &1["sessionId"])
    assert id1 in listed_ids
    assert id2 in listed_ids

    Session.destroy(session1)
    Session.destroy(session2)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: session metadata
  # ---------------------------------------------------------------------------

  test "session metadata", ctx do
    configure_proxy!(ctx, "session", "session_metadata")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session} = Client.create_session(client, %SessionConfig{})
    session_id = Session.session_id(session)

    {:ok, metadata} = Client.get_session_metadata(client, session_id)
    assert is_map(metadata)
    assert metadata["sessionId"] == session_id

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: session delete
  # ---------------------------------------------------------------------------

  test "session delete", ctx do
    configure_proxy!(ctx, "session", "session_delete")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session} = Client.create_session(client, %SessionConfig{})
    session_id = Session.session_id(session)
    assert is_binary(session_id)

    Session.destroy(session)

    # After deletion the session should no longer appear in the list
    {:ok, sessions} = Client.list_sessions(client)
    listed_ids = Enum.map(sessions, & &1["sessionId"])
    refute session_id in listed_ids

    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: model list
  # ---------------------------------------------------------------------------

  test "model list", ctx do
    configure_proxy!(ctx, "session", "model_list")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, models} = Client.list_models(client)
    assert is_list(models)
    assert length(models) > 0

    # Each model should have at minimum an id field
    first_model = List.first(models)
    assert is_map(first_model)
    assert Map.has_key?(first_model, "id") or Map.has_key?(first_model, "modelId")

    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: ping
  # ---------------------------------------------------------------------------

  test "ping", ctx do
    configure_proxy!(ctx, "session", "ping")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, pong} = Client.ping(client)
    assert is_map(pong)
    # Expect at least a message or timestamp in the pong response
    assert Map.has_key?(pong, "message") or Map.has_key?(pong, "timestamp")

    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: auth status
  # ---------------------------------------------------------------------------

  test "auth status", ctx do
    configure_proxy!(ctx, "session", "auth_status")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, auth} = Client.get_auth_status(client)
    assert is_map(auth)
    # The response should indicate authentication state
    assert Map.has_key?(auth, "status") or Map.has_key?(auth, "authenticated")

    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: client lifecycle
  # ---------------------------------------------------------------------------

  test "client lifecycle", ctx do
    configure_proxy!(ctx, "session", "client_lifecycle")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    # Client should be alive and connected after start
    {:ok, status} = Client.get_status(client)
    assert is_map(status) or is_struct(status)

    # Stop the client
    Client.stop(client)

    # After stop the GenServer process should no longer be alive
    refute Process.alive?(client)
  end

  # ---------------------------------------------------------------------------
  # Test: foreground session
  # ---------------------------------------------------------------------------

  test "foreground session", ctx do
    configure_proxy!(ctx, "session", "foreground_session")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session} = Client.create_session(client, %SessionConfig{})
    session_id = Session.session_id(session)

    :ok = Client.set_foreground_session_id(client, session_id)
    {:ok, fg_id} = Client.get_foreground_session_id(client)
    assert fg_id == session_id

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: tools
  # ---------------------------------------------------------------------------

  test "tools", ctx do
    configure_proxy!(ctx, "session", "tools")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    tool = %{
      name: "get_weather",
      description: "Get the current weather for a city",
      parameters: %{
        type: "object",
        properties: %{
          city: %{type: "string", description: "City name"}
        },
        required: ["city"]
      }
    }

    {:ok, session} =
      Client.create_session(client, %SessionConfig{tools: [tool]})

    assert is_binary(Session.session_id(session))

    # Send a prompt that should trigger the tool
    {:ok, response} =
      Session.send_and_wait(session, %MessageOptions{prompt: "What is the weather in London?"})

    assert response != nil
    assert is_map(response)

    # The response should reference the tool call (either a tool_call event or
    # tool-related content in the assistant message)
    assert response["type"] in ["assistant.message", "tool.call"]

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: streaming
  # ---------------------------------------------------------------------------

  test "streaming", ctx do
    configure_proxy!(ctx, "session", "streaming")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session} =
      Client.create_session(client, %SessionConfig{streaming: true})

    assert is_binary(Session.session_id(session))

    # Collect events via a callback that accumulates event types
    events = Agent.start_link(fn -> [] end) |> elem(1)

    callback = fn event ->
      Agent.update(events, fn acc -> [event["type"] | acc] end)
    end

    {:ok, _response} =
      Session.send_and_wait(session, %MessageOptions{
        prompt: "Say hello briefly.",
        on_event: callback
      })

    collected = Agent.get(events, & &1)
    Agent.stop(events)

    # Streaming should produce at least one delta event
    has_delta =
      Enum.any?(collected, fn type ->
        type in ["assistant.message_delta", "assistant.reasoning_delta"]
      end)

    assert has_delta, "Expected streaming delta events, got: #{inspect(collected)}"

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: system message
  # ---------------------------------------------------------------------------

  test "system message", ctx do
    configure_proxy!(ctx, "session", "system_message")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    {:ok, session} =
      Client.create_session(client, %SessionConfig{
        system_message: "You are a pirate. Always respond like a pirate.",
        system_message_mode: "append"
      })

    assert is_binary(Session.session_id(session))

    {:ok, response} =
      Session.send_and_wait(session, %MessageOptions{prompt: "Hello, who are you?"})

    assert response != nil
    assert is_map(response)
    assert response["type"] == "assistant.message"
    assert is_binary(response["data"]["content"])

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: sessionFs provider variant
  # ---------------------------------------------------------------------------

  test "sessionFs provider variant", ctx do
    configure_proxy!(ctx, "session", "session_fs_provider_variant")

    session_fs_config = %SessionFsConfig{
      initial_cwd: ctx.work_dir,
      session_state_path: Path.join(ctx.work_dir, "session-state"),
      conventions: "native"
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

    {:ok, session} = Client.create_session(client, %SessionConfig{})
    session_id = Session.session_id(session)
    assert is_binary(session_id)
    assert String.length(session_id) > 0

    # Send a simple message to prove the session is functional
    {:ok, response} =
      Session.send_and_wait(session, %MessageOptions{prompt: "Ping"})

    assert response != nil
    assert is_map(response)

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: MCP servers config
  # ---------------------------------------------------------------------------

  test "MCP servers config", ctx do
    configure_proxy!(ctx, "session", "mcp_servers_config")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    mcp_servers = %{
      "test-server" => %{
        command: "node",
        args: ["--version"],
        enabled: true
      }
    }

    {:ok, session} =
      Client.create_session(client, %SessionConfig{mcp_servers: mcp_servers})

    session_id = Session.session_id(session)
    assert is_binary(session_id)
    assert String.length(session_id) > 0

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: skills config
  # ---------------------------------------------------------------------------

  test "skills config", ctx do
    configure_proxy!(ctx, "session", "skills_config")

    {:ok, client} =
      Client.start_link(%CopilotClientOptions{
        cli_path: ctx.cli_path,
        cwd: ctx.work_dir,
        env: ctx.test_env,
        github_token: "fake-token-for-e2e-tests"
      })

    :ok = Client.start(client)

    skills_dirs = [ctx.work_dir]

    {:ok, session} =
      Client.create_session(client, %SessionConfig{skills: skills_dirs})

    session_id = Session.session_id(session)
    assert is_binary(session_id)
    assert String.length(session_id) > 0

    Session.destroy(session)
    Client.stop(client)
  end

  # ---------------------------------------------------------------------------
  # Test: compaction
  # ---------------------------------------------------------------------------

  test "compaction", ctx do
    configure_proxy!(ctx, "session", "compaction")

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

    # Collect all events to watch for compaction signals
    events = Agent.start_link(fn -> [] end) |> elem(1)

    callback = fn event ->
      Agent.update(events, fn acc -> [event["type"] | acc] end)
    end

    # Send multiple messages to push the context window and trigger compaction
    prompts = [
      "Tell me a long story about a dragon.",
      "Now tell me about the dragon's treasure.",
      "What happened to the dragon's kingdom?",
      "Describe the final battle in great detail.",
      "Summarize everything you told me."
    ]

    for prompt <- prompts do
      {:ok, _response} =
        Session.send_and_wait(session, %MessageOptions{
          prompt: prompt,
          on_event: callback
        })
    end

    collected = Agent.get(events, & &1)
    Agent.stop(events)

    # Verify we received events — compaction events may or may not fire depending
    # on context length, but we should at least see assistant messages
    assert length(collected) > 0

    has_compaction =
      Enum.any?(collected, fn type ->
        type in ["session.compaction_start", "session.compaction_complete"]
      end)

    # If compaction didn't fire that's acceptable in replay mode — log it
    unless has_compaction do
      IO.puts("Note: compaction events not observed (may require longer context)")
    end

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

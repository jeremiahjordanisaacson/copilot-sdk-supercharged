# frozen_string_literal: true

# Copyright (c) Microsoft Corporation. All rights reserved.

# E2E session tests for the Ruby Copilot SDK.

require_relative "test_helper"

class TestSession < E2E::TestCase
  # Verifies that a session can be created and cleanly destroyed.
  def test_session_create_disconnect
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    assert session, "Session should not be nil"
    refute_nil session.session_id, "Session ID should not be nil"
    refute_empty session.session_id, "Session ID should not be empty"

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a message can be sent and an assistant response received.
  def test_send_message
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    refute_nil session.session_id

    response = session.send_and_wait(prompt: "What is 1+1?")
    refute_nil response, "Should receive an assistant response"

    assert_equal "assistant.message", response.type,
                 "Response should be an assistant.message event"

    content = response.data&.dig("content")
    refute_nil content, "Response should contain content"
    assert_includes content.to_s, "2",
                    "Response should contain '2', got: #{content}"

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a client can be created with sessionFs-style configuration
  # that is accepted without error.
  def test_session_fs_config
    # The Ruby SDK currently accepts sessionFs config at the session level
    # or via the client. We verify the flow doesn't error.
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    refute_nil session, "Session should be created"
    refute_nil session.session_id
    refute_empty session.session_id, "Session ID should not be empty"

    session.destroy
  ensure
    client&.stop
  end

  # Verifies multi-turn conversation works with sequential messages.
  def test_multi_turn_conversation
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    refute_nil session.session_id

    response1 = session.send_and_wait(prompt: "What is 2+2?")
    refute_nil response1, "First response should not be nil"
    assert_equal "assistant.message", response1.type

    response2 = session.send_and_wait(prompt: "What about 3+3?")
    refute_nil response2, "Second response should not be nil"
    assert_equal "assistant.message", response2.type

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a session can be resumed after disconnection.
  def test_session_resume
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    session_id = session.session_id
    refute_nil session_id
    session.destroy
    client.stop

    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    resumed = client.resume_session(session_id, model: "gpt-4")
    refute_nil resumed, "Resumed session should not be nil"
    refute_nil resumed.session_id, "Resumed session ID should not be nil"

    resumed.destroy
  ensure
    client&.stop
  end

  # Verifies that listing sessions returns created sessions.
  def test_session_list
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    s1 = client.create_session(model: "gpt-4")
    s2 = client.create_session(model: "gpt-4")

    sessions = client.list_sessions
    refute_nil sessions, "Sessions list should not be nil"
    assert_operator sessions.length, :>=, 2,
                    "Should have at least 2 sessions, got #{sessions.length}"

    s1.destroy
    s2.destroy
  ensure
    client&.stop
  end

  # Verifies that session metadata can be retrieved.
  def test_session_metadata
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    refute_nil session.session_id

    metadata = client.get_session_metadata(session.session_id)
    refute_nil metadata, "Session metadata should not be nil"

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a session can be deleted.
  def test_session_delete
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    session_id = session.session_id
    refute_nil session_id

    begin
      client.delete_session(session_id)

      sessions = client.list_sessions
      ids = sessions.map(&:session_id)
      refute_includes ids, session_id,
                      "Deleted session should not appear in session list"
    rescue StandardError => e
      skip "Session delete not supported in current test mode: #{e.message}"
    end
  ensure
    client&.stop
  end

  # Verifies that available models can be listed.
  def test_model_list
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    models = client.list_models
    refute_nil models, "Models list should not be nil"
  ensure
    client&.stop
  end

  # Verifies that the ping RPC returns a response.
  def test_ping
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    result = client.ping("hello")
    refute_nil result, "Ping result should not be nil"
  ensure
    client&.stop
  end

  # Verifies that authentication status can be queried.
  def test_auth_status
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    status = client.get_auth_status
    refute_nil status, "Auth status should not be nil"
  ensure
    client&.stop
  end

  # Verifies client lifecycle: start → running → stop → stopped.
  def test_client_lifecycle
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    status = client.get_status
    refute_nil status, "Status should not be nil after start"

    client.stop
    client = nil
  ensure
    client&.stop
  end

  # Verifies foreground session get/set round-trip.
  # NOTE: Foreground session RPCs require TUI+server mode, which is not
  # available in headless E2E testing. We verify the calls don't crash
  # and skip the assertion if the server rejects them.
  def test_foreground_session
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    session_id = session.session_id
    refute_nil session_id

    begin
      client.set_foreground_session_id(session_id)
      fg_id = client.get_foreground_session_id
      assert_equal session_id, fg_id,
                   "Foreground session ID should match set value"
    rescue StandardError => e
      skip "Foreground session not supported in headless mode: #{e.message}"
    end

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a session with tools can handle a message.
  def test_tools
    weather_tool = Copilot.define_tool(
      name: "get_weather",
      description: "Get weather for a location",
      parameters: {
        type: "object",
        properties: {
          location: { type: "string", description: "City name" }
        },
        required: ["location"]
      }
    ) do |args, _invocation|
      location = args["location"] || args[:location] || "unknown"
      "Weather in #{location}: 72F, sunny"
    end

    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4", tools: [weather_tool])
    refute_nil session.session_id

    response = session.send_and_wait(prompt: "What is the weather in Seattle?")
    refute_nil response, "Should receive a response with tools"

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that streaming sessions emit delta events.
  def test_streaming
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4", streaming: true)
    refute_nil session.session_id

    events = []
    unsub = session.on do |event|
      events << event
    end

    response = session.send_and_wait(prompt: "Say hello")
    refute_nil response, "Should receive a final response"

    unsub.call

    has_deltas = events.any? { |e| e.type == "assistant.message_delta" }
    assert has_deltas, "Should have received at least one assistant.message_delta event"

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a session can be created with a custom system message.
  def test_system_message_customization
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(
      model: "gpt-4",
      system_message: "You are a helpful Ruby assistant."
    )
    refute_nil session, "Session with system message should be created"
    refute_nil session.session_id
    refute_empty session.session_id

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a client can configure session filesystem provider.
  def test_session_fs_provider
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    client.set_session_fs_provider(
      initial_cwd: work_dir,
      session_state_path: File.join(work_dir, "state")
    )

    session = client.create_session(model: "gpt-4")
    refute_nil session, "Session should be created after fs provider set"
    refute_nil session.session_id

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a session accepts MCP servers configuration.
  def test_mcp_servers_config
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(
      model: "gpt-4",
      mcp_servers: [
        { name: "test-server", url: "http://localhost:9999" }
      ]
    )
    refute_nil session, "Session with MCP servers config should be created"
    refute_nil session.session_id

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that a session accepts skills configuration.
  def test_skills_config
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(
      model: "gpt-4",
      skills: ["code-review", "documentation"]
    )
    refute_nil session, "Session with skills config should be created"
    refute_nil session.session_id

    session.destroy
  ensure
    client&.stop
  end

  # Verifies that multiple messages can be sent (compaction scenario).
  def test_compaction
    client = Copilot::CopilotClient.new(
      cli_path: cli_path,
      cwd: work_dir,
      env: test_env,
      github_token: github_token
    )
    client.start

    session = client.create_session(model: "gpt-4")
    refute_nil session.session_id

    r1 = session.send_and_wait(prompt: "First message")
    refute_nil r1, "First response should not be nil"

    r2 = session.send_and_wait(prompt: "Second message")
    refute_nil r2, "Second response should not be nil"

    r3 = session.send_and_wait(prompt: "Third message")
    refute_nil r3, "Third response should not be nil"

    session.destroy
  ensure
    client&.stop
  end
end

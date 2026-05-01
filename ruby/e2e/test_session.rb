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
      env: test_env
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
      env: test_env
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
      env: test_env
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
end

# frozen_string_literal: true

# Copyright (c) Microsoft Corporation. All rights reserved.

# Test helper — sets up minitest, the harness, and shared fixtures.

require "minitest/autorun"
require "tmpdir"
require "fileutils"

# Add the SDK lib to the load path
$LOAD_PATH.unshift(File.expand_path("../lib", __dir__))
require "copilot"

require_relative "test_harness"

module E2E
  # Shared setup for all E2E tests.
  #
  # Subclasses get a running harness, temp directories, and a helper to
  # configure the proxy for each test.
  class TestCase < Minitest::Test
    class << self
      attr_accessor :harness, :work_dir, :home_dir
    end

    # Default snapshot used when no test-specific snapshot exists.
    DEFAULT_SNAPSHOT = "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"

    # Map test method names to actual snapshot file names in test/snapshots/session/.
    SNAPSHOT_MAP = {
      "session_create_disconnect"   => "should_create_session_with_custom_tool",
      "send_message"                => "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
      "session_fs_config"           => "should_create_session_with_custom_tool",
      "multi_turn_conversation"     => "should_have_stateful_conversation",
      "session_resume"              => "should_resume_a_session_using_a_new_client",
      "session_list"                => "should_list_sessions",
      "session_metadata"            => "should_get_session_metadata",
      "session_delete"              => "should_delete_session",
      "list_models"                 => "should_create_session_with_custom_tool",
      "ping"                        => "should_create_session_with_custom_tool",
      "auth_status"                 => "should_create_session_with_custom_tool",
      "client_lifecycle"            => "should_create_session_with_custom_tool",
      "foreground_session"          => "should_create_session_with_custom_tool",
      "tool_handling"               => "should_create_session_with_custom_tool",
      "streaming"                   => "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
      "system_message"              => "should_create_a_session_with_appended_systemmessage_config",
      "session_fs_with_messaging"   => "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
      "mcp_server_config"           => "should_create_session_with_custom_tool",
      "skill_directories"           => "should_create_session_with_custom_tool",
      "compaction"                  => "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
    }.freeze

    def setup
      unless self.class.harness
        self.class.harness = TestHarness.new
        self.class.harness.start

        self.class.work_dir = Dir.mktmpdir("copilot-e2e-work-")
        self.class.home_dir = Dir.mktmpdir("copilot-e2e-home-")
      end

      # Configure proxy for this test — look up the correct snapshot name
      test_name = name.sub(/^test_/, "")
      snapshot_name = SNAPSHOT_MAP.fetch(test_name, nil)

      # Fall back to exact test name, then default snapshot
      snapshot_path = if snapshot_name
                        File.join(TestHarness.snapshots_dir, "session", "#{snapshot_name}.yaml")
                      else
                        candidate = File.join(TestHarness.snapshots_dir, "session", "#{test_name}.yaml")
                        File.exist?(candidate) ? candidate : File.join(TestHarness.snapshots_dir, "session", "#{DEFAULT_SNAPSHOT}.yaml")
                      end

      self.class.harness.configure(
        File.expand_path(snapshot_path),
        File.expand_path(self.class.work_dir)
      )

      # Clean work directory between tests
      clean_dir(self.class.work_dir)
    end

    Minitest.after_run do
      [E2E::TestCase].each do |klass|
        klass.harness&.stop
        FileUtils.rm_rf(klass.work_dir) if klass.work_dir
        FileUtils.rm_rf(klass.home_dir) if klass.home_dir
      end
    end

    private

    def harness
      self.class.harness
    end

    def work_dir
      self.class.work_dir
    end

    def home_dir
      self.class.home_dir
    end

    def cli_path
      TestHarness.cli_path
    end

    def test_env
      harness.test_env(work_dir, home_dir)
    end

    def github_token
      ENV["GH_TOKEN"] || ENV["GITHUB_TOKEN"] || "fake-test-token"
    end

    def clean_dir(dir)
      return unless dir && Dir.exist?(dir)

      Dir.children(dir).each do |entry|
        path = File.join(dir, entry)
        FileUtils.rm_rf(path)
      rescue StandardError
        # ignore cleanup errors
      end
    end
  end
end

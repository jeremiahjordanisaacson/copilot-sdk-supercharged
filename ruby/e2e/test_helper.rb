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

    def setup
      unless self.class.harness
        self.class.harness = TestHarness.new
        self.class.harness.start

        self.class.work_dir = Dir.mktmpdir("copilot-e2e-work-")
        self.class.home_dir = Dir.mktmpdir("copilot-e2e-home-")
      end

      # Configure proxy for this test
      test_name = name.sub(/^test_/, "")
      snapshot_path = File.join(
        TestHarness.snapshots_dir, "session", "#{test_name}.yaml"
      )
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

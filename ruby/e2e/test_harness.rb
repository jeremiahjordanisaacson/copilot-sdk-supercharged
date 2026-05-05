# frozen_string_literal: true

# Copyright (c) Microsoft Corporation. All rights reserved.

# Test harness that manages a replaying CAPI proxy for E2E tests.
#
# Spawns the shared test harness server from test/harness/server.ts and
# provides methods to configure and stop the proxy.

require "open3"
require "net/http"
require "uri"
require "json"
require "tmpdir"

module E2E
  class TestHarness
    attr_reader :proxy_url

    # Compute the repo root: ruby/e2e/../../ = repo root
    REPO_ROOT = File.expand_path("../..", __dir__)
    SNAPSHOTS_DIR = File.join(REPO_ROOT, "test", "snapshots")

    def initialize
      @process_stdin  = nil
      @process_stdout = nil
      @process_stderr = nil
      @process_thread = nil
      @pid            = nil
      @proxy_url      = nil
    end

    # Start the replaying proxy server.
    def start
      return if @proxy_url

      server_path = File.join(REPO_ROOT, "test", "harness", "server.ts")
      harness_dir = File.dirname(server_path)

      # On Windows, spawn via cmd /c to find npx
      is_windows = RUBY_PLATFORM =~ /mswin|mingw|cygwin/
      cmd = if is_windows
              ["cmd", "/c", "npx", "tsx", server_path]
            else
              ["npx", "tsx", server_path]
            end

      @process_stdin, @process_stdout, @process_stderr, @process_thread =
        Open3.popen3(*cmd, chdir: harness_dir)

      # Read the first line to get the listening URL
      line = @process_stdout.gets
      unless line
        stop
        raise "Failed to read proxy URL — server produced no output"
      end

      match = line.strip.match(/Listening: (http:\/\/\S+)/)
      unless match
        stop
        raise "Unexpected proxy output: #{line}"
      end

      @proxy_url = match[1]
      @pid = @process_thread[:pid] if @process_thread.respond_to?(:[])
    end

    # Configure the proxy for a specific test snapshot.
    #
    # @param snapshot_path [String] absolute path to the YAML snapshot
    # @param work_dir      [String] working directory for the test
    def configure(snapshot_path, work_dir)
      raise "Proxy not started" unless @proxy_url

      uri = URI("#{@proxy_url}/config")
      body = JSON.generate({ "filePath" => snapshot_path, "workDir" => work_dir })

      http = Net::HTTP.new(uri.host, uri.port)
      req = Net::HTTP::Post.new(uri.path, "Content-Type" => "application/json")
      req.body = body

      resp = http.request(req)
      raise "Proxy config failed with status #{resp.code}" unless resp.code == "200"
    end

    # Stop the proxy server gracefully.
    def stop
      if @proxy_url
        begin
          uri = URI("#{@proxy_url}/stop?skipWritingCache=true")
          http = Net::HTTP.new(uri.host, uri.port)
          req = Net::HTTP::Post.new(uri.path + "?" + uri.query)
          req["Content-Length"] = "0"
          http.request(req)
        rescue StandardError
          # Best effort
        end
      end

      @process_stdin&.close  rescue nil
      @process_stdout&.close rescue nil
      @process_stderr&.close rescue nil

      if @process_thread
        begin
          Process.kill("KILL", @process_thread.pid)
        rescue StandardError
          # Already exited
        end
        @process_thread.value rescue nil
      end

      @proxy_url      = nil
      @process_stdin  = nil
      @process_stdout = nil
      @process_stderr = nil
      @process_thread = nil
    end

    # Returns the absolute path to the snapshots directory.
    def self.snapshots_dir
      SNAPSHOTS_DIR
    end

    # Returns the repo root path.
    def self.repo_root
      REPO_ROOT
    end

    # Returns the CLI path for tests.
    def self.cli_path
      env_path = ENV["COPILOT_CLI_PATH"]
      return File.expand_path(env_path) if env_path && File.exist?(env_path)

      cli = File.join(REPO_ROOT, "nodejs", "node_modules", "@github", "copilot", "index.js")
      return File.expand_path(cli) if File.exist?(cli)

      raise "CLI not found. Set COPILOT_CLI_PATH or run 'npm install' in the nodejs directory."
    end

    # Returns environment variables configured for isolated testing.
    def test_env(work_dir, home_dir)
      env = ENV.to_h.dup
      env["COPILOT_API_URL"]  = @proxy_url
      env["COPILOT_HOME"]     = home_dir
      env["XDG_CONFIG_HOME"]  = home_dir
      env["XDG_STATE_HOME"]   = home_dir
      env
    end
  end
end

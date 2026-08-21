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
      @connect_proxy_url = nil
      @ca_file_path      = nil
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

      match = line.strip.match(/Listening:\s+(http:\/\/\S+)\s*(.*)/)
      unless match
        stop
        raise "Unexpected proxy output: #{line}"
      end

      @proxy_url = match[1]
      @pid = @process_thread[:pid] if @process_thread.respond_to?(:[])

      # Parse JSON metadata (connectProxyUrl, caFilePath) if present
      metadata_str = match[2]
      if metadata_str && metadata_str.start_with?("{")
        begin
          meta = JSON.parse(metadata_str)
          @connect_proxy_url = meta["connectProxyUrl"]
          @ca_file_path = meta["caFilePath"]
        rescue JSON::ParserError
          # ignore metadata parse errors
        end
      end
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

      github_modules = File.join(REPO_ROOT, "nodejs", "node_modules", "@github")

      # As of CLI 1.0.64-1 the runnable index.js ships in a platform-specific
      # package (e.g. @github/copilot-linux-x64); prefer it when present.
      platform_cli = Dir.glob(File.join(github_modules, "copilot-*", "index.js"))
                        .reject { |p| p.include?("language-server") }
                        .find { |p| File.exist?(p) }
      return File.expand_path(platform_cli) if platform_cli

      cli = File.join(github_modules, "copilot", "index.js")
      return File.expand_path(cli) if File.exist?(cli)

      raise "CLI not found. Set COPILOT_CLI_PATH or run 'npm install' in the nodejs directory."
    end

    # Returns environment variables configured for isolated testing.
    def test_env(work_dir, home_dir)
      env = ENV.to_h.dup
      env["COPILOT_API_URL"]  = @proxy_url
      env["COPILOT_HOME"]     = home_dir
      env["GH_CONFIG_DIR"]    = home_dir
      env["XDG_CONFIG_HOME"]  = home_dir
      env["XDG_STATE_HOME"]   = home_dir
      # Provide a fake token so the CLI authenticates against the replay proxy
      env["GH_TOKEN"]         = env["GH_TOKEN"] || "fake-test-token"
      env["GITHUB_TOKEN"]     = env["GITHUB_TOKEN"] || "fake-test-token"

      # CONNECT proxy env vars for HTTPS interception
      if @connect_proxy_url
        env["HTTPS_PROXY"] = @connect_proxy_url
        env["https_proxy"] = @connect_proxy_url
        # Exempt loopback so CAPI requests to the replay proxy (127.0.0.1) go
        # direct instead of being routed through the CONNECT proxy (which only
        # handles github hosts and would return 502 for 127.0.0.1). Without this
        # the chat turn's CAPI call fails and the session never becomes idle.
        no_proxy = "127.0.0.1,localhost,::1"
        env["NO_PROXY"] = no_proxy
        env["no_proxy"] = no_proxy
      end
      if @ca_file_path
        env["NODE_EXTRA_CA_CERTS"] = @ca_file_path
        env["SSL_CERT_FILE"] = @ca_file_path
      end

      env
    end
  end
end

# E2E test harness for the Crystal Copilot SDK.
#
# Spawns the shared replay proxy (npx tsx test/harness/server.ts),
# parses the `Listening: http://...` URL from stdout, and provides
# helpers for E2E tests.

require "http/client"
require "json"

module CopilotE2E
  class ProxyHandle
    getter url : String
    @process : Process

    def initialize(@url : String, @process : Process)
    end

    # Configure the proxy for a specific test snapshot.
    def configure(file_path : String, work_dir : String)
      body = {filePath: file_path, workDir: work_dir}.to_json
      uri = URI.parse("#{@url}/config")
      HTTP::Client.post(uri, headers: HTTP::Headers{"Content-Type" => "application/json"}, body: body)
    end

    # HTTP GET helper against the proxy.
    def http_get(path : String) : String
      uri = URI.parse("#{@url}#{path}")
      response = HTTP::Client.get(uri)
      response.body
    end

    # HTTP POST helper against the proxy.
    def http_post(path : String, body : String) : String
      uri = URI.parse("#{@url}#{path}")
      headers = HTTP::Headers{
        "Content-Type"  => "application/json",
        "Authorization" => "Bearer fake-token-for-e2e-tests",
      }
      response = HTTP::Client.post(uri, headers: headers, body: body)
      response.body
    end

    # Stop the proxy process.
    def stop
      begin
        @process.signal(:term)
      rescue
        # Best effort
      end
    end
  end

  # Start the replay proxy and return a handle.
  def self.start_proxy : ProxyHandle
    # Path from crystal/e2e to repo-root/test/harness/server.ts
    server_path = File.expand_path("../../test/harness/server.ts", __DIR__)
    cwd = File.expand_path("../../test/harness", __DIR__)

    process = Process.new(
      "npx",
      ["tsx", server_path],
      chdir: cwd,
      output: Process::Redirect::Pipe,
      error: Process::Redirect::Inherit,
    )

    # Read the first line to get the proxy URL
    stdout = process.output
    line = stdout.gets
    raise "Failed to read proxy output" unless line

    if match = line.match(/Listening: (http:\/\/[^\s]+)/)
      url = match[1]
    else
      process.signal(:term)
      raise "Could not parse proxy URL from: #{line}"
    end

    ENV["COPILOT_API_URL"] = url
    puts "[crystal-e2e] Proxy listening at #{url}"

    ProxyHandle.new(url: url, process: process)
  end
end

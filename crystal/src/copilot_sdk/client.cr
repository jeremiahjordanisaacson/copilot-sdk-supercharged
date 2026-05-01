# Copyright (c) Microsoft Corporation. All rights reserved.

# CopilotClient - Main entry point for the Copilot SDK.
#
# This module provides the `CopilotClient` class, which manages the connection
# to the Copilot CLI server and provides session management capabilities.
# It can spawn a CLI server process via stdio or connect to an existing
# server over TCP.

require "json"
require "socket"
require "uuid"

module CopilotSDK
  # Error raised by SDK operations.
  class CopilotError < Exception
  end

  # Main client for interacting with the Copilot CLI.
  #
  # The CopilotClient manages the connection to the Copilot CLI server and
  # provides methods to create and manage conversation sessions. It can either
  # spawn a CLI server process or connect to an existing server.
  #
  # ```
  # client = CopilotSDK::CopilotClient.new(
  #   CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
  # )
  # client.start
  #
  # session = client.create_session(CopilotSDK::SessionConfig.new(model: "gpt-4"))
  # session.on { |event|
  #   if content = event.assistant_message_content
  #     puts "Assistant: #{content}"
  #   end
  # }
  #
  # session.send(CopilotSDK::MessageOptions.new(prompt: "Hello!"))
  #
  # session.destroy
  # client.stop
  # ```
  class CopilotClient
    getter state : ConnectionState
    getter options : CopilotClientOptions

    @rpc : JsonRpcClient?
    @cli_process : Process?
    @sessions : Hash(String, CopilotSession)
    @models_cache : Array(ModelInfo)?
    @mutex : Mutex
    @is_external_server : Bool
    @process_stdin : IO?
    @process_stdout : IO?

    def initialize(@options = CopilotClientOptions.new)
      @state = ConnectionState::Disconnected
      @rpc = nil
      @cli_process = nil
      @sessions = {} of String => CopilotSession
      @models_cache = nil
      @mutex = Mutex.new
      @is_external_server = false
      @process_stdin = nil
      @process_stdout = nil
    end

    # Start the connection to the Copilot CLI server.
    # If `cli_url` is set, connects to an existing server via TCP.
    # Otherwise, spawns a new CLI process using stdio.
    def start : Nil
      raise CopilotError.new("Already connected") if @state == ConnectionState::Connected

      @state = ConnectionState::Connecting

      begin
        if url = @options.cli_url
          connect_to_server(url)
        else
          spawn_cli_process
        end

        register_rpc_handlers
        rpc.start

        verify_protocol_version

        @state = ConnectionState::Connected
      rescue ex
        @state = ConnectionState::Error
        raise CopilotError.new("Failed to start: #{ex.message}")
      end
    end

    # Stop the connection and clean up all resources.
    def stop : Nil
      # Destroy all active sessions
      sessions_to_destroy = @mutex.synchronize { @sessions.values.dup }
      sessions_to_destroy.each do |session|
        begin
          session.destroy
        rescue
          # Best-effort cleanup
        end
      end
      @mutex.synchronize { @sessions.clear }

      # Stop the RPC client
      if r = @rpc
        r.stop
      end

      # Terminate the CLI process if we spawned one
      if !@is_external_server
        if stdin = @process_stdin
          begin
            stdin.close
          rescue
          end
        end
        # The process should exit when stdin closes
      end

      @rpc = nil
      @cli_process = nil
      @process_stdin = nil
      @process_stdout = nil
      @models_cache = nil
      @state = ConnectionState::Disconnected
    end

    # Create a new conversation session.
    def create_session(config : SessionConfig = SessionConfig.new) : CopilotSession
      ensure_connected!

      params = JSON.parse(config.to_json)
      result = rpc.send_request("session/create", params, @options.request_timeout)

      session_id = result["sessionId"]?.try(&.as_s) || raise CopilotError.new("No sessionId in response")
      model = result["model"]?.try(&.as_s) || config.model

      session = CopilotSession.new(session_id, rpc, model)
      @mutex.synchronize { @sessions[session_id] = session }

      session
    end

    # Resume an existing session by ID.
    def resume_session(config : ResumeSessionConfig) : CopilotSession
      ensure_connected!

      params = JSON.parse(config.to_json)
      result = rpc.send_request("session/resume", params, @options.request_timeout)

      session_id = config.session_id
      model = result["model"]?.try(&.as_s) || config.model

      session = CopilotSession.new(session_id, rpc, model)
      @mutex.synchronize { @sessions[session_id] = session }

      session
    end

    # Get the status of the connected server.
    def get_status : GetStatusResponse
      ensure_connected!
      result = rpc.send_request("getStatus")
      GetStatusResponse.from_json(result.to_json)
    end

    # Get the authentication status.
    def get_auth_status : GetAuthStatusResponse
      ensure_connected!
      result = rpc.send_request("getAuthStatus")
      GetAuthStatusResponse.from_json(result.to_json)
    end

    # List available models from the server.
    def list_models(force_refresh : Bool = false) : Array(ModelInfo)
      ensure_connected!

      if !force_refresh && (cached = @models_cache)
        return cached
      end

      result = rpc.send_request("listModels")
      models_array = result.as_a? || [] of JSON::Any
      models = models_array.map { |m| ModelInfo.from_json(m.to_json) }
      @models_cache = models
      models
    end

    # List active sessions.
    def list_sessions : Array(SessionMetadata)
      ensure_connected!
      result = rpc.send_request("session/list")
      sessions_array = result.as_a? || [] of JSON::Any
      sessions_array.map { |s| SessionMetadata.from_json(s.to_json) }
    end

    # Get the foreground session ID.
    def get_foreground_session_id : String
      ensure_connected!
      result = rpc.send_request("session.getForeground")
      result["sessionId"]?.try(&.as_s) || raise CopilotError.new("No sessionId in response")
    end

    # Set the foreground session ID.
    def set_foreground_session_id(session_id : String) : Nil
      ensure_connected!
      params = JSON.parse({"sessionId" => session_id}.to_json)
      result = rpc.send_request("session.setForeground", params)
      success = result["success"]?.try(&.as_bool?) || false
      unless success
        error_msg = result["error"]?.try(&.as_s?) || "Unknown"
        raise CopilotError.new("Failed to set foreground session: #{error_msg}")
      end
    end

    # Ping the server to verify connectivity.
    def ping(message : String = "ping") : JSON::Any
      ensure_connected!
      params = JSON.parse({"message" => message}.to_json)
      rpc.send_request("ping", params)
    end

    # Get the last session ID, or nil if none.
    def get_last_session_id : String?
      ensure_connected!
      result = rpc.send_request("session.getLastId")
      result["sessionId"]?.try(&.as_s)
    end

    # Get metadata for a session by ID.
    def get_session_metadata(session_id : String) : JSON::Any
      ensure_connected!
      params = JSON.parse({"sessionId" => session_id}.to_json)
      rpc.send_request("session.getMetadata", params)
    end

    # Returns the session with the given ID, or nil.
    def get_session(session_id : String) : CopilotSession?
      @mutex.synchronize { @sessions[session_id]? }
    end

    # Returns true if the client is currently connected.
    def connected? : Bool
      @state == ConnectionState::Connected
    end

    # --------------------------------------------------------------------------
    # Private
    # --------------------------------------------------------------------------

    private def rpc : JsonRpcClient
      @rpc || raise CopilotError.new("Not connected")
    end

    private def ensure_connected! : Nil
      return if @state == ConnectionState::Connected

      if @options.auto_start && @state == ConnectionState::Disconnected
        start
        return
      end

      raise CopilotError.new("Client is not connected (state: #{@state})")
    end

    private def spawn_cli_process : Nil
      cli_path = @options.cli_path || find_cli_path

      process = Process.new(
        cli_path,
        args: ["--stdio"],
        input: Process::Redirect::Pipe,
        output: Process::Redirect::Pipe,
        error: Process::Redirect::Close
      )

      @cli_process = process
      @process_stdin = process.input
      @process_stdout = process.output
      @is_external_server = false

      stdin = process.input || raise CopilotError.new("Failed to get CLI stdin")
      stdout = process.output || raise CopilotError.new("Failed to get CLI stdout")

      @rpc = JsonRpcClient.new(reader: stdout, writer: stdin)
    end

    private def connect_to_server(url : String) : Nil
      host, port_str = url.split(":", 2)
      port = port_str.to_i

      socket = TCPSocket.new(host, port)
      @is_external_server = true

      @rpc = JsonRpcClient.new(reader: socket, writer: socket)
    end

    private def find_cli_path : String
      # Check environment variable
      if env_path = ENV["COPILOT_CLI_PATH"]?
        return env_path if File.exists?(env_path)
      end

      # Check common locations
      candidates = [
        File.join(Dir.current, "node_modules", ".bin", "copilot-cli"),
        File.join(Dir.current, "node_modules", "@github", "copilot", "index.js"),
      ]

      candidates.each do |candidate|
        return candidate if File.exists?(candidate)
      end

      raise CopilotError.new(
        "Could not find Copilot CLI. Set COPILOT_CLI_PATH, pass cli_path in options, " \
        "or install @github/copilot via npm."
      )
    end

    private def verify_protocol_version : Nil
      status = get_status
      if version = status.protocol_version
        if version < MIN_PROTOCOL_VERSION
          raise CopilotError.new(
            "Server protocol version #{version} is below minimum #{MIN_PROTOCOL_VERSION}"
          )
        end
      end
    end

    private def register_rpc_handlers : Nil
      r = rpc

      # Handle tool call requests from the server
      r.on_request("toolCall") do |params|
        payload = ToolCallRequestPayload.from_json(params.to_json)
        session = @mutex.synchronize { @sessions[payload.session_id]? }

        if session
          session.handle_tool_call(payload)
        else
          JSON.parse(%Q({"error": "Unknown session: #{payload.session_id}"}))
        end
      end

      # Handle permission requests from the server
      r.on_request("permissionRequest") do |params|
        request = PermissionRequest.from_json(params.to_json)
        session = @mutex.synchronize { @sessions[request.session_id]? }

        if session
          result = session.handle_permission_request(request)
          JSON.parse(result.to_json)
        else
          JSON.parse(%Q({"allowed": false, "reason": "Unknown session"}))
        end
      end

      # Handle session events (notifications) from the server
      r.on_notification("session/event") do |params|
        session_id = params["sessionId"]?.try(&.as_s)
        event_type = params["type"]?.try(&.as_s)

        if session_id && event_type
          event = SessionEvent.new(type: event_type, data: params["data"]?)
          if session = @mutex.synchronize { @sessions[session_id]? }
            session.dispatch_event(event)
          end
        end
      end

      # Handle turn.end notifications
      r.on_notification("session/turnEnd") do |params|
        session_id = params["sessionId"]?.try(&.as_s)
        if session_id
          event = SessionEvent.new(type: "turn.end", data: params)
          if session = @mutex.synchronize { @sessions[session_id]? }
            session.dispatch_event(event)
          end
        end
      end
    end
  end
end

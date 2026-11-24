# frozen_string_literal: true

# Copyright (c) Microsoft Corporation. All rights reserved.

require "open3"
require "securerandom"
require "socket"
require "thread"
require "fileutils"
require "time"

module Copilot
  # Main client for interacting with the Copilot CLI.
  #
  # The CopilotClient manages the connection to the Copilot CLI server and provides
  # methods to create and manage conversation sessions. It can either spawn a CLI
  # server process or connect to an existing server.
  #
  # @example
  #   client = Copilot::CopilotClient.new(cli_path: "/usr/local/bin/copilot")
  #   client.start
  #
  #   session = client.create_session(model: "gpt-4")
  #   response = session.send_and_wait(prompt: "Hello!")
  #   puts response&.data&.dig("content")
  #
  #   session.destroy
  #   client.stop
  class CopilotClient
    # @return [String] the current connection state
    attr_reader :state

    # Create a new CopilotClient.
    #
    # @param cli_path         [String, nil]  path to the Copilot CLI executable
    # @param cli_args         [Array<String>] extra arguments for the CLI
    # @param cwd              [String, nil]  working directory for the CLI process
    # @param port             [Integer]      TCP port (0 = random); ignored in stdio mode
    # @param use_stdio        [Boolean]      use stdio transport (default: true)
    # @param cli_url          [String, nil]  URL of an existing server ("host:port")
    # @param log_level        [String]       log level for the CLI ("info", "debug", etc.)
    # @param auto_start       [Boolean]      auto-start on first use (default: true)
    # @param auto_restart     [Boolean]      auto-restart if the server crashes (default: true)
    # @param env              [Hash, nil]    environment variables for the CLI process
    # @param github_token     [String, nil]  GitHub token for authentication
    # @param use_logged_in_user [Boolean, nil] use logged-in user auth (default: true unless github_token)
    # @param client_info      [ClientInfo, Hash, nil] identity of the integrating application
    #   (application_name, application_version, integration_name, integration_version),
    #   declared on the +connect+ handshake when any field is set
    # @param mode             [String] client mode: "coding-agent" (default) or "empty"
    #   (see {ClientMode}); "copilot-cli" is accepted as an alias of "coding-agent"
    # @param network          [NetworkConfig, Hash, nil] host network callbacks
    #   (scope, resolve_proxy, authenticate_negotiate, authenticate_basic), registered
    #   with +network.setProvider+ after connecting
    #
    # @raise [ArgumentError] if mutually exclusive options are provided
    def initialize(
      cli_path: nil,
      cli_args: [],
      cwd: nil,
      port: 0,
      use_stdio: true,
      cli_url: nil,
      log_level: "info",
      auto_start: true,
      auto_restart: true,
      env: nil,
      github_token: nil,
      use_logged_in_user: nil,
      on_get_trace_context: nil,
      client_info: nil,
      mode: ClientMode::CODING_AGENT,
      network: nil
    )
      # Validate mutually exclusive options
      if cli_url && (use_stdio == false || cli_path)
        # cli_url with explicit use_stdio=false is fine; but cli_url with cli_path is not
      end
      if cli_url && cli_path
        raise ArgumentError, "cli_url is mutually exclusive with cli_path"
      end
      if cli_url && (github_token || !use_logged_in_user.nil?)
        raise ArgumentError,
              "github_token and use_logged_in_user cannot be used with cli_url " \
              "(external server manages its own auth)"
      end

      @is_external_server = false
      @actual_host = "localhost"
      @actual_port = nil

      if cli_url
        @actual_host, @actual_port = parse_cli_url(cli_url)
        @is_external_server = true
      end

      # Default use_logged_in_user based on github_token
      use_logged_in_user = github_token ? false : true if use_logged_in_user.nil?

      client_info = ClientInfo.new(**client_info.transform_keys(&:to_sym)) if client_info.is_a?(Hash)

      mode = (mode || ClientMode::CODING_AGENT).to_s
      mode = ClientMode::CODING_AGENT if mode == ClientMode::COPILOT_CLI
      unless [ClientMode::CODING_AGENT, ClientMode::EMPTY].include?(mode)
        raise ArgumentError, "mode must be \"#{ClientMode::CODING_AGENT}\" or \"#{ClientMode::EMPTY}\""
      end

      network = NetworkConfig.new(**network.transform_keys(&:to_sym)) if network.is_a?(Hash)
      if network && !network.scope.nil? && ![NetworkScope::CONNECTION, NetworkScope::RUNTIME].include?(network.scope.to_s)
        raise ArgumentError, "network scope must be \"#{NetworkScope::CONNECTION}\" or \"#{NetworkScope::RUNTIME}\""
      end

      @options = ClientOptions.new(
        cli_path: cli_path || "copilot",
        cli_args: cli_args,
        cwd: cwd || Dir.pwd,
        port: port,
        use_stdio: cli_url ? false : use_stdio,
        cli_url: cli_url,
        log_level: log_level,
        auto_start: auto_start,
        auto_restart: auto_restart,
        env: env,
        github_token: github_token,
        use_logged_in_user: use_logged_in_user,
        on_get_trace_context: on_get_trace_context,
        client_info: client_info,
        mode: mode,
        network: network,
      )

      @process    = nil
      @stdin      = nil
      @stdout     = nil
      @stderr     = nil
      @rpc_client = nil
      @socket     = nil
      @state      = ConnectionState::DISCONNECTED

      @sessions      = {}
      @sessions_lock = Mutex.new

      @models_cache      = nil
      @models_cache_lock = Mutex.new

      @lifecycle_handlers       = []
      @typed_lifecycle_handlers = {} # type => [handler]
      @lifecycle_handlers_lock  = Mutex.new

      # registration id => { provider:, session_id: } for session github_token_provider callbacks
      @github_token_providers      = {}
      @github_token_providers_lock = Mutex.new

      @stderr_thread = nil
    end

    # Start the CLI server and establish a connection.
    #
    # If connecting to an external server (via +cli_url+), only establishes the connection.
    # Otherwise, spawns the CLI server process and then connects.
    #
    # @return [void]
    # @raise [RuntimeError] if the server fails to start or the connection fails
    def start
      return if @state == ConnectionState::CONNECTED

      @state = ConnectionState::CONNECTING
      begin
        start_cli_server unless @is_external_server
        connect_to_server
        send_client_info
        verify_protocol_version
        register_network_provider
        @state = ConnectionState::CONNECTED
      rescue StandardError
        @state = ConnectionState::ERROR
        raise
      end
    end

    # Stop the CLI server and close all active sessions.
    #
    # @return [Array<StopError>] errors encountered during cleanup (empty = success)
    def stop
      errors = []

      # Destroy all active sessions
      sessions_to_destroy = @sessions_lock.synchronize do
        s = @sessions.values.dup
        @sessions.clear
        s
      end

      sessions_to_destroy.each do |session|
        begin
          session.destroy
        rescue StandardError => e
          errors << StopError.new(message: "Failed to destroy session #{session.session_id}: #{e.message}")
        end
      end

      # Stop RPC client
      if @rpc_client
        @rpc_client.stop
        @rpc_client = nil
      end

      # Clear models cache
      @models_cache_lock.synchronize { @models_cache = nil }

      # Drop all GitHub token provider registrations
      @github_token_providers_lock.synchronize { @github_token_providers.clear }

      # Close socket if TCP
      if @socket
        begin
          @socket.close
        rescue StandardError => e
          errors << StopError.new(message: "Failed to close socket: #{e.message}")
        end
        @socket = nil
      end

      # Close stdio streams
      [@stdin, @stdout].each do |io|
        begin
          io&.close
        rescue StandardError
          # ignore
        end
      end
      @stdin = @stdout = nil

      # Kill CLI process (only if we spawned it)
      if @process && !@is_external_server
        begin
          Process.kill("TERM", @process)
          Process.wait(@process)
        rescue Errno::ESRCH, Errno::ECHILD
          # Process already gone
        rescue StandardError => e
          errors << StopError.new(message: "Failed to kill CLI process: #{e.message}")
        end
        @process = nil
      end

      @stderr_thread&.join(2.0)
      @stderr_thread = nil

      @state = ConnectionState::DISCONNECTED
      @actual_port = nil unless @is_external_server

      errors
    end

    # Forcefully stop the CLI server without graceful cleanup.
    #
    # @return [void]
    def force_stop
      @sessions_lock.synchronize { @sessions.clear }

      if @rpc_client
        begin
          @rpc_client.stop
        rescue StandardError
          # ignore
        end
        @rpc_client = nil
      end

      @models_cache_lock.synchronize { @models_cache = nil }
      @github_token_providers_lock.synchronize { @github_token_providers.clear }

      if @socket
        begin
          @socket.close
        rescue StandardError
          # ignore
        end
        @socket = nil
      end

      [@stdin, @stdout].each do |io|
        begin
          io&.close
        rescue StandardError
          # ignore
        end
      end
      @stdin = @stdout = nil

      if @process && !@is_external_server
        begin
          Process.kill("KILL", @process)
          Process.wait(@process)
        rescue StandardError
          # ignore
        end
        @process = nil
      end

      @stderr_thread = nil
      @state = ConnectionState::DISCONNECTED
      @actual_port = nil unless @is_external_server
    end

    # Create a new conversation session.
    #
    # @param config [SessionConfig, Hash] session configuration
    # @return [CopilotSession]
    # @raise [RuntimeError] if not connected and auto_start is disabled
    def create_session(**config)
      ensure_connected!
      validate_session_config_for_mode!(config)

      registration_id = register_github_token_provider(config, config[:session_id])
      payload = build_create_session_payload(config)
      payload[:gitHubTokenProviderRegistrationId] = registration_id if registration_id
      payload = Copilot.group_session_params(payload, resume: false)
      Copilot.apply_empty_mode_session_defaults(payload) if @options.mode == ClientMode::EMPTY

      begin
        response = @rpc_client.request("session.create", payload)
      rescue StandardError
        unregister_github_token_provider(registration_id)
        raise
      end

      session_id = response["sessionId"]
      workspace_path = response["workspacePath"]

      session = CopilotSession.new(session_id, @rpc_client, workspace_path)
      session._register_tools(config[:tools])
      session._register_permission_handler(config[:on_permission_request]) if config[:on_permission_request]
      session._register_user_input_handler(config[:on_user_input_request]) if config[:on_user_input_request]
      session._register_hooks(config[:hooks]) if config[:hooks]
      session._register_exit_plan_mode_handler(config[:on_exit_plan_mode]) if config[:on_exit_plan_mode]
      register_session_v3_handlers(session, config)
      if @options.on_get_trace_context
        session._register_trace_context_provider(@options.on_get_trace_context)
      end
      attach_github_token_provider(session, registration_id)

      @sessions_lock.synchronize { @sessions[session_id] = session }
      session
    end

    # Resume an existing session.
    #
    # @param session_id [String] the session ID to resume
    # @param config     [ResumeSessionConfig, Hash] resume configuration
    # @return [CopilotSession]
    def resume_session(session_id, **config)
      ensure_connected!
      validate_session_config_for_mode!(config)

      registration_id = register_github_token_provider(config, session_id)
      payload = build_resume_session_payload(session_id, config)
      payload[:gitHubTokenProviderRegistrationId] = registration_id if registration_id
      payload = Copilot.group_session_params(payload, resume: true)
      Copilot.apply_empty_mode_session_defaults(payload) if @options.mode == ClientMode::EMPTY

      begin
        response = @rpc_client.request("session.resume", payload)
      rescue StandardError
        unregister_github_token_provider(registration_id)
        raise
      end

      resumed_id = response["sessionId"]
      workspace_path = response["workspacePath"]

      session = CopilotSession.new(resumed_id, @rpc_client, workspace_path)
      session._register_tools(config[:tools])
      session._register_permission_handler(config[:on_permission_request]) if config[:on_permission_request]
      session._register_user_input_handler(config[:on_user_input_request]) if config[:on_user_input_request]
      session._register_hooks(config[:hooks]) if config[:hooks]
      session._register_exit_plan_mode_handler(config[:on_exit_plan_mode]) if config[:on_exit_plan_mode]
      register_session_v3_handlers(session, config)
      if @options.on_get_trace_context
        session._register_trace_context_provider(@options.on_get_trace_context)
      end
      attach_github_token_provider(session, registration_id)

      @sessions_lock.synchronize { @sessions[resumed_id] = session }
      session
    end

    # Send a ping to verify connectivity.
    #
    # @param message [String, nil] optional message
    # @return [PingResponse]
    def ping(message = nil)
      raise_not_connected! unless @rpc_client

      result = @rpc_client.request("ping", { message: message })
      PingResponse.from_hash(result)
    end

    # Get CLI status including version and protocol information.
    #
    # @return [GetStatusResponse]
    def get_status
      raise_not_connected! unless @rpc_client

      result = @rpc_client.request("status.get", {})
      GetStatusResponse.from_hash(result)
    end

    # Get current authentication status.
    #
    # @return [GetAuthStatusResponse]
    def get_auth_status
      raise_not_connected! unless @rpc_client

      result = @rpc_client.request("auth.getStatus", {})
      GetAuthStatusResponse.from_hash(result)
    end

    # List available models. Results are cached after the first call.
    #
    # @return [Array<ModelInfo>]
    def list_models
      raise_not_connected! unless @rpc_client

      @models_cache_lock.synchronize do
        return @models_cache.dup if @models_cache

        response = @rpc_client.request("models.list", {})
        models_data = response["models"] || []
        @models_cache = models_data.map { |m| ModelInfo.from_hash(m) }
        @models_cache.dup
      end
    end

    # List all sessions known to the server.
    #
    # @return [Array<SessionMetadata>]
    def list_sessions
      raise_not_connected! unless @rpc_client

      response = @rpc_client.request("session.list", {})
      sessions_data = response["sessions"] || []
      sessions_data.map { |s| SessionMetadata.from_hash(s) }
    end

    # Get the last (most recently updated) session ID.
    #
    # @return [String, nil]
    def get_last_session_id
      raise_not_connected! unless @rpc_client

      response = @rpc_client.request("session.getLastId", {})
      response["sessionId"]
    end

    # Get metadata for a specific session.
    #
    # @param session_id [String]
    # @return [Hash] session metadata
    def get_session_metadata(session_id)
      raise_not_connected! unless @rpc_client

      @rpc_client.request("session.getMetadata", { sessionId: session_id })
    end

    # Delete a session permanently.
    #
    # @param session_id [String]
    # @return [void]
    # @raise [RuntimeError] if deletion fails
    def delete_session(session_id)
      raise_not_connected! unless @rpc_client

      response = @rpc_client.request("session.delete", { sessionId: session_id })
      unless response["success"]
        error = response["error"] || "Unknown error"
        raise "Failed to delete session #{session_id}: #{error}"
      end

      @sessions_lock.synchronize { @sessions.delete(session_id) }
    end

    # Get the foreground session ID (TUI+server mode).
    #
    # @return [String, nil]
    def get_foreground_session_id
      raise_not_connected! unless @rpc_client

      response = @rpc_client.request("session.getForeground", {})
      response["sessionId"]
    end

    # Set the foreground session (TUI+server mode).
    #
    # @param session_id [String]
    # @return [void]
    def set_foreground_session_id(session_id)
      raise_not_connected! unless @rpc_client

      response = @rpc_client.request("session.setForeground", { sessionId: session_id })
      unless response["success"]
        raise response["error"] || "Failed to set foreground session"
      end
    end

    # Sets the session filesystem provider configuration.
    #
    # @param initial_cwd [String, nil] Initial working directory
    # @param session_state_path [String, nil] Path for session state persistence
    # @param conventions [String, nil] Filesystem conventions ("posix" or "windows").
    #   Required by the CLI; defaults to the host platform's conventions when omitted.
    # @return [void]
    def set_session_fs_provider(initial_cwd: nil, session_state_path: nil, conventions: nil)
      raise_not_connected! unless @rpc_client

      conventions ||= (RUBY_PLATFORM =~ /mswin|mingw|cygwin/ ? "windows" : "posix")

      params = {}
      params[:initialCwd] = initial_cwd if initial_cwd
      params[:sessionStatePath] = session_state_path if session_state_path
      params[:conventions] = conventions

      @rpc_client.request("sessionFs.setProvider", params)
    end

    # Clear the runtime's cached managed (enterprise policy) settings so they are
    # re-read on next use.
    #
    # @return [Hash] the raw result
    def clear_managed_settings_cache
      raise_not_connected! unless @rpc_client

      @rpc_client.request("managedSettings.clearCache", {})
    end

    # Save a legacy MCP configuration credential in the runtime-owned secret store
    # (+mcp.secrets.save+). Requires Copilot CLI 1.0.116 or newer.
    #
    # @param secret_id [String] identifier of the credential
    # @param value [String] the credential; it is never logged
    # @param config_dir [String, nil] configuration directory (the runtime's default when nil)
    # @return [McpSecretsSaveResult] +used_keychain+ is true when the OS keychain stored the
    #   credential and false for the file backend
    def mcp_secrets_save(secret_id, value, config_dir: nil)
      raise_not_connected! unless @rpc_client

      params = { secretId: secret_id, value: value }
      params[:configDir] = config_dir if config_dir
      result = @rpc_client.request("mcp.secrets.save", params)
      McpSecretsSaveResult.from_hash(result.is_a?(Hash) ? result : {})
    end

    # Delete a legacy MCP configuration credential from the runtime-owned secret store
    # (+mcp.secrets.delete+). Requires Copilot CLI 1.0.116 or newer.
    #
    # @param secret_id [String] identifier of the credential
    # @param config_dir [String, nil] configuration directory (the runtime's default when nil)
    # @return [void]
    def mcp_secrets_delete(secret_id, config_dir: nil)
      raise_not_connected! unless @rpc_client

      params = { secretId: secret_id }
      params[:configDir] = config_dir if config_dir
      @rpc_client.request("mcp.secrets.delete", params)
      nil
    end

    # Delete the legacy credentials indexed for an MCP server (+mcp.secrets.deleteServer+).
    # Requires Copilot CLI 1.0.116 or newer.
    #
    # @param server_name [String] the MCP server name
    # @param config_dir [String, nil] configuration directory (the runtime's default when nil)
    # @return [void]
    def mcp_secrets_delete_server(server_name, config_dir: nil)
      raise_not_connected! unless @rpc_client

      params = { serverName: server_name }
      params[:configDir] = config_dir if config_dir
      @rpc_client.request("mcp.secrets.deleteServer", params)
      nil
    end

    # Persist a pending plugin extension permission request as a user-scoped approval,
    # before the host answers that request normally (+permissions.extensionApprovals.grant+).
    # Requires Copilot CLI 1.0.116 or newer.
    #
    # @param session_id [String] session that owns the pending request
    # @param request_id [String] id of the pending extension permission request
    # @return [PermissionsExtensionApprovalsGrantResult] whether the request was eligible and
    #   persisted, and the stored approval
    def extension_approvals_grant(session_id, request_id)
      raise_not_connected! unless @rpc_client

      result = @rpc_client.request("permissions.extensionApprovals.grant",
                                   { sessionId: session_id, requestId: request_id })
      PermissionsExtensionApprovalsGrantResult.from_hash(result.is_a?(Hash) ? result : {})
    end

    # List the user-scoped plugin extension approvals (+permissions.extensionApprovals.list+).
    # Requires Copilot CLI 1.0.116 or newer.
    #
    # @param session_id [String, nil] attached session whose configuration profile to list
    #   (the server's profile when nil)
    # @return [PermissionsExtensionApprovalsListResult] the approvals
    def extension_approvals_list(session_id: nil)
      raise_not_connected! unless @rpc_client

      params = {}
      params[:sessionId] = session_id if session_id
      result = @rpc_client.request("permissions.extensionApprovals.list", params)
      PermissionsExtensionApprovalsListResult.from_hash(result.is_a?(Hash) ? result : {})
    end

    # Revoke one user-scoped plugin extension approval (+permissions.extensionApprovals.revoke+).
    # Requires Copilot CLI 1.0.116 or newer.
    #
    # @param extension_name [String] the approval's +extension_name+
    # @param installation_key [String] the approval's +installation_key+, unchanged
    # @param session_id [String, nil] attached session whose configuration profile to use
    # @return [PermissionsExtensionApprovalsRevokeResult] whether a matching approval was revoked
    def extension_approvals_revoke(extension_name, installation_key, session_id: nil)
      raise_not_connected! unless @rpc_client

      params = { extensionName: extension_name, installationKey: installation_key }
      params[:sessionId] = session_id if session_id
      result = @rpc_client.request("permissions.extensionApprovals.revoke", params)
      PermissionsExtensionApprovalsRevokeResult.from_hash(result.is_a?(Hash) ? result : {})
    end

    # Subscribe to session lifecycle events.
    #
    # @overload on(&handler)
    #   Subscribe to all lifecycle events.
    #   @yield [event] called for every lifecycle event
    #   @yieldparam event [SessionLifecycleEvent]
    #   @return [Proc] unsubscribe function
    #
    # @overload on(event_type, &handler)
    #   Subscribe to a specific lifecycle event type.
    #   @param event_type [String]
    #   @yield [event]
    #   @yieldparam event [SessionLifecycleEvent]
    #   @return [Proc] unsubscribe function
    def on(event_type = nil, &handler)
      raise ArgumentError, "Block required" unless handler

      @lifecycle_handlers_lock.synchronize do
        if event_type
          (@typed_lifecycle_handlers[event_type] ||= []) << handler
        else
          @lifecycle_handlers << handler
        end
      end

      -> {
        @lifecycle_handlers_lock.synchronize do
          if event_type
            @typed_lifecycle_handlers[event_type]&.delete(handler)
          else
            @lifecycle_handlers.delete(handler)
          end
        end
      }
    end

    private

    def ensure_connected!
      return if @rpc_client

      if @options.auto_start
        start
      else
        raise_not_connected!
      end
    end

    def raise_not_connected!
      raise "Client not connected. Call start() first."
    end

    def parse_cli_url(url)
      clean = url.sub(%r{^https?://}, "")

      if clean.match?(/\A\d+\z/)
        port = clean.to_i
        raise ArgumentError, "Invalid port in cli_url: #{url}" if port <= 0 || port > 65535

        return ["localhost", port]
      end

      parts = clean.split(":")
      raise ArgumentError, "Invalid cli_url format: #{url}" unless parts.length == 2

      host = parts[0].empty? ? "localhost" : parts[0]
      port = parts[1].to_i
      raise ArgumentError, "Invalid port in cli_url: #{url}" if port <= 0 || port > 65535

      [host, port]
    end

    def start_cli_server
      cli_path = @options.cli_path

      unless File.exist?(cli_path) || which(cli_path)
        raise "Copilot CLI not found at #{cli_path}"
      end

      args = []
      args.concat(@options.cli_args) if @options.cli_args
      args.push("--headless", "--no-auto-update", "--log-level", @options.log_level)

      # Auth flags
      if @options.github_token
        args.push("--auth-token-env", "COPILOT_SDK_AUTH_TOKEN")
      end
      unless @options.use_logged_in_user
        args.push("--no-auto-login")
      end

      # Transport mode
      if @options.use_stdio
        args.push("--stdio")
      elsif @options.port > 0
        args.push("--port", @options.port.to_s)
      end

      # Build environment
      spawn_env = @options.env ? @options.env.dup : ENV.to_h
      spawn_env["COPILOT_SDK_AUTH_TOKEN"] = @options.github_token if @options.github_token

      cmd = cli_path.end_with?(".js") ? ["node", cli_path] + args : [cli_path] + args

      if @options.use_stdio
        @stdin, @stdout, @stderr, wait_thr = Open3.popen3(spawn_env, *cmd, chdir: @options.cwd)
        @process = wait_thr.pid

        # Forward stderr in background
        @stderr_thread = Thread.new do
          @stderr.each_line do |line|
            $stderr.puts("[CLI subprocess] #{line.rstrip}") unless line.strip.empty?
          end
        rescue IOError
          # stream closed
        end
      else
        @stdin, @stdout, @stderr, wait_thr = Open3.popen3(spawn_env, *cmd, chdir: @options.cwd)
        @process = wait_thr.pid

        # Wait for port announcement
        deadline = Time.now + 10
        found = false
        @stdout.each_line do |line|
          if (match = line.match(/listening on port (\d+)/i))
            @actual_port = match[1].to_i
            found = true
            break
          end
          break if Time.now > deadline
        end

        raise "Timeout waiting for CLI server to start" unless found
      end
    end

    def connect_to_server
      if @options.use_stdio
        connect_via_stdio
      else
        connect_via_tcp
      end
    end

    def connect_via_stdio
      raise "CLI process not started" unless @stdout && @stdin

      @rpc_client = JsonRpcClient.new(@stdout, @stdin)
      attach_connection_handlers
      @rpc_client.start
    end

    def connect_via_tcp
      raise "Server port not available" unless @actual_port

      @socket = TCPSocket.new(@actual_host, @actual_port)
      @rpc_client = JsonRpcClient.new(@socket, @socket)
      attach_connection_handlers
      @rpc_client.start
    end

    def attach_connection_handlers
      # Notifications
      @rpc_client.on_notification do |method, params|
        case method
        when "session.event"
          handle_session_event_notification(params)
        when "session.lifecycle"
          handle_session_lifecycle_notification(params)
        end
      end

      # Server -> Client requests
      @rpc_client.on_request("tool.call") do |params|
        handle_tool_call_request(params)
      end

      @rpc_client.on_request("permission.request") do |params|
        handle_permission_request(params)
      end

      @rpc_client.on_request("userInput.request") do |params|
        handle_user_input_request(params)
      end

      @rpc_client.on_request("hooks.invoke") do |params|
        handle_hooks_invoke(params)
      end

      @rpc_client.on_request("exitPlanMode.request") do |params|
        handle_exit_plan_mode_request(params)
      end

      @rpc_client.on_request("gitHubToken.getToken") do |params|
        handle_github_token_request(params)
      end

      # Shell authentication prompts run off the reader thread so that a
      # shellAuthentication.cancel can be processed while the handler prompts.
      @rpc_client.on_request("shellAuthentication.request", async: true) do |params|
        handle_shell_authentication_request(params)
      end

      @rpc_client.on_request("shellAuthentication.cancel") do |params|
        handle_shell_authentication_cancel(params)
      end

      @rpc_client.on_request("memoryStore.store", async: true) do |params|
        handle_memory_store_request(params)
      end

      register_network_handlers
      register_session_fs_handlers
    end

    # Registers the client-side session filesystem request handlers.
    #
    # When a session filesystem provider is set via {#set_session_fs_provider},
    # the CLI drives filesystem access by sending +sessionFs.*+ requests back to
    # the client. These handlers service those requests against the real
    # filesystem using the absolute paths supplied by the CLI. Errors are
    # returned in-band as +{ code:, message: }+ (SessionFsError) rather than
    # raised, matching the reference SDK contract.
    def register_session_fs_handlers
      @rpc_client.on_request("sessionFs.readFile") { |params| handle_session_fs_read_file(params) }
      @rpc_client.on_request("sessionFs.writeFile") { |params| handle_session_fs_write_file(params) }
      @rpc_client.on_request("sessionFs.appendFile") { |params| handle_session_fs_append_file(params) }
      @rpc_client.on_request("sessionFs.exists") { |params| handle_session_fs_exists(params) }
      @rpc_client.on_request("sessionFs.stat") { |params| handle_session_fs_stat(params) }
      @rpc_client.on_request("sessionFs.mkdir") { |params| handle_session_fs_mkdir(params) }
      @rpc_client.on_request("sessionFs.readdir") { |params| handle_session_fs_readdir(params) }
      @rpc_client.on_request("sessionFs.readdirWithTypes") { |params| handle_session_fs_readdir_with_types(params) }
      @rpc_client.on_request("sessionFs.rm") { |params| handle_session_fs_rm(params) }
      @rpc_client.on_request("sessionFs.rename") { |params| handle_session_fs_rename(params) }
    end

    # Declare the integrating application's identity (+client_info+) on the
    # +connect+ handshake, before the protocol-version ping. Skipped when no field
    # is set; older CLIs that do not implement +connect+ are tolerated.
    def send_client_info
      wire = @options.client_info&.to_wire
      return unless wire

      @rpc_client.request("connect", { clientInfo: wire })
    rescue JsonRpcError => e
      raise unless e.code == -32601 || e.message.include?("Unhandled method connect")
    end

    def verify_protocol_version
      expected = Copilot.sdk_protocol_version
      result = ping
      server_version = result.protocol_version

      if server_version.nil?
        raise "SDK protocol version mismatch: SDK supports version #{expected}, " \
              "but server does not report a protocol version. " \
              "Please update your server to ensure compatibility."
      end

      if server_version != expected
        raise "SDK protocol version mismatch: SDK supports version #{expected}, " \
              "but server reports version #{server_version}. " \
              "Please update your SDK or server to ensure compatibility."
      end
    end

    # Registers the host network callbacks with +network.setProvider+. An empty object
    # marks each callback the client answers; the scope is sent only when set.
    def register_network_provider
      network = @options.network
      return unless network&.any_callback?

      params = {}
      params[:scope] = network.scope.to_s if network.scope
      params[:resolveProxy] = {} if network.resolve_proxy
      params[:authenticateNegotiate] = {} if network.authenticate_negotiate
      params[:authenticateBasic] = {} if network.authenticate_basic
      @rpc_client.request("network.setProvider", params)
    end

    # Registers the server -> client network callback handlers for the configured
    # callbacks. They run off the reader thread so slow lookups never stall it.
    def register_network_handlers
      network = @options.network
      return unless network

      if network.resolve_proxy
        @rpc_client.on_request("network.resolveProxy", async: true) do |params|
          handle_network_resolve_proxy(params)
        end
      end
      if network.authenticate_negotiate
        @rpc_client.on_request("network.authenticateNegotiate", async: true) do |params|
          handle_network_authenticate_negotiate(params)
        end
      end
      if network.authenticate_basic
        @rpc_client.on_request("network.authenticateBasic", async: true) do |params|
          handle_network_authenticate_basic(params)
        end
      end
    end

    # ---- Notification handlers ----

    def handle_session_event_notification(params)
      session_id = params["sessionId"]
      event_hash = params["event"]
      return unless session_id && event_hash

      event = SessionEvent.from_hash(event_hash)
      session = @sessions_lock.synchronize { @sessions[session_id] }
      session&._dispatch_event(event)
    end

    def handle_session_lifecycle_notification(params)
      event = SessionLifecycleEvent.from_hash(params)

      handlers = @lifecycle_handlers_lock.synchronize do
        typed = @typed_lifecycle_handlers[event.type]&.dup || []
        wildcard = @lifecycle_handlers.dup
        typed + wildcard
      end

      handlers.each do |handler|
        handler.call(event)
      rescue StandardError
        # ignore handler errors
      end
    end

    # ---- Server request handlers ----

    def handle_tool_call_request(params)
      session_id = params["sessionId"]
      tool_call_id = params["toolCallId"]
      tool_name = params["toolName"]
      arguments = params["arguments"]

      raise "Invalid tool call payload" unless session_id && tool_call_id && tool_name

      session = @sessions_lock.synchronize { @sessions[session_id] }
      raise "Unknown session #{session_id}" unless session

      handler = session._get_tool_handler(tool_name)
      unless handler
        return { result: build_unsupported_tool_result(tool_name) }
      end

      execute_tool_call(session_id, tool_call_id, tool_name, arguments, handler)
    end

    def execute_tool_call(session_id, tool_call_id, tool_name, arguments, handler)
      invocation = ToolInvocation.new(
        session_id: session_id,
        tool_call_id: tool_call_id,
        tool_name: tool_name,
        arguments: arguments
      )

      begin
        result = handler.call(arguments, invocation)
        { result: Copilot.normalize_tool_result(result) }
      rescue StandardError => e
        {
          result: {
            textResultForLlm: "Invoking this tool produced an error. Detailed information is not available.",
            resultType: ToolResultType::FAILURE,
            error: e.message,
            toolTelemetry: {},
          }
        }
      end
    end

    def handle_permission_request(params)
      session_id = params["sessionId"]
      permission_request = params["permissionRequest"]

      raise "Invalid permission request payload" unless session_id && permission_request

      session = @sessions_lock.synchronize { @sessions[session_id] }
      raise "Session not found: #{session_id}" unless session

      begin
        result = session._handle_permission_request(permission_request)
        { result: result }
      rescue StandardError
        { result: { kind: PermissionKind::DENIED_NO_APPROVAL } }
      end
    end

    def handle_user_input_request(params)
      session_id = params["sessionId"]
      question = params["question"]

      raise "Invalid user input request payload" unless session_id && question

      session = @sessions_lock.synchronize { @sessions[session_id] }
      raise "Session not found: #{session_id}" unless session

      result = session._handle_user_input_request(params)
      reply = { answer: result[:answer], wasFreeform: result[:wasFreeform] }
      attachments = result[:attachments]
      reply[:attachments] = attachments.map { |a| a.respond_to?(:to_wire) ? a.to_wire : a } if attachments
      reply
    end

    def handle_hooks_invoke(params)
      session_id = params["sessionId"]
      hook_type = params["hookType"]
      input_data = params["input"]

      raise "Invalid hooks invoke payload" unless session_id && hook_type

      session = @sessions_lock.synchronize { @sessions[session_id] }
      raise "Session not found: #{session_id}" unless session

      output = session._handle_hooks_invoke(hook_type, input_data)
      { output: output }
    end

    def handle_exit_plan_mode_request(params)
      session_id = params["sessionId"]
      raise "Invalid exit plan mode request payload" unless session_id

      session = @sessions_lock.synchronize { @sessions[session_id] }
      raise "Session not found: #{session_id}" unless session

      begin
        result = session._handle_exit_plan_mode_request(params)
        result
      rescue StandardError
        { approved: true }
      end
    end

    # ---- GitHub token provider (gitHubToken.getToken) ----

    # Validates a session's +github_token_provider+ and stores it in the client-level
    # registry. Returns the opaque registration id sent on the wire, or nil.
    def register_github_token_provider(config, session_id = nil)
      provider = config[:github_token_provider]
      return nil unless provider

      if config[:github_token] || config[:git_hub_token] || config[:auth_token]
        raise ArgumentError, "github_token and github_token_provider are mutually exclusive"
      end
      raise ArgumentError, "github_token_provider must respond to #call" unless provider.respond_to?(:call)

      registration_id = SecureRandom.uuid
      @github_token_providers_lock.synchronize do
        @github_token_providers[registration_id] = { provider: provider, session_id: session_id }
      end
      registration_id
    end

    def unregister_github_token_provider(registration_id)
      return unless registration_id

      @github_token_providers_lock.synchronize { @github_token_providers.delete(registration_id) }
    end

    # Binds a registration to its session and releases it when the session is destroyed.
    def attach_github_token_provider(session, registration_id)
      return unless registration_id

      @github_token_providers_lock.synchronize do
        entry = @github_token_providers[registration_id]
        entry[:session_id] = session.session_id if entry
      end
      session._on_destroy { unregister_github_token_provider(registration_id) }
    end

    def handle_github_token_request(params)
      registration_id = params["registrationId"]
      registration = @github_token_providers_lock.synchronize { @github_token_providers[registration_id] }
      unless registration
        raise "No GitHub token provider registered for registration ID \"#{registration_id}\""
      end

      args = GitHubTokenProviderArgs.new(
        host: params["host"],
        session_id: params["sessionId"] || registration[:session_id],
        reason: params["reason"]
      )
      normalize_github_token_result(registration[:provider].call(args))
    end

    # Accepts a GitHubTokenResult or a Hash with camelCase or snake_case keys.
    def normalize_github_token_result(result)
      return result.to_wire if result.respond_to?(:to_wire)
      raise "GitHub token provider must return a Hash or GitHubTokenResult" unless result.is_a?(Hash)

      h = result.transform_keys(&:to_s)
      return { kind: "cancelled" } if h["kind"].to_s == "cancelled"

      GitHubTokenResult.new(
        kind: "token",
        access_token: h["accessToken"] || h["access_token"],
        token_type: h["tokenType"] || h["token_type"],
        expires_in: h["expiresIn"] || h["expires_in"]
      ).to_wire
    end

    # ---- v3.0.0 session callbacks and client mode ----

    # Registers the session's shell authentication and memory store callbacks.
    def register_session_v3_handlers(session, config)
      if config[:on_shell_authentication_request]
        session._register_shell_authentication_handler(config[:on_shell_authentication_request])
      end
      session._register_memory_store(config[:store_memory]) if config[:store_memory]
    end

    # In "empty" mode every session must explicitly opt into its tools.
    def validate_session_config_for_mode!(config)
      if config[:on_shell_authentication_request] && !config[:on_shell_authentication_request].respond_to?(:call)
        raise ArgumentError, "on_shell_authentication_request must respond to #call"
      end
      if config[:store_memory] && !config[:store_memory].respond_to?(:call)
        raise ArgumentError, "store_memory must respond to #call"
      end
      return unless @options.mode == ClientMode::EMPTY && config[:available_tools].nil?

      raise ArgumentError,
            "CopilotClient is in mode \"empty\" but the session config did not specify " \
            "available_tools. Empty mode requires every session to explicitly opt into " \
            "the tools it wants."
    end

    SHELL_AUTHENTICATION_INPUT_KINDS = [
      ShellAuthenticationInputKind::SECRET,
      ShellAuthenticationInputKind::TEXT,
      ShellAuthenticationInputKind::CONFIRM,
    ].freeze
    private_constant :SHELL_AUTHENTICATION_INPUT_KINDS

    # Answers +shellAuthentication.request+ with +{ action: "accept", value: }+ or
    # +{ action: "cancel" }+. Malformed requests and unknown sessions cancel.
    def handle_shell_authentication_request(params)
      unless params.is_a?(Hash) && params["sessionId"].is_a?(String) && params["requestId"].is_a?(String) &&
             params["program"].is_a?(String) && params["prompt"].is_a?(String) &&
             SHELL_AUTHENTICATION_INPUT_KINDS.include?(params["inputKind"])
        return { action: "cancel" }
      end

      session = @sessions_lock.synchronize { @sessions[params["sessionId"]] }
      return { action: "cancel" } unless session

      session._handle_shell_authentication_request(ShellAuthenticationRequest.from_hash(params))
    end

    # Handles +shellAuthentication.cancel+ by cancelling the pending prompt, if any.
    def handle_shell_authentication_cancel(params)
      if params.is_a?(Hash) && params["sessionId"].is_a?(String) && params["requestId"].is_a?(String)
        session = @sessions_lock.synchronize { @sessions[params["sessionId"]] }
        session&._cancel_shell_authentication_request(params["requestId"])
      end
      {}
    end

    # Handles +memoryStore.store+ by invoking the session's +store_memory+ callback.
    # Answers JSON null on success.
    def handle_memory_store_request(params)
      session_id = params.is_a?(Hash) ? params["sessionId"] : nil
      session = @sessions_lock.synchronize { @sessions[session_id] }
      raise "Memory store owner is unavailable" unless session

      session._handle_memory_store(StoredMemory.from_hash(params))
      nil
    end

    # Answers +network.resolveProxy+ with +{ proxy: }+, or +{}+ to keep the runtime's
    # own proxy resolution.
    def handle_network_resolve_proxy(params)
      request = NetworkResolveProxyRequest.new(url: params["url"])
      proxy = invoke_network_callback("Proxy resolution") { @options.network.resolve_proxy.call(request) }
      proxy.nil? || proxy.to_s.empty? ? {} : { proxy: proxy.to_s }
    end

    # Answers +network.authenticateNegotiate+ with +{ token: }+ or +{}+. The token is never logged.
    def handle_network_authenticate_negotiate(params)
      request = NetworkAuthenticateNegotiateRequest.new(spn: params["spn"])
      token = invoke_network_callback("Negotiate authentication") do
        @options.network.authenticate_negotiate.call(request)
      end
      token.nil? || token.to_s.empty? ? {} : { token: token.to_s }
    end

    # Answers +network.authenticateBasic+ with +{ username:, password: }+ or +{}+. Only the
    # credential fields are sent, and they are never logged.
    def handle_network_authenticate_basic(params)
      request = NetworkAuthenticateBasicRequest.new(
        proxy: params["proxy"],
        realm: params["realm"],
        previous_rejected: params["previousRejected"] ? true : false
      )
      credentials = invoke_network_callback("Basic authentication") do
        @options.network.authenticate_basic.call(request)
      end
      return {} unless credentials

      creds = credentials.respond_to?(:to_h) ? credentials.to_h : {}
      username = creds[:username] || creds["username"]
      password = creds[:password] || creds["password"]
      result = {}
      result[:username] = username.to_s unless username.nil?
      result[:password] = password.to_s unless password.nil?
      result
    end

    # Runs a network callback. Failures are reported by type only so that secrets in an
    # error message are never logged or sent back over the wire.
    def invoke_network_callback(label)
      yield
    rescue StandardError => e
      raise "#{label} callback failed (#{e.class})"
    end

    # ---- SessionFs provider request handlers ----
    #
    # These service the CLI's +sessionFs.*+ callbacks against the real
    # filesystem. Success returns mirror the generated result shapes and errors
    # are returned in-band as a SessionFsError (+{ code:, message: }+) so the CLI
    # can distinguish a missing file (ENOENT) from other failures without the
    # request failing at the JSON-RPC layer.

    def handle_session_fs_read_file(params)
      content = File.read(params["path"], encoding: "UTF-8")
      { content: content }
    rescue StandardError => e
      { content: "", error: session_fs_error(e) }
    end

    def handle_session_fs_write_file(params)
      path = params["path"]
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, params["content"].to_s)
      File.chmod(params["mode"], path) if params["mode"]
      nil
    rescue StandardError => e
      session_fs_error(e)
    end

    def handle_session_fs_append_file(params)
      path = params["path"]
      FileUtils.mkdir_p(File.dirname(path))
      File.open(path, "a") { |f| f.write(params["content"].to_s) }
      File.chmod(params["mode"], path) if params["mode"]
      nil
    rescue StandardError => e
      session_fs_error(e)
    end

    def handle_session_fs_exists(params)
      { exists: File.exist?(params["path"]) }
    rescue StandardError
      { exists: false }
    end

    def handle_session_fs_stat(params)
      st = File.stat(params["path"])
      {
        isFile: st.file?,
        isDirectory: st.directory?,
        size: st.size,
        mtime: st.mtime.utc.iso8601,
        birthtime: session_fs_birthtime(st).utc.iso8601,
      }
    rescue StandardError => e
      now = Time.now.utc.iso8601
      {
        isFile: false,
        isDirectory: false,
        size: 0,
        mtime: now,
        birthtime: now,
        error: session_fs_error(e),
      }
    end

    def handle_session_fs_mkdir(params)
      path = params["path"]
      if params["recursive"]
        FileUtils.mkdir_p(path)
      else
        Dir.mkdir(path)
      end
      File.chmod(params["mode"], path) if params["mode"]
      nil
    rescue StandardError => e
      session_fs_error(e)
    end

    def handle_session_fs_readdir(params)
      { entries: Dir.children(params["path"]) }
    rescue StandardError => e
      { entries: [], error: session_fs_error(e) }
    end

    def handle_session_fs_readdir_with_types(params)
      path = params["path"]
      entries = Dir.children(path).map do |name|
        { name: name, type: File.directory?(File.join(path, name)) ? "directory" : "file" }
      end
      { entries: entries }
    rescue StandardError => e
      { entries: [], error: session_fs_error(e) }
    end

    def handle_session_fs_rm(params)
      path = params["path"]
      force = params["force"]
      if params["recursive"]
        force ? FileUtils.rm_rf(path) : FileUtils.rm_r(path)
      elsif force
        FileUtils.rm_f(path)
      else
        File.delete(path)
      end
      nil
    rescue Errno::ENOENT => e
      params["force"] ? nil : session_fs_error(e)
    rescue StandardError => e
      session_fs_error(e)
    end

    def handle_session_fs_rename(params)
      File.rename(params["src"], params["dest"])
      nil
    rescue StandardError => e
      session_fs_error(e)
    end

    # Returns the creation time when the platform supports it, else falls back to mtime.
    def session_fs_birthtime(stat)
      stat.birthtime
    rescue NotImplementedError
      stat.mtime
    end

    # Maps a Ruby exception to a SessionFsError hash. Missing paths map to
    # "ENOENT"; everything else maps to "UNKNOWN", matching the reference SDK.
    def session_fs_error(err)
      code = err.is_a?(Errno::ENOENT) ? "ENOENT" : "UNKNOWN"
      { code: code, message: err.message }
    end

    # ---- Payload builders ----

    def build_create_session_payload(config, resume: false)
      payload = {}

      payload[:model] = config[:model] if config[:model]
      payload[:sessionId] = config[:session_id] if config[:session_id]
      payload[:reasoningEffort] = config[:reasoning_effort] if config[:reasoning_effort]
      payload[:configDir] = config[:config_dir] if config[:config_dir]

      if config[:tools]
        payload[:tools] = config[:tools].map(&:to_wire)
      end

      payload[:systemMessage] = convert_system_message(config[:system_message]) if config[:system_message]
      payload[:availableTools] = config[:available_tools] if config[:available_tools]
      payload[:excludedTools] = config[:excluded_tools] if config[:excluded_tools]
      payload[:requestPermission] = true if config[:on_permission_request]
      payload[:requestUserInput] = true if config[:on_user_input_request]
      payload[:hooks] = true if config[:hooks]&.respond_to?(:any_handler?) && config[:hooks].any_handler?
      payload[:requestExitPlanMode] = true if config[:on_exit_plan_mode]
      payload[:workingDirectory] = config[:working_directory] if config[:working_directory]
      payload[:streaming] = config[:streaming] unless config[:streaming].nil?
      unless config[:include_sub_agent_streaming_events].nil?
        payload[:includeSubAgentStreamingEvents] = config[:include_sub_agent_streaming_events]
      end
      payload[:enableConfigDiscovery] = config[:enable_config_discovery] unless config[:enable_config_discovery].nil?
      payload[:instructionDirectories] = config[:instruction_directories] if config[:instruction_directories]

      if config[:provider]
        payload[:provider] = config[:provider].respond_to?(:to_wire) ? config[:provider].to_wire : config[:provider]
      end

      token = config[:auth_token] || config[:github_token] || config[:git_hub_token]
      payload[:gitHubToken] = token if token

      payload[:mcpServers] = config[:mcp_servers] if config[:mcp_servers]

      if config[:custom_agents]
        payload[:customAgents] = config[:custom_agents].map { |a|
          a.respond_to?(:to_wire) ? a.to_wire : a
        }
      end

      payload[:skillDirectories] = config[:skill_directories] if config[:skill_directories]
      payload[:disabledSkills] = config[:disabled_skills] if config[:disabled_skills]

      if config[:infinite_sessions]
        is_cfg = config[:infinite_sessions]
        payload[:infiniteSessions] = is_cfg.respond_to?(:to_wire) ? is_cfg.to_wire : is_cfg
      end

      # --- Upstream-sync session options (parity with @github/copilot-sdk) ---
      payload[:enableCitations] = config[:enable_citations] unless config[:enable_citations].nil?
      payload[:excludedBuiltinAgents] = config[:excluded_builtin_agents] if config[:excluded_builtin_agents]
      if config[:session_limits]
        sl = config[:session_limits]
        payload[:sessionLimits] = sl.respond_to?(:to_wire) ? sl.to_wire : sl
      end
      if config[:memory]
        mem = config[:memory]
        payload[:memory] = mem.respond_to?(:to_wire) ? mem.to_wire : mem
      end
      payload[:otlpProtocol] = config[:otlp_protocol] if config[:otlp_protocol]
      # enable_web_socket_responses is sent inside the +capi+ object (see below).
      payload[:expAssignments] = config[:exp_assignments] if config[:exp_assignments]
      # --- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk) ---
      payload[:rewindEnabled] = config[:rewind_enabled] unless config[:rewind_enabled].nil? # enable session rewind
      payload[:additionalDirectories] = config[:additional_directories] if config[:additional_directories]
      payload[:disabledMcpServers] = config[:disabled_mcp_servers] if config[:disabled_mcp_servers]
      payload[:githubMcpToolConfig] = config[:github_mcp_tool_config] if config[:github_mcp_tool_config]
      payload[:canvasProvider] = config[:canvas_provider] if config[:canvas_provider]
      payload[:customAgentsLocalOnly] = config[:custom_agents_local_only] unless config[:custom_agents_local_only].nil?
      payload[:builtinPluginDirectories] = config[:builtin_plugin_directories] if config[:builtin_plugin_directories]
      payload[:argsSchema] = config[:args_schema] if config[:args_schema] # agent-factory authoring schema
      payload[:decisionContext] = config[:decision_context] if config[:decision_context] # permission decision context
      payload[:toolSearch] = config[:tool_search] unless config[:tool_search].nil?
      payload[:inProcess] = config[:in_process] unless config[:in_process].nil?
      payload[:experimentalMode] = config[:experimental_mode] unless config[:experimental_mode].nil?
      payload[:contentExclusion] = config[:content_exclusion] unless config[:content_exclusion].nil?
      # MCP OAuth host token handler: signal to the runtime that a handler is registered.
      payload[:mcpAuthHandler] = true if config[:on_mcp_auth_request]

      # --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) ---
      # Explicit +capi:+ ({ enabled: }) is merged with the Auto tier / WebSocket options.
      capi = {}
      if config[:capi]
        user_capi = config[:capi].respond_to?(:to_wire) ? config[:capi].to_wire : config[:capi]
        capi.merge!(user_capi.transform_keys(&:to_sym)) if user_capi.is_a?(Hash)
      end
      capi[:autoTier] = config[:auto_tier] if config[:auto_tier]
      capi[:enableWebSocketResponses] = config[:enable_web_socket_responses] unless config[:enable_web_socket_responses].nil?
      payload[:capi] = capi unless capi.empty?
      payload[:askUserVariant] = config[:ask_user_variant] if config[:ask_user_variant]
      if config[:sandbox_config]
        sc = config[:sandbox_config]
        payload[:sandboxConfig] = sc.respond_to?(:to_wire) ? sc.to_wire : sc
      end
      payload[:featureFlags] = config[:feature_flags] unless config[:feature_flags].nil?
      payload[:allowedModels] = config[:allowed_models] if config[:allowed_models]
      payload[:authClientIdMetadataUrl] = config[:auth_client_id_metadata_url] if config[:auth_client_id_metadata_url]
      if config[:model_capabilities]
        mc = config[:model_capabilities]
        payload[:modelCapabilities] = mc.respond_to?(:to_wire) ? mc.to_wire : mc
      end
      payload[:agentDirectories] = config[:agent_directories] if config[:agent_directories]
      payload[:hookDirectories] = config[:hook_directories] if config[:hook_directories]
      payload[:interactiveUser] = config[:interactive_user] unless config[:interactive_user].nil?
      payload[:excludedManagedSettings] = config[:excluded_managed_settings] if config[:excluded_managed_settings]
      payload[:enforceManagedModelDefaults] = config[:enforce_managed_model_defaults] unless config[:enforce_managed_model_defaults].nil?
      if config[:image_generation]
        ig = config[:image_generation]
        payload[:imageGeneration] = ig.respond_to?(:to_wire) ? ig.to_wire : ig
      end
      if config[:autopilot]
        ap = config[:autopilot]
        payload[:autopilot] = ap.respond_to?(:to_wire) ? ap.to_wire : ap
      end

      # --- v3.0.0 (SDK protocol 4) session options ---
      if config[:providers]
        payload[:providers] = config[:providers].map { |p| p.respond_to?(:to_wire) ? p.to_wire : p }
      end
      if config[:models]
        payload[:models] = config[:models].map { |m| m.respond_to?(:to_wire) ? m.to_wire : m }
      end
      # On resume an explicit default_provider: nil clears the session's default provider.
      if config[:default_provider] || (resume && config.key?(:default_provider))
        payload[:defaultProvider] = config[:default_provider]
      end
      payload[:requestShellAuthentication] = true if config[:on_shell_authentication_request]
      payload[:hasMemoryStore] = true if config[:store_memory]
      payload[:permissionMode] = config[:permission_mode] if config[:permission_mode]

      unless resume
        # Create-only options.
        payload[:name] = config[:name] if config[:name]
        payload[:trustWorkingDirectory] = config[:trust_working_directory] if config[:trust_working_directory]
        payload[:refreshCustomInstructions] = config[:refresh_custom_instructions] unless config[:refresh_custom_instructions].nil?
      end

      payload
    end

    def build_resume_session_payload(session_id, config)
      payload = build_create_session_payload(config, resume: true)
      payload[:sessionId] = session_id
      payload[:disableResume] = config[:disable_resume] if config[:disable_resume]
      payload[:allowTranscriptRecovery] = config[:allow_transcript_recovery] unless config[:allow_transcript_recovery].nil?
      payload
    end

    def convert_system_message(msg)
      return msg if msg.is_a?(Hash)
      # A plain String is appended to the default system message.
      return { mode: "append", content: msg } if msg.is_a?(String)

      msg.respond_to?(:to_h) ? msg.to_h : msg
    end

    def build_unsupported_tool_result(tool_name)
      {
        textResultForLlm: "Tool '#{tool_name}' is not supported by this client instance.",
        resultType: ToolResultType::FAILURE,
        error: "tool '#{tool_name}' not supported",
        toolTelemetry: {},
      }
    end

    # Check if a command exists on PATH.
    def which(cmd)
      exts = ENV["PATHEXT"] ? ENV["PATHEXT"].split(";") : [""]
      ENV["PATH"].split(File::PATH_SEPARATOR).each do |path|
        exts.each do |ext|
          exe = File.join(path, "#{cmd}#{ext}")
          return exe if File.executable?(exe) && !File.directory?(exe)
        end
      end
      nil
    end
  end
end

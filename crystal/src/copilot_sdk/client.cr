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
    # Resolved session defaults profile (`ClientMode::CODING_AGENT` or `ClientMode::EMPTY`).
    getter mode : String

    @rpc : JsonRpcClient?
    @cli_process : Process?
    @sessions : Hash(String, CopilotSession)
    @models_cache : Array(ModelInfo)?
    @mutex : Mutex
    @is_external_server : Bool
    @process_stdin : IO?
    @process_stdout : IO?
    @github_token_providers : Hash(String, GitHubTokenProvider)

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
      @github_token_providers = {} of String => GitHubTokenProvider
      @mode = CopilotClient.resolve_mode(@options.mode)
    end

    # Resolves `CopilotClientOptions#mode`; `"copilot-cli"` is an alias of
    # `"coding-agent"`.
    def self.resolve_mode(mode : String?) : String
      case mode
      when nil, ClientMode::CODING_AGENT, ClientMode::COPILOT_CLI
        ClientMode::CODING_AGENT
      when ClientMode::EMPTY
        ClientMode::EMPTY
      else
        raise CopilotError.new("Unsupported client mode '#{mode}'; use 'coding-agent' or 'empty'")
      end
    end

    # Start the connection to the Copilot CLI server.
    # If `cli_url` is set, connects to an existing server via TCP.
    # Otherwise, spawns a new CLI process using stdio.
    #
    # The handshake declares `client_info` (`connect`, when configured),
    # verifies that the server speaks SDK protocol 4, and registers the network
    # callbacks (`network.setProvider`, when configured).
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

        send_client_info
        verify_protocol_version
        register_network_provider

        @state = ConnectionState::Connected

        # Set up session filesystem if configured
        if fs = @options.session_fs
          set_session_fs_provider(fs)
        end
      rescue ex
        # Do not keep talking to a server that failed the handshake.
        stop
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
      @mutex.synchronize do
        @sessions.clear
        @github_token_providers.clear
      end

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
    #
    # The flat options are grouped into the SDK protocol 4 payload right before
    # `session.create` is sent. A failure raises `JsonRpcError`; its
    # `policy_helper_failure` exposes a managed-policy Policy Helper failure.
    def create_session(config : SessionConfig = SessionConfig.new) : CopilotSession
      validate_session_options(config.github_token, config.github_token_provider,
        config.available_tools, config.memory_config)
      ensure_connected!

      registration_id = config.github_token_provider ? UUID.random.to_s : nil
      flat = JSON.parse(config.to_json).as_h
      payload = prepare_session_payload(flat, false, config.hooks,
        config.on_shell_authentication_request, config.on_user_input_request,
        config.memory_config, registration_id)
      result = send_session_request("session.create", payload, registration_id,
        config.github_token_provider)

      session_id = result["sessionId"]?.try(&.as_s) || raise CopilotError.new("No sessionId in response")
      model = result["model"]?.try(&.as_s) || config.model

      session = CopilotSession.new(session_id, rpc, model, @options.on_get_trace_context)
      attach_session(session, registration_id, config.hooks,
        config.on_shell_authentication_request, config.on_user_input_request,
        config.memory_config)
      session
    end

    # Resume an existing session by ID.
    #
    # The flat options are grouped into the SDK protocol 4 payload right before
    # `session.resume` is sent (`suppress_resume_event`, alias
    # `disable_resume`, and `allow_transcript_recovery` stay top level).
    def resume_session(config : ResumeSessionConfig) : CopilotSession
      validate_session_options(config.github_token, config.github_token_provider,
        config.available_tools, config.memory_config)
      ensure_connected!

      registration_id = config.github_token_provider ? UUID.random.to_s : nil
      flat = JSON.parse(config.to_json).as_h
      if (legacy = flat.delete("disableResume")) && !flat.has_key?("suppressResumeEvent")
        flat["suppressResumeEvent"] = legacy
      end
      if config.default_provider.nil? && config.clear_default_provider
        flat["defaultProvider"] = JSON::Any.new(nil)
      end
      payload = prepare_session_payload(flat, true, config.hooks,
        config.on_shell_authentication_request, config.on_user_input_request,
        config.memory_config, registration_id)
      result = send_session_request("session.resume", payload, registration_id,
        config.github_token_provider)

      session_id = result["sessionId"]?.try(&.as_s?) || config.session_id
      model = result["model"]?.try(&.as_s) || config.model

      session = CopilotSession.new(session_id, rpc, model, @options.on_get_trace_context)
      attach_session(session, registration_id, config.hooks,
        config.on_shell_authentication_request, config.on_user_input_request,
        config.memory_config)
      session
    end

    # Clear the runtime's cached managed (enterprise) settings so they are
    # fetched again (`managedSettings.clearCache`).
    def clear_managed_settings_cache : Nil
      ensure_connected!
      rpc.send_request("managedSettings.clearCache", JSON::Any.new({} of String => JSON::Any),
        @options.request_timeout)
      nil
    end

    # Save a legacy MCP configuration credential in the runtime-owned secret
    # store (`mcp.secrets.save`). The value is never logged. `config_dir`
    # defaults to the runtime's configuration directory. The result's
    # `used_keychain` is true when the OS keychain stored the credential and
    # false for the file backend. Requires Copilot CLI 1.0.116 or newer.
    def mcp_secrets_save(secret_id : String, value : String,
                         config_dir : String? = nil) : McpSecretsSaveResult
      ensure_connected!
      params = {"secretId" => JSON::Any.new(secret_id), "value" => JSON::Any.new(value)}
      params["configDir"] = JSON::Any.new(config_dir) if config_dir
      result = rpc.send_request("mcp.secrets.save", JSON::Any.new(params), @options.request_timeout)
      return McpSecretsSaveResult.new unless result.as_h?
      McpSecretsSaveResult.from_json(result.to_json)
    end

    # Delete a legacy MCP configuration credential from the runtime-owned
    # secret store (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
    def mcp_secrets_delete(secret_id : String, config_dir : String? = nil) : Nil
      ensure_connected!
      params = {"secretId" => JSON::Any.new(secret_id)}
      params["configDir"] = JSON::Any.new(config_dir) if config_dir
      rpc.send_request("mcp.secrets.delete", JSON::Any.new(params), @options.request_timeout)
      nil
    end

    # Delete every legacy credential indexed for an MCP server
    # (`mcp.secrets.deleteServer`). Requires Copilot CLI 1.0.116 or newer.
    def mcp_secrets_delete_server(server_name : String, config_dir : String? = nil) : Nil
      ensure_connected!
      params = {"serverName" => JSON::Any.new(server_name)}
      params["configDir"] = JSON::Any.new(config_dir) if config_dir
      rpc.send_request("mcp.secrets.deleteServer", JSON::Any.new(params), @options.request_timeout)
      nil
    end

    # Persist a pending plugin extension permission request as a user-scoped
    # approval, before answering that request normally
    # (`permissions.extensionApprovals.grant`). Requires Copilot CLI 1.0.116 or newer.
    def extension_approvals_grant(session_id : String,
                                  request_id : String) : PermissionsExtensionApprovalsGrantResult
      ensure_connected!
      params = {"sessionId" => JSON::Any.new(session_id), "requestId" => JSON::Any.new(request_id)}
      result = rpc.send_request("permissions.extensionApprovals.grant", JSON::Any.new(params),
        @options.request_timeout)
      return PermissionsExtensionApprovalsGrantResult.new unless result.as_h?
      PermissionsExtensionApprovalsGrantResult.from_json(result.to_json)
    end

    # List the user-scoped plugin extension approvals
    # (`permissions.extensionApprovals.list`). `session_id` selects an attached
    # session's configuration profile. Requires Copilot CLI 1.0.116 or newer.
    def extension_approvals_list(session_id : String? = nil) : PermissionsExtensionApprovalsListResult
      ensure_connected!
      params = {} of String => JSON::Any
      params["sessionId"] = JSON::Any.new(session_id) if session_id
      result = rpc.send_request("permissions.extensionApprovals.list", JSON::Any.new(params),
        @options.request_timeout)
      return PermissionsExtensionApprovalsListResult.new unless result.as_h?
      PermissionsExtensionApprovalsListResult.from_json(result.to_json)
    end

    # Revoke one user-scoped plugin extension approval
    # (`permissions.extensionApprovals.revoke`). Pass the approval's
    # `installation_key` back unchanged. Requires Copilot CLI 1.0.116 or newer.
    def extension_approvals_revoke(extension_name : String, installation_key : String,
                                   session_id : String? = nil) : PermissionsExtensionApprovalsRevokeResult
      ensure_connected!
      params = {
        "extensionName"   => JSON::Any.new(extension_name),
        "installationKey" => JSON::Any.new(installation_key),
      }
      params["sessionId"] = JSON::Any.new(session_id) if session_id
      result = rpc.send_request("permissions.extensionApprovals.revoke", JSON::Any.new(params),
        @options.request_timeout)
      return PermissionsExtensionApprovalsRevokeResult.new unless result.as_h?
      PermissionsExtensionApprovalsRevokeResult.from_json(result.to_json)
    end

    # Get the status of the connected server.
    def get_status : GetStatusResponse
      ensure_connected!
      result = rpc.send_request("status.get")
      GetStatusResponse.from_json(result.to_json)
    end

    # Get the authentication status.
    def get_auth_status : GetAuthStatusResponse
      ensure_connected!
      result = rpc.send_request("auth.getStatus")
      GetAuthStatusResponse.from_json(result.to_json)
    end

    # List available models from the server.
    def list_models(force_refresh : Bool = false) : Array(ModelInfo)
      ensure_connected!

      if !force_refresh && (cached = @models_cache)
        return cached
      end

      result = rpc.send_request("models.list")
      models_array = result.as_a? || [] of JSON::Any
      models = models_array.map { |m| ModelInfo.from_json(m.to_json) }
      @models_cache = models
      models
    end

    # List active sessions.
    def list_sessions : Array(SessionMetadata)
      ensure_connected!
      result = rpc.send_request("session.list")
      sessions_array = result.as_a? || [] of JSON::Any
      sessions_array.map { |s| SessionMetadata.from_json(s.to_json) }
    end

    # Delete a session by ID.
    def delete_session(session_id : String) : Nil
      ensure_connected!
      params = JSON.parse({"sessionId" => session_id}.to_json)
      rpc.send_request("session.delete", params, @options.request_timeout)
      @mutex.synchronize { @sessions.delete(session_id) }
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

    # Register a session filesystem provider with the CLI server.
    def set_session_fs_provider(config : SessionFsConfig) : JSON::Any
      ensure_connected!
      params = JSON.parse(config.to_json)
      rpc.send_request("sessionFs.setProvider", params, @options.request_timeout)
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

      args = ["--headless", "--no-auto-update", "--stdio"]
      command = cli_path
      if cli_path.ends_with?(".js")
        # Node entry points (for example `@github/copilot/npm-loader.js`) run with node.
        args.unshift(cli_path)
        command = "node"
      end

      process = Process.new(
        command,
        args: args,
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

    # Resolves the CLI: `COPILOT_CLI_PATH`, then `@github/copilot/npm-loader.js`
    # (Copilot CLI 1.0.113+, run with node), then the legacy entry points.
    private def find_cli_path : String
      # Check environment variable
      if env_path = ENV["COPILOT_CLI_PATH"]?
        return env_path if File.exists?(env_path)
      end

      # Check common locations
      candidates = [
        File.join(Dir.current, "node_modules", "@github", "copilot", "npm-loader.js"),
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

    # Verifies that the server speaks SDK protocol 4 (`ping` reports the version).
    private def verify_protocol_version : Nil
      result = rpc.send_request("ping", JSON::Any.new({"message" => JSON::Any.new("ping")}),
        @options.request_timeout)
      version = result.as_h?.try(&.["protocolVersion"]?).try(&.as_i?)
      unless version
        raise CopilotError.new(
          "SDK protocol version mismatch: SDK supports version #{SDK_PROTOCOL_VERSION}, " \
          "but server does not report a protocol version. " \
          "Please update your server to ensure compatibility."
        )
      end
      if version < MIN_PROTOCOL_VERSION || version > SDK_PROTOCOL_VERSION
        raise CopilotError.new(
          "SDK protocol version mismatch: SDK supports version #{SDK_PROTOCOL_VERSION}, " \
          "but server reports version #{version}. " \
          "Please update your SDK or server to ensure compatibility."
        )
      end
    end

    # Declares `CopilotClientOptions#client_info` on the `connect` handshake.
    # Runtimes without `connect` are tolerated.
    private def send_client_info : Nil
      wire = @options.client_info.try(&.to_wire)
      return unless wire
      begin
        rpc.send_request("connect", JSON::Any.new({"clientInfo" => JSON::Any.new(wire)}),
          @options.request_timeout)
      rescue ex : JsonRpcError
        raise ex unless ex.method_not_found?
      end
    end

    # Calls `network.setProvider` when any network callback is configured,
    # marking each registered callback with an empty object.
    private def register_network_provider : Nil
      network = @options.network
      return unless network && network.has_callbacks?
      params = {} of String => JSON::Any
      if scope = network.scope
        params["scope"] = JSON::Any.new(scope)
      end
      marker = JSON::Any.new({} of String => JSON::Any)
      params["resolveProxy"] = marker if network.resolve_proxy
      params["authenticateNegotiate"] = marker if network.authenticate_negotiate
      params["authenticateBasic"] = marker if network.authenticate_basic
      rpc.send_request("network.setProvider", JSON::Any.new(params), @options.request_timeout)
    end

    # Validates options shared by `create_session` and `resume_session`.
    private def validate_session_options(github_token : String?,
                                         github_token_provider : GitHubTokenProvider?,
                                         available_tools : Array(String)?,
                                         memory : MemoryConfiguration?) : Nil
      if github_token && github_token_provider
        raise CopilotError.new("github_token and github_token_provider are mutually exclusive")
      end
      if @mode == ClientMode::EMPTY && available_tools.nil?
        raise CopilotError.new(
          "CopilotClient is in mode 'empty' but the session config did not specify " \
          "'available_tools'. Empty mode requires every session to opt into the tools it wants."
        )
      end
      if memory && memory.store_memory && memory.enabled == false
        raise CopilotError.new("memory_config.store_memory requires memory_config.enabled to be true.")
      end
    end

    # Adds the handler-derived options to the flat payload, groups it for SDK
    # protocol 4, and applies the empty-mode defaults under the caller's values.
    private def prepare_session_payload(flat : Hash(String, JSON::Any), resume : Bool,
                                        hooks : SessionHooks?,
                                        shell_auth : ShellAuthenticationHandler?,
                                        user_input : UserInputHandler?,
                                        memory : MemoryConfiguration?,
                                        registration_id : String?) : Hash(String, JSON::Any)
      # Protocol 4 takes a system message object; a plain string is appended.
      if system_message = flat["systemMessage"]?.try(&.as_s?)
        flat["systemMessage"] = JSON::Any.new({
          "mode"    => JSON::Any.new("append"),
          "content" => JSON::Any.new(system_message),
        })
      end
      # The legacy WebSocket option lives in `github.capi` on protocol 4.
      if web_socket = flat.delete("enableWebSocketResponses")
        capi = flat["capi"]?.try(&.as_h?) || {} of String => JSON::Any
        capi["enableWebSocketResponses"] = web_socket unless capi.has_key?("enableWebSocketResponses")
        flat["capi"] = JSON::Any.new(capi)
      end
      flat["hooks"] = JSON::Any.new(true) if hooks && hooks.any?
      flat["requestShellAuthentication"] = JSON::Any.new(true) if shell_auth
      flat["requestUserInput"] = JSON::Any.new(true) if user_input
      if memory && memory.store_memory
        flat["hasMemoryStore"] = JSON::Any.new(true)
        wire_memory = flat["memory"]?.try(&.as_h?) || {} of String => JSON::Any
        wire_memory["enabled"] = JSON::Any.new(true) unless wire_memory.has_key?("enabled")
        flat["memory"] = JSON::Any.new(wire_memory)
      end
      if registration_id
        flat["gitHubTokenProviderRegistrationId"] = JSON::Any.new(registration_id)
      end

      payload = CopilotSDK.group_session_params(flat, resume)
      payload = CopilotSDK.apply_empty_mode_session_defaults(payload) if @mode == ClientMode::EMPTY
      payload
    end

    # Sends `session.create` / `session.resume` with the GitHub token provider
    # registered, since the runtime validates it while creating the session.
    private def send_session_request(method : String, payload : Hash(String, JSON::Any),
                                     registration_id : String?,
                                     provider : GitHubTokenProvider?) : JSON::Any
      if registration_id && provider
        @mutex.synchronize { @github_token_providers[registration_id] = provider }
      end
      begin
        rpc.send_request(method, JSON::Any.new(payload), @options.request_timeout)
      rescue ex
        if registration_id
          @mutex.synchronize { @github_token_providers.delete(registration_id) }
        end
        raise ex
      end
    end

    # Registers the session and its handlers; detaching the session unregisters
    # it (and its GitHub token provider) from this client.
    private def attach_session(session : CopilotSession, registration_id : String?,
                               hooks : SessionHooks?, shell_auth : ShellAuthenticationHandler?,
                               user_input : UserInputHandler?,
                               memory : MemoryConfiguration?) : Nil
      session.register_hooks(hooks)
      session.register_shell_authentication_handler(shell_auth)
      session.register_user_input_handler(user_input)
      session.register_memory_store(memory.try(&.store_memory))
      session_id = session.session_id
      session.on_close = -> {
        @mutex.synchronize do
          if (current = @sessions[session_id]?) && current.same?(session)
            @sessions.delete(session_id)
          end
          @github_token_providers.delete(registration_id) if registration_id
        end
        nil
      }
      @mutex.synchronize { @sessions[session_id] = session }
    end

    # Answers `gitHubToken.getToken` with the provider registered for the
    # request's `registrationId`.
    private def handle_github_token_request(params : JSON::Any) : JSON::Any
      registration_id = params["registrationId"]?.try(&.as_s?) || ""
      provider = @mutex.synchronize { @github_token_providers[registration_id]? }
      unless provider
        raise CopilotError.new("No GitHub token provider registered for \"#{registration_id}\"")
      end
      result = provider.call(GitHubTokenRequest.new(
        host: params["host"]?.try(&.as_s?) || "",
        session_id: params["sessionId"]?.try(&.as_s?),
        reason: params["reason"]?.try(&.as_s?) || ""
      ))
      result.to_json_any
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

      # Handle exit-plan-mode requests from the server
      r.on_request("exitPlanMode.request") do |params|
        request = ExitPlanModeRequest.from_json(params.to_json)
        session_id = params["sessionId"]?.try(&.as_s)
        session = session_id ? @mutex.synchronize { @sessions[session_id]? } : nil

        if session
          result = session.handle_exit_plan_mode(request)
          JSON.parse(result.to_json)
        else
          JSON.parse(%Q({"approved": true}))
        end
      end

      # Handle session events (notifications) from the server
      r.on_notification("session.event") do |params|
        session_id = params["sessionId"]?.try(&.as_s?)
        event_json = params["event"]?
        event_type = event_json.try(&.as_h?).try(&.["type"]?).try(&.as_s?)

        if session_id && event_json && event_type
          event = SessionEvent.new(
            type: event_type,
            data: event_json["data"]?,
            id: event_json["id"]?.try(&.as_s?),
            timestamp: event_json["timestamp"]?.try(&.as_s?),
            parent_id: event_json["parentId"]?.try(&.as_s?),
            agent_id: event_json["agentId"]?.try(&.as_s?),
            ephemeral: event_json["ephemeral"]?.try(&.as_bool?)
          )
          if session = @mutex.synchronize { @sessions[session_id]? }
            session.dispatch_event(event)
          end
        end
      end

      # Legacy session event notification shape.
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

      # Hook handlers (including subagentStart / subagentStop)
      r.on_request("hooks.invoke") do |params|
        session_id = params["sessionId"]?.try(&.as_s?) || ""
        hook_type = params["hookType"]?.try(&.as_s?) || ""
        session = @mutex.synchronize { @sessions[session_id]? }
        output = session.try &.handle_hooks_invoke(hook_type, params["input"]? || JSON::Any.new(nil))
        JSON::Any.new({"output" => output || JSON::Any.new({} of String => JSON::Any)})
      end

      # GitHub token provider callbacks
      r.on_request("gitHubToken.getToken") do |params|
        handle_github_token_request(params)
      end

      # Shell authentication prompts and their cancellation
      r.on_request("shellAuthentication.request") do |params|
        request = ShellAuthenticationRequest.from_json(params.to_json)
        session = @mutex.synchronize { @sessions[request.session_id]? }
        response = session ? session.handle_shell_authentication_request(request) : ShellAuthenticationResponse.cancel
        response.to_json_any
      end

      r.on_request("shellAuthentication.cancel") do |params|
        session_id = params["sessionId"]?.try(&.as_s?)
        request_id = params["requestId"]?.try(&.as_s?)
        if session_id && request_id
          if session = @mutex.synchronize { @sessions[session_id]? }
            session.cancel_shell_authentication_request(request_id)
          end
        end
        JSON::Any.new({} of String => JSON::Any)
      end

      # ask_user questions (`userInput.request`), including image answers
      r.on_request("userInput.request") do |params|
        request = UserInputRequest.from_json(params.to_json)
        session = @mutex.synchronize { @sessions[request.session_id]? }
        raise CopilotError.new("Session not found: #{request.session_id}") unless session
        session.handle_user_input_request(request).to_json_any
      end

      # App-side memory store (replied to with null)
      r.on_request("memoryStore.store") do |params|
        memory = StoredMemory.from_json(params.to_json)
        session = @mutex.synchronize { @sessions[memory.session_id]? }
        raise CopilotError.new("Memory store owner is unavailable") unless session
        session.handle_memory_store(memory)
        JSON::Any.new(nil)
      end

      register_network_handlers(r)
    end

    # Registers the `network.*` request handlers for the configured callbacks.
    # Tokens and passwords are never logged.
    private def register_network_handlers(r : JsonRpcClient) : Nil
      network = @options.network
      return unless network

      if resolve_proxy = network.resolve_proxy
        r.on_request("network.resolveProxy") do |params|
          proxy = resolve_proxy.call(params["url"]?.try(&.as_s?) || "")
          result = {} of String => JSON::Any
          result["proxy"] = JSON::Any.new(proxy) if proxy && !proxy.empty?
          JSON::Any.new(result)
        end
      end

      if authenticate_negotiate = network.authenticate_negotiate
        r.on_request("network.authenticateNegotiate") do |params|
          token = authenticate_negotiate.call(params["spn"]?.try(&.as_s?) || "")
          result = {} of String => JSON::Any
          result["token"] = JSON::Any.new(token) if token && !token.empty?
          JSON::Any.new(result)
        end
      end

      if authenticate_basic = network.authenticate_basic
        r.on_request("network.authenticateBasic") do |params|
          request = NetworkAuthenticateBasicRequest.new(
            proxy: params["proxy"]?.try(&.as_s?) || "",
            realm: params["realm"]?.try(&.as_s?),
            previous_rejected: params["previousRejected"]?.try(&.as_bool?) || false
          )
          result = {} of String => JSON::Any
          if credentials = authenticate_basic.call(request)
            if username = credentials.username
              result["username"] = JSON::Any.new(username)
            end
            if password = credentials.password
              result["password"] = JSON::Any.new(password)
            end
          end
          JSON::Any.new(result)
        end
      end
    end
  end
end

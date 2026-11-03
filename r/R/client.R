#' CopilotClient
#'
#' Main client for interacting with the GitHub Copilot CLI.
#'
#' @description
#' The CopilotClient manages the connection to the Copilot CLI server and provides
#' methods to create and manage conversation sessions. It spawns the CLI server
#' as a subprocess communicating via JSON-RPC 2.0 over stdio.
#'
#' @export
CopilotClient <- R6::R6Class(
  "CopilotClient",
  public = list(
    #' @field options Named list of client options.
    options = NULL,

    #' @description Create a new CopilotClient.
    #'
    #' @param cli_path Character. Path to the Copilot CLI executable.
    #'   Default: "copilot" (found via PATH).
    #' @param cwd Character. Working directory for the CLI process.
    #'   Default: current working directory.
    #' @param log_level Character. Log level ("none","error","warning","info","debug","all").
    #'   Default: "info".
    #' @param auto_start Logical. Auto-start CLI on first use. Default: TRUE.
    #' @param github_token Character or NULL. GitHub token for authentication.
    #' @param use_logged_in_user Logical. Use stored OAuth / gh CLI auth.
    #'   Default: TRUE (but FALSE when github_token is provided).
    #' @param env Named list or NULL. Additional environment variables.
    #' @param session_fs SessionFsConfig or NULL. Custom session filesystem provider config.
    #' @param client_info ClientInfo, named list, or NULL. Identity of the integrating
    #'   application (application_name, application_version, integration_name,
    #'   integration_version), sent on the \code{connect} handshake when any field is set.
    #'
    #' @return A CopilotClient R6 object.
    #'
    #' @examples
    #' \dontrun{
    #' client <- CopilotClient$new()
    #' client$start()
    #' session <- client$create_session()
    #' }
    initialize = function(cli_path = NULL, cwd = getwd(), log_level = "info",
                          auto_start = TRUE, github_token = NULL,
                          use_logged_in_user = NULL, env = NULL,
                          session_fs = NULL, client_info = NULL) {
      # Determine CLI path
      if (is.null(cli_path)) {
        cli_path <- Sys.which("copilot")
        if (nchar(cli_path) == 0) {
          cli_path <- "copilot"
        }
      }

      # Default use_logged_in_user
      if (is.null(use_logged_in_user)) {
        use_logged_in_user <- is.null(github_token)
      }

      self$options <- list(
        cli_path = cli_path,
        cwd = cwd,
        log_level = log_level,
        auto_start = auto_start,
        github_token = github_token,
        use_logged_in_user = use_logged_in_user,
        env = env,
        client_info = client_info
      )

      private$process <- NULL
      private$client <- NULL
      private$state <- "disconnected"
      private$sessions <- new.env(parent = emptyenv())
      private$models_cache <- NULL
      private$lifecycle_handlers <- list()
      private$session_fs_config <- session_fs
      private$github_token_providers <- new.env(parent = emptyenv())
    },

    #' @description Start the CLI server and establish connection.
    #'
    #' Spawns the Copilot CLI subprocess and connects via stdio.
    #' Verifies protocol version compatibility via ping.
    #'
    #' @return Invisible self.
    start = function() {
      if (private$state == "connected") return(invisible(self))

      private$state <- "connecting"

      tryCatch(
        {
          private$start_cli_server()
          private$connect_to_server()
          private$send_connect_handshake()
          private$verify_protocol_version()

          # Register session filesystem provider if configured
          if (!is.null(private$session_fs_config)) {
            private$client$request("sessionFs.setProvider", list(
              initialCwd = private$session_fs_config$initial_cwd,
              sessionStatePath = private$session_fs_config$session_state_path,
              conventions = private$session_fs_config$conventions
            ))
          }

          private$state <- "connected"
        },
        error = function(e) {
          private$state <- "error"
          stop(e)
        }
      )

      invisible(self)
    },

    #' @description Stop the CLI server and close all sessions.
    #'
    #' @return A list of error messages that occurred during cleanup (empty if all ok).
    stop = function() {
      errors <- character(0)

      # Destroy all sessions
      session_ids <- ls(private$sessions)
      for (sid in session_ids) {
        tryCatch(
          {
            session <- get(sid, envir = private$sessions)
            session$destroy()
          },
          error = function(e) {
            errors <<- c(errors, paste0("Failed to destroy session ", sid, ": ", e$message))
          }
        )
      }
      # Clear sessions
      rm(list = ls(private$sessions), envir = private$sessions)
      rm(list = ls(private$github_token_providers), envir = private$github_token_providers)

      # Stop client
      if (!is.null(private$client)) {
        private$client$stop()
        private$client <- NULL
      }

      # Clear models cache
      private$models_cache <- NULL

      # Kill CLI process
      if (!is.null(private$process)) {
        tryCatch(
          {
            if (private$process$is_alive()) {
              private$process$signal(tools::SIGTERM)
              private$process$wait(timeout = 5000)
            }
          },
          error = function(e) {
            tryCatch(private$process$kill(), error = function(e2) NULL)
          }
        )
        private$process <- NULL
      }

      private$state <- "disconnected"
      errors
    },

    #' @description Force stop without graceful cleanup.
    force_stop = function() {
      rm(list = ls(private$sessions), envir = private$sessions)
      rm(list = ls(private$github_token_providers), envir = private$github_token_providers)

      if (!is.null(private$client)) {
        tryCatch(private$client$stop(), error = function(e) NULL)
        private$client <- NULL
      }

      private$models_cache <- NULL

      if (!is.null(private$process)) {
        tryCatch(private$process$kill(), error = function(e) NULL)
        private$process <- NULL
      }

      private$state <- "disconnected"
      invisible(NULL)
    },

    #' @description Get the current connection state.
    #' @return Character: "disconnected", "connecting", "connected", or "error".
    get_state = function() {
      private$state
    },

    #' @description Create a new conversation session.
    #'
    #' @param model Character or NULL. Model to use (e.g. "gpt-5").
    #' @param session_id Character or NULL. Custom session ID.
    #' @param tools List of Tool R6 objects or NULL.
    #' @param system_message Named list or NULL. System message config.
    #' @param streaming Logical or NULL. Enable streaming delta events.
    #' @param available_tools Character vector or NULL. Tool names to allow.
    #' @param excluded_tools Character vector or NULL. Tool names to exclude.
    #' @param on_permission_request Function or NULL. Permission request handler.
    #' @param on_user_input_request Function or NULL. User input handler.
    #' @param on_elicitation_request Function or NULL. Elicitation request handler.
    #' @param hooks Named list or NULL. Hook handlers.
    #' @param working_directory Character or NULL. Working directory for session.
    #' @param provider Named list or NULL. Custom provider config (BYOK).
    #' @param mcp_servers Named list or NULL. MCP server configurations.
    #' @param config_dir Character or NULL. Override config directory.
    #' @param infinite_sessions Named list or NULL. Infinite session config.
    #' @param github_token Character or NULL. GitHub token for this session.
    #' @param model_capabilities Named list or NULL. Model capabilities overrides.
    #' @param enable_config_discovery Logical or NULL. Auto-discover MCP server configs.
    #' @param include_sub_agent_streaming_events Logical or NULL. Include sub-agent streaming events.
    #' @param commands List of CommandDefinition or NULL. Slash commands for this session.
    #' @param request_headers Named list or NULL. Extra HTTP headers for runtime requests.
    #' @param response_format Character or NULL. Response format ("text", "image", "json_object").
    #' @param enable_citations Logical or NULL. Enable response citations.
    #' @param excluded_builtin_agents Character vector or NULL. Built-in agent names to exclude.
    #' @param session_limits SessionLimitsConfig or NULL. Per-session AI-credit spending limits.
    #' @param memory MemoryConfiguration or NULL. Persistent session memory configuration.
    #' @param otlp_protocol Character or NULL. OTLP telemetry protocol ("grpc" or "http/protobuf").
    #' @param enable_web_socket_responses Logical or NULL. Use the WebSocket response transport.
    #' @param exp_assignments Named list or NULL. Experiment assignment overrides.
    #' @param rewind_enabled Logical or NULL. Enable session rewind.
    #' @param additional_directories Character vector or NULL. Additional session directories.
    #' @param disabled_mcp_servers Character vector or NULL. MCP server names to disable.
    #' @param github_mcp_tool_config Named list or NULL. GitHub MCP tool configuration.
    #' @param canvas_provider Named list, character, or NULL. Canvas provider config.
    #' @param custom_agents_local_only Logical or NULL. Restrict custom agents to local only.
    #' @param builtin_plugin_directories Character vector or NULL. Built-in plugin directories.
    #' @param args_schema Named list or NULL. Agent-factory authoring args schema.
    #' @param decision_context Named list or NULL. Permission decision context.
    #' @param tool_search Named list, logical, or NULL. Tool search configuration.
    #' @param in_process Logical or NULL. Use the in-process FFI transport.
    #' @param experimental_mode Logical or NULL. Enable experimental mode.
    #' @param content_exclusion Logical or NULL. Enable content exclusion.
    #' @param reasoning_effort Character or NULL. Reasoning effort control (e.g. "low", "medium", "high").
    #' @param on_mcp_auth_request Function or NULL. MCP OAuth host token handler.
    #' @param bearer_token_provider Function or NULL. BYOK callback receiving ProviderTokenArgs.
    #' @param agent_mode Character or NULL. Default agent mode applied to sent messages.
    #' @param display_prompt Character or NULL. Default display prompt applied to sent messages.
    #' @param auto_tier Character or NULL. Experimental Auto routing preference
    #'   ("efficiency", "balance", "intelligence", or "fast"); sent inside \code{capi}.
    #' @param ask_user_variant Character or NULL. Ask-user tool variant ("legacy" or "elicitation").
    #' @param github_token_provider Function or NULL. Callback receiving GitHubTokenProviderArgs
    #'   (host, session_id, reason) and returning \code{list(kind = "token", accessToken = ...,
    #'   expiresIn = ...)} or \code{list(kind = "cancelled")}. Mutually exclusive with github_token.
    #' @param sandbox_config SandboxConfig, named list, or NULL. Sandbox configuration.
    #' @param feature_flags Named list of logicals or NULL. Runtime feature-flag overrides
    #'   (sent whenever provided, even when empty).
    #' @param allowed_models Character vector or NULL. Model allowlist for the session.
    #' @param auth_client_id_metadata_url Character or NULL. OAuth client ID metadata document URL.
    #' @param name Character or NULL. Human-readable session name.
    #' @param agent_directories Character vector or NULL. Extra custom-agent directories.
    #' @param hook_directories Character vector or NULL. Extra hook-definition directories.
    #' @param interactive_user Logical or NULL. Whether a human user is present.
    #' @param excluded_managed_settings Character vector or NULL. Managed settings to ignore.
    #' @param enforce_managed_model_defaults Logical or NULL. Enforce managed model defaults.
    #' @param trust_working_directory Character or NULL. Trust scope for the working directory
    #'   ("session" or "user").
    #' @param refresh_custom_instructions Logical or NULL. Re-read custom instructions.
    #' @param image_generation ImageGenerationConfig, named list, or NULL. Image generation opt-in.
    #' @param autopilot AutopilotOptions, named list, or NULL. Autopilot continuation options.
    #'
    #' @return A CopilotSession R6 object.
    create_session = function(model = NULL, session_id = NULL, tools = NULL,
                              system_message = NULL, streaming = NULL,
                              available_tools = NULL, excluded_tools = NULL,
                              on_permission_request = NULL,
                              on_user_input_request = NULL,
                              on_elicitation_request = NULL,
                              hooks = NULL, working_directory = NULL,
                              provider = NULL, mcp_servers = NULL,
                              config_dir = NULL, infinite_sessions = NULL,
                              github_token = NULL, model_capabilities = NULL,
                              enable_config_discovery = NULL,
                              include_sub_agent_streaming_events = NULL,
                              commands = NULL,
                              request_headers = NULL,
                              response_format = NULL,
                              enable_citations = NULL,
                              excluded_builtin_agents = NULL,
                              session_limits = NULL,
                              memory = NULL,
                              otlp_protocol = NULL,
                              enable_web_socket_responses = NULL,
                              exp_assignments = NULL,
                              rewind_enabled = NULL,
                              additional_directories = NULL,
                              disabled_mcp_servers = NULL,
                              github_mcp_tool_config = NULL,
                              canvas_provider = NULL,
                              custom_agents_local_only = NULL,
                              builtin_plugin_directories = NULL,
                              args_schema = NULL,
                              decision_context = NULL,
                              tool_search = NULL,
                              in_process = NULL,
                              experimental_mode = NULL,
                              content_exclusion = NULL,
                              reasoning_effort = NULL,
                              on_mcp_auth_request = NULL,
                              bearer_token_provider = NULL,
                              agent_mode = NULL,
                              display_prompt = NULL,
                              auto_tier = NULL,
                              ask_user_variant = NULL,
                              github_token_provider = NULL,
                              sandbox_config = NULL,
                              feature_flags = NULL,
                              allowed_models = NULL,
                              auth_client_id_metadata_url = NULL,
                              name = NULL,
                              agent_directories = NULL,
                              hook_directories = NULL,
                              interactive_user = NULL,
                              excluded_managed_settings = NULL,
                              enforce_managed_model_defaults = NULL,
                              trust_working_directory = NULL,
                              refresh_custom_instructions = NULL,
                              image_generation = NULL,
                              autopilot = NULL) {
      if (!is.null(github_token) && !is.null(github_token_provider)) {
        stop("github_token and github_token_provider are mutually exclusive")
      }
      if (is.null(private$client)) {
        if (isTRUE(self$options$auto_start)) {
          self$start()
        } else {
          stop("Client not connected. Call start() first.")
        }
      }

      # Build tool definitions for the wire
      tool_defs <- list()
      if (!is.null(tools)) {
        for (tool in tools) {
          defn <- list(name = tool$name, description = tool$description)
          if (!is.null(tool$parameters)) defn$parameters <- tool$parameters
          tool_defs <- c(tool_defs, list(defn))
        }
      }

      # Build payload
      payload <- list()
      if (!is.null(model)) payload$model <- model
      if (!is.null(session_id)) payload$sessionId <- session_id
      if (length(tool_defs) > 0) payload$tools <- tool_defs
      if (!is.null(system_message)) payload$systemMessage <- system_message
      if (!is.null(available_tools)) payload$availableTools <- available_tools
      if (!is.null(excluded_tools)) payload$excludedTools <- excluded_tools
      if (!is.null(on_permission_request)) payload$requestPermission <- TRUE
      if (!is.null(on_user_input_request)) payload$requestUserInput <- TRUE
      if (!is.null(on_elicitation_request)) payload$requestElicitation <- TRUE
      if (!is.null(hooks) && length(hooks) > 0) {
        # Supported hook names: see COPILOT_HOOK_NAMES
        # (incl. on_post_tool_use and on_pre_mcp_tool_call).
        unknown_hooks <- setdiff(names(hooks), COPILOT_HOOK_NAMES)
        if (length(unknown_hooks) > 0) {
          warning(paste0("Unknown hook name(s): ", paste(unknown_hooks, collapse = ", ")))
        }
        payload$hooks <- TRUE
      }
      if (!is.null(working_directory)) payload$workingDirectory <- working_directory
      if (!is.null(streaming)) payload$streaming <- streaming
      if (!is.null(provider)) payload$provider <- private$convert_provider(provider)
      if (!is.null(mcp_servers)) payload$mcpServers <- mcp_servers
      if (!is.null(config_dir)) payload$configDir <- config_dir
      if (!is.null(github_token)) payload$gitHubToken <- github_token
      if (!is.null(model_capabilities)) payload$modelCapabilities <- model_capabilities
      if (!is.null(enable_config_discovery)) payload$enableConfigDiscovery <- enable_config_discovery
      if (!is.null(include_sub_agent_streaming_events)) {
        payload$includeSubAgentStreamingEvents <- include_sub_agent_streaming_events
      }
      if (!is.null(commands)) {
        cmds_list <- lapply(commands, function(cmd) {
          c_list <- list(name = cmd$name)
          if (!is.null(cmd$description)) c_list$description <- cmd$description
          c_list
        })
        payload$commands <- cmds_list
      }
      if (!is.null(request_headers)) payload$requestHeaders <- request_headers
      if (!is.null(response_format)) payload$responseFormat <- response_format
      # Wire sessionIdleTimeoutSeconds (idleTimeout) and skills / skillDirectories
      if (!is.null(infinite_sessions)) {
        wire_config <- list()
        if (!is.null(infinite_sessions$enabled)) wire_config$enabled <- infinite_sessions$enabled
        if (!is.null(infinite_sessions$background_compaction_threshold)) {
          wire_config$backgroundCompactionThreshold <- infinite_sessions$background_compaction_threshold
        }
        if (!is.null(infinite_sessions$buffer_exhaustion_threshold)) {
          wire_config$bufferExhaustionThreshold <- infinite_sessions$buffer_exhaustion_threshold
        }
        payload$infiniteSessions <- wire_config
      }

      # --- Upstream-sync session options (parity with @github/copilot-sdk) ---
      if (!is.null(enable_citations)) payload$enableCitations <- enable_citations
      if (!is.null(excluded_builtin_agents)) payload$excludedBuiltinAgents <- excluded_builtin_agents
      if (!is.null(session_limits)) {
        payload$sessionLimits <- if (inherits(session_limits, "SessionLimitsConfig")) {
          session_limits$to_list()
        } else {
          session_limits
        }
      }
      if (!is.null(memory)) {
        payload$memory <- if (inherits(memory, "MemoryConfiguration")) memory$to_list() else memory
      }
      if (!is.null(otlp_protocol)) payload$otlpProtocol <- otlp_protocol
      # CAPI transport options: the runtime only accepts enableWebSocketResponses inside `capi`.
      capi <- build_capi_options(auto_tier, enable_web_socket_responses)
      if (!is.null(capi)) payload$capi <- capi
      if (!is.null(exp_assignments)) payload$expAssignments <- exp_assignments
      # --- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk) ---
      if (!is.null(rewind_enabled)) payload$rewindEnabled <- rewind_enabled
      if (!is.null(additional_directories)) payload$additionalDirectories <- additional_directories
      if (!is.null(disabled_mcp_servers)) payload$disabledMcpServers <- disabled_mcp_servers
      if (!is.null(github_mcp_tool_config)) payload$githubMcpToolConfig <- github_mcp_tool_config
      if (!is.null(canvas_provider)) payload$canvasProvider <- canvas_provider
      if (!is.null(custom_agents_local_only)) payload$customAgentsLocalOnly <- custom_agents_local_only
      if (!is.null(builtin_plugin_directories)) payload$builtinPluginDirectories <- builtin_plugin_directories
      if (!is.null(args_schema)) payload$argsSchema <- args_schema
      if (!is.null(decision_context)) payload$decisionContext <- decision_context
      if (!is.null(tool_search)) payload$toolSearch <- tool_search
      if (!is.null(in_process)) payload$inProcess <- in_process
      if (!is.null(experimental_mode)) payload$experimentalMode <- experimental_mode
      if (!is.null(content_exclusion)) payload$contentExclusion <- content_exclusion
      if (!is.null(reasoning_effort)) payload$reasoningEffort <- reasoning_effort
      # MCP OAuth host token handler: signal the runtime that a handler is registered.
      if (!is.null(on_mcp_auth_request)) payload$mcpAuthHandler <- TRUE
      # BYOK: resolve an initial per-session token via the bearer_token_provider callback.
      if (!is.null(bearer_token_provider)) {
        token <- bearer_token_provider(ProviderTokenArgs$new(session_id = session_id))
        if (!is.null(token)) {
          if (is.null(payload$provider)) payload$provider <- list()
          payload$provider$bearerToken <- token
        }
      }
      # --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) ---
      if (!is.null(name)) payload$name <- name
      if (!is.null(trust_working_directory)) payload$trustWorkingDirectory <- trust_working_directory
      if (!is.null(refresh_custom_instructions)) {
        payload$refreshCustomInstructions <- refresh_custom_instructions
      }
      payload <- add_common_session_options(
        payload,
        ask_user_variant = ask_user_variant,
        sandbox_config = sandbox_config,
        feature_flags = feature_flags,
        allowed_models = allowed_models,
        auth_client_id_metadata_url = auth_client_id_metadata_url,
        agent_directories = agent_directories,
        hook_directories = hook_directories,
        interactive_user = interactive_user,
        excluded_managed_settings = excluded_managed_settings,
        enforce_managed_model_defaults = enforce_managed_model_defaults,
        image_generation = image_generation,
        autopilot = autopilot
      )
      gh_registration_id <- NULL
      if (!is.null(github_token_provider)) {
        gh_registration_id <- private$register_github_token_provider(github_token_provider, session_id)
        payload$gitHubTokenProviderRegistrationId <- gh_registration_id
      }

      response <- tryCatch(
        private$client$request("session.create", payload),
        error = function(e) {
          if (!is.null(gh_registration_id)) private$unregister_github_token_provider(gh_registration_id)
          stop(e)
        }
      )

      sid <- response$sessionId
      workspace_path <- response$workspacePath

      session <- CopilotSession$new(sid, private$client, workspace_path)
      private$commit_github_token_provider(session, gh_registration_id)

      # Register tools
      if (!is.null(tools)) {
        session$register_tools(tools)
      }
      if (!is.null(on_permission_request)) {
        session$register_permission_handler(on_permission_request)
      }
      if (!is.null(on_user_input_request)) {
        session$register_user_input_handler(on_user_input_request)
      }
      if (!is.null(hooks)) {
        session$register_hooks(hooks)
      }
      if (!is.null(on_elicitation_request)) {
        session$register_elicitation_handler(on_elicitation_request)
      }
      if (!is.null(commands)) {
        session$register_commands(commands)
      }
      if (!is.null(agent_mode) || !is.null(display_prompt)) {
        session$set_default_message_options(agent_mode = agent_mode, display_prompt = display_prompt)
      }

      assign(sid, session, envir = private$sessions)
      session
    },

    #' @description Resume an existing session by ID.
    #'
    #' @param session_id Character. The session ID to resume.
    #' @param model Character or NULL. Override model.
    #' @param tools List of Tool R6 objects or NULL.
    #' @param system_message Named list or NULL.
    #' @param streaming Logical or NULL.
    #' @param on_permission_request Function or NULL.
    #' @param on_user_input_request Function or NULL.
    #' @param on_elicitation_request Function or NULL. Elicitation request handler.
    #' @param hooks Named list or NULL.
    #' @param working_directory Character or NULL.
    #' @param provider Named list or NULL.
    #' @param disable_resume Logical or NULL.
    #' @param excluded_tools Character vector or NULL. Tool names to exclude.
    #' @param github_token Character or NULL. GitHub token for this session.
    #' @param model_capabilities Named list or NULL. Model capabilities overrides.
    #' @param enable_config_discovery Logical or NULL. Auto-discover MCP server configs.
    #' @param include_sub_agent_streaming_events Logical or NULL. Include sub-agent streaming events.
    #' @param commands List of CommandDefinition or NULL. Slash commands for this session.
    #' @param enable_web_socket_responses Logical or NULL. Use the WebSocket response transport.
    #' @param auto_tier Character or NULL. Experimental Auto routing preference; sent inside \code{capi}.
    #' @param ask_user_variant Character or NULL. Ask-user tool variant ("legacy" or "elicitation").
    #' @param github_token_provider Function or NULL. GitHub token callback (see create_session).
    #' @param sandbox_config SandboxConfig, named list, or NULL. Sandbox configuration.
    #' @param feature_flags Named list of logicals or NULL. Runtime feature-flag overrides.
    #' @param allowed_models Character vector or NULL. Model allowlist for the session.
    #' @param auth_client_id_metadata_url Character or NULL. OAuth client ID metadata document URL.
    #' @param agent_directories Character vector or NULL. Extra custom-agent directories.
    #' @param hook_directories Character vector or NULL. Extra hook-definition directories.
    #' @param interactive_user Logical or NULL. Whether a human user is present.
    #' @param excluded_managed_settings Character vector or NULL. Managed settings to ignore.
    #' @param enforce_managed_model_defaults Logical or NULL. Enforce managed model defaults.
    #' @param allow_transcript_recovery Logical or NULL. Allow recovering from a damaged transcript.
    #' @param image_generation ImageGenerationConfig, named list, or NULL. Image generation opt-in.
    #' @param autopilot AutopilotOptions, named list, or NULL. Autopilot continuation options.
    #'
    #' @return A CopilotSession R6 object.
    resume_session = function(session_id, model = NULL, tools = NULL,
                              system_message = NULL, streaming = NULL,
                              on_permission_request = NULL,
                              on_user_input_request = NULL,
                              on_elicitation_request = NULL,
                              hooks = NULL, working_directory = NULL,
                              provider = NULL, disable_resume = NULL,
                              excluded_tools = NULL, github_token = NULL,
                              model_capabilities = NULL,
                              enable_config_discovery = NULL,
                              include_sub_agent_streaming_events = NULL,
                              commands = NULL,
                              enable_web_socket_responses = NULL,
                              auto_tier = NULL,
                              ask_user_variant = NULL,
                              github_token_provider = NULL,
                              sandbox_config = NULL,
                              feature_flags = NULL,
                              allowed_models = NULL,
                              auth_client_id_metadata_url = NULL,
                              agent_directories = NULL,
                              hook_directories = NULL,
                              interactive_user = NULL,
                              excluded_managed_settings = NULL,
                              enforce_managed_model_defaults = NULL,
                              allow_transcript_recovery = NULL,
                              image_generation = NULL,
                              autopilot = NULL) {
      if (!is.null(github_token) && !is.null(github_token_provider)) {
        stop("github_token and github_token_provider are mutually exclusive")
      }
      if (is.null(private$client)) {
        if (isTRUE(self$options$auto_start)) {
          self$start()
        } else {
          stop("Client not connected. Call start() first.")
        }
      }

      tool_defs <- list()
      if (!is.null(tools)) {
        for (tool in tools) {
          defn <- list(name = tool$name, description = tool$description)
          if (!is.null(tool$parameters)) defn$parameters <- tool$parameters
          tool_defs <- c(tool_defs, list(defn))
        }
      }

      payload <- list(sessionId = session_id)
      if (!is.null(model)) payload$model <- model
      if (length(tool_defs) > 0) payload$tools <- tool_defs
      if (!is.null(system_message)) payload$systemMessage <- system_message
      if (!is.null(streaming)) payload$streaming <- streaming
      if (!is.null(on_permission_request)) payload$requestPermission <- TRUE
      if (!is.null(on_user_input_request)) payload$requestUserInput <- TRUE
      if (!is.null(on_elicitation_request)) payload$requestElicitation <- TRUE
      if (!is.null(hooks) && length(hooks) > 0) payload$hooks <- TRUE
      if (!is.null(working_directory)) payload$workingDirectory <- working_directory
      if (!is.null(provider)) payload$provider <- private$convert_provider(provider)
      if (isTRUE(disable_resume)) payload$disableResume <- TRUE
      if (!is.null(excluded_tools)) payload$excludedTools <- excluded_tools
      if (!is.null(github_token)) payload$gitHubToken <- github_token
      if (!is.null(model_capabilities)) payload$modelCapabilities <- model_capabilities
      if (!is.null(enable_config_discovery)) payload$enableConfigDiscovery <- enable_config_discovery
      if (!is.null(include_sub_agent_streaming_events)) {
        payload$includeSubAgentStreamingEvents <- include_sub_agent_streaming_events
      }
      if (!is.null(commands)) {
        cmds_list <- lapply(commands, function(cmd) {
          c_list <- list(name = cmd$name)
          if (!is.null(cmd$description)) c_list$description <- cmd$description
          c_list
        })
        payload$commands <- cmds_list
      }
      # --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) ---
      capi <- build_capi_options(auto_tier, enable_web_socket_responses)
      if (!is.null(capi)) payload$capi <- capi
      if (!is.null(allow_transcript_recovery)) {
        payload$allowTranscriptRecovery <- allow_transcript_recovery
      }
      payload <- add_common_session_options(
        payload,
        ask_user_variant = ask_user_variant,
        sandbox_config = sandbox_config,
        feature_flags = feature_flags,
        allowed_models = allowed_models,
        auth_client_id_metadata_url = auth_client_id_metadata_url,
        agent_directories = agent_directories,
        hook_directories = hook_directories,
        interactive_user = interactive_user,
        excluded_managed_settings = excluded_managed_settings,
        enforce_managed_model_defaults = enforce_managed_model_defaults,
        image_generation = image_generation,
        autopilot = autopilot
      )
      gh_registration_id <- NULL
      if (!is.null(github_token_provider)) {
        gh_registration_id <- private$register_github_token_provider(github_token_provider, session_id)
        payload$gitHubTokenProviderRegistrationId <- gh_registration_id
      }

      response <- tryCatch(
        private$client$request("session.resume", payload),
        error = function(e) {
          if (!is.null(gh_registration_id)) private$unregister_github_token_provider(gh_registration_id)
          stop(e)
        }
      )

      sid <- response$sessionId
      workspace_path <- response$workspacePath

      session <- CopilotSession$new(sid, private$client, workspace_path)
      private$commit_github_token_provider(session, gh_registration_id)
      if (!is.null(tools)) session$register_tools(tools)
      if (!is.null(on_permission_request)) session$register_permission_handler(on_permission_request)
      if (!is.null(on_user_input_request)) session$register_user_input_handler(on_user_input_request)
      if (!is.null(hooks)) session$register_hooks(hooks)
      if (!is.null(on_elicitation_request)) session$register_elicitation_handler(on_elicitation_request)
      if (!is.null(commands)) session$register_commands(commands)

      assign(sid, session, envir = private$sessions)
      session
    },

    #' @description Send a ping to verify connectivity.
    #' @param message Character or NULL. Optional message.
    #' @return A PingResponse R6 object.
    ping = function(message = NULL) {
      if (is.null(private$client)) stop("Client not connected")
      result <- private$client$request("ping", list(message = message))
      ping_response_from_list(result)
    },

    #' @description List available models.
    #' @return A list of ModelInfo R6 objects.
    list_models = function() {
      if (is.null(private$client)) stop("Client not connected")

      # Return cached if available
      if (!is.null(private$models_cache)) return(private$models_cache)

      response <- private$client$request("models.list", list())
      models_data <- response$models %||% list()
      models <- lapply(models_data, model_info_from_list)
      private$models_cache <- models
      models
    },

    #' @description List all sessions known to the server.
    #' @return A list of SessionMetadata R6 objects.
    list_sessions = function() {
      if (is.null(private$client)) stop("Client not connected")
      response <- private$client$request("session.list", list())
      sessions_data <- response$sessions %||% list()
      lapply(sessions_data, session_metadata_from_list)
    },

    #' @description Delete a session permanently.
    #' @param session_id Character. Session ID to delete.
    delete_session = function(session_id) {
      if (is.null(private$client)) stop("Client not connected")
      response <- private$client$request("session.delete", list(sessionId = session_id))
      if (!isTRUE(response$success)) {
        err <- response$error %||% "Unknown error"
        stop(paste0("Failed to delete session ", session_id, ": ", err))
      }
      # Remove from local map
      if (exists(session_id, envir = private$sessions)) {
        rm(list = session_id, envir = private$sessions)
      }
      invisible(NULL)
    },

    #' @description Get server status.
    #' @return The status response from the server.
    get_status = function() {
      if (is.null(private$client)) stop("Client not connected")
      private$client$request("status.get", list())
    },

    #' @description Get authentication status.
    #' @return The auth status response from the server.
    get_auth_status = function() {
      if (is.null(private$client)) stop("Client not connected")
      private$client$request("auth.getStatus", list())
    },

    #' @description Clear the runtime's cached managed (enterprise) settings so they are
    #'   re-read on next use.
    #' @return The response from the server.
    clear_managed_settings_cache = function() {
      if (is.null(private$client)) stop("Client not connected")
      private$client$request("managedSettings.clearCache", empty_json_object())
    },

    #' @description Get the last session ID.
    #' @return Character or NULL if no previous session.
    get_last_session_id = function() {
      if (is.null(private$client)) stop("Client not connected")
      response <- private$client$request("session.getLastId", list())
      response$sessionId
    },

    #' @description Get metadata for a session.
    #' @param session_id Character. The session ID to query.
    #' @return The metadata response from the server.
    get_session_metadata = function(session_id) {
      if (is.null(private$client)) stop("Client not connected")
      private$client$request("session.getMetadata", list(sessionId = session_id))
    },

    #' @description Get the foreground session ID.
    #' @return Character or NULL if no foreground session.
    get_foreground_session_id = function() {
      if (is.null(private$client)) stop("Client not connected")
      response <- private$client$request("session.getForeground", list())
      response$sessionId
    },

    #' @description Set the foreground session ID.
    #' @param session_id Character. Session ID to set as foreground.
    set_foreground_session_id = function(session_id) {
      if (is.null(private$client)) stop("Client not connected")
      response <- private$client$request("session.setForeground", list(sessionId = session_id))
      if (!isTRUE(response$success)) {
        err <- response$error %||% "Unknown error"
        stop(paste0("Failed to set foreground session: ", err))
      }
      invisible(NULL)
    },

    #' @description Subscribe to session lifecycle events.
    #' @param handler Function(SessionLifecycleEvent).
    #' @return A function that when called, unsubscribes the handler.
    on = function(handler) {
      private$lifecycle_handlers <- c(private$lifecycle_handlers, list(handler))
      local_handler <- handler
      function() {
        private$lifecycle_handlers <- Filter(
          function(h) !identical(h, local_handler),
          private$lifecycle_handlers
        )
      }
    },

    #' @description Poll for and process incoming messages.
    #'
    #' Call this periodically (e.g. in a loop or via later/shiny reactive)
    #' to process incoming notifications and requests from the CLI server.
    #'
    #' @param timeout_ms Integer. Poll timeout in milliseconds.
    #' @return Logical. TRUE if a message was processed.
    poll = function(timeout_ms = 100L) {
      if (is.null(private$client)) return(FALSE)
      private$client$poll(timeout_ms)
    }
  ),

  private = list(
    process = NULL,
    client = NULL,
    state = "disconnected",
    sessions = NULL,
    models_cache = NULL,
    lifecycle_handlers = NULL,
    session_fs_config = NULL,
    github_token_providers = NULL,

    start_cli_server = function() {
      cli_path <- self$options$cli_path

      args <- c("--headless", "--no-auto-update",
                "--log-level", self$options$log_level,
                "--stdio")

      # Auth flags
      if (!is.null(self$options$github_token)) {
        args <- c(args, "--auth-token-env", "COPILOT_SDK_AUTH_TOKEN")
      }
      if (!isTRUE(self$options$use_logged_in_user)) {
        args <- c(args, "--no-auto-login")
      }

      # Build environment
      env <- Sys.getenv()
      if (!is.null(self$options$env)) {
        for (nm in names(self$options$env)) {
          env[[nm]] <- self$options$env[[nm]]
        }
      }
      if (!is.null(self$options$github_token)) {
        env[["COPILOT_SDK_AUTH_TOKEN"]] <- self$options$github_token
      }

      # Convert env to character vector "KEY=VALUE"
      env_vec <- paste0(names(env), "=", unname(env))

      # Determine if cli_path is a .js file
      if (grepl("\\.js$", cli_path)) {
        command <- "node"
        args <- c(cli_path, args)
      } else {
        command <- cli_path
      }

      private$process <- processx::process$new(
        command = command,
        args = args,
        stdin = "|",
        stdout = "|",
        stderr = "|",
        env = env_vec,
        wd = self$options$cwd,
        cleanup = TRUE,
        cleanup_tree = TRUE
      )

      if (!private$process$is_alive()) {
        stop("Failed to start CLI process")
      }
    },

    connect_to_server = function() {
      private$client <- JsonRpcClient$new(private$process)

      # Set up notification handler
      private$client$set_notification_handler(function(method, params) {
        if (identical(method, "session.event")) {
          session_id <- params$sessionId
          event_dict <- params$event
          event <- session_event_from_list(event_dict)
          if (exists(session_id, envir = private$sessions)) {
            session <- get(session_id, envir = private$sessions)
            session$dispatch_event(event)
          }
        } else if (identical(method, "session.lifecycle")) {
          lifecycle_event <- session_lifecycle_event_from_list(params)
          private$dispatch_lifecycle_event(lifecycle_event)
        }
      })

      # Set up request handlers for tool.call, permission.request, etc.
      private$client$set_request_handler("tool.call", function(params) {
        private$handle_tool_call(params)
      })

      private$client$set_request_handler("permission.request", function(params) {
        private$handle_permission_request(params)
      })

      private$client$set_request_handler("userInput.request", function(params) {
        private$handle_user_input_request(params)
      })

      private$client$set_request_handler("hooks.invoke", function(params) {
        private$handle_hooks_invoke(params)
      })

      private$client$set_request_handler("gitHubToken.getToken", function(params) {
        private$handle_github_token_request(params)
      })

      private$client$start()
    },

    # Send the `connect` handshake carrying the integrating application's identity.
    # Skipped when no client_info field is set; older CLIs without `connect` are tolerated.
    send_connect_handshake = function() {
      wire <- client_info_to_wire(self$options$client_info)
      if (is.null(wire)) return(invisible(NULL))
      tryCatch(
        private$client$request("connect", list(clientInfo = wire)),
        json_rpc_error = function(e) {
          method_not_found <- identical(as.integer(e$code), -32601L) ||
            grepl("Unhandled method connect", e$rpc_message %||% "", fixed = TRUE)
          if (!method_not_found) stop(e)
        }
      )
      invisible(NULL)
    },

    verify_protocol_version = function() {
      expected <- get_sdk_protocol_version()
      ping_result <- self$ping()
      server_version <- ping_result$protocol_version

      if (is.null(server_version)) {
        stop(paste0(
          "SDK protocol version mismatch: SDK expects version ", expected,
          ", but server does not report a protocol version. ",
          "Please update your server to ensure compatibility."
        ))
      }

      if (server_version != expected) {
        stop(paste0(
          "SDK protocol version mismatch: SDK expects version ", expected,
          ", but server reports version ", server_version, ". ",
          "Please update your SDK or server to ensure compatibility."
        ))
      }
    },

    convert_provider = function(provider) {
      wire <- list(type = provider$type)
      if (!is.null(provider$base_url)) wire$baseUrl <- provider$base_url
      if (!is.null(provider$api_key)) wire$apiKey <- provider$api_key
      if (!is.null(provider$wire_api)) wire$wireApi <- provider$wire_api
      if (!is.null(provider$bearer_token)) wire$bearerToken <- provider$bearer_token
      if (!is.null(provider$model_provider)) wire$modelProvider <- provider$model_provider
      if (!is.null(provider$api_key_command)) wire$apiKeyCommand <- provider$api_key_command
      if (!is.null(provider$azure)) {
        wire_azure <- list()
        if (!is.null(provider$azure$api_version)) wire_azure$apiVersion <- provider$azure$api_version
        wire$azure <- wire_azure
      }
      wire
    },

    dispatch_lifecycle_event = function(event) {
      for (handler in private$lifecycle_handlers) {
        tryCatch(handler(event), error = function(e) NULL)
      }
    },

    handle_tool_call = function(params) {
      session_id <- params$sessionId
      tool_call_id <- params$toolCallId
      tool_name <- params$toolName

      if (is.null(session_id) || is.null(tool_call_id) || is.null(tool_name)) {
        stop("Invalid tool call payload")
      }

      if (!exists(session_id, envir = private$sessions)) {
        stop(paste0("Unknown session ", session_id))
      }

      session <- get(session_id, envir = private$sessions)
      handler <- session$get_tool_handler(tool_name)

      if (is.null(handler)) {
        return(list(result = list(
          textResultForLlm = paste0("Tool '", tool_name, "' is not supported."),
          resultType = "failure",
          error = paste0("tool '", tool_name, "' not supported"),
          toolTelemetry = list()
        )))
      }

      invocation <- ToolInvocation$new(
        session_id = session_id,
        tool_call_id = tool_call_id,
        tool_name = tool_name,
        arguments = params$arguments
      )

      result <- tryCatch(
        {
          res <- handler(invocation)
          if (is.null(res)) {
            list(
              textResultForLlm = "Tool returned no result.",
              resultType = "failure",
              error = "tool returned no result",
              toolTelemetry = list()
            )
          } else if (inherits(res, "ToolResultObject")) {
            res$to_list()
          } else if (is.list(res) && !is.null(res$textResultForLlm)) {
            res
          } else if (is.character(res)) {
            list(textResultForLlm = res, resultType = "success")
          } else {
            list(
              textResultForLlm = jsonlite::toJSON(res, auto_unbox = TRUE),
              resultType = "success"
            )
          }
        },
        error = function(e) {
          list(
            textResultForLlm = "Invoking this tool produced an error. Detailed information is not available.",
            resultType = "failure",
            error = e$message,
            toolTelemetry = list()
          )
        }
      )

      list(result = result)
    },

    handle_permission_request = function(params) {
      session_id <- params$sessionId
      permission_request <- params$permissionRequest

      if (is.null(session_id) || is.null(permission_request)) {
        stop("Invalid permission request payload")
      }

      if (!exists(session_id, envir = private$sessions)) {
        stop(paste0("Unknown session ", session_id))
      }

      session <- get(session_id, envir = private$sessions)

      tryCatch(
        {
          result <- session$handle_permission_request(permission_request)
          list(result = result)
        },
        error = function(e) {
          list(result = list(kind = "denied-no-approval-rule-and-could-not-request-from-user"))
        }
      )
    },

    handle_user_input_request = function(params) {
      session_id <- params$sessionId
      question <- params$question

      if (is.null(session_id) || is.null(question)) {
        stop("Invalid user input request payload")
      }

      if (!exists(session_id, envir = private$sessions)) {
        stop(paste0("Unknown session ", session_id))
      }

      session <- get(session_id, envir = private$sessions)
      result <- session$handle_user_input_request(params)
      list(answer = result$answer, wasFreeform = result$was_freeform)
    },

    handle_hooks_invoke = function(params) {
      session_id <- params$sessionId
      hook_type <- params$hookType
      input_data <- params$input

      if (is.null(session_id) || is.null(hook_type)) {
        stop("Invalid hooks invoke payload")
      }

      if (!exists(session_id, envir = private$sessions)) {
        stop(paste0("Unknown session ", session_id))
      }

      session <- get(session_id, envir = private$sessions)
      output <- session$handle_hooks_invoke(hook_type, input_data)
      list(output = output)
    },

    # --- GitHub token provider registry (gitHubToken.getToken) ---

    register_github_token_provider = function(provider, session_id = NULL) {
      registration_id <- uuid::UUIDgenerate()
      assign(
        registration_id,
        list(provider = provider, session_id = session_id, committed = FALSE),
        envir = private$github_token_providers
      )
      registration_id
    },

    unregister_github_token_provider = function(registration_id) {
      if (!is.null(registration_id) &&
          exists(registration_id, envir = private$github_token_providers, inherits = FALSE)) {
        rm(list = registration_id, envir = private$github_token_providers)
      }
      invisible(NULL)
    },

    # Bind a registration to its session and drop it when the session is destroyed.
    # A later resume of the same session replaces the previously committed registration.
    commit_github_token_provider = function(session, registration_id) {
      if (is.null(registration_id)) return(invisible(NULL))
      sid <- session$session_id
      for (other_id in ls(private$github_token_providers)) {
        if (identical(other_id, registration_id)) next
        other <- get(other_id, envir = private$github_token_providers, inherits = FALSE)
        if (isTRUE(other$committed) && identical(other$session_id, sid)) {
          rm(list = other_id, envir = private$github_token_providers)
        }
      }
      if (exists(registration_id, envir = private$github_token_providers, inherits = FALSE)) {
        registration <- get(registration_id, envir = private$github_token_providers, inherits = FALSE)
        registration$session_id <- sid
        registration$committed <- TRUE
        assign(registration_id, registration, envir = private$github_token_providers)
      }
      session$register_disconnect_callback(function() {
        private$unregister_github_token_provider(registration_id)
      })
      invisible(NULL)
    },

    handle_github_token_request = function(params) {
      registration_id <- params$registrationId
      registration <- NULL
      if (is.character(registration_id) && length(registration_id) == 1 &&
          exists(registration_id, envir = private$github_token_providers, inherits = FALSE)) {
        registration <- get(registration_id, envir = private$github_token_providers, inherits = FALSE)
      }
      if (is.null(registration)) {
        stop(paste0(
          "No GitHub token provider registered for registration ID \"",
          paste(registration_id, collapse = ""), "\""
        ))
      }
      args <- GitHubTokenProviderArgs$new(
        host = params$host,
        session_id = params$sessionId %||% registration$session_id,
        reason = params$reason
      )
      github_token_result_to_wire(registration$provider(args))
    }
  )
)


# ---------------------------------------------------------------------------
# Upstream-sync feature types & constants (parity with @github/copilot-sdk)
# ---------------------------------------------------------------------------

#' SessionLimitsConfig
#'
#' Per-session AI-credit budget. Set \code{max_ai_credits} to cap spend.
#'
#' @field max_ai_credits Numeric or NULL. Maximum AI credits for the session.
#' @keywords internal
SessionLimitsConfig <- R6::R6Class(
  "SessionLimitsConfig",
  public = list(
    max_ai_credits = NULL,

    #' @description Create a new SessionLimitsConfig.
    #' @param max_ai_credits Numeric or NULL.
    initialize = function(max_ai_credits = NULL) {
      self$max_ai_credits <- max_ai_credits
    },

    #' @description Convert to a wire list (camelCase keys).
    to_list = function() {
      result <- list()
      if (!is.null(self$max_ai_credits)) result$maxAiCredits <- self$max_ai_credits
      result
    }
  )
)

#' MemoryConfiguration
#'
#' Opt-in persistent session memory.
#'
#' @field enabled Logical or NULL. Whether persistent memory is enabled.
#' @keywords internal
MemoryConfiguration <- R6::R6Class(
  "MemoryConfiguration",
  public = list(
    enabled = NULL,

    #' @description Create a new MemoryConfiguration.
    #' @param enabled Logical or NULL.
    initialize = function(enabled = NULL) {
      self$enabled <- enabled
    },

    #' @description Convert to a wire list.
    to_list = function() {
      result <- list()
      if (!is.null(self$enabled)) result$enabled <- self$enabled
      result
    }
  )
)

#' ProviderTokenArgs
#'
#' Arguments passed to a BYOK \code{bearer_token_provider} callback.
#'
#' @field session_id Character or NULL. The session requesting a token.
#' @keywords internal
ProviderTokenArgs <- R6::R6Class(
  "ProviderTokenArgs",
  public = list(
    session_id = NULL,

    #' @description Create new ProviderTokenArgs.
    #' @param session_id Character or NULL.
    initialize = function(session_id = NULL) {
      self$session_id <- session_id
    }
  )
)

#' CopilotRequestHandler
#'
#' Intercept outbound LLM inference HTTP/WebSocket requests. Subclass and override
#' \code{send_request} to mutate, replace, or forward the request. BYOK providers may
#' also supply a \code{bearer_token_provider}.
#'
#' @keywords internal
CopilotRequestHandler <- R6::R6Class(
  "CopilotRequestHandler",
  public = list(
    #' @description Handle an outbound request.
    #' @param request The request object.
    #' @param context Named list of contextual data.
    #' @return The (possibly modified) request.
    send_request = function(request, context = NULL) {
      request
    }
  )
)

#' ToolDefer
#'
#' Tool "defer" loading policy: eager pre-load (\code{"never"}) or lazy via search (\code{"auto"}).
#'
#' @keywords internal
ToolDefer <- list(
  AUTO  = "auto",
  NEVER = "never"
)

#' SystemMessageSection
#'
#' System-message section identifiers. The \code{"preamble"} section targets only the
#' identity preamble; the \code{"preserve"} action protects an individually-addressable
#' section from a group-level remove.
#'
#' @keywords internal
SystemMessageSection <- list(
  PREAMBLE          = "preamble",
  IDENTITY          = "identity",
  TOOL_INSTRUCTIONS = "tool_instructions",
  PRESERVE          = "preserve"
)

#' GitHubAttachment
#'
#' GitHub-anchored attachment variants.
#'
#' @keywords internal
GitHubAttachment <- list(
  GITHUB_COMMIT          = "GitHubCommit",
  GITHUB_RELEASE         = "GitHubRelease",
  GITHUB_ACTIONS_JOB     = "GitHubActionsJob",
  GITHUB_REPOSITORY      = "GitHubRepository",
  GITHUB_FILE_DIFF       = "GitHubFileDiff",
  GITHUB_TREE_COMPARISON = "GitHubTreeComparison",
  GITHUB_URL             = "GitHubUrl",
  GITHUB_FILE            = "GitHubFile",
  GITHUB_SNIPPET         = "GitHubSnippet"
)

#' COPILOT_HOOK_NAMES
#'
#' Supported hook names, including \code{on_post_tool_use} and \code{on_pre_mcp_tool_call}
#' (and the \code{on_mcp_auth_request} MCP OAuth host token handler), plus the
#' \code{on_subagent_start} / \code{on_subagent_stop} sub-agent lifecycle hooks.
#'
#' @keywords internal
COPILOT_HOOK_NAMES <- c(
  "on_pre_tool_use",
  "on_post_tool_use",
  "on_user_prompt_submitted",
  "on_user_prompt_transformed",
  "on_session_start",
  "on_session_end",
  "on_error_occurred",
  "on_pre_mcp_tool_call",
  "on_mcp_auth_request",
  "on_subagent_start",
  "on_subagent_stop"
)


# ---------------------------------------------------------------------------
# v2.6.0 upstream-sync types & helpers (parity with @github/copilot-sdk)
# ---------------------------------------------------------------------------

#' ClientInfo
#'
#' Identity of the application integrating the SDK. Sent on the \code{connect}
#' handshake (as editorName / editorVersion / extensionName / extensionVersion)
#' when at least one field is non-empty.
#'
#' @field application_name Character or NULL. Integrating application name.
#' @field application_version Character or NULL. Integrating application version.
#' @field integration_name Character or NULL. Integration (extension) name.
#' @field integration_version Character or NULL. Integration (extension) version.
#' @export
ClientInfo <- R6::R6Class(
  "ClientInfo",
  public = list(
    application_name = NULL,
    application_version = NULL,
    integration_name = NULL,
    integration_version = NULL,

    #' @description Create a new ClientInfo.
    #' @param application_name Character or NULL.
    #' @param application_version Character or NULL.
    #' @param integration_name Character or NULL.
    #' @param integration_version Character or NULL.
    initialize = function(application_name = NULL, application_version = NULL,
                          integration_name = NULL, integration_version = NULL) {
      self$application_name <- application_name
      self$application_version <- application_version
      self$integration_name <- integration_name
      self$integration_version <- integration_version
    },

    #' @description Convert to the \code{connect} wire shape (empty fields dropped).
    to_list = function() {
      client_info_to_wire(self) %||% list()
    }
  )
)

#' SandboxConfig
#'
#' Sandbox configuration for a session (wire key \code{sandboxConfig}).
#'
#' @field enabled Logical. Whether sandboxing is enabled.
#' @field user_policy Named list or NULL. User policy fragment merged into the base policy.
#' @field add_current_working_directory Logical or NULL. Auto-add the working directory.
#' @field sandbox_mcp_servers Logical or NULL. Confine MCP servers (FALSE opts out).
#' @field sandbox_lsp_servers Logical or NULL. Confine language servers (FALSE opts out).
#' @field sandbox_extensions Logical or NULL. Confine extension processes (FALSE opts out).
#' @field allow_bypass Logical or NULL. Opt in to letting the agent request that an individual
#'   command run outside the sandbox, approved through the normal permission flow.
#' @export
SandboxConfig <- R6::R6Class(
  "SandboxConfig",
  public = list(
    enabled = TRUE,
    user_policy = NULL,
    add_current_working_directory = NULL,
    sandbox_mcp_servers = NULL,
    sandbox_lsp_servers = NULL,
    sandbox_extensions = NULL,
    allow_bypass = NULL,

    #' @description Create a new SandboxConfig.
    #' @param enabled Logical. Default TRUE.
    #' @param user_policy Named list or NULL.
    #' @param add_current_working_directory Logical or NULL.
    #' @param sandbox_mcp_servers Logical or NULL.
    #' @param sandbox_lsp_servers Logical or NULL.
    #' @param sandbox_extensions Logical or NULL.
    #' @param allow_bypass Logical or NULL.
    initialize = function(enabled = TRUE, user_policy = NULL,
                          add_current_working_directory = NULL,
                          sandbox_mcp_servers = NULL, sandbox_lsp_servers = NULL,
                          sandbox_extensions = NULL, allow_bypass = NULL) {
      self$enabled <- enabled
      self$user_policy <- user_policy
      self$add_current_working_directory <- add_current_working_directory
      self$sandbox_mcp_servers <- sandbox_mcp_servers
      self$sandbox_lsp_servers <- sandbox_lsp_servers
      self$sandbox_extensions <- sandbox_extensions
      self$allow_bypass <- allow_bypass
    },

    #' @description Convert to a wire list (camelCase keys).
    to_list = function() {
      result <- list(enabled = isTRUE(self$enabled))
      if (!is.null(self$user_policy)) result$userPolicy <- self$user_policy
      if (!is.null(self$add_current_working_directory)) {
        result$addCurrentWorkingDirectory <- self$add_current_working_directory
      }
      if (!is.null(self$sandbox_mcp_servers)) result$sandboxMcpServers <- self$sandbox_mcp_servers
      if (!is.null(self$sandbox_lsp_servers)) result$sandboxLspServers <- self$sandbox_lsp_servers
      if (!is.null(self$sandbox_extensions)) result$sandboxExtensions <- self$sandbox_extensions
      if (!is.null(self$allow_bypass)) result$allowBypass <- self$allow_bypass
      result
    }
  )
)

#' ImageGenerationConfig
#'
#' Non-persisted image generation opt-in (wire key \code{imageGeneration}).
#'
#' @field enabled Logical or NULL. Opt in to image generation.
#' @field watermark Logical or NULL. Request a visible "Made with AI" label.
#' @field save_to_disk Logical or NULL. Save generated images under the session state directory.
#' @export
ImageGenerationConfig <- R6::R6Class(
  "ImageGenerationConfig",
  public = list(
    enabled = NULL,
    watermark = NULL,
    save_to_disk = NULL,

    #' @description Create a new ImageGenerationConfig.
    #' @param enabled Logical or NULL.
    #' @param watermark Logical or NULL.
    #' @param save_to_disk Logical or NULL.
    initialize = function(enabled = NULL, watermark = NULL, save_to_disk = NULL) {
      self$enabled <- enabled
      self$watermark <- watermark
      self$save_to_disk <- save_to_disk
    },

    #' @description Convert to a wire list (camelCase keys).
    to_list = function() {
      result <- empty_json_object()
      if (!is.null(self$enabled)) result$enabled <- self$enabled
      if (!is.null(self$watermark)) result$watermark <- self$watermark
      if (!is.null(self$save_to_disk)) result$saveToDisk <- self$save_to_disk
      result
    }
  )
)

#' AutopilotOptions
#'
#' Autopilot continuation options (wire key \code{autopilot}).
#'
#' @field continuation_enabled Logical or NULL. Whether autopilot may continue automatically.
#' @field max_continues Integer or NULL. Maximum automatic continuations.
#' @export
AutopilotOptions <- R6::R6Class(
  "AutopilotOptions",
  public = list(
    continuation_enabled = NULL,
    max_continues = NULL,

    #' @description Create new AutopilotOptions.
    #' @param continuation_enabled Logical or NULL.
    #' @param max_continues Integer or NULL.
    initialize = function(continuation_enabled = NULL, max_continues = NULL) {
      self$continuation_enabled <- continuation_enabled
      self$max_continues <- max_continues
    },

    #' @description Convert to a wire list (camelCase keys).
    to_list = function() {
      result <- empty_json_object()
      if (!is.null(self$continuation_enabled)) result$continuationEnabled <- self$continuation_enabled
      if (!is.null(self$max_continues)) result$maxContinues <- as.integer(self$max_continues)
      result
    }
  )
)

#' GitHubTokenProviderArgs
#'
#' Arguments passed to a \code{github_token_provider} callback when the runtime
#' sends a \code{gitHubToken.getToken} request.
#'
#' @field host Character. GitHub host the token is for.
#' @field session_id Character or NULL. Session receiving the token.
#' @field reason Character. "initial" or "refresh".
#' @export
GitHubTokenProviderArgs <- R6::R6Class(
  "GitHubTokenProviderArgs",
  public = list(
    host = NULL,
    session_id = NULL,
    reason = NULL,

    #' @description Create new GitHubTokenProviderArgs.
    #' @param host Character.
    #' @param session_id Character or NULL.
    #' @param reason Character.
    initialize = function(host = NULL, session_id = NULL, reason = NULL) {
      self$host <- host
      self$session_id <- session_id
      self$reason <- reason
    }
  )
)

#' MessageSource
#'
#' Message source values for the \code{source} option of \code{session$send()}:
#' \code{MessageSource$USER}, \code{MessageSource$SYSTEM}, or
#' \code{MessageSource$agent(agent_id)} (which builds \code{"agent-<id>"}).
#'
#' @export
MessageSource <- list(
  USER = "user",
  SYSTEM = "system",
  agent = function(agent_id) paste0("agent-", agent_id)
)

#' Empty JSON object (encodes as \code{{}} rather than \code{[]}).
#' @keywords internal
empty_json_object <- function() {
  structure(list(), names = character(0))
}

#' Convert an R6 config object with \code{to_list()} to its wire list; pass lists through.
#' @keywords internal
config_to_wire <- function(x) {
  if (inherits(x, "R6") && is.function(x$to_list)) x$to_list() else x
}

#' Map client info (ClientInfo or named list) to the \code{connect} wire shape.
#' Empty fields are dropped; returns NULL when no field is set.
#' @keywords internal
client_info_to_wire <- function(info) {
  if (is.null(info)) return(NULL)
  if (is.atomic(info)) info <- as.list(info)
  pick <- function(snake, camel) {
    value <- info[[snake]]
    if (is.null(value)) value <- info[[camel]]
    if (is.null(value) || length(value) == 0) return(NULL)
    value <- as.character(value[[1]])
    if (is.na(value) || !nzchar(value)) return(NULL)
    value
  }
  wire <- list()
  application_name <- pick("application_name", "applicationName")
  application_version <- pick("application_version", "applicationVersion")
  integration_name <- pick("integration_name", "integrationName")
  integration_version <- pick("integration_version", "integrationVersion")
  if (!is.null(application_name)) wire$editorName <- application_name
  if (!is.null(application_version)) wire$editorVersion <- application_version
  if (!is.null(integration_name)) wire$extensionName <- integration_name
  if (!is.null(integration_version)) wire$extensionVersion <- integration_version
  if (length(wire) == 0) NULL else wire
}

#' Build the \code{capi} create/resume object (only set fields); NULL when empty.
#' @keywords internal
build_capi_options <- function(auto_tier = NULL, enable_web_socket_responses = NULL) {
  capi <- list()
  if (!is.null(auto_tier)) capi$autoTier <- auto_tier
  if (!is.null(enable_web_socket_responses)) {
    capi$enableWebSocketResponses <- enable_web_socket_responses
  }
  if (length(capi) == 0) NULL else capi
}

#' Convert feature flags to a JSON object (an empty input still encodes as \code{{}}).
#' @keywords internal
feature_flags_to_wire <- function(flags) {
  flags <- as.list(flags)
  if (length(flags) == 0) return(empty_json_object())
  flags
}

#' Apply the session options shared by session.create and session.resume.
#' @keywords internal
add_common_session_options <- function(payload, ask_user_variant = NULL, sandbox_config = NULL,
                                       feature_flags = NULL, allowed_models = NULL,
                                       auth_client_id_metadata_url = NULL,
                                       agent_directories = NULL, hook_directories = NULL,
                                       interactive_user = NULL,
                                       excluded_managed_settings = NULL,
                                       enforce_managed_model_defaults = NULL,
                                       image_generation = NULL, autopilot = NULL) {
  if (!is.null(ask_user_variant)) payload$askUserVariant <- ask_user_variant
  if (!is.null(sandbox_config)) payload$sandboxConfig <- config_to_wire(sandbox_config)
  if (!is.null(feature_flags)) payload$featureFlags <- feature_flags_to_wire(feature_flags)
  if (!is.null(allowed_models)) payload$allowedModels <- as.list(allowed_models)
  if (!is.null(auth_client_id_metadata_url)) {
    payload$authClientIdMetadataUrl <- auth_client_id_metadata_url
  }
  if (!is.null(agent_directories)) payload$agentDirectories <- as.list(agent_directories)
  if (!is.null(hook_directories)) payload$hookDirectories <- as.list(hook_directories)
  if (!is.null(interactive_user)) payload$interactiveUser <- interactive_user
  if (!is.null(excluded_managed_settings)) {
    payload$excludedManagedSettings <- as.list(excluded_managed_settings)
  }
  if (!is.null(enforce_managed_model_defaults)) {
    payload$enforceManagedModelDefaults <- enforce_managed_model_defaults
  }
  if (!is.null(image_generation)) payload$imageGeneration <- config_to_wire(image_generation)
  if (!is.null(autopilot)) payload$autopilot <- config_to_wire(autopilot)
  payload
}

#' Normalize a \code{github_token_provider} result to the wire shape.
#' Accepts camelCase or snake_case names; \code{kind} defaults to "token".
#' @keywords internal
github_token_result_to_wire <- function(result) {
  if (is.null(result)) stop("GitHub token provider returned no result")
  result <- config_to_wire(result)
  if (!is.list(result)) stop("GitHub token provider must return a list")
  kind <- result[["kind"]] %||% "token"
  if (identical(kind, "cancelled")) return(list(kind = "cancelled"))
  wire <- list(kind = kind)
  access_token <- result[["accessToken"]] %||% result[["access_token"]]
  if (!is.null(access_token)) wire$accessToken <- access_token
  token_type <- result[["tokenType"]] %||% result[["token_type"]]
  if (!is.null(token_type)) wire$tokenType <- token_type
  expires_in <- result[["expiresIn"]] %||% result[["expires_in"]]
  if (!is.null(expires_in)) wire$expiresIn <- expires_in
  wire
}

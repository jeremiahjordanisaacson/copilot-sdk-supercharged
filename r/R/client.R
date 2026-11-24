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
    #' @param mode Character. Client mode: \code{"coding-agent"} (default, CLI-like coding
    #'   defaults) or \code{"empty"}. Empty mode is enforced by the SDK: every
    #'   \code{create_session()} / \code{resume_session()} call must pass \code{available_tools},
    #'   and restrictive defaults (no experimental mode, telemetry, embedding retrieval,
    #'   cross-session store, memory, on-demand instruction discovery, repository hooks, or host
    #'   git operations; in-memory OAuth/embedding storage) are applied under explicit options.
    #'   \code{"copilot-cli"} is accepted as an alias of \code{"coding-agent"}.
    #' @param network Named list or NULL. Network callbacks registered with
    #'   \code{network.setProvider} after connecting: \code{scope} (\code{"connection"} or
    #'   \code{"runtime"}), \code{resolve_proxy = function(request)} returning a PAC-style proxy
    #'   list (e.g. \code{"PROXY proxy:8080; DIRECT"}) or NULL, \code{authenticate_negotiate =
    #'   function(request)} returning a base64 SPNEGO token or NULL, and \code{authenticate_basic =
    #'   function(request)} returning \code{list(username = ..., password = ...)} or NULL.
    #'   Tokens and passwords are never logged.
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
                          session_fs = NULL, client_info = NULL,
                          mode = "coding-agent", network = NULL) {
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
        client_info = client_info,
        mode = normalize_client_mode(mode)
      )

      private$process <- NULL
      private$client <- NULL
      private$state <- "disconnected"
      private$sessions <- new.env(parent = emptyenv())
      private$models_cache <- NULL
      private$lifecycle_handlers <- list()
      private$session_fs_config <- session_fs
      private$github_token_providers <- new.env(parent = emptyenv())
      private$network_config <- network
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
          private$register_network_provider()

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
    #' @param providers List of NamedProviderConfig (or named lists) or NULL. Named BYOK provider
    #'   connections; select a registered model with \code{"provider/id"}.
    #' @param models List of ProviderModelConfig (or named lists) or NULL. BYOK model definitions
    #'   that reference a provider connection by name.
    #' @param default_provider Character or NULL. Connection used for unqualified model aliases
    #'   when Copilot API models are disabled.
    #' @param capi Named list or NULL. Built-in Copilot API options, e.g.
    #'   \code{list(enabled = FALSE)} for a BYOK-only catalog.
    #' @param on_shell_authentication_request Function or NULL. Called with
    #'   (ShellAuthenticationRequest, context) when a shell command run by the agent asks for a
    #'   password or other answer; return \code{list(action = "accept", value = "...")} or
    #'   \code{list(action = "cancel")}.
    #' @param store_memory Function or NULL. Called with (StoredMemory, context) to store a memory
    #'   in the application instead of the built-in memory backends.
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
                              autopilot = NULL,
                              providers = NULL,
                              models = NULL,
                              default_provider = NULL,
                              capi = NULL,
                              on_shell_authentication_request = NULL,
                              store_memory = NULL) {
      if (!is.null(github_token) && !is.null(github_token_provider)) {
        stop("github_token and github_token_provider are mutually exclusive")
      }
      private$require_available_tools_for_mode(available_tools)
      if (is.null(private$client)) {
        if (isTRUE(self$options$auto_start)) {
          self$start()
        } else {
          stop("Client not connected. Call start() first.")
        }
      }

      # Build tool definitions for the wire
      tool_defs <- tool_definitions_to_wire(tools)

      # Build payload
      payload <- list()
      if (!is.null(model)) payload$model <- model
      if (!is.null(session_id)) payload$sessionId <- session_id
      if (length(tool_defs) > 0) payload$tools <- tool_defs
      if (!is.null(system_message)) payload$systemMessage <- system_message
      if (!is.null(available_tools)) payload$availableTools <- as.list(available_tools)
      if (!is.null(excluded_tools)) payload$excludedTools <- as.list(excluded_tools)
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
      # CAPI options (moved under `github.capi` on the wire): explicit `capi` members plus the
      # auto routing tier and WebSocket transport toggles.
      capi_options <- build_capi_options(auto_tier, enable_web_socket_responses, capi)
      if (!is.null(capi_options)) payload$capi <- capi_options
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
          legacy_provider <- payload[["provider"]]
          if (is.null(legacy_provider)) legacy_provider <- list()
          legacy_provider$bearerToken <- token
          payload[["provider"]] <- legacy_provider
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
        autopilot = autopilot,
        providers = providers,
        models = models,
        default_provider = default_provider,
        on_shell_authentication_request = on_shell_authentication_request,
        store_memory = store_memory
      )
      gh_registration_id <- NULL
      if (!is.null(github_token_provider)) {
        gh_registration_id <- private$register_github_token_provider(github_token_provider, session_id)
        payload$gitHubTokenProviderRegistrationId <- gh_registration_id
      }

      response <- tryCatch(
        private$client$request("session.create", private$to_wire_session_params(payload, FALSE)),
        error = function(e) {
          if (!is.null(gh_registration_id)) private$unregister_github_token_provider(gh_registration_id)
          stop(with_policy_helper_failure(e))
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
      if (!is.null(on_shell_authentication_request)) {
        session$register_shell_authentication_handler(on_shell_authentication_request)
      }
      if (!is.null(store_memory)) {
        session$register_memory_store(store_memory)
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
    #' @param available_tools Character vector or NULL. Tool names to allow (required in
    #'   \code{"empty"} client mode).
    #' @param providers List of NamedProviderConfig (or named lists) or NULL. Named BYOK provider
    #'   connections (re-supply them on cold resume).
    #' @param models List of ProviderModelConfig (or named lists) or NULL. BYOK model definitions.
    #' @param default_provider Character, NA, or NULL. Default connection for unqualified model
    #'   aliases; \code{NA} sends JSON null, which clears the saved default.
    #' @param capi Named list or NULL. Built-in Copilot API options, e.g. \code{list(enabled = TRUE)}.
    #' @param on_shell_authentication_request Function or NULL. Shell authentication handler
    #'   (see create_session).
    #' @param store_memory Function or NULL. Memory store callback (see create_session).
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
                              autopilot = NULL,
                              available_tools = NULL,
                              providers = NULL,
                              models = NULL,
                              default_provider = NULL,
                              capi = NULL,
                              on_shell_authentication_request = NULL,
                              store_memory = NULL) {
      if (!is.null(github_token) && !is.null(github_token_provider)) {
        stop("github_token and github_token_provider are mutually exclusive")
      }
      private$require_available_tools_for_mode(available_tools)
      if (is.null(private$client)) {
        if (isTRUE(self$options$auto_start)) {
          self$start()
        } else {
          stop("Client not connected. Call start() first.")
        }
      }

      tool_defs <- tool_definitions_to_wire(tools)

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
      if (!is.null(available_tools)) payload$availableTools <- as.list(available_tools)
      if (!is.null(excluded_tools)) payload$excludedTools <- as.list(excluded_tools)
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
      capi_options <- build_capi_options(auto_tier, enable_web_socket_responses, capi)
      if (!is.null(capi_options)) payload$capi <- capi_options
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
        autopilot = autopilot,
        providers = providers,
        models = models,
        default_provider = default_provider,
        on_shell_authentication_request = on_shell_authentication_request,
        store_memory = store_memory
      )
      gh_registration_id <- NULL
      if (!is.null(github_token_provider)) {
        gh_registration_id <- private$register_github_token_provider(github_token_provider, session_id)
        payload$gitHubTokenProviderRegistrationId <- gh_registration_id
      }

      response <- tryCatch(
        private$client$request("session.resume", private$to_wire_session_params(payload, TRUE)),
        error = function(e) {
          if (!is.null(gh_registration_id)) private$unregister_github_token_provider(gh_registration_id)
          stop(with_policy_helper_failure(e))
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
      if (!is.null(on_shell_authentication_request)) {
        session$register_shell_authentication_handler(on_shell_authentication_request)
      }
      if (!is.null(store_memory)) session$register_memory_store(store_memory)

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

    #' @description Save an MCP configuration credential in the runtime-owned secret store.
    #'   Requires Copilot CLI 1.0.116 or newer. The value is a credential: it is sent to the
    #'   runtime and never logged.
    #' @param secret_id Character. Secret reference used by an MCP server configuration.
    #' @param value Character. Credential value to persist.
    #' @param config_dir Character or NULL. Expected mcp-secrets directory (NULL uses the
    #'   runtime configuration root).
    #' @return An McpSecretsSaveResult (\code{used_keychain} is TRUE when the OS keychain
    #'   stored the credential, FALSE for the file backend).
    mcp_secrets_save = function(secret_id, value, config_dir = NULL) {
      if (is.null(private$client)) stop("Client not connected")
      params <- list(secretId = secret_id, value = value)
      if (!is.null(config_dir)) params$configDir <- config_dir
      result <- private$client$request("mcp.secrets.save", params)
      McpSecretsSaveResult$new(used_keychain = result$usedKeychain)
    },

    #' @description Delete an MCP configuration credential from the runtime-owned secret
    #'   store. Requires Copilot CLI 1.0.116 or newer.
    #' @param secret_id Character. Secret reference to remove.
    #' @param config_dir Character or NULL. Expected mcp-secrets directory.
    mcp_secrets_delete = function(secret_id, config_dir = NULL) {
      if (is.null(private$client)) stop("Client not connected")
      params <- list(secretId = secret_id)
      if (!is.null(config_dir)) params$configDir <- config_dir
      private$client$request("mcp.secrets.delete", params)
      invisible(NULL)
    },

    #' @description Delete the credentials indexed for an MCP server. Requires Copilot CLI
    #'   1.0.116 or newer.
    #' @param server_name Character. MCP server name.
    #' @param config_dir Character or NULL. Expected mcp-secrets directory.
    mcp_secrets_delete_server = function(server_name, config_dir = NULL) {
      if (is.null(private$client)) stop("Client not connected")
      params <- list(serverName = server_name)
      if (!is.null(config_dir)) params$configDir <- config_dir
      private$client$request("mcp.secrets.deleteServer", params)
      invisible(NULL)
    },

    #' @description Persist a pending plugin extension permission request as a user-scoped
    #'   approval, before answering that request normally. Requires Copilot CLI 1.0.116 or
    #'   newer.
    #' @param session_id Character. Session that owns the pending request.
    #' @param request_id Character. Identifier of the pending extension permission request.
    #' @return A PermissionsExtensionApprovalsGrantResult (\code{granted}, plus the persisted
    #'   \code{approval} when granted).
    extension_approvals_grant = function(session_id, request_id) {
      if (is.null(private$client)) stop("Client not connected")
      result <- private$client$request(
        "permissions.extensionApprovals.grant",
        list(sessionId = session_id, requestId = request_id)
      )
      PermissionsExtensionApprovalsGrantResult$new(
        granted = result$granted,
        approval = permissions_extension_approval_from_list(result$approval)
      )
    },

    #' @description List the user-scoped plugin extension approvals. Requires Copilot CLI
    #'   1.0.116 or newer.
    #' @param session_id Character or NULL. Attached session whose configuration profile to
    #'   list (NULL lists the server profile).
    #' @return A list of PermissionsExtensionApproval objects.
    extension_approvals_list = function(session_id = NULL) {
      if (is.null(private$client)) stop("Client not connected")
      params <- empty_json_object()
      if (!is.null(session_id)) params$sessionId <- session_id
      result <- private$client$request("permissions.extensionApprovals.list", params)
      lapply(result$approvals %||% list(), permissions_extension_approval_from_list)
    },

    #' @description Revoke one user-scoped plugin extension approval. Requires Copilot CLI
    #'   1.0.116 or newer.
    #' @param extension_name Character. Extension identity of the approval to revoke.
    #' @param installation_key Character. Opaque installation key from a grant or list result,
    #'   passed back unchanged.
    #' @param session_id Character or NULL. Attached session whose configuration profile holds
    #'   the approval.
    #' @return A PermissionsExtensionApprovalsRevokeResult (\code{revoked}).
    extension_approvals_revoke = function(extension_name, installation_key, session_id = NULL) {
      if (is.null(private$client)) stop("Client not connected")
      params <- list(extensionName = extension_name, installationKey = installation_key)
      if (!is.null(session_id)) params$sessionId <- session_id
      result <- private$client$request("permissions.extensionApprovals.revoke", params)
      PermissionsExtensionApprovalsRevokeResult$new(revoked = result$revoked)
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
    network_config = NULL,

    # Empty client mode: every create/resume must declare its tool allowlist.
    require_available_tools_for_mode = function(available_tools) {
      if (identical(self$options$mode, "empty") && is.null(available_tools)) {
        stop(paste0(
          "Client mode \"empty\" requires available_tools on every create_session() ",
          "and resume_session() call"
        ))
      }
      invisible(NULL)
    },

    # Convert the flat session payload to the SDK protocol 4 grouped shape, applying the
    # empty-mode defaults under explicit options.
    to_wire_session_params = function(payload, is_resume) {
      wire <- group_session_params(payload, is_resume = is_resume)
      if (identical(self$options$mode, "empty")) wire <- apply_empty_mode_defaults(wire)
      wire
    },

    # Register the network callbacks (network.setProvider) when any callback is configured.
    register_network_provider = function() {
      network <- private$network_config
      if (is.null(network)) return(invisible(NULL))
      params <- empty_json_object()
      if (!is.null(network[["scope"]])) params$scope <- network[["scope"]]
      registered <- FALSE
      if (is.function(network[["resolve_proxy"]])) {
        params$resolveProxy <- empty_json_object()
        registered <- TRUE
      }
      if (is.function(network[["authenticate_negotiate"]])) {
        params$authenticateNegotiate <- empty_json_object()
        registered <- TRUE
      }
      if (is.function(network[["authenticate_basic"]])) {
        params$authenticateBasic <- empty_json_object()
        registered <- TRUE
      }
      if (registered) private$client$request("network.setProvider", params)
      invisible(NULL)
    },

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

      # Build environment: processx needs a named character vector
      env_vec <- unclass(Sys.getenv())
      if (!is.null(self$options$env)) {
        for (nm in names(self$options$env)) {
          env_vec[[nm]] <- as.character(self$options$env[[nm]])
        }
      }
      if (!is.null(self$options$github_token)) {
        env_vec[["COPILOT_SDK_AUTH_TOKEN"]] <- self$options$github_token
      }

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
            private$handle_broadcast_event(session_id, session, event_dict)
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

      private$client$set_request_handler("shellAuthentication.request", function(params) {
        private$handle_shell_authentication_request(params)
      })

      # R handlers run synchronously, so a cancellation can only arrive after the handler
      # for that request has already returned; acknowledge it.
      private$client$set_request_handler("shellAuthentication.cancel", function(params) {
        empty_json_object()
      })

      private$client$set_request_handler("memoryStore.store", function(params) {
        private$handle_memory_store(params)
      })

      private$client$set_request_handler("network.resolveProxy", function(params) {
        callback <- private$network_config[["resolve_proxy"]]
        if (!is.function(callback)) stop("No network resolve_proxy callback is registered")
        proxy <- callback(list(url = params$url))
        result <- empty_json_object()
        if (!is.null(proxy)) result$proxy <- as.character(proxy)[[1]]
        result
      })

      private$client$set_request_handler("network.authenticateNegotiate", function(params) {
        callback <- private$network_config[["authenticate_negotiate"]]
        if (!is.function(callback)) stop("No network authenticate_negotiate callback is registered")
        token <- callback(list(spn = params$spn))
        result <- empty_json_object()
        if (!is.null(token)) result$token <- as.character(token)[[1]]
        result
      })

      private$client$set_request_handler("network.authenticateBasic", function(params) {
        callback <- private$network_config[["authenticate_basic"]]
        if (!is.function(callback)) stop("No network authenticate_basic callback is registered")
        credentials <- callback(list(
          proxy = params$proxy,
          realm = params$realm,
          previous_rejected = isTRUE(params$previousRejected)
        ))
        # Send only the credential fields, never other members of the returned list.
        result <- empty_json_object()
        if (is.list(credentials)) {
          if (!is.null(credentials[["username"]])) result$username <- credentials[["username"]]
          if (!is.null(credentials[["password"]])) result$password <- credentials[["password"]]
        }
        result
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
          "SDK protocol version mismatch: SDK supports version ", expected,
          ", but server does not report a protocol version. ",
          "Please update your server to ensure compatibility."
        ))
      }

      if (server_version != expected) {
        stop(paste0(
          "SDK protocol version mismatch: SDK supports version ", expected,
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
      # Connection extras and model-specific limits, carried into the v4 provider/model split.
      extras <- provider_config_to_wire(provider[intersect(names(provider), c(
        "transport", "headers", "local_model", "model_id", "wire_model", "max_prompt_tokens",
        "max_context_window_tokens", "max_output_tokens", "capabilities"
      ))])
      for (key in names(extras)) wire[[key]] <- extras[[key]]
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

    # SDK protocol 4 delivers tool calls and permission prompts as session events
    # (external_tool.requested, permission.requested), answered with
    # session.tools.handlePendingToolCall and
    # session.permissions.handlePendingPermissionRequest. Replies are sent without waiting
    # for their response because this runs inside the synchronous message loop.
    handle_broadcast_event = function(session_id, session, event_dict) {
      type <- event_dict[["type"]]
      data <- event_dict[["data"]]
      if (!is.character(type) || !is.list(data)) return(invisible(NULL))
      request_id <- data[["requestId"]]
      if (!is.character(request_id)) return(invisible(NULL))

      if (identical(type, "external_tool.requested")) {
        tool_name <- data[["toolName"]]
        if (isFALSE(data[["hasHandler"]]) || !is.character(tool_name) ||
            is.null(session$get_tool_handler(tool_name))) {
          return(invisible(NULL))
        }
        params <- list(sessionId = session_id, requestId = request_id)
        tryCatch(
          {
            outcome <- private$handle_tool_call(list(
              sessionId = session_id,
              toolCallId = data[["toolCallId"]] %||% "",
              toolName = tool_name,
              arguments = data[["arguments"]]
            ))
            result <- outcome[["result"]]
            if (length(result[["toolTelemetry"]]) == 0) result[["toolTelemetry"]] <- NULL
            params$result <- result
          },
          error = function(e) {
            params$error <<- conditionMessage(e)
          }
        )
        private$client$request_no_wait("session.tools.handlePendingToolCall", params)
      } else if (identical(type, "permission.requested")) {
        if (isTRUE(data[["resolvedByHook"]]) || !session$has_permission_handler()) {
          return(invisible(NULL))
        }
        decision <- permission_decision_to_v4(
          session$handle_permission_request(data[["permissionRequest"]])
        )
        if (!is.null(decision)) {
          private$client$request_no_wait(
            "session.permissions.handlePendingPermissionRequest",
            list(sessionId = session_id, requestId = request_id, result = decision)
          )
        }
      }
      invisible(NULL)
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
      reply <- list(answer = result$answer, wasFreeform = result$was_freeform)
      attachments <- user_input_attachments_to_wire(result$attachments)
      if (!is.null(attachments)) reply$attachments <- attachments
      reply
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

    handle_shell_authentication_request = function(params) {
      cancel <- list(action = "cancel")
      session_id <- params$sessionId
      input_kind <- params$inputKind
      if (!is.character(session_id) || !is.character(params$requestId) ||
          !is.character(params$program) || !is.character(params$prompt) ||
          !is.character(input_kind) || !(input_kind %in% c("secret", "text", "confirm"))) {
        return(cancel)
      }
      if (!exists(session_id, envir = private$sessions, inherits = FALSE)) return(cancel)
      session <- get(session_id, envir = private$sessions, inherits = FALSE)
      session$handle_shell_authentication_request(params)
    },

    handle_memory_store = function(params) {
      session_id <- params$sessionId
      if (!is.character(session_id) ||
          !exists(session_id, envir = private$sessions, inherits = FALSE)) {
        stop("Memory store owner is unavailable")
      }
      session <- get(session_id, envir = private$sessions, inherits = FALSE)
      session$handle_memory_store(params)
      json_rpc_null()
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
#' Explicit \code{capi} members (e.g. \code{list(enabled = FALSE)}) are kept; the auto routing
#' tier and WebSocket toggle are added on top. The translation layer moves it to \code{github.capi}.
#' @keywords internal
build_capi_options <- function(auto_tier = NULL, enable_web_socket_responses = NULL, capi = NULL) {
  result <- list()
  if (!is.null(capi)) {
    capi <- config_to_wire(capi)
    for (key in names(capi)) result[[key]] <- capi[[key]]
  }
  if (!is.null(auto_tier)) result$autoTier <- auto_tier
  if (!is.null(enable_web_socket_responses)) {
    result$enableWebSocketResponses <- enable_web_socket_responses
  }
  if (length(result) == 0) NULL else result
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
                                       image_generation = NULL, autopilot = NULL,
                                       providers = NULL, models = NULL,
                                       default_provider = NULL,
                                       on_shell_authentication_request = NULL,
                                       store_memory = NULL) {
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
  # --- v3.0.0 (SDK protocol 4) session options ---
  if (!is.null(providers)) {
    payload$providers <- unname(lapply(as_config_list(providers), provider_config_to_wire))
  }
  if (!is.null(models)) {
    payload$models <- unname(lapply(as_config_list(models), provider_config_to_wire))
  }
  # NA is sent as JSON null, which clears a saved default provider on resume.
  if (!is.null(default_provider)) payload$defaultProvider <- default_provider
  if (!is.null(on_shell_authentication_request)) payload$requestShellAuthentication <- TRUE
  if (!is.null(store_memory)) payload$hasMemoryStore <- TRUE
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


# ---------------------------------------------------------------------------
# v3.0.0 (SDK protocol 4) types & helpers
# ---------------------------------------------------------------------------

#' ClientMode
#'
#' Client modes for \code{CopilotClient$new(mode = ...)}. \code{"coding-agent"} (the default)
#' keeps the CLI-like coding tools and defaults; \code{"empty"} requires an explicit
#' \code{available_tools} allowlist on every session and applies restrictive defaults. The
#' legacy \code{"copilot-cli"} value is accepted as an alias of \code{"coding-agent"}.
#'
#' @export
ClientMode <- list(
  CODING_AGENT = "coding-agent",
  EMPTY = "empty"
)

#' Normalize and validate a client mode.
#' @keywords internal
normalize_client_mode <- function(mode) {
  if (is.null(mode)) return(ClientMode$CODING_AGENT)
  mode <- as.character(mode)[[1]]
  if (identical(mode, "copilot-cli")) return(ClientMode$CODING_AGENT)
  if (!(mode %in% unlist(ClientMode))) {
    stop(paste0("Invalid client mode \"", mode, "\": expected \"coding-agent\" or \"empty\""))
  }
  mode
}

#' Defaults applied (under explicit options) to every session of an \code{"empty"} mode client,
#' as \code{list(group, member, value)} entries of the grouped protocol 4 payload.
#' @keywords internal
EMPTY_MODE_SESSION_DEFAULTS <- list(
  list("experiments", "enableExperimentalMode", FALSE),
  list("github", "telemetryEnabled", FALSE),
  list("mcp", "oauthTokenStorage", "in-memory"),
  list("state", "skipEmbeddingRetrieval", TRUE),
  list("state", "embeddingCacheStorage", "in-memory"),
  list("state", "enableCrossSessionStore", FALSE),
  list("state", "memory", list(enabled = FALSE)),
  list("instructions", "onDemandDiscovery", FALSE),
  list("hooks", "enableRepositoryHooks", FALSE),
  list("git", "enableHostGitOperations", FALSE)
)

#' Apply the empty-mode defaults to a grouped payload without overriding explicit members.
#' @keywords internal
apply_empty_mode_defaults <- function(params) {
  for (entry in EMPTY_MODE_SESSION_DEFAULTS) {
    group <- entry[[1]]
    member <- entry[[2]]
    members <- params[[group]]
    if (is.null(members)) members <- list()
    if (is.null(members[[member]])) {
      members[[member]] <- entry[[3]]
      params[[group]] <- members
    }
  }
  params
}

#' Option groups of the SDK protocol 4 session.create / session.resume payload.
#' @keywords internal
SESSION_OPTION_GROUPS <- c(
  "inference", "tools", "permissions", "interaction", "events", "agents", "skills", "mcp",
  "github", "filesystem", "execution", "instructions", "hooks", "git", "state", "plugins",
  "canvases", "extensions", "experiments", "policy"
)

#' Flat session payload key -> \code{c(group, member)} in the SDK protocol 4 payload.
#' Keys not listed here (root keys such as \code{model}, and unknown keys) stay at the root.
#' @keywords internal
SESSION_PARAM_GROUPS <- list(
  reasoningEffort = c("inference", "reasoningEffort"),
  reasoningSummary = c("inference", "reasoningSummary"),
  contextTier = c("inference", "contextTier"),
  allowMultipleToolCalls = c("inference", "allowMultipleToolCalls"),
  enableCitations = c("inference", "enableCitations"),
  modelCapabilities = c("inference", "capabilities"),
  verbosity = c("inference", "verbosity"),
  providers = c("inference", "providers"),
  models = c("inference", "models"),
  defaultProvider = c("inference", "defaultProvider"),
  tools = c("tools", "definitions"),
  availableTools = c("tools", "availableTools"),
  excludedTools = c("tools", "excludedTools"),
  toolSearch = c("tools", "search"),
  largeOutput = c("tools", "largeOutput"),
  imageGeneration = c("tools", "imageGeneration"),
  manageScheduleEnabled = c("tools", "manageScheduleEnabled"),
  webSearch = c("tools", "webSearch"),
  shell = c("tools", "shell"),
  maxInlineBinaryBytes = c("tools", "maxInlineBinaryBytes"),
  requestPermission = c("permissions", "request"),
  sandboxConfig = c("permissions", "sandbox"),
  sandbox = c("permissions", "sandbox"),
  permissionMode = c("permissions", "mode"),
  sandboxConfigSource = c("permissions", "sandboxSource"),
  commands = c("interaction", "commands"),
  askUserVariant = c("interaction", "askUserVariant"),
  requestUserInput = c("interaction", "requestUserInput"),
  requestElicitation = c("interaction", "requestElicitation"),
  requestShellAuthentication = c("interaction", "requestShellAuthentication"),
  requestExitPlanMode = c("interaction", "requestExitPlanMode"),
  requestAutoModeSwitch = c("interaction", "requestAutoModeSwitch"),
  requestAutoModeRecovery = c("interaction", "requestAutoModeRecovery"),
  streaming = c("events", "streaming"),
  includeSubAgentStreamingEvents = c("events", "includeSubAgentStreamingEvents"),
  customAgents = c("agents", "definitions"),
  defaultAgent = c("agents", "default"),
  agent = c("agents", "selected"),
  includedBuiltinAgents = c("agents", "includedBuiltinAgents"),
  excludedBuiltinAgents = c("agents", "excludedBuiltinAgents"),
  customAgentsLocalOnly = c("agents", "localDiscoveryOnly"),
  agentDirectories = c("agents", "directories"),
  skillDirectories = c("skills", "directories"),
  enableSkills = c("skills", "enabled"),
  includedBuiltinSkills = c("skills", "includedBuiltinSkills"),
  disabledSkills = c("skills", "disabledSkills"),
  hasSkillProvider = c("skills", "hasProvider"),
  ignoredSkillsLocations = c("skills", "ignoredLocations"),
  mcpServers = c("mcp", "servers"),
  disabledMcpServers = c("mcp", "disabledServers"),
  requestMcpApps = c("mcp", "enableApps"),
  enableMcpApps = c("mcp", "enableApps"),
  mcpOAuthTokenStorage = c("mcp", "oauthTokenStorage"),
  authClientIdMetadataUrl = c("mcp", "authClientIdMetadataUrl"),
  envValueMode = c("mcp", "envValueMode"),
  managedMcpServers = c("mcp", "managedServers"),
  allowAllMcpServerInstructions = c("mcp", "allowAllServerInstructions"),
  gitHubToken = c("github", "token"),
  gitHubTokenProviderRegistrationId = c("github", "tokenProviderRegistrationId"),
  githubMcpToolConfig = c("github", "mcp"),
  capi = c("github", "capi"),
  enableSessionTelemetry = c("github", "telemetryEnabled"),
  enableManagedSettings = c("github", "fetchManagedSettings"),
  interactiveUser = c("github", "interactiveUser"),
  enableGitHubTelemetryForwarding = c("github", "telemetryForwarding"),
  authInfo = c("github", "authInfo"),
  copilotUrl = c("github", "copilotUrl"),
  additionalDirectories = c("filesystem", "additionalDirectories"),
  enableFileChangeTracking = c("filesystem", "trackFileChanges"),
  trustWorkingDirectory = c("filesystem", "trustWorkingDirectory"),
  sessionLimits = c("execution", "limits"),
  autopilot = c("execution", "autopilot"),
  autopilotContinuation = c("execution", "autopilotContinuation"),
  instructionDirectories = c("instructions", "directories"),
  organizationCustomInstructions = c("instructions", "organizationText"),
  enableOnDemandInstructionDiscovery = c("instructions", "onDemandDiscovery"),
  skipCustomInstructions = c("instructions", "skipCustomInstructions"),
  refreshCustomInstructions = c("instructions", "refreshCustomInstructions"),
  hooks = c("hooks", "hasHandlers"),
  hookDirectories = c("hooks", "directories"),
  enableFileHooks = c("hooks", "enableRepositoryHooks"),
  coauthorEnabled = c("git", "coauthorEnabled"),
  enableHostGitOperations = c("git", "enableHostGitOperations"),
  infiniteSessions = c("state", "infiniteSessions"),
  memory = c("state", "memory"),
  hasMemoryStore = c("state", "hasMemoryStore"),
  enableSessionStore = c("state", "enableCrossSessionStore"),
  skipEmbeddingRetrieval = c("state", "skipEmbeddingRetrieval"),
  embeddingCacheStorage = c("state", "embeddingCacheStorage"),
  pluginDirectories = c("plugins", "directories"),
  installedPlugins = c("plugins", "installed"),
  canvases = c("canvases", "definitions"),
  canvasProvider = c("canvases", "provider"),
  requestCanvasRenderer = c("canvases", "requestRenderer"),
  openCanvases = c("canvases", "open"),
  requestExtensions = c("extensions", "enabled"),
  extensionSdkPath = c("extensions", "sdkPath"),
  extensionInfo = c("extensions", "identity"),
  enableExperimentalMode = c("experiments", "enableExperimentalMode"),
  isExperimentalMode = c("experiments", "enableExperimentalMode"),
  featureFlags = c("experiments", "flags"),
  expAssignments = c("experiments", "assignments"),
  featureFlagConfiguration = c("experiments", "configuration"),
  allowedModels = c("policy", "allowedModels"),
  managedSettings = c("policy", "managedSettings"),
  excludedManagedSettings = c("policy", "excludedManagedSettings"),
  enforceManagedModelDefaults = c("policy", "enforceManagedModelDefaults")
)

#' Members of a legacy single \code{provider} that belong to a model definition.
#' @keywords internal
LEGACY_PROVIDER_MODEL_KEYS <- c(
  "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens",
  "capabilities"
)

#' Split a legacy single BYOK \code{provider} into a named "default" connection and, when it
#' carries model-specific members and a session model is set, a model definition.
#' @keywords internal
legacy_provider_to_v4 <- function(provider, model = NULL) {
  provider <- provider[!vapply(provider, is.null, logical(1))]
  keys <- names(provider)
  connection <- c(
    list(name = "default"),
    provider[setdiff(keys, c(LEGACY_PROVIDER_MODEL_KEYS, "name"))]
  )
  result <- list(providers = list(connection))
  model_keys <- intersect(keys, LEGACY_PROVIDER_MODEL_KEYS)
  if (length(model_keys) > 0 && !is.null(model)) {
    result$models <- list(c(list(provider = "default", id = model), provider[model_keys]))
  }
  result
}

#' Convert a flat session.create / session.resume payload to the SDK protocol 4 grouped shape.
#'
#' Root keys (\code{sessionId}, \code{model}, \code{systemMessage}, ...) and unknown keys stay at
#' the top level; every other known flat key moves into its option group (for example
#' \code{streaming} becomes \code{events.streaming} and \code{hooks = TRUE} becomes
#' \code{hooks.hasHandlers}). Already-grouped objects are merged with their explicit members
#' winning, empty groups are not emitted, \code{configDir} becomes \code{configDirectory}, and
#' \code{disableResume} becomes \code{suppressResumeEvent} on resume. A legacy single
#' \code{provider} becomes the named "default" connection with Copilot API models disabled,
#' unless named providers, a default provider, or \code{capi.enabled} are set explicitly.
#'
#' @param flat Named list. The flat payload.
#' @param is_resume Logical. TRUE for session.resume (drops create-only members).
#' @return Named list. The grouped payload.
#' @keywords internal
group_session_params <- function(flat, is_resume = FALSE) {
  root <- list()
  groups <- list()
  explicit <- list()
  set_member <- function(group, member, value) {
    members <- groups[[group]]
    if (is.null(members)) members <- list()
    members[[member]] <- value
    groups[[group]] <<- members
  }

  for (key in names(flat)) {
    value <- flat[[key]]
    if (is.null(value) || identical(key, "provider")) next
    if (key %in% SESSION_OPTION_GROUPS && is.list(value) && !is.null(names(value))) {
      explicit[[key]] <- value
      next
    }
    if (identical(key, "configDir")) {
      if (is.null(flat[["configDirectory"]])) root$configDirectory <- value
      next
    }
    if (identical(key, "disableResume")) {
      if (isTRUE(is_resume) && is.null(flat[["suppressResumeEvent"]])) {
        root$suppressResumeEvent <- value
      }
      next
    }
    if (isTRUE(is_resume) && key %in% c("trustWorkingDirectory", "refreshCustomInstructions")) next
    if (!isTRUE(is_resume) && identical(key, "openCanvases")) next
    path <- SESSION_PARAM_GROUPS[[key]]
    if (is.null(path)) {
      root[[key]] <- value
    } else {
      set_member(path[[1]], path[[2]], value)
    }
  }

  for (group in names(explicit)) {
    for (member in names(explicit[[group]])) {
      value <- explicit[[group]][[member]]
      if (!is.null(value)) set_member(group, member, value)
    }
  }

  provider <- flat[["provider"]]
  if (is.list(provider) && length(provider) > 0) {
    inference <- groups[["inference"]]
    capi <- groups[["github"]][["capi"]]
    explicit_routing <- !is.null(inference[["providers"]]) ||
      !is.null(inference[["defaultProvider"]]) || !is.null(capi[["enabled"]])
    if (!explicit_routing) {
      translated <- legacy_provider_to_v4(provider, flat[["model"]])
      set_member("inference", "providers", translated$providers)
      set_member("inference", "defaultProvider", "default")
      if (!is.null(translated$models) && is.null(inference[["models"]])) {
        set_member("inference", "models", translated$models)
      }
      if (is.null(capi)) capi <- list()
      capi$enabled <- FALSE
      set_member("github", "capi", capi)
    }
  }

  if (!is.null(groups[["mcp"]][["servers"]]) && is.null(groups[["mcp"]][["envValueMode"]])) {
    set_member("mcp", "envValueMode", "direct")
  }

  for (group in names(groups)) {
    if (length(groups[[group]]) > 0) root[[group]] <- groups[[group]]
  }
  if (length(root) == 0) return(empty_json_object())
  root
}

#' Build the wire tool definitions (\code{tools.definitions}) for a list of Tool objects.
#' @keywords internal
tool_definitions_to_wire <- function(tools) {
  tool_defs <- list()
  if (is.null(tools)) return(tool_defs)
  for (tool in tools) {
    defn <- list(name = tool$name, description = tool$description)
    if (!is.null(tool$parameters)) defn$parameters <- tool$parameters
    if (!is.null(tool$idempotent)) defn$idempotent <- isTRUE(tool$idempotent)
    if (isTRUE(tool$skip_permission)) defn$skipPermission <- TRUE
    tool_defs <- c(tool_defs, list(defn))
  }
  tool_defs
}

#' Wire (camelCase) names of NamedProviderConfig / ProviderModelConfig members.
#' @keywords internal
PROVIDER_WIRE_KEYS <- c(
  base_url = "baseUrl", api_key = "apiKey", api_key_command = "apiKeyCommand",
  bearer_token = "bearerToken", wire_api = "wireApi", model_provider = "modelProvider",
  local_model = "localModel", model_id = "modelId", wire_model = "wireModel",
  max_prompt_tokens = "maxPromptTokens", max_context_window_tokens = "maxContextWindowTokens",
  max_output_tokens = "maxOutputTokens"
)

#' Convert a provider connection or model definition (R6 object, or a named list with snake_case
#' or camelCase names) to its wire list. NULL members are dropped.
#' @keywords internal
provider_config_to_wire <- function(config) {
  if (inherits(config, "R6") && is.function(config$to_list)) return(config$to_list())
  config <- as.list(config)
  config <- config[!vapply(config, is.null, logical(1))]
  keys <- names(config)
  if (is.null(keys)) return(config)
  renamed <- keys %in% names(PROVIDER_WIRE_KEYS)
  keys[renamed] <- unname(PROVIDER_WIRE_KEYS[keys[renamed]])
  names(config) <- keys
  azure <- config[["azure"]]
  if (is.list(azure) && !is.null(azure[["api_version"]])) {
    azure[["apiVersion"]] <- azure[["api_version"]]
    azure[["api_version"]] <- NULL
    config[["azure"]] <- azure
  }
  config
}

#' Wrap a single provider/model config so a list of configs can be iterated.
#' @keywords internal
as_config_list <- function(x) {
  if (inherits(x, "R6")) return(list(x))
  single_keys <- c("name", "id", "provider", "base_url", "baseUrl")
  if (is.list(x) && !is.null(names(x)) && any(single_keys %in% names(x))) return(list(x))
  as.list(x)
}

#' NamedProviderConfig
#'
#' A named BYOK provider connection (wire \code{inference.providers[]}). Model definitions
#' reference it by \code{name}; select a registered model with \code{"<name>/<id>"}.
#'
#' @field name Character. Stable connection name (must not contain "/").
#' @field base_url Character. API endpoint URL.
#' @field type Character or NULL. "openai" (default), "azure", or "anthropic".
#' @field api_key Character or NULL. Static API key.
#' @field api_key_command Character or NULL. Command run before each request for a fresh key.
#' @field bearer_token Character or NULL. Static bearer token (takes precedence over api_key).
#' @field wire_api Character or NULL. "completions" (default) or "responses".
#' @field transport Character or NULL. "http" (default) or "websockets".
#' @field azure Named list or NULL. Azure options, e.g. \code{list(api_version = "2024-10-21")}.
#' @field headers Named list or NULL. Extra HTTP headers sent with every provider request.
#' @field model_provider Character or NULL. Product serving the models (telemetry only).
#' @export
NamedProviderConfig <- R6::R6Class(
  "NamedProviderConfig",
  public = list(
    name = NULL,
    base_url = NULL,
    type = NULL,
    api_key = NULL,
    api_key_command = NULL,
    bearer_token = NULL,
    wire_api = NULL,
    transport = NULL,
    azure = NULL,
    headers = NULL,
    model_provider = NULL,

    #' @description Create a new NamedProviderConfig.
    #' @param name Character.
    #' @param base_url Character.
    #' @param type Character or NULL.
    #' @param api_key Character or NULL.
    #' @param api_key_command Character or NULL.
    #' @param bearer_token Character or NULL.
    #' @param wire_api Character or NULL.
    #' @param transport Character or NULL.
    #' @param azure Named list or NULL.
    #' @param headers Named list or NULL.
    #' @param model_provider Character or NULL.
    initialize = function(name, base_url, type = NULL, api_key = NULL, api_key_command = NULL,
                          bearer_token = NULL, wire_api = NULL, transport = NULL, azure = NULL,
                          headers = NULL, model_provider = NULL) {
      self$name <- name
      self$base_url <- base_url
      self$type <- type
      self$api_key <- api_key
      self$api_key_command <- api_key_command
      self$bearer_token <- bearer_token
      self$wire_api <- wire_api
      self$transport <- transport
      self$azure <- azure
      self$headers <- headers
      self$model_provider <- model_provider
    },

    #' @description Convert to a wire list (camelCase keys).
    to_list = function() {
      provider_config_to_wire(list(
        name = self$name, type = self$type, base_url = self$base_url,
        api_key = self$api_key, api_key_command = self$api_key_command,
        bearer_token = self$bearer_token, wire_api = self$wire_api,
        transport = self$transport, azure = self$azure, headers = self$headers,
        model_provider = self$model_provider
      ))
    }
  )
)

#' ProviderModelConfig
#'
#' A BYOK model definition (wire \code{inference.models[]}) served by a NamedProviderConfig.
#'
#' @field provider Character. Name of the NamedProviderConfig serving the model.
#' @field id Character. Provider-local model id; select it with \code{"<provider>/<id>"}.
#' @field model_id Character or NULL. Well-known base model for capability lookup.
#' @field wire_model Character or NULL. Model or deployment name sent to the provider API.
#' @field name Character or NULL. Display name.
#' @field max_prompt_tokens Integer or NULL.
#' @field max_context_window_tokens Integer or NULL.
#' @field max_output_tokens Integer or NULL.
#' @field capabilities Named list or NULL. Capability overrides (wire keys as documented upstream).
#' @export
ProviderModelConfig <- R6::R6Class(
  "ProviderModelConfig",
  public = list(
    provider = NULL,
    id = NULL,
    model_id = NULL,
    wire_model = NULL,
    name = NULL,
    max_prompt_tokens = NULL,
    max_context_window_tokens = NULL,
    max_output_tokens = NULL,
    capabilities = NULL,

    #' @description Create a new ProviderModelConfig.
    #' @param provider Character.
    #' @param id Character.
    #' @param model_id Character or NULL.
    #' @param wire_model Character or NULL.
    #' @param name Character or NULL.
    #' @param max_prompt_tokens Integer or NULL.
    #' @param max_context_window_tokens Integer or NULL.
    #' @param max_output_tokens Integer or NULL.
    #' @param capabilities Named list or NULL.
    initialize = function(provider, id, model_id = NULL, wire_model = NULL, name = NULL,
                          max_prompt_tokens = NULL, max_context_window_tokens = NULL,
                          max_output_tokens = NULL, capabilities = NULL) {
      self$provider <- provider
      self$id <- id
      self$model_id <- model_id
      self$wire_model <- wire_model
      self$name <- name
      self$max_prompt_tokens <- max_prompt_tokens
      self$max_context_window_tokens <- max_context_window_tokens
      self$max_output_tokens <- max_output_tokens
      self$capabilities <- capabilities
    },

    #' @description Convert to a wire list (camelCase keys).
    to_list = function() {
      provider_config_to_wire(list(
        provider = self$provider, id = self$id, model_id = self$model_id,
        wire_model = self$wire_model, name = self$name,
        max_prompt_tokens = self$max_prompt_tokens,
        max_context_window_tokens = self$max_context_window_tokens,
        max_output_tokens = self$max_output_tokens, capabilities = self$capabilities
      ))
    }
  )
)

#' SessionEventType
#'
#' Common session event type names. Events of other (including future) types are still
#' delivered unchanged; \code{tool.execution_partial_result} is no longer emitted by protocol 4
#' runtimes but is tolerated when received.
#'
#' @export
SessionEventType <- list(
  ASSISTANT_MESSAGE = "assistant.message",
  ASSISTANT_MESSAGE_DELTA = "assistant.message_delta",
  SESSION_IDLE = "session.idle",
  SESSION_ERROR = "session.error",
  TOOL_SHELL_OUTPUT = "tool.shell_output",
  TOOL_EXECUTION_PARTIAL_RESULT = "tool.execution_partial_result"
)

#' ShellOutputEventData
#'
#' Data of a \code{tool.shell_output} event: a chunk of output streamed by a shell tool call.
#'
#' @field tool_call_id Character. Tool call that produced the output.
#' @field text Character. Output text chunk.
#' @field sequence Integer. Chunk sequence number within the tool call.
#' @field stream Character or NULL. "stdout", "stderr", or NULL when not reported.
#' @export
ShellOutputEventData <- R6::R6Class(
  "ShellOutputEventData",
  public = list(
    tool_call_id = NULL,
    text = NULL,
    sequence = NULL,
    stream = NULL,

    #' @description Create a new ShellOutputEventData.
    #' @param tool_call_id Character.
    #' @param text Character.
    #' @param sequence Integer.
    #' @param stream Character or NULL.
    initialize = function(tool_call_id = NULL, text = NULL, sequence = NULL, stream = NULL) {
      self$tool_call_id <- tool_call_id
      self$text <- text
      self$sequence <- sequence
      self$stream <- stream
    }
  )
)

#' Parse the data of a \code{tool.shell_output} event.
#'
#' @param event SessionEvent, or the event's \code{data} named list.
#' @return A ShellOutputEventData, or NULL when the event is not a shell output event.
#' @export
shell_output_event_data <- function(event) {
  data <- event
  if (inherits(event, "SessionEvent")) {
    if (!identical(event$type, SessionEventType$TOOL_SHELL_OUTPUT)) return(NULL)
    data <- event$data
  }
  if (!is.list(data)) return(NULL)
  ShellOutputEventData$new(
    tool_call_id = data[["toolCallId"]],
    text = data[["text"]],
    sequence = data[["sequence"]],
    stream = data[["stream"]]
  )
}

#' ShellAuthenticationRequest
#'
#' A password or other answer that a shell command run by the agent asks for (for example
#' \code{sudo -A}, \code{ssh}, or \code{git} over HTTPS). The answer goes only to the program
#' that asked; it is not shown to the model or recorded in session events. Never log it.
#'
#' @field request_id Character. Request identifier (a cancellation carries the same id).
#' @field program Character. The program that asked, such as "sudo", "ssh", or "git".
#' @field prompt Character. Prompt text supplied by the program.
#' @field input_kind Character. "secret" (mask input), "text", or "confirm" (answer "yes").
#' @field command Character. Shell command that triggered the prompt.
#' @field cwd Character. Working directory of the command.
#' @export
ShellAuthenticationRequest <- R6::R6Class(
  "ShellAuthenticationRequest",
  public = list(
    request_id = NULL,
    program = NULL,
    prompt = NULL,
    input_kind = NULL,
    command = NULL,
    cwd = NULL,

    #' @description Create a new ShellAuthenticationRequest.
    #' @param request_id Character.
    #' @param program Character.
    #' @param prompt Character.
    #' @param input_kind Character.
    #' @param command Character.
    #' @param cwd Character.
    initialize = function(request_id, program, prompt, input_kind, command = "", cwd = "") {
      self$request_id <- request_id
      self$program <- program
      self$prompt <- prompt
      self$input_kind <- input_kind
      self$command <- command
      self$cwd <- cwd
    }
  )
)

#' StoredMemory
#'
#' A memory the agent asked to store (after runtime validation, secret filtering, and approval),
#' passed to a session's \code{store_memory} callback.
#'
#' @field session_id Character. Session that stored the memory.
#' @field subject Character. Short topic of the memory.
#' @field fact Character. The fact to remember.
#' @field citations Character. Sources supporting the fact.
#' @field reason Character. Why the fact is worth remembering.
#' @export
StoredMemory <- R6::R6Class(
  "StoredMemory",
  public = list(
    session_id = NULL,
    subject = NULL,
    fact = NULL,
    citations = NULL,
    reason = NULL,

    #' @description Create a new StoredMemory.
    #' @param session_id Character.
    #' @param subject Character.
    #' @param fact Character.
    #' @param citations Character.
    #' @param reason Character.
    initialize = function(session_id = NULL, subject = NULL, fact = NULL, citations = NULL,
                          reason = NULL) {
      self$session_id <- session_id
      self$subject <- subject
      self$fact <- fact
      self$citations <- citations
      self$reason <- reason
    }
  )
)

#' PolicyHelperFailure
#'
#' Structured Policy Helper failure reported by the runtime, either in the \code{data} of a failed
#' session.create / session.resume (available as \code{e$policy_helper_failure} on the error) or on
#' a managed settings snapshot. Helper-authored text is untrusted: render it as plain text.
#'
#' @field category Character. Runtime classification (for example "timeout" or "spawn").
#' @field failure_mode Character. "ignore" or "failClosed".
#' @field message Character. Primary plain-text explanation.
#' @field previous_policy_retained Logical. Whether an earlier helper layer still applies.
#' @field reason Character. Invocation reason (for example "sessionStart").
#' @field severity Character or NULL. "warning" or "error".
#' @field authority Character or NULL. Managed authority that registered the helper.
#' @field code Character or NULL. Helper-defined error identifier.
#' @field details List or NULL. Field-specific findings (\code{message}, optional \code{path}).
#' @field diagnostic Character or NULL. Secondary troubleshooting text.
#' @field help_link Named list or NULL. Validated link (\code{url}, optional \code{label}).
#' @field remediation Character or NULL. Recovery guidance (never execute it automatically).
#' @export
PolicyHelperFailure <- R6::R6Class(
  "PolicyHelperFailure",
  public = list(
    category = NULL,
    failure_mode = NULL,
    message = NULL,
    previous_policy_retained = FALSE,
    reason = NULL,
    severity = NULL,
    authority = NULL,
    code = NULL,
    details = NULL,
    diagnostic = NULL,
    help_link = NULL,
    remediation = NULL,

    #' @description Create a new PolicyHelperFailure.
    #' @param category Character.
    #' @param failure_mode Character.
    #' @param message Character.
    #' @param previous_policy_retained Logical.
    #' @param reason Character.
    #' @param severity Character or NULL.
    #' @param authority Character or NULL.
    #' @param code Character or NULL.
    #' @param details List or NULL.
    #' @param diagnostic Character or NULL.
    #' @param help_link Named list or NULL.
    #' @param remediation Character or NULL.
    initialize = function(category = NULL, failure_mode = NULL, message = NULL,
                          previous_policy_retained = FALSE, reason = NULL, severity = NULL,
                          authority = NULL, code = NULL, details = NULL, diagnostic = NULL,
                          help_link = NULL, remediation = NULL) {
      self$category <- category
      self$failure_mode <- failure_mode
      self$message <- message
      self$previous_policy_retained <- isTRUE(previous_policy_retained)
      self$reason <- reason
      self$severity <- severity
      self$authority <- authority
      self$code <- code
      self$details <- details
      self$diagnostic <- diagnostic
      self$help_link <- help_link
      self$remediation <- remediation
    }
  )
)

#' Parse a \code{policyHelperFailure} wire object.
#'
#' @param obj Named list (parsed JSON) or NULL.
#' @return A PolicyHelperFailure, or NULL when \code{obj} is not an object.
#' @export
policy_helper_failure_from_list <- function(obj) {
  if (!is.list(obj) || is.null(names(obj))) return(NULL)
  PolicyHelperFailure$new(
    category = obj[["category"]],
    failure_mode = obj[["failureMode"]],
    message = obj[["message"]],
    previous_policy_retained = obj[["previousPolicyRetained"]],
    reason = obj[["reason"]],
    severity = obj[["severity"]],
    authority = obj[["authority"]],
    code = obj[["code"]],
    details = obj[["details"]],
    diagnostic = obj[["diagnostic"]],
    help_link = obj[["helpLink"]],
    remediation = obj[["remediation"]]
  )
}

#' Attach the PolicyHelperFailure carried in a JSON-RPC error's \code{data} (as
#' \code{policy_helper_failure}) so callers of create/resume can inspect it.
#' @keywords internal
with_policy_helper_failure <- function(e) {
  data <- if (inherits(e, "json_rpc_error")) e$data else NULL
  if (is.list(data)) {
    failure <- policy_helper_failure_from_list(data[["policyHelperFailure"]])
    if (!is.null(failure)) e$policy_helper_failure <- failure
  }
  e
}

#' Map a permission handler result to an SDK protocol 4 permission decision
#'
#' Protocol 4 kinds ("approve-once", "approve-for-session", "approve-permanently", "reject",
#' "user-not-available") are sent as given. Earlier kinds are mapped: "approved" to
#' "approve-once", "denied-interactively-by-user" to "reject", and the other "denied-*"
#' kinds to "user-not-available". \code{TRUE} / \code{FALSE} approve once or reject.
#' "no-result" returns \code{NULL}, leaving the request pending for another client.
#'
#' @param result Handler result: a list with \code{kind}, a single string, or a logical.
#' @return Named list or NULL.
#' @keywords internal
permission_decision_to_v4 <- function(result) {
  if (isTRUE(result)) return(list(kind = "approve-once"))
  if (isFALSE(result)) return(list(kind = "reject"))
  if (is.character(result) && length(result) == 1L) result <- list(kind = result)
  if (!is.list(result) || !is.character(result[["kind"]])) {
    return(list(kind = "user-not-available"))
  }
  kind <- result[["kind"]]
  if (identical(kind, "no-result")) return(NULL)
  legacy <- c(
    "approved" = "approve-once",
    "denied-interactively-by-user" = "reject",
    "denied-by-rules" = "user-not-available",
    "denied-no-approval-rule-and-could-not-request-from-user" = "user-not-available",
    "denied-by-content-exclusion-policy" = "user-not-available"
  )
  if (kind %in% names(legacy)) {
    return(list(kind = legacy[[kind]]))
  }
  result
}

#' PermissionModeRefusalReason
#'
#' Known values of \code{PermissionModeSetResult$reason}, explaining why a requested permission
#' mode was refused.
#'
#' @export
PermissionModeRefusalReason <- list(
  MANAGED_POLICY = "managed_policy"
)

#' PermissionModeSetResult
#'
#' Result of \code{session$set_permission_mode()}.
#'
#' @field success Logical. Whether the requested mode was applied.
#' @field mode Character. Authoritative permission mode after the request.
#' @field reason Character or NULL. Why the mode was refused when known (for example
#'   \code{PermissionModeRefusalReason$MANAGED_POLICY}).
#' @export
PermissionModeSetResult <- R6::R6Class(
  "PermissionModeSetResult",
  public = list(
    success = FALSE,
    mode = NULL,
    reason = NULL,

    #' @description Create a new PermissionModeSetResult.
    #' @param success Logical.
    #' @param mode Character.
    #' @param reason Character or NULL.
    initialize = function(success = FALSE, mode = NULL, reason = NULL) {
      self$success <- isTRUE(success)
      self$mode <- mode
      self$reason <- reason
    }
  )
)

#' McpSecretsSaveResult
#'
#' Result of \code{client$mcp_secrets_save()}.
#'
#' @field used_keychain Logical. TRUE when the OS keychain stored the credential, FALSE for the
#'   file backend.
#' @export
McpSecretsSaveResult <- R6::R6Class(
  "McpSecretsSaveResult",
  public = list(
    used_keychain = FALSE,

    #' @description Create a new McpSecretsSaveResult.
    #' @param used_keychain Logical.
    initialize = function(used_keychain = FALSE) {
      self$used_keychain <- isTRUE(used_keychain)
    }
  )
)

#' PermissionsExtensionApproval
#'
#' A user-scoped approval for one installed plugin extension.
#'
#' @field extension_name Character. Stable extension identity (for example
#'   "plugin:plugin-name:extension-name").
#' @field plugin_name Character. Plugin that contributed the extension.
#' @field capabilities Character vector. Exact privileged capabilities the user approved.
#' @field granted_at Character. RFC 3339 timestamp of the grant.
#' @field installation_key Character. Opaque installation identity; pass it back unchanged to
#'   \code{client$extension_approvals_revoke()}.
#' @field installation_label Character or NULL. Human-readable installation source label.
#' @export
PermissionsExtensionApproval <- R6::R6Class(
  "PermissionsExtensionApproval",
  public = list(
    extension_name = NULL,
    plugin_name = NULL,
    capabilities = NULL,
    granted_at = NULL,
    installation_key = NULL,
    installation_label = NULL,

    #' @description Create a new PermissionsExtensionApproval.
    #' @param extension_name Character.
    #' @param plugin_name Character.
    #' @param capabilities Character vector.
    #' @param granted_at Character.
    #' @param installation_key Character.
    #' @param installation_label Character or NULL.
    initialize = function(extension_name = NULL, plugin_name = NULL, capabilities = character(0),
                          granted_at = NULL, installation_key = NULL, installation_label = NULL) {
      self$extension_name <- extension_name
      self$plugin_name <- plugin_name
      self$capabilities <- capabilities
      self$granted_at <- granted_at
      self$installation_key <- installation_key
      self$installation_label <- installation_label
    }
  )
)

#' Parse a \code{PermissionsExtensionApproval} wire object.
#' @param obj Named list (parsed JSON) or NULL.
#' @return A PermissionsExtensionApproval, or NULL when \code{obj} is not an object.
#' @keywords internal
permissions_extension_approval_from_list <- function(obj) {
  if (!is.list(obj) || is.null(names(obj))) return(NULL)
  PermissionsExtensionApproval$new(
    extension_name = obj[["extensionName"]],
    plugin_name = obj[["pluginName"]],
    capabilities = as.character(unlist(obj[["capabilities"]])),
    granted_at = obj[["grantedAt"]],
    installation_key = obj[["installationKey"]],
    installation_label = obj[["installationLabel"]]
  )
}

#' PermissionsExtensionApprovalsGrantResult
#'
#' Result of \code{client$extension_approvals_grant()}.
#'
#' @field granted Logical. Whether the pending request was eligible and its approval persisted.
#' @field approval PermissionsExtensionApproval or NULL. The persisted approval when granted.
#' @export
PermissionsExtensionApprovalsGrantResult <- R6::R6Class(
  "PermissionsExtensionApprovalsGrantResult",
  public = list(
    granted = FALSE,
    approval = NULL,

    #' @description Create a new PermissionsExtensionApprovalsGrantResult.
    #' @param granted Logical.
    #' @param approval PermissionsExtensionApproval or NULL.
    initialize = function(granted = FALSE, approval = NULL) {
      self$granted <- isTRUE(granted)
      self$approval <- approval
    }
  )
)

#' PermissionsExtensionApprovalsRevokeResult
#'
#' Result of \code{client$extension_approvals_revoke()}.
#'
#' @field revoked Logical. Whether a matching approval existed and was removed.
#' @export
PermissionsExtensionApprovalsRevokeResult <- R6::R6Class(
  "PermissionsExtensionApprovalsRevokeResult",
  public = list(
    revoked = FALSE,

    #' @description Create a new PermissionsExtensionApprovalsRevokeResult.
    #' @param revoked Logical.
    initialize = function(revoked = FALSE) {
      self$revoked <- isTRUE(revoked)
    }
  )
)

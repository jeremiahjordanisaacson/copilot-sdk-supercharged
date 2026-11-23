#' CopilotSession
#'
#' Represents a single conversation session with the Copilot CLI.
#'
#' @description
#' A session maintains conversation state, handles events, and manages tool execution.
#' Sessions are created via \code{CopilotClient$create_session()} or resumed via
#' \code{CopilotClient$resume_session()}.
#'
#' @export
CopilotSession <- R6::R6Class(
  "CopilotSession",
  public = list(
    #' @field session_id Character. The unique identifier for this session.
    session_id = NULL,

    #' @field workspace_path Character or NULL. Path to workspace directory
    #'   when infinite sessions are enabled.
    workspace_path = NULL,

    #' @description Create a new CopilotSession.
    #'
    #' Note: This constructor is internal. Use CopilotClient$create_session()
    #' to create sessions.
    #'
    #' @param session_id Character. Session identifier.
    #' @param client JsonRpcClient R6 object.
    #' @param workspace_path Character or NULL.
    initialize = function(session_id, client, workspace_path = NULL) {
      self$session_id <- session_id
      self$workspace_path <- workspace_path
      private$client <- client
      private$event_handlers <- list()
      private$tool_handlers <- list()
      private$permission_handler <- NULL
      private$user_input_handler <- NULL
      private$exit_plan_mode_handler <- NULL
      private$trace_context_provider <- NULL
      private$hooks <- NULL
      private$disconnect_callbacks <- list()
    },

    #' @description Send a message to this session.
    #'
    #' @param prompt Character. The prompt/message text to send.
    #' @param attachments List or NULL. Optional file/directory attachments.
    #' @param mode Character or NULL. "enqueue" or "immediate".
    #' @param agent_mode Character or NULL. Agent mode for this message.
    #' @param display_prompt Character or NULL. Alternate prompt text to display.
    #' @param request_headers Named list or NULL. Extra HTTP headers for this message.
    #' @param response_format Character or NULL. Response format ("text", "image", or "json_object").
    #' @param image_options List or NULL. Image generation options (from \code{image_options()}).
    #' @param source Character or NULL. Message source: \code{MessageSource$USER},
    #'   \code{MessageSource$SYSTEM}, or \code{MessageSource$agent(id)}.
    #' @param response_schema Named list or NULL. JSON Schema for a structured response; sent as
    #'   a \code{json_schema} \code{responseFormat} (takes precedence over \code{response_format}).
    #' @param client_turn_id Character or NULL. Opaque caller-chosen ID for this turn, echoed on
    #'   the turn's events so the application can correlate them.
    #'
    #' @return Character. The message ID of the response.
    send = function(prompt, attachments = NULL, mode = NULL,
                    agent_mode = NULL, display_prompt = NULL, request_headers = NULL,
                    response_format = NULL, image_options = NULL,
                    source = NULL, response_schema = NULL, client_turn_id = NULL) {
      agent_mode <- agent_mode %||% private$default_agent_mode
      display_prompt <- display_prompt %||% private$default_display_prompt
      payload <- list(
        sessionId = self$session_id,
        prompt = prompt
      )
      if (!is.null(source)) payload$source <- source
      if (!is.null(client_turn_id)) payload$clientTurnId <- client_turn_id
      if (!is.null(attachments)) payload$attachments <- attachments
      if (!is.null(mode)) payload$mode <- mode
      if (!is.null(agent_mode)) payload$agentMode <- agent_mode
      if (!is.null(display_prompt)) payload$displayPrompt <- display_prompt
      if (!is.null(request_headers)) payload$requestHeaders <- request_headers
      if (!is.null(response_format)) payload$responseFormat <- response_format
      if (!is.null(response_schema)) {
        payload$responseFormat <- structured_response_format(response_schema)
      }
      if (!is.null(image_options)) payload$imageOptions <- image_options

      response <- private$client$request("session.send", payload)
      response$messageId
    },

    #' @description Send a message and wait until the session becomes idle.
    #'
    #' This is a convenience method that sends a message and then polls for
    #' events until session.idle is received. The event handlers registered
    #' via \code{on()} are still called for each event during the wait.
    #'
    #' @param prompt Character. The prompt/message text.
    #' @param attachments List or NULL. Optional attachments.
    #' @param mode Character or NULL. Processing mode.
    #' @param response_format Character or NULL. Response format ("text", "image", or "json_object").
    #' @param image_options List or NULL. Image generation options (from \code{image_options()}).
    #' @param timeout Numeric. Timeout in seconds (default 60).
    #' @param source Character or NULL. Message source (see \code{MessageSource}).
    #' @param response_schema Named list or NULL. JSON Schema for a structured response.
    #' @param client_turn_id Character or NULL. Opaque caller-chosen ID for this turn.
    #'
    #' @return The last assistant.message SessionEvent, or NULL if none received.
    send_and_wait = function(prompt, attachments = NULL, mode = NULL,
                             agent_mode = NULL, display_prompt = NULL,
                             request_headers = NULL,
                             response_format = NULL, image_options = NULL,
                             timeout = 60, source = NULL, response_schema = NULL,
                             client_turn_id = NULL) {
      last_assistant_message <- NULL
      error_event <- NULL
      idle_received <- FALSE

      # Temporary event handler to track idle and messages
      temp_handler <- function(event) {
        if (identical(event$type, "assistant.message")) {
          last_assistant_message <<- event
        } else if (identical(event$type, "session.idle")) {
          idle_received <<- TRUE
        } else if (identical(event$type, "session.error")) {
          error_msg <- if (!is.null(event$data$message)) event$data$message else "Unknown error"
          error_event <<- paste0("Session error: ", error_msg)
          idle_received <<- TRUE
        }
      }

      unsubscribe <- self$on(temp_handler)
      on.exit(unsubscribe(), add = TRUE)

      # Send the message
      self$send(prompt = prompt, attachments = attachments, mode = mode,
                agent_mode = agent_mode, display_prompt = display_prompt,
                request_headers = request_headers,
                response_format = response_format, image_options = image_options,
                source = source, response_schema = response_schema,
                client_turn_id = client_turn_id)

      # Poll until idle or timeout
      start_time <- proc.time()[["elapsed"]]
      while (!idle_received) {
        elapsed <- proc.time()[["elapsed"]] - start_time
        if (elapsed > timeout) {
          stop(paste0("Timeout after ", timeout, "s waiting for session.idle"))
        }
        # Poll processes incoming messages (notifications, requests)
        private$client$poll(timeout_ms = 500L)
      }

      if (!is.null(error_event)) {
        stop(error_event)
      }

      last_assistant_message
    },

    #' @description Send a message with a JSON Schema and return the parsed structured result.
    #'
    #' Sets \code{response_schema}, waits via \code{send_and_wait()}, and parses the final
    #' assistant message content as JSON.
    #'
    #' @param prompt Character. The prompt/message text.
    #' @param response_schema Named list. JSON Schema describing the expected result
    #'   (use \code{list(...)} for JSON arrays such as \code{required}).
    #' @param attachments List or NULL. Optional attachments.
    #' @param mode Character or NULL. Processing mode.
    #' @param agent_mode Character or NULL. Agent mode for this message.
    #' @param display_prompt Character or NULL. Alternate prompt text to display.
    #' @param request_headers Named list or NULL. Extra HTTP headers for this message.
    #' @param source Character or NULL. Message source (see \code{MessageSource}).
    #' @param timeout Numeric. Timeout in seconds (default 60).
    #' @param client_turn_id Character or NULL. Opaque caller-chosen ID for this turn.
    #'
    #' @return The parsed JSON result (named list / list).
    send_and_wait_structured = function(prompt, response_schema, attachments = NULL,
                                        mode = NULL, agent_mode = NULL,
                                        display_prompt = NULL, request_headers = NULL,
                                        source = NULL, timeout = 60, client_turn_id = NULL) {
      if (is.null(response_schema)) stop("response_schema is required")
      event <- self$send_and_wait(
        prompt = prompt, attachments = attachments, mode = mode,
        agent_mode = agent_mode, display_prompt = display_prompt,
        request_headers = request_headers, timeout = timeout,
        source = source, response_schema = response_schema,
        client_turn_id = client_turn_id
      )
      content <- if (is.null(event)) NULL else event$data[["content"]]
      if (!is.character(content) || length(content) != 1 || !nzchar(trimws(content))) {
        stop("The requested run completed without a structured assistant response")
      }
      jsonlite::fromJSON(content, simplifyVector = FALSE)
    },

    #' @description Subscribe to events from this session.
    #'
    #' @param handler Function(event). Called with each SessionEvent.
    #'
    #' @return A function that when called, unsubscribes the handler.
    on = function(handler) {
      private$event_handlers <- c(private$event_handlers, list(handler))
      local_handler <- handler
      function() {
        private$event_handlers <- Filter(
          function(h) !identical(h, local_handler),
          private$event_handlers
        )
      }
    },

    #' @description Dispatch an event to all registered handlers.
    #' @param event SessionEvent R6 object.
    #' @keywords internal
    dispatch_event = function(event) {
      # Handle elicitation.requested events internally
      if (identical(event$type, "elicitation.requested") &&
          !is.null(private$elicitation_handler)) {
        private$handle_elicitation_request(event$data)
      }

      for (handler in private$event_handlers) {
        tryCatch(
          handler(event),
          error = function(e) {
            message(paste("Error in session event handler:", e$message))
          }
        )
      }
    },

    #' @description Register custom tool handlers.
    #' @param tools List of Tool R6 objects or NULL.
    #' @keywords internal
    register_tools = function(tools) {
      private$tool_handlers <- list()
      if (is.null(tools)) return(invisible(NULL))
      for (tool in tools) {
        if (!is.null(tool$name) && !is.null(tool$handler)) {
          private$tool_handlers[[tool$name]] <- tool$handler
        }
      }
    },

    #' @description Get a registered tool handler by name.
    #' @param name Character. Tool name.
    #' @return Function or NULL.
    #' @keywords internal
    get_tool_handler = function(name) {
      private$tool_handlers[[name]]
    },

    #' @description Register a permission request handler.
    #' @param handler Function or NULL.
    #' @keywords internal
    register_permission_handler = function(handler) {
      private$permission_handler <- handler
    },

    #' @description Whether a permission request handler is registered.
    #' @return Logical.
    #' @keywords internal
    has_permission_handler = function() {
      !is.null(private$permission_handler)
    },

    #' @description Handle a permission request from the CLI.
    #' @param request Named list. The permission request data.
    #' @return Named list with permission decision.
    #' @keywords internal
    handle_permission_request = function(request) {
      if (is.null(private$permission_handler)) {
        return(list(kind = "denied-no-approval-rule-and-could-not-request-from-user"))
      }

      tryCatch(
        {
          result <- private$permission_handler(
            request,
            list(session_id = self$session_id)
          )
          if (inherits(result, "PermissionRequestResult")) {
            result$to_list()
          } else {
            result
          }
        },
        error = function(e) {
          list(kind = "denied-no-approval-rule-and-could-not-request-from-user")
        }
      )
    },

    #' @description Register a user input handler.
    #' @param handler Function or NULL.
    #' @keywords internal
    register_user_input_handler = function(handler) {
      private$user_input_handler <- handler
    },

    #' @description Handle a user input request from the CLI.
    #' @param request Named list. The user input request data.
    #' @return Named list with answer and was_freeform.
    #' @keywords internal
    handle_user_input_request = function(request) {
      if (is.null(private$user_input_handler)) {
        stop("User input requested but no handler registered")
      }

      input_req <- UserInputRequest$new(
        question = request$question %||% "",
        choices = request$choices,
        allow_freeform = if (!is.null(request$allowFreeform)) request$allowFreeform else TRUE
      )

      result <- private$user_input_handler(
        input_req,
        list(session_id = self$session_id)
      )

      if (inherits(result, "UserInputResponse")) {
        list(answer = result$answer, was_freeform = result$was_freeform)
      } else {
        list(answer = result$answer, was_freeform = isTRUE(result$wasFreeform %||% result$was_freeform))
      }
    },

    #' @description Register hook handlers.
    #' @param hooks Named list of hook handler functions.
    #' @keywords internal
    register_hooks = function(hooks) {
      private$hooks <- hooks
    },

    #' @description Register an elicitation request handler.
    #' @param handler Function or NULL. Called with (context, request_id).
    #' @keywords internal
    register_elicitation_handler = function(handler) {
      private$elicitation_handler <- handler
    },

    #' @description Register an exit-plan-mode request handler.
    #' @param handler Function or NULL. Called with ExitPlanModeRequest.
    #' @keywords internal
    register_exit_plan_mode_handler = function(handler) {
      private$exit_plan_mode_handler <- handler
    },

    #' @description Register a shell authentication request handler.
    #' @param handler Function or NULL. Called with (ShellAuthenticationRequest, context).
    #' @keywords internal
    register_shell_authentication_handler = function(handler) {
      private$shell_authentication_handler <- handler
    },

    #' @description Answer a \code{shellAuthentication.request} from the CLI.
    #'
    #' Handler failures and any answer other than an accepted string become a cancellation.
    #' The answer is never logged.
    #' @param params Named list. The request payload.
    #' @return Named list: \code{list(action = "accept", value = ...)} or
    #'   \code{list(action = "cancel")}.
    #' @keywords internal
    handle_shell_authentication_request = function(params) {
      cancel <- list(action = "cancel")
      handler <- private$shell_authentication_handler
      if (is.null(handler)) return(cancel)
      request <- ShellAuthenticationRequest$new(
        request_id = params$requestId,
        program = params$program,
        prompt = params$prompt,
        input_kind = params$inputKind,
        command = if (is.character(params$command)) params$command else "",
        cwd = if (is.character(params$cwd)) params$cwd else ""
      )
      result <- tryCatch(
        handler(request, list(session_id = self$session_id)),
        error = function(e) NULL
      )
      result <- config_to_wire(result)
      if (!is.list(result) || !identical(result[["action"]], "accept")) return(cancel)
      value <- result[["value"]]
      if (!is.character(value) || length(value) != 1 || is.na(value)) return(cancel)
      list(action = "accept", value = value)
    },

    #' @description Register a memory store callback.
    #' @param handler Function or NULL. Called with (StoredMemory, context).
    #' @keywords internal
    register_memory_store = function(handler) {
      private$memory_store_handler <- handler
    },

    #' @description Handle a \code{memoryStore.store} request from the CLI.
    #' @param params Named list. The stored memory payload.
    #' @keywords internal
    handle_memory_store = function(params) {
      handler <- private$memory_store_handler
      if (is.null(handler)) stop("No memory store is registered")
      memory <- StoredMemory$new(
        session_id = params$sessionId,
        subject = params$subject,
        fact = params$fact,
        citations = params$citations,
        reason = params$reason
      )
      tryCatch(
        handler(memory, list(session_id = self$session_id)),
        error = function(e) {
          warning(paste("Memory store callback failed:", conditionMessage(e)))
          stop("Memory store callback failed")
        }
      )
      invisible(NULL)
    },

    #' @description Handle an exit-plan-mode request from the CLI.
    #' @param params Named list. The exit-plan-mode request data.
    #' @return Named list with approval decision.
    #' @keywords internal
    handle_exit_plan_mode_request = function(params) {
      if (is.null(private$exit_plan_mode_handler)) {
        return(list(approved = TRUE))
      }
      request <- ExitPlanModeRequest$new(
        summary = params$summary %||% "",
        actions = params$actions %||% character(0),
        recommended_action = params$recommendedAction %||% "",
        plan_content = params$planContent
      )
      result <- private$exit_plan_mode_handler(request)
      if (inherits(result, "ExitPlanModeResult")) {
        return(result$to_list())
      }
      return(result)
    },

    #' @description Register a trace context provider.
    #' @param provider Function or NULL. Called with no arguments, returns TraceContext or list.
    #' @keywords internal
    register_trace_context_provider = function(provider) {
      private$trace_context_provider <- provider
    },

    #' @description Register slash commands for this session.
    #' @param commands List of CommandDefinition objects or NULL.
    #' @keywords internal
    register_commands = function(commands) {
      private$commands <- commands
    },

    #' @description Set default message-send options applied to subsequent sends.
    #' @param agent_mode Character or NULL. Default agent mode.
    #' @param display_prompt Character or NULL. Default display prompt.
    #' @keywords internal
    set_default_message_options = function(agent_mode = NULL, display_prompt = NULL) {
      if (!is.null(agent_mode)) private$default_agent_mode <- agent_mode
      if (!is.null(display_prompt)) private$default_display_prompt <- display_prompt
      invisible(NULL)
    },

    #' @description Handle a hooks invocation from the CLI.
    #' @param hook_type Character. Hook type.
    #' @param input_data Any. Hook input.
    #' @return Hook output or NULL.
    #' @keywords internal
    handle_hooks_invoke = function(hook_type, input_data) {
      if (is.null(private$hooks)) return(NULL)

      handler_map <- list(
        preToolUse = private$hooks$on_pre_tool_use,
        postToolUse = private$hooks$on_post_tool_use,
        preMcpToolCall = private$hooks$on_pre_mcp_tool_call,
        userPromptSubmitted = private$hooks$on_user_prompt_submitted,
        userPromptTransformed = private$hooks$on_user_prompt_transformed,
        sessionStart = private$hooks$on_session_start,
        sessionEnd = private$hooks$on_session_end,
        errorOccurred = private$hooks$on_error_occurred,
        subagentStart = private$hooks$on_subagent_start,
        subagentStop = private$hooks$on_subagent_stop
      )

      handler <- handler_map[[hook_type]]
      if (is.null(handler)) return(NULL)

      tryCatch(
        handler(input_data, list(session_id = self$session_id)),
        error = function(e) NULL
      )
    },

    #' @description Retrieve metadata for this session.
    #' @return A named list containing the session metadata.
    get_metadata = function() {
      private$client$request(
        "session.getMetadata",
        list(sessionId = self$session_id)
      )
    },

    #' @description Retrieve all events/messages from this session's history.
    #' @return A list of SessionEvent R6 objects.
    get_messages = function() {
      response <- private$client$request(
        "session.getMessages",
        list(sessionId = self$session_id)
      )
      events_dicts <- response$events %||% list()
      lapply(events_dicts, session_event_from_list)
    },

    #' @description Destroy this session and release resources.
    #'
    #' Sends \code{session.detach} (falling back to \code{session.destroy} on CLIs that
    #' do not implement it). Session state on disk is preserved for later resume.
    destroy = function() {
      params <- list(sessionId = self$session_id)
      tryCatch(
        private$client$request("session.detach", params),
        json_rpc_error = function(e) {
          if (!identical(as.integer(e$code), -32601L)) stop(e)
          private$client$request("session.destroy", params)
        }
      )
      private$event_handlers <- list()
      private$tool_handlers <- list()
      private$permission_handler <- NULL
      private$user_input_handler <- NULL
      private$hooks <- NULL
      private$shell_authentication_handler <- NULL
      private$memory_store_handler <- NULL
      callbacks <- private$disconnect_callbacks
      private$disconnect_callbacks <- list()
      for (callback in callbacks) {
        tryCatch(callback(), error = function(e) NULL)
      }
      invisible(NULL)
    },

    #' @description Register a callback run once when this session is destroyed.
    #' @param callback Function with no arguments.
    #' @keywords internal
    register_disconnect_callback = function(callback) {
      private$disconnect_callbacks <- c(private$disconnect_callbacks, list(callback))
      invisible(NULL)
    },

    #' @description Change the experimental Auto routing preference without changing the model.
    #' @param auto_tier Character ("efficiency", "balance", "intelligence", "fast") or NULL to
    #'   return to the provider's default Auto routing (sent as JSON null).
    #' @return Named list (status, effectiveAutoTier, pendingAutoTier, activatingAutoTier,
    #'   supersededAutoTier).
    set_auto_tier = function(auto_tier) {
      private$client$request(
        "session.model.switchAutoTier",
        list(sessionId = self$session_id, autoTier = auto_tier)
      )
    },

    #' @description Get the managed (enterprise) settings resolved for this session.
    #' @return Named list with the resolved managed settings.
    get_managed_settings = function() {
      private$client$request(
        "session.managedSettings.get",
        list(sessionId = self$session_id)
      )
    },

    #' @description Re-run the managed-settings policy helper for this session.
    #' @return Named list with the refreshed managed settings.
    recheck_policy_helper = function() {
      private$client$request(
        "session.managedSettings.recheckPolicyHelper",
        list(sessionId = self$session_id)
      )
    },

    #' @description Change the session's permission mode.
    #' @param mode Character. Permission mode to apply.
    #' @return A PermissionModeSetResult. When the runtime refuses the mode, \code{success} is
    #'   FALSE and \code{reason} explains why when known (for example
    #'   \code{PermissionModeRefusalReason$MANAGED_POLICY}).
    set_permission_mode = function(mode) {
      result <- private$client$request(
        "session.permissions.setMode",
        list(sessionId = self$session_id, mode = mode)
      )
      PermissionModeSetResult$new(
        success = result$success,
        mode = result$mode,
        reason = result$reason
      )
    },

    #' @description Abort the currently processing message.
    abort = function() {
      private$client$request("session.abort", list(sessionId = self$session_id))
      invisible(NULL)
    }
  ),

  private = list(
    client = NULL,
    event_handlers = NULL,
    tool_handlers = NULL,
    permission_handler = NULL,
    user_input_handler = NULL,
    exit_plan_mode_handler = NULL,
    trace_context_provider = NULL,
    hooks = NULL,
    elicitation_handler = NULL,
    commands = NULL,
    default_agent_mode = NULL,
    default_display_prompt = NULL,
    disconnect_callbacks = NULL,
    shell_authentication_handler = NULL,
    memory_store_handler = NULL,

    handle_elicitation_request = function(data) {
      request_id <- data$requestId
      context <- list(
        session_id = self$session_id,
        message = data$message,
        requested_schema = data$requestedSchema,
        mode = data$mode,
        elicitation_source = data$elicitationSource,
        url = data$url
      )

      tryCatch(
        {
          result <- private$elicitation_handler(context)
          private$client$request("ui.handlePendingElicitation", list(
            requestId = request_id,
            result = result
          ))
        },
        error = function(e) {
          tryCatch(
            private$client$request("ui.handlePendingElicitation", list(
              requestId = request_id,
              result = list(action = "cancel")
            )),
            error = function(e2) NULL
          )
        }
      )
    }
  )
)

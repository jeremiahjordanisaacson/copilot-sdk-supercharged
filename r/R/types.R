#' Type definitions for the Copilot R SDK
#'
#' R6 classes representing the data types used in the Copilot SDK protocol.
#' These mirror the types in the Python/Node.js/Go/.NET SDKs.

# ---------------------------------------------------------------------------
# SessionEvent
# ---------------------------------------------------------------------------

#' SessionEvent
#'
#' Represents an event emitted by a Copilot session.
#'
#' @field type Character. The event type (e.g. "assistant.message", "session.idle").
#' @field id Character. UUID of the event.
#' @field timestamp Character. ISO 8601 timestamp.
#' @field data List. Event-specific data payload.
#' @field ephemeral Logical or NULL. Whether this event is ephemeral.
#' @field parent_id Character or NULL. Parent event UUID.
#' @export
SessionEvent <- R6::R6Class(
  "SessionEvent",
  public = list(
    type = NULL,
    id = NULL,
    timestamp = NULL,
    data = NULL,
    ephemeral = NULL,
    parent_id = NULL,

    #' @description Create a new SessionEvent.
    #' @param type Event type string.
    #' @param id Event UUID string.
    #' @param timestamp ISO 8601 timestamp string.
    #' @param data Event data as a named list.
    #' @param ephemeral Logical or NULL.
    #' @param parent_id Parent event UUID string or NULL.
    initialize = function(type, id, timestamp, data, ephemeral = NULL, parent_id = NULL) {
      self$type <- type
      self$id <- id
      self$timestamp <- timestamp
      self$data <- data
      self$ephemeral <- ephemeral
      self$parent_id <- parent_id
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list(
        type = self$type,
        id = self$id,
        timestamp = self$timestamp,
        data = self$data
      )
      if (!is.null(self$ephemeral)) result$ephemeral <- self$ephemeral
      if (!is.null(self$parent_id)) result$parentId <- self$parent_id
      result
    }
  )
)

#' Create a SessionEvent from a named list (parsed JSON).
#'
#' @param obj A named list from parsed JSON.
#' @return A SessionEvent R6 object.
#' @keywords internal
session_event_from_list <- function(obj) {
  SessionEvent$new(
    type = obj$type %||% "unknown",
    id = obj$id %||% "",
    timestamp = obj$timestamp %||% "",
    data = obj$data %||% list(),
    ephemeral = obj$ephemeral,
    parent_id = obj$parentId
  )
}


# ---------------------------------------------------------------------------
# PermissionRequest / PermissionRequestResult
# ---------------------------------------------------------------------------

#' PermissionRequest
#'
#' Represents a permission request from the server.
#'
#' @field kind Character. The kind of permission ("shell", "write", "mcp", "read", "url").
#' @field tool_call_id Character. The tool call ID.
#' @field extra List. Additional fields depending on kind.
#' @export
PermissionRequest <- R6::R6Class(
  "PermissionRequest",
  public = list(
    kind = NULL,
    tool_call_id = NULL,
    extra = NULL,

    #' @description Create a new PermissionRequest.
    #' @param kind Permission kind string.
    #' @param tool_call_id Tool call ID string.
    #' @param extra Named list of additional fields.
    initialize = function(kind = NULL, tool_call_id = NULL, extra = list()) {
      self$kind <- kind
      self$tool_call_id <- tool_call_id
      self$extra <- extra
    },

    #' @description Convert to list.
    to_list = function() {
      result <- self$extra
      if (!is.null(self$kind)) result$kind <- self$kind
      if (!is.null(self$tool_call_id)) result$toolCallId <- self$tool_call_id
      result
    }
  )
)

#' PermissionRequestResult
#'
#' Result of a permission request.
#'
#' @field kind Character. Decision kind.
#' @field rules List or NULL. Associated rules.
#' @export
PermissionRequestResult <- R6::R6Class(
  "PermissionRequestResult",
  public = list(
    kind = NULL,
    rules = NULL,

    #' @description Create a new PermissionRequestResult.
    #' @param kind Decision kind string.
    #' @param rules List or NULL.
    initialize = function(kind, rules = NULL) {
      self$kind <- kind
      self$rules <- rules
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(kind = self$kind)
      if (!is.null(self$rules)) result$rules <- self$rules
      result
    }
  )
)


# ---------------------------------------------------------------------------
# UserInputRequest / UserInputResponse
# ---------------------------------------------------------------------------

#' UserInputRequest
#'
#' Request for user input from the agent (enables ask_user tool).
#'
#' @field question Character. The question to ask.
#' @field choices Character vector or NULL. Optional list of choices.
#' @field allow_freeform Logical. Whether freeform input is allowed.
#' @export
UserInputRequest <- R6::R6Class(
  "UserInputRequest",
  public = list(
    question = NULL,
    choices = NULL,
    allow_freeform = TRUE,

    #' @description Create a new UserInputRequest.
    #' @param question The question string.
    #' @param choices Character vector or NULL.
    #' @param allow_freeform Logical, default TRUE.
    initialize = function(question = "", choices = NULL, allow_freeform = TRUE) {
      self$question <- question
      self$choices <- choices
      self$allow_freeform <- allow_freeform
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(question = self$question, allowFreeform = self$allow_freeform)
      if (!is.null(self$choices)) result$choices <- self$choices
      result
    }
  )
)

#' UserInputResponse
#'
#' Response to a user input request.
#'
#' @field answer Character. The answer text.
#' @field was_freeform Logical. Whether the answer was freeform.
#' @export
UserInputResponse <- R6::R6Class(
  "UserInputResponse",
  public = list(
    answer = NULL,
    was_freeform = FALSE,

    #' @description Create a new UserInputResponse.
    #' @param answer Answer string.
    #' @param was_freeform Logical.
    initialize = function(answer, was_freeform = FALSE) {
      self$answer <- answer
      self$was_freeform <- was_freeform
    },

    #' @description Convert to list.
    to_list = function() {
      list(answer = self$answer, wasFreeform = self$was_freeform)
    }
  )
)


# ---------------------------------------------------------------------------
# PingResponse
# ---------------------------------------------------------------------------

#' PingResponse
#'
#' Response from a ping request.
#'
#' @field message Character. Echo message with "pong: " prefix.
#' @field timestamp Numeric. Server timestamp in milliseconds.
#' @field protocol_version Integer. Protocol version for SDK compatibility.
#' @export
PingResponse <- R6::R6Class(
  "PingResponse",
  public = list(
    message = NULL,
    timestamp = NULL,
    protocol_version = NULL,

    #' @description Create a new PingResponse.
    #' @param message Character.
    #' @param timestamp Numeric.
    #' @param protocol_version Integer.
    initialize = function(message, timestamp, protocol_version) {
      self$message <- message
      self$timestamp <- timestamp
      self$protocol_version <- as.integer(protocol_version)
    },

    #' @description Convert to list.
    to_list = function() {
      list(
        message = self$message,
        timestamp = self$timestamp,
        protocolVersion = self$protocol_version
      )
    }
  )
)

#' Create a PingResponse from a named list.
#' @param obj Named list from parsed JSON.
#' @return PingResponse R6 object.
#' @keywords internal
ping_response_from_list <- function(obj) {
  PingResponse$new(
    message = obj$message,
    timestamp = obj$timestamp,
    protocol_version = obj$protocolVersion
  )
}


# ---------------------------------------------------------------------------
# Model types
# ---------------------------------------------------------------------------

#' ModelSupports
#'
#' Model support flags.
#'
#' @field vision Logical. Whether vision is supported.
#' @field reasoning_effort Logical. Whether reasoning effort is supported.
#' @export
ModelSupports <- R6::R6Class(
  "ModelSupports",
  public = list(
    vision = FALSE,
    reasoning_effort = FALSE,

    #' @description Create a new ModelSupports.
    #' @param vision Logical.
    #' @param reasoning_effort Logical.
    initialize = function(vision = FALSE, reasoning_effort = FALSE) {
      self$vision <- vision
      self$reasoning_effort <- reasoning_effort
    },

    #' @description Convert to list.
    to_list = function() {
      list(vision = self$vision, reasoningEffort = self$reasoning_effort)
    }
  )
)

#' ModelLimits
#'
#' Model limits.
#'
#' @field max_prompt_tokens Numeric or NULL.
#' @field max_context_window_tokens Numeric or NULL.
#' @export
ModelLimits <- R6::R6Class(
  "ModelLimits",
  public = list(
    max_prompt_tokens = NULL,
    max_context_window_tokens = NULL,

    #' @description Create a new ModelLimits.
    #' @param max_prompt_tokens Numeric or NULL.
    #' @param max_context_window_tokens Numeric or NULL.
    initialize = function(max_prompt_tokens = NULL, max_context_window_tokens = NULL) {
      self$max_prompt_tokens <- max_prompt_tokens
      self$max_context_window_tokens <- max_context_window_tokens
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list()
      if (!is.null(self$max_prompt_tokens)) result$max_prompt_tokens <- self$max_prompt_tokens
      if (!is.null(self$max_context_window_tokens)) {
        result$max_context_window_tokens <- self$max_context_window_tokens
      }
      result
    }
  )
)

#' ModelCapabilities
#'
#' Model capabilities and limits.
#'
#' @field supports ModelSupports R6 object.
#' @field limits ModelLimits R6 object.
#' @export
ModelCapabilities <- R6::R6Class(
  "ModelCapabilities",
  public = list(
    supports = NULL,
    limits = NULL,

    #' @description Create a new ModelCapabilities.
    #' @param supports ModelSupports R6 object.
    #' @param limits ModelLimits R6 object.
    initialize = function(supports, limits) {
      self$supports <- supports
      self$limits <- limits
    },

    #' @description Convert to list.
    to_list = function() {
      list(supports = self$supports$to_list(), limits = self$limits$to_list())
    }
  )
)

#' ModelPolicy
#'
#' Model policy state.
#'
#' @field state Character. "enabled", "disabled", or "unconfigured".
#' @field terms Character. Policy terms.
#' @export
ModelPolicy <- R6::R6Class(
  "ModelPolicy",
  public = list(
    state = NULL,
    terms = NULL,

    #' @description Create a new ModelPolicy.
    #' @param state Character.
    #' @param terms Character.
    initialize = function(state, terms) {
      self$state <- state
      self$terms <- terms
    },

    #' @description Convert to list.
    to_list = function() {
      list(state = self$state, terms = self$terms)
    }
  )
)
# ---------------------------------------------------------------------------
# SlashCommandInputCompletion (enum-like constants)
# ---------------------------------------------------------------------------

#' SlashCommandInputCompletion
#'
#' Constants for slash command input completion types.
#' @export
SlashCommandInputCompletion <- list(
  DIRECTORY = "directory"
)

# ---------------------------------------------------------------------------
# SlashCommandKind (enum-like constants)
# ---------------------------------------------------------------------------

#' SlashCommandKind
#'
#' Constants for slash command kinds.
#' @export
SlashCommandKind <- list(
  BUILTIN = "builtin",
  CLIENT  = "client",
  SKILL   = "skill"
)

# ---------------------------------------------------------------------------
# ModelPickerPriceCategory (enum-like constants)
# ---------------------------------------------------------------------------

#' ModelPickerPriceCategory
#'
#' Constants for model picker price categories.
#' @export
ModelPickerPriceCategory <- list(
  HIGH      = "high",
  LOW       = "low",
  MEDIUM    = "medium",
  VERY_HIGH = "very_high"
)

# ---------------------------------------------------------------------------
# SlashCommandInput
# ---------------------------------------------------------------------------

#' SlashCommandInput
#'
#' Input configuration for a slash command.
#'
#' @field hint Character. Hint text for the input.
#' @field completion Character or NULL. Completion type.
#' @export
SlashCommandInput <- R6::R6Class(
  "SlashCommandInput",
  public = list(
    hint = NULL,
    completion = NULL,

    #' @description Create a new SlashCommandInput.
    #' @param hint Character. Required.
    #' @param completion Character or NULL.
    initialize = function(hint, completion = NULL) {
      self$hint <- hint
      self$completion <- completion
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(hint = self$hint)
      if (!is.null(self$completion)) result$completion <- self$completion
      result
    }
  )
)

# ---------------------------------------------------------------------------
# SlashCommandInfo
# ---------------------------------------------------------------------------

#' SlashCommandInfo
#'
#' Information about a slash command.
#'
#' @field allow_during_agent_execution Logical. Whether command can run during agent execution.
#' @field description Character. Description of the command.
#' @field kind Character. Kind of command (builtin, client, skill).
#' @field name Character. Name of the command.
#' @field aliases Character vector or NULL. Alternative names.
#' @field experimental Logical or NULL. Whether command is experimental.
#' @field input SlashCommandInput or NULL. Input configuration.
#' @export
SlashCommandInfo <- R6::R6Class(
  "SlashCommandInfo",
  public = list(
    allow_during_agent_execution = NULL,
    description = NULL,
    kind = NULL,
    name = NULL,
    aliases = NULL,
    experimental = NULL,
    input = NULL,

    #' @description Create a new SlashCommandInfo.
    #' @param allow_during_agent_execution Logical.
    #' @param description Character.
    #' @param kind Character.
    #' @param name Character.
    #' @param aliases Character vector or NULL.
    #' @param experimental Logical or NULL.
    #' @param input SlashCommandInput or NULL.
    initialize = function(allow_during_agent_execution, description, kind, name,
                          aliases = NULL, experimental = NULL, input = NULL) {
      self$allow_during_agent_execution <- allow_during_agent_execution
      self$description <- description
      self$kind <- kind
      self$name <- name
      self$aliases <- aliases
      self$experimental <- experimental
      self$input <- input
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(
        allowDuringAgentExecution = self$allow_during_agent_execution,
        description = self$description,
        kind = self$kind,
        name = self$name
      )
      if (!is.null(self$aliases)) result$aliases <- self$aliases
      if (!is.null(self$experimental)) result$experimental <- self$experimental
      if (!is.null(self$input)) result$input <- self$input$to_list()
      result
    }
  )
)

# ---------------------------------------------------------------------------
# CommandsInvokeRequest
# ---------------------------------------------------------------------------

#' CommandsInvokeRequest
#'
#' Request to invoke a command.
#'
#' @field name Character. Command name.
#' @field input Character or NULL. Command input.
#' @export
CommandsInvokeRequest <- R6::R6Class(
  "CommandsInvokeRequest",
  public = list(
    name = NULL,
    input = NULL,

    #' @description Create a new CommandsInvokeRequest.
    #' @param name Character.
    #' @param input Character or NULL.
    initialize = function(name, input = NULL) {
      self$name <- name
      self$input <- input
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(name = self$name)
      if (!is.null(self$input)) result$input <- self$input
      result
    }
  )
)

# ---------------------------------------------------------------------------
# CommandsListRequest
# ---------------------------------------------------------------------------

#' CommandsListRequest
#'
#' Request to list available commands.
#'
#' @field include_builtins Logical or NULL.
#' @field include_client_commands Logical or NULL.
#' @field include_skills Logical or NULL.
#' @export
CommandsListRequest <- R6::R6Class(
  "CommandsListRequest",
  public = list(
    include_builtins = NULL,
    include_client_commands = NULL,
    include_skills = NULL,

    #' @description Create a new CommandsListRequest.
    #' @param include_builtins Logical or NULL.
    #' @param include_client_commands Logical or NULL.
    #' @param include_skills Logical or NULL.
    initialize = function(include_builtins = NULL, include_client_commands = NULL,
                          include_skills = NULL) {
      self$include_builtins <- include_builtins
      self$include_client_commands <- include_client_commands
      self$include_skills <- include_skills
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list()
      if (!is.null(self$include_builtins)) result$includeBuiltins <- self$include_builtins
      if (!is.null(self$include_client_commands)) result$includeClientCommands <- self$include_client_commands
      if (!is.null(self$include_skills)) result$includeSkills <- self$include_skills
      result
    }
  )
)

# ---------------------------------------------------------------------------
# ModelBillingTokenPrices
# ---------------------------------------------------------------------------

#' ModelBillingTokenPrices
#'
#' Token prices for model billing.
#'
#' @field batch_size Integer or NULL.
#' @field cache_price Integer or NULL.
#' @field input_price Integer or NULL.
#' @field output_price Integer or NULL.
#' @export
ModelBillingTokenPrices <- R6::R6Class(
  "ModelBillingTokenPrices",
  public = list(
    batch_size = NULL,
    cache_price = NULL,
    input_price = NULL,
    output_price = NULL,

    #' @description Create a new ModelBillingTokenPrices.
    #' @param batch_size Integer or NULL.
    #' @param cache_price Integer or NULL.
    #' @param input_price Integer or NULL.
    #' @param output_price Integer or NULL.
    initialize = function(batch_size = NULL, cache_price = NULL,
                          input_price = NULL, output_price = NULL) {
      self$batch_size <- batch_size
      self$cache_price <- cache_price
      self$input_price <- input_price
      self$output_price <- output_price
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list()
      if (!is.null(self$batch_size)) result$batchSize <- self$batch_size
      if (!is.null(self$cache_price)) result$cachePrice <- self$cache_price
      if (!is.null(self$input_price)) result$inputPrice <- self$input_price
      if (!is.null(self$output_price)) result$outputPrice <- self$output_price
      result
    }
  )
)

#' ModelBilling
#'
#' Model billing information.
#'
#' @field multiplier Numeric. Billing multiplier.
#' @field token_prices ModelBillingTokenPrices or NULL. Token prices.
#' @field picker_price_category Character or NULL. Price category.
#' @export
ModelBilling <- R6::R6Class(
  "ModelBilling",
  public = list(
    multiplier = NULL,
    token_prices = NULL,
    picker_price_category = NULL,

    #' @description Create a new ModelBilling.
    #' @param multiplier Numeric.
    #' @param token_prices ModelBillingTokenPrices or NULL.
    #' @param picker_price_category Character or NULL.
    initialize = function(multiplier, token_prices = NULL, picker_price_category = NULL) {
      self$multiplier <- multiplier
      self$token_prices <- token_prices
      self$picker_price_category <- picker_price_category
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(multiplier = self$multiplier)
      if (!is.null(self$token_prices)) result$tokenPrices <- self$token_prices$to_list()
      if (!is.null(self$picker_price_category)) result$pickerPriceCategory <- self$picker_price_category
      result
    }
  )
)

#' ModelInfo
#'
#' Information about an available model.
#'
#' @field id Character. Model identifier (e.g., "claude-sonnet-4.5").
#' @field name Character. Display name.
#' @field capabilities ModelCapabilities R6 object.
#' @field policy ModelPolicy or NULL.
#' @field billing ModelBilling or NULL.
#' @field supported_reasoning_efforts Character vector or NULL.
#' @field default_reasoning_effort Character or NULL.
#' @export
ModelInfo <- R6::R6Class(
  "ModelInfo",
  public = list(
    id = NULL,
    name = NULL,
    capabilities = NULL,
    policy = NULL,
    billing = NULL,
    supported_reasoning_efforts = NULL,
    default_reasoning_effort = NULL,

    #' @description Create a new ModelInfo.
    #' @param id Character.
    #' @param name Character.
    #' @param capabilities ModelCapabilities R6 object.
    #' @param policy ModelPolicy or NULL.
    #' @param billing ModelBilling or NULL.
    #' @param supported_reasoning_efforts Character vector or NULL.
    #' @param default_reasoning_effort Character or NULL.
    initialize = function(id, name, capabilities, policy = NULL, billing = NULL,
                          supported_reasoning_efforts = NULL,
                          default_reasoning_effort = NULL) {
      self$id <- id
      self$name <- name
      self$capabilities <- capabilities
      self$policy <- policy
      self$billing <- billing
      self$supported_reasoning_efforts <- supported_reasoning_efforts
      self$default_reasoning_effort <- default_reasoning_effort
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(
        id = self$id,
        name = self$name,
        capabilities = self$capabilities$to_list()
      )
      if (!is.null(self$policy)) result$policy <- self$policy$to_list()
      if (!is.null(self$billing)) result$billing <- self$billing$to_list()
      if (!is.null(self$supported_reasoning_efforts)) {
        result$supportedReasoningEfforts <- self$supported_reasoning_efforts
      }
      if (!is.null(self$default_reasoning_effort)) {
        result$defaultReasoningEffort <- self$default_reasoning_effort
      }
      result
    }
  )
)

#' Create a ModelInfo from a named list (parsed JSON).
#' @param obj Named list from parsed JSON.
#' @return ModelInfo R6 object.
#' @keywords internal
model_info_from_list <- function(obj) {
  caps_obj <- obj$capabilities
  supports_obj <- caps_obj$supports %||% list()
  limits_obj <- caps_obj$limits %||% list()

  supports <- ModelSupports$new(
    vision = isTRUE(supports_obj$vision),
    reasoning_effort = isTRUE(supports_obj$reasoningEffort)
  )
  limits <- ModelLimits$new(
    max_prompt_tokens = limits_obj$max_prompt_tokens,
    max_context_window_tokens = limits_obj$max_context_window_tokens
  )
  capabilities <- ModelCapabilities$new(supports = supports, limits = limits)

  policy <- NULL
  if (!is.null(obj$policy)) {
    policy <- ModelPolicy$new(state = obj$policy$state, terms = obj$policy$terms)
  }

  billing <- NULL
  if (!is.null(obj$billing)) {
    token_prices <- NULL
    if (!is.null(obj$billing$tokenPrices)) {
      token_prices <- ModelBillingTokenPrices$new(
        batch_size = obj$billing$tokenPrices$batchSize,
        cache_price = obj$billing$tokenPrices$cachePrice,
        input_price = obj$billing$tokenPrices$inputPrice,
        output_price = obj$billing$tokenPrices$outputPrice
      )
    }
    billing <- ModelBilling$new(
      multiplier = obj$billing$multiplier,
      token_prices = token_prices,
      picker_price_category = obj$billing$pickerPriceCategory
    )
  }

  ModelInfo$new(
    id = obj$id,
    name = obj$name,
    capabilities = capabilities,
    policy = policy,
    billing = billing,
    supported_reasoning_efforts = obj$supportedReasoningEfforts,
    default_reasoning_effort = obj$defaultReasoningEffort
  )
}


# ---------------------------------------------------------------------------
# ExitPlanModeRequest / ExitPlanModeResult
# ---------------------------------------------------------------------------

#' ExitPlanModeRequest
#'
#' Request to exit plan mode from the agent.
#'
#' @field summary Character. Summary of the plan.
#' @field plan_content Character or NULL. Full plan content.
#' @field actions Character vector. Available actions.
#' @field recommended_action Character. The recommended action.
#' @export
ExitPlanModeRequest <- R6::R6Class(
  "ExitPlanModeRequest",
  public = list(
    summary = NULL,
    plan_content = NULL,
    actions = NULL,
    recommended_action = NULL,

    #' @description Create a new ExitPlanModeRequest.
    #' @param summary Character. Summary of the plan.
    #' @param actions Character vector. Available actions.
    #' @param recommended_action Character. The recommended action.
    #' @param plan_content Character or NULL. Full plan content.
    initialize = function(summary, actions, recommended_action, plan_content = NULL) {
      self$summary <- summary
      self$plan_content <- plan_content
      self$actions <- actions
      self$recommended_action <- recommended_action
    }
  )
)

#' ExitPlanModeResult
#'
#' Response to an exit-plan-mode request.
#'
#' @field approved Logical. Whether the user approved exiting plan mode.
#' @field selected_action Character or NULL. Selected action.
#' @field feedback Character or NULL. User feedback.
#' @export
ExitPlanModeResult <- R6::R6Class(
  "ExitPlanModeResult",
  public = list(
    approved = NULL,
    selected_action = NULL,
    feedback = NULL,

    #' @description Create a new ExitPlanModeResult.
    #' @param approved Logical. Whether the user approved exiting plan mode.
    #' @param selected_action Character or NULL. Selected action.
    #' @param feedback Character or NULL. User feedback.
    initialize = function(approved, selected_action = NULL, feedback = NULL) {
      self$approved <- approved
      self$selected_action <- selected_action
      self$feedback <- feedback
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list(approved = self$approved)
      if (!is.null(self$selected_action)) result$selectedAction <- self$selected_action
      if (!is.null(self$feedback)) result$feedback <- self$feedback
      result
    }
  )
)


# ---------------------------------------------------------------------------
# TraceContext
# ---------------------------------------------------------------------------

#' TraceContext
#'
#' W3C Trace Context propagation data for distributed tracing.
#'
#' @field traceparent Character or NULL.
#' @field tracestate Character or NULL.
#' @export
TraceContext <- R6::R6Class(
  "TraceContext",
  public = list(
    traceparent = NULL,
    tracestate = NULL,

    #' @description Create a new TraceContext.
    #' @param traceparent Character or NULL.
    #' @param tracestate Character or NULL.
    initialize = function(traceparent = NULL, tracestate = NULL) {
      self$traceparent <- traceparent
      self$tracestate <- tracestate
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list()
      if (!is.null(self$traceparent)) result$traceparent <- self$traceparent
      if (!is.null(self$tracestate)) result$tracestate <- self$tracestate
      result
    }
  )
)


# ---------------------------------------------------------------------------
# SessionMetadata
# ---------------------------------------------------------------------------

#' SessionMetadata
#'
#' Metadata about a session.
#'
#' @field session_id Character. Session identifier.
#' @field start_time Character. ISO 8601 timestamp when session was created.
#' @field modified_time Character. ISO 8601 timestamp when session was last modified.
#' @field is_remote Logical. Whether the session is remote.
#' @field summary Character or NULL. Optional summary.
#' @export
SessionMetadata <- R6::R6Class(
  "SessionMetadata",
  public = list(
    session_id = NULL,
    start_time = NULL,
    modified_time = NULL,
    is_remote = FALSE,
    summary = NULL,

    #' @description Create a new SessionMetadata.
    #' @param session_id Character.
    #' @param start_time Character.
    #' @param modified_time Character.
    #' @param is_remote Logical.
    #' @param summary Character or NULL.
    initialize = function(session_id, start_time, modified_time, is_remote, summary = NULL) {
      self$session_id <- session_id
      self$start_time <- start_time
      self$modified_time <- modified_time
      self$is_remote <- is_remote
      self$summary <- summary
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(
        sessionId = self$session_id,
        startTime = self$start_time,
        modifiedTime = self$modified_time,
        isRemote = self$is_remote
      )
      if (!is.null(self$summary)) result$summary <- self$summary
      result
    }
  )
)

#' Create a SessionMetadata from a named list.
#' @param obj Named list.
#' @return SessionMetadata R6 object.
#' @keywords internal
session_metadata_from_list <- function(obj) {
  SessionMetadata$new(
    session_id = obj$sessionId,
    start_time = obj$startTime,
    modified_time = obj$modifiedTime,
    is_remote = isTRUE(obj$isRemote),
    summary = obj$summary
  )
}


# ---------------------------------------------------------------------------
# SessionLifecycleEvent
# ---------------------------------------------------------------------------

#' SessionLifecycleEvent
#'
#' Session lifecycle event notification.
#'
#' @field type Character. Event type (e.g. "session.created", "session.deleted").
#' @field session_id Character. Session identifier.
#' @field metadata Named list or NULL. Lifecycle event metadata.
#' @export
SessionLifecycleEvent <- R6::R6Class(
  "SessionLifecycleEvent",
  public = list(
    type = NULL,
    session_id = NULL,
    metadata = NULL,

    #' @description Create a new SessionLifecycleEvent.
    #' @param type Event type string.
    #' @param session_id Session ID string.
    #' @param metadata Named list or NULL.
    initialize = function(type, session_id, metadata = NULL) {
      self$type <- type
      self$session_id <- session_id
      self$metadata <- metadata
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(type = self$type, sessionId = self$session_id)
      if (!is.null(self$metadata)) result$metadata <- self$metadata
      result
    }
  )
)

#' Create a SessionLifecycleEvent from a named list.
#' @param obj Named list.
#' @return SessionLifecycleEvent R6 object.
#' @keywords internal
session_lifecycle_event_from_list <- function(obj) {
  SessionLifecycleEvent$new(
    type = obj$type %||% "session.updated",
    session_id = obj$sessionId %||% "",
    metadata = obj$metadata
  )
}


# ---------------------------------------------------------------------------
# ToolResultObject / ToolInvocation / Tool
# ---------------------------------------------------------------------------

#' ToolResultObject
#'
#' Result of a tool invocation.
#'
#' @field text_result_for_llm Character. Text result for the LLM.
#' @field result_type Character. "success", "failure", "rejected", or "denied".
#' @field error Character or NULL. Error message if any.
#' @field session_log Character or NULL. Session log message.
#' @field tool_telemetry Named list or NULL. Telemetry data.
#' @export
ToolResultObject <- R6::R6Class(
  "ToolResultObject",
  public = list(
    text_result_for_llm = "",
    result_type = "success",
    error = NULL,
    session_log = NULL,
    tool_telemetry = NULL,

    #' @description Create a new ToolResultObject.
    #' @param text_result_for_llm Character.
    #' @param result_type Character.
    #' @param error Character or NULL.
    #' @param session_log Character or NULL.
    #' @param tool_telemetry Named list or NULL.
    initialize = function(text_result_for_llm = "", result_type = "success",
                          error = NULL, session_log = NULL, tool_telemetry = NULL) {
      self$text_result_for_llm <- text_result_for_llm
      self$result_type <- result_type
      self$error <- error
      self$session_log <- session_log
      self$tool_telemetry <- tool_telemetry
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(
        textResultForLlm = self$text_result_for_llm,
        resultType = self$result_type
      )
      if (!is.null(self$error)) result$error <- self$error
      if (!is.null(self$session_log)) result$sessionLog <- self$session_log
      if (!is.null(self$tool_telemetry)) result$toolTelemetry <- self$tool_telemetry
      result
    }
  )
)

#' ToolInvocation
#'
#' Represents a tool invocation context.
#'
#' @field session_id Character. The session making the tool call.
#' @field tool_call_id Character. Unique ID for this tool call.
#' @field tool_name Character. Name of the tool being called.
#' @field arguments Any. The arguments passed to the tool.
#' @export
ToolInvocation <- R6::R6Class(
  "ToolInvocation",
  public = list(
    session_id = NULL,
    tool_call_id = NULL,
    tool_name = NULL,
    arguments = NULL,

    #' @description Create a new ToolInvocation.
    #' @param session_id Character.
    #' @param tool_call_id Character.
    #' @param tool_name Character.
    #' @param arguments Any.
    initialize = function(session_id, tool_call_id, tool_name, arguments = NULL) {
      self$session_id <- session_id
      self$tool_call_id <- tool_call_id
      self$tool_name <- tool_name
      self$arguments <- arguments
    }
  )
)

#' Tool
#'
#' Definition of a custom tool exposed to the Copilot CLI.
#'
#' @field name Character. Tool name.
#' @field description Character. Tool description shown to the LLM.
#' @field handler Function. The handler function(invocation) -> ToolResultObject or list.
#' @field parameters Named list or NULL. JSON schema for parameters.
#' @export
Tool <- R6::R6Class(
  "Tool",
  public = list(
    name = NULL,
    description = NULL,
    handler = NULL,
    parameters = NULL,

    #' @description Create a new Tool.
    #' @param name Character. Tool name.
    #' @param description Character. Tool description.
    #' @param handler Function accepting a ToolInvocation, returning a ToolResultObject or list.
    #' @param parameters Named list or NULL. JSON schema for parameters.
    initialize = function(name, description, handler, parameters = NULL) {
      self$name <- name
      self$description <- description
      self$handler <- handler
      self$parameters <- parameters
    }
  )
)


# ---------------------------------------------------------------------------
# MessageOptions
# ---------------------------------------------------------------------------

#' MessageOptions
#'
#' Options for sending a message to a session.
#'
#' @field prompt Character. The prompt/message text to send.
#' @field attachments List or NULL. Optional file/directory attachments.
#' @field mode Character or NULL. Message processing mode ("enqueue" or "immediate").
#' @field response_format Character or NULL. Response format ("text", "image", or "json_object").
#' @field image_options List or NULL. Image generation options (from \code{image_options()}).
#' @field request_headers Named list or NULL. Custom HTTP headers for outbound model requests.
#' @export
MessageOptions <- R6::R6Class(
  "MessageOptions",
  public = list(
    prompt = NULL,
    attachments = NULL,
    mode = NULL,
    response_format = NULL,
    image_options = NULL,
    request_headers = NULL,

    #' @description Create new MessageOptions.
    #' @param prompt Character. The prompt text.
    #' @param attachments List or NULL.
    #' @param mode Character or NULL.
    #' @param response_format Character or NULL. One of RESPONSE_FORMATS.
    #' @param image_options List or NULL. Image generation options.
    #' @param request_headers Named list or NULL. Custom HTTP headers for outbound model requests.
    initialize = function(prompt, attachments = NULL, mode = NULL,
                          response_format = NULL, image_options = NULL,
                          request_headers = NULL) {
      self$prompt <- prompt
      self$attachments <- attachments
      self$mode <- mode
      self$response_format <- response_format
      self$image_options <- image_options
      self$request_headers <- request_headers
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(prompt = self$prompt)
      if (!is.null(self$attachments)) result$attachments <- self$attachments
      if (!is.null(self$mode)) result$mode <- self$mode
      if (!is.null(self$response_format)) result$responseFormat <- self$response_format
      if (!is.null(self$image_options)) result$imageOptions <- self$image_options
      if (!is.null(self$request_headers)) result$requestHeaders <- self$request_headers
      result
    }
  )
)


# ---------------------------------------------------------------------------
# Image Generation Types
# ---------------------------------------------------------------------------

#' Valid response format values
#' @export
RESPONSE_FORMATS <- c("text", "image", "json_object")

#' Create image options for image generation
#'
#' @param size Image size (e.g. "1024x1024")
#' @param quality Image quality ("hd" or "standard")
#' @param style Image style ("natural" or "vivid")
#' @return A list of image options
#' @export
image_options <- function(size = NULL, quality = NULL, style = NULL) {
  opts <- list()
  if (!is.null(size)) opts$size <- size
  if (!is.null(quality)) opts$quality <- quality
  if (!is.null(style)) opts$style <- style
  opts
}

#' Parse assistant image data from JSON response
#'
#' @param data A list containing image data fields
#' @return A list with format, base64, url, revisedPrompt, width, height
#' @export
parse_assistant_image_data <- function(data) {
  list(
    format = data$format,
    base64 = data$base64,
    url = data$url,
    revisedPrompt = data$revisedPrompt,
    width = data$width,
    height = data$height
  )
}

#' Parse a content block from a mixed text+image response
#'
#' @param block A list with type and content fields
#' @return A parsed content block list
#' @export
parse_content_block <- function(block) {
  if (block$type == "image" && !is.null(block$image)) {
    block$image <- parse_assistant_image_data(block$image)
  }
  block
}


# ---------------------------------------------------------------------------
# Session Filesystem Types
# ---------------------------------------------------------------------------

#' SessionFsConfig
#'
#' Configuration for a custom session filesystem provider.
#'
#' @field initial_cwd Character. Initial working directory.
#' @field session_state_path Character. Path for session state storage.
#' @field conventions Character. Path conventions ("windows" or "posix").
#' @export
SessionFsConfig <- R6::R6Class(
  "SessionFsConfig",
  public = list(
    initial_cwd = NULL,
    session_state_path = NULL,
    conventions = NULL,

    #' @description Create a new SessionFsConfig.
    #' @param initial_cwd Character.
    #' @param session_state_path Character.
    #' @param conventions Character.
    initialize = function(initial_cwd, session_state_path, conventions) {
      self$initial_cwd <- initial_cwd
      self$session_state_path <- session_state_path
      self$conventions <- conventions
    },

    #' @description Convert to list.
    to_list = function() {
      list(
        initialCwd = self$initial_cwd,
        sessionStatePath = self$session_state_path,
        conventions = self$conventions
      )
    }
  )
)

#' SessionFsFileInfo
#'
#' File metadata returned by session filesystem operations.
#'
#' @field name Character. File or directory name.
#' @field size Numeric. Size in bytes.
#' @field is_directory Logical. Whether the entry is a directory.
#' @field is_file Logical. Whether the entry is a file.
#' @field created_at Character or NULL. ISO 8601 creation timestamp.
#' @field modified_at Character or NULL. ISO 8601 modification timestamp.
#' @export
SessionFsFileInfo <- R6::R6Class(
  "SessionFsFileInfo",
  public = list(
    name = NULL,
    size = 0,
    is_directory = FALSE,
    is_file = FALSE,
    created_at = NULL,
    modified_at = NULL,

    #' @description Create a new SessionFsFileInfo.
    #' @param name Character.
    #' @param size Numeric.
    #' @param is_directory Logical.
    #' @param is_file Logical.
    #' @param created_at Character or NULL.
    #' @param modified_at Character or NULL.
    initialize = function(name, size, is_directory, is_file,
                          created_at = NULL, modified_at = NULL) {
      self$name <- name
      self$size <- size
      self$is_directory <- is_directory
      self$is_file <- is_file
      self$created_at <- created_at
      self$modified_at <- modified_at
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(
        name = self$name,
        size = self$size,
        isDirectory = self$is_directory,
        isFile = self$is_file
      )
      if (!is.null(self$created_at)) result$createdAt <- self$created_at
      if (!is.null(self$modified_at)) result$modifiedAt <- self$modified_at
      result
    }
  )
)

#' Session Filesystem Provider
#'
#' To implement a session filesystem provider in R, create a list with the
#' following function elements. Each function receives a session ID and
#' path arguments, and should return the appropriate result or stop() on error.
#'
#' Expected functions:
#' \itemize{
#'   \item \code{read_file(session_id, path)} → character
#'   \item \code{write_file(session_id, path, content)} → invisible NULL
#'   \item \code{append_file(session_id, path, content)} → invisible NULL
#'   \item \code{exists(session_id, path)} → logical
#'   \item \code{stat(session_id, path)} → SessionFsFileInfo
#'   \item \code{mkdir(session_id, path, recursive)} → invisible NULL
#'   \item \code{readdir(session_id, path)} → character vector
#'   \item \code{readdir_with_types(session_id, path)} → list of SessionFsFileInfo
#'   \item \code{rm(session_id, path, recursive)} → invisible NULL
#'   \item \code{rename(session_id, old_path, new_path)} → invisible NULL
#' }
#'
#' @name SessionFsProvider
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# CopilotClientOptions
# ---------------------------------------------------------------------------

#' CopilotClientOptions
#'
#' Options for creating a CopilotClient connection.
#'
#' @field session_idle_timeout_seconds Integer or NULL. Server-wide idle timeout for sessions in seconds.
#' @field session_fs List or NULL. Session filesystem provider (list of callback functions).
#' @export
CopilotClientOptions <- R6::R6Class(
  "CopilotClientOptions",
  public = list(
    session_idle_timeout_seconds = NULL,
    session_fs = NULL,
    ## GitHub token for authentication.
    github_token = NULL,
    ## Copilot home directory path.
    copilot_home = NULL,
    ## TCP connection token for authentication.
    tcp_connection_token = NULL,

    #' @description Create new CopilotClientOptions.
    #' @param session_idle_timeout_seconds Integer or NULL. Server-wide idle timeout in seconds.
    #' @param session_fs List or NULL. Session filesystem provider (list of callback functions).
    #' @param github_token Character or NULL. GitHub token for authentication.
    #' @param copilot_home Character or NULL. Copilot home directory path.
    #' @param tcp_connection_token Character or NULL. TCP connection token for authentication.
    initialize = function(session_idle_timeout_seconds = NULL, session_fs = NULL, github_token = NULL,
                          copilot_home = NULL, tcp_connection_token = NULL) {
      self$session_idle_timeout_seconds <- session_idle_timeout_seconds
      self$session_fs <- session_fs
      self$github_token <- github_token
      self$copilot_home <- copilot_home
      self$tcp_connection_token <- tcp_connection_token
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list()
      if (!is.null(self$session_idle_timeout_seconds)) {
        result$sessionIdleTimeoutSeconds <- as.integer(self$session_idle_timeout_seconds)
      }
      if (!is.null(self$session_fs)) {
        result$sessionFs <- self$session_fs
      }
      if (!is.null(self$github_token)) {
        result$gitHubToken <- self$github_token
      }
      if (!is.null(self$copilot_home)) {
        result$copilotHome <- self$copilot_home
      }
      if (!is.null(self$tcp_connection_token)) {
        result$tcpConnectionToken <- self$tcp_connection_token
      }
      result
    }
  )
)


# ---------------------------------------------------------------------------
# AgentConfig
# ---------------------------------------------------------------------------

#' AgentConfig
#'
#' Configuration for a custom agent.
#'
#' @field name Character. Agent name.
#' @field description Character or NULL. Agent description.
#' @field prompt Character or NULL. Agent prompt.
#' @field skills Character vector or NULL. List of skill names to preload.
#' @export
AgentConfig <- R6::R6Class(
  "AgentConfig",
  public = list(
    name = NULL,
    description = NULL,
    prompt = NULL,
    skills = NULL,

    #' @description Create a new AgentConfig.
    #' @param name Character. Agent name.
    #' @param description Character or NULL.
    #' @param prompt Character or NULL.
    #' @param skills Character vector or NULL. List of skill names to preload.
    initialize = function(name, description = NULL, prompt = NULL, skills = NULL) {
      self$name <- name
      self$description <- description
      self$prompt <- prompt
      self$skills <- skills
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list(name = self$name)
      if (!is.null(self$description)) result$description <- self$description
      if (!is.null(self$prompt)) result$prompt <- self$prompt
      if (!is.null(self$skills)) result$skills <- self$skills
      result
    }
  )
)


# ---------------------------------------------------------------------------
# MCP Server Configuration
# ---------------------------------------------------------------------------

#' MCPLocalServerConfig
#'
#' Configuration for a local/stdio MCP server.
#'
#' @field tools Character vector. Tool filter: c() = none, c("*") = all.
#' @field type Character or NULL. "local" or "stdio".
#' @field timeout Integer or NULL. Tool call timeout in ms.
#' @field command Character. Command to run.
#' @field args Character vector. Command arguments.
#' @field env Named character vector or NULL. Environment variables.
#' @field cwd Character or NULL. Working directory.
#' @export
MCPLocalServerConfig <- R6::R6Class(
  "MCPLocalServerConfig",
  public = list(
    tools = NULL,
    type = NULL,
    timeout = NULL,
    command = NULL,
    args = NULL,
    env = NULL,
    cwd = NULL,

    #' @description Create a new MCPLocalServerConfig.
    #' @param command Character. Command to run.
    #' @param args Character vector. Command arguments.
    #' @param tools Character vector. Default c("*").
    #' @param type Character or NULL. "local" or "stdio".
    #' @param timeout Integer or NULL.
    #' @param env Named character vector or NULL.
    #' @param cwd Character or NULL.
    initialize = function(command, args = character(0), tools = c("*"),
                          type = NULL, timeout = NULL, env = NULL, cwd = NULL) {
      self$command <- command
      self$args <- args
      self$tools <- tools
      self$type <- type
      self$timeout <- timeout
      self$env <- env
      self$cwd <- cwd
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list(tools = self$tools, command = self$command, args = self$args)
      if (!is.null(self$type)) result$type <- self$type
      if (!is.null(self$timeout)) result$timeout <- self$timeout
      if (!is.null(self$env)) result$env <- self$env
      if (!is.null(self$cwd)) result$cwd <- self$cwd
      result
    }
  )
)

#' MCPRemoteServerConfig
#'
#' Configuration for a remote MCP server (HTTP or SSE).
#'
#' @field tools Character vector. Tool filter.
#' @field type Character. "http" or "sse".
#' @field timeout Integer or NULL. Tool call timeout in ms.
#' @field url Character. Server URL.
#' @field headers Named character vector or NULL. HTTP headers.
#' @export
MCPRemoteServerConfig <- R6::R6Class(
  "MCPRemoteServerConfig",
  public = list(
    tools = NULL,
    type = NULL,
    timeout = NULL,
    url = NULL,
    headers = NULL,

    #' @description Create a new MCPRemoteServerConfig.
    #' @param url Character. Server URL.
    #' @param type Character. "http" or "sse".
    #' @param tools Character vector. Default c("*").
    #' @param timeout Integer or NULL.
    #' @param headers Named character vector or NULL.
    initialize = function(url, type = "http", tools = c("*"),
                          timeout = NULL, headers = NULL) {
      self$url <- url
      self$type <- type
      self$tools <- tools
      self$timeout <- timeout
      self$headers <- headers
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list(tools = self$tools, type = self$type, url = self$url)
      if (!is.null(self$timeout)) result$timeout <- self$timeout
      if (!is.null(self$headers)) result$headers <- self$headers
      result
    }
  )
)


# ---------------------------------------------------------------------------
# System Message Configuration
# ---------------------------------------------------------------------------

#' Known system prompt section identifiers for the "customize" mode.
#' @export
SYSTEM_PROMPT_SECTIONS <- c(
  "identity", "tone", "tool_efficiency", "environment_context",
  "code_change_rules", "guidelines", "safety", "tool_instructions",
  "custom_instructions", "last_instructions"
)

#' Override action constants for system prompt sections.
#' @export
SECTION_OVERRIDE_ACTIONS <- c("replace", "remove", "append", "prepend")

#' SectionOverride
#'
#' Override operation for a single system prompt section.
#'
#' @field action Character. The override action ("replace", "remove", "append", "prepend").
#' @field content Character or NULL. Content for the override.
#' @export
SectionOverride <- R6::R6Class(
  "SectionOverride",
  public = list(
    action = NULL,
    content = NULL,

    #' @description Create a new SectionOverride.
    #' @param action Character. Override action.
    #' @param content Character or NULL. Override content.
    initialize = function(action, content = NULL) {
      self$action <- action
      self$content <- content
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list(action = self$action)
      if (!is.null(self$content)) result$content <- self$content
      result
    }
  )
)

#' SystemMessageConfig
#'
#' System message configuration for session creation.
#'
#' Supports three modes:
#' - "append" (default): SDK foundation + optional custom content
#' - "replace": Full control, caller provides entire system message
#' - "customize": Section-level overrides with graceful fallback
#'
#' @field mode Character or NULL. The configuration mode.
#' @field content Character or NULL. Content string.
#' @field sections Named list of SectionOverride or NULL. Section overrides (customize mode).
#' @export
SystemMessageConfig <- R6::R6Class(
  "SystemMessageConfig",
  public = list(
    mode = NULL,
    content = NULL,
    sections = NULL,

    #' @description Create a new SystemMessageConfig.
    #' @param mode Character or NULL.
    #' @param content Character or NULL.
    #' @param sections Named list of SectionOverride or NULL.
    initialize = function(mode = NULL, content = NULL, sections = NULL) {
      self$mode <- mode
      self$content <- content
      self$sections <- sections
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list()
      if (!is.null(self$mode)) result$mode <- self$mode
      if (!is.null(self$content)) result$content <- self$content
      if (!is.null(self$sections)) {
        sections_list <- list()
        for (key in names(self$sections)) {
          sections_list[[key]] <- self$sections[[key]]$to_list()
        }
        result$sections <- sections_list
      }
      result
    }
  )
)

# ---------------------------------------------------------------------------
# CommandContext
# ---------------------------------------------------------------------------

#' CommandContext
#'
#' Context for a slash-command invocation.
#'
#' @field session_id Character. Session ID where the command was invoked.
#' @field command Character. Full command text.
#' @field command_name Character. Command name without leading /.
#' @field args Character. Raw argument string after the command name.
#' @export
CommandContext <- R6::R6Class(
  "CommandContext",
  public = list(
    session_id = NULL,
    command = NULL,
    command_name = NULL,
    args = NULL,

    #' @description Create a new CommandContext.
    #' @param session_id Character.
    #' @param command Character.
    #' @param command_name Character.
    #' @param args Character.
    initialize = function(session_id, command, command_name, args = "") {
      self$session_id <- session_id
      self$command <- command
      self$command_name <- command_name
      self$args <- args
    }
  )
)

# ---------------------------------------------------------------------------
# CommandDefinition
# ---------------------------------------------------------------------------

#' CommandDefinition
#'
#' Definition of a slash command registered with the session.
#'
#' @field name Character. Command name (without leading /).
#' @field description Character or NULL. Human-readable description.
#' @field handler Function. Handler callback receiving a CommandContext.
#' @export
CommandDefinition <- R6::R6Class(
  "CommandDefinition",
  public = list(
    name = NULL,
    description = NULL,
    handler = NULL,

    #' @description Create a new CommandDefinition.
    #' @param name Character.
    #' @param description Character or NULL.
    #' @param handler Function.
    initialize = function(name, description = NULL, handler) {
      self$name <- name
      self$description <- description
      self$handler <- handler
    }
  )
)

# ---------------------------------------------------------------------------
# ElicitationContext
# ---------------------------------------------------------------------------

#' ElicitationContext
#'
#' Context for an elicitation request from the server.
#'
#' @field session_id Character.
#' @field message Character.
#' @field requested_schema Named list or NULL.
#' @field mode Character or NULL.
#' @field elicitation_source Character or NULL.
#' @field url Character or NULL.
#' @export
ElicitationContext <- R6::R6Class(
  "ElicitationContext",
  public = list(
    session_id = NULL,
    message = NULL,
    requested_schema = NULL,
    mode = NULL,
    elicitation_source = NULL,
    url = NULL,

    #' @description Create a new ElicitationContext.
    #' @param session_id Character.
    #' @param message Character.
    #' @param requested_schema Named list or NULL.
    #' @param mode Character or NULL.
    #' @param elicitation_source Character or NULL.
    #' @param url Character or NULL.
    initialize = function(session_id, message, requested_schema = NULL,
                          mode = NULL, elicitation_source = NULL, url = NULL) {
      self$session_id <- session_id
      self$message <- message
      self$requested_schema <- requested_schema
      self$mode <- mode
      self$elicitation_source <- elicitation_source
      self$url <- url
    }
  )
)

# ---------------------------------------------------------------------------
# ElicitationResult
# ---------------------------------------------------------------------------

#' ElicitationResult
#'
#' Result returned from an elicitation handler.
#'
#' @field action Character. "accept", "decline", or "cancel".
#' @field content Named list or NULL.
#' @export
ElicitationResult <- R6::R6Class(
  "ElicitationResult",
  public = list(
    action = NULL,
    content = NULL,

    #' @description Create a new ElicitationResult.
    #' @param action Character.
    #' @param content Named list or NULL.
    initialize = function(action, content = NULL) {
      self$action <- action
      self$content <- content
    }
  )
)

# ---------------------------------------------------------------------------
# SessionConfig
# ---------------------------------------------------------------------------

#' SessionConfig
#'
#' Configuration for creating a Copilot session.
#'
#' @field model Character or NULL. Model identifier.
#' @field system_message Character or NULL. System message.
#' @field model_capabilities Named list or NULL. Model capabilities overrides.
#' @field enable_config_discovery Logical or NULL. Auto-discover MCP server configs. Default: FALSE.
#' @field include_sub_agent_streaming_events Logical or NULL. Include sub-agent streaming events. Default: TRUE.
#' @field streaming Logical or NULL. Enable streaming mode.
#' @field mcp_servers Named list or NULL. Map of server name to MCPLocalServerConfig/MCPRemoteServerConfig.
#' @field commands List of CommandDefinition or NULL. Slash commands registered for this session.
#' @field on_elicitation_request Function or NULL. Handler for elicitation requests from the server.
#' @field excluded_tools Character vector or NULL. Tool names to exclude.
#' @export
SessionConfig <- R6::R6Class(
  "SessionConfig",
  public = list(
    model = NULL,
    system_message = NULL,
    model_capabilities = NULL,
    enable_config_discovery = NULL,
    include_sub_agent_streaming_events = NULL,
    streaming = NULL,
    mcp_servers = NULL,
    github_token = NULL,
    commands = NULL,
    on_elicitation_request = NULL,
    excluded_tools = NULL,
    instruction_directories = NULL,

    #' @description Create a new SessionConfig.
    #' @param model Character or NULL.
    #' @param system_message Character or NULL.
    #' @param model_capabilities Named list or NULL. Model capabilities overrides.
    #' @param enable_config_discovery Logical or NULL. Auto-discover MCP server configs.
    #' @param include_sub_agent_streaming_events Logical or NULL. Include sub-agent streaming events.
    #' @param streaming Logical or NULL.
    #' @param mcp_servers Named list or NULL. Map of name to MCP server config.
    #' @param github_token Character or NULL. GitHub token for authentication. Overrides client-level token for this session.
    #' @param commands List of CommandDefinition or NULL.
    #' @param on_elicitation_request Function or NULL.
    #' @param excluded_tools Character vector or NULL.
    #' @param instruction_directories Character vector or NULL. Instruction directories for prompt customization.
    initialize = function(model = NULL, system_message = NULL,
                          model_capabilities = NULL,
                          enable_config_discovery = NULL,
                          include_sub_agent_streaming_events = NULL,
                          streaming = NULL,
                          mcp_servers = NULL,
                          github_token = NULL,
                          commands = NULL,
                          on_elicitation_request = NULL,
                          excluded_tools = NULL,
                          instruction_directories = NULL) {
      self$model <- model
      self$system_message <- system_message
      self$model_capabilities <- model_capabilities
      self$enable_config_discovery <- enable_config_discovery
      self$include_sub_agent_streaming_events <- include_sub_agent_streaming_events
      self$streaming <- streaming
      self$mcp_servers <- mcp_servers
      self$github_token <- github_token
      self$commands <- commands
      self$on_elicitation_request <- on_elicitation_request
      self$excluded_tools <- excluded_tools
      self$instruction_directories <- instruction_directories
    },

    #' @description Convert to list.
    to_list = function() {
      result <- list()
      if (!is.null(self$model)) result$model <- self$model
      if (!is.null(self$system_message)) {
        if (inherits(self$system_message, "SystemMessageConfig")) {
          result$systemMessage <- self$system_message$to_list()
        } else {
          result$systemMessage <- self$system_message
        }
      }
      if (!is.null(self$model_capabilities)) result$modelCapabilities <- self$model_capabilities
      if (!is.null(self$enable_config_discovery)) {
        result$enableConfigDiscovery <- self$enable_config_discovery
      }
      if (!is.null(self$include_sub_agent_streaming_events)) {
        result$includeSubAgentStreamingEvents <- self$include_sub_agent_streaming_events
      }
      if (!is.null(self$streaming)) result$streaming <- self$streaming
      if (!is.null(self$mcp_servers)) {
        mcp_list <- list()
        for (name in names(self$mcp_servers)) {
          srv <- self$mcp_servers[[name]]
          if (inherits(srv, "MCPLocalServerConfig") || inherits(srv, "MCPRemoteServerConfig")) {
            mcp_list[[name]] <- srv$to_list()
          } else {
            mcp_list[[name]] <- srv
          }
        }
        result$mcpServers <- mcp_list
      }
      if (!is.null(self$github_token)) result$gitHubToken <- self$github_token
      if (!is.null(self$excluded_tools)) result$excludedTools <- self$excluded_tools
      if (!is.null(self$on_elicitation_request)) result$requestElicitation <- TRUE
      if (!is.null(self$instruction_directories)) result$instructionDirectories <- self$instruction_directories
      if (!is.null(self$commands)) {
        cmds_list <- lapply(self$commands, function(cmd) {
          c_list <- list(name = cmd$name)
          if (!is.null(cmd$description)) c_list$description <- cmd$description
          c_list
        })
        result$commands <- cmds_list
      }
      result
    }
  )
)


# ---------------------------------------------------------------------------
# Experimental: SkillsLoadDiagnostics
# ---------------------------------------------------------------------------

#' SkillsLoadDiagnostics (Experimental)
#'
#' Diagnostics from loading skills.
#'
#' @field errors Character vector. Error messages.
#' @field warnings Character vector. Warning messages.
#' @export
SkillsLoadDiagnostics <- R6::R6Class(
  "SkillsLoadDiagnostics",
  public = list(
    errors = NULL,
    warnings = NULL,

    #' @description Create a new SkillsLoadDiagnostics.
    #' @param errors Character vector.
    #' @param warnings Character vector.
    initialize = function(errors, warnings) {
      self$errors <- errors
      self$warnings <- warnings
    },

    #' @description Convert to list.
    to_list = function() {
      list(
        errors = self$errors,
        warnings = self$warnings
      )
    }
  )
)

# ---------------------------------------------------------------------------
# Remote Session Types
# ---------------------------------------------------------------------------

#' Valid remote session mode values
#'
#' Per-session remote mode. "off" disables remote, "export" exports session
#' events to Mission Control without enabling remote steering, "on" enables
#' both export and remote steering.
#' @export
REMOTE_SESSION_MODES <- c("export", "off", "on")

#' RemoteEnableRequest (Experimental)
#'
#' Request to enable remote mode for a session.
#'
#' @field mode Character or NULL. Per-session remote mode.
#' @export
RemoteEnableRequest <- R6::R6Class(
  "RemoteEnableRequest",
  public = list(
    mode = NULL,

    #' @description Create a new RemoteEnableRequest.
    #' @param mode Character or NULL. Per-session remote mode.
    initialize = function(mode = NULL) {
      self$mode <- mode
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list()
      if (!is.null(self$mode)) result$mode <- self$mode
      result
    }
  )
)

#' RemoteEnableResult (Experimental)
#'
#' Result of enabling remote mode for a session.
#'
#' @field remote_steerable Logical. Whether remote steering is enabled.
#' @field url Character or NULL. Mission Control frontend URL for this session.
#' @export
RemoteEnableResult <- R6::R6Class(
  "RemoteEnableResult",
  public = list(
    remote_steerable = NULL,
    url = NULL,

    #' @description Create a new RemoteEnableResult.
    #' @param remote_steerable Logical. Whether remote steering is enabled.
    #' @param url Character or NULL. Mission Control frontend URL.
    initialize = function(remote_steerable, url = NULL) {
      self$remote_steerable <- remote_steerable
      self$url <- url
    },

    #' @description Convert to list for JSON serialization.
    to_list = function() {
      result <- list(remoteSteerable = self$remote_steerable)
      if (!is.null(self$url)) result$url <- self$url
      result
    }
  )
)


# ---------------------------------------------------------------------------
# NULL-coalescing operator
# ---------------------------------------------------------------------------

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

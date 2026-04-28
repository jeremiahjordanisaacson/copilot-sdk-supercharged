# Error Handling in R

Patterns for handling errors when using the GitHub Copilot SDK in R.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors gracefully using R's condition system with tryCatch and withCallingHandlers.

## Basic tryCatch

```r
library(copilot)

run_conversation <- function() {
  client <- NULL

  tryCatch({
    client <- copilot_client()
    start(client)

    session <- create_session(client)
    response <- send_and_wait(session, message = "What is R?")
    cat("Response:", response$message, "\n")

  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
  }, warning = function(w) {
    cat("Warning:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  }, finally = {
    if (!is.null(client)) {
      tryCatch(stop(client), error = function(e) {
        cat("Cleanup error:", conditionMessage(e), "\n")
      })
    }
  })
}

run_conversation()
```

## Retry with Exponential Backoff

```r
library(copilot)

connect_with_retry <- function(max_retries = 3) {
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      client <- copilot_client()
      start(client)
      cat(sprintf("Connected on attempt %d\n", attempt))
      return(client)
    }, error = function(e) {
      cat(sprintf("Attempt %d failed: %s\n", attempt, conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) return(result)

    if (attempt < max_retries) {
      delay <- attempt * 2
      cat(sprintf("Retrying in %d seconds...\n", delay))
      Sys.sleep(delay)
    }
  }

  stop("Failed to connect after ", max_retries, " attempts")
}
```

## Typed Error Conditions

Define custom conditions for more precise error handling.

```r
# Define custom condition constructors
copilot_connection_error <- function(message, call = NULL) {
  structure(
    class = c("copilot_connection_error", "error", "condition"),
    list(message = message, call = call)
  )
}

copilot_timeout_error <- function(message, call = NULL) {
  structure(
    class = c("copilot_timeout_error", "error", "condition"),
    list(message = message, call = call)
  )
}

# Handle different error types
safe_ask <- function(client, message) {
  tryCatch(
    {
      session <- create_session(client)
      response <- send_and_wait(session, message = message)
      response$message
    },
    copilot_connection_error = function(e) {
      cat("Connection error:", conditionMessage(e), "\n")
      NA_character_
    },
    copilot_timeout_error = function(e) {
      cat("Timeout:", conditionMessage(e), "\n")
      NA_character_
    },
    error = function(e) {
      cat("Unexpected error:", conditionMessage(e), "\n")
      NA_character_
    }
  )
}
```

## Using withCallingHandlers for Logging

```r
library(copilot)

run_with_logging <- function(client, message) {
  withCallingHandlers(
    tryCatch({
      session <- create_session(client)
      response <- send_and_wait(session, message = message)
      cat("Success:", response$message, "\n")
      response$message
    }, error = function(e) {
      cat("Caught error:", conditionMessage(e), "\n")
      NA_character_
    }),
    warning = function(w) {
      cat("[WARN]", conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      cat("[INFO]", conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
}
```

## Safe Wrapper Function

```r
library(copilot)

with_copilot <- function(fn, ...) {
  client <- NULL

  tryCatch({
    client <- copilot_client()
    start(client)
    fn(client, ...)
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    invisible(NULL)
  }, finally = {
    if (!is.null(client)) {
      tryCatch(stop(client), error = function(e) NULL)
    }
  })
}

# Usage
with_copilot(function(client) {
  session <- create_session(client)
  response <- send_and_wait(session, message = "Explain data frames in R")
  cat(response$message, "\n")
})
```

## Best Practices

- **Always use tryCatch with finally**: Ensure cleanup runs regardless of success or failure.
- **Initialize resources to NULL**: Check for NULL before cleanup to avoid secondary errors.
- **Use custom conditions**: Define specific condition classes for different error categories.
- **Use withCallingHandlers for logging**: Log warnings and messages without interrupting the call stack.
- **Wrap cleanup in its own tryCatch**: Prevent cleanup errors from masking the original error.
- **Return NA or NULL on failure**: Use sentinel values that are idiomatic in R rather than re-throwing.

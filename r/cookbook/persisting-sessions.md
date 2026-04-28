# Persisting Sessions in R

Patterns for saving and resuming Copilot sessions across application restarts in R.

## Scenario

Your application needs to save a conversation so users can close the app and return later to continue exactly where they left off.

## Saving and Loading Session IDs

```r
session_file <- "session_state.rds"

save_session_id <- function(session_id, file = session_file) {
  saveRDS(session_id, file = file)
}

load_session_id <- function(file = session_file) {
  if (!file.exists(file)) return(NULL)

  tryCatch(
    readRDS(file),
    error = function(e) {
      warning("Could not read session file: ", conditionMessage(e))
      NULL
    }
  )
}
```

## Resuming a Session

```r
library(copilot)

get_or_create_session <- function(client) {
  saved_id <- load_session_id()

  if (!is.null(saved_id)) {
    session <- tryCatch(
      create_session(client, session_id = saved_id),
      error = function(e) {
        cat("Could not resume session:", conditionMessage(e), "\n")
        NULL
      }
    )
    if (!is.null(session)) {
      cat("Resumed session:", saved_id, "\n")
      return(session)
    }
  }

  # Create a new session and save the ID
  session <- create_session(client)
  save_session_id(session$id)
  cat("Created new session:", session$id, "\n")
  session
}
```

## Full Example: Persistent Chat

```r
library(copilot)

persistent_chat <- function() {
  client <- copilot_client()
  start(client)

  on.exit({
    tryCatch(stop(client), error = function(e) NULL)
  })

  session <- get_or_create_session(client)

  cat("Chat (type 'quit' to exit):\n")

  repeat {
    input <- readline(prompt = "> ")
    if (input == "quit") break

    tryCatch({
      response <- send_and_wait(session, message = input)
      cat("Assistant:", response$message, "\n")
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
    })
  }

  cat("Session saved. You can resume later.\n")
}

persistent_chat()
```

## Storing Multiple Sessions

```r
sessions_file <- "sessions.rds"

load_all_sessions <- function(file = sessions_file) {
  if (!file.exists(file)) return(list())

  tryCatch(
    readRDS(file),
    error = function(e) {
      warning("Could not read sessions file: ", conditionMessage(e))
      list()
    }
  )
}

save_all_sessions <- function(sessions, file = sessions_file) {
  saveRDS(sessions, file = file)
}

get_named_session <- function(client, name, system_prompt = NULL) {
  sessions <- load_all_sessions()
  saved_id <- sessions[[name]]

  if (!is.null(saved_id)) {
    session <- tryCatch(
      create_session(client, session_id = saved_id),
      error = function(e) NULL
    )
    if (!is.null(session)) {
      cat("Resumed", name, "session\n")
      return(session)
    }
  }

  session <- create_session(client, system_prompt = system_prompt)
  sessions[[name]] <- session$id
  save_all_sessions(sessions)
  cat("Created new", name, "session\n")
  session
}

# Usage
client <- copilot_client()
start(client)

code_session <- get_named_session(client, "code", system_prompt = "You are an R expert.")
docs_session <- get_named_session(client, "docs", system_prompt = "You write R documentation.")

stop(client)
```

## Best Practices

- **Persist only the session ID**: The SDK and CLI handle conversation state internally. You just need the ID.
- **Use `saveRDS`/`readRDS`**: R's native serialization handles strings cleanly and is type-safe.
- **Try to resume before creating**: Attempt to restore a saved session first, then fall back to a new one.
- **Handle stale sessions with tryCatch**: If resuming fails (session expired), create a fresh session.
- **Use `on.exit` for cleanup**: Ensure the client is stopped even if an error occurs.
- **Save on creation, not on exit**: Write the session ID immediately after creating it to avoid data loss on crashes.

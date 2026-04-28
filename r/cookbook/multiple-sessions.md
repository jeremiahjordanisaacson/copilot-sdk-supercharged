# Multiple Sessions in R

Patterns for managing multiple independent conversations with the GitHub Copilot SDK in R.

## Scenario

Your application needs to run several conversations at the same time, each with its own context and history, sharing a single client connection.

## Creating Multiple Sessions

```r
library(copilot)

client <- copilot_client()
start(client)

# Create separate sessions for different tasks
code_session <- create_session(client, system_prompt = "You are an R expert.")
docs_session <- create_session(client, system_prompt = "You are a technical writer.")

# Each session maintains its own conversation history
code_reply <- send_and_wait(code_session, message = "Explain data.table vs dplyr")
cat("Code:", code_reply$message, "\n")

docs_reply <- send_and_wait(docs_session, message = "Write a vignette template")
cat("Docs:", docs_reply$message, "\n")

stop(client)
```

## Session Manager with Environments

```r
library(copilot)

create_session_manager <- function(client) {
  env <- new.env(parent = emptyenv())
  env$client <- client
  env$sessions <- list()

  env$get_or_create <- function(name, system_prompt = NULL) {
    if (!is.null(env$sessions[[name]])) {
      return(env$sessions[[name]])
    }

    session <- create_session(env$client, system_prompt = system_prompt)
    env$sessions[[name]] <- session
    session
  }

  env$remove <- function(name) {
    env$sessions[[name]] <- NULL
  }

  env$count <- function() {
    length(env$sessions)
  }

  env$names <- function() {
    names(env$sessions)
  }

  env
}

# Usage
client <- copilot_client()
start(client)

manager <- create_session_manager(client)

frontend <- manager$get_or_create("frontend", system_prompt = "You are a Shiny expert.")
backend <- manager$get_or_create("backend", system_prompt = "You are a plumber API expert.")

r1 <- send_and_wait(frontend, message = "Explain reactive values")
cat("Frontend:", r1$message, "\n")

r2 <- send_and_wait(backend, message = "Explain plumber endpoints")
cat("Backend:", r2$message, "\n")

cat("Active sessions:", manager$count(), "\n")

stop(client)
```

## Running Multiple Tasks with lapply

```r
library(copilot)

run_parallel_tasks <- function(client, tasks) {
  results <- lapply(tasks, function(task) {
    tryCatch({
      session <- create_session(client, system_prompt = task$persona)
      response <- send_and_wait(session, message = task$prompt)
      list(name = task$name, result = response$message, error = NULL)
    }, error = function(e) {
      list(name = task$name, result = NULL, error = conditionMessage(e))
    })
  })

  results
}

# Usage
client <- copilot_client()
start(client)

tasks <- list(
  list(name = "review", prompt = "Review this function", persona = "You review R code."),
  list(name = "docs", prompt = "Generate roxygen docs", persona = "You write R docs."),
  list(name = "tests", prompt = "Write testthat tests", persona = "You write R tests.")
)

results <- run_parallel_tasks(client, tasks)

for (r in results) {
  if (is.null(r$error)) {
    cat(sprintf("[%s] %s\n", r$name, r$result))
  } else {
    cat(sprintf("[%s] ERROR: %s\n", r$name, r$error))
  }
}

stop(client)
```

## Best Practices

- **Reuse a single client**: Create one `copilot_client()` and share it across all sessions.
- **Use environments for managers**: R environments provide mutable state for tracking sessions.
- **Use lapply for batch tasks**: Process multiple tasks cleanly with functional iteration.
- **Wrap each task in tryCatch**: Prevent one failed session from stopping the rest.
- **Set distinct system prompts**: Give each session a focused persona for better results.
- **Clean up sessions you no longer need**: Remove finished sessions from the manager to free memory.

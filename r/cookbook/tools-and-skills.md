# Tools and Skills in R

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in R.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```r
library(copilot)

time_tool <- define_tool(
  "get_current_time",
  "Returns the current date and time",
  function(context) {
    list(content = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }
)
```

## Defining a Tool with Parameters

```r
library(copilot)

read_file_tool <- define_tool(
  "read_file",
  "Reads the contents of a file given its path",
  function(context) {
    path <- context$get_param("path")
    if (is.null(path) || path == "") {
      return(list(error = "Missing required parameter: path"))
    }

    if (!file.exists(path)) {
      return(list(error = paste("File not found:", path)))
    }

    content <- tryCatch(
      paste(readLines(path, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )

    if (is.null(content)) {
      list(error = paste("Could not read file:", path))
    } else {
      list(content = content)
    }
  }
)
```

## Registering Tools with a Client

```r
library(copilot)

client <- copilot_client(tools = list(time_tool, read_file_tool))
start(client)

session <- create_session(client,
  system_prompt = "You have access to file reading and time tools."
)

response <- send_and_wait(session, message = "What time is it right now?")
cat(response$message, "\n")

stop(client)
```

## Defining Multiple Related Tools (Skill Pattern)

```r
library(copilot)

create_kv_skill <- function() {
  store <- new.env(parent = emptyenv())

  kv_get <- define_tool("kv_get", "Get a value by key", function(context) {
    key <- context$get_param("key")
    value <- tryCatch(get(key, envir = store), error = function(e) NULL)
    if (is.null(value)) {
      list(content = paste("Key not found:", key))
    } else {
      list(content = value)
    }
  })

  kv_set <- define_tool("kv_set", "Set a key-value pair", function(context) {
    key <- context$get_param("key")
    value <- context$get_param("value")
    assign(key, value, envir = store)
    list(content = paste("Stored:", key))
  })

  kv_list <- define_tool("kv_list", "List all keys", function(context) {
    keys <- ls(envir = store)
    if (length(keys) == 0) {
      list(content = "Store is empty")
    } else {
      list(content = paste(keys, collapse = "\n"))
    }
  })

  list(kv_get, kv_set, kv_list)
}

# Usage
kv_tools <- create_kv_skill()

client <- copilot_client(tools = kv_tools)
start(client)

session <- create_session(client)
response <- send_and_wait(session,
  message = "Store my name as 'Alice' and then retrieve it"
)
cat(response$message, "\n")

stop(client)
```

## Sub-Agent Orchestration

```r
library(copilot)

orchestrate <- function(client) {
  # Create specialized sessions as sub-agents
  planner <- create_session(client,
    system_prompt = "You are a planner. Break tasks into steps."
  )
  coder <- create_session(client,
    system_prompt = "You are an R developer."
  )
  reviewer <- create_session(client,
    system_prompt = "You review R code for bugs."
  )

  # Step 1: Plan
  plan <- send_and_wait(planner, message = "Plan an R package for data validation")
  cat("Plan:", plan$message, "\n\n")

  # Step 2: Code
  code <- send_and_wait(coder,
    message = paste("Implement this plan:\n", plan$message)
  )
  cat("Code:", code$message, "\n\n")

  # Step 3: Review
  review <- send_and_wait(reviewer,
    message = paste("Review this code:\n", code$message)
  )
  cat("Review:", review$message, "\n\n")
}

# Usage
client <- copilot_client()
start(client)
orchestrate(client)
stop(client)
```

## Best Practices

- **Use environments for shared tool state**: R environments provide mutable storage that closures can capture.
- **Return named lists from tool handlers**: Use `list(content = ...)` or `list(error = ...)` consistently.
- **Use factory functions for skills**: Create a function that returns a list of related tools sharing state.
- **Validate parameters early**: Check for NULL and empty strings at the start of each handler.
- **Use separate sessions for sub-agents**: Each agent gets its own system prompt and conversation context.
- **Chain agent outputs with paste**: Pass one agent's output as input to the next using `paste()`.

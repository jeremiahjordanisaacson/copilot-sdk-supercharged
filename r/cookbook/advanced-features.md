# Advanced v2.0 Features in R

Recipes for using advanced v2.0 features of the GitHub Copilot SDK in R, including per-session auth, SessionFs, commands, system prompts, skills, streaming, and more.

## Scenario

Your application needs fine-grained control over authentication, filesystem access, prompt customization, agent skills, and other advanced capabilities introduced in SDK v2.0.

## 1. Per-Session Authentication

Supply a GitHub token per session instead of globally.

```r
library(copilot)

client <- create_client(options = list())
start_client(client)

session <- create_session(client, config = list(
  github_token = "ghu_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
))

response <- send_and_wait(session, message = "Who am I authenticated as?")
cat(response$message, "\n")

stop_client(client)
```

## 2. Session Idle Timeout

Automatically expire sessions after a period of inactivity.

```r
session <- create_session(client, config = list(
  session_idle_timeout_seconds = 600  # 10-minute idle timeout
))

# Session will be cleaned up automatically if idle for 10 minutes
response <- send_and_wait(session,
  message = "This session expires after 10 minutes of inactivity."
)
```

## 3. SessionFs (Session Filesystem)

Configure a filesystem provider with the 10 required I/O operations.

```r
library(copilot)

# Implement the 10 required I/O operations
fs_provider <- list(
  read_file = function(path) {
    list(content = readLines(path, warn = FALSE) |> paste(collapse = "\n"))
  },

  write_file = function(path, content) {
    writeLines(content, path)
    list(success = TRUE)
  },

  delete_file = function(path) {
    file.remove(path)
    list(success = TRUE)
  },

  list_directory = function(path) {
    list(entries = list.files(path))
  },

  create_directory = function(path) {
    dir.create(path, recursive = TRUE)
    list(success = TRUE)
  },

  stat = function(path) {
    info <- file.info(path)
    list(size = info$size, is_directory = info$isdir)
  },

  rename = function(old_path, new_path) {
    file.rename(old_path, new_path)
    list(success = TRUE)
  },

  copy = function(src, dst) {
    file.copy(src, dst)
    list(success = TRUE)
  },

  exists = function(path) {
    list(exists = file.exists(path))
  },

  watch = function(path, callback) {
    # Set up file watching (platform-specific)
    list(success = TRUE)
  }
)

session <- create_session(client, config = list(
  session_fs = list(
    enabled = TRUE,
    root_path = "/workspace/project",
    provider = fs_provider
  )
))

response <- send_and_wait(session,
  message = "List the files in the workspace."
)
cat(response$message, "\n")
```

## 4. Commands and UI Elicitation

Register slash commands and handle elicitation requests from the agent.

```r
library(copilot)

# Define available commands
commands <- list(
  list(name = "/deploy", description = "Deploy the current project"),
  list(name = "/test",   description = "Run the test suite"),
  list(name = "/status", description = "Show project status")
)

# Handle elicitation: agent asks the user for input
elicitation_handler <- function(req) {
  cat("Agent asks:", req$message, "\n")
  if (req$type == "confirmation") {
    return(list(confirmed = TRUE))
  }
  list(text = "user-provided-value")
}

session <- create_session(client, config = list(
  commands = commands,
  elicitation_handler = elicitation_handler
))

response <- send_and_wait(session, message = "/deploy to staging")
cat(response$message, "\n")
```

## 5. System Prompt Customization

Use replace or customize modes with structured sections.

```r
# Mode 1: Replace the entire system prompt
session1 <- create_session(client, config = list(
  system_prompt = list(
    mode = "replace",
    content = "You are an R/Tidyverse expert. Only discuss R topics."
  )
))

# Mode 2: Customize with structured sections
session2 <- create_session(client, config = list(
  system_prompt = list(
    mode = "customize",
    sections = list(
      list(id = "role",
           content = "You are a senior R developer."),
      list(id = "constraints",
           content = "Always use tidyverse idioms. Prefer pipes over nested calls."),
      list(id = "output_format",
           content = "Include roxygen2 documentation in all function examples.")
    )
  )
))
```

## 6. Per-Agent Skills

Configure skill directories and disable specific skills per agent.

```r
session <- create_session(client, config = list(
  skill_directories = c(
    "/home/user/.copilot/skills",
    "/project/.copilot/skills"
  ),
  disabled_skills = c("web-search", "code-execution")
))

response <- send_and_wait(session,
  message = "Use the custom project skills to analyze the codebase."
)
```

## 7. Per-Agent Tool Visibility

Hide specific tools from certain agents using excluded_tools.

```r
# Create a read-only agent that cannot modify files
reviewer <- create_session(client, config = list(
  system_prompt = list(
    mode = "replace",
    content = "You are a code reviewer. Analyze but do not modify."
  ),
  excluded_tools = c(
    "write_file", "delete_file", "execute_command", "create_directory"
  )
))

# Create a full-access agent
developer <- create_session(client, config = list(
  system_prompt = list(
    mode = "replace",
    content = "You are a developer with full access."
  )
))
```

## 8. Runtime Request Headers

Attach custom headers to individual requests for tracing or auth.

```r
response <- send_and_wait(session,
  message = "Summarize the latest deploy logs.",
  request_headers = list(
    "X-Request-Id"  = "req-abc-123",
    "X-Trace-Id"    = "trace-xyz-789",
    "Authorization"  = "Bearer custom-token-here"
  )
)
```

## 9. Model Capabilities Override

Override model capabilities for a session to control behavior.

```r
session <- create_session(client, config = list(
  model_capabilities = list(
    streaming = TRUE,
    tool_calling = TRUE,
    vision = FALSE,
    max_tokens = 8192,
    context_window = 128000
  )
))
```

## 10. Config Discovery

Enable automatic discovery of project-level configuration files.

```r
session <- create_session(client, config = list(
  enable_config_discovery = TRUE
))

# The SDK will automatically discover and load:
#   .copilot/config.yml
#   .copilot/prompts/*.md
#   .copilot/skills/
# from the project root and parent directories.
response <- send_and_wait(session,
  message = "What project configuration did you discover?"
)
```

## 11. Sub-Agent Streaming Events

Receive streaming events from sub-agents during orchestration.

```r
library(copilot)

session <- create_session(client, config = list(
  include_sub_agent_streaming_events = TRUE
))

on_event(session, function(event) {
  if (event$type == "assistant.message_delta") {
    cat("[delta]", event$content)
  } else if (event$type == "sub_agent.message_delta") {
    cat(sprintf("[sub-agent:%s] %s", event$agent_id, event$content))
  }
})

send_message(session, message = "Coordinate the planner and coder agents.")
```

## 12. Session Metadata

Retrieve metadata about the current session.

```r
metadata <- get_session_metadata(session)
cat("Session ID: ", metadata$session_id, "\n")
cat("Created at: ", metadata$created_at, "\n")
cat("Turn count: ", metadata$turn_count, "\n")
cat("Model:      ", metadata$model, "\n")
cat("Token usage:", metadata$total_tokens, "\n")
```

## 13. MCP Server Configuration

Configure Model Context Protocol servers using stdio or HTTP transports.

```r
session <- create_session(client, config = list(
  mcp_servers = list(
    # Stdio transport: launch a local MCP server process
    list(
      name = "filesystem",
      transport = "stdio",
      command = "npx",
      args = c("-y", "@modelcontextprotocol/server-filesystem", "/workspace")
    ),
    # HTTP transport: connect to a remote MCP server
    list(
      name = "remote-db",
      transport = "http",
      url = "https://mcp.example.com/db",
      headers = list(Authorization = "Bearer token123")
    )
  )
))

response <- send_and_wait(session,
  message = "List the files in the workspace using the MCP server."
)
```

## 14. Image Generation

Configure the response format to request image generation.

```r
library(copilot)

session <- create_session(client, config = list(
  response_format = list(
    type = "image_generation",
    image_size = "1024x1024",
    image_quality = "high"
  )
))

response <- send_and_wait(session,
  message = "Generate an image of a futuristic cityscape at sunset."
)

# Save the generated image
if (!is.null(response$image_data)) {
  writeBin(response$image_data, "cityscape.png")
  cat("Image saved to cityscape.png\n")
}
```

## Best Practices

- **Scope tokens tightly**: Use per-session `github_token` with minimal scopes for multi-tenant apps.
- **Use list-based config**: R's named lists provide a natural fit for nested configuration structures.
- **Combine features**: Per-agent skills, tool visibility, and system prompts work together to create specialized agents.
- **Use config discovery in dev**: Enable `enable_config_discovery` during development so project-level configs are picked up automatically.
- **Use callbacks for events**: Register event handlers with `on_event()` for reactive streaming processing.
- **Set idle timeouts in server apps**: Use `session_idle_timeout_seconds` to prevent resource leaks from abandoned sessions.

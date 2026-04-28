# Advanced v2.0 Features - Shell/Bash

Recipes for v2.0 SDK features in Shell/Bash: per-session auth, SessionFs, commands, system prompts, skills, config discovery, image generation, and more.

## Per-Session Authentication

**Scenario:** Provide a GitHub token on each session for user-scoped auth instead of a global token.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_A=$(copilot_create_session \
  --github-token "$GITHUB_TOKEN_USER_A" \
  --system-prompt "You are a helpful assistant.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_A" \
  --message "Summarize my recent pull requests.")

echo "Response: $RESPONSE"

# Create a second session with a different user token
SESSION_B=$(copilot_create_session \
  --github-token "$GITHUB_TOKEN_USER_B" \
  --system-prompt "You are a code reviewer.")
```

## Session Idle Timeout

**Scenario:** Automatically expire sessions after a period of inactivity.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --session-idle-timeout-seconds 300 \
  --system-prompt "You are a helpful assistant.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Hello!")

echo "Response: $RESPONSE"

# Session automatically expires after 300s of inactivity.
# Sending a message after timeout returns an error.
```

## SessionFs (Session Filesystem)

**Scenario:** Configure a session filesystem provider with I/O operations for file-based context.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Define SessionFs provider functions (10 I/O operations)
session_fs_read_file() {
  local path="$1"
  cat "$path" 2>/dev/null || echo '{"error":"File not found"}'
}

session_fs_write_file() {
  local path="$1"
  local content="$2"
  echo "$content" > "$path"
  echo '{"success":true}'
}

session_fs_list_directory() {
  local path="$1"
  ls -1 "$path" 2>/dev/null | jq -R -s 'split("\n") | map(select(. != ""))'
}

session_fs_create_directory() {
  local path="$1"
  mkdir -p "$path"
  echo '{"success":true}'
}

session_fs_delete_file() {
  local path="$1"
  rm -f "$path"
  echo '{"success":true}'
}

session_fs_file_exists() {
  local path="$1"
  [[ -e "$path" ]] && echo "true" || echo "false"
}

session_fs_get_file_info() {
  local path="$1"
  stat --printf='{"size":%s,"modified":"%Y"}' "$path" 2>/dev/null \
    || echo '{"error":"File not found"}'
}

session_fs_copy_file() {
  local source="$1"
  local destination="$2"
  cp "$source" "$destination"
  echo '{"success":true}'
}

session_fs_move_file() {
  local source="$1"
  local destination="$2"
  mv "$source" "$destination"
  echo '{"success":true}'
}

session_fs_search_files() {
  local path="$1"
  local pattern="$2"
  grep -rl "$pattern" "$path" 2>/dev/null | jq -R -s 'split("\n") | map(select(. != ""))'
}

# Register all SessionFs operations
copilot_register_session_fs \
  --root-path "/workspace/project" \
  --writable true \
  --read-file session_fs_read_file \
  --write-file session_fs_write_file \
  --list-directory session_fs_list_directory \
  --create-directory session_fs_create_directory \
  --delete-file session_fs_delete_file \
  --file-exists session_fs_file_exists \
  --get-file-info session_fs_get_file_info \
  --copy-file session_fs_copy_file \
  --move-file session_fs_move_file \
  --search-files session_fs_search_files

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a file management assistant." \
  --session-fs true)

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "List all files in the current project directory.")

echo "Response: $RESPONSE"
```

## Commands and UI Elicitation

**Scenario:** Register commands and handle UI elicitation requests from the model.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Define available commands as JSON
COMMANDS='[
  {
    "name": "deploy",
    "description": "Deploy the application to a target environment",
    "parameters": [
      {"name": "environment", "type": "string", "required": true},
      {"name": "version", "type": "string", "required": false}
    ]
  },
  {
    "name": "rollback",
    "description": "Roll back the most recent deployment",
    "parameters": [
      {"name": "environment", "type": "string", "required": true}
    ]
  }
]'

# Handle elicitation: the model asks the user for input
elicitation_handler() {
  local request_json="$1"
  local message
  message=$(echo "$request_json" | jq -r '.message')
  echo "Model asks: $message" >&2

  local options
  options=$(echo "$request_json" | jq -r '.options[]? // empty')
  if [[ -n "$options" ]]; then
    local i=1
    while IFS= read -r option; do
      echo "  ${i}) ${option}" >&2
      ((i++))
    done <<< "$options"
  fi

  read -rp "Your choice: " answer
  echo "{\"response\": \"$answer\"}"
}

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a deployment assistant." \
  --commands "$COMMANDS" \
  --elicitation-handler elicitation_handler)

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "/deploy")

echo "Response: $RESPONSE"
```

## System Prompt Customization

**Scenario:** Use replace and customize modes with sections to control the system prompt.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Replace mode: completely replace the default system prompt
SESSION_REPLACE=$(copilot_create_session \
  --system-prompt-mode "replace" \
  --system-prompt "You are a Bash scripting expert. Only answer shell questions.")

# Customize mode: add sections before/after the default prompt
BEFORE_SECTION="You are assisting a senior DevOps engineer."
AFTER_SECTION="Always prefer POSIX-compliant syntax.
Use shellcheck-clean patterns.
Cite the Bash Reference Manual when relevant."

SESSION_CUSTOM=$(copilot_create_session \
  --system-prompt-mode "customize" \
  --system-prompt-section-before "$BEFORE_SECTION" \
  --system-prompt-section-after "$AFTER_SECTION")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_CUSTOM" \
  --message "How do I safely iterate over files with spaces in names?")

echo "Response: $RESPONSE"
```

## Per-Agent Skills

**Scenario:** Configure skill directories and disable specific skills per agent.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a code assistant with limited skills." \
  --skill-directories "$HOME/.copilot/skills,/project/.copilot-skills" \
  --disabled-skills "web-search,image-generation")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Refactor this function to use proper error handling.")

echo "Response: $RESPONSE"
```

## Per-Agent Tool Visibility

**Scenario:** Hide specific tools from the model for a given session.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a read-only code reviewer." \
  --excluded-tools "file_write,shell_execute,git_push")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Review this script and suggest improvements.")

echo "Response: $RESPONSE"
```

## Runtime Request Headers

**Scenario:** Attach custom headers to individual send requests for tracing or routing.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a helpful assistant.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Explain process substitution in Bash." \
  --request-header "X-Request-Id: req-abc-123" \
  --request-header "X-Trace-Parent: 00-traceid-spanid-01" \
  --request-header "X-Custom-Routing: priority-queue")

echo "Response: $RESPONSE"
```

## Model Capabilities Override

**Scenario:** Override model capabilities for a session, such as vision or function calling.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

MODEL_CAPS='{
  "vision": true,
  "function_calling": true,
  "json_output": true,
  "max_tokens": 8192
}'

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are an assistant with extended capabilities." \
  --model-capabilities "$MODEL_CAPS")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Analyze this architecture diagram.")

echo "Response: $RESPONSE"
```

## Config Discovery

**Scenario:** Enable automatic discovery of project-level configuration files.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init --enable-config-discovery
copilot_start

# The SDK automatically scans for:
#   .copilot/config.sh
#   .copilot/config.json
#   .github/copilot-config.yml
# in the workspace and its parents.

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a helpful assistant.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "What configuration is active for this project?")

echo "Response: $RESPONSE"
```

## Sub-Agent Streaming Events

**Scenario:** Subscribe to streaming events from sub-agents during orchestration.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are an orchestrator that delegates to sub-agents." \
  --include-sub-agent-streaming-events true)

# Stream events and handle each type
copilot_send_streaming \
  --session "$SESSION_ID" \
  --message "Research Bash best practices and write a tutorial." \
  | while IFS= read -r event_json; do
    event_type=$(echo "$event_json" | jq -r '.type')
    case "$event_type" in
      assistant.message_delta)
        delta=$(echo "$event_json" | jq -r '.delta')
        printf "%s" "$delta"
        ;;
      sub_agent.start)
        agent_name=$(echo "$event_json" | jq -r '.agent_name')
        printf "\n[Sub-agent started: %s]\n" "$agent_name"
        ;;
      sub_agent.message_delta)
        delta=$(echo "$event_json" | jq -r '.delta')
        printf "%s" "$delta"
        ;;
      sub_agent.end)
        agent_name=$(echo "$event_json" | jq -r '.agent_name')
        printf "\n[Sub-agent finished: %s]\n" "$agent_name"
        ;;
    esac
  done

echo ""
```

## Session Metadata

**Scenario:** Retrieve metadata about an active session.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a helpful assistant.")

copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Hello!" > /dev/null

METADATA=$(copilot_get_session_metadata --session "$SESSION_ID")

echo "Session ID: $(echo "$METADATA" | jq -r '.session_id')"
echo "Created at: $(echo "$METADATA" | jq -r '.created_at')"
echo "Turn count: $(echo "$METADATA" | jq -r '.turn_count')"
echo "Model:      $(echo "$METADATA" | jq -r '.model')"
echo "Token usage: $(echo "$METADATA" | jq '.token_usage')"
```

## MCP Server Configuration

**Scenario:** Configure MCP (Model Context Protocol) servers using stdio and HTTP transports.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Stdio transport: launch a local MCP server as a child process
MCP_STDIO='{
  "name": "local-tools",
  "transport": "stdio",
  "command": "node",
  "args": ["./mcp-server/index.js"],
  "env": {"MCP_LOG_LEVEL": "info"}
}'

SESSION_STDIO=$(copilot_create_session \
  --system-prompt "You are an assistant with MCP tools." \
  --mcp-server "$MCP_STDIO")

# HTTP transport: connect to a remote MCP server
MCP_HTTP='{
  "name": "remote-tools",
  "transport": "http",
  "url": "https://mcp.example.com/v1",
  "headers": {"Authorization": "Bearer '"$MCP_API_KEY"'"}
}'

SESSION_HTTP=$(copilot_create_session \
  --system-prompt "You are an assistant with remote tools." \
  --mcp-server "$MCP_HTTP")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_STDIO" \
  --message "Use the local tools to analyze the project.")

echo "Response: $RESPONSE"
```

## Image Generation

**Scenario:** Configure the response format for image generation tasks.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

MODEL_CAPS='{
  "vision": true,
  "image_generation": true
}'

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a creative assistant that generates images." \
  --model-capabilities "$MODEL_CAPS")

RESPONSE_FORMAT='{
  "type": "image",
  "size": "1024x1024",
  "quality": "high"
}'

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Generate an image of a terminal with colorful ASCII art." \
  --response-format "$RESPONSE_FORMAT")

# Parse image results
IMAGE_COUNT=$(echo "$RESPONSE" | jq '.images | length')

if [[ "$IMAGE_COUNT" -gt 0 ]]; then
  for i in $(seq 0 $((IMAGE_COUNT - 1))); do
    url=$(echo "$RESPONSE" | jq -r ".images[$i].url")
    echo "Image $((i+1)) URL: $url"

    # Save base64 image data if returned inline
    b64=$(echo "$RESPONSE" | jq -r ".images[$i].base64_data // empty")
    if [[ -n "$b64" ]]; then
      echo "$b64" | base64 -d > "generated_$((i+1)).png"
      echo "Image $((i+1)) saved to generated_$((i+1)).png"
    fi
  done
else
  echo "Response: $(echo "$RESPONSE" | jq -r '.message')"
fi
```

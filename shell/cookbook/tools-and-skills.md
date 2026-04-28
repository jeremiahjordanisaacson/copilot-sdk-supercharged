# Tools and Skills - Shell/Bash

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Shell/Bash.

## Defining a Simple Tool

**Scenario:** Expose a shell function as a tool that the Copilot model can call during a conversation.

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

# Define a tool that returns the current date and time
get_current_time() {
  date "+%Y-%m-%d %H:%M:%S %Z"
}

copilot_define_tool "get_current_time" "Returns the current date and time" get_current_time

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a helpful assistant with access to tools.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "What is the current time?")

echo "Response: $RESPONSE"
```

## Tool with Parameters

**Scenario:** Define a tool that accepts JSON parameters from the model and processes them.

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

# Calculator tool that reads JSON params from stdin
calculate() {
  local params="$1"
  local a b operation

  a=$(echo "$params" | jq -r '.a')
  b=$(echo "$params" | jq -r '.b')
  operation=$(echo "$params" | jq -r '.operation')

  local result
  case "$operation" in
    add)      result=$(echo "$a + $b" | bc) ;;
    subtract) result=$(echo "$a - $b" | bc) ;;
    multiply) result=$(echo "$a * $b" | bc) ;;
    divide)
      if [[ "$b" == "0" ]]; then
        echo '{"error": "Division by zero"}'
        return 0
      fi
      result=$(echo "scale=4; $a / $b" | bc)
      ;;
    *)
      echo "{\"error\": \"Unknown operation: $operation\"}"
      return 0
      ;;
  esac

  echo "{\"result\": $result}"
}

copilot_define_tool "calculate" "Performs arithmetic: add, subtract, multiply, divide" calculate

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a calculator. Use the calculate tool for math.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "What is 1234 times 5678?")

echo "Response: $RESPONSE"
```

## File System Tools

**Scenario:** Give the model the ability to explore the file system using shell commands.

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

# Tool to read a file
read_file() {
  local params="$1"
  local filepath
  filepath=$(echo "$params" | jq -r '.path')

  if [[ ! -f "$filepath" ]]; then
    echo "{\"error\": \"File not found: $filepath\"}"
    return 0
  fi

  local content
  content=$(head -c 10000 "$filepath")
  local size
  size=$(wc -c < "$filepath")

  # Escape content for JSON
  local escaped
  escaped=$(echo "$content" | jq -Rs '.')

  echo "{\"path\": \"$filepath\", \"content\": $escaped, \"size\": $size}"
}

# Tool to list directory contents
list_directory() {
  local params="$1"
  local dir
  dir=$(echo "$params" | jq -r '.directory // "."')

  if [[ ! -d "$dir" ]]; then
    echo "{\"error\": \"Directory not found: $dir\"}"
    return 0
  fi

  local files
  files=$(ls -1 "$dir" | jq -R . | jq -s .)

  echo "{\"directory\": \"$dir\", \"files\": $files}"
}

# Tool to search file contents
search_files() {
  local params="$1"
  local pattern dir
  pattern=$(echo "$params" | jq -r '.pattern')
  dir=$(echo "$params" | jq -r '.directory // "."')

  local results
  results=$(grep -rl "$pattern" "$dir" 2>/dev/null | head -20 | jq -R . | jq -s .)

  echo "{\"pattern\": \"$pattern\", \"matches\": $results}"
}

copilot_define_tool "read_file" "Reads the contents of a file" read_file
copilot_define_tool "list_directory" "Lists files in a directory" list_directory
copilot_define_tool "search_files" "Searches files for a pattern" search_files

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a file explorer. Use your tools to navigate and read files.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "List the files in the current directory.")

echo "$RESPONSE"
```

## System Information Tools

**Scenario:** Provide the model with tools to inspect the system environment.

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

get_system_info() {
  local os_name kernel hostname uptime
  os_name=$(uname -s)
  kernel=$(uname -r)
  hostname=$(hostname)
  uptime=$(uptime -p 2>/dev/null || uptime)

  cat << EOF
{"os": "$os_name", "kernel": "$kernel", "hostname": "$hostname", "uptime": "$uptime"}
EOF
}

get_disk_usage() {
  df -h --output=target,size,used,avail,pcent 2>/dev/null | \
    tail -n +2 | \
    jq -R 'split(" ") | map(select(. != ""))' | \
    jq -s '.'
}

get_running_processes() {
  local params="$1"
  local limit
  limit=$(echo "$params" | jq -r '.limit // 10')

  ps aux --sort=-%mem | head -n "$((limit + 1))" | \
    tail -n +"2" | \
    awk '{print $1, $2, $3, $4, $11}' | \
    jq -R 'split(" ") | {user: .[0], pid: .[1], cpu: .[2], mem: .[3], command: .[4]}' | \
    jq -s '.'
}

copilot_define_tool "get_system_info" "Returns OS, kernel, hostname, and uptime" get_system_info
copilot_define_tool "get_disk_usage" "Returns disk usage for all mounted filesystems" get_disk_usage
copilot_define_tool "get_running_processes" "Returns top processes by memory usage" get_running_processes

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a system administrator assistant. Use your tools to inspect the system.")

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Give me an overview of this system.")

echo "$RESPONSE"
```

## Orchestrating Sub-Agent Sessions

**Scenario:** Use tool functions to delegate work to specialized sessions.

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

# Create specialized sub-agent sessions
CODE_SESSION=$(copilot_create_session \
  --system-prompt "You generate shell scripts. Output only code, no explanations.")

REVIEW_SESSION=$(copilot_create_session \
  --system-prompt "You review shell scripts for bugs and security issues. Be concise.")

# Tool that delegates to the code generation agent
generate_script() {
  local params="$1"
  local request
  request=$(echo "$params" | jq -r '.request')

  copilot_send_and_wait --session "$CODE_SESSION" --message "$request"
}

# Tool that delegates to the code review agent
review_script() {
  local params="$1"
  local code
  code=$(echo "$params" | jq -r '.code')

  copilot_send_and_wait \
    --session "$REVIEW_SESSION" \
    --message "Review this script for issues:
$code"
}

copilot_define_tool "generate_script" \
  "Generates a shell script from a description" generate_script

copilot_define_tool "review_script" \
  "Reviews a shell script for bugs and security issues" review_script

# Orchestrator session uses the sub-agent tools
ORCHESTRATOR=$(copilot_create_session \
  --system-prompt "You orchestrate tasks. Use generate_script and review_script tools.")

RESPONSE=$(copilot_send_and_wait \
  --session "$ORCHESTRATOR" \
  --message "Generate a script to back up a PostgreSQL database, then review it for security issues.")

echo "$RESPONSE"
```

## Best Practices

1. **Use `jq` for JSON handling** in tool functions to parse parameters and format output reliably.
2. **Return valid JSON from tool handlers** so the model can parse structured responses.
3. **Limit output size** using `head` or truncation to stay within token limits.
4. **Use descriptive tool names** in verb-noun format (e.g., `read_file`, `get_system_info`).
5. **Handle missing or invalid parameters** gracefully, returning JSON error objects.
6. **Wrap tool handler errors** to prevent them from crashing the entire script.
7. **Use sub-agent delegation** for multi-step workflows that need specialized expertise.

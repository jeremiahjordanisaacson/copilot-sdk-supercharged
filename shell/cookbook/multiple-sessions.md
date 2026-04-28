# Multiple Sessions - Shell/Bash

Patterns for managing multiple independent Copilot conversations simultaneously in Shell/Bash.

## Basic Multi-Session Setup

**Scenario:** Run two or more independent conversations, each with its own system prompt and context.

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

# Create two independent sessions
CODE_SESSION=$(copilot_create_session \
  --system-prompt "You are an expert shell programmer. Provide concise scripts.")

DOCS_SESSION=$(copilot_create_session \
  --system-prompt "You are a technical writer. Write clear man-page style docs.")

# Each session has its own conversation history
CODE_RESPONSE=$(copilot_send_and_wait \
  --session "$CODE_SESSION" \
  --message "Write a function to find duplicate files by checksum.")
echo "=== Code ===" 
echo "$CODE_RESPONSE"

DOCS_RESPONSE=$(copilot_send_and_wait \
  --session "$DOCS_SESSION" \
  --message "Write docs for a duplicate file finder utility.")
echo "=== Docs ==="
echo "$DOCS_RESPONSE"
```

## Session Registry with Associative Arrays

**Scenario:** Manage a named pool of sessions using Bash 4+ associative arrays.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

# Requires Bash 4+ for associative arrays
declare -A SESSIONS

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Create or get a named session
session_get_or_create() {
  local name="$1"
  local prompt="$2"

  if [[ -z "${SESSIONS[$name]:-}" ]]; then
    SESSIONS[$name]=$(copilot_create_session --system-prompt "$prompt")
    echo "Created session: $name" >&2
  fi

  echo "${SESSIONS[$name]}"
}

# Send a message to a named session
session_send() {
  local name="$1"
  local message="$2"

  local session_id="${SESSIONS[$name]:-}"
  if [[ -z "$session_id" ]]; then
    echo "ERROR: No session named '$name'" >&2
    return 1
  fi

  copilot_send_and_wait --session "$session_id" --message "$message"
}

# List all active session names
session_list() {
  for name in "${!SESSIONS[@]}"; do
    echo "$name"
  done
}

# Remove a session from the registry
session_remove() {
  local name="$1"
  unset "SESSIONS[$name]"
  echo "Removed session: $name" >&2
}

# Usage
session_get_or_create "frontend" "You are a frontend expert."
session_get_or_create "backend" "You are a backend expert."

RESP=$(session_send "frontend" "How do I debounce input in JavaScript?")
echo "Frontend says: $RESP"

echo "Active sessions:"
session_list
```

## Processing Multiple Inputs Across Sessions

**Scenario:** Read a list of tasks from a file and route each task to the appropriate session.

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

GENERAL_SESSION=$(copilot_create_session \
  --system-prompt "You are a helpful assistant. Give concise answers.")

CODING_SESSION=$(copilot_create_session \
  --system-prompt "You are a coding assistant. Provide only code, no explanations.")

# Process a list of tasks, routing by prefix
process_tasks() {
  local tasks_file="$1"

  while IFS= read -r line; do
    # Skip empty lines and comments
    [[ -z "$line" || "$line" == \#* ]] && continue

    if [[ "$line" == code:* ]]; then
      local task="${line#code:}"
      echo "--- Coding Task: $task ---"
      copilot_send_and_wait --session "$CODING_SESSION" --message "$task"
    else
      echo "--- General Task: $line ---"
      copilot_send_and_wait --session "$GENERAL_SESSION" --message "$line"
    fi

    echo ""
  done < "$tasks_file"
}

# Example: create a tasks file and process it
cat > tasks.txt << 'EOF'
# My task list
code: Write a bash function to validate an email address
What are the POSIX signals?
code: Write a function to parse CSV in bash
Explain the difference between sh and bash
EOF

process_tasks "tasks.txt"
rm -f tasks.txt
```

## Per-User Sessions via Files

**Scenario:** In a script serving multiple users, maintain a session ID file per user.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

SESSION_DIR="./user_sessions"
mkdir -p "$SESSION_DIR"

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Get or create a session for a user
get_user_session() {
  local user_id="$1"
  local session_file="$SESSION_DIR/${user_id}.session"

  if [[ -f "$session_file" ]]; then
    cat "$session_file"
  else
    local session_id
    session_id=$(copilot_create_session \
      --system-prompt "You are a helpful assistant for user $user_id.")
    echo "$session_id" > "$session_file"
    echo "$session_id"
  fi
}

# Chat as a specific user
user_chat() {
  local user_id="$1"
  local message="$2"

  local session_id
  session_id=$(get_user_session "$user_id")

  copilot_send_and_wait --session "$session_id" --message "$message"
}

# End a user session
end_user_session() {
  local user_id="$1"
  rm -f "$SESSION_DIR/${user_id}.session"
  echo "Session ended for user: $user_id" >&2
}

# Usage
RESP1=$(user_chat "alice" "How do I use awk to sum a column?")
echo "Alice: $RESP1"

RESP2=$(user_chat "bob" "How do I use sed for in-place editing?")
echo "Bob: $RESP2"

# Clean up
end_user_session "alice"
rm -rf "$SESSION_DIR"
```

## Sequential Pipeline Across Sessions

**Scenario:** Chain the output of one session as input to the next, creating a multi-stage pipeline.

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

DRAFT_SESSION=$(copilot_create_session \
  --system-prompt "You write first drafts of shell scripts. Output only code.")

REVIEW_SESSION=$(copilot_create_session \
  --system-prompt "You review shell scripts for bugs, security issues, and style. Be concise.")

POLISH_SESSION=$(copilot_create_session \
  --system-prompt "You improve shell scripts based on review feedback. Output only code.")

# Stage 1: Draft
echo "=== Stage 1: Drafting ==="
DRAFT=$(copilot_send_and_wait \
  --session "$DRAFT_SESSION" \
  --message "Write a script that safely deletes files older than 30 days.")
echo "$DRAFT"

# Stage 2: Review
echo "=== Stage 2: Reviewing ==="
REVIEW=$(copilot_send_and_wait \
  --session "$REVIEW_SESSION" \
  --message "Review this script for issues:
$DRAFT")
echo "$REVIEW"

# Stage 3: Polish
echo "=== Stage 3: Polishing ==="
FINAL=$(copilot_send_and_wait \
  --session "$POLISH_SESSION" \
  --message "Improve this script based on the review feedback:

Script:
$DRAFT

Feedback:
$REVIEW")
echo "$FINAL"
```

## Best Practices

1. **Use associative arrays** (Bash 4+) for named session registries instead of positional variables.
2. **Always register a cleanup trap** to stop the client on script exit.
3. **Store session IDs in files** when sessions need to survive across separate script invocations.
4. **Route messages by prefix or pattern** to direct work to the right specialized session.
5. **Chain sessions in pipelines** for multi-stage workflows like draft, review, and polish.
6. **Clean up session files** when they are no longer needed to avoid stale data.

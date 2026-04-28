# Persisting Sessions - Shell/Bash

Patterns for saving and resuming Copilot sessions across script restarts in Shell/Bash.

## Basic Save and Load

**Scenario:** Save the session state to a file so a future script invocation can pick up where it left off.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

STATE_FILE="./session_state.json"

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Create session and have a conversation
SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a helpful assistant.")

copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "Remember: the project name is Phoenix."

# Save session state to file
copilot_save_session --session "$SESSION_ID" --output "$STATE_FILE"
echo "Session saved to $STATE_FILE"
```

## Resuming a Saved Session

**Scenario:** Load a previously saved session and continue the conversation.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

STATE_FILE="./session_state.json"

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

if [[ -f "$STATE_FILE" ]]; then
  echo "Resuming saved session..."
  SESSION_ID=$(copilot_create_session \
    --system-prompt "You are a helpful assistant." \
    --state-file "$STATE_FILE")
else
  echo "Starting fresh session..."
  SESSION_ID=$(copilot_create_session \
    --system-prompt "You are a helpful assistant.")
fi

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "What is the project name?")
echo "Response: $RESPONSE"

# Save updated state
copilot_save_session --session "$SESSION_ID" --output "$STATE_FILE"
```

## Named Session Store

**Scenario:** Manage multiple saved sessions in a directory, each identified by name.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

SESSION_STORE_DIR="./session_store"
mkdir -p "$SESSION_STORE_DIR"

# Save a named session
store_save() {
  local name="$1"
  local session_id="$2"
  copilot_save_session \
    --session "$session_id" \
    --output "$SESSION_STORE_DIR/${name}.json"
  echo "Saved session: $name" >&2
}

# Load a named session (returns empty string if not found)
store_load_path() {
  local name="$1"
  local path="$SESSION_STORE_DIR/${name}.json"
  if [[ -f "$path" ]]; then
    echo "$path"
  fi
}

# Delete a named session
store_delete() {
  local name="$1"
  rm -f "$SESSION_STORE_DIR/${name}.json"
  echo "Deleted session: $name" >&2
}

# List all saved session names
store_list() {
  for f in "$SESSION_STORE_DIR"/*.json; do
    [[ -f "$f" ]] || continue
    basename "$f" .json
  done
}

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

# Create or resume a session
SESSION_NAME="project-alpha"
STATE_PATH=$(store_load_path "$SESSION_NAME")

if [[ -n "$STATE_PATH" ]]; then
  echo "Resuming session: $SESSION_NAME"
  SESSION_ID=$(copilot_create_session \
    --system-prompt "You are a project planning assistant." \
    --state-file "$STATE_PATH")
else
  echo "Creating new session: $SESSION_NAME"
  SESSION_ID=$(copilot_create_session \
    --system-prompt "You are a project planning assistant.")
fi

RESPONSE=$(copilot_send_and_wait \
  --session "$SESSION_ID" \
  --message "What tasks are left?")
echo "$RESPONSE"

# Save after interaction
store_save "$SESSION_NAME" "$SESSION_ID"

echo "All saved sessions:"
store_list
```

## Auto-Save Wrapper

**Scenario:** Automatically save the session after every message exchange so progress is never lost.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

STATE_FILE="./autosave_session.json"

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

# Wrapper that sends a message and saves state afterward
send_and_save() {
  local session_id="$1"
  local message="$2"
  local save_path="$3"

  local response
  response=$(copilot_send_and_wait \
    --session "$session_id" \
    --message "$message")

  # Auto-save after each turn
  copilot_save_session --session "$session_id" --output "$save_path"

  echo "$response"
}

copilot_init
copilot_start

# Resume or start fresh
if [[ -f "$STATE_FILE" ]]; then
  SESSION_ID=$(copilot_create_session \
    --system-prompt "You are a helpful assistant." \
    --state-file "$STATE_FILE")
else
  SESSION_ID=$(copilot_create_session \
    --system-prompt "You are a helpful assistant.")
fi

# Each call saves automatically
send_and_save "$SESSION_ID" "Hello, remember I like bash scripts." "$STATE_FILE"
send_and_save "$SESSION_ID" "What do I like?" "$STATE_FILE"
```

## Session with Metadata

**Scenario:** Store metadata (timestamps, description, turn count) alongside session state.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

STORE_DIR="./sessions_with_meta"
mkdir -p "$STORE_DIR"

# Save session state with metadata sidecar
save_with_metadata() {
  local name="$1"
  local session_id="$2"
  local description="${3:-}"
  local turn_count="${4:-0}"

  local state_file="$STORE_DIR/${name}.json"
  local meta_file="$STORE_DIR/${name}.meta"

  copilot_save_session --session "$session_id" --output "$state_file"

  cat > "$meta_file" << EOF
name=$name
description=$description
turn_count=$turn_count
saved_at=$(date -u "+%Y-%m-%dT%H:%M:%SZ")
EOF

  echo "Saved session with metadata: $name" >&2
}

# Load metadata for a session
load_metadata() {
  local name="$1"
  local meta_file="$STORE_DIR/${name}.meta"

  if [[ -f "$meta_file" ]]; then
    cat "$meta_file"
  else
    echo "No metadata found for: $name" >&2
  fi
}

# List sessions with their metadata
list_sessions_with_meta() {
  for meta_file in "$STORE_DIR"/*.meta; do
    [[ -f "$meta_file" ]] || continue
    echo "---"
    cat "$meta_file"
  done
}

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session \
  --system-prompt "You are a helpful assistant.")

copilot_send_and_wait --session "$SESSION_ID" --message "Hello!"

save_with_metadata "demo" "$SESSION_ID" "Cookbook demo session" 1

echo "=== Session Metadata ==="
load_metadata "demo"

# Cleanup
rm -rf "$STORE_DIR"
```

## Best Practices

1. **Use a dedicated directory** for session state files to keep things organized.
2. **Auto-save after every turn** in interactive or long-running scripts to prevent data loss.
3. **Store metadata in sidecar files** (`.meta`) alongside state files for easy inspection.
4. **Check for state file existence** before attempting to resume; fall back to a fresh session.
5. **Use `trap cleanup EXIT`** to ensure the client stops even if saving fails.
6. **Clean up stale session files** periodically to prevent disk usage growth.
7. **Use meaningful session names** derived from the task or user, not random IDs.

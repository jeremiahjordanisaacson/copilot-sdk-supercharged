# Error Handling - Shell/Bash

Patterns for handling errors gracefully in the Copilot SDK for Shell/Bash, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with Exit Codes

**Scenario:** Detect and handle failures from SDK function calls using exit codes.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

# Start the client and check for failure
if ! copilot_init; then
  echo "ERROR: Failed to initialize Copilot client" >&2
  exit 1
fi

if ! copilot_start; then
  echo "ERROR: Failed to start Copilot client" >&2
  exit 1
fi

# Create a session
SESSION_ID=$(copilot_create_session --system-prompt "You are a helpful assistant.")
if [[ -z "$SESSION_ID" ]]; then
  echo "ERROR: Failed to create session" >&2
  copilot_stop
  exit 1
fi

# Send a message
RESPONSE=$(copilot_send_and_wait --session "$SESSION_ID" --message "Hello!")
echo "Response: $RESPONSE"

copilot_stop
```

## Cleanup with trap

**Scenario:** Ensure the Copilot client is always stopped, even if the script exits unexpectedly.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

cleanup() {
  echo "Cleaning up..." >&2
  copilot_stop 2>/dev/null || true
}

# Register cleanup to run on EXIT, INT, and TERM signals
trap cleanup EXIT INT TERM

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session --system-prompt "You are a helpful assistant.")

RESPONSE=$(copilot_send_and_wait --session "$SESSION_ID" --message "Explain shell traps.")
echo "$RESPONSE"

# cleanup runs automatically when the script exits
```

## Retry Logic

**Scenario:** Retry transient failures automatically with configurable attempts and delay.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

retry() {
  local max_attempts="$1"
  local delay="$2"
  shift 2
  local cmd=("$@")

  local attempt=1
  while [[ $attempt -le $max_attempts ]]; do
    if output=$("${cmd[@]}" 2>&1); then
      echo "$output"
      return 0
    fi

    if [[ $attempt -lt $max_attempts ]]; then
      echo "Attempt $attempt/$max_attempts failed. Retrying in ${delay}s..." >&2
      sleep "$delay"
    fi

    attempt=$((attempt + 1))
  done

  echo "ERROR: All $max_attempts attempts failed" >&2
  return 1
}

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session --system-prompt "You are a helpful assistant.")

# Retry the send_and_wait call up to 3 times with a 2-second delay
RESPONSE=$(retry 3 2 copilot_send_and_wait --session "$SESSION_ID" --message "Hello!")
echo "Response: $RESPONSE"
```

## Validating Prerequisites

**Scenario:** Check that all required dependencies and environment variables are set before running.

```bash
#!/usr/bin/env bash
set -euo pipefail

check_prerequisites() {
  local missing=()

  # Check for required commands
  if ! command -v copilot_sdk.sh &>/dev/null; then
    # Try sourcing from known locations
    if [[ -f "./lib/copilot_sdk.sh" ]]; then
      source "./lib/copilot_sdk.sh"
    else
      missing+=("copilot_sdk.sh")
    fi
  fi

  # Check environment variables
  if [[ -z "${COPILOT_CLI_PATH:-}" ]]; then
    # Try to find the CLI
    if command -v copilot-cli &>/dev/null; then
      export COPILOT_CLI_PATH=$(command -v copilot-cli)
    else
      missing+=("COPILOT_CLI_PATH environment variable")
    fi
  fi

  if [[ ${#missing[@]} -gt 0 ]]; then
    echo "ERROR: Missing prerequisites:" >&2
    for item in "${missing[@]}"; do
      echo "  - $item" >&2
    done
    return 1
  fi

  return 0
}

if ! check_prerequisites; then
  exit 1
fi

source copilot_sdk.sh

copilot_init
copilot_start

echo "All prerequisites met. Client started."
copilot_stop
```

## Timeout Handling

**Scenario:** Enforce a maximum duration on SDK calls to prevent hanging.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

# Run a command with a timeout
run_with_timeout() {
  local timeout_seconds="$1"
  shift

  if command -v timeout &>/dev/null; then
    timeout "$timeout_seconds" "$@"
  else
    # Fallback for systems without timeout command
    "$@" &
    local pid=$!
    (
      sleep "$timeout_seconds"
      kill "$pid" 2>/dev/null
    ) &
    local watchdog=$!
    wait "$pid" 2>/dev/null
    local status=$?
    kill "$watchdog" 2>/dev/null
    wait "$watchdog" 2>/dev/null
    return $status
  fi
}

cleanup() {
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

copilot_init
copilot_start

SESSION_ID=$(copilot_create_session --system-prompt "You are a helpful assistant.")

# Send with a 30-second timeout
if RESPONSE=$(run_with_timeout 30 copilot_send_and_wait \
    --session "$SESSION_ID" \
    --message "Explain timeout handling."); then
  echo "Response: $RESPONSE"
else
  echo "ERROR: Request timed out after 30 seconds" >&2
  exit 1
fi
```

## Logging Errors to a File

**Scenario:** Log all errors to a file while keeping stdout clean for output.

```bash
#!/usr/bin/env bash
set -euo pipefail

source copilot_sdk.sh

LOG_FILE="copilot_errors.log"

log_error() {
  local timestamp
  timestamp=$(date -u "+%Y-%m-%dT%H:%M:%SZ")
  echo "[$timestamp] ERROR: $*" >> "$LOG_FILE"
  echo "ERROR: $*" >&2
}

log_info() {
  local timestamp
  timestamp=$(date -u "+%Y-%m-%dT%H:%M:%SZ")
  echo "[$timestamp] INFO: $*" >> "$LOG_FILE"
}

cleanup() {
  log_info "Stopping client"
  copilot_stop 2>/dev/null || true
}
trap cleanup EXIT

if ! copilot_init 2>>"$LOG_FILE"; then
  log_error "Failed to initialize client"
  exit 1
fi

if ! copilot_start 2>>"$LOG_FILE"; then
  log_error "Failed to start client"
  exit 1
fi

log_info "Client started successfully"

SESSION_ID=$(copilot_create_session --system-prompt "You are a helpful assistant.")

if RESPONSE=$(copilot_send_and_wait --session "$SESSION_ID" --message "Hello!" 2>>"$LOG_FILE"); then
  echo "$RESPONSE"
  log_info "Message sent and response received"
else
  log_error "Failed to get response from session"
  exit 1
fi
```

## Best Practices

1. **Always use `set -euo pipefail`** at the top of scripts to catch errors early.
2. **Use `trap cleanup EXIT`** to guarantee the client is stopped on any exit path.
3. **Redirect errors to stderr** with `>&2` and keep stdout for program output.
4. **Validate prerequisites** before starting the client to give clear error messages.
5. **Implement retry logic** for network-sensitive operations with configurable limits.
6. **Use timeout wrappers** to prevent indefinite hangs in production scripts.
7. **Log errors with timestamps** to a file for post-mortem debugging.

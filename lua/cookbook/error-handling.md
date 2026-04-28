# Error Handling - Lua

Patterns for handling errors gracefully in the Copilot SDK for Lua, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with pcall

**Scenario:** You want to catch errors from SDK calls without crashing your application.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({
  cli_path = os.getenv("COPILOT_CLI_PATH"),
})

-- Wrap SDK calls in pcall for safe error handling
local ok, err = pcall(function()
  client:start()
end)

if not ok then
  print("Failed to start client: " .. tostring(err))
  os.exit(1)
end

-- Safe session creation
local session_ok, session_or_err = pcall(function()
  return client:create_session({
    system_prompt = "You are a helpful assistant.",
  })
end)

if not session_ok then
  print("Session creation failed: " .. tostring(session_or_err))
  client:stop()
  os.exit(1)
end

local session = session_or_err
```

## Extended Error Handling with xpcall

**Scenario:** You need a full stack trace when errors occur for debugging.

```lua
local copilot = require("copilot")

local function error_handler(err)
  -- Capture stack trace along with the error message
  return debug.traceback(tostring(err), 2)
end

local function run_session(client, prompt)
  local session = client:create_session({
    system_prompt = "You are a helpful assistant.",
  })

  local response = session:send_and_wait({
    message = prompt,
  })

  return response
end

local client = copilot.CopilotClient:new({})
client:start()

local ok, result = xpcall(run_session, error_handler, client, "Hello!")

if ok then
  print("Response: " .. result.message)
else
  -- result contains the full traceback
  io.stderr:write("Error with traceback:\n" .. result .. "\n")
end

client:stop()
```

## Retry Logic

**Scenario:** Transient failures (network blips, timeouts) should be retried automatically.

```lua
local copilot = require("copilot")

local function retry(fn, max_attempts, delay_seconds)
  local last_err
  for attempt = 1, max_attempts do
    local ok, result = pcall(fn)
    if ok then
      return result
    end
    last_err = result
    if attempt < max_attempts then
      print(string.format(
        "Attempt %d/%d failed: %s. Retrying in %ds...",
        attempt, max_attempts, tostring(last_err), delay_seconds
      ))
      -- Simple sleep using os.execute
      os.execute("sleep " .. delay_seconds)
    end
  end
  error("All " .. max_attempts .. " attempts failed. Last error: " .. tostring(last_err))
end

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

-- Retry sending a message up to 3 times with a 2-second delay
local response = retry(function()
  return session:send_and_wait({
    message = "Summarize the benefits of Lua.",
    timeout = 30,
  })
end, 3, 2)

print("Response: " .. response.message)
client:stop()
```

## Cleanup with a Finally Pattern

**Scenario:** Ensure the client is always stopped, even if an error occurs mid-session.

```lua
local copilot = require("copilot")

local function with_client(opts, fn)
  local client = copilot.CopilotClient:new(opts)
  client:start()

  local ok, result = pcall(fn, client)

  -- Always stop the client, regardless of success or failure
  local stop_ok, stop_err = pcall(function()
    client:stop()
  end)

  if not stop_ok then
    io.stderr:write("Warning: failed to stop client: " .. tostring(stop_err) .. "\n")
  end

  if not ok then
    error(result)
  end

  return result
end

-- Usage
with_client({}, function(client)
  local session = client:create_session({
    system_prompt = "You are a helpful assistant.",
  })

  local response = session:send_and_wait({
    message = "What is error handling?",
  })

  print("Response: " .. response.message)
end)
```

## Handling Specific Error Types

**Scenario:** You want to react differently to connection errors versus timeout errors.

```lua
local copilot = require("copilot")

local function classify_error(err_msg)
  local msg = tostring(err_msg):lower()
  if msg:find("timeout") then
    return "timeout"
  elseif msg:find("connection") or msg:find("refused") then
    return "connection"
  elseif msg:find("auth") or msg:find("token") then
    return "auth"
  else
    return "unknown"
  end
end

local client = copilot.CopilotClient:new({})

local ok, err = pcall(function()
  client:start()
end)

if not ok then
  local kind = classify_error(err)
  if kind == "connection" then
    print("Cannot connect to Copilot CLI. Is it installed and running?")
  elseif kind == "auth" then
    print("Authentication error. Check your credentials.")
  else
    print("Unexpected error: " .. tostring(err))
  end
  os.exit(1)
end

client:stop()
```

## Best Practices

1. **Always use pcall/xpcall** around SDK calls to prevent unhandled crashes.
2. **Use xpcall with debug.traceback** during development for full error context.
3. **Implement a cleanup wrapper** (like `with_client`) to guarantee resource release.
4. **Retry transient errors** with exponential backoff for production systems.
5. **Classify errors** by pattern matching on the error message to take targeted recovery actions.
6. **Log errors to stderr** using `io.stderr:write()` to keep stdout clean for program output.

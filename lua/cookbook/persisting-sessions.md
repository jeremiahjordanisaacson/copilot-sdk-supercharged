# Persisting Sessions - Lua

Patterns for saving and resuming Copilot sessions across application restarts in Lua.

## Basic Save and Load with JSON

**Scenario:** Save session state to a JSON file so it can be restored later.

```lua
local copilot = require("copilot")
local json = require("cjson") -- or dkjson, lunajson, etc.

local function save_session_state(session, filepath)
  local state = session:get_state()
  local file = io.open(filepath, "w")
  if not file then
    error("Cannot open file for writing: " .. filepath)
  end
  file:write(json.encode(state))
  file:close()
  print("Session saved to " .. filepath)
end

local function load_session_state(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return nil
  end
  local content = file:read("*a")
  file:close()
  return json.decode(content)
end

-- Save a session
local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

session:send_and_wait({ message = "Remember that my name is Alice." })
save_session_state(session, "session_state.json")

client:stop()
```

## Resuming a Saved Session

**Scenario:** Restore a previously saved session and continue the conversation.

```lua
local copilot = require("copilot")
local json = require("cjson")

local function load_session_state(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return nil
  end
  local content = file:read("*a")
  file:close()
  return json.decode(content)
end

local client = copilot.CopilotClient:new({})
client:start()

local saved_state = load_session_state("session_state.json")

local session
if saved_state then
  -- Resume from saved state
  session = client:create_session({
    system_prompt = "You are a helpful assistant.",
    state = saved_state,
  })
  print("Resumed existing session")
else
  -- Start fresh
  session = client:create_session({
    system_prompt = "You are a helpful assistant.",
  })
  print("Started new session")
end

local response = session:send_and_wait({ message = "What is my name?" })
print("Response: " .. response.message)

client:stop()
```

## Session Store with Multiple Named Sessions

**Scenario:** Persist multiple named sessions to disk, each in its own file.

```lua
local copilot = require("copilot")
local json = require("cjson")

local SessionStore = {}
SessionStore.__index = SessionStore

function SessionStore:new(directory)
  -- Ensure directory exists
  os.execute("mkdir -p " .. directory)
  return setmetatable({
    directory = directory,
  }, self)
end

function SessionStore:filepath(name)
  return self.directory .. "/" .. name .. ".json"
end

function SessionStore:save(name, session)
  local state = session:get_state()
  local file = io.open(self:filepath(name), "w")
  if not file then
    error("Cannot write session file: " .. self:filepath(name))
  end
  file:write(json.encode(state))
  file:close()
end

function SessionStore:load(name)
  local file = io.open(self:filepath(name), "r")
  if not file then
    return nil
  end
  local content = file:read("*a")
  file:close()
  return json.decode(content)
end

function SessionStore:delete(name)
  os.remove(self:filepath(name))
end

function SessionStore:list()
  local names = {}
  local handle = io.popen('ls "' .. self.directory .. '"/*.json 2>/dev/null')
  if handle then
    for line in handle:lines() do
      local name = line:match("([^/]+)%.json$")
      if name then
        table.insert(names, name)
      end
    end
    handle:close()
  end
  return names
end

-- Usage
local client = copilot.CopilotClient:new({})
client:start()

local store = SessionStore:new("./sessions")

-- Create and save a session
local session = client:create_session({
  system_prompt = "You are a project planning assistant.",
})

session:send_and_wait({ message = "We are building a REST API in Lua." })
store:save("project-alpha", session)

-- Later, restore it
local saved = store:load("project-alpha")
if saved then
  local restored = client:create_session({
    system_prompt = "You are a project planning assistant.",
    state = saved,
  })
  local resp = restored:send_and_wait({ message = "What are we building?" })
  print(resp.message)
end

-- List all saved sessions
print("Saved sessions: " .. table.concat(store:list(), ", "))

client:stop()
```

## Auto-Save on Every Turn

**Scenario:** Automatically save session state after every message exchange so nothing is lost.

```lua
local copilot = require("copilot")
local json = require("cjson")

local function auto_saving_send(session, opts, save_path)
  local response = session:send_and_wait(opts)

  -- Save state after each turn
  local state = session:get_state()
  local file = io.open(save_path, "w")
  if file then
    file:write(json.encode(state))
    file:close()
  end

  return response
end

-- Usage
local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

local save_path = "./current_session.json"

-- Each call auto-saves
auto_saving_send(session, { message = "Hello!" }, save_path)
auto_saving_send(session, { message = "What can you help with?" }, save_path)

print("Session auto-saved to " .. save_path)

client:stop()
```

## Session State with Metadata

**Scenario:** Save extra metadata alongside the session state (creation time, turn count, description).

```lua
local copilot = require("copilot")
local json = require("cjson")

local function save_with_metadata(session, filepath, metadata)
  local envelope = {
    metadata = metadata,
    state = session:get_state(),
    saved_at = os.date("!%Y-%m-%dT%H:%M:%SZ"),
  }
  local file = io.open(filepath, "w")
  if not file then
    error("Cannot write: " .. filepath)
  end
  file:write(json.encode(envelope))
  file:close()
end

local function load_with_metadata(filepath)
  local file = io.open(filepath, "r")
  if not file then
    return nil
  end
  local content = file:read("*a")
  file:close()
  return json.decode(content)
end

-- Usage
local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

session:send_and_wait({ message = "Hello!" })

save_with_metadata(session, "session_with_meta.json", {
  description = "Test session for cookbook example",
  created_by = "cookbook",
  turn_count = 1,
})

-- Load and inspect
local envelope = load_with_metadata("session_with_meta.json")
if envelope then
  print("Saved at: " .. envelope.saved_at)
  print("Description: " .. envelope.metadata.description)
end

client:stop()
```

## Best Practices

1. **Use a well-tested JSON library** (cjson, dkjson, or lunajson) for serialization.
2. **Save after every meaningful exchange** to minimize data loss on crashes.
3. **Store metadata** (timestamps, descriptions, turn counts) alongside session state for easier management.
4. **Use a directory-based store** for multi-session applications to keep files organized.
5. **Handle missing files gracefully** by returning nil from load functions instead of raising errors.
6. **Clean up old session files** periodically to avoid disk usage growth in long-running applications.

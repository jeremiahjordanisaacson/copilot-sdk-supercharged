# Multiple Sessions - Lua

Patterns for managing multiple independent Copilot conversations simultaneously in Lua.

## Basic Multi-Session Setup

**Scenario:** You need two or more independent conversations running at the same time, each with its own context.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

-- Create independent sessions with different system prompts
local code_session = client:create_session({
  system_prompt = "You are an expert Lua programmer. Provide concise code examples.",
})

local docs_session = client:create_session({
  system_prompt = "You are a technical writer. Write clear, concise documentation.",
})

-- Each session maintains its own conversation history
local code_response = code_session:send_and_wait({
  message = "Write a function to reverse a table in Lua.",
})
print("Code:\n" .. code_response.message)

local docs_response = docs_session:send_and_wait({
  message = "Write docs for a table reversal function.",
})
print("Docs:\n" .. docs_response.message)

client:stop()
```

## Session Registry

**Scenario:** Manage a dynamic pool of named sessions that can be looked up, created on demand, and cleaned up together.

```lua
local copilot = require("copilot")

local SessionRegistry = {}
SessionRegistry.__index = SessionRegistry

function SessionRegistry:new(client)
  return setmetatable({
    client = client,
    sessions = {},
  }, self)
end

function SessionRegistry:get_or_create(name, config)
  if not self.sessions[name] then
    self.sessions[name] = self.client:create_session(config)
    print("Created session: " .. name)
  end
  return self.sessions[name]
end

function SessionRegistry:remove(name)
  self.sessions[name] = nil
  print("Removed session: " .. name)
end

function SessionRegistry:list()
  local names = {}
  for name, _ in pairs(self.sessions) do
    table.insert(names, name)
  end
  return names
end

function SessionRegistry:send(name, message)
  local session = self.sessions[name]
  if not session then
    error("No session found with name: " .. name)
  end
  return session:send_and_wait({ message = message })
end

-- Usage
local client = copilot.CopilotClient:new({})
client:start()

local registry = SessionRegistry:new(client)

registry:get_or_create("frontend", {
  system_prompt = "You are a frontend expert.",
})

registry:get_or_create("backend", {
  system_prompt = "You are a backend expert.",
})

local resp = registry:send("frontend", "How do I lazy-load images?")
print(resp.message)

print("Active sessions: " .. table.concat(registry:list(), ", "))

client:stop()
```

## Per-User Sessions

**Scenario:** In a multi-user application, each user gets their own isolated session.

```lua
local copilot = require("copilot")

local UserSessions = {}
UserSessions.__index = UserSessions

function UserSessions:new(client, default_config)
  return setmetatable({
    client = client,
    default_config = default_config or {},
    user_sessions = {},
  }, self)
end

function UserSessions:get_session(user_id)
  if not self.user_sessions[user_id] then
    local config = {}
    for k, v in pairs(self.default_config) do
      config[k] = v
    end
    config.system_prompt = (config.system_prompt or "You are a helpful assistant.")
      .. "\nYou are assisting user: " .. user_id

    self.user_sessions[user_id] = self.client:create_session(config)
  end
  return self.user_sessions[user_id]
end

function UserSessions:chat(user_id, message)
  local session = self:get_session(user_id)
  return session:send_and_wait({ message = message })
end

function UserSessions:end_session(user_id)
  self.user_sessions[user_id] = nil
end

-- Usage
local client = copilot.CopilotClient:new({})
client:start()

local user_sessions = UserSessions:new(client, {
  system_prompt = "You are a helpful coding assistant.",
})

-- Simulate two users chatting
local resp1 = user_sessions:chat("alice", "How do I read a file in Lua?")
print("Alice got: " .. resp1.message)

local resp2 = user_sessions:chat("bob", "How do I write to a file in Lua?")
print("Bob got: " .. resp2.message)

-- Clean up a single user
user_sessions:end_session("alice")

client:stop()
```

## Round-Robin Across Sessions

**Scenario:** Distribute requests across multiple sessions to compare responses or balance load.

```lua
local copilot = require("copilot")

local RoundRobinPool = {}
RoundRobinPool.__index = RoundRobinPool

function RoundRobinPool:new(client, configs)
  local sessions = {}
  for i, config in ipairs(configs) do
    sessions[i] = client:create_session(config)
  end

  return setmetatable({
    sessions = sessions,
    index = 0,
  }, self)
end

function RoundRobinPool:next_session()
  self.index = (self.index % #self.sessions) + 1
  return self.sessions[self.index], self.index
end

function RoundRobinPool:send(message)
  local session, idx = self:next_session()
  local response = session:send_and_wait({ message = message })
  return response, idx
end

-- Usage
local client = copilot.CopilotClient:new({})
client:start()

local pool = RoundRobinPool:new(client, {
  { system_prompt = "You are a concise assistant. Keep answers under 50 words." },
  { system_prompt = "You are a detailed assistant. Provide thorough explanations." },
  { system_prompt = "You are a creative assistant. Use analogies and examples." },
})

local questions = {
  "What is a coroutine?",
  "Explain metatables.",
  "What is tail call optimization?",
}

for _, question in ipairs(questions) do
  local response, session_idx = pool:send(question)
  print(string.format("Session %d answered '%s':\n%s\n", session_idx, question, response.message))
end

client:stop()
```

## Best Practices

1. **One client, many sessions.** Create a single `CopilotClient` and spawn sessions from it to share the underlying connection.
2. **Use a registry or pool pattern** to manage session lifecycles and avoid losing references.
3. **Set nil to release sessions** when they are no longer needed so Lua's garbage collector can reclaim memory.
4. **Give each session a distinct system prompt** so it stays focused on its purpose.
5. **Wrap session operations in pcall** to isolate failures in one session from affecting others.
6. **Avoid unbounded session creation.** Track active sessions and set a maximum limit for production use.

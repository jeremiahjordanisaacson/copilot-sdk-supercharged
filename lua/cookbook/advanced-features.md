# Advanced v2.0 Features - Lua

Recipes for v2.0 SDK features in Lua: per-session auth, SessionFs, commands, system prompts, skills, config discovery, image generation, and more.

## Per-Session Authentication

**Scenario:** Provide a GitHub token on each session for user-scoped auth instead of a global token.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  github_token = os.getenv("GITHUB_TOKEN_USER_A"),
  system_prompt = "You are a helpful assistant.",
})

local response = session:send_and_wait({
  message = "Summarize my recent pull requests.",
})

print("Response: " .. response.message)

-- Create a second session with a different user token
local session_b = client:create_session({
  github_token = os.getenv("GITHUB_TOKEN_USER_B"),
  system_prompt = "You are a code reviewer.",
})

client:stop()
```

## Session Idle Timeout

**Scenario:** Automatically expire sessions after a period of inactivity.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  session_idle_timeout_seconds = 300, -- 5 minutes
  system_prompt = "You are a helpful assistant.",
})

local response = session:send_and_wait({
  message = "Hello!",
})

print("Response: " .. response.message)

-- Session automatically expires after 300s of inactivity.
-- Sending a message after timeout returns an error.

client:stop()
```

## SessionFs (Session Filesystem)

**Scenario:** Configure a session filesystem provider with I/O operations for file-based context.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

-- Define a SessionFs provider with 10 I/O operations
local session_fs = {
  read_file = function(params)
    local f = io.open(params.path, "r")
    if not f then return json.encode({ error = "File not found" }) end
    local content = f:read("*a")
    f:close()
    return content
  end,
  write_file = function(params)
    local f = io.open(params.path, "w")
    f:write(params.content)
    f:close()
    return json.encode({ success = true })
  end,
  list_directory = function(params)
    local handle = io.popen('ls -1 "' .. params.path .. '"')
    local result = handle:read("*a")
    handle:close()
    return result
  end,
  create_directory = function(params)
    os.execute('mkdir -p "' .. params.path .. '"')
    return json.encode({ success = true })
  end,
  delete_file = function(params)
    os.remove(params.path)
    return json.encode({ success = true })
  end,
  file_exists = function(params)
    local f = io.open(params.path, "r")
    if f then f:close() return "true" else return "false" end
  end,
  get_file_info = function(params)
    local handle = io.popen('stat -c "%s %Y" "' .. params.path .. '"')
    local result = handle:read("*a")
    handle:close()
    return result
  end,
  copy_file = function(params)
    os.execute('cp "' .. params.source .. '" "' .. params.destination .. '"')
    return json.encode({ success = true })
  end,
  move_file = function(params)
    os.rename(params.source, params.destination)
    return json.encode({ success = true })
  end,
  search_files = function(params)
    local handle = io.popen('grep -rl "' .. params.pattern .. '" "' .. params.path .. '"')
    local result = handle:read("*a")
    handle:close()
    return result
  end,
}

local session = client:create_session({
  system_prompt = "You are a file management assistant.",
  session_fs = {
    provider = session_fs,
    root_path = "/workspace/project",
    writable = true,
  },
})

local response = session:send_and_wait({
  message = "List all files in the current project directory.",
})

print("Response: " .. response.message)

client:stop()
```

## Commands and UI Elicitation

**Scenario:** Register commands and handle UI elicitation requests from the model.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

-- Define available commands
local commands = {
  {
    name = "deploy",
    description = "Deploy the application to a target environment",
    parameters = {
      { name = "environment", type = "string", required = true },
      { name = "version", type = "string", required = false },
    },
  },
  {
    name = "rollback",
    description = "Roll back the most recent deployment",
    parameters = {
      { name = "environment", type = "string", required = true },
    },
  },
}

-- Handle elicitation: the model asks the user for input
local function elicitation_handler(request)
  print("Model asks: " .. request.message)
  for i, option in ipairs(request.options or {}) do
    print(string.format("  %d) %s", i, option))
  end
  io.write("Your choice: ")
  local answer = io.read("*l")
  return { response = answer }
end

local session = client:create_session({
  system_prompt = "You are a deployment assistant.",
  commands = commands,
  elicitation_handler = elicitation_handler,
})

local response = session:send_and_wait({
  message = "/deploy",
})

print("Response: " .. response.message)

client:stop()
```

## System Prompt Customization

**Scenario:** Use replace and customize modes with sections to control the system prompt.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

-- Replace mode: completely replace the default system prompt
local session_replace = client:create_session({
  system_prompt = {
    mode = "replace",
    content = "You are a Lua programming expert. Only answer Lua questions.",
  },
})

-- Customize mode: add sections before/after the default prompt
local session_custom = client:create_session({
  system_prompt = {
    mode = "customize",
    sections = {
      {
        position = "before",
        content = "You are assisting a senior Lua developer.",
      },
      {
        position = "after",
        content = table.concat({
          "Always prefer LuaJIT-compatible patterns.",
          "Use metatables for OOP where appropriate.",
          "Cite the Lua 5.4 reference manual when relevant.",
        }, "\n"),
      },
    },
  },
})

local response = session_custom:send_and_wait({
  message = "How do I implement iterators in Lua?",
})

print("Response: " .. response.message)

client:stop()
```

## Per-Agent Skills

**Scenario:** Configure skill directories and disable specific skills per agent.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a code assistant with limited skills.",
  skill_directories = {
    "/home/user/.copilot/skills",
    "/project/.copilot-skills",
  },
  disabled_skills = {
    "web-search",
    "image-generation",
  },
})

local response = session:send_and_wait({
  message = "Refactor this function to use tail recursion.",
})

print("Response: " .. response.message)

client:stop()
```

## Per-Agent Tool Visibility

**Scenario:** Hide specific tools from the model for a given session.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a read-only code reviewer.",
  excluded_tools = {
    "file_write",
    "shell_execute",
    "git_push",
  },
})

local response = session:send_and_wait({
  message = "Review this code and suggest improvements.",
})

print("Response: " .. response.message)

client:stop()
```

## Runtime Request Headers

**Scenario:** Attach custom headers to individual send requests for tracing or routing.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

local response = session:send_and_wait({
  message = "Explain coroutines in Lua.",
  request_headers = {
    ["X-Request-Id"] = "req-abc-123",
    ["X-Trace-Parent"] = "00-traceid-spanid-01",
    ["X-Custom-Routing"] = "priority-queue",
  },
})

print("Response: " .. response.message)

client:stop()
```

## Model Capabilities Override

**Scenario:** Override model capabilities for a session, such as vision or function calling.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are an assistant with extended capabilities.",
  model_capabilities = {
    vision = true,
    function_calling = true,
    json_output = true,
    max_tokens = 8192,
  },
})

local response = session:send_and_wait({
  message = "Analyze this architecture diagram.",
})

print("Response: " .. response.message)

client:stop()
```

## Config Discovery

**Scenario:** Enable automatic discovery of project-level configuration files.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({
  enable_config_discovery = true,
})
client:start()

-- The SDK automatically scans for:
--   .copilot/config.lua
--   .copilot/config.json
--   .github/copilot-config.yml
-- in the workspace and its parents.

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

local response = session:send_and_wait({
  message = "What configuration is active for this project?",
})

print("Response: " .. response.message)

client:stop()
```

## Sub-Agent Streaming Events

**Scenario:** Subscribe to streaming events from sub-agents during orchestration.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are an orchestrator that delegates to sub-agents.",
  include_sub_agent_streaming_events = true,
})

session:send({
  message = "Research Lua coroutines and write a tutorial.",
  streaming = true,
  on_event = function(event)
    if event.type == "assistant.message_delta" then
      io.write(event.delta)
    elseif event.type == "sub_agent.start" then
      print("\n[Sub-agent started: " .. event.agent_name .. "]")
    elseif event.type == "sub_agent.message_delta" then
      io.write(event.delta)
    elseif event.type == "sub_agent.end" then
      print("\n[Sub-agent finished: " .. event.agent_name .. "]")
    end
  end,
})

client:stop()
```

## Session Metadata

**Scenario:** Retrieve metadata about an active session.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a helpful assistant.",
})

session:send_and_wait({ message = "Hello!" })

local metadata = session:get_session_metadata()

print("Session ID: " .. metadata.session_id)
print("Created at: " .. metadata.created_at)
print("Turn count: " .. tostring(metadata.turn_count))
print("Model: " .. metadata.model)
print("Token usage: " .. json.encode(metadata.token_usage))

client:stop()
```

## MCP Server Configuration

**Scenario:** Configure MCP (Model Context Protocol) servers using stdio and HTTP transports.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

-- Stdio transport: launch a local MCP server as a child process
local session_stdio = client:create_session({
  system_prompt = "You are an assistant with MCP tools.",
  mcp_servers = {
    {
      name = "local-tools",
      transport = "stdio",
      command = "node",
      args = { "./mcp-server/index.js" },
      env = { MCP_LOG_LEVEL = "info" },
    },
  },
})

-- HTTP transport: connect to a remote MCP server
local session_http = client:create_session({
  system_prompt = "You are an assistant with remote tools.",
  mcp_servers = {
    {
      name = "remote-tools",
      transport = "http",
      url = "https://mcp.example.com/v1",
      headers = {
        ["Authorization"] = "Bearer " .. os.getenv("MCP_API_KEY"),
      },
    },
  },
})

local response = session_stdio:send_and_wait({
  message = "Use the local tools to analyze the project.",
})

print("Response: " .. response.message)

client:stop()
```

## Image Generation

**Scenario:** Configure the response format for image generation tasks.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

local session = client:create_session({
  system_prompt = "You are a creative assistant that generates images.",
  model_capabilities = {
    vision = true,
    image_generation = true,
  },
})

local response = session:send_and_wait({
  message = "Generate an image of a Lua programming mascot.",
  response_format = {
    type = "image",
    size = "1024x1024",
    quality = "high",
  },
})

if response.images and #response.images > 0 then
  for i, img in ipairs(response.images) do
    print(string.format("Image %d URL: %s", i, img.url))

    -- Save base64 image data if returned inline
    if img.base64_data then
      local f = io.open("generated_" .. i .. ".png", "wb")
      -- Decode base64 and write (using a base64 library)
      f:write(copilot.decode_base64(img.base64_data))
      f:close()
      print(string.format("Image %d saved to generated_%d.png", i, i))
    end
  end
else
  print("Response: " .. response.message)
end

client:stop()
```

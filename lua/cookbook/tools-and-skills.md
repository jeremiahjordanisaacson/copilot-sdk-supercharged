# Tools and Skills - Lua

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Lua.

## Defining a Simple Tool

**Scenario:** Expose a Lua function as a tool that the Copilot model can call during a conversation.

```lua
local copilot = require("copilot")

local client = copilot.CopilotClient:new({})
client:start()

-- Define a tool that returns the current date and time
client:define_tool("get_current_time", "Returns the current date and time", function(params)
  return os.date("%Y-%m-%d %H:%M:%S")
end)

local session = client:create_session({
  system_prompt = "You are a helpful assistant with access to tools.",
})

local response = session:send_and_wait({
  message = "What is the current date and time?",
})

print("Response: " .. response.message)

client:stop()
```

## Tool with Parameters

**Scenario:** Define a tool that accepts structured input parameters from the model.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

-- Define a calculator tool
client:define_tool("calculate", "Performs basic arithmetic operations", function(params)
  local a = tonumber(params.a)
  local b = tonumber(params.b)
  local op = params.operation

  if not a or not b then
    return json.encode({ error = "Both 'a' and 'b' must be numbers" })
  end

  local result
  if op == "add" then
    result = a + b
  elseif op == "subtract" then
    result = a - b
  elseif op == "multiply" then
    result = a * b
  elseif op == "divide" then
    if b == 0 then
      return json.encode({ error = "Division by zero" })
    end
    result = a / b
  else
    return json.encode({ error = "Unknown operation: " .. tostring(op) })
  end

  return json.encode({ result = result })
end)

local session = client:create_session({
  system_prompt = "You are a calculator assistant. Use the calculate tool for math.",
})

local response = session:send_and_wait({
  message = "What is 42 multiplied by 17?",
})

print("Response: " .. response.message)

client:stop()
```

## File System Tool

**Scenario:** Give the model the ability to read files from the local file system.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

client:define_tool("read_file", "Reads and returns the contents of a file", function(params)
  local filepath = params.path
  if not filepath then
    return json.encode({ error = "Missing 'path' parameter" })
  end

  local file = io.open(filepath, "r")
  if not file then
    return json.encode({ error = "File not found: " .. filepath })
  end

  local content = file:read("*a")
  file:close()

  -- Truncate very large files
  local max_length = 10000
  if #content > max_length then
    content = content:sub(1, max_length) .. "\n... (truncated)"
  end

  return json.encode({
    path = filepath,
    content = content,
    size = #content,
  })
end)

client:define_tool("list_files", "Lists files in a directory", function(params)
  local dir = params.directory or "."
  local files = {}

  local handle = io.popen('ls -1 "' .. dir .. '" 2>/dev/null')
  if handle then
    for line in handle:lines() do
      table.insert(files, line)
    end
    handle:close()
  end

  return json.encode({ directory = dir, files = files })
end)

local session = client:create_session({
  system_prompt = "You are a file explorer assistant. Use your tools to read and list files.",
})

local response = session:send_and_wait({
  message = "List the files in the current directory.",
})

print(response.message)

client:stop()
```

## Multiple Tools Working Together

**Scenario:** Register several related tools that the model can combine to accomplish complex tasks.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

-- In-memory key-value store
local store = {}

client:define_tool("store_set", "Stores a value under a key", function(params)
  store[params.key] = params.value
  return json.encode({ status = "ok", key = params.key })
end)

client:define_tool("store_get", "Retrieves a value by key", function(params)
  local value = store[params.key]
  if value == nil then
    return json.encode({ error = "Key not found: " .. params.key })
  end
  return json.encode({ key = params.key, value = value })
end)

client:define_tool("store_list", "Lists all keys in the store", function(_params)
  local keys = {}
  for k, _ in pairs(store) do
    table.insert(keys, k)
  end
  return json.encode({ keys = keys })
end)

client:define_tool("store_delete", "Deletes a key from the store", function(params)
  store[params.key] = nil
  return json.encode({ status = "deleted", key = params.key })
end)

local session = client:create_session({
  system_prompt = "You are a key-value store assistant. Use the store tools to manage data.",
})

local response = session:send_and_wait({
  message = "Store my name as Alice, my age as 30, then list all keys.",
})

print(response.message)

client:stop()
```

## Tool with Error Handling

**Scenario:** Handle errors inside tool handlers gracefully so the model gets useful error messages.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

client:define_tool("safe_divide", "Divides two numbers with error handling", function(params)
  local ok, result = pcall(function()
    local a = tonumber(params.numerator)
    local b = tonumber(params.denominator)

    if not a or not b then
      error("Both numerator and denominator must be valid numbers")
    end

    if b == 0 then
      error("Cannot divide by zero")
    end

    return a / b
  end)

  if ok then
    return json.encode({ result = result })
  else
    return json.encode({ error = tostring(result) })
  end
end)

local session = client:create_session({
  system_prompt = "You are a math assistant. Use safe_divide for division.",
})

local response = session:send_and_wait({
  message = "What is 100 divided by 0?",
})

print(response.message)

client:stop()
```

## Orchestrating Sub-Agents

**Scenario:** Use one session to coordinate work across multiple specialized sessions, each with their own tools.

```lua
local copilot = require("copilot")
local json = require("cjson")

local client = copilot.CopilotClient:new({})
client:start()

-- Create specialized sub-agent sessions
local code_agent = client:create_session({
  system_prompt = "You generate Lua code. Only output code, no explanations.",
})

local review_agent = client:create_session({
  system_prompt = "You review Lua code for bugs and style issues. Be concise.",
})

-- Define a tool for the orchestrator to delegate work
client:define_tool("generate_code", "Asks the code agent to generate Lua code", function(params)
  local response = code_agent:send_and_wait({ message = params.request })
  return response.message
end)

client:define_tool("review_code", "Asks the review agent to review Lua code", function(params)
  local response = review_agent:send_and_wait({ message = "Review this code:\n" .. params.code })
  return response.message
end)

-- Orchestrator session uses the sub-agent tools
local orchestrator = client:create_session({
  system_prompt = "You orchestrate tasks. Use generate_code to create code and review_code to review it.",
})

local response = orchestrator:send_and_wait({
  message = "Generate a Lua function to merge two sorted tables, then review it.",
})

print(response.message)

client:stop()
```

## Best Practices

1. **Return JSON from tool handlers** for structured data that the model can parse reliably.
2. **Always handle errors inside tool handlers** using pcall so the model receives an error message rather than causing a crash.
3. **Truncate large outputs** to stay within token limits and keep responses fast.
4. **Keep tool descriptions clear and specific** so the model knows exactly when and how to use each tool.
5. **Combine related tools** into a coherent set (CRUD operations, file tools) rather than making one monolithic tool.
6. **Use sub-agent orchestration** for complex multi-step workflows where different sessions specialize in different tasks.

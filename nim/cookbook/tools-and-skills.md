# Tools and Skills - Nim

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Nim.

## Defining a Simple Tool

**Scenario:** Expose a Nim proc as a tool that the Copilot model can call during a conversation.

```nim
import std/[asyncdispatch, json, times]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  await client.defineTool(
    "get_current_time",
    "Returns the current date and time",
    @[],
    proc(params: JsonNode): string =
      $(%*{"time": $now()}),
  )

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant with access to tools.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "What is the current date and time?",
  ))
  echo "Response: ", response.message

waitFor main()
```

## Tool with Parameters

**Scenario:** Define a tool that accepts structured input parameters from the model.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  await client.defineTool(
    "calculate",
    "Performs basic arithmetic operations",
    @[
      newToolParameter("a", "number", "First operand"),
      newToolParameter("b", "number", "Second operand"),
      newToolParameter("operation", "string", "add, subtract, multiply, divide"),
    ],
    proc(params: JsonNode): string =
      let a = params["a"].getFloat()
      let b = params["b"].getFloat()
      case params["operation"].getStr()
      of "add": $(%*{"result": a + b})
      of "subtract": $(%*{"result": a - b})
      of "multiply": $(%*{"result": a * b})
      of "divide":
        if b == 0: $(%*{"error": "Division by zero"})
        else: $(%*{"result": a / b})
      else: $(%*{"error": "Unknown operation"}),
  )

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a calculator assistant. Use the calculate tool for math.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "What is 42 multiplied by 17?",
  ))
  echo "Response: ", response.message

waitFor main()
```

## File System Tool

**Scenario:** Give the model the ability to read files from the local file system.

```nim
import std/[asyncdispatch, json, os]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  await client.defineTool(
    "read_file",
    "Reads and returns the contents of a file",
    @[newToolParameter("path", "string", "File path to read")],
    proc(params: JsonNode): string =
      let path = params["path"].getStr()
      try:
        let content = readFile(path)
        let truncated = if content.len > 10000: content[0 ..< 10000] & "\n... (truncated)"
                        else: content
        $(%*{"path": path, "content": truncated, "size": content.len})
      except IOError as e:
        $(%*{"error": e.msg}),
  )

  await client.defineTool(
    "list_files",
    "Lists files in a directory",
    @[newToolParameter("directory", "string", "Directory path", required = false)],
    proc(params: JsonNode): string =
      let dir = params{"directory"}.getStr(".")
      var files: seq[string]
      try:
        for kind, path in walkDir(dir):
          files.add(extractFilename(path))
      except OSError as e:
        return $(%*{"error": e.msg})
      $(%*{"directory": dir, "files": files}),
  )

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a file explorer assistant. Use your tools to read and list files.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "List the files in the current directory.",
  ))
  echo response.message

waitFor main()
```

## Multiple Tools Working Together

**Scenario:** Register several related tools that the model can combine to accomplish complex tasks.

```nim
import std/[asyncdispatch, json, tables]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  var store = initTable[string, string]()

  await client.registerTools(@[
    newTool("store_set", "Stores a value under a key",
      @[newToolParameter("key", "string", "Key"),
        newToolParameter("value", "string", "Value")],
      proc(params: JsonNode): string =
        store[params["key"].getStr()] = params["value"].getStr()
        $(%*{"status": "ok", "key": params["key"].getStr()})),

    newTool("store_get", "Retrieves a value by key",
      @[newToolParameter("key", "string", "Key")],
      proc(params: JsonNode): string =
        let key = params["key"].getStr()
        if store.hasKey(key): $(%*{"key": key, "value": store[key]})
        else: $(%*{"error": "Key not found: " & key})),

    newTool("store_list", "Lists all keys in the store", @[],
      proc(params: JsonNode): string =
        var keys: seq[string]
        for k in store.keys: keys.add(k)
        $(%*{"keys": keys})),
  ])

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a key-value store assistant. Use the store tools to manage data.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "Store my name as Alice, my age as 30, then list all keys.",
  ))
  echo response.message

waitFor main()
```

## Tool with Error Handling

**Scenario:** Handle errors inside tool handlers gracefully.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  await client.defineTool(
    "safe_divide",
    "Divides two numbers with error handling",
    @[
      newToolParameter("numerator", "number", "Numerator"),
      newToolParameter("denominator", "number", "Denominator"),
    ],
    proc(params: JsonNode): string =
      try:
        let a = params["numerator"].getFloat()
        let b = params["denominator"].getFloat()
        if b == 0:
          raise newException(ArithmeticDefect, "Cannot divide by zero")
        $(%*{"result": a / b})
      except CatchableError as e:
        $(%*{"error": e.msg}),
  )

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a math assistant. Use safe_divide for division.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "What is 100 divided by 0?",
  ))
  echo response.message

waitFor main()
```

## Orchestrating Sub-Agents

**Scenario:** Use one session to coordinate work across multiple specialized sessions, each with their own tools.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let codeAgent = await client.createSession(newSessionConfig(
    systemPrompt = "You generate Nim code. Only output code, no explanations.",
  ))
  let reviewAgent = await client.createSession(newSessionConfig(
    systemPrompt = "You review Nim code for bugs and style issues. Be concise.",
  ))

  await client.defineTool(
    "generate_code",
    "Asks the code agent to generate Nim code",
    @[newToolParameter("request", "string", "Code generation request")],
    proc(params: JsonNode): string =
      let resp = waitFor codeAgent.sendAndWait(newMessageOptions(
        params["request"].getStr()))
      resp.message,
  )

  await client.defineTool(
    "review_code",
    "Asks the review agent to review Nim code",
    @[newToolParameter("code", "string", "Code to review")],
    proc(params: JsonNode): string =
      let resp = waitFor reviewAgent.sendAndWait(newMessageOptions(
        "Review this code:\n" & params["code"].getStr()))
      resp.message,
  )

  let orchestrator = await client.createSession(newSessionConfig(
    systemPrompt = "You orchestrate tasks. Use generate_code to create code and review_code to review it.",
  ))

  let response = await orchestrator.sendAndWait(newMessageOptions(
    "Generate a Nim proc to merge two sorted sequences, then review it.",
  ))
  echo response.message

waitFor main()
```

## Best Practices

1. **Return JSON from tool handlers** for structured data that the model can parse reliably.
2. **Always handle errors inside tool handlers** with try/except so the model receives an error message rather than causing a crash.
3. **Truncate large outputs** to stay within token limits and keep responses fast.
4. **Keep tool descriptions clear and specific** so the model knows exactly when and how to use each tool.
5. **Combine related tools** into a coherent set (CRUD operations, file tools) rather than making one monolithic tool.
6. **Use sub-agent orchestration** for complex multi-step workflows where different sessions specialize in different tasks.

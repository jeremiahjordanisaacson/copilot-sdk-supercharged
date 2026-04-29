## Basic example demonstrating the GitHub Copilot SDK for Nim.
##
## Shows how to create a client, start a session, register a tool,
## send a message, and read the response.

import std/[json, asyncdispatch, os]

# Adjust import path for running from nim/examples/
import ../src/copilot_sdk

proc main() {.async.} =
  # Create and start the client
  let client = newCopilotClient(newClientConfig(
    cliPath = getEnv("COPILOT_CLI_PATH", ""),
  ))

  await client.start()
  echo "Client connected."

  # Register a simple tool
  await client.defineTool(
    "get_weather",
    "Returns the current weather for a city",
    @[
      newToolParameter("city", "string", "City name"),
    ],
    proc(params: JsonNode): string =
      let city = params{"city"}.getStr("unknown")
      $(%*{"city": city, "temperature": "22C", "condition": "sunny"}),
  )

  # Create a session with a system prompt
  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant with access to weather tools.",
  ))
  echo "Session created: ", session.sessionId

  # Subscribe to streaming events
  session.on("assistant.message_delta", proc(event: SessionEvent) =
    if event.data.hasKey("delta"):
      stdout.write(event.data["delta"].getStr())
  )

  # Send a message and wait for the complete response
  let response = await session.sendAndWait(newMessageOptions(
    "What is the weather like in Tokyo?",
  ))
  echo "\n\nFull response: ", response.message

  # Clean up
  await session.disconnect()
  client.stop()
  echo "Done."

waitFor main()

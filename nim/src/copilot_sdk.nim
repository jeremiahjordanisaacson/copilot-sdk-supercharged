## GitHub Copilot SDK for Nim
##
## This is the main entry point that re-exports all public modules.
## Import this module to access the full SDK:
##
## .. code-block:: nim
##   import copilot_sdk
##
##   let client = newCopilotClient()
##   waitFor client.start()
##   let session = waitFor client.createSession(
##     newSessionConfig(systemPrompt = "You are a helpful assistant."))
##   let result = waitFor session.sendAndWait(
##     newMessageOptions("Hello!"))
##   echo result.message
##   client.stop()

import copilot_sdk/types
import copilot_sdk/jsonrpc
import copilot_sdk/tools
import copilot_sdk/session
import copilot_sdk/client

export types
export jsonrpc
export tools
export session
export client

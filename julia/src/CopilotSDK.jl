# --------------------------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
# --------------------------------------------------------------------------------------------

"""
    CopilotSDK

Julia SDK for the GitHub Copilot CLI.

Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio or TCP,
using Content-Length header framing (LSP protocol style).

# Quick start

```julia
using CopilotSDK

client = CopilotClient(cli_path="/path/to/copilot-cli")
start!(client)

session = create_session(client;
    model="gpt-4",
    on_permission_request=approve_all,
)

on(session) do event
    if event.type == "assistant.message"
        println(get(event.data, "content", ""))
    end
end

send(session, "What is 2+2?")
sleep(5)

disconnect(session)
stop!(client)
```
"""
module CopilotSDK

using JSON3
using UUIDs
import Sockets

# Source files (order matters: types first, then transport, then higher-level)
include("types.jl")
include("jsonrpc.jl")
include("tools.jl")
include("session.jl")
include("client.jl")

# Public API
export CopilotClient, CopilotClientOptions, CopilotSession
export SessionConfig, MessageOptions, SessionEvent
export Tool, ToolResult, ToolInvocation, ToolResultType, define_tool
export PermissionRequest, ServerStatus, AuthStatus, ModelInfo, SessionMetadata
export ConnectionState, LogLevel
export start!, stop!, create_session, get_status, get_models, list_sessions
export on, send, send_and_wait, disconnect
export approve_all, deny_all, tool_to_wire

end # module CopilotSDK

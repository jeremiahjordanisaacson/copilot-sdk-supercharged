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
export SessionConfig, SessionFsConfig, MessageOptions, SessionEvent
export Tool, ToolResult, ToolInvocation, ToolResultType, define_tool
export PermissionRequest, ServerStatus, AuthStatus, ModelInfo, SessionMetadata
export ExitPlanModeRequest, ExitPlanModeResult, TraceContext
export ConnectionState, LogLevel
export start!, stop!, create_session, resume_session, get_status, get_models, list_sessions
export get_session_metadata, set_session_fs_provider
export on, send, send_and_wait, disconnect
export handle_exit_plan_mode, get_trace_context
export approve_all, deny_all, tool_to_wire

# Enum values & constants used by tests and consumers
export DISCONNECTED, CONNECTING, CONNECTED, CONNECTION_ERROR
export LOG_NONE, LOG_ERROR, LOG_WARNING, LOG_INFO, LOG_DEBUG, LOG_ALL
export LOG_LEVEL_STRINGS
export TOOL_SUCCESS, TOOL_FAILURE, TOOL_REJECTED, TOOL_DENIED, TOOL_TIMEOUT
export TOOL_RESULT_STRINGS

# Error types
export JsonRpcError, ProcessExitedError

end # module CopilotSDK

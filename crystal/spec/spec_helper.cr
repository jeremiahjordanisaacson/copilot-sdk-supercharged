# Copyright (c) Microsoft Corporation. All rights reserved.

require "spec"
require "../src/copilot_sdk"

# Shared test helpers for Crystal specs.

# Create a pair of connected IO pipes for testing JSON-RPC communication.
def create_pipe_pair : Tuple(IO, IO)
  reader, writer = IO.pipe
  {reader, writer}
end

# Build a JSON-RPC message with Content-Length framing.
def frame_message(body : String) : String
  "Content-Length: #{body.bytesize}\r\n\r\n#{body}"
end

# Build a JSON-RPC response message.
def json_rpc_response(id : String, result : String) : String
  body = %Q({"jsonrpc":"2.0","id":"#{id}","result":#{result}})
  frame_message(body)
end

# Build a JSON-RPC error response.
def json_rpc_error(id : String, code : Int32, message : String) : String
  body = %Q({"jsonrpc":"2.0","id":"#{id}","error":{"code":#{code},"message":"#{message}"}})
  frame_message(body)
end

# Build a JSON-RPC notification.
def json_rpc_notification(method : String, params : String) : String
  body = %Q({"jsonrpc":"2.0","method":"#{method}","params":#{params}})
  frame_message(body)
end

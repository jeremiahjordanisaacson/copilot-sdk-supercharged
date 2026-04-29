# Copyright (c) Microsoft Corporation. All rights reserved.

# GitHub Copilot SDK for Crystal
#
# A Crystal client library for programmatic control of GitHub Copilot CLI
# via JSON-RPC 2.0. Communicates with the Copilot CLI server using
# Content-Length header framing (LSP protocol style) over stdio or TCP.

require "./copilot_sdk/version"
require "./copilot_sdk/types"
require "./copilot_sdk/jsonrpc"
require "./copilot_sdk/tools"
require "./copilot_sdk/session"
require "./copilot_sdk/client"

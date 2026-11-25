# Copyright (c) Microsoft Corporation. All rights reserved.

module CopilotSDK
  VERSION = "3.1.1"

  # SDK protocol version this SDK speaks. Copilot CLI 1.0.113 and newer report 4.
  SDK_PROTOCOL_VERSION = 4

  # Minimum protocol version this SDK can communicate with. Only SDK protocol 4 is
  # accepted, so this equals the SDK protocol version.
  MIN_PROTOCOL_VERSION = 4
end

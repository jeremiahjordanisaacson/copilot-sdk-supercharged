## SDK protocol version for the GitHub Copilot SDK for Nim.
##
## The Copilot CLI reports its protocol version as a JSON number in the
## `connect` / `ping` response (`protocolVersion`). This SDK only talks to a
## server that reports exactly this version (Copilot CLI 1.0.113 or newer).

const
  SdkProtocolVersion* = 4

proc getSdkProtocolVersion*(): int =
  ## Returns the SDK protocol version this SDK speaks.
  SdkProtocolVersion

{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.SdkProtocolVersion;

interface

const
  // SDK protocol version spoken with the Copilot CLI (CLI 1.0.113 or newer).
  SDK_PROTOCOL_VERSION = '4';
  // The startup handshake accepts exactly this protocol version range.
  SDK_PROTOCOL_VERSION_MIN = 4;
  SDK_PROTOCOL_VERSION_MAX = 4;

implementation

end.

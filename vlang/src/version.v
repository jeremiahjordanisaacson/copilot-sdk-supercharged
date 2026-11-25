// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - Protocol version constants

module copilot

// sdk_version is the version of this V SDK.
pub const sdk_version = '3.1.1'

// protocol_version is the SDK protocol version this SDK speaks. The server must
// report exactly this version (Copilot CLI 1.0.113 or newer).
pub const protocol_version = 4

// min_protocol_version is the minimum protocol version the server must support.
pub const min_protocol_version = 4

// sdk_name identifies this SDK in handshake metadata.
pub const sdk_name = 'copilot-sdk-vlang'

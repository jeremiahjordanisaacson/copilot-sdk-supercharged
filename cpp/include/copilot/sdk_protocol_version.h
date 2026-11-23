/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

namespace copilot {

/// The SDK protocol version that this SDK expects the CLI server to support.
/// This must match the server's reported protocolVersion from the ping response.
/// SDK protocol 4 (grouped session payloads) requires Copilot CLI 1.0.113 or newer.
constexpr int SDK_PROTOCOL_VERSION = 4;

} // namespace copilot

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

namespace copilot {

/// The SDK protocol version that this SDK expects the CLI server to support.
/// This must match the server's reported protocolVersion from the ping response.
constexpr int SDK_PROTOCOL_VERSION = 2;

} // namespace copilot

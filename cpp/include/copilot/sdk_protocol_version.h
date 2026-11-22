/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

<<<<<<<< HEAD:cpp/include/copilot/sdk_protocol_version.h
#pragma once
========
package com.microsoft.copilot.ffi;
>>>>>>>> upstream/main:java/sdk/src/main/java25/com/microsoft/copilot/ffi/ReaderThreadFactory.java

namespace copilot {

/// The SDK protocol version that this SDK expects the CLI server to support.
/// This must match the server's reported protocolVersion from the ping response.
constexpr int SDK_PROTOCOL_VERSION = 3;

} // namespace copilot

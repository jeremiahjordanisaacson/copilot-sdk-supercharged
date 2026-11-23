// Copyright (c) Microsoft Corporation. All rights reserved.

// Code generated from sdk-protocol-version.json. DO NOT EDIT.

/// The SDK protocol version.
/// This must match the version reported by the Copilot runtime server
/// (Copilot CLI 1.0.113 and newer report protocol 4).
const int sdkProtocolVersion = 4;

/// Gets the SDK protocol version.
int getSdkProtocolVersion() => sdkProtocolVersion;

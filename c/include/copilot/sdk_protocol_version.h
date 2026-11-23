/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file sdk_protocol_version.h
 * @brief SDK protocol version for compatibility verification with the Copilot CLI server.
 *
 * This version must match the server's protocol version. A mismatch indicates
 * that the SDK and CLI server are incompatible and need to be updated.
 * SDK protocol 4 (grouped session payloads) requires Copilot CLI 1.0.113 or newer.
 */

#ifndef COPILOT_SDK_PROTOCOL_VERSION_H
#define COPILOT_SDK_PROTOCOL_VERSION_H

#define COPILOT_SDK_PROTOCOL_VERSION 4

#endif /* COPILOT_SDK_PROTOCOL_VERSION_H */

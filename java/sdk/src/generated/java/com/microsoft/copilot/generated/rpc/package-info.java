/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

/**
 * Auto-generated RPC parameter and result types for the Microsoft Copilot SDK.
 *
 * <p>
 * This package contains Java records and classes generated from the Copilot
 * CLI's {@code api.schema.json}. These types represent the request parameters
 * and response payloads for all JSON-RPC methods exposed by the CLI.
 *
 * <h2>Key Classes</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.generated.rpc.RpcCaller} - Functional interface
 * for invoking JSON-RPC methods with typed responses.</li>
 * <li>{@link com.microsoft.copilot.generated.rpc.ServerRpc} - Typed client for
 * server-level RPC methods (session management, model listing, etc.).</li>
 * <li>{@link com.microsoft.copilot.generated.rpc.SessionRpc} - Typed client for
 * session-scoped RPC methods (send messages, manage tools, etc.). Automatically
 * injects the {@code sessionId} into every call.</li>
 * </ul>
 *
 * <h2>Related Packages</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot} - Core SDK classes</li>
 * <li>{@link com.microsoft.copilot.generated} - Auto-generated session event
 * types</li>
 * </ul>
 *
 * @see com.microsoft.copilot.CopilotClient
 * @see com.microsoft.copilot.generated.rpc.ServerRpc
 * @see com.microsoft.copilot.generated.rpc.SessionRpc
 */
package com.microsoft.copilot.generated.rpc;

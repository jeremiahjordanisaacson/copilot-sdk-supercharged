/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

/**
 * Host-provided OAuth token data for a pending MCP OAuth request.
 *
 * @since 1.0.0
 */
public record McpAuthToken(String accessToken, String tokenType, Long expiresIn) {
}

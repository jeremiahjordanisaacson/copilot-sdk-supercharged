/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import com.microsoft.copilot.generated.McpOauthRequiredStaticClientConfig;
import com.microsoft.copilot.generated.McpOauthRequestReason;
import com.microsoft.copilot.generated.McpOauthWWWAuthenticateParams;

/**
 * MCP OAuth request that the SDK host can satisfy with a host-acquired token.
 *
 * @since 1.0.0
 */
public record McpAuthRequest(String requestId, String serverName, String serverUrl, McpOauthRequestReason reason,
        McpOauthWWWAuthenticateParams wwwAuthenticateParams, String resourceMetadata,
        McpOauthRequiredStaticClientConfig staticClientConfig) {
}

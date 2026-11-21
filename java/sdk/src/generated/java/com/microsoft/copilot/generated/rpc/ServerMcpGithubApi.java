/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code mcp.github} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerMcpGithubApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerMcpGithubApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Resolves a secret-free built-in GitHub MCP descriptor through this connection's original credential owner. Preserves invocation tool overrides, shell filters, Azure DevOps narrowing, and cached GitHub IQ selection within the existing two-second inspection budget. Does not start an MCP connection.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<McpGitHubResolveInvocationResult> resolveForInvocation(McpGithubResolveForInvocationParams params) {
        return caller.invoke("mcp.github.resolveForInvocation", params, McpGitHubResolveInvocationResult.class);
    }

}

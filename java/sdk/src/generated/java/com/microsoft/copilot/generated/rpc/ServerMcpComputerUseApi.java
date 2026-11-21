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
 * API methods for the {@code mcp.computerUse} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerMcpComputerUseApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerMcpComputerUseApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Applies the runtime host's existing Computer Use platform overlay to a complete MCP server map and effective disabled list. Preserves configuration and provenance outside that overlay and starts no server.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<McpComputerUseApplyConfigurationResult> applyConfiguration(McpComputerUseApplyConfigurationParams params) {
        return caller.invoke("mcp.computerUse.applyConfiguration", params, McpComputerUseApplyConfigurationResult.class);
    }

}

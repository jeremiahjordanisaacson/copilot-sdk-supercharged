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
 * API methods for the {@code mcp.secrets} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerMcpSecretsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerMcpSecretsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Persists a legacy MCP configuration credential in the runtime-owned store. Returns the selected backend; storage and installation-ownership errors propagate. Installation-owned credentials require the installation APIs.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<McpSecretsSaveResult> save(McpSecretsSaveParams params) {
        return caller.invoke("mcp.secrets.save", params, McpSecretsSaveResult.class);
    }

    /**
     * Deletes a legacy MCP configuration credential from the runtime-owned store. Installation-owned credentials require the installation APIs.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> delete(McpSecretsDeleteParams params) {
        return caller.invoke("mcp.secrets.delete", params, Void.class);
    }

    /**
     * Deletes legacy credentials indexed for an MCP server. Slash-named servers require MCP configuration removal to preserve shared credential ownership.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> deleteServer(McpSecretsDeleteServerParams params) {
        return caller.invoke("mcp.secrets.deleteServer", params, Void.class);
    }

}

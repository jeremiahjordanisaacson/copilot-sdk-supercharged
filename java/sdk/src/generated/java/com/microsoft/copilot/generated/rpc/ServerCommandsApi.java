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
 * API methods for the {@code commands} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerCommandsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerCommandsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Lists well-known built-in slash commands without an active session. By default, commands that need session state, authentication, or a synced session are omitted. Editors and terminal hosts can request the complete catalog and its argument grammar with includeUnavailable. Presentation flags never grant execution permissions.
     * <p>
     * Invokes the method with no params, applying the runtime defaults.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<CommandsListResult> list() {
        return list(null);
    }

    /**
     * Lists well-known built-in slash commands without an active session. By default, commands that need session state, authentication, or a synced session are omitted. Editors and terminal hosts can request the complete catalog and its argument grammar with includeUnavailable. Presentation flags never grant execution permissions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<CommandsListResult> list(CommandsListParams params) {
        return caller.invoke("commands.list", params == null ? java.util.Map.of() : params, CommandsListResult.class);
    }

}

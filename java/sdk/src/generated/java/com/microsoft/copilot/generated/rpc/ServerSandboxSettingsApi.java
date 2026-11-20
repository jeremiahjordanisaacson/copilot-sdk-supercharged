/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.databind.JsonNode;
import com.microsoft.copilot.CopilotExperimental;
import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code sandbox.settings} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerSandboxSettingsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerSandboxSettingsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Resolves a stored sandbox settings object into the effective one, filling every value the user left unset with its default. Read this to render or reason about the sandbox configuration a session would actually run under, rather than the sparse document the user wrote.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<JsonNode> applyDefaults(SandboxSettingsApplyDefaultsParams params) {
        return caller.invoke("sandbox.settings.applyDefaults", params, JsonNode.class);
    }

    /**
     * Resolves the settings file for an explicit host context. Editors can capture their file baseline before an asynchronous settings read.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<String> getPath(SandboxSettingsGetPathParams params) {
        return caller.invoke("sandbox.settings.getPath", params, String.class);
    }

    /**
     * Reads sandbox settings from the supplied host context and applies runtime defaults. Editors and other local hosts can inspect both the resolved settings and the sparse stored override.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SandboxSettingsLoadResult> load(SandboxSettingsLoadParams params) {
        return caller.invoke("sandbox.settings.load", params, SandboxSettingsLoadResult.class);
    }

    /**
     * Merges and persists a sandbox settings patch under the runtime settings transaction. Filesystem, network, and seatbelt patches update userPolicy without replacing unrelated settings.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> update(SandboxSettingsUpdateParams params) {
        return caller.invoke("sandbox.settings.update", params, Void.class);
    }

    /**
     * Saves an accepted sandbox path grant for future sessions. Grants remain literal, duplicate entries are not added, and reported read-only entries are removed only by their exact stored spelling.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> persistPathGrant(SandboxSettingsPersistPathGrantParams params) {
        return caller.invoke("sandbox.settings.persistPathGrant", params, Void.class);
    }

}

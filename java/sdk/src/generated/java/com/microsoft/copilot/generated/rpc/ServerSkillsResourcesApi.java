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
 * API methods for the {@code skills.resources} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerSkillsResourcesApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerSkillsResourcesApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Loads standalone skill records and diagnostics with explicit host settings, workspace paths, and plugin sources.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SkillResourceDiscoverResult> discover(SkillsResourcesDiscoverParams params) {
        return caller.invoke("skills.resources.discover", params, SkillResourceDiscoverResult.class);
    }

    /**
     * Registers a skill directory or installs a local or HTTPS definition through the original runtime owner.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SkillResourceInstallResult> install(SkillsResourcesInstallParams params) {
        return caller.invoke("skills.resources.install", params, SkillResourceInstallResult.class);
    }

    /**
     * Removes a personal or project skill or unregisters a directory with the existing scoped removal rules.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<SkillResourceRemoveResult> remove(SkillsResourcesRemoveParams params) {
        return caller.invoke("skills.resources.remove", params, SkillResourceRemoveResult.class);
    }

    /**
     * Atomically changes one user's skill disable entry under explicit host settings paths.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> setEnabled(SkillsResourcesSetEnabledParams params) {
        return caller.invoke("skills.resources.setEnabled", params, Void.class);
    }

}

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
 * API methods for the {@code secrets} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerSecretsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerSecretsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Registers secret values for best-effort redaction in session logs and exports while this client remains connected. Repeated adds are idempotent. Discouraged for runtime code: retain an owned secret registration instead. Remove rotating or unused values with secrets.removeFilterValues; historical output and late echoes may no longer be redacted after removal or disconnect.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SecretsAddFilterValuesResult> addFilterValues(SecretsAddFilterValuesParams params) {
        return caller.invoke("secrets.addFilterValues", params, SecretsAddFilterValuesResult.class);
    }

    /**
     * Releases values previously added by this client. Repeated or unmatched removals are no-ops; other live registrations remain effective. Only remove values once they are reasonably unlikely to be logged again: historical output and late echoes may no longer be redacted.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SecretsRemoveFilterValuesResult> removeFilterValues(SecretsRemoveFilterValuesParams params) {
        return caller.invoke("secrets.removeFilterValues", params, SecretsRemoveFilterValuesResult.class);
    }

    /**
     * Redacts text or JSON with the runtime's process filter, including registered values and live environment sources. Requires COPILOT_ENABLE_SECRET_FILTERING=true in the runtime's environment, as for secrets.addFilterValues.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<SecretsFilterResult> filter(SecretsFilterParams params) {
        return caller.invoke("secrets.filter", params, SecretsFilterResult.class);
    }

}

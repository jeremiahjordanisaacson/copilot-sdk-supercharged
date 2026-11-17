/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import java.util.concurrent.CompletableFuture;

import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.EntraTokenAcquireRequest;
import com.microsoft.copilot.generated.rpc.EntraTokenAcquireResult;

/** Host-owned Entra identity or resource-token acquisition. */
@CopilotExperimental
@FunctionalInterface
public interface EntraTokenProvider {
    /**
     * Selects/revalidates an identity or acquires its resource token. Renewal must
     * be silent and account-pinned; unavailability and cancellation fail closed.
     *
     * @param request
     *            the requested acquisition and exact resource/account targeting
     * @param context
     *            cancelled when the request or original connection ends
     * @return the matching identity, token, unavailable, or cancelled result
     */
    CompletableFuture<EntraTokenAcquireResult> getToken(EntraTokenAcquireRequest request,
            EntraTokenProviderContext context);
}

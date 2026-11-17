/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CreateSessionOptions;
import java.util.concurrent.CompletionStage;

/**
 * An application-owned AHP session creation request. Preserve the supplied
 * settings when adding application tools, prompts, permissions, and hooks.
 *
 * @param config
 *            host-selected session settings
 * @param cancellation
 *            completes normally when the participation ends; a late factory
 *            result is still delivered to the release callback
 * @param authentication
 *            trusted admission identity supplied by the host, never session
 *            configuration
 */
@CopilotExperimental
public record AhpSessionCreateRequest(CreateSessionOptions config, CompletionStage<Void> cancellation,
        AhpSessionAuthentication authentication) {
}

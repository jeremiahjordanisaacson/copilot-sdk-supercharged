/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.ResumeSessionOptions;
import java.util.concurrent.CompletionStage;

/**
 * An application-owned durable AHP session resume request. Reattach application
 * callbacks using the supplied settings, or return the exact retained session.
 *
 * @param sessionId
 *            durable session identity to resume
 * @param config
 *            host-selected resume settings
 * @param cancellation
 *            completes normally when the participation ends
 * @param authentication
 *            trusted current host admission identity, never session
 *            configuration
 */
@CopilotExperimental
public record AhpSessionResumeRequest(String sessionId, ResumeSessionOptions config, CompletionStage<Void> cancellation,
        AhpSessionAuthentication authentication) {
}

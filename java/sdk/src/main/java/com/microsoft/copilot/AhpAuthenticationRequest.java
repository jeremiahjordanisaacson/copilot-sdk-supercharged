/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot;

import java.util.concurrent.CompletionStage;

/**
 * Application admission validation. The token is never an inference credential.
 *
 * @param token
 *            decrypted admission token
 * @param cancellation
 *            completes when validation ends, including owner disconnect or
 *            timeout
 */
@CopilotExperimental
public record AhpAuthenticationRequest(String token, CompletionStage<Void> cancellation) {
}

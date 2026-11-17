/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

/**
 * Explicit host-wide admission policy, independent of inference credentials.
 *
 * @param mode
 *            admission mode: github, none, or custom
 * @param requireIdentity
 *            stable owner identity for custom admission
 * @param authenticate
 *            asynchronous custom validator; returning null rejects admission
 */
@CopilotExperimental
public record AhpHostAuthentication(String mode, String requireIdentity,
        Function<AhpAuthenticationRequest, CompletableFuture<AhpAuthenticationResult>> authenticate) {
    /**
     * Creates a GitHub admission policy.
     *
     * @return the policy
     */
    public static AhpHostAuthentication github() {
        return new AhpHostAuthentication("github", null, null);
    }

    /**
     * Explicitly selects anonymous admission without disabling the WebSocket
     * secret.
     *
     * @return the policy
     */
    public static AhpHostAuthentication none() {
        return new AhpHostAuthentication("none", null, null);
    }

    /**
     * Creates application-owned admission.
     *
     * @param requireIdentity
     *            stable accepted identity
     * @param authenticate
     *            asynchronous validator receiving the decrypted admission token
     * @return the policy
     */
    public static AhpHostAuthentication custom(String requireIdentity,
            Function<AhpAuthenticationRequest, CompletableFuture<AhpAuthenticationResult>> authenticate) {
        return new AhpHostAuthentication("custom", requireIdentity, Objects.requireNonNull(authenticate));
    }
}

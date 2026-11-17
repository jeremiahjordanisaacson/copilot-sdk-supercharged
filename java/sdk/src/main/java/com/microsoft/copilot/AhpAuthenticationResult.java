/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot;

/**
 * Accepted application identity with a required future expiration.
 *
 * @param identity
 *            must match the host's stable requireIdentity exactly
 * @param expiresAt
 *            future RFC3339 timestamp with an explicit offset
 */
@CopilotExperimental
public record AhpAuthenticationResult(String identity, String expiresAt) {
}

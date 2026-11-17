/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot;

/**
 * Trusted host admission context, separate from session configuration and
 * inference.
 *
 * @param mode
 *            github, custom, or none
 * @param identity
 *            verified owner identity; null for none
 */
@CopilotExperimental
public record AhpSessionAuthentication(String mode, String identity) {
}

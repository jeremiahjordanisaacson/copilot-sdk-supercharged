/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Begin an interactive login flow for a provider kind, optionally reusing a cached base identity or explicitly requesting provider account selection.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionAccountsLoginBeginParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** The provider kind to sign in with. */
    @JsonProperty("kind") LoginProviderKind kind,
    /** How to authorize: `web` (default) or `device-code`. Device code is offered for `githubDotCom` and `proxima`; `proxima` still asks for its host through `input-required` first. */
    @JsonProperty("method") AuthLoginMethod method,
    /** Neutral identity-selection intent. Omitted is equivalent to default. */
    @JsonProperty("mode") AuthLoginMode mode,
    /** Opaque cached base identity id returned by accounts.enumerate loginAccounts. Valid only with default mode; consumers must not parse it. */
    @JsonProperty("accountId") String accountId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionAccountsLoginBeginParams {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String sessionId;
        private LoginProviderKind kind;
        private AuthLoginMethod method;
        private AuthLoginMode mode;
        private String accountId;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder kind(LoginProviderKind value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code method} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder method(AuthLoginMethod value) {
            this.method = value;
            return this;
        }

        /**
         * Sets the {@code mode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder mode(AuthLoginMode value) {
            this.mode = value;
            return this;
        }

        /**
         * Sets the {@code accountId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder accountId(String value) {
            this.accountId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionAccountsLoginBeginParams build() {
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            return new SessionAccountsLoginBeginParams(sessionId, kind, method, mode, accountId);
        }
    }
}

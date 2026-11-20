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
 * The selected account and its credential-free identity metadata.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionAccountsGetActiveResult(
    /** The selected account, or null when none is active. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("account") Account account,
    /** Credential-free identity metadata for the active account, including resolved Copilot user information when available. */
    @JsonProperty("authInfo") AuthIdentity authInfo,
    /** Opaque credential selection id for the active authentication. This remains available when the canonical account is not currently switchable. */
    @JsonProperty("authSelectionId") String authSelectionId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionAccountsGetActiveResult {}

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
        private Account account;
        private boolean $presence0;
        private AuthIdentity authInfo;
        private String authSelectionId;

        /**
         * Sets the {@code account} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder account(Account value) {
            this.account = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code authInfo} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authInfo(AuthIdentity value) {
            this.authInfo = value;
            return this;
        }

        /**
         * Sets the {@code authSelectionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authSelectionId(String value) {
            this.authSelectionId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionAccountsGetActiveResult build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: account");
            return new SessionAccountsGetActiveResult(account, authInfo, authSelectionId);
        }
    }
}

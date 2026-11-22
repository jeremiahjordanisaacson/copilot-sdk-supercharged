/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * Relay endpoints from the original account owner and its captured credential environment.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AccountRelayBootstrapConfig(
    /** GitHub host of the active account. */
    @JsonProperty("relayHost") String relayHost,
    /** Mission Control endpoint, including a configured endpoint override. */
    @JsonProperty("missionControlUrl") String missionControlUrl,
    /** Whether an interactive flow can upgrade the active credential. */
    @JsonProperty("tokenUpgradeable") Boolean tokenUpgradeable
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AccountRelayBootstrapConfig {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    public static final class Builder {
        private Builder() {}
        private String relayHost;
        private String missionControlUrl;
        private Boolean tokenUpgradeable;

        /**
         * Sets the {@code relayHost} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder relayHost(String value) {
            this.relayHost = value;
            return this;
        }

        /**
         * Sets the {@code missionControlUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder missionControlUrl(String value) {
            this.missionControlUrl = value;
            return this;
        }

        /**
         * Sets the {@code tokenUpgradeable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder tokenUpgradeable(Boolean value) {
            this.tokenUpgradeable = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountRelayBootstrapConfig build() {
            if (relayHost == null) throw new IllegalStateException("Missing required property: relayHost");
            if (missionControlUrl == null) throw new IllegalStateException("Missing required property: missionControlUrl");
            if (tokenUpgradeable == null) throw new IllegalStateException("Missing required property: tokenUpgradeable");
            return new AccountRelayBootstrapConfig(relayHost, missionControlUrl, tokenUpgradeable);
        }
    }
}

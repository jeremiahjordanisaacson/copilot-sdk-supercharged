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
 * Authoritative policy labels and opt-out capability for a host.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SandboxPolicyPresentationResult(
    /** Whether the effective policy permits a session opt-out. */
    @JsonProperty("allowsOptOut") Boolean allowsOptOut,
    /** Notice used when the user declines an advisory managed floor. */
    @JsonProperty("advisoryFloorDeclinedNotice") String advisoryFloorDeclinedNotice,
    /** Notice when managed policy overrides an explicit flag. */
    @JsonProperty("flagOverriddenByPolicyNotice") String flagOverriddenByPolicyNotice,
    /** Notice when managed policy enables the sandbox. */
    @JsonProperty("managedFloorNotice") String managedFloorNotice
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxPolicyPresentationResult {}

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
        private Boolean allowsOptOut;
        private String advisoryFloorDeclinedNotice;
        private String flagOverriddenByPolicyNotice;
        private String managedFloorNotice;

        /**
         * Sets the {@code allowsOptOut} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowsOptOut(Boolean value) {
            this.allowsOptOut = value;
            return this;
        }

        /**
         * Sets the {@code advisoryFloorDeclinedNotice} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder advisoryFloorDeclinedNotice(String value) {
            this.advisoryFloorDeclinedNotice = value;
            return this;
        }

        /**
         * Sets the {@code flagOverriddenByPolicyNotice} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flagOverriddenByPolicyNotice(String value) {
            this.flagOverriddenByPolicyNotice = value;
            return this;
        }

        /**
         * Sets the {@code managedFloorNotice} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder managedFloorNotice(String value) {
            this.managedFloorNotice = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxPolicyPresentationResult build() {
            if (allowsOptOut == null) throw new IllegalStateException("Missing required property: allowsOptOut");
            if (advisoryFloorDeclinedNotice == null) throw new IllegalStateException("Missing required property: advisoryFloorDeclinedNotice");
            return new SandboxPolicyPresentationResult(allowsOptOut, advisoryFloorDeclinedNotice, flagOverriddenByPolicyNotice, managedFloorNotice);
        }
    }
}

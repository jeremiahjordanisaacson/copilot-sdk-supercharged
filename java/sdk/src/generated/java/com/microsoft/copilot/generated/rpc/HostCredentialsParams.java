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
 * Private host protocol for one runtime-owned credential transaction. Only the admitted in-process CLI host can use this protocol.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record HostCredentialsParams(
    /** Credential lifecycle transition or host-owned credential observation. */
    @JsonProperty("action") HostCredentialLifecycleAction action,
    /** Opaque runtime transaction capability returned by begin. */
    @JsonProperty("transactionId") String transactionId,
    /** Original callback connection. The runtime verifies that it belongs to this host's engine. */
    @JsonProperty("connectionId") Long connectionId,
    /** Existing credential registration, never a bearer token. */
    @JsonProperty("registrationId") String registrationId,
    /** Session resolved by the original host lifecycle. */
    @JsonProperty("sessionId") String sessionId,
    /** Registration retained by a resident ambient-auth rollback. */
    @JsonProperty("previousRegistrationId") String previousRegistrationId,
    /** Whether begin resumes an existing session. */
    @JsonProperty("resume") Boolean resume,
    /** Whether the original connection owns a renewable SDK callback rather than a static token context. */
    @JsonProperty("callback") Boolean callback
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    HostCredentialsParams {}

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
        private HostCredentialLifecycleAction action;
        private String transactionId;
        private Long connectionId;
        private String registrationId;
        private String sessionId;
        private String previousRegistrationId;
        private Boolean resume;
        private Boolean callback;

        /**
         * Sets the {@code action} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder action(HostCredentialLifecycleAction value) {
            this.action = value;
            return this;
        }

        /**
         * Sets the {@code transactionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder transactionId(String value) {
            this.transactionId = value;
            return this;
        }

        /**
         * Sets the {@code connectionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder connectionId(Long value) {
            this.connectionId = value;
            return this;
        }

        /**
         * Sets the {@code registrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder registrationId(String value) {
            this.registrationId = value;
            return this;
        }

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code previousRegistrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder previousRegistrationId(String value) {
            this.previousRegistrationId = value;
            return this;
        }

        /**
         * Sets the {@code resume} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder resume(Boolean value) {
            this.resume = value;
            return this;
        }

        /**
         * Sets the {@code callback} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder callback(Boolean value) {
            this.callback = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HostCredentialsParams build() {
            if (action == null) throw new IllegalStateException("Missing required property: action");
            return new HostCredentialsParams(action, transactionId, connectionId, registrationId, sessionId, previousRegistrationId, resume, callback);
        }
    }
}

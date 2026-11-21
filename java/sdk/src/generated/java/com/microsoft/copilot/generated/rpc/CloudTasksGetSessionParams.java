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
 * Identifies a Mission Control session and the identity that reads its connection target.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record CloudTasksGetSessionParams(
    /** Identity whose credential and Copilot endpoint the runtime resolves for this request. */
    @JsonProperty("authInfo") AuthIdentity authInfo,
    /** Registered credential owner, when the host uses one. */
    @JsonProperty("authContextId") String authContextId,
    /** Live session owned by this SDK server whose retained credential authenticates the lookup.
The identity must match that session. Cannot be combined with authContextId. */
    @JsonProperty("credentialSessionId") String credentialSessionId,
    /** Integration identifier sent to Mission Control. */
    @JsonProperty("integrationId") String integrationId,
    /** Remote Mission Control session identifier. */
    @JsonProperty("sessionId") String sessionId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    CloudTasksGetSessionParams {}

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
        private AuthIdentity authInfo;
        private String authContextId;
        private String credentialSessionId;
        private String integrationId;
        private String sessionId;

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
         * Sets the {@code authContextId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authContextId(String value) {
            this.authContextId = value;
            return this;
        }

        /**
         * Sets the {@code credentialSessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder credentialSessionId(String value) {
            this.credentialSessionId = value;
            return this;
        }

        /**
         * Sets the {@code integrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder integrationId(String value) {
            this.integrationId = value;
            return this;
        }

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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public CloudTasksGetSessionParams build() {
            if (authInfo == null) throw new IllegalStateException("Missing required property: authInfo");
            if (integrationId == null) throw new IllegalStateException("Missing required property: integrationId");
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            return new CloudTasksGetSessionParams(authInfo, authContextId, credentialSessionId, integrationId, sessionId);
        }
    }
}

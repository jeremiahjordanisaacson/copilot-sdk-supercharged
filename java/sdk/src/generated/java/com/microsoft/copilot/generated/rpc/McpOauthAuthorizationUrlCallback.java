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
 * Authorization URL presented by the original SDK requester.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpOauthAuthorizationUrlCallback(
    /** Session that owns the authentication attempt. */
    @JsonProperty("sessionId") String sessionId,
    /** Identity reserved by the original requester. */
    @JsonProperty("flowId") String flowId,
    /** Authorization URL that the requester must present. */
    @JsonProperty("authorizationUrl") String authorizationUrl
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpOauthAuthorizationUrlCallback {}

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
        private String sessionId;
        private String flowId;
        private String authorizationUrl;

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
         * Sets the {@code flowId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flowId(String value) {
            this.flowId = value;
            return this;
        }

        /**
         * Sets the {@code authorizationUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authorizationUrl(String value) {
            this.authorizationUrl = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpOauthAuthorizationUrlCallback build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (flowId == null) throw new IllegalStateException("Missing required property: flowId");
            if (authorizationUrl == null) throw new IllegalStateException("Missing required property: authorizationUrl");
            return new McpOauthAuthorizationUrlCallback(sessionId, flowId, authorizationUrl);
        }
    }
}

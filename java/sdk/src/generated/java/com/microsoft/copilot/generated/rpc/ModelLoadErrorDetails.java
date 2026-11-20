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
 * Model-catalog failure facts without host-specific presentation text.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ModelLoadErrorDetails(
    /** HTTP status reported by the provider, if available. */
    @JsonProperty("status") Long status,
    /** Request identifier extracted from the provider error. */
    @JsonProperty("requestId") String requestId,
    /** Service request identifier extracted from the provider error. */
    @JsonProperty("serviceRequestId") String serviceRequestId,
    /** Classified authorization failure for an HTTP 403 response, when recognized. */
    @JsonProperty("authorization") ModelLoadAuthorizationError authorization,
    /** Copilot settings URL for the session's account when a policy refusal needs remediation. */
    @JsonProperty("settingsUrl") String settingsUrl
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ModelLoadErrorDetails {}

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
        private Long status;
        private String requestId;
        private String serviceRequestId;
        private ModelLoadAuthorizationError authorization;
        private String settingsUrl;

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(Long value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code requestId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder requestId(String value) {
            this.requestId = value;
            return this;
        }

        /**
         * Sets the {@code serviceRequestId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder serviceRequestId(String value) {
            this.serviceRequestId = value;
            return this;
        }

        /**
         * Sets the {@code authorization} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authorization(ModelLoadAuthorizationError value) {
            this.authorization = value;
            return this;
        }

        /**
         * Sets the {@code settingsUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder settingsUrl(String value) {
            this.settingsUrl = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ModelLoadErrorDetails build() {
            return new ModelLoadErrorDetails(status, requestId, serviceRequestId, authorization, settingsUrl);
        }
    }
}

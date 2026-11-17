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
 * GitHub integration and session credentials.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionUpdateGitHubOptions(
    /** Options scoped to the built-in CAPI (Copilot API) provider. */
    @JsonProperty("capi") CapiSessionOptions capi,
    /** Override URL for the Copilot API endpoint. */
    @JsonProperty("copilotUrl") String copilotUrl
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateGitHubOptions {}

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
        private CapiSessionOptions capi;
        private String copilotUrl;

        /**
         * Sets the {@code capi} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder capi(CapiSessionOptions value) {
            this.capi = value;
            return this;
        }

        /**
         * Sets the {@code copilotUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder copilotUrl(String value) {
            this.copilotUrl = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateGitHubOptions build() {
            return new SessionUpdateGitHubOptions(capi, copilotUrl);
        }
    }
}

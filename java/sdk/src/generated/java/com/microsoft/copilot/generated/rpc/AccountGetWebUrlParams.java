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
 * A path to resolve against the selected GitHub host.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AccountGetWebUrlParams(
    /** Host-relative path, without a leading slash, for example `settings/copilot/features`. */
    @JsonProperty("path") String path,
    /** Identity whose host the path resolves against. Omit both identity and authOwnerId to use the public github.com host. */
    @JsonProperty("identity") AuthIdentity identity,
    /** Opaque selector for a live host-owned credential binding. Cannot be combined with identity. */
    @JsonProperty("authOwnerId") String authOwnerId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AccountGetWebUrlParams {}

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
        private String path;
        private AuthIdentity identity;
        private String authOwnerId;

        /**
         * Sets the {@code path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder path(String value) {
            this.path = value;
            return this;
        }

        /**
         * Sets the {@code identity} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder identity(AuthIdentity value) {
            this.identity = value;
            return this;
        }

        /**
         * Sets the {@code authOwnerId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authOwnerId(String value) {
            this.authOwnerId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountGetWebUrlParams build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            return new AccountGetWebUrlParams(path, identity, authOwnerId);
        }
    }
}

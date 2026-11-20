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
 * Repository identity and forge host.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitRepoIdentifierInfo(
    /** Repository identifier. */
    @JsonProperty("identifier") String identifier,
    /** Forge type: github or ado. */
    @JsonProperty("hostType") SessionWorkingDirectoryContextHostType hostType,
    /** Forge host. */
    @JsonProperty("host") String host
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitRepoIdentifierInfo {}

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
        private String identifier;
        private SessionWorkingDirectoryContextHostType hostType;
        private String host;

        /**
         * Sets the {@code identifier} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder identifier(String value) {
            this.identifier = value;
            return this;
        }

        /**
         * Sets the {@code hostType} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hostType(SessionWorkingDirectoryContextHostType value) {
            this.hostType = value;
            return this;
        }

        /**
         * Sets the {@code host} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder host(String value) {
            this.host = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitRepoIdentifierInfo build() {
            if (identifier == null) throw new IllegalStateException("Missing required property: identifier");
            if (hostType == null) throw new IllegalStateException("Missing required property: hostType");
            if (host == null) throw new IllegalStateException("Missing required property: host");
            return new GitRepoIdentifierInfo(identifier, hostType, host);
        }
    }
}

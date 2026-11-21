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
 * Reads repository resource disables using the bound session's configuration and trust authority.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionPermissionsRepositoryDisabledNamesParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Directory whose repository settings to inspect. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Resource disable list to inspect. */
    @JsonProperty("key") RepositoryDisabledKey key
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionPermissionsRepositoryDisabledNamesParams {}

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
        private String workingDirectory;
        private RepositoryDisabledKey key;

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
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code key} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder key(RepositoryDisabledKey value) {
            this.key = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPermissionsRepositoryDisabledNamesParams build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (key == null) throw new IllegalStateException("Missing required property: key");
            return new SessionPermissionsRepositoryDisabledNamesParams(sessionId, workingDirectory, key);
        }
    }
}

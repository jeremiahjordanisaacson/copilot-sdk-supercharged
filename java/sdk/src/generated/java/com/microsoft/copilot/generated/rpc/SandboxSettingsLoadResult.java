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
 * The resolved sandbox settings and the user's sparse stored override.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SandboxSettingsLoadResult(
    /** The resolved path to the user's settings.json file. */
    @JsonProperty("settingsPath") String settingsPath,
    /** Sandbox settings with all runtime defaults applied. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("resolved") Object resolved,
    /** The stored sandbox object, or an empty object when the user has no override. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("stored") Object stored
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxSettingsLoadResult {}

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
        private String settingsPath;
        private Object resolved;
        private boolean $presence1;
        private Object stored;
        private boolean $presence2;

        /**
         * Sets the {@code settingsPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder settingsPath(String value) {
            this.settingsPath = value;
            return this;
        }

        /**
         * Sets the {@code resolved} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder resolved(Object value) {
            this.resolved = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code stored} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder stored(Object value) {
            this.stored = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxSettingsLoadResult build() {
            if (settingsPath == null) throw new IllegalStateException("Missing required property: settingsPath");
            if (!$presence1) throw new IllegalStateException("Missing required property: resolved");
            if (!$presence2) throw new IllegalStateException("Missing required property: stored");
            return new SandboxSettingsLoadResult(settingsPath, resolved, stored);
        }
    }
}

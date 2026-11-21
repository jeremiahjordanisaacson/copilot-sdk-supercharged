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
 * Host-owned paths and environment for standalone skill management.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SkillResourceContext(
    /** Settings paths and hook environment for the original host. */
    @JsonProperty("context") UserSettingsHostContext context,
    /** Workspace directory for repository sources and relative paths. */
    @JsonProperty("workingDirectory") String workingDirectory
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SkillResourceContext {}

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
        private UserSettingsHostContext context;
        private String workingDirectory;

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(UserSettingsHostContext value) {
            this.context = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SkillResourceContext build() {
            if (context == null) throw new IllegalStateException("Missing required property: context");
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            return new SkillResourceContext(context, workingDirectory);
        }
    }
}

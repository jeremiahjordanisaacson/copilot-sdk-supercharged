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
 * Reads a contextual user document with trusted repository settings for one working directory.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record UserSettingsLoadForWorkingDirectoryParams(
    /** Host paths and environment that select the user document. */
    @JsonProperty("context") UserSettingsHostContext context,
    /** Directory whose repository and persisted trust determine the overlay. */
    @JsonProperty("workingDirectory") String workingDirectory
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    UserSettingsLoadForWorkingDirectoryParams {}

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
        public UserSettingsLoadForWorkingDirectoryParams build() {
            if (context == null) throw new IllegalStateException("Missing required property: context");
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            return new UserSettingsLoadForWorkingDirectoryParams(context, workingDirectory);
        }
    }
}

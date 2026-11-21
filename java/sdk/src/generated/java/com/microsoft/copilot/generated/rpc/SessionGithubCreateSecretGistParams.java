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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Creates a secret gist through the bound session's original credential owner.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionGithubCreateSecretGistParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** File names mapped to their text content. */
    @JsonProperty("files") Map<String, GitHubSecretGistFile> files,
    /** The description that GitHub stores with the gist. */
    @JsonProperty("description") String description,
    /** The purpose that selects account restrictions and error messages. */
    @JsonProperty("useCase") GitHubSecretGistUseCase useCase
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionGithubCreateSecretGistParams {}

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
        private String sessionId;
        private Map<String, GitHubSecretGistFile> files;
        private String description;
        private GitHubSecretGistUseCase useCase;

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
         * Sets the {@code files} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder files(Map<String, GitHubSecretGistFile> value) {
            this.files = value;
            return this;
        }

        /**
         * Sets the {@code description} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Sets the {@code useCase} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder useCase(GitHubSecretGistUseCase value) {
            this.useCase = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionGithubCreateSecretGistParams build() {
            if (files == null) throw new IllegalStateException("Missing required property: files");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (useCase == null) throw new IllegalStateException("Missing required property: useCase");
            return new SessionGithubCreateSecretGistParams(sessionId, files, description, useCase);
        }
    }
}

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Callback and filesystem hook sources.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenHookOptions(
    /** Explicit discovery-only file-hook roots, separate from callback hooks and filesystem grants. Available without enableConfigDiscovery or enableFileHooks, which controls repository-origin hooks only; hook/managed disables still apply. Relative SDK paths resolve once when supplied against the request's effective workingDirectory; retained roots do not move on cwd changes. Null and omission are equivalent; [] clears only the explicit SDK contribution. */
    @JsonProperty("directories") List<String> directories,
    /** Live gate for repository-origin file hooks, including the primary repository, trusted repository-configured roots, and accepted startup additional directories (including CLI --add-dir). False suppresses these hooks; explicit hookDirectories and SDK callbacks are independent. Additional-root eligibility and the DISABLE_ADDITIONAL_DIRECTORY_HOOKS escape hatch are captured at create/cold resume, not expanded by later grants or feature-flag changes. Discovery uses containing Git roots, so supply only trusted repositories: commands and prompts need no further approval. Policy, hook-disable settings, and sandbox rules still apply. Provider-backed sessions skip repository hooks with a warning on omission, skip silently on false, and reject explicit true. */
    @JsonProperty("enableRepositoryHooks") Boolean enableRepositoryHooks
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenHookOptions {}

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
        private List<String> directories;
        private Boolean enableRepositoryHooks;

        /**
         * Sets the {@code directories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder directories(List<String> value) {
            this.directories = value;
            return this;
        }

        /**
         * Sets the {@code enableRepositoryHooks} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableRepositoryHooks(Boolean value) {
            this.enableRepositoryHooks = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenHookOptions build() {
            return new SessionOpenHookOptions(directories, enableRepositoryHooks);
        }
    }
}

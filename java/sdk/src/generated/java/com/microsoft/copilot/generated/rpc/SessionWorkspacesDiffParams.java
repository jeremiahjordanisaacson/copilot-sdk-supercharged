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
 * Parameters for computing a workspace diff.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionWorkspacesDiffParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Diff mode requested by the client. */
    @JsonProperty("mode") WorkspaceDiffMode mode,
    /** When true, ignore whitespace-only changes (git `--ignore-all-space`). Defaults to false. */
    @JsonProperty("ignoreWhitespace") Boolean ignoreWhitespace,
    /** Include the full before/after text used to compute each session diff. Defaults to false; true is accepted only for session mode. Existing capture/read limits still apply, and binary or unavailable contents are not returned. This can substantially increase response size. */
    @JsonProperty("includeContents") Boolean includeContents
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionWorkspacesDiffParams {}

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
        private WorkspaceDiffMode mode;
        private Boolean ignoreWhitespace;
        private Boolean includeContents;

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
         * Sets the {@code mode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder mode(WorkspaceDiffMode value) {
            this.mode = value;
            return this;
        }

        /**
         * Sets the {@code ignoreWhitespace} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder ignoreWhitespace(Boolean value) {
            this.ignoreWhitespace = value;
            return this;
        }

        /**
         * Sets the {@code includeContents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder includeContents(Boolean value) {
            this.includeContents = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionWorkspacesDiffParams build() {
            if (mode == null) throw new IllegalStateException("Missing required property: mode");
            return new SessionWorkspacesDiffParams(sessionId, mode, ignoreWhitespace, includeContents);
        }
    }
}

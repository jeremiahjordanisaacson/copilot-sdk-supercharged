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
 * One memory to import through the session's cloud memory service.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionMemoryImportRecordParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Memory scope. Repository scope uses the session's resolved repository. */
    @JsonProperty("scope") MemoryImportScope scope,
    /** Short description of the memory's subject. */
    @JsonProperty("subject") String subject,
    /** Fact to preserve. */
    @JsonProperty("fact") String fact,
    /** Reason to preserve the fact. */
    @JsonProperty("reason") String reason,
    /** Sources that support the fact. */
    @JsonProperty("citations") String citations
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionMemoryImportRecordParams {}

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
        private MemoryImportScope scope;
        private String subject;
        private String fact;
        private String reason;
        private String citations;

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
         * Sets the {@code scope} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder scope(MemoryImportScope value) {
            this.scope = value;
            return this;
        }

        /**
         * Sets the {@code subject} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder subject(String value) {
            this.subject = value;
            return this;
        }

        /**
         * Sets the {@code fact} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder fact(String value) {
            this.fact = value;
            return this;
        }

        /**
         * Sets the {@code reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder reason(String value) {
            this.reason = value;
            return this;
        }

        /**
         * Sets the {@code citations} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder citations(String value) {
            this.citations = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionMemoryImportRecordParams build() {
            if (scope == null) throw new IllegalStateException("Missing required property: scope");
            if (subject == null) throw new IllegalStateException("Missing required property: subject");
            if (fact == null) throw new IllegalStateException("Missing required property: fact");
            if (reason == null) throw new IllegalStateException("Missing required property: reason");
            if (citations == null) throw new IllegalStateException("Missing required property: citations");
            return new SessionMemoryImportRecordParams(sessionId, scope, subject, fact, reason, citations);
        }
    }
}

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
 * Semantic session import outcome.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionsImportSemanticResult(
    /** Whether validation and persistence succeeded. */
    @JsonProperty("ok") Boolean ok,
    /** Stable operation code. */
    @JsonProperty("code") String code,
    /** Human-readable operation result. */
    @JsonProperty("message") String message,
    /** External session identifier from the document. */
    @JsonProperty("externalId") String externalId,
    /** Runtime session identifier after persistence, or incomplete rollback. */
    @JsonProperty("sessionId") String sessionId,
    /** Whether the call only validated input. */
    @JsonProperty("dryRun") Boolean dryRun,
    /** Imported or validated record counts. */
    @JsonProperty("imported") SemanticSessionImportCounts imported,
    /** Input or persistence diagnostics. */
    @JsonProperty("diagnostics") List<SemanticImportDiagnostic> diagnostics
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionsImportSemanticResult {}

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
        private Boolean ok;
        private String code;
        private String message;
        private String externalId;
        private String sessionId;
        private Boolean dryRun;
        private SemanticSessionImportCounts imported;
        private List<SemanticImportDiagnostic> diagnostics;

        /**
         * Sets the {@code ok} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ok(Boolean value) {
            this.ok = value;
            return this;
        }

        /**
         * Sets the {@code code} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder code(String value) {
            this.code = value;
            return this;
        }

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Sets the {@code externalId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder externalId(String value) {
            this.externalId = value;
            return this;
        }

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
         * Sets the {@code dryRun} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder dryRun(Boolean value) {
            this.dryRun = value;
            return this;
        }

        /**
         * Sets the {@code imported} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder imported(SemanticSessionImportCounts value) {
            this.imported = value;
            return this;
        }

        /**
         * Sets the {@code diagnostics} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder diagnostics(List<SemanticImportDiagnostic> value) {
            this.diagnostics = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsImportSemanticResult build() {
            if (ok == null) throw new IllegalStateException("Missing required property: ok");
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            if (dryRun == null) throw new IllegalStateException("Missing required property: dryRun");
            if (imported == null) throw new IllegalStateException("Missing required property: imported");
            if (diagnostics == null) throw new IllegalStateException("Missing required property: diagnostics");
            return new SessionsImportSemanticResult(ok, code, message, externalId, sessionId, dryRun, imported, diagnostics);
        }
    }
}

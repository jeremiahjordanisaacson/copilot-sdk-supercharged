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
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Result of a semantic memory import.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record MemoriesImportResult(
    /** Whether every record succeeded or was skipped. */
    @JsonProperty("ok") Boolean ok,
    /** Stable batch result code. */
    @JsonProperty("code") String code,
    /** Description of the batch outcome. */
    @JsonProperty("message") String message,
    /** Whether validation ran without submission. */
    @JsonProperty("dryRun") Boolean dryRun,
    /** Batch outcome counts. */
    @JsonProperty("imported") MemoryImportCounts imported,
    /** Input validation diagnostics. */
    @JsonProperty("diagnostics") List<ImportDiagnostic> diagnostics,
    /** Individual outcomes in input order. */
    @JsonProperty("records") List<MemoryImportItemResult> records
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public MemoriesImportResult {}

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
        private Boolean ok;
        private String code;
        private String message;
        private Boolean dryRun;
        private MemoryImportCounts imported;
        private List<ImportDiagnostic> diagnostics;
        private List<MemoryImportItemResult> records;

        /**
         * Sets the {@code ok} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Sets the {@code dryRun} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
        public Builder imported(MemoryImportCounts value) {
            this.imported = value;
            return this;
        }

        /**
         * Sets the {@code diagnostics} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder diagnostics(List<ImportDiagnostic> value) {
            this.diagnostics = value;
            return this;
        }

        /**
         * Sets the {@code records} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder records(List<MemoryImportItemResult> value) {
            this.records = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public MemoriesImportResult build() {
            if (ok == null) throw new IllegalStateException("Missing required property: ok");
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            if (dryRun == null) throw new IllegalStateException("Missing required property: dryRun");
            if (imported == null) throw new IllegalStateException("Missing required property: imported");
            if (diagnostics == null) throw new IllegalStateException("Missing required property: diagnostics");
            if (records == null) throw new IllegalStateException("Missing required property: records");
            return new MemoriesImportResult(ok, code, message, dryRun, imported, diagnostics, records);
        }
    }
}

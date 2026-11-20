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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Reconciles a generated artifact with the configured canonical store.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionsForgeProposalsReconcileParams(
    /** Repository and branch that own the proposal. */
    @JsonProperty("scope") ForgeProposalScope scope,
    /** Existing generation identifier, when generation started earlier. */
    @JsonProperty("proposalId") String proposalId,
    /** Whether a new generation may start when no existing generation completes. */
    @JsonProperty("triggerMode") ForgeProposalTriggerMode triggerMode,
    /** Content-addressed artifact identity. */
    @JsonProperty("fingerprint") String fingerprint,
    /** Ordered restoration metadata. */
    @JsonProperty("manifest") List<ForgeProposalManifestEntry> manifest,
    /** File-change counts. */
    @JsonProperty("summary") ForgeProposalSummary summary,
    /** Original workspace capture for an on-demand generation. */
    @JsonProperty("workspaceBeforeByPath") Map<String, String> workspaceBeforeByPath
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionsForgeProposalsReconcileParams {}

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
        private ForgeProposalScope scope;
        private String proposalId;
        private ForgeProposalTriggerMode triggerMode;
        private String fingerprint;
        private List<ForgeProposalManifestEntry> manifest;
        private ForgeProposalSummary summary;
        private Map<String, String> workspaceBeforeByPath;

        /**
         * Sets the {@code scope} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder scope(ForgeProposalScope value) {
            this.scope = value;
            return this;
        }

        /**
         * Sets the {@code proposalId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder proposalId(String value) {
            this.proposalId = value;
            return this;
        }

        /**
         * Sets the {@code triggerMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder triggerMode(ForgeProposalTriggerMode value) {
            this.triggerMode = value;
            return this;
        }

        /**
         * Sets the {@code fingerprint} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fingerprint(String value) {
            this.fingerprint = value;
            return this;
        }

        /**
         * Sets the {@code manifest} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder manifest(List<ForgeProposalManifestEntry> value) {
            this.manifest = value;
            return this;
        }

        /**
         * Sets the {@code summary} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder summary(ForgeProposalSummary value) {
            this.summary = value;
            return this;
        }

        /**
         * Sets the {@code workspaceBeforeByPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workspaceBeforeByPath(Map<String, String> value) {
            this.workspaceBeforeByPath = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsForgeProposalsReconcileParams build() {
            if (scope == null) throw new IllegalStateException("Missing required property: scope");
            if (triggerMode == null) throw new IllegalStateException("Missing required property: triggerMode");
            if (fingerprint == null) throw new IllegalStateException("Missing required property: fingerprint");
            if (manifest == null) throw new IllegalStateException("Missing required property: manifest");
            if (summary == null) throw new IllegalStateException("Missing required property: summary");
            return new SessionsForgeProposalsReconcileParams(scope, proposalId, triggerMode, fingerprint, manifest, summary, workspaceBeforeByPath);
        }
    }
}

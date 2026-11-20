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
 * Canonical Forge proposal with the unchanged persisted field names.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ForgeProposalRecord(
    /** Proposal identifier assigned at generation admission. */
    @JsonProperty("id") String id,
    /** Repository owner when known. */
    @JsonProperty("repo_owner") String repoOwner,
    /** Repository name when known. */
    @JsonProperty("repo_name") String repoName,
    /** Working tree root in persisted path format. */
    @JsonProperty("git_root_path") String gitRootPath,
    /** Proposal branch. */
    @JsonProperty("branch_name") String branchName,
    /** Original generation trigger. */
    @JsonProperty("trigger_mode") ForgeProposalTriggerMode triggerMode,
    /** Existing persisted proposal state. */
    @JsonProperty("status") ForgeProposalStatus status,
    /** Content-addressed proposal identity. */
    @JsonProperty("fingerprint") String fingerprint,
    /** Ordered restoration metadata. */
    @JsonProperty("manifest") List<ForgeProposalManifestEntry> manifest,
    /** File-change counts when generation produced them. */
    @JsonProperty("summary") ForgeProposalSummary summary,
    /** Replacement proposal identifier. */
    @JsonProperty("superseded_by") String supersededBy,
    /** Existing failure classification. */
    @JsonProperty("failure_reason") String failureReason,
    /** Creation time in ISO 8601 format. */
    @JsonProperty("created_at") String createdAt,
    /** Last transition time in ISO 8601 format. */
    @JsonProperty("updated_at") String updatedAt
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ForgeProposalRecord {}

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
        private String id;
        private String repoOwner;
        private String repoName;
        private String gitRootPath;
        private String branchName;
        private ForgeProposalTriggerMode triggerMode;
        private ForgeProposalStatus status;
        private String fingerprint;
        private List<ForgeProposalManifestEntry> manifest;
        private ForgeProposalSummary summary;
        private String supersededBy;
        private String failureReason;
        private String createdAt;
        private String updatedAt;

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder id(String value) {
            this.id = value;
            return this;
        }

        /**
         * Sets the {@code repo_owner} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder repoOwner(String value) {
            this.repoOwner = value;
            return this;
        }

        /**
         * Sets the {@code repo_name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder repoName(String value) {
            this.repoName = value;
            return this;
        }

        /**
         * Sets the {@code git_root_path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitRootPath(String value) {
            this.gitRootPath = value;
            return this;
        }

        /**
         * Sets the {@code branch_name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder branchName(String value) {
            this.branchName = value;
            return this;
        }

        /**
         * Sets the {@code trigger_mode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder triggerMode(ForgeProposalTriggerMode value) {
            this.triggerMode = value;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(ForgeProposalStatus value) {
            this.status = value;
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
         * Sets the {@code superseded_by} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder supersededBy(String value) {
            this.supersededBy = value;
            return this;
        }

        /**
         * Sets the {@code failure_reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder failureReason(String value) {
            this.failureReason = value;
            return this;
        }

        /**
         * Sets the {@code created_at} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder createdAt(String value) {
            this.createdAt = value;
            return this;
        }

        /**
         * Sets the {@code updated_at} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder updatedAt(String value) {
            this.updatedAt = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ForgeProposalRecord build() {
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (gitRootPath == null) throw new IllegalStateException("Missing required property: git_root_path");
            if (branchName == null) throw new IllegalStateException("Missing required property: branch_name");
            if (triggerMode == null) throw new IllegalStateException("Missing required property: trigger_mode");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (manifest == null) throw new IllegalStateException("Missing required property: manifest");
            if (createdAt == null) throw new IllegalStateException("Missing required property: created_at");
            if (updatedAt == null) throw new IllegalStateException("Missing required property: updated_at");
            return new ForgeProposalRecord(id, repoOwner, repoName, gitRootPath, branchName, triggerMode, status, fingerprint, manifest, summary, supersededBy, failureReason, createdAt, updatedAt);
        }
    }
}

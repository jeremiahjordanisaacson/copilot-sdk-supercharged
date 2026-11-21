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
 * One available plugin, including inactive live plugins and trusted bundled provenance.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionPluginInventoryEntry(
    /** Plugin name from its manifest or installation record. */
    @JsonProperty("name") String name,
    /** Marketplace name, or an empty string for a direct or bundled plugin. */
    @JsonProperty("marketplace") String marketplace,
    /** Current effective enablement, independent of presence on disk. */
    @JsonProperty("enabled") Boolean enabled,
    /** Version reported by the manifest. */
    @JsonProperty("version") String version,
    /** Trusted runtime provenance; bundled plugins use builtin. */
    @JsonProperty("source") String source,
    /** Manifest description, with surrounding whitespace removed for installed and bundled entries. */
    @JsonProperty("description") String description,
    /** Marketplace directory for a live plugin that has no persisted copy. */
    @JsonProperty("installedFrom") String installedFrom,
    /** Resolved content directory; absent for live plugins whose content is unresolved.
This path does not grant filesystem access. */
    @JsonProperty("rootPath") String rootPath,
    /** Identifies an explicit host mount rather than a persisted installation. */
    @JsonProperty("external") Boolean external
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionPluginInventoryEntry {}

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
        private String name;
        private String marketplace;
        private Boolean enabled;
        private String version;
        private String source;
        private String description;
        private String installedFrom;
        private String rootPath;
        private Boolean external;

        /**
         * Sets the {@code name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder name(String value) {
            this.name = value;
            return this;
        }

        /**
         * Sets the {@code marketplace} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder marketplace(String value) {
            this.marketplace = value;
            return this;
        }

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Sets the {@code version} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder version(String value) {
            this.version = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(String value) {
            this.source = value;
            return this;
        }

        /**
         * Sets the {@code description} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Sets the {@code installedFrom} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder installedFrom(String value) {
            this.installedFrom = value;
            return this;
        }

        /**
         * Sets the {@code rootPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder rootPath(String value) {
            this.rootPath = value;
            return this;
        }

        /**
         * Sets the {@code external} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder external(Boolean value) {
            this.external = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPluginInventoryEntry build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (marketplace == null) throw new IllegalStateException("Missing required property: marketplace");
            if (enabled == null) throw new IllegalStateException("Missing required property: enabled");
            return new SessionPluginInventoryEntry(name, marketplace, enabled, version, source, description, installedFrom, rootPath, external);
        }
    }
}

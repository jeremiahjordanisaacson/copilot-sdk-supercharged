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
 * The persisted result of a skill source installation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SkillResourceInstallResult(
    /** Source classification. */
    @JsonProperty("kind") SkillResourceInstallKind kind,
    /** Installed definition path or registered directory. */
    @JsonProperty("path") String path,
    /** Skill name, absent for a registered directory. */
    @JsonProperty("name") String name,
    /** Destination scope, absent for a registered directory. */
    @JsonProperty("scope") SkillResourceInstallScope scope
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SkillResourceInstallResult {}

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
        private SkillResourceInstallKind kind;
        private String path;
        private String name;
        private SkillResourceInstallScope scope;

        /**
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder kind(SkillResourceInstallKind value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder path(String value) {
            this.path = value;
            return this;
        }

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
         * Sets the {@code scope} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder scope(SkillResourceInstallScope value) {
            this.scope = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SkillResourceInstallResult build() {
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            if (path == null) throw new IllegalStateException("Missing required property: path");
            return new SkillResourceInstallResult(kind, path, name, scope);
        }
    }
}

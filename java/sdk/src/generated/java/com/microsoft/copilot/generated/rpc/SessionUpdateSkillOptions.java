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
 * Skill loading, sources, and provider capability.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionUpdateSkillOptions.$NullableDeserializer.class)
public record SessionUpdateSkillOptions(
    /** Explicit discovery-only skill roots, not filesystem grants. Available without enableConfigDiscovery, subject to skill/managed disables. Relative SDK paths resolve once when supplied against the request's effective workingDirectory; retained roots do not move on cwd changes. Null and omission are equivalent; [] clears only the explicit SDK contribution. */
    @JsonProperty("directories") List<String> directories,
    /** Skill scan directories and descendants excluded from discovery. Supports `~`-relative paths. */
    @JsonProperty("ignoredLocations") List<String> ignoredLocations,
    /** Built-in skill names to include in this session. When specified, only these runtime-bundled skills are available. Skills from other sources with the same name remain available. Set to null to remove the allowlist restriction. */
    @JsonProperty("includedBuiltinSkills") List<String> includedBuiltinSkills,
    /** Skill IDs that should be excluded from this session. */
    @JsonProperty("disabledSkills") List<String> disabledSkills,
    /** Whether skill loading is enabled. Explicit false disables every source, including explicit SDK roots and a bound SDK provider; changing the value invalidates the loaded skill snapshot. When omitted, creation falls back to enableConfigDiscovery unless explicit SDK skill roots or an SDK skill provider are supplied. */
    @JsonProperty("enabled") Boolean enabled,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateSkillOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private List<String> ignoredLocations;
        private List<String> includedBuiltinSkills;
        private boolean $presence2;
        private List<String> disabledSkills;
        private Boolean enabled;

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
         * Sets the {@code ignoredLocations} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ignoredLocations(List<String> value) {
            this.ignoredLocations = value;
            return this;
        }

        /**
         * Sets the {@code includedBuiltinSkills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includedBuiltinSkills(List<String> value) {
            this.includedBuiltinSkills = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code disabledSkills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disabledSkills(List<String> value) {
            this.disabledSkills = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateSkillOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence2 && includedBuiltinSkills == null) $explicitNulls.add("includedBuiltinSkills");
            return new SessionUpdateSkillOptions(directories, ignoredLocations, includedBuiltinSkills, disabledSkills, enabled, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("includedBuiltinSkills") && includedBuiltinSkills == null) values.put("includedBuiltinSkills", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionUpdateSkillOptions> {
        @Override
        public SessionUpdateSkillOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionUpdateSkillOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("includedBuiltinSkills") && tree.get("includedBuiltinSkills").isNull()) $explicitNulls.add("includedBuiltinSkills");
            return new SessionUpdateSkillOptions(readProperty(context, tree, "directories", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "ignoredLocations", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "includedBuiltinSkills", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "disabledSkills", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "enabled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

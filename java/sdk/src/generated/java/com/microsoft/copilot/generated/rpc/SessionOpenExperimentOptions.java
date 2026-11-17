/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Feature experimentation and trusted assignments.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionOpenExperimentOptions.$NullableDeserializer.class)
public record SessionOpenExperimentOptions(
    /** ExP assignment ('flight') data injected by an SDK integrator, in the same JSON shape the Copilot CLI fetches from the experimentation service (CopilotExpAssignmentResponse). When supplied this is fed into the FeatureFlagService exactly like CLI-fetched assignments and ExP-backed flags wait for it. When absent the session does not block on ExP. */
    @JsonProperty("assignments") Object assignments,
    /** Trusted host defaults consumed by the runtime during session construction. */
    @JsonProperty("configuration") SessionFeatureFlagConfiguration configuration,
    /** Feature-flag values resolved by the host. */
    @JsonProperty("flags") Map<String, Boolean> flags,
    /** Whether experimental behavior is enabled. */
    @JsonProperty("enableExperimentalMode") Boolean enableExperimentalMode,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenExperimentOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object assignments;
        private boolean $presence0;
        private SessionFeatureFlagConfiguration configuration;
        private Map<String, Boolean> flags;
        private Boolean enableExperimentalMode;

        /**
         * Sets the {@code assignments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder assignments(Object value) {
            this.assignments = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code configuration} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configuration(SessionFeatureFlagConfiguration value) {
            this.configuration = value;
            return this;
        }

        /**
         * Sets the {@code flags} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flags(Map<String, Boolean> value) {
            this.flags = value;
            return this;
        }

        /**
         * Sets the {@code enableExperimentalMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableExperimentalMode(Boolean value) {
            this.enableExperimentalMode = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenExperimentOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && assignments == null) $explicitNulls.add("assignments");
            return new SessionOpenExperimentOptions(assignments, configuration, flags, enableExperimentalMode, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("assignments") && assignments == null) values.put("assignments", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionOpenExperimentOptions> {
        @Override
        public SessionOpenExperimentOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionOpenExperimentOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("assignments") && tree.get("assignments").isNull()) $explicitNulls.add("assignments");
            return new SessionOpenExperimentOptions(readProperty(context, tree, "assignments", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "configuration", new com.fasterxml.jackson.core.type.TypeReference<SessionFeatureFlagConfiguration>() {}), readProperty(context, tree, "flags", new com.fasterxml.jackson.core.type.TypeReference<Map<String, Boolean>>() {}), readProperty(context, tree, "enableExperimentalMode", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * Update availability without installation or checkout mutation. An error means availability is unknown.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionPluginsMarketplacesCheckUpdateResult.$NullableDeserializer.class)
public record SessionPluginsMarketplacesCheckUpdateResult(
    /** True only when the probe establishes an available update. */
    @JsonProperty("updateAvailable") Boolean updateAvailable,
    /** Installed plugin version, when known. */
    @JsonProperty("installedVersion") String installedVersion,
    /** Cached catalog version, when known. */
    @JsonProperty("latestVersion") String latestVersion,
    /** True when a marketplace has no cached checkout yet. */
    @JsonProperty("notFetched") Boolean notFetched,
    /** Explanation when availability cannot be established. */
    @JsonProperty("error") String error,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionPluginsMarketplacesCheckUpdateResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Boolean updateAvailable;
        private String installedVersion;
        private boolean $presence1;
        private String latestVersion;
        private boolean $presence2;
        private Boolean notFetched;
        private boolean $presence3;
        private String error;
        private boolean $presence4;

        /**
         * Sets the {@code updateAvailable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder updateAvailable(Boolean value) {
            this.updateAvailable = value;
            return this;
        }

        /**
         * Sets the {@code installedVersion} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder installedVersion(String value) {
            this.installedVersion = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code latestVersion} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder latestVersion(String value) {
            this.latestVersion = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code notFetched} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder notFetched(Boolean value) {
            this.notFetched = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code error} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder error(String value) {
            this.error = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPluginsMarketplacesCheckUpdateResult build() {
            if (updateAvailable == null) throw new IllegalStateException("Missing required property: updateAvailable");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && installedVersion == null) $explicitNulls.add("installedVersion");
            if ($presence2 && latestVersion == null) $explicitNulls.add("latestVersion");
            if ($presence3 && notFetched == null) $explicitNulls.add("notFetched");
            if ($presence4 && error == null) $explicitNulls.add("error");
            return new SessionPluginsMarketplacesCheckUpdateResult(updateAvailable, installedVersion, latestVersion, notFetched, error, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("installedVersion") && installedVersion == null) values.put("installedVersion", null);
        if ($explicitNulls.contains("latestVersion") && latestVersion == null) values.put("latestVersion", null);
        if ($explicitNulls.contains("notFetched") && notFetched == null) values.put("notFetched", null);
        if ($explicitNulls.contains("error") && error == null) values.put("error", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionPluginsMarketplacesCheckUpdateResult> {
        @Override
        public SessionPluginsMarketplacesCheckUpdateResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionPluginsMarketplacesCheckUpdateResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("installedVersion") && tree.get("installedVersion").isNull()) $explicitNulls.add("installedVersion");
            if (tree.has("latestVersion") && tree.get("latestVersion").isNull()) $explicitNulls.add("latestVersion");
            if (tree.has("notFetched") && tree.get("notFetched").isNull()) $explicitNulls.add("notFetched");
            if (tree.has("error") && tree.get("error").isNull()) $explicitNulls.add("error");
            return new SessionPluginsMarketplacesCheckUpdateResult(readProperty(context, tree, "updateAvailable", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "installedVersion", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "latestVersion", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "notFetched", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "error", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * A marketplace plugin with its installation state in the session's profile.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = PluginMarketplaceSearchEntry.$NullableDeserializer.class)
public record PluginMarketplaceSearchEntry(
    /** Name declared by the marketplace entry. */
    @JsonProperty("name") String name,
    /** Configured marketplace name. */
    @JsonProperty("marketplace") String marketplace,
    /** Plugin source accepted by session.plugins.install. */
    @JsonProperty("installSpec") String installSpec,
    /** Whether this profile has an installation of this plugin from this marketplace. */
    @JsonProperty("installed") Boolean installed,
    /** Runtime activation state for a plugin read directly from its marketplace directory. */
    @JsonProperty("live") PluginMarketplaceLiveState live,
    /** Description declared by the marketplace entry. */
    @JsonProperty("description") String description,
    /** Version declared by the marketplace entry, when present. */
    @JsonProperty("version") String version,
    /** Marketplace source shown for this catalog entry, when available. */
    @JsonProperty("sourceUrl") String sourceUrl,
    /** Plugin-supplied catalog metadata. These fields grant no session or managed-policy authority. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("metadata") Object metadata,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PluginMarketplaceSearchEntry { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String installSpec;
        private Boolean installed;
        private PluginMarketplaceLiveState live;
        private boolean $presence4;
        private String description;
        private boolean $presence5;
        private String version;
        private boolean $presence6;
        private String sourceUrl;
        private boolean $presence7;
        private Object metadata;
        private boolean $presence8;

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
         * Sets the {@code installSpec} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder installSpec(String value) {
            this.installSpec = value;
            return this;
        }

        /**
         * Sets the {@code installed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder installed(Boolean value) {
            this.installed = value;
            return this;
        }

        /**
         * Sets the {@code live} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder live(PluginMarketplaceLiveState value) {
            this.live = value;
            this.$presence4 = true;
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
            this.$presence5 = true;
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
            this.$presence6 = true;
            return this;
        }

        /**
         * Sets the {@code sourceUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sourceUrl(String value) {
            this.sourceUrl = value;
            this.$presence7 = true;
            return this;
        }

        /**
         * Sets the {@code metadata} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder metadata(Object value) {
            this.metadata = value;
            this.$presence8 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginMarketplaceSearchEntry build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (marketplace == null) throw new IllegalStateException("Missing required property: marketplace");
            if (installSpec == null) throw new IllegalStateException("Missing required property: installSpec");
            if (installed == null) throw new IllegalStateException("Missing required property: installed");
            if (!$presence8) throw new IllegalStateException("Missing required property: metadata");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence4 && live == null) $explicitNulls.add("live");
            if ($presence5 && description == null) $explicitNulls.add("description");
            if ($presence6 && version == null) $explicitNulls.add("version");
            if ($presence7 && sourceUrl == null) $explicitNulls.add("sourceUrl");
            return new PluginMarketplaceSearchEntry(name, marketplace, installSpec, installed, live, description, version, sourceUrl, metadata, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("live") && live == null) values.put("live", null);
        if ($explicitNulls.contains("description") && description == null) values.put("description", null);
        if ($explicitNulls.contains("version") && version == null) values.put("version", null);
        if ($explicitNulls.contains("sourceUrl") && sourceUrl == null) values.put("sourceUrl", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<PluginMarketplaceSearchEntry> {
        @Override
        public PluginMarketplaceSearchEntry deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(PluginMarketplaceSearchEntry.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("live") && tree.get("live").isNull()) $explicitNulls.add("live");
            if (tree.has("description") && tree.get("description").isNull()) $explicitNulls.add("description");
            if (tree.has("version") && tree.get("version").isNull()) $explicitNulls.add("version");
            if (tree.has("sourceUrl") && tree.get("sourceUrl").isNull()) $explicitNulls.add("sourceUrl");
            return new PluginMarketplaceSearchEntry(readProperty(context, tree, "name", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "marketplace", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "installSpec", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "installed", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "live", new com.fasterxml.jackson.core.type.TypeReference<PluginMarketplaceLiveState>() {}), readProperty(context, tree, "description", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "version", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sourceUrl", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "metadata", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * Existing persisted restoration metadata for one proposal file.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = ForgeProposalManifestEntry.$NullableDeserializer.class)
record ForgeProposalManifestEntry(
    /** Path in the proposal's working tree. */
    @JsonProperty("path") String path,
    /** Original path of a renamed file. */
    @JsonProperty("old_path") String oldPath,
    /** File-change classification. */
    @JsonProperty("change_type") ForgeProposalChangeType changeType,
    /** Content hash before generation; null means that no file existed. */
    @JsonProperty("before_hash") String beforeHash,
    /** Expected content hash after generation; null means deletion. */
    @JsonProperty("after_hash") String afterHash,
    /** Captured content before generation. Absence remains distinct from explicit null. */
    @JsonProperty("before_content") String beforeContent,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ForgeProposalManifestEntry { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String path;
        private String oldPath;
        private ForgeProposalChangeType changeType;
        private String beforeHash;
        private boolean $presence3;
        private String afterHash;
        private boolean $presence4;
        private String beforeContent;
        private boolean $presence5;

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
         * Sets the {@code old_path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder oldPath(String value) {
            this.oldPath = value;
            return this;
        }

        /**
         * Sets the {@code change_type} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder changeType(ForgeProposalChangeType value) {
            this.changeType = value;
            return this;
        }

        /**
         * Sets the {@code before_hash} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder beforeHash(String value) {
            this.beforeHash = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code after_hash} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder afterHash(String value) {
            this.afterHash = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Sets the {@code before_content} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder beforeContent(String value) {
            this.beforeContent = value;
            this.$presence5 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ForgeProposalManifestEntry build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (changeType == null) throw new IllegalStateException("Missing required property: change_type");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && beforeHash == null) $explicitNulls.add("before_hash");
            if ($presence4 && afterHash == null) $explicitNulls.add("after_hash");
            if ($presence5 && beforeContent == null) $explicitNulls.add("before_content");
            return new ForgeProposalManifestEntry(path, oldPath, changeType, beforeHash, afterHash, beforeContent, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("before_hash") && beforeHash == null) values.put("before_hash", null);
        if ($explicitNulls.contains("after_hash") && afterHash == null) values.put("after_hash", null);
        if ($explicitNulls.contains("before_content") && beforeContent == null) values.put("before_content", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<ForgeProposalManifestEntry> {
        @Override
        public ForgeProposalManifestEntry deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(ForgeProposalManifestEntry.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("before_hash") && tree.get("before_hash").isNull()) $explicitNulls.add("before_hash");
            if (tree.has("after_hash") && tree.get("after_hash").isNull()) $explicitNulls.add("after_hash");
            if (tree.has("before_content") && tree.get("before_content").isNull()) $explicitNulls.add("before_content");
            return new ForgeProposalManifestEntry(readProperty(context, tree, "path", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "old_path", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "change_type", new com.fasterxml.jackson.core.type.TypeReference<ForgeProposalChangeType>() {}), readProperty(context, tree, "before_hash", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "after_hash", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "before_content", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

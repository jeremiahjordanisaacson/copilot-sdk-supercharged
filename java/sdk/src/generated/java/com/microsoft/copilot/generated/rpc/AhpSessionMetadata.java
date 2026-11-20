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
 * Host timestamps and title for the attached session.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = AhpSessionMetadata.$NullableDeserializer.class)
record AhpSessionMetadata(
    /** Runtime identity used by all standard session methods. */
    @JsonProperty("sessionId") String sessionId,
    /** Host creation time in ISO 8601 format. */
    @JsonProperty("startTime") String startTime,
    /** Host modification time in ISO 8601 format. */
    @JsonProperty("modifiedTime") String modifiedTime,
    /** Host session title. */
    @JsonProperty("summary") String summary,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpSessionMetadata { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String sessionId;
        private String startTime;
        private String modifiedTime;
        private String summary;
        private boolean $presence3;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code startTime} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder startTime(String value) {
            this.startTime = value;
            return this;
        }

        /**
         * Sets the {@code modifiedTime} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder modifiedTime(String value) {
            this.modifiedTime = value;
            return this;
        }

        /**
         * Sets the {@code summary} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder summary(String value) {
            this.summary = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpSessionMetadata build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (startTime == null) throw new IllegalStateException("Missing required property: startTime");
            if (modifiedTime == null) throw new IllegalStateException("Missing required property: modifiedTime");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && summary == null) $explicitNulls.add("summary");
            return new AhpSessionMetadata(sessionId, startTime, modifiedTime, summary, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("summary") && summary == null) values.put("summary", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<AhpSessionMetadata> {
        @Override
        public AhpSessionMetadata deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(AhpSessionMetadata.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("summary") && tree.get("summary").isNull()) $explicitNulls.add("summary");
            return new AhpSessionMetadata(readProperty(context, tree, "sessionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "startTime", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "modifiedTime", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "summary", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

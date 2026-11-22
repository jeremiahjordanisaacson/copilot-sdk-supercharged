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
 * The result of one host-owned ACP telemetry step.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = AcpTelemetryReply.$NullableDeserializer.class)
record AcpTelemetryReply(
    /** The reservation produced by prepare. */
    @JsonProperty("preparationId") String preparationId,
    /** The captured managed sandbox policy. */
    @JsonProperty("sandboxFloor") Object sandboxFloor,
    /** The captured account selection. */
    @JsonProperty("selectionId") String selectionId,
    /** Whether the host cannot use its saved sandbox setting. */
    @JsonProperty("sandboxHostUnsupported") Boolean sandboxHostUnsupported,
    /** Whether apply bound the captured credentials. */
    @JsonProperty("applied") Boolean applied,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AcpTelemetryReply { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String preparationId;
        private Object sandboxFloor;
        private boolean $presence1;
        private String selectionId;
        private Boolean sandboxHostUnsupported;
        private Boolean applied;

        /**
         * Sets the {@code preparationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder preparationId(String value) {
            this.preparationId = value;
            return this;
        }

        /**
         * Sets the {@code sandboxFloor} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sandboxFloor(Object value) {
            this.sandboxFloor = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code selectionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder selectionId(String value) {
            this.selectionId = value;
            return this;
        }

        /**
         * Sets the {@code sandboxHostUnsupported} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sandboxHostUnsupported(Boolean value) {
            this.sandboxHostUnsupported = value;
            return this;
        }

        /**
         * Sets the {@code applied} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder applied(Boolean value) {
            this.applied = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AcpTelemetryReply build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && sandboxFloor == null) $explicitNulls.add("sandboxFloor");
            return new AcpTelemetryReply(preparationId, sandboxFloor, selectionId, sandboxHostUnsupported, applied, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("sandboxFloor") && sandboxFloor == null) values.put("sandboxFloor", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<AcpTelemetryReply> {
        @Override
        public AcpTelemetryReply deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(AcpTelemetryReply.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("sandboxFloor") && tree.get("sandboxFloor").isNull()) $explicitNulls.add("sandboxFloor");
            return new AcpTelemetryReply(readProperty(context, tree, "preparationId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sandboxFloor", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "selectionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sandboxHostUnsupported", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "applied", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

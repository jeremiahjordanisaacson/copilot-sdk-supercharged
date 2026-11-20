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
 * Creates an inert outgoing connection under the caller's existing runtime auth owner.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = AhpConnectionsCreateParams.$NullableDeserializer.class)
record AhpConnectionsCreateParams(
    /** The one transport this connection uses. */
    @JsonProperty("transport") Object transport,
    /** Optional stable client identity. Omission uses Mission Control's assigned identity or the engine default. */
    @JsonProperty("clientId") String clientId,
    /** Optional connection-owned diagnostic log created before authentication. */
    @JsonProperty("logId") String logId,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionsCreateParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object transport;
        private String clientId;
        private boolean $presence1;
        private String logId;
        private boolean $presence2;

        /**
         * Sets the {@code transport} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder transport(Object value) {
            this.transport = value;
            return this;
        }

        /**
         * Sets the {@code clientId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder clientId(String value) {
            this.clientId = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code logId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder logId(String value) {
            this.logId = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionsCreateParams build() {
            if (transport == null) throw new IllegalStateException("Missing required property: transport");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && clientId == null) $explicitNulls.add("clientId");
            if ($presence2 && logId == null) $explicitNulls.add("logId");
            return new AhpConnectionsCreateParams(transport, clientId, logId, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("clientId") && clientId == null) values.put("clientId", null);
        if ($explicitNulls.contains("logId") && logId == null) values.put("logId", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<AhpConnectionsCreateParams> {
        @Override
        public AhpConnectionsCreateParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(AhpConnectionsCreateParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("clientId") && tree.get("clientId").isNull()) $explicitNulls.add("clientId");
            if (tree.has("logId") && tree.get("logId").isNull()) $explicitNulls.add("logId");
            return new AhpConnectionsCreateParams(readProperty(context, tree, "transport", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "clientId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "logId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * Publishes a controller event through the runtime-owned session log.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionEventLogPublishParams.$NullableDeserializer.class)
record SessionEventLogPublishParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Controller-assigned UUID returned to its synchronous UI caller. */
    @JsonProperty("eventId") String eventId,
    /** Existing session event type. */
    @JsonProperty("eventType") String eventType,
    /** Serialized event payload. */
    @JsonProperty("dataJson") String dataJson,
    /** Whether this event belongs only to the live event stream. */
    @JsonProperty("ephemeral") Boolean ephemeral,
    /** Optional agent attribution. */
    @JsonProperty("agentId") String agentId,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionEventLogPublishParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String sessionId;
        private String eventId;
        private String eventType;
        private String dataJson;
        private Boolean ephemeral;
        private String agentId;
        private boolean $presence5;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code eventId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder eventId(String value) {
            this.eventId = value;
            return this;
        }

        /**
         * Sets the {@code eventType} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder eventType(String value) {
            this.eventType = value;
            return this;
        }

        /**
         * Sets the {@code dataJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder dataJson(String value) {
            this.dataJson = value;
            return this;
        }

        /**
         * Sets the {@code ephemeral} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder ephemeral(Boolean value) {
            this.ephemeral = value;
            return this;
        }

        /**
         * Sets the {@code agentId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder agentId(String value) {
            this.agentId = value;
            this.$presence5 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionEventLogPublishParams build() {
            if (eventId == null) throw new IllegalStateException("Missing required property: eventId");
            if (eventType == null) throw new IllegalStateException("Missing required property: eventType");
            if (dataJson == null) throw new IllegalStateException("Missing required property: dataJson");
            if (ephemeral == null) throw new IllegalStateException("Missing required property: ephemeral");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence5 && agentId == null) $explicitNulls.add("agentId");
            return new SessionEventLogPublishParams(sessionId, eventId, eventType, dataJson, ephemeral, agentId, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("agentId") && agentId == null) values.put("agentId", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionEventLogPublishParams> {
        @Override
        public SessionEventLogPublishParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionEventLogPublishParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("agentId") && tree.get("agentId").isNull()) $explicitNulls.add("agentId");
            return new SessionEventLogPublishParams(readProperty(context, tree, "sessionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "eventId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "eventType", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "dataJson", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "ephemeral", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "agentId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

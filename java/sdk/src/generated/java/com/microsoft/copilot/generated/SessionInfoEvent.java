/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * Session event "session.info". Informational message for timeline display with categorization
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionInfoEvent extends SessionEvent {

    @Override
    public String getType() { return "session.info"; }

    @JsonProperty("data")
    private SessionInfoEventData data;

    public SessionInfoEventData getData() { return data; }
    public void setData(SessionInfoEventData data) { this.data = data; }

    /** Data payload for {@link SessionInfoEvent}. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionInfoEventData.$NullableDeserializer.class)
    public record SessionInfoEventData(
        /** Credential-free provider routing policy applied at this configuration boundary. */
        @JsonProperty("providerRouting") Object providerRouting,
        /** Category of informational message (e.g., "notification", "timing", "context_window", "mcp", "snapshot", "configuration", "authentication", "model") */
        @JsonProperty("infoType") String infoType,
        /** Human-readable informational message for display in the timeline */
        @JsonProperty("message") String message,
        /** Optional URL associated with this message that the user can open in a browser */
        @JsonProperty("url") String url,
        /** Optional actionable tip displayed with this message */
        @JsonProperty("tip") String tip,
        /** Reasoning configuration consumed at this conversation boundary. Present on reasoning_effort updates; preserves the original request baseline and applied effort without creating a user message. */
        @JsonProperty("responsesReasoning") ResponsesReasoning responsesReasoning,
        @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public SessionInfoEventData { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
            private Object providerRouting;
            private boolean $presence0;
            private String infoType;
            private String message;
            private String url;
            private String tip;
            private ResponsesReasoning responsesReasoning;

            /**
             * Sets the {@code providerRouting} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder providerRouting(Object value) {
                this.providerRouting = value;
                this.$presence0 = true;
                return this;
            }

            /**
             * Sets the {@code infoType} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder infoType(String value) {
                this.infoType = value;
                return this;
            }

            /**
             * Sets the {@code message} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder message(String value) {
                this.message = value;
                return this;
            }

            /**
             * Sets the {@code url} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder url(String value) {
                this.url = value;
                return this;
            }

            /**
             * Sets the {@code tip} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder tip(String value) {
                this.tip = value;
                return this;
            }

            /**
             * Sets the {@code responsesReasoning} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder responsesReasoning(ResponsesReasoning value) {
                this.responsesReasoning = value;
                return this;
            }

            /**
             * Builds a record after checking required property presence.
             * @return a new record
             * @throws IllegalStateException if a required property is missing
             * @since 2.0.0
             */
            public SessionInfoEventData build() {
                if (infoType == null) throw new IllegalStateException("Missing required property: infoType");
                if (message == null) throw new IllegalStateException("Missing required property: message");
                java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
                if ($presence0 && providerRouting == null) $explicitNulls.add("providerRouting");
                return new SessionInfoEventData(providerRouting, infoType, message, url, tip, responsesReasoning, $explicitNulls);
            }
        }

        @com.fasterxml.jackson.annotation.JsonAnyGetter
        @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
        public java.util.Map<String, Object> $nullableProperties() {
            java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
            if ($explicitNulls.contains("providerRouting") && providerRouting == null) values.put("providerRouting", null);
            return values;
        }

        public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionInfoEventData> {
            @Override
            public SessionInfoEventData deserialize(com.fasterxml.jackson.core.JsonParser parser,
                    com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
                com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
                if (!tree.isObject()) return context.reportInputMismatch(SessionInfoEventData.class, "Expected a JSON object");
                java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
                if (tree.has("providerRouting") && tree.get("providerRouting").isNull()) $explicitNulls.add("providerRouting");
                return new SessionInfoEventData(readProperty(context, tree, "providerRouting", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "infoType", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "message", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "url", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "tip", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "responsesReasoning", new com.fasterxml.jackson.core.type.TypeReference<ResponsesReasoning>() {}), $explicitNulls);
            }

            private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                    com.fasterxml.jackson.databind.JsonNode tree, String name,
                    com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
                if (!tree.has(name)) return null;
                return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
            }
        }
    }
}

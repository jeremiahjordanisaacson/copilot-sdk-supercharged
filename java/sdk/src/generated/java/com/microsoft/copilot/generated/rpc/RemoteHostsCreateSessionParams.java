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
 * Create a session on the existing remote host.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = RemoteHostsCreateSessionParams.$NullableDeserializer.class)
record RemoteHostsCreateSessionParams(
    /** Owned outgoing connection UUID. */
    @JsonProperty("hostId") String hostId,
    /** Optional initial prompt, sent exactly once by the owning engine. */
    @JsonProperty("text") String text,
    /** Native AHP session options, including model, provider, directory, and project selection. */
    @JsonProperty("options") Object options,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteHostsCreateSessionParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String hostId;
        private String text;
        private boolean $presence1;
        private Object options;
        private boolean $presence2;

        /**
         * Sets the {@code hostId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hostId(String value) {
            this.hostId = value;
            return this;
        }

        /**
         * Sets the {@code text} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder text(String value) {
            this.text = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code options} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder options(Object value) {
            this.options = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostsCreateSessionParams build() {
            if (hostId == null) throw new IllegalStateException("Missing required property: hostId");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && text == null) $explicitNulls.add("text");
            if ($presence2 && options == null) $explicitNulls.add("options");
            return new RemoteHostsCreateSessionParams(hostId, text, options, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("text") && text == null) values.put("text", null);
        if ($explicitNulls.contains("options") && options == null) values.put("options", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<RemoteHostsCreateSessionParams> {
        @Override
        public RemoteHostsCreateSessionParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(RemoteHostsCreateSessionParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("text") && tree.get("text").isNull()) $explicitNulls.add("text");
            if (tree.has("options") && tree.get("options").isNull()) $explicitNulls.add("options");
            return new RemoteHostsCreateSessionParams(readProperty(context, tree, "hostId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "text", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "options", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

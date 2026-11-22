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
 * Allocate an inert outgoing connection under the calling SDK connection.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = RemoteHostsCreateParams.$NullableDeserializer.class)
record RemoteHostsCreateParams(
    /** Application-generated UUID, also usable to dispose a canceled creation. */
    @JsonProperty("hostId") String hostId,
    /** The single endpoint this connection may contact. */
    @JsonProperty("transport") RemoteHostTransport transport,
    /** Optional stable application client identity for the AHP handshake. */
    @JsonProperty("clientId") String clientId,
    /** Optional file logger owned by this SDK connection, with its filter and flush boundary. */
    @JsonProperty("diagnosticLogId") String diagnosticLogId,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteHostsCreateParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private RemoteHostTransport transport;
        private String clientId;
        private boolean $presence2;
        private String diagnosticLogId;
        private boolean $presence3;

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
         * Sets the {@code transport} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder transport(RemoteHostTransport value) {
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
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code diagnosticLogId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder diagnosticLogId(String value) {
            this.diagnosticLogId = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostsCreateParams build() {
            if (hostId == null) throw new IllegalStateException("Missing required property: hostId");
            if (transport == null) throw new IllegalStateException("Missing required property: transport");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence2 && clientId == null) $explicitNulls.add("clientId");
            if ($presence3 && diagnosticLogId == null) $explicitNulls.add("diagnosticLogId");
            return new RemoteHostsCreateParams(hostId, transport, clientId, diagnosticLogId, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("clientId") && clientId == null) values.put("clientId", null);
        if ($explicitNulls.contains("diagnosticLogId") && diagnosticLogId == null) values.put("diagnosticLogId", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<RemoteHostsCreateParams> {
        @Override
        public RemoteHostsCreateParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(RemoteHostsCreateParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("clientId") && tree.get("clientId").isNull()) $explicitNulls.add("clientId");
            if (tree.has("diagnosticLogId") && tree.get("diagnosticLogId").isNull()) $explicitNulls.add("diagnosticLogId");
            return new RemoteHostsCreateParams(readProperty(context, tree, "hostId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "transport", new com.fasterxml.jackson.core.type.TypeReference<RemoteHostTransport>() {}), readProperty(context, tree, "clientId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "diagnosticLogId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

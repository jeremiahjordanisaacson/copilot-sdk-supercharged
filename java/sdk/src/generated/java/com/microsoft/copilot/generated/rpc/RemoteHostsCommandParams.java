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
 * Execute a closed native operation on an owned remote host.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = RemoteHostsCommandParams.$NullableDeserializer.class)
record RemoteHostsCommandParams(
    /** Owned outgoing connection UUID. */
    @JsonProperty("hostId") String hostId,
    /** AHP session or chat URI; absent only for connection-wide commands. */
    @JsonProperty("sessionUri") String sessionUri,
    /** Native engine command; arbitrary protocol methods are not accepted. */
    @JsonProperty("command") RemoteHostCommand command,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteHostsCommandParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String sessionUri;
        private boolean $presence1;
        private RemoteHostCommand command;

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
         * Sets the {@code sessionUri} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionUri(String value) {
            this.sessionUri = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code command} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder command(RemoteHostCommand value) {
            this.command = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostsCommandParams build() {
            if (hostId == null) throw new IllegalStateException("Missing required property: hostId");
            if (command == null) throw new IllegalStateException("Missing required property: command");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && sessionUri == null) $explicitNulls.add("sessionUri");
            return new RemoteHostsCommandParams(hostId, sessionUri, command, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("sessionUri") && sessionUri == null) values.put("sessionUri", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<RemoteHostsCommandParams> {
        @Override
        public RemoteHostsCommandParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(RemoteHostsCommandParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("sessionUri") && tree.get("sessionUri").isNull()) $explicitNulls.add("sessionUri");
            return new RemoteHostsCommandParams(readProperty(context, tree, "hostId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sessionUri", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "command", new com.fasterxml.jackson.core.type.TypeReference<RemoteHostCommand>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * ACP transport configuration for an existing in-process SDK engine.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = AcpServeParams.$NullableDeserializer.class)
record AcpServeParams(
    /** The SDK engine that owns the calling connection. */
    @JsonProperty("serverId") Long serverId,
    /** Whether to serve the process's standard input and output. */
    @JsonProperty("stdio") Boolean stdio,
    /** The TCP port, required unless stdio is selected. */
    @JsonProperty("port") Long port,
    /** The TCP interface, or absent for loopback. */
    @JsonProperty("host") String host,
    /** The session options resolved by the embedding host. */
    @JsonProperty("bootOptions") Object bootOptions,
    /** The host application version. */
    @JsonProperty("agentVersion") String agentVersion,
    /** The executable an editor can use to authenticate. */
    @JsonProperty("loginCommand") String loginCommand,
    /** The authentication command arguments. */
    @JsonProperty("loginArgs") List<String> loginArgs,
    /** An absolute attachment directory, or absent for the platform default. */
    @JsonProperty("tempDir") String tempDir,
    /** Whether the SDK client supplies the ACP telemetry callback. */
    @JsonProperty("telemetry") Boolean telemetry,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AcpServeParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Long serverId;
        private Boolean stdio;
        private Long port;
        private String host;
        private Object bootOptions;
        private boolean $presence4;
        private String agentVersion;
        private String loginCommand;
        private List<String> loginArgs;
        private String tempDir;
        private Boolean telemetry;

        /**
         * Sets the {@code serverId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder serverId(Long value) {
            this.serverId = value;
            return this;
        }

        /**
         * Sets the {@code stdio} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder stdio(Boolean value) {
            this.stdio = value;
            return this;
        }

        /**
         * Sets the {@code port} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder port(Long value) {
            this.port = value;
            return this;
        }

        /**
         * Sets the {@code host} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder host(String value) {
            this.host = value;
            return this;
        }

        /**
         * Sets the {@code bootOptions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder bootOptions(Object value) {
            this.bootOptions = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Sets the {@code agentVersion} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agentVersion(String value) {
            this.agentVersion = value;
            return this;
        }

        /**
         * Sets the {@code loginCommand} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder loginCommand(String value) {
            this.loginCommand = value;
            return this;
        }

        /**
         * Sets the {@code loginArgs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder loginArgs(List<String> value) {
            this.loginArgs = value;
            return this;
        }

        /**
         * Sets the {@code tempDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder tempDir(String value) {
            this.tempDir = value;
            return this;
        }

        /**
         * Sets the {@code telemetry} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder telemetry(Boolean value) {
            this.telemetry = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AcpServeParams build() {
            if (serverId == null) throw new IllegalStateException("Missing required property: serverId");
            if (stdio == null) throw new IllegalStateException("Missing required property: stdio");
            if (agentVersion == null) throw new IllegalStateException("Missing required property: agentVersion");
            if (loginCommand == null) throw new IllegalStateException("Missing required property: loginCommand");
            if (loginArgs == null) throw new IllegalStateException("Missing required property: loginArgs");
            if (telemetry == null) throw new IllegalStateException("Missing required property: telemetry");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence4 && bootOptions == null) $explicitNulls.add("bootOptions");
            return new AcpServeParams(serverId, stdio, port, host, bootOptions, agentVersion, loginCommand, loginArgs, tempDir, telemetry, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("bootOptions") && bootOptions == null) values.put("bootOptions", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<AcpServeParams> {
        @Override
        public AcpServeParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(AcpServeParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("bootOptions") && tree.get("bootOptions").isNull()) $explicitNulls.add("bootOptions");
            return new AcpServeParams(readProperty(context, tree, "serverId", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "stdio", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "port", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "host", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "bootOptions", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "agentVersion", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "loginCommand", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "loginArgs", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "tempDir", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "telemetry", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

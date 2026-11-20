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
 * Why a remote listing step failed, with the HTTP details needed to rebuild the caller's typed error.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = RemoteSessionPageFailure.$NullableDeserializer.class)
record RemoteSessionPageFailure(
    /** Human-readable failure message. */
    @JsonProperty("message") String message,
    /** HTTP status code of the failed request, when there was a response. */
    @JsonProperty("statusCode") Long statusCode,
    /** Category of the failure, such as `github`. */
    @JsonProperty("errorKind") String errorKind,
    /** Parsed error response body, when there was one. */
    @JsonProperty("errorPayload") Object errorPayload,
    /** Whether the step ran out of time. */
    @JsonProperty("timedOut") Boolean timedOut,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteSessionPageFailure { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String message;
        private Long statusCode;
        private String errorKind;
        private Object errorPayload;
        private boolean $presence3;
        private Boolean timedOut;

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
         * Sets the {@code statusCode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder statusCode(Long value) {
            this.statusCode = value;
            return this;
        }

        /**
         * Sets the {@code errorKind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder errorKind(String value) {
            this.errorKind = value;
            return this;
        }

        /**
         * Sets the {@code errorPayload} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder errorPayload(Object value) {
            this.errorPayload = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code timedOut} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder timedOut(Boolean value) {
            this.timedOut = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteSessionPageFailure build() {
            if (message == null) throw new IllegalStateException("Missing required property: message");
            if (timedOut == null) throw new IllegalStateException("Missing required property: timedOut");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && errorPayload == null) $explicitNulls.add("errorPayload");
            return new RemoteSessionPageFailure(message, statusCode, errorKind, errorPayload, timedOut, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("errorPayload") && errorPayload == null) values.put("errorPayload", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<RemoteSessionPageFailure> {
        @Override
        public RemoteSessionPageFailure deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(RemoteSessionPageFailure.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("errorPayload") && tree.get("errorPayload").isNull()) $explicitNulls.add("errorPayload");
            return new RemoteSessionPageFailure(readProperty(context, tree, "message", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "statusCode", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "errorKind", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "errorPayload", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "timedOut", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * Projection of the authoritative runtime credential transaction.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = HostCredentialLifecycleResult.$NullableDeserializer.class)
record HostCredentialLifecycleResult(
    /** Opaque runtime transaction capability. */
    @JsonProperty("transactionId") String transactionId,
    /** Current registration when one exists. */
    @JsonProperty("registrationId") String registrationId,
    /** Pinned identity resolved through the original credential owner. */
    @JsonProperty("authInfo") Object authInfo,
    /** Whether resume retains the session's existing registration. */
    @JsonProperty("reusingSessionRegistration") Boolean reusingSessionRegistration,
    /** Whether the runtime transaction bound its registration. */
    @JsonProperty("committed") Boolean committed,
    /** Whether rollback restored the displaced mapping. */
    @JsonProperty("restored") Boolean restored,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    HostCredentialLifecycleResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String transactionId;
        private String registrationId;
        private boolean $presence1;
        private Object authInfo;
        private boolean $presence2;
        private Boolean reusingSessionRegistration;
        private Boolean committed;
        private Boolean restored;

        /**
         * Sets the {@code transactionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder transactionId(String value) {
            this.transactionId = value;
            return this;
        }

        /**
         * Sets the {@code registrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder registrationId(String value) {
            this.registrationId = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code authInfo} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authInfo(Object value) {
            this.authInfo = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code reusingSessionRegistration} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reusingSessionRegistration(Boolean value) {
            this.reusingSessionRegistration = value;
            return this;
        }

        /**
         * Sets the {@code committed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder committed(Boolean value) {
            this.committed = value;
            return this;
        }

        /**
         * Sets the {@code restored} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder restored(Boolean value) {
            this.restored = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HostCredentialLifecycleResult build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && registrationId == null) $explicitNulls.add("registrationId");
            if ($presence2 && authInfo == null) $explicitNulls.add("authInfo");
            return new HostCredentialLifecycleResult(transactionId, registrationId, authInfo, reusingSessionRegistration, committed, restored, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("registrationId") && registrationId == null) values.put("registrationId", null);
        if ($explicitNulls.contains("authInfo") && authInfo == null) values.put("authInfo", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<HostCredentialLifecycleResult> {
        @Override
        public HostCredentialLifecycleResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(HostCredentialLifecycleResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("registrationId") && tree.get("registrationId").isNull()) $explicitNulls.add("registrationId");
            if (tree.has("authInfo") && tree.get("authInfo").isNull()) $explicitNulls.add("authInfo");
            return new HostCredentialLifecycleResult(readProperty(context, tree, "transactionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "registrationId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "authInfo", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "reusingSessionRegistration", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "committed", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "restored", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * Identifies one plugin extension installation approval to revoke.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = PermissionsExtensionApprovalsRevokeParams.$NullableDeserializer.class)
public record PermissionsExtensionApprovalsRevokeParams(
    /** Stable extension identity of the approval to revoke. */
    @JsonProperty("extensionName") String extensionName,
    /** Opaque installation identity returned by the list or grant operation. */
    @JsonProperty("installationKey") String installationKey,
    /** Optional attached session whose configuration profile contains the approval. */
    @JsonProperty("sessionId") String sessionId,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PermissionsExtensionApprovalsRevokeParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String extensionName;
        private String installationKey;
        private String sessionId;
        private boolean $presence2;

        /**
         * Sets the {@code extensionName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder extensionName(String value) {
            this.extensionName = value;
            return this;
        }

        /**
         * Sets the {@code installationKey} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder installationKey(String value) {
            this.installationKey = value;
            return this;
        }

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PermissionsExtensionApprovalsRevokeParams build() {
            if (extensionName == null) throw new IllegalStateException("Missing required property: extensionName");
            if (installationKey == null) throw new IllegalStateException("Missing required property: installationKey");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence2 && sessionId == null) $explicitNulls.add("sessionId");
            return new PermissionsExtensionApprovalsRevokeParams(extensionName, installationKey, sessionId, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("sessionId") && sessionId == null) values.put("sessionId", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<PermissionsExtensionApprovalsRevokeParams> {
        @Override
        public PermissionsExtensionApprovalsRevokeParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(PermissionsExtensionApprovalsRevokeParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("sessionId") && tree.get("sessionId").isNull()) $explicitNulls.add("sessionId");
            return new PermissionsExtensionApprovalsRevokeParams(readProperty(context, tree, "extensionName", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "installationKey", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sessionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

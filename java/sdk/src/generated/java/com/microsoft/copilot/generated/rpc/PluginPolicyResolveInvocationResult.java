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
 * Resolved plugin policy and authoritative failure posture. Fresh successful resolutions retain the original policy layers for mutation guards; cache-only inspection never changes retained policy.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = PluginPolicyResolveInvocationResult.$NullableDeserializer.class)
record PluginPolicyResolveInvocationResult(
    /** Effective device, server, and helper settings when available. */
    @JsonProperty("settings") Object settings,
    /** Raw server policy for hosts that inspect the account layer alone. */
    @JsonProperty("serverResponse") Object serverResponse,
    /** Whether the server policy could not be resolved. */
    @JsonProperty("serverFetchFailed") Boolean serverFetchFailed,
    /** Whether no usable credential was available. */
    @JsonProperty("serverTokenUnavailable") Boolean serverTokenUnavailable,
    /** Whether required fresh confirmation failed and the caller must refuse. */
    @JsonProperty("serverForcedRefreshFailure") Boolean serverForcedRefreshFailure,
    /** Server resolution error, when present. */
    @JsonProperty("serverFetchError") String serverFetchError,
    /** Whether the helper failed. */
    @JsonProperty("policyHelperLoadFailed") Boolean policyHelperLoadFailed,
    /** Whether helper failure requires refusal. */
    @JsonProperty("policyHelperFailClosed") Boolean policyHelperFailClosed,
    /** Whether this host has a configured helper. */
    @JsonProperty("policyHelperConfigured") Boolean policyHelperConfigured,
    /** Resolution diagnostics in source order. */
    @JsonProperty("diagnostics") List<PluginPolicyDiagnostic> diagnostics,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginPolicyResolveInvocationResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object settings;
        private boolean $presence0;
        private Object serverResponse;
        private boolean $presence1;
        private Boolean serverFetchFailed;
        private Boolean serverTokenUnavailable;
        private Boolean serverForcedRefreshFailure;
        private String serverFetchError;
        private Boolean policyHelperLoadFailed;
        private Boolean policyHelperFailClosed;
        private Boolean policyHelperConfigured;
        private List<PluginPolicyDiagnostic> diagnostics;

        /**
         * Sets the {@code settings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder settings(Object value) {
            this.settings = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code serverResponse} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder serverResponse(Object value) {
            this.serverResponse = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code serverFetchFailed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder serverFetchFailed(Boolean value) {
            this.serverFetchFailed = value;
            return this;
        }

        /**
         * Sets the {@code serverTokenUnavailable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder serverTokenUnavailable(Boolean value) {
            this.serverTokenUnavailable = value;
            return this;
        }

        /**
         * Sets the {@code serverForcedRefreshFailure} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder serverForcedRefreshFailure(Boolean value) {
            this.serverForcedRefreshFailure = value;
            return this;
        }

        /**
         * Sets the {@code serverFetchError} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder serverFetchError(String value) {
            this.serverFetchError = value;
            return this;
        }

        /**
         * Sets the {@code policyHelperLoadFailed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder policyHelperLoadFailed(Boolean value) {
            this.policyHelperLoadFailed = value;
            return this;
        }

        /**
         * Sets the {@code policyHelperFailClosed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder policyHelperFailClosed(Boolean value) {
            this.policyHelperFailClosed = value;
            return this;
        }

        /**
         * Sets the {@code policyHelperConfigured} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder policyHelperConfigured(Boolean value) {
            this.policyHelperConfigured = value;
            return this;
        }

        /**
         * Sets the {@code diagnostics} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder diagnostics(List<PluginPolicyDiagnostic> value) {
            this.diagnostics = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginPolicyResolveInvocationResult build() {
            if (serverFetchFailed == null) throw new IllegalStateException("Missing required property: serverFetchFailed");
            if (serverTokenUnavailable == null) throw new IllegalStateException("Missing required property: serverTokenUnavailable");
            if (serverForcedRefreshFailure == null) throw new IllegalStateException("Missing required property: serverForcedRefreshFailure");
            if (policyHelperLoadFailed == null) throw new IllegalStateException("Missing required property: policyHelperLoadFailed");
            if (policyHelperFailClosed == null) throw new IllegalStateException("Missing required property: policyHelperFailClosed");
            if (policyHelperConfigured == null) throw new IllegalStateException("Missing required property: policyHelperConfigured");
            if (diagnostics == null) throw new IllegalStateException("Missing required property: diagnostics");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && settings == null) $explicitNulls.add("settings");
            if ($presence1 && serverResponse == null) $explicitNulls.add("serverResponse");
            return new PluginPolicyResolveInvocationResult(settings, serverResponse, serverFetchFailed, serverTokenUnavailable, serverForcedRefreshFailure, serverFetchError, policyHelperLoadFailed, policyHelperFailClosed, policyHelperConfigured, diagnostics, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("settings") && settings == null) values.put("settings", null);
        if ($explicitNulls.contains("serverResponse") && serverResponse == null) values.put("serverResponse", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<PluginPolicyResolveInvocationResult> {
        @Override
        public PluginPolicyResolveInvocationResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(PluginPolicyResolveInvocationResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("settings") && tree.get("settings").isNull()) $explicitNulls.add("settings");
            if (tree.has("serverResponse") && tree.get("serverResponse").isNull()) $explicitNulls.add("serverResponse");
            return new PluginPolicyResolveInvocationResult(readProperty(context, tree, "settings", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "serverResponse", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "serverFetchFailed", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "serverTokenUnavailable", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "serverForcedRefreshFailure", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "serverFetchError", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "policyHelperLoadFailed", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "policyHelperFailClosed", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "policyHelperConfigured", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "diagnostics", new com.fasterxml.jackson.core.type.TypeReference<List<PluginPolicyDiagnostic>>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

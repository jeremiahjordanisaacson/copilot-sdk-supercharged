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
 * Known forge hosts reachable through repository remotes.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = GitRemoteHostDetection.$NullableDeserializer.class)
record GitRemoteHostDetection(
    /** Whether a remote points to Azure DevOps. */
    @JsonProperty("hasAzureDevOps") Boolean hasAzureDevOps,
    /** The Azure DevOps URL, when present. */
    @JsonProperty("azureDevOpsUrl") String azureDevOpsUrl,
    /** Whether a remote points to GitHub. */
    @JsonProperty("hasGitHub") Boolean hasGitHub,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitRemoteHostDetection { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Boolean hasAzureDevOps;
        private String azureDevOpsUrl;
        private boolean $presence1;
        private Boolean hasGitHub;

        /**
         * Sets the {@code hasAzureDevOps} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasAzureDevOps(Boolean value) {
            this.hasAzureDevOps = value;
            return this;
        }

        /**
         * Sets the {@code azureDevOpsUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder azureDevOpsUrl(String value) {
            this.azureDevOpsUrl = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code hasGitHub} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasGitHub(Boolean value) {
            this.hasGitHub = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitRemoteHostDetection build() {
            if (hasAzureDevOps == null) throw new IllegalStateException("Missing required property: hasAzureDevOps");
            if (hasGitHub == null) throw new IllegalStateException("Missing required property: hasGitHub");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && azureDevOpsUrl == null) $explicitNulls.add("azureDevOpsUrl");
            return new GitRemoteHostDetection(hasAzureDevOps, azureDevOpsUrl, hasGitHub, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("azureDevOpsUrl") && azureDevOpsUrl == null) values.put("azureDevOpsUrl", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<GitRemoteHostDetection> {
        @Override
        public GitRemoteHostDetection deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(GitRemoteHostDetection.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("azureDevOpsUrl") && tree.get("azureDevOpsUrl").isNull()) $explicitNulls.add("azureDevOpsUrl");
            return new GitRemoteHostDetection(readProperty(context, tree, "hasAzureDevOps", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "azureDevOpsUrl", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "hasGitHub", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * A worktree to create from a selected base.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = GitCreateWorktreeFromBaseParams.$NullableDeserializer.class)
record GitCreateWorktreeFromBaseParams(
    /** Absolute repository root. */
    @JsonProperty("gitRoot") String gitRoot,
    /** Requested branch name. */
    @JsonProperty("branch") String branch,
    /** Permit a numeric suffix on branch collisions. */
    @JsonProperty("allowSuffix") Boolean allowSuffix,
    /** Base ref. Omission resolves the default branch, with a HEAD fallback. */
    @JsonProperty("baseRef") String baseRef,
    /** Worktree path template. Omission uses the normal sibling layout. */
    @JsonProperty("pathTemplate") String pathTemplate,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitCreateWorktreeFromBaseParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String gitRoot;
        private String branch;
        private Boolean allowSuffix;
        private String baseRef;
        private boolean $presence3;
        private String pathTemplate;
        private boolean $presence4;

        /**
         * Sets the {@code gitRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitRoot(String value) {
            this.gitRoot = value;
            return this;
        }

        /**
         * Sets the {@code branch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder branch(String value) {
            this.branch = value;
            return this;
        }

        /**
         * Sets the {@code allowSuffix} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowSuffix(Boolean value) {
            this.allowSuffix = value;
            return this;
        }

        /**
         * Sets the {@code baseRef} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder baseRef(String value) {
            this.baseRef = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code pathTemplate} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pathTemplate(String value) {
            this.pathTemplate = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitCreateWorktreeFromBaseParams build() {
            if (gitRoot == null) throw new IllegalStateException("Missing required property: gitRoot");
            if (branch == null) throw new IllegalStateException("Missing required property: branch");
            if (allowSuffix == null) throw new IllegalStateException("Missing required property: allowSuffix");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && baseRef == null) $explicitNulls.add("baseRef");
            if ($presence4 && pathTemplate == null) $explicitNulls.add("pathTemplate");
            return new GitCreateWorktreeFromBaseParams(gitRoot, branch, allowSuffix, baseRef, pathTemplate, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("baseRef") && baseRef == null) values.put("baseRef", null);
        if ($explicitNulls.contains("pathTemplate") && pathTemplate == null) values.put("pathTemplate", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<GitCreateWorktreeFromBaseParams> {
        @Override
        public GitCreateWorktreeFromBaseParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(GitCreateWorktreeFromBaseParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("baseRef") && tree.get("baseRef").isNull()) $explicitNulls.add("baseRef");
            if (tree.has("pathTemplate") && tree.get("pathTemplate").isNull()) $explicitNulls.add("pathTemplate");
            return new GitCreateWorktreeFromBaseParams(readProperty(context, tree, "gitRoot", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "branch", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "allowSuffix", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "baseRef", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "pathTemplate", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

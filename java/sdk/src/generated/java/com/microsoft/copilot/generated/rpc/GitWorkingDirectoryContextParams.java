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
 * Directory context with optional expensive branch and base probes.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = GitWorkingDirectoryContextParams.$NullableDeserializer.class)
record GitWorkingDirectoryContextParams(
    /** Absolute working-tree directory. */
    @JsonProperty("cwd") String cwd,
    /** Include the default branch. Defaults to false. */
    @JsonProperty("includeDefaultBranch") Boolean includeDefaultBranch,
    /** Include merge-base probes. Defaults to true; false supports fast startup. */
    @JsonProperty("includeBaseCommit") Boolean includeBaseCommit,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitWorkingDirectoryContextParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String cwd;
        private Boolean includeDefaultBranch;
        private boolean $presence1;
        private Boolean includeBaseCommit;
        private boolean $presence2;

        /**
         * Sets the {@code cwd} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder cwd(String value) {
            this.cwd = value;
            return this;
        }

        /**
         * Sets the {@code includeDefaultBranch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeDefaultBranch(Boolean value) {
            this.includeDefaultBranch = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code includeBaseCommit} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeBaseCommit(Boolean value) {
            this.includeBaseCommit = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitWorkingDirectoryContextParams build() {
            if (cwd == null) throw new IllegalStateException("Missing required property: cwd");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && includeDefaultBranch == null) $explicitNulls.add("includeDefaultBranch");
            if ($presence2 && includeBaseCommit == null) $explicitNulls.add("includeBaseCommit");
            return new GitWorkingDirectoryContextParams(cwd, includeDefaultBranch, includeBaseCommit, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("includeDefaultBranch") && includeDefaultBranch == null) values.put("includeDefaultBranch", null);
        if ($explicitNulls.contains("includeBaseCommit") && includeBaseCommit == null) values.put("includeBaseCommit", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<GitWorkingDirectoryContextParams> {
        @Override
        public GitWorkingDirectoryContextParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(GitWorkingDirectoryContextParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("includeDefaultBranch") && tree.get("includeDefaultBranch").isNull()) $explicitNulls.add("includeDefaultBranch");
            if (tree.has("includeBaseCommit") && tree.get("includeBaseCommit").isNull()) $explicitNulls.add("includeBaseCommit");
            return new GitWorkingDirectoryContextParams(readProperty(context, tree, "cwd", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "includeDefaultBranch", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "includeBaseCommit", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

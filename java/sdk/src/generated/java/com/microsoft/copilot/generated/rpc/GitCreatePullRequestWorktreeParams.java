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
 * A worktree for an issue or pull request.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = GitCreatePullRequestWorktreeParams.$NullableDeserializer.class)
record GitCreatePullRequestWorktreeParams(
    /** Absolute repository root. */
    @JsonProperty("gitRoot") String gitRoot,
    /** Issue or pull request number. */
    @JsonProperty("number") Long number,
    /** Resource title used in the branch name. */
    @JsonProperty("title") String title,
    /** Remote to fetch. Omission uses the normal remote. */
    @JsonProperty("remote") String remote,
    /** Worktree path template. */
    @JsonProperty("pathTemplate") String pathTemplate,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitCreatePullRequestWorktreeParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Long number;
        private String title;
        private String remote;
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
         * Sets the {@code number} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder number(Long value) {
            this.number = value;
            return this;
        }

        /**
         * Sets the {@code title} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder title(String value) {
            this.title = value;
            return this;
        }

        /**
         * Sets the {@code remote} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder remote(String value) {
            this.remote = value;
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
        public GitCreatePullRequestWorktreeParams build() {
            if (gitRoot == null) throw new IllegalStateException("Missing required property: gitRoot");
            if (number == null) throw new IllegalStateException("Missing required property: number");
            if (title == null) throw new IllegalStateException("Missing required property: title");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && remote == null) $explicitNulls.add("remote");
            if ($presence4 && pathTemplate == null) $explicitNulls.add("pathTemplate");
            return new GitCreatePullRequestWorktreeParams(gitRoot, number, title, remote, pathTemplate, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("remote") && remote == null) values.put("remote", null);
        if ($explicitNulls.contains("pathTemplate") && pathTemplate == null) values.put("pathTemplate", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<GitCreatePullRequestWorktreeParams> {
        @Override
        public GitCreatePullRequestWorktreeParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(GitCreatePullRequestWorktreeParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("remote") && tree.get("remote").isNull()) $explicitNulls.add("remote");
            if (tree.has("pathTemplate") && tree.get("pathTemplate").isNull()) $explicitNulls.add("pathTemplate");
            return new GitCreatePullRequestWorktreeParams(readProperty(context, tree, "gitRoot", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "number", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "title", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "remote", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "pathTemplate", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

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
 * A completed move or an actionable recovery message.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = GitMoveWorktreeResult.$NullableDeserializer.class)
record GitMoveWorktreeResult(
    /** Successful move, when present. */
    @JsonProperty("moved") GitMovedWorktree moved,
    /** Recovery instructions when the move failed. */
    @JsonProperty("failureMessage") String failureMessage,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitMoveWorktreeResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private GitMovedWorktree moved;
        private boolean $presence0;
        private String failureMessage;
        private boolean $presence1;

        /**
         * Sets the {@code moved} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder moved(GitMovedWorktree value) {
            this.moved = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code failureMessage} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder failureMessage(String value) {
            this.failureMessage = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitMoveWorktreeResult build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && moved == null) $explicitNulls.add("moved");
            if ($presence1 && failureMessage == null) $explicitNulls.add("failureMessage");
            return new GitMoveWorktreeResult(moved, failureMessage, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("moved") && moved == null) values.put("moved", null);
        if ($explicitNulls.contains("failureMessage") && failureMessage == null) values.put("failureMessage", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<GitMoveWorktreeResult> {
        @Override
        public GitMoveWorktreeResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(GitMoveWorktreeResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("moved") && tree.get("moved").isNull()) $explicitNulls.add("moved");
            if (tree.has("failureMessage") && tree.get("failureMessage").isNull()) $explicitNulls.add("failureMessage");
            return new GitMoveWorktreeResult(readProperty(context, tree, "moved", new com.fasterxml.jackson.core.type.TypeReference<GitMovedWorktree>() {}), readProperty(context, tree, "failureMessage", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

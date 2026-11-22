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
 * Transient controls owned by the original embedding host.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionHostUpdateControlsParams.$NullableDeserializer.class)
record SessionHostUpdateControlsParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Whether the original host currently shows this session's shell password prompts. */
    @JsonProperty("shellAuthenticationPromptsShown") Boolean shellAuthenticationPromptsShown,
    /** Whether the original host supplies the plan-approval callback. */
    @JsonProperty("exitPlanModeCallbackEnabled") Boolean exitPlanModeCallbackEnabled,
    /** Whether managed-policy reconciliation pauses queued prompt processing. */
    @JsonProperty("managedPolicyQueueDrainPaused") Boolean managedPolicyQueueDrainPaused,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionHostUpdateControlsParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String sessionId;
        private Boolean shellAuthenticationPromptsShown;
        private boolean $presence1;
        private Boolean exitPlanModeCallbackEnabled;
        private boolean $presence2;
        private Boolean managedPolicyQueueDrainPaused;
        private boolean $presence3;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code shellAuthenticationPromptsShown} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder shellAuthenticationPromptsShown(Boolean value) {
            this.shellAuthenticationPromptsShown = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code exitPlanModeCallbackEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder exitPlanModeCallbackEnabled(Boolean value) {
            this.exitPlanModeCallbackEnabled = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code managedPolicyQueueDrainPaused} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder managedPolicyQueueDrainPaused(Boolean value) {
            this.managedPolicyQueueDrainPaused = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionHostUpdateControlsParams build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && shellAuthenticationPromptsShown == null) $explicitNulls.add("shellAuthenticationPromptsShown");
            if ($presence2 && exitPlanModeCallbackEnabled == null) $explicitNulls.add("exitPlanModeCallbackEnabled");
            if ($presence3 && managedPolicyQueueDrainPaused == null) $explicitNulls.add("managedPolicyQueueDrainPaused");
            return new SessionHostUpdateControlsParams(sessionId, shellAuthenticationPromptsShown, exitPlanModeCallbackEnabled, managedPolicyQueueDrainPaused, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("shellAuthenticationPromptsShown") && shellAuthenticationPromptsShown == null) values.put("shellAuthenticationPromptsShown", null);
        if ($explicitNulls.contains("exitPlanModeCallbackEnabled") && exitPlanModeCallbackEnabled == null) values.put("exitPlanModeCallbackEnabled", null);
        if ($explicitNulls.contains("managedPolicyQueueDrainPaused") && managedPolicyQueueDrainPaused == null) values.put("managedPolicyQueueDrainPaused", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionHostUpdateControlsParams> {
        @Override
        public SessionHostUpdateControlsParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionHostUpdateControlsParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("shellAuthenticationPromptsShown") && tree.get("shellAuthenticationPromptsShown").isNull()) $explicitNulls.add("shellAuthenticationPromptsShown");
            if (tree.has("exitPlanModeCallbackEnabled") && tree.get("exitPlanModeCallbackEnabled").isNull()) $explicitNulls.add("exitPlanModeCallbackEnabled");
            if (tree.has("managedPolicyQueueDrainPaused") && tree.get("managedPolicyQueueDrainPaused").isNull()) $explicitNulls.add("managedPolicyQueueDrainPaused");
            return new SessionHostUpdateControlsParams(readProperty(context, tree, "sessionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "shellAuthenticationPromptsShown", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "exitPlanModeCallbackEnabled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "managedPolicyQueueDrainPaused", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

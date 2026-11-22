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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Current transient prompts and native service state for an attached conversation view.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionUiSnapshotResult.$NullableDeserializer.class)
public record SessionUiSnapshotResult(
    /** Startup prompts from the runtime-owned hook configuration. */
    @JsonProperty("startupPrompts") List<String> startupPrompts,
    /** Whether the current hook generation has repository hooks left to load. */
    @JsonProperty("hasPendingRepoHooks") Boolean hasPendingRepoHooks,
    /** Unanswered user-input requests, including their runtime request IDs. */
    @JsonProperty("pendingUserInputRequests") List<UIPendingUserInput> pendingUserInputRequests,
    /** Unanswered elicitation requests, including their source and runtime request IDs. */
    @JsonProperty("pendingElicitationRequests") List<UIPendingElicitation> pendingElicitationRequests,
    /** Unanswered plan-approval requests, including their runtime request IDs. */
    @JsonProperty("pendingExitPlanModeRequests") List<UIPendingPlanApproval> pendingExitPlanModeRequests,
    /** Long-lived services and their current status, progress, and display logs. */
    @JsonProperty("serviceTasks") List<UIServiceTask> serviceTasks,
    /** Number of services that have not completed initialization. */
    @JsonProperty("initializingServiceCount") Long initializingServiceCount,
    /** Background agents managed by the session's sidekick registry. */
    @JsonProperty("sidekickBackgroundTasks") List<UIBackgroundAgentTask> sidekickBackgroundTasks,
    /** Whether the main agent turn is active. */
    @JsonProperty("agentTurnActive") Boolean agentTurnActive,
    /** Whether the session currently processes queued or active messages. */
    @JsonProperty("processing") Boolean processing,
    /** Whether this session enables experimental CLI behavior. */
    @JsonProperty("isExperimentalMode") Boolean isExperimentalMode,
    /** Whether the session's current telemetry pipeline accepts events. */
    @JsonProperty("isSessionTelemetryEnabled") Boolean isSessionTelemetryEnabled,
    /** Whether a turn, queue reservation, resume continuation, or native send is active. */
    @JsonProperty("hasActiveResumeWork") Boolean hasActiveResumeWork,
    /** Durable event counts by type for the primary agent, excluding subagent events. */
    @JsonProperty("primaryEventCounts") Map<String, Long> primaryEventCounts,
    /** Canonical working-directory revision for this live session. */
    @JsonProperty("workingDirectoryRevision") Long workingDirectoryRevision,
    /** Whether the session supports interactive MCP Apps. */
    @JsonProperty("supportsMcpApps") Boolean supportsMcpApps,
    /** Current effective plugin activation, or null before activation resolves. */
    @JsonProperty("pluginActivationSnapshot") UIPluginActivationSnapshot pluginActivationSnapshot,
    /** Runtime-owned plugin discovery and trust policy, or null when absent. */
    @JsonProperty("pluginActivationPolicy") UIPluginActivationPolicy pluginActivationPolicy,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUiSnapshotResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private List<String> startupPrompts;
        private Boolean hasPendingRepoHooks;
        private List<UIPendingUserInput> pendingUserInputRequests;
        private List<UIPendingElicitation> pendingElicitationRequests;
        private List<UIPendingPlanApproval> pendingExitPlanModeRequests;
        private List<UIServiceTask> serviceTasks;
        private Long initializingServiceCount;
        private List<UIBackgroundAgentTask> sidekickBackgroundTasks;
        private Boolean agentTurnActive;
        private Boolean processing;
        private Boolean isExperimentalMode;
        private Boolean isSessionTelemetryEnabled;
        private Boolean hasActiveResumeWork;
        private Map<String, Long> primaryEventCounts;
        private Long workingDirectoryRevision;
        private Boolean supportsMcpApps;
        private UIPluginActivationSnapshot pluginActivationSnapshot;
        private boolean $presence16;
        private UIPluginActivationPolicy pluginActivationPolicy;
        private boolean $presence17;

        /**
         * Sets the {@code startupPrompts} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder startupPrompts(List<String> value) {
            this.startupPrompts = value;
            return this;
        }

        /**
         * Sets the {@code hasPendingRepoHooks} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasPendingRepoHooks(Boolean value) {
            this.hasPendingRepoHooks = value;
            return this;
        }

        /**
         * Sets the {@code pendingUserInputRequests} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pendingUserInputRequests(List<UIPendingUserInput> value) {
            this.pendingUserInputRequests = value;
            return this;
        }

        /**
         * Sets the {@code pendingElicitationRequests} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pendingElicitationRequests(List<UIPendingElicitation> value) {
            this.pendingElicitationRequests = value;
            return this;
        }

        /**
         * Sets the {@code pendingExitPlanModeRequests} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pendingExitPlanModeRequests(List<UIPendingPlanApproval> value) {
            this.pendingExitPlanModeRequests = value;
            return this;
        }

        /**
         * Sets the {@code serviceTasks} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder serviceTasks(List<UIServiceTask> value) {
            this.serviceTasks = value;
            return this;
        }

        /**
         * Sets the {@code initializingServiceCount} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder initializingServiceCount(Long value) {
            this.initializingServiceCount = value;
            return this;
        }

        /**
         * Sets the {@code sidekickBackgroundTasks} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sidekickBackgroundTasks(List<UIBackgroundAgentTask> value) {
            this.sidekickBackgroundTasks = value;
            return this;
        }

        /**
         * Sets the {@code agentTurnActive} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agentTurnActive(Boolean value) {
            this.agentTurnActive = value;
            return this;
        }

        /**
         * Sets the {@code processing} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder processing(Boolean value) {
            this.processing = value;
            return this;
        }

        /**
         * Sets the {@code isExperimentalMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder isExperimentalMode(Boolean value) {
            this.isExperimentalMode = value;
            return this;
        }

        /**
         * Sets the {@code isSessionTelemetryEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder isSessionTelemetryEnabled(Boolean value) {
            this.isSessionTelemetryEnabled = value;
            return this;
        }

        /**
         * Sets the {@code hasActiveResumeWork} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasActiveResumeWork(Boolean value) {
            this.hasActiveResumeWork = value;
            return this;
        }

        /**
         * Sets the {@code primaryEventCounts} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder primaryEventCounts(Map<String, Long> value) {
            this.primaryEventCounts = value;
            return this;
        }

        /**
         * Sets the {@code workingDirectoryRevision} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectoryRevision(Long value) {
            this.workingDirectoryRevision = value;
            return this;
        }

        /**
         * Sets the {@code supportsMcpApps} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder supportsMcpApps(Boolean value) {
            this.supportsMcpApps = value;
            return this;
        }

        /**
         * Sets the {@code pluginActivationSnapshot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pluginActivationSnapshot(UIPluginActivationSnapshot value) {
            this.pluginActivationSnapshot = value;
            this.$presence16 = true;
            return this;
        }

        /**
         * Sets the {@code pluginActivationPolicy} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pluginActivationPolicy(UIPluginActivationPolicy value) {
            this.pluginActivationPolicy = value;
            this.$presence17 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUiSnapshotResult build() {
            if (startupPrompts == null) throw new IllegalStateException("Missing required property: startupPrompts");
            if (pendingUserInputRequests == null) throw new IllegalStateException("Missing required property: pendingUserInputRequests");
            if (pendingElicitationRequests == null) throw new IllegalStateException("Missing required property: pendingElicitationRequests");
            if (pendingExitPlanModeRequests == null) throw new IllegalStateException("Missing required property: pendingExitPlanModeRequests");
            if (serviceTasks == null) throw new IllegalStateException("Missing required property: serviceTasks");
            if (initializingServiceCount == null) throw new IllegalStateException("Missing required property: initializingServiceCount");
            if (sidekickBackgroundTasks == null) throw new IllegalStateException("Missing required property: sidekickBackgroundTasks");
            if (agentTurnActive == null) throw new IllegalStateException("Missing required property: agentTurnActive");
            if (processing == null) throw new IllegalStateException("Missing required property: processing");
            if (supportsMcpApps == null) throw new IllegalStateException("Missing required property: supportsMcpApps");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence16 && pluginActivationSnapshot == null) $explicitNulls.add("pluginActivationSnapshot");
            if ($presence17 && pluginActivationPolicy == null) $explicitNulls.add("pluginActivationPolicy");
            return new SessionUiSnapshotResult(startupPrompts, hasPendingRepoHooks, pendingUserInputRequests, pendingElicitationRequests, pendingExitPlanModeRequests, serviceTasks, initializingServiceCount, sidekickBackgroundTasks, agentTurnActive, processing, isExperimentalMode, isSessionTelemetryEnabled, hasActiveResumeWork, primaryEventCounts, workingDirectoryRevision, supportsMcpApps, pluginActivationSnapshot, pluginActivationPolicy, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("pluginActivationSnapshot") && pluginActivationSnapshot == null) values.put("pluginActivationSnapshot", null);
        if ($explicitNulls.contains("pluginActivationPolicy") && pluginActivationPolicy == null) values.put("pluginActivationPolicy", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionUiSnapshotResult> {
        @Override
        public SessionUiSnapshotResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionUiSnapshotResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("pluginActivationSnapshot") && tree.get("pluginActivationSnapshot").isNull()) $explicitNulls.add("pluginActivationSnapshot");
            if (tree.has("pluginActivationPolicy") && tree.get("pluginActivationPolicy").isNull()) $explicitNulls.add("pluginActivationPolicy");
            return new SessionUiSnapshotResult(readProperty(context, tree, "startupPrompts", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "hasPendingRepoHooks", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "pendingUserInputRequests", new com.fasterxml.jackson.core.type.TypeReference<List<UIPendingUserInput>>() {}), readProperty(context, tree, "pendingElicitationRequests", new com.fasterxml.jackson.core.type.TypeReference<List<UIPendingElicitation>>() {}), readProperty(context, tree, "pendingExitPlanModeRequests", new com.fasterxml.jackson.core.type.TypeReference<List<UIPendingPlanApproval>>() {}), readProperty(context, tree, "serviceTasks", new com.fasterxml.jackson.core.type.TypeReference<List<UIServiceTask>>() {}), readProperty(context, tree, "initializingServiceCount", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "sidekickBackgroundTasks", new com.fasterxml.jackson.core.type.TypeReference<List<UIBackgroundAgentTask>>() {}), readProperty(context, tree, "agentTurnActive", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "processing", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "isExperimentalMode", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "isSessionTelemetryEnabled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "hasActiveResumeWork", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "primaryEventCounts", new com.fasterxml.jackson.core.type.TypeReference<Map<String, Long>>() {}), readProperty(context, tree, "workingDirectoryRevision", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "supportsMcpApps", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "pluginActivationSnapshot", new com.fasterxml.jackson.core.type.TypeReference<UIPluginActivationSnapshot>() {}), readProperty(context, tree, "pluginActivationPolicy", new com.fasterxml.jackson.core.type.TypeReference<UIPluginActivationPolicy>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}

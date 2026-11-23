/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Usage summed across invocations of one configured agent.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UsageMetricsAgentMetric(
    /** Configured agent name with plugin namespace separators normalized from `/` to `:`, when this is a subagent */
    @JsonProperty("agentName") String agentName,
    /** Legacy invocation label. Omitted from name-aggregated metrics because task descriptions differ between invocations. */
    @JsonProperty("agentDisplayName") String agentDisplayName,
    /** Number of distinct subagent starts in this aggregate. Omitted for main and in legacy instance-keyed metrics. */
    @JsonProperty("invocationCount") Long invocationCount,
    /** Time spent in model API calls by this agent, in milliseconds */
    @JsonProperty("totalApiDurationMs") Long totalApiDurationMs,
    /** Accumulated nano-AI units cost for this agent */
    @JsonProperty("totalNanoAiu") Double totalNanoAiu,
    /** Per-model usage for this agent, keyed by model identifier */
    @JsonProperty("modelMetrics") Map<String, UsageMetricsModelMetric> modelMetrics
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UsageMetricsAgentMetric {}

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
        private String agentName;
        private String agentDisplayName;
        private Long invocationCount;
        private Long totalApiDurationMs;
        private Double totalNanoAiu;
        private Map<String, UsageMetricsModelMetric> modelMetrics;

        /**
         * Sets the {@code agentName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agentName(String value) {
            this.agentName = value;
            return this;
        }

        /**
         * Sets the {@code agentDisplayName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agentDisplayName(String value) {
            this.agentDisplayName = value;
            return this;
        }

        /**
         * Sets the {@code invocationCount} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder invocationCount(Long value) {
            this.invocationCount = value;
            return this;
        }

        /**
         * Sets the {@code totalApiDurationMs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder totalApiDurationMs(Long value) {
            this.totalApiDurationMs = value;
            return this;
        }

        /**
         * Sets the {@code totalNanoAiu} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder totalNanoAiu(Double value) {
            this.totalNanoAiu = value;
            return this;
        }

        /**
         * Sets the {@code modelMetrics} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder modelMetrics(Map<String, UsageMetricsModelMetric> value) {
            this.modelMetrics = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UsageMetricsAgentMetric build() {
            if (totalApiDurationMs == null) throw new IllegalStateException("Missing required property: totalApiDurationMs");
            if (totalNanoAiu == null) throw new IllegalStateException("Missing required property: totalNanoAiu");
            if (modelMetrics == null) throw new IllegalStateException("Missing required property: modelMetrics");
            return new UsageMetricsAgentMetric(agentName, agentDisplayName, invocationCount, totalApiDurationMs, totalNanoAiu, modelMetrics);
        }
    }
}

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
 * Existing initial interactive CLI readiness observation.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record CafeMetricsRecordCliReadyParams(
    /** Existing startup_duration_ms, without rounding or a new clock. Absent when unavailable. */
    @JsonProperty("durationMs") Double durationMs,
    /** Intent captured before the first interactive mount. */
    @JsonProperty("launchScenario") CafeMetricsLaunchScenario launchScenario
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    CafeMetricsRecordCliReadyParams {}

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
        private Double durationMs;
        private CafeMetricsLaunchScenario launchScenario;

        /**
         * Sets the {@code durationMs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder durationMs(Double value) {
            this.durationMs = value;
            return this;
        }

        /**
         * Sets the {@code launchScenario} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder launchScenario(CafeMetricsLaunchScenario value) {
            this.launchScenario = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public CafeMetricsRecordCliReadyParams build() {
            if (launchScenario == null) throw new IllegalStateException("Missing required property: launchScenario");
            return new CafeMetricsRecordCliReadyParams(durationMs, launchScenario);
        }
    }
}

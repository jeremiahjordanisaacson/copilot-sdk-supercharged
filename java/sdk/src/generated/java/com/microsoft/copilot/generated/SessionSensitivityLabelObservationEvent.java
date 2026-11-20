/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Session event "session.sensitivity_label_observation". A provider-reported sensitivity label observation for one model request. Each observation describes only its own request; it neither sets nor clears a session-level label.
 *
 * @apiNote This event type is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionSensitivityLabelObservationEvent extends SessionEvent {

    @Override
    public String getType() { return "session.sensitivity_label_observation"; }

    @JsonProperty("data")
    private SessionSensitivityLabelObservationEventData data;

    public SessionSensitivityLabelObservationEventData getData() { return data; }
    public void setData(SessionSensitivityLabelObservationEventData data) { this.data = data; }

    /** Data payload for {@link SessionSensitivityLabelObservationEvent}. */
    @CopilotExperimental
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SessionSensitivityLabelObservationEventData(
        /** Provider that served the model request. */
        @JsonProperty("provider") ModelProviderRef provider,
        /** Agent-loop iteration within the interaction that issued the model request. */
        @JsonProperty("turnId") String turnId,
        /** Identifier of the user interaction that owns the model request, matching assistant.turn_start.interactionId when available. */
        @JsonProperty("interactionId") String interactionId,
        /** Provider request identifier of the response that carried the observation, when known. */
        @JsonProperty("requestId") String requestId,
        /** How the provider resolved the label for this request. */
        @JsonProperty("outcome") SensitivityLabelOutcome outcome,
        /** The inherited label. Present only when `outcome` is `inherited`. */
        @JsonProperty("label") SensitivityLabel label,
        /** Why the label could not be determined. Present only when `outcome` is `indeterminate` and the provider described the failure. */
        @JsonProperty("failure") SensitivityLabelFailure failure
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public SessionSensitivityLabelObservationEventData {}

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
            private ModelProviderRef provider;
            private String turnId;
            private String interactionId;
            private String requestId;
            private SensitivityLabelOutcome outcome;
            private SensitivityLabel label;
            private SensitivityLabelFailure failure;

            /**
             * Sets the {@code provider} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder provider(ModelProviderRef value) {
                this.provider = value;
                return this;
            }

            /**
             * Sets the {@code turnId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder turnId(String value) {
                this.turnId = value;
                return this;
            }

            /**
             * Sets the {@code interactionId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder interactionId(String value) {
                this.interactionId = value;
                return this;
            }

            /**
             * Sets the {@code requestId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder requestId(String value) {
                this.requestId = value;
                return this;
            }

            /**
             * Sets the {@code outcome} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder outcome(SensitivityLabelOutcome value) {
                this.outcome = value;
                return this;
            }

            /**
             * Sets the {@code label} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder label(SensitivityLabel value) {
                this.label = value;
                return this;
            }

            /**
             * Sets the {@code failure} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder failure(SensitivityLabelFailure value) {
                this.failure = value;
                return this;
            }

            /**
             * Builds a record after checking required property presence.
             * @return a new record
             * @throws IllegalStateException if a required property is missing
             * @since 2.0.0
             */
            public SessionSensitivityLabelObservationEventData build() {
                if (provider == null) throw new IllegalStateException("Missing required property: provider");
                if (turnId == null) throw new IllegalStateException("Missing required property: turnId");
                if (outcome == null) throw new IllegalStateException("Missing required property: outcome");
                return new SessionSensitivityLabelObservationEventData(provider, turnId, interactionId, requestId, outcome, label, failure);
            }
        }
    }
}

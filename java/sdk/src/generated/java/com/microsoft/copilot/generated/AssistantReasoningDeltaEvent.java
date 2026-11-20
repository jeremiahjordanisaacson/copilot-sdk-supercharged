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
 * Session event "assistant.reasoning_delta". Streaming reasoning delta for incremental extended thinking updates
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class AssistantReasoningDeltaEvent extends SessionEvent {

    @Override
    public String getType() { return "assistant.reasoning_delta"; }

    @JsonProperty("data")
    private AssistantReasoningDeltaEventData data;

    public AssistantReasoningDeltaEventData getData() { return data; }
    public void setData(AssistantReasoningDeltaEventData data) { this.data = data; }

    /** Data payload for {@link AssistantReasoningDeltaEvent}. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record AssistantReasoningDeltaEventData(
        /** Reasoning block ID this delta belongs to, matching the corresponding assistant.reasoning event */
        @JsonProperty("reasoningId") String reasoningId,
        /** Incremental text chunk to append to the reasoning content */
        @JsonProperty("deltaContent") String deltaContent,
        /** Experimental HydraFusion phase attribution. Present without a `commitId` while a staged root phase streams provisional output that the orchestrator may still discard. */
        @CopilotExperimental
        @JsonProperty("fusion") FusionAttribution fusion
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public AssistantReasoningDeltaEventData {}

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
            private String reasoningId;
            private String deltaContent;
            private FusionAttribution fusion;

            /**
             * Sets the {@code reasoningId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder reasoningId(String value) {
                this.reasoningId = value;
                return this;
            }

            /**
             * Sets the {@code deltaContent} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder deltaContent(String value) {
                this.deltaContent = value;
                return this;
            }

            /**
             * Sets the {@code fusion} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            @com.microsoft.copilot.CopilotExperimental
            public Builder fusion(FusionAttribution value) {
                this.fusion = value;
                return this;
            }

            /**
             * Builds a record after checking required property presence.
             * @return a new record
             * @throws IllegalStateException if a required property is missing
             * @since 2.0.0
             */
            public AssistantReasoningDeltaEventData build() {
                if (reasoningId == null) throw new IllegalStateException("Missing required property: reasoningId");
                if (deltaContent == null) throw new IllegalStateException("Missing required property: deltaContent");
                return new AssistantReasoningDeltaEventData(reasoningId, deltaContent, fusion);
            }
        }
    }
}

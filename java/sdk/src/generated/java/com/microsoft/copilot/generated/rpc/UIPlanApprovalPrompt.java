/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * A plan that awaits approval.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPlanApprovalPrompt(
    /** Runtime identity of this request. */
    @JsonProperty("requestId") String requestId,
    /** Plan summary. */
    @JsonProperty("summary") String summary,
    /** Full plan content. */
    @JsonProperty("planContent") String planContent,
    /** Available actions. */
    @JsonProperty("actions") List<UIExitPlanModeAction> actions,
    /** Action selected by default. */
    @JsonProperty("recommendedAction") UIExitPlanModeAction recommendedAction,
    /** Model that authored the plan. */
    @JsonProperty("model") String model
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPlanApprovalPrompt {}

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
        private String requestId;
        private String summary;
        private String planContent;
        private List<UIExitPlanModeAction> actions;
        private UIExitPlanModeAction recommendedAction;
        private String model;

        /**
         * Sets the {@code requestId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder requestId(String value) {
            this.requestId = value;
            return this;
        }

        /**
         * Sets the {@code summary} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder summary(String value) {
            this.summary = value;
            return this;
        }

        /**
         * Sets the {@code planContent} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder planContent(String value) {
            this.planContent = value;
            return this;
        }

        /**
         * Sets the {@code actions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder actions(List<UIExitPlanModeAction> value) {
            this.actions = value;
            return this;
        }

        /**
         * Sets the {@code recommendedAction} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder recommendedAction(UIExitPlanModeAction value) {
            this.recommendedAction = value;
            return this;
        }

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder model(String value) {
            this.model = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPlanApprovalPrompt build() {
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (summary == null) throw new IllegalStateException("Missing required property: summary");
            if (planContent == null) throw new IllegalStateException("Missing required property: planContent");
            if (actions == null) throw new IllegalStateException("Missing required property: actions");
            if (recommendedAction == null) throw new IllegalStateException("Missing required property: recommendedAction");
            return new UIPlanApprovalPrompt(requestId, summary, planContent, actions, recommendedAction, model);
        }
    }
}

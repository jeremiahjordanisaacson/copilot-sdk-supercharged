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
 * Structured reason that the task cannot continue without intervention
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record TaskBlocker(
    /** Category of intervention that blocked the task */
    @JsonProperty("kind") TaskBlockerKind kind,
    /** Controlled reason for the current blocked state */
    @JsonProperty("reason") TaskBlockerReason reason,
    /** Whether a later user response or steering message can resume the task */
    @JsonProperty("resumable") Boolean resumable,
    /** Permission-recovery episode that produced this blocker */
    @JsonProperty("permissionRecovery") PermissionRecoveryData permissionRecovery
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public TaskBlocker {}

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
        private TaskBlockerKind kind;
        private TaskBlockerReason reason;
        private Boolean resumable;
        private PermissionRecoveryData permissionRecovery;

        /**
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder kind(TaskBlockerKind value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder reason(TaskBlockerReason value) {
            this.reason = value;
            return this;
        }

        /**
         * Sets the {@code resumable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder resumable(Boolean value) {
            this.resumable = value;
            return this;
        }

        /**
         * Sets the {@code permissionRecovery} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder permissionRecovery(PermissionRecoveryData value) {
            this.permissionRecovery = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public TaskBlocker build() {
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            if (reason == null) throw new IllegalStateException("Missing required property: reason");
            if (resumable == null) throw new IllegalStateException("Missing required property: resumable");
            if (permissionRecovery == null) throw new IllegalStateException("Missing required property: permissionRecovery");
            return new TaskBlocker(kind, reason, resumable, permissionRecovery);
        }
    }
}

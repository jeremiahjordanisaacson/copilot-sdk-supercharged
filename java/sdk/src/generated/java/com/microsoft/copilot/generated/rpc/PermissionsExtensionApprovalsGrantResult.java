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
 * Whether the pending request was eligible and persisted.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record PermissionsExtensionApprovalsGrantResult(
    /** Whether the pending request was eligible and its approval was persisted. */
    @JsonProperty("granted") Boolean granted,
    /** The persisted approval when `granted` is true. */
    @JsonProperty("approval") PermissionsExtensionApproval approval
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PermissionsExtensionApprovalsGrantResult {}

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
        private Boolean granted;
        private PermissionsExtensionApproval approval;

        /**
         * Sets the {@code granted} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder granted(Boolean value) {
            this.granted = value;
            return this;
        }

        /**
         * Sets the {@code approval} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder approval(PermissionsExtensionApproval value) {
            this.approval = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PermissionsExtensionApprovalsGrantResult build() {
            if (granted == null) throw new IllegalStateException("Missing required property: granted");
            return new PermissionsExtensionApprovalsGrantResult(granted, approval);
        }
    }
}

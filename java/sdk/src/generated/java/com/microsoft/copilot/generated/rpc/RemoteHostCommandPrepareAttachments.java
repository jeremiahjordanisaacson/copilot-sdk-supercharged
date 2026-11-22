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
 * Prepare local controller attachments before they cross the remote boundary.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandPrepareAttachments extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "prepareAttachments";

    @Override
    public String getType() { return type; }

    /** Existing SDK attachment descriptions. */
    @JsonProperty("attachments")
    private List<Object> attachments;

    public List<Object> getAttachments() { return attachments; }
    public void setAttachments(List<Object> attachments) { this.attachments = attachments; }


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
        private List<Object> attachments;

        /**
         * Sets the {@code attachments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder attachments(List<Object> value) {
            this.attachments = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandPrepareAttachments build() {
            if (attachments == null) throw new IllegalStateException("Missing required property: attachments");
            RemoteHostCommandPrepareAttachments instance = new RemoteHostCommandPrepareAttachments();
            instance.setAttachments(attachments);
            return instance;
        }
    }
}

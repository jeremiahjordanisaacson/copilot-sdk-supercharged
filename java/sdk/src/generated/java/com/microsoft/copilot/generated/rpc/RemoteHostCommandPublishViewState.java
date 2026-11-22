/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * Publish shared terminal view state.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandPublishViewState extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "publishViewState";

    @Override
    public String getType() { return type; }

    /** Application view projection. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("view")
    private Object view;

    public Object getView() { return view; }
    public void setView(Object view) { this.view = view; }


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
        private Object view;
        private boolean $presence0;

        /**
         * Sets the {@code view} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder view(Object value) {
            this.view = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandPublishViewState build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: view");
            RemoteHostCommandPublishViewState instance = new RemoteHostCommandPublishViewState();
            instance.setView(view);
            return instance;
        }
    }
}

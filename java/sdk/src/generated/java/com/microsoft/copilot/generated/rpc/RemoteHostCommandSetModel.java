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
 * Select an AHP model.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandSetModel extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "setModel";

    @Override
    public String getType() { return type; }

    /** Host-advertised model selection. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("model")
    private Object model;

    public Object getModel() { return model; }
    public void setModel(Object model) { this.model = model; }


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
        private Object model;
        private boolean $presence0;

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder model(Object value) {
            this.model = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandSetModel build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: model");
            RemoteHostCommandSetModel instance = new RemoteHostCommandSetModel();
            instance.setModel(model);
            return instance;
        }
    }
}

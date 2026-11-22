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
 * The delegate completed the spawn request.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class EmbedderAgentRegistrySpawnResultResult extends EmbedderAgentRegistrySpawnResult {

    @JsonProperty("kind")
    private final String kind = "result";

    @Override
    public String getKind() { return kind; }

    /** Canonical process-launch result. */
    @JsonProperty("result")
    private Object result;

    public Object getResult() { return result; }
    public void setResult(Object result) { this.result = result; }


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
        private Object result;

        /**
         * Sets the {@code result} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder result(Object value) {
            this.result = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public EmbedderAgentRegistrySpawnResultResult build() {
            if (result == null) throw new IllegalStateException("Missing required property: result");
            EmbedderAgentRegistrySpawnResultResult instance = new EmbedderAgentRegistrySpawnResultResult();
            instance.setResult(result);
            return instance;
        }
    }
}

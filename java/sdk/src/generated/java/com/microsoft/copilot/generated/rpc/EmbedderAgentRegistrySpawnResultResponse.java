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
 * The delegate rejected the request with a structured protocol error.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class EmbedderAgentRegistrySpawnResultResponse extends EmbedderAgentRegistrySpawnResult {

    @JsonProperty("kind")
    private final String kind = "response";

    @Override
    public String getKind() { return kind; }

    /** Original JSON-RPC error code. */
    @JsonProperty("code")
    private Long code;

    /** Original error message. */
    @JsonProperty("message")
    private String message;

    /** Original structured error data. */
    @JsonProperty("data")
    private Object data;

    public Long getCode() { return code; }
    public void setCode(Long code) { this.code = code; }

    public String getMessage() { return message; }
    public void setMessage(String message) { this.message = message; }

    public Object getData() { return data; }
    public void setData(Object data) { this.data = data; }


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
        private Long code;
        private String message;
        private Object data;

        /**
         * Sets the {@code code} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder code(Long value) {
            this.code = value;
            return this;
        }

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Sets the {@code data} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder data(Object value) {
            this.data = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public EmbedderAgentRegistrySpawnResultResponse build() {
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            EmbedderAgentRegistrySpawnResultResponse instance = new EmbedderAgentRegistrySpawnResultResponse();
            instance.setCode(code);
            instance.setMessage(message);
            instance.setData(data);
            return instance;
        }
    }
}

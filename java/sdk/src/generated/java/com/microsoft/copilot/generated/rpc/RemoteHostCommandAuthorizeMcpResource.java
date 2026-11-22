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
 * Resolve a pending MCP resource approval through the session owner.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandAuthorizeMcpResource extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "authorizeMcpResource";

    @Override
    public String getType() { return type; }

    /** Native session identifier. */
    @JsonProperty("nativeSessionId")
    private String nativeSessionId;

    /** Pending request identifier. */
    @JsonProperty("requestId")
    private String requestId;

    /** Existing resource authorization context. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("context")
    private Object context;

    /** Protected resource URI. */
    @JsonProperty("resource")
    private String resource;

    public String getNativeSessionId() { return nativeSessionId; }
    public void setNativeSessionId(String nativeSessionId) { this.nativeSessionId = nativeSessionId; }

    public String getRequestId() { return requestId; }
    public void setRequestId(String requestId) { this.requestId = requestId; }

    public Object getContext() { return context; }
    public void setContext(Object context) { this.context = context; }

    public String getResource() { return resource; }
    public void setResource(String resource) { this.resource = resource; }


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
        private String nativeSessionId;
        private String requestId;
        private Object context;
        private boolean $presence2;
        private String resource;

        /**
         * Sets the {@code nativeSessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder nativeSessionId(String value) {
            this.nativeSessionId = value;
            return this;
        }

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
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(Object value) {
            this.context = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code resource} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder resource(String value) {
            this.resource = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandAuthorizeMcpResource build() {
            if (nativeSessionId == null) throw new IllegalStateException("Missing required property: nativeSessionId");
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (!$presence2) throw new IllegalStateException("Missing required property: context");
            if (resource == null) throw new IllegalStateException("Missing required property: resource");
            RemoteHostCommandAuthorizeMcpResource instance = new RemoteHostCommandAuthorizeMcpResource();
            instance.setNativeSessionId(nativeSessionId);
            instance.setRequestId(requestId);
            instance.setContext(context);
            instance.setResource(resource);
            return instance;
        }
    }
}

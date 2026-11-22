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
 * An unanswered elicitation request.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPendingElicitation(
    /** Runtime identity accepted by the elicitation response method. */
    @JsonProperty("requestId") String requestId,
    /** Original form or URL prompt. */
    @JsonProperty("request") UIElicitationPrompt request,
    /** MCP source of the request, when present. */
    @JsonProperty("elicitationSource") String elicitationSource,
    /** Runtime-verified display name of the source. */
    @JsonProperty("elicitationSourceDisplayName") String elicitationSourceDisplayName
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPendingElicitation {}

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
        private UIElicitationPrompt request;
        private String elicitationSource;
        private String elicitationSourceDisplayName;

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
         * Sets the {@code request} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder request(UIElicitationPrompt value) {
            this.request = value;
            return this;
        }

        /**
         * Sets the {@code elicitationSource} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder elicitationSource(String value) {
            this.elicitationSource = value;
            return this;
        }

        /**
         * Sets the {@code elicitationSourceDisplayName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder elicitationSourceDisplayName(String value) {
            this.elicitationSourceDisplayName = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPendingElicitation build() {
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (request == null) throw new IllegalStateException("Missing required property: request");
            return new UIPendingElicitation(requestId, request, elicitationSource, elicitationSourceDisplayName);
        }
    }
}

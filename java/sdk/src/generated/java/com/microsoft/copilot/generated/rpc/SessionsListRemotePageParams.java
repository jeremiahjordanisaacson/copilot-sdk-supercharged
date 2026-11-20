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
 * Requests the next step of a Mission Control task listing that follows `Link: rel="next"` pagination.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionsListRemotePageParams(
    /** Opaque continuation from the previous step's result. Omit to start a listing. Continuations are only valid in the runtime process that returned them. */
    @JsonProperty("continuation") String continuation,
    /** Most raw Mission Control records this step may accept, before deduplication or filtering. Pass the listing's remaining record budget: when a step has to drop records from a page to fit, it ends the listing and returns no continuation, because resuming would skip them. */
    @JsonProperty("maxRecords") Long maxRecords,
    /** Integration identifier to send to Mission Control, so the listing is attributed to the calling client. Defaults to the runtime's integration identifier. */
    @JsonProperty("integrationId") String integrationId,
    /** Signed-in account to list tasks for, as an account selection identifier. When set, the step fails rather than falling back to another account if this one is no longer signed in. Defaults to the runtime's current account. */
    @JsonProperty("selectionId") String selectionId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionsListRemotePageParams {}

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
        private String continuation;
        private Long maxRecords;
        private String integrationId;
        private String selectionId;

        /**
         * Sets the {@code continuation} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder continuation(String value) {
            this.continuation = value;
            return this;
        }

        /**
         * Sets the {@code maxRecords} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder maxRecords(Long value) {
            this.maxRecords = value;
            return this;
        }

        /**
         * Sets the {@code integrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder integrationId(String value) {
            this.integrationId = value;
            return this;
        }

        /**
         * Sets the {@code selectionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder selectionId(String value) {
            this.selectionId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsListRemotePageParams build() {
            if (maxRecords == null) throw new IllegalStateException("Missing required property: maxRecords");
            return new SessionsListRemotePageParams(continuation, maxRecords, integrationId, selectionId);
        }
    }
}

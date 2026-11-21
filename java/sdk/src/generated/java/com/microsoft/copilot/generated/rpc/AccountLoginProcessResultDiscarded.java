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
 * The current credential removal result.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessResultDiscarded extends AccountLoginProcessResult {

    @JsonProperty("kind")
    private final String kind = "discarded";

    @Override
    public String getKind() { return kind; }

    /** Whether a credential was removed. */
    @JsonProperty("removed")
    private Boolean removed;

    public Boolean getRemoved() { return removed; }
    public void setRemoved(Boolean removed) { this.removed = removed; }


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
        private Boolean removed;

        /**
         * Sets the {@code removed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder removed(Boolean value) {
            this.removed = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessResultDiscarded build() {
            if (removed == null) throw new IllegalStateException("Missing required property: removed");
            AccountLoginProcessResultDiscarded instance = new AccountLoginProcessResultDiscarded();
            instance.setRemoved(removed);
            return instance;
        }
    }
}

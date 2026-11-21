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
 * The advisory repository disable list. A read failure returns no names and a diagnostic, rather than blocking a listing or mutation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record RepositoryDisabledNamesResult(
    /** Repository-owned names, excluding the user contribution. */
    @JsonProperty("names") List<String> names,
    /** Debug diagnostic for a repository that could not be inspected. */
    @JsonProperty("warning") String warning
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RepositoryDisabledNamesResult {}

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
        private List<String> names;
        private String warning;

        /**
         * Sets the {@code names} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder names(List<String> value) {
            this.names = value;
            return this;
        }

        /**
         * Sets the {@code warning} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder warning(String value) {
            this.warning = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RepositoryDisabledNamesResult build() {
            if (names == null) throw new IllegalStateException("Missing required property: names");
            return new RepositoryDisabledNamesResult(names, warning);
        }
    }
}

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
 * The original login persistence result, without credentials.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ProcessLoginPersistence(
    /** The original completed, needs-plaintext-consent, needs-account-selection, or declined status. */
    @JsonProperty("status") String status,
    /** The authenticated host, when available. */
    @JsonProperty("host") String host,
    /** The authenticated login, when available. */
    @JsonProperty("login") String login,
    /** The offered accounts, when account selection is required. */
    @JsonProperty("accounts") List<ProcessLoginAccount> accounts,
    /** The runtime's original user description, when host and login are available. */
    @JsonProperty("description") String description
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ProcessLoginPersistence {}

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
        private String status;
        private String host;
        private String login;
        private List<ProcessLoginAccount> accounts;
        private String description;

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(String value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code host} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder host(String value) {
            this.host = value;
            return this;
        }

        /**
         * Sets the {@code login} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder login(String value) {
            this.login = value;
            return this;
        }

        /**
         * Sets the {@code accounts} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder accounts(List<ProcessLoginAccount> value) {
            this.accounts = value;
            return this;
        }

        /**
         * Sets the {@code description} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ProcessLoginPersistence build() {
            if (status == null) throw new IllegalStateException("Missing required property: status");
            return new ProcessLoginPersistence(status, host, login, accounts, description);
        }
    }
}

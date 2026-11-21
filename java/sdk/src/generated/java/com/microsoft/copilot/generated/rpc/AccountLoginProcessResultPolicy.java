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
 * The organization policy verdict.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessResultPolicy extends AccountLoginProcessResult {

    @JsonProperty("kind")
    private final String kind = "policy";

    @Override
    public String getKind() { return kind; }

    /** The verdict from both original policy channels. */
    @JsonProperty("decision")
    private ForceLoginOrgsDecision decision;

    /** Original policy warnings for the host's configured logger. */
    @JsonProperty("warnings")
    private List<String> warnings;

    public ForceLoginOrgsDecision getDecision() { return decision; }
    public void setDecision(ForceLoginOrgsDecision decision) { this.decision = decision; }

    public List<String> getWarnings() { return warnings; }
    public void setWarnings(List<String> warnings) { this.warnings = warnings; }


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
        private ForceLoginOrgsDecision decision;
        private List<String> warnings;

        /**
         * Sets the {@code decision} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder decision(ForceLoginOrgsDecision value) {
            this.decision = value;
            return this;
        }

        /**
         * Sets the {@code warnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder warnings(List<String> value) {
            this.warnings = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessResultPolicy build() {
            if (decision == null) throw new IllegalStateException("Missing required property: decision");
            if (warnings == null) throw new IllegalStateException("Missing required property: warnings");
            AccountLoginProcessResultPolicy instance = new AccountLoginProcessResultPolicy();
            instance.setDecision(decision);
            instance.setWarnings(warnings);
            return instance;
        }
    }
}

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
 * A selectable agent tool group.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record AgentToolCategory(
    /** The stable, closed category identifier. */
    @JsonProperty("id") AgentToolCategoryId id,
    /** The display label. */
    @JsonProperty("label") String label,
    /** Tool names in their canonical order. */
    @JsonProperty("tools") List<String> tools
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public AgentToolCategory {}

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
        private AgentToolCategoryId id;
        private String label;
        private List<String> tools;

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder id(AgentToolCategoryId value) {
            this.id = value;
            return this;
        }

        /**
         * Sets the {@code label} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder label(String value) {
            this.label = value;
            return this;
        }

        /**
         * Sets the {@code tools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder tools(List<String> value) {
            this.tools = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AgentToolCategory build() {
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (label == null) throw new IllegalStateException("Missing required property: label");
            if (tools == null) throw new IllegalStateException("Missing required property: tools");
            return new AgentToolCategory(id, label, tools);
        }
    }
}

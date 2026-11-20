/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * A sensitivity label the provider applied to one model request.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SensitivityLabel(
    /** Label identifier, a lowercase hyphenated GUID. */
    @JsonProperty("id") String id,
    /** Display name of the label. */
    @JsonProperty("name") String name,
    /** Display color of the label, when the provider supplied one. */
    @JsonProperty("color") String color,
    /** Tooltip text describing the label, when the provider supplied one. */
    @JsonProperty("tooltip") String tooltip,
    /** Whether the label encrypts content, when the provider stated it explicitly. Absent means unknown. */
    @JsonProperty("isEncrypted") Boolean isEncrypted
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SensitivityLabel {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String id;
        private String name;
        private String color;
        private String tooltip;
        private Boolean isEncrypted;

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder id(String value) {
            this.id = value;
            return this;
        }

        /**
         * Sets the {@code name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder name(String value) {
            this.name = value;
            return this;
        }

        /**
         * Sets the {@code color} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder color(String value) {
            this.color = value;
            return this;
        }

        /**
         * Sets the {@code tooltip} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder tooltip(String value) {
            this.tooltip = value;
            return this;
        }

        /**
         * Sets the {@code isEncrypted} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isEncrypted(Boolean value) {
            this.isEncrypted = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SensitivityLabel build() {
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (name == null) throw new IllegalStateException("Missing required property: name");
            return new SensitivityLabel(id, name, color, tooltip, isEncrypted);
        }
    }
}

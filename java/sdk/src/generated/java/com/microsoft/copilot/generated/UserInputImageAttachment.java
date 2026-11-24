/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * An inline image supplied in an answer to a legacy ask_user request that advertises image support.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UserInputImageAttachment(
    /** Attachment type. Always image. */
    @JsonProperty("type") String type,
    /** Nonempty image bytes encoded as strict padded standard base64, without a data URL prefix or whitespace. */
    @JsonProperty("data") String data,
    /** MIME type of the image, matching an advertised allowed MIME type. */
    @JsonProperty("mimeType") String mimeType,
    /** Optional client-facing name of the image. */
    @JsonProperty("displayName") String displayName,
    /** Optional description of the image. */
    @JsonProperty("description") String description
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UserInputImageAttachment {}

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
        private String type;
        private String data;
        private String mimeType;
        private String displayName;
        private String description;

        /**
         * Sets the {@code type} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder type(String value) {
            this.type = value;
            return this;
        }

        /**
         * Sets the {@code data} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder data(String value) {
            this.data = value;
            return this;
        }

        /**
         * Sets the {@code mimeType} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder mimeType(String value) {
            this.mimeType = value;
            return this;
        }

        /**
         * Sets the {@code displayName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder displayName(String value) {
            this.displayName = value;
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
        public UserInputImageAttachment build() {
            if (type == null) throw new IllegalStateException("Missing required property: type");
            if (data == null) throw new IllegalStateException("Missing required property: data");
            if (mimeType == null) throw new IllegalStateException("Missing required property: mimeType");
            return new UserInputImageAttachment(type, data, mimeType, displayName, description);
        }
    }
}

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
 * An inline attachment from an external conversation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record HistoryImportAttachment(
    /** Attachment format. */
    @JsonProperty("type") HistoryImportAttachmentType type,
    /** Original MIME type. */
    @JsonProperty("mimeType") String mimeType,
    /** Name shown with the attachment. */
    @JsonProperty("displayName") String displayName,
    /** Base64-encoded attachment bytes. */
    @JsonProperty("data") String data
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public HistoryImportAttachment {}

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
        private HistoryImportAttachmentType type;
        private String mimeType;
        private String displayName;
        private String data;

        /**
         * Sets the {@code type} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder type(HistoryImportAttachmentType value) {
            this.type = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HistoryImportAttachment build() {
            if (type == null) throw new IllegalStateException("Missing required property: type");
            if (mimeType == null) throw new IllegalStateException("Missing required property: mimeType");
            if (displayName == null) throw new IllegalStateException("Missing required property: displayName");
            if (data == null) throw new IllegalStateException("Missing required property: data");
            return new HistoryImportAttachment(type, mimeType, displayName, data);
        }
    }
}

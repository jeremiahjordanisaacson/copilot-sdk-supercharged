/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Runtime-advertised image support for a legacy ask_user request. Absent when the request cannot accept images.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UserInputImageAttachmentConstraints(
    /** MIME types accepted by this request. Images must have readable nonzero
dimensions; GIF and WebP images must not exceed 2,000 pixels per side. */
    @JsonProperty("allowedMimeTypes") List<String> allowedMimeTypes,
    /** Maximum number of images accepted in one answer. */
    @JsonProperty("maxCount") Long maxCount,
    /** Maximum decoded byte length of each image. */
    @JsonProperty("maxBytesPerImage") Long maxBytesPerImage
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UserInputImageAttachmentConstraints {}

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
        private List<String> allowedMimeTypes;
        private Long maxCount;
        private Long maxBytesPerImage;

        /**
         * Sets the {@code allowedMimeTypes} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowedMimeTypes(List<String> value) {
            this.allowedMimeTypes = value;
            return this;
        }

        /**
         * Sets the {@code maxCount} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder maxCount(Long value) {
            this.maxCount = value;
            return this;
        }

        /**
         * Sets the {@code maxBytesPerImage} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder maxBytesPerImage(Long value) {
            this.maxBytesPerImage = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UserInputImageAttachmentConstraints build() {
            if (allowedMimeTypes == null) throw new IllegalStateException("Missing required property: allowedMimeTypes");
            if (maxCount == null) throw new IllegalStateException("Missing required property: maxCount");
            if (maxBytesPerImage == null) throw new IllegalStateException("Missing required property: maxBytesPerImage");
            return new UserInputImageAttachmentConstraints(allowedMimeTypes, maxCount, maxBytesPerImage);
        }
    }
}

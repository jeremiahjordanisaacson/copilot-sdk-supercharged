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
 * The new file exists.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class AgentsCreateFileResultSuccess extends AgentsCreateFileResult {

    @JsonProperty("kind")
    private final String kind = "success";

    @Override
    public String getKind() { return kind; }

    /** The created file path. */
    @JsonProperty("filePath")
    private String filePath;

    public String getFilePath() { return filePath; }
    public void setFilePath(String filePath) { this.filePath = filePath; }


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
        private String filePath;

        /**
         * Sets the {@code filePath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder filePath(String value) {
            this.filePath = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AgentsCreateFileResultSuccess build() {
            if (filePath == null) throw new IllegalStateException("Missing required property: filePath");
            AgentsCreateFileResultSuccess instance = new AgentsCreateFileResultSuccess();
            instance.setFilePath(filePath);
            return instance;
        }
    }
}

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Extensions discovered for the session, with their current status.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionExtensionsReconcileResult(
    /** Discovered extensions and their current status */
    @JsonProperty("extensions") List<Extension> extensions,
    /** Whether the runtime installed an extension service, including an empty catalog. */
    @JsonProperty("configured") Boolean configured,
    /** Whether the latest explicit create or resume choice disabled extensions. */
    @JsonProperty("explicitOptOut") Boolean explicitOptOut
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionExtensionsReconcileResult {}

    /**
     * Creates a record without optional runtime metadata.
     * @param extensions the {@code extensions} property
     */
    public SessionExtensionsReconcileResult(List<Extension> extensions) {
        this(extensions, null, null);
    }

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
        private List<Extension> extensions;
        private Boolean configured;
        private Boolean explicitOptOut;

        /**
         * Sets the {@code extensions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder extensions(List<Extension> value) {
            this.extensions = value;
            return this;
        }

        /**
         * Sets the {@code configured} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configured(Boolean value) {
            this.configured = value;
            return this;
        }

        /**
         * Sets the {@code explicitOptOut} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder explicitOptOut(Boolean value) {
            this.explicitOptOut = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionExtensionsReconcileResult build() {
            if (extensions == null) throw new IllegalStateException("Missing required property: extensions");
            return new SessionExtensionsReconcileResult(extensions, configured, explicitOptOut);
        }
    }
}

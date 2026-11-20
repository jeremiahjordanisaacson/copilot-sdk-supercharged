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
import javax.annotation.processing.Generated;

/**
 * Attaches a runtime session facade to an existing AHP session.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpConnectionsAttachSessionParams(
    /** Connection returned by ahp.connections.create, owned by this caller. */
    @JsonProperty("connectionId") String connectionId,
    /** Session URI advertised by that connection's host. */
    @JsonProperty("sessionUri") String sessionUri,
    /** Serialized session options. The runtime retains its existing auth owner. */
    @JsonProperty("optionsJson") String optionsJson,
    /** Controller working directory used only by local UI services. */
    @JsonProperty("processWorkingDirectory") String processWorkingDirectory,
    /** Controller feature service configuration, without live native handles. */
    @JsonProperty("featureFlagServiceJson") String featureFlagServiceJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionsAttachSessionParams {}

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
        private String connectionId;
        private String sessionUri;
        private String optionsJson;
        private String processWorkingDirectory;
        private String featureFlagServiceJson;

        /**
         * Sets the {@code connectionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder connectionId(String value) {
            this.connectionId = value;
            return this;
        }

        /**
         * Sets the {@code sessionUri} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionUri(String value) {
            this.sessionUri = value;
            return this;
        }

        /**
         * Sets the {@code optionsJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder optionsJson(String value) {
            this.optionsJson = value;
            return this;
        }

        /**
         * Sets the {@code processWorkingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder processWorkingDirectory(String value) {
            this.processWorkingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code featureFlagServiceJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder featureFlagServiceJson(String value) {
            this.featureFlagServiceJson = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionsAttachSessionParams build() {
            if (connectionId == null) throw new IllegalStateException("Missing required property: connectionId");
            if (sessionUri == null) throw new IllegalStateException("Missing required property: sessionUri");
            if (optionsJson == null) throw new IllegalStateException("Missing required property: optionsJson");
            if (processWorkingDirectory == null) throw new IllegalStateException("Missing required property: processWorkingDirectory");
            if (featureFlagServiceJson == null) throw new IllegalStateException("Missing required property: featureFlagServiceJson");
            return new AhpConnectionsAttachSessionParams(connectionId, sessionUri, optionsJson, processWorkingDirectory, featureFlagServiceJson);
        }
    }
}

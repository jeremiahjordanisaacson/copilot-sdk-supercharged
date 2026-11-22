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
 * Connect through the authenticated account's Mission Control environment.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostTransportMissionControl extends RemoteHostTransport {

    @JsonProperty("kind")
    private final String kind = "missionControl";

    @Override
    public String getKind() { return kind; }

    /** Mission Control service endpoint. */
    @JsonProperty("baseUrl")
    private String baseUrl;

    /** Environment identifier, preserving its original case. */
    @JsonProperty("environmentId")
    private String environmentId;

    public String getBaseUrl() { return baseUrl; }
    public void setBaseUrl(String baseUrl) { this.baseUrl = baseUrl; }

    public String getEnvironmentId() { return environmentId; }
    public void setEnvironmentId(String environmentId) { this.environmentId = environmentId; }


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
        private String baseUrl;
        private String environmentId;

        /**
         * Sets the {@code baseUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder baseUrl(String value) {
            this.baseUrl = value;
            return this;
        }

        /**
         * Sets the {@code environmentId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder environmentId(String value) {
            this.environmentId = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostTransportMissionControl build() {
            if (baseUrl == null) throw new IllegalStateException("Missing required property: baseUrl");
            if (environmentId == null) throw new IllegalStateException("Missing required property: environmentId");
            RemoteHostTransportMissionControl instance = new RemoteHostTransportMissionControl();
            instance.setBaseUrl(baseUrl);
            instance.setEnvironmentId(environmentId);
            return instance;
        }
    }
}

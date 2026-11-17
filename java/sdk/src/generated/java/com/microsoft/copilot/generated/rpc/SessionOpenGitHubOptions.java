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
 * GitHub integration and session credentials.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenGitHubOptions(
    /** Opt-in: self-fetch and enforce enterprise managed settings, including managed hook policies, at session bootstrap. */
    @JsonProperty("fetchManagedSettings") Boolean fetchManagedSettings,
    /** Declares that a human drives this session's conversation, enabling user-message analysis (sentiment, task completion criteria, and task analytics) for user-authored messages. Automated messages remain excluded. Defaults to false for SDK sessions. Applied when the session is created or loaded from storage; ignored when resuming a session that is already active, and for cloud sessions. */
    @JsonProperty("interactiveUser") Boolean interactiveUser,
    /** Initial authentication info for the session. */
    @JsonProperty("authInfo") Object authInfo,
    /** Options scoped to the built-in CAPI (Copilot API) provider. */
    @JsonProperty("capi") CapiSessionOptions capi,
    /** Whether a CLI host explicitly requested the initial Auto preference. False preserves a settings-derived preference without validating availability during creation; execution still validates it. Defaults to true and is ignored for non-CLI callers. */
    @JsonProperty("autoTierIsExplicit") Boolean autoTierIsExplicit,
    /** Override URL for the Copilot API endpoint. */
    @JsonProperty("copilotUrl") String copilotUrl
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenGitHubOptions {}

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
        private Boolean fetchManagedSettings;
        private Boolean interactiveUser;
        private Object authInfo;
        private CapiSessionOptions capi;
        private Boolean autoTierIsExplicit;
        private String copilotUrl;

        /**
         * Sets the {@code fetchManagedSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fetchManagedSettings(Boolean value) {
            this.fetchManagedSettings = value;
            return this;
        }

        /**
         * Sets the {@code interactiveUser} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder interactiveUser(Boolean value) {
            this.interactiveUser = value;
            return this;
        }

        /**
         * Sets the {@code authInfo} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authInfo(Object value) {
            this.authInfo = value;
            return this;
        }

        /**
         * Sets the {@code capi} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder capi(CapiSessionOptions value) {
            this.capi = value;
            return this;
        }

        /**
         * Sets the {@code autoTierIsExplicit} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder autoTierIsExplicit(Boolean value) {
            this.autoTierIsExplicit = value;
            return this;
        }

        /**
         * Sets the {@code copilotUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder copilotUrl(String value) {
            this.copilotUrl = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenGitHubOptions build() {
            return new SessionOpenGitHubOptions(fetchManagedSettings, interactiveUser, authInfo, capi, autoTierIsExplicit, copilotUrl);
        }
    }
}

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
 * Session event "session.sandbox_state_changed". Read-only sandbox state after a live session policy transition.
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionSandboxStateChangedEvent extends SessionEvent {

    @Override
    public String getType() { return "session.sandbox_state_changed"; }

    @JsonProperty("data")
    private SessionSandboxStateChangedEventData data;

    public SessionSandboxStateChangedEventData getData() { return data; }
    public void setData(SessionSandboxStateChangedEventData data) { this.data = data; }

    /** Data payload for {@link SessionSandboxStateChangedEvent}. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SessionSandboxStateChangedEventData(
        /** Runtime incarnation of the live session. */
        @JsonProperty("incarnation") Long incarnation,
        /** Monotonic revision of this live session's sandbox projection. */
        @JsonProperty("revision") Long revision,
        /** Effective sandbox configuration, including the current managed floor. */
        @JsonInclude(JsonInclude.Include.ALWAYS)
        @JsonProperty("config") Object config,
        /** Exact runtime attribution of the effective sandbox choice. */
        @JsonProperty("configSource") String configSource,
        /** Whether this session holds an authorized or local session opt-out. */
        @JsonProperty("disabledForSession") Boolean disabledForSession,
        /** Whether queued sandbox bypass requests may use the session opt-out. */
        @JsonProperty("autoApproveBypass") Boolean autoApproveBypass,
        /** Explicit session enable choice, when one exists. */
        @JsonInclude(JsonInclude.Include.ALWAYS)
        @JsonProperty("sessionOverride") Boolean sessionOverride
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public SessionSandboxStateChangedEventData {}

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
            private Long incarnation;
            private Long revision;
            private Object config;
            private boolean $presence2;
            private String configSource;
            private Boolean disabledForSession;
            private Boolean autoApproveBypass;
            private Boolean sessionOverride;
            private boolean $presence6;

            /**
             * Sets the {@code incarnation} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder incarnation(Long value) {
                this.incarnation = value;
                return this;
            }

            /**
             * Sets the {@code revision} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder revision(Long value) {
                this.revision = value;
                return this;
            }

            /**
             * Sets the {@code config} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder config(Object value) {
                this.config = value;
                this.$presence2 = true;
                return this;
            }

            /**
             * Sets the {@code configSource} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder configSource(String value) {
                this.configSource = value;
                return this;
            }

            /**
             * Sets the {@code disabledForSession} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder disabledForSession(Boolean value) {
                this.disabledForSession = value;
                return this;
            }

            /**
             * Sets the {@code autoApproveBypass} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder autoApproveBypass(Boolean value) {
                this.autoApproveBypass = value;
                return this;
            }

            /**
             * Sets the {@code sessionOverride} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder sessionOverride(Boolean value) {
                this.sessionOverride = value;
                this.$presence6 = true;
                return this;
            }

            /**
             * Builds a record after checking required property presence.
             * @return a new record
             * @throws IllegalStateException if a required property is missing
             * @since 2.0.0
             */
            public SessionSandboxStateChangedEventData build() {
                if (incarnation == null) throw new IllegalStateException("Missing required property: incarnation");
                if (revision == null) throw new IllegalStateException("Missing required property: revision");
                if (!$presence2) throw new IllegalStateException("Missing required property: config");
                if (configSource == null) throw new IllegalStateException("Missing required property: configSource");
                if (disabledForSession == null) throw new IllegalStateException("Missing required property: disabledForSession");
                if (autoApproveBypass == null) throw new IllegalStateException("Missing required property: autoApproveBypass");
                if (!$presence6) throw new IllegalStateException("Missing required property: sessionOverride");
                return new SessionSandboxStateChangedEventData(incarnation, revision, config, configSource, disabledForSession, autoApproveBypass, sessionOverride);
            }
        }
    }
}

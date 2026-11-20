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
 * Session event "session.error". Session error details for timeline display. Request-correlated completion RPC failures carry the terminal error's same fields in their error data, with stack and completionEventId. errorCode is an optional runtime or Copilot API classification, not a closed enum; clients must tolerate unknown values. CAPI HTTP 429 normalizes missing or unknown codes to rate_limited, while HTTP 402 forwards upstream codes or omits an absent code. Direct BYOK provider codes use the separate providerErrorCode field. A code alone does not define remediation or retry policy.
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionErrorEvent extends SessionEvent {

    @Override
    public String getType() { return "session.error"; }

    @JsonProperty("data")
    private SessionErrorEventData data;

    public SessionErrorEventData getData() { return data; }
    public void setData(SessionErrorEventData data) { this.data = data; }

    /** Data payload for {@link SessionErrorEvent}. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SessionErrorEventData(
        /** Original root user-message ID of the failed request, including failures before user.message is emitted. Absent for session-wide errors or legacy hosts. */
        @JsonProperty("originatingMessageId") String originatingMessageId,
        /** Category of error (e.g., "authentication", "authorization", "policy", "quota", "rate_limit", "context_limit", "query") */
        @JsonProperty("errorType") String errorType,
        /** Optional fine-grained runtime or Copilot API classification; an extensible string, not a closed enum. May be absent; clients must tolerate unknown values. Runtime-local codes include `ai_credit_soft_cap_exhausted` (`errorType: "session_limits"`) and `no_account` (`errorType: "authentication"`): the session has no account, or no credential resolves for its account, so no request was sent. Session RPC rejections for the same state carry `no_account` in their error data. Policy codes include `misalignment_policy_violation` and `cyber_policy`. CAPI rate limits normalize missing or unknown codes to `rate_limited`; recognized codes also include `user_model_rate_limited`, `user_global_rate_limited`, `user_weekly_rate_limited`, and `integration_rate_limited`. HTTP 402 quota failures forward upstream codes, including unknown values, or omit the field if no code is available. Other terminal CAPI query failures expose only reviewed codes. Direct BYOK provider codes use providerErrorCode instead. A code alone does not define remediation or retry policy. */
        @JsonProperty("errorCode") String errorCode,
        /** Exact error code from the model endpoint's structured error envelope (Copilot API, BYOK, or another provider), preserved verbatim when available; unset otherwise. Unlike errorCode, this is not a runtime or Copilot API classification and does not imply a remediation or retry policy. */
        @JsonProperty("providerErrorCode") String providerErrorCode,
        /** Provider-defined error type preserved from a structured upstream error envelope, when available (for example, `invalid_request_error`). */
        @JsonProperty("providerErrorType") String providerErrorType,
        /** Sanitized provider-defined error parameter preserved from a structured upstream error envelope, when available. */
        @JsonProperty("providerErrorParam") String providerErrorParam,
        /** Only set on `errorType: "rate_limit"`. When `true`, the runtime will follow this error with an `auto_mode_switch.requested` event (or silently switch if `continueOnAutoMode` is enabled). UI clients can use this flag to suppress duplicate rendering of the rate-limit error when they show their own auto-mode-switch prompt. */
        @JsonProperty("eligibleForAutoSwitch") Boolean eligibleForAutoSwitch,
        /** Human-readable error message */
        @JsonProperty("message") String message,
        /** What the user must do to recover, when the runtime knows of an action. The `message` never names a client affordance, so a client that offers one — a slash command, a settings pane, a link — renders it from this value. */
        @JsonProperty("remediation") RemediationAction remediation,
        /** Error stack trace, when available */
        @JsonProperty("stack") String stack,
        /** HTTP status code from the upstream request, if applicable */
        @JsonProperty("statusCode") Long statusCode,
        /** GitHub request tracing ID (x-github-request-id header) for correlating with server-side logs */
        @JsonProperty("providerCallId") String providerCallId,
        /** Copilot service request ID (x-copilot-service-request-id header) for CAPI log correlation */
        @JsonProperty("serviceRequestId") String serviceRequestId,
        /** Optional URL associated with this error that the user can open in a browser */
        @JsonProperty("url") String url,
        /** Identifier of the agent-loop turn whose model call failed, matching that turn's assistant.turn_start turnId within the request named by originatingMessageId. Absent when the failure happened outside a model turn. */
        @JsonProperty("turnId") String turnId,
        /** Typed DLP block, set only when providerErrorCode is a known DLP code. Classified from the exact code, never from the message or HTTP status. Other provider codes remain in providerErrorCode. */
        @JsonProperty("dlpBlock") DlpBlockKind dlpBlock
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public SessionErrorEventData {}

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
            private String originatingMessageId;
            private String errorType;
            private String errorCode;
            private String providerErrorCode;
            private String providerErrorType;
            private String providerErrorParam;
            private Boolean eligibleForAutoSwitch;
            private String message;
            private RemediationAction remediation;
            private String stack;
            private Long statusCode;
            private String providerCallId;
            private String serviceRequestId;
            private String url;
            private String turnId;
            private DlpBlockKind dlpBlock;

            /**
             * Sets the {@code originatingMessageId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder originatingMessageId(String value) {
                this.originatingMessageId = value;
                return this;
            }

            /**
             * Sets the {@code errorType} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder errorType(String value) {
                this.errorType = value;
                return this;
            }

            /**
             * Sets the {@code errorCode} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder errorCode(String value) {
                this.errorCode = value;
                return this;
            }

            /**
             * Sets the {@code providerErrorCode} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder providerErrorCode(String value) {
                this.providerErrorCode = value;
                return this;
            }

            /**
             * Sets the {@code providerErrorType} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder providerErrorType(String value) {
                this.providerErrorType = value;
                return this;
            }

            /**
             * Sets the {@code providerErrorParam} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder providerErrorParam(String value) {
                this.providerErrorParam = value;
                return this;
            }

            /**
             * Sets the {@code eligibleForAutoSwitch} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder eligibleForAutoSwitch(Boolean value) {
                this.eligibleForAutoSwitch = value;
                return this;
            }

            /**
             * Sets the {@code message} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder message(String value) {
                this.message = value;
                return this;
            }

            /**
             * Sets the {@code remediation} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder remediation(RemediationAction value) {
                this.remediation = value;
                return this;
            }

            /**
             * Sets the {@code stack} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder stack(String value) {
                this.stack = value;
                return this;
            }

            /**
             * Sets the {@code statusCode} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder statusCode(Long value) {
                this.statusCode = value;
                return this;
            }

            /**
             * Sets the {@code providerCallId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder providerCallId(String value) {
                this.providerCallId = value;
                return this;
            }

            /**
             * Sets the {@code serviceRequestId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder serviceRequestId(String value) {
                this.serviceRequestId = value;
                return this;
            }

            /**
             * Sets the {@code url} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder url(String value) {
                this.url = value;
                return this;
            }

            /**
             * Sets the {@code turnId} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder turnId(String value) {
                this.turnId = value;
                return this;
            }

            /**
             * Sets the {@code dlpBlock} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder dlpBlock(DlpBlockKind value) {
                this.dlpBlock = value;
                return this;
            }

            /**
             * Builds a record after checking required property presence.
             * @return a new record
             * @throws IllegalStateException if a required property is missing
             * @since 2.0.0
             */
            public SessionErrorEventData build() {
                if (errorType == null) throw new IllegalStateException("Missing required property: errorType");
                if (message == null) throw new IllegalStateException("Missing required property: message");
                return new SessionErrorEventData(originatingMessageId, errorType, errorCode, providerErrorCode, providerErrorType, providerErrorParam, eligibleForAutoSwitch, message, remediation, stack, statusCode, providerCallId, serviceRequestId, url, turnId, dlpBlock);
            }
        }
    }
}

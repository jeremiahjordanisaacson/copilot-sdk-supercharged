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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Parameters for sending zero or more user messages to the session in a single turn. Remote-backed (Mission Control) sessions do not support this method and will return an error.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionSendMessagesParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** The user messages to append to the conversation, in order, before running one agent loop. When the batch starts a run, its final message is the primary initiating message; earlier messages provide context, not separate runs or replies. May be empty, in which case a single turn runs over the existing history with no new user message or originatingMessageId. */
    @JsonProperty("messages") List<SendMessageItem> messages,
    /** How to deliver the messages. `enqueue` (default) appends to the message queue. `immediate` interjects during an in-progress turn. */
    @JsonProperty("mode") SendMode mode,
    /** If true, adds the messages to the front of the queue instead of the end */
    @JsonProperty("prepend") Boolean prepend,
    /** The UI mode the agent was in when these messages were sent. Defaults to the session's current mode. */
    @JsonProperty("agentMode") SendAgentMode agentMode,
    /** Custom HTTP headers to include in outbound model requests for this turn. Merged with session-level provider headers; per-turn headers augment and overwrite session-level headers with the same key. */
    @JsonProperty("requestHeaders") Map<String, String> requestHeaders,
    /** Provider-native output format for the whole turn, including an empty message batch and all tool-call iterations. Not inherited by later turns or subagents. Ordinary steering inherits the active format; specifying responseFormat with mode: immediate is an error, even while idle. Returned assistant content remains text; the runtime does not parse or validate it. Unsupported models or schemas produce provider errors. */
    @JsonProperty("responseFormat") SessionSendMessagesParamsResponseFormat responseFormat,
    /** W3C Trace Context traceparent header for distributed tracing of this agent turn */
    @JsonProperty("traceparent") String traceparent,
    /** W3C Trace Context tracestate header for distributed tracing */
    @JsonProperty("tracestate") String tracestate,
    /** Opaque caller-chosen ID for the turn this batch starts. The runtime copies it unchanged into `params._meta["com.github.copilot/turnId"]` on every MCP `tools/call` the agent makes while serving that turn, including calls from sub-agents it spawns and from Autopilot continuations of it. Visible user steering (`mode: "immediate"`) that a running turn consumes hands it to a successor turn: the steering's value replaces the running turn's from the next model call onward, and steering without one clears it. System and hidden steering leave it unchanged. A turn started without one, including one the runtime starts itself, carries no turn ID. The `user.message` event for each message the send delivers echoes it as `clientTurnId` when the message is consumed, and is persisted like other events; the value stamped on MCP calls is held in memory only and is not restored on resume. */
    @JsonProperty("clientTurnId") String clientTurnId,
    /** If true, await completion of the agentic loop for this turn before returning. Defaults to false (fire-and-forget). When true, the result still contains the same `messageIds`; the caller can rely on the agent having processed the messages before the call resolves. Transport-dependent tail semantics: on a LOCAL (in-process) session the wait additionally blocks until the completed turn's event tail has been dispatched to this session's in-process subscribers, so a subsequent read of subscriber state already reflects the turn; on a REMOTE session the wait resolves once the loop completes and mirrored delivery follows over the wire. Callers that need the stronger local guarantee on remote sessions should await the event stream explicitly. */
    @JsonProperty("wait") Boolean wait_
) {

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SessionSendMessagesParamsResponseFormat(
        /** JSON Schema and provider options for the turn's output. */
        @JsonProperty("jsonSchema") JsonSchemaResponseFormat jsonSchema,
        /** Output format discriminator. Currently only json_schema is supported. */
        @JsonProperty("type") String type
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public SessionSendMessagesParamsResponseFormat {}

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
            private JsonSchemaResponseFormat jsonSchema;
            private String type;

            /**
             * Sets the {@code jsonSchema} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder jsonSchema(JsonSchemaResponseFormat value) {
                this.jsonSchema = value;
                return this;
            }

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
             * Builds a record after checking required property presence.
             * @return a new record
             * @throws IllegalStateException if a required property is missing
             * @since 2.0.0
             */
            public SessionSendMessagesParamsResponseFormat build() {
                if (jsonSchema == null) throw new IllegalStateException("Missing required property: jsonSchema");
                if (type == null) throw new IllegalStateException("Missing required property: type");
                return new SessionSendMessagesParamsResponseFormat(jsonSchema, type);
            }
        }
    }

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionSendMessagesParams {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String sessionId;
        private List<SendMessageItem> messages;
        private SendMode mode;
        private Boolean prepend;
        private SendAgentMode agentMode;
        private Map<String, String> requestHeaders;
        private SessionSendMessagesParamsResponseFormat responseFormat;
        private String traceparent;
        private String tracestate;
        private String clientTurnId;
        private Boolean wait_;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code messages} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder messages(List<SendMessageItem> value) {
            this.messages = value;
            return this;
        }

        /**
         * Sets the {@code mode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder mode(SendMode value) {
            this.mode = value;
            return this;
        }

        /**
         * Sets the {@code prepend} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder prepend(Boolean value) {
            this.prepend = value;
            return this;
        }

        /**
         * Sets the {@code agentMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder agentMode(SendAgentMode value) {
            this.agentMode = value;
            return this;
        }

        /**
         * Sets the {@code requestHeaders} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder requestHeaders(Map<String, String> value) {
            this.requestHeaders = value;
            return this;
        }

        /**
         * Sets the {@code responseFormat} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder responseFormat(SessionSendMessagesParamsResponseFormat value) {
            this.responseFormat = value;
            return this;
        }

        /**
         * Sets the {@code traceparent} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder traceparent(String value) {
            this.traceparent = value;
            return this;
        }

        /**
         * Sets the {@code tracestate} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder tracestate(String value) {
            this.tracestate = value;
            return this;
        }

        /**
         * Sets the {@code clientTurnId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder clientTurnId(String value) {
            this.clientTurnId = value;
            return this;
        }

        /**
         * Sets the {@code wait} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder wait_(Boolean value) {
            this.wait_ = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionSendMessagesParams build() {
            if (messages == null) throw new IllegalStateException("Missing required property: messages");
            return new SessionSendMessagesParams(sessionId, messages, mode, prepend, agentMode, requestHeaders, responseFormat, traceparent, tracestate, clientTurnId, wait_);
        }
    }
}

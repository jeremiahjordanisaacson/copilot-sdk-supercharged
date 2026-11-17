/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.generated.SessionEvent;
import java.util.Optional;
import java.util.function.Consumer;
/** Grouped SessionEvent configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionEventOptions {

    private boolean streaming;
    private Boolean includeSubAgentStreamingEvents;
    private Consumer<SessionEvent> onEvent;

    /**
     * Returns whether streaming is enabled.
     *
     * @return {@code true} if streaming is enabled
     */
    public boolean isStreaming() {
        return streaming;
    }

    /**
     * Sets whether to enable streaming of response chunks.
     *
     * @param streaming
     *            {@code true} to enable streaming
     * @return this config for method chaining
     */
    public SessionEventOptions setStreaming(boolean streaming) {
        this.streaming = streaming;
        return this;
    }

    /**
     * Gets whether sub-agent streaming events are included.
     *
     * @return {@code true} to include sub-agent streaming events, {@code false} to
     *         suppress them, or {@code null} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getIncludeSubAgentStreamingEvents() {
        return Optional.ofNullable(includeSubAgentStreamingEvents);
    }

    /**
     * Sets whether to include sub-agent streaming events in the event stream.
     *
     * @param includeSubAgentStreamingEvents
     *            {@code true} to include streaming events, {@code false} to
     *            suppress
     * @return this config for method chaining
     */
    public SessionEventOptions setIncludeSubAgentStreamingEvents(boolean includeSubAgentStreamingEvents) {
        this.includeSubAgentStreamingEvents = includeSubAgentStreamingEvents;
        return this;
    }

    /**
     * Gets the event handler registered before the session.resume RPC is issued.
     *
     * @return the event handler, or {@code null} if not set
     */
    @JsonIgnore
    public Consumer<SessionEvent> getOnEvent() {
        return onEvent;
    }

    /**
     * Sets an event handler that is registered on the session before the
     * {@code session.resume} RPC is issued.
     * <p>
     * Equivalent to calling
     * {@link com.microsoft.copilot.CopilotSession#on(Consumer)} immediately after
     * resumption, but executes earlier in the lifecycle so no events are missed.
     *
     * @param onEvent
     *            the event handler to register before session resumption
     * @return this config for method chaining
     */
    public SessionEventOptions setOnEvent(Consumer<SessionEvent> onEvent) {
        this.onEvent = onEvent;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionEventOptions clone() {
        SessionEventOptions copy = new SessionEventOptions();
        copy.streaming = this.streaming;
        copy.includeSubAgentStreamingEvents = this.includeSubAgentStreamingEvents;
        copy.onEvent = this.onEvent;
        return copy;
    }

    /**
     * Clears the includeSubAgentStreamingEvents setting, reverting to the default
     * behavior.
     *
     * @return this instance for method chaining
     */
    public SessionEventOptions clearIncludeSubAgentStreamingEvents() {
        this.includeSubAgentStreamingEvents = null;
        return this;
    }
}

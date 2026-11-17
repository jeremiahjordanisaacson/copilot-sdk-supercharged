/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
/** Grouped SessionEventRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionEventRequestOptions {

    @JsonProperty("streaming")
    private Boolean streaming;

    @JsonProperty("includeSubAgentStreamingEvents")
    private Boolean includeSubAgentStreamingEvents;

    /** Gets streaming flag. @return the flag */
    public Boolean getStreaming() {
        return streaming;
    }

    /** Sets streaming flag. @param streaming the flag */
    public SessionEventRequestOptions setStreaming(boolean streaming) {
        this.streaming = streaming;
        return this;
    }

    /** Gets include sub-agent streaming events flag. @return the flag */
    public Boolean getIncludeSubAgentStreamingEvents() {
        return includeSubAgentStreamingEvents;
    }

    /**
     * Sets include sub-agent streaming events flag. @param
     * includeSubAgentStreamingEvents the flag
     */
    public SessionEventRequestOptions setIncludeSubAgentStreamingEvents(boolean includeSubAgentStreamingEvents) {
        this.includeSubAgentStreamingEvents = includeSubAgentStreamingEvents;
        return this;
    }

    /**
     * Clears the streaming setting, reverting to the default behavior.
     */
    public void clearStreaming() {
        this.streaming = null;
    }

    /**
     * Clears the includeSubAgentStreamingEvents setting, reverting to the default
     * behavior.
     */
    public void clearIncludeSubAgentStreamingEvents() {
        this.includeSubAgentStreamingEvents = null;
    }
}

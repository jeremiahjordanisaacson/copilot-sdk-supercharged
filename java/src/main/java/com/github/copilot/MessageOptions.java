/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import java.util.List;
import java.util.Map;

/**
 * Options for sending a message to a session.
 */
public class MessageOptions {
    private String prompt;
    private List<Map<String, Object>> attachments;
    private String mode;
    private String responseFormat;
    private Types.ImageOptions imageOptions;
    /** Custom HTTP headers to include in outbound model requests for this turn. */
    private Map<String, String> requestHeaders;
    /** Agent execution mode for this turn (e.g. "agent", "chat"). */
    private String agentMode;
    /** Prompt text to display in the UI, when it differs from the model prompt. */
    private String displayPrompt;

    /**
     * Creates message options for the given prompt.
     *
     * @param prompt the message prompt
     */
    public MessageOptions(String prompt) {
        this.prompt = prompt;
    }

    public MessageOptions prompt(String prompt) { this.prompt = prompt; return this; }
    public MessageOptions attachments(List<Map<String, Object>> attachments) { this.attachments = attachments; return this; }
    public MessageOptions mode(String mode) { this.mode = mode; return this; }
    public MessageOptions responseFormat(String responseFormat) { this.responseFormat = responseFormat; return this; }
    public MessageOptions imageOptions(Types.ImageOptions imageOptions) { this.imageOptions = imageOptions; return this; }
    public MessageOptions requestHeaders(Map<String, String> headers) { this.requestHeaders = headers; return this; }
    public MessageOptions agentMode(String agentMode) { this.agentMode = agentMode; return this; }
    public MessageOptions displayPrompt(String displayPrompt) { this.displayPrompt = displayPrompt; return this; }

    public String getPrompt() { return prompt; }
    public List<Map<String, Object>> getAttachments() { return attachments; }
    public String getMode() { return mode; }
    public String getResponseFormat() { return responseFormat; }
    public Types.ImageOptions getImageOptions() { return imageOptions; }
    public Map<String, String> getRequestHeaders() { return requestHeaders; }
    public String getAgentMode() { return agentMode; }
    public String getDisplayPrompt() { return displayPrompt; }
}

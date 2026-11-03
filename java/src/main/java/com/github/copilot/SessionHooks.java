/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import java.util.Map;

/**
 * Configuration for session hooks - lifecycle event interceptors.
 */
public class SessionHooks {
    private HookHandler onPreToolUse;
    private HookHandler onPostToolUse;
    private HookHandler onPreMcpToolCall;
    private HookHandler onUserPromptSubmitted;
    /** Invoked after the user prompt has been transformed. */
    private HookHandler onUserPromptTransformed;
    private HookHandler onSessionStart;
    private HookHandler onSessionEnd;
    private HookHandler onErrorOccurred;
    /**
     * Invoked before a sub-agent's first turn ({@code "subagentStart"}). Input carries
     * {@code transcriptPath}, {@code agentName}, {@code agentDisplayName}, {@code agentDescription}
     * plus the base hook fields; return {@code additionalContext} to prepend to the child's prompt.
     */
    private HookHandler onSubagentStart;
    /**
     * Invoked after a sub-agent completes a turn ({@code "subagentStop"}). Input adds {@code agentId},
     * {@code agentType}, {@code stopReason} and {@code response}; return {@code decision}
     * ({@code "block"} or {@code "allow"}), {@code reason}, or {@code modifiedResponse}.
     */
    private HookHandler onSubagentStop;

    /** Generic hook handler. */
    @FunctionalInterface
    public interface HookHandler {
        /**
         * Handles a session hook invocation.
         *
         * @param input the hook input payload
         * @param sessionId the session identifier
         * @return the hook output payload
         * @throws Exception if handling fails
         */
        Map<String, Object> handle(Map<String, Object> input, String sessionId) throws Exception;
    }

    public SessionHooks onPreToolUse(HookHandler handler) { this.onPreToolUse = handler; return this; }
    public SessionHooks onPostToolUse(HookHandler handler) { this.onPostToolUse = handler; return this; }
    public SessionHooks onPreMcpToolCall(HookHandler handler) { this.onPreMcpToolCall = handler; return this; }
    public SessionHooks onUserPromptSubmitted(HookHandler handler) { this.onUserPromptSubmitted = handler; return this; }
    public SessionHooks onUserPromptTransformed(HookHandler handler) { this.onUserPromptTransformed = handler; return this; }
    public SessionHooks onSessionStart(HookHandler handler) { this.onSessionStart = handler; return this; }
    public SessionHooks onSessionEnd(HookHandler handler) { this.onSessionEnd = handler; return this; }
    public SessionHooks onErrorOccurred(HookHandler handler) { this.onErrorOccurred = handler; return this; }
    public SessionHooks onSubagentStart(HookHandler handler) { this.onSubagentStart = handler; return this; }
    public SessionHooks onSubagentStop(HookHandler handler) { this.onSubagentStop = handler; return this; }

    public HookHandler getOnPreToolUse() { return onPreToolUse; }
    public HookHandler getOnPostToolUse() { return onPostToolUse; }
    public HookHandler getOnPreMcpToolCall() { return onPreMcpToolCall; }
    public HookHandler getOnUserPromptSubmitted() { return onUserPromptSubmitted; }
    public HookHandler getOnUserPromptTransformed() { return onUserPromptTransformed; }
    public HookHandler getOnSessionStart() { return onSessionStart; }
    public HookHandler getOnSessionEnd() { return onSessionEnd; }
    public HookHandler getOnErrorOccurred() { return onErrorOccurred; }
    public HookHandler getOnSubagentStart() { return onSubagentStart; }
    public HookHandler getOnSubagentStop() { return onSubagentStop; }

    /**
     * Returns whether any hook handler is configured.
     *
     * @return true if at least one hook handler is set
     */
    public boolean hasAnyHook() {
        return onPreToolUse != null || onPostToolUse != null || onPreMcpToolCall != null || onUserPromptSubmitted != null
            || onUserPromptTransformed != null
            || onSessionStart != null || onSessionEnd != null || onErrorOccurred != null
            || onSubagentStart != null || onSubagentStop != null;
    }

    /** Get handler by hook type name. */
    public HookHandler getHandler(String hookType) {
        return switch (hookType) {
            case "preToolUse" -> onPreToolUse;
            case "postToolUse" -> onPostToolUse;
            case "preMcpToolCall" -> onPreMcpToolCall;
            case "userPromptSubmitted" -> onUserPromptSubmitted;
            case "userPromptTransformed" -> onUserPromptTransformed;
            case "sessionStart" -> onSessionStart;
            case "sessionEnd" -> onSessionEnd;
            case "errorOccurred" -> onErrorOccurred;
            case "subagentStart" -> onSubagentStart;
            case "subagentStop" -> onSubagentStop;
            default -> null;
        };
    }
}

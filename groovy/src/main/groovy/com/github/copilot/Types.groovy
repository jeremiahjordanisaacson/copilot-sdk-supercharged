/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import groovy.transform.CompileStatic
import groovy.transform.ToString
import groovy.transform.EqualsAndHashCode

/**
 * Type definitions for the Groovy Copilot SDK.
 *
 * Uses Groovy idioms: {@code @ToString}, {@code @EqualsAndHashCode}, closures as handlers,
 * and map-friendly constructors.
 */
@CompileStatic
final class Types {
    private Types() {}

    /** Connection state of the client. */
    enum ConnectionState {
        DISCONNECTED, CONNECTING, CONNECTED, ERROR
    }

    /** Reasoning effort levels for models that support it. */
    enum ReasoningEffort {
        LOW, MEDIUM, HIGH, XHIGH
    }

    /** Result type from a tool execution. */
    enum ToolResultType {
        SUCCESS, FAILURE, REJECTED, DENIED
    }

    /** Structured result from a tool execution. */
    @ToString(includeNames = true)
    static class ToolResultObject {
        String textResultForLlm
        String resultType
        String error
        String sessionLog
        Map<String, Object> toolTelemetry = [:]
    }

    /** Information about a tool invocation. */
    @ToString(includeNames = true)
    static class ToolInvocation {
        String sessionId
        String toolCallId
        String toolName
        Object arguments

        ToolInvocation(String sessionId, String toolCallId, String toolName, Object arguments) {
            this.sessionId = sessionId
            this.toolCallId = toolCallId
            this.toolName = toolName
            this.arguments = arguments
        }
    }

    /** Tool definition for registering custom tools. */
    @ToString(includeNames = true, excludes = ['handler'])
    static class Tool {
        String name
        String description
        Map<String, Object> parameters
        Closure handler

        Tool(String name, String description, Map<String, Object> parameters, Closure handler) {
            this.name = name
            this.description = description
            this.parameters = parameters
            this.handler = handler
        }
    }

    /** Permission request from the server. */
    @ToString(includeNames = true)
    static class PermissionRequest {
        String kind
        String toolCallId
    }

    /** Permission request result. */
    @ToString(includeNames = true)
    static class PermissionRequestResult {
        String kind
        List<Object> rules

        PermissionRequestResult() {}
        PermissionRequestResult(String kind) { this.kind = kind }
    }

    /** User input request from the agent. */
    @ToString(includeNames = true)
    static class UserInputRequest {
        String question
        List<String> choices
        Boolean allowFreeform
    }

    /** User input response. */
    @ToString(includeNames = true)
    static class UserInputResponse {
        String answer
        boolean wasFreeform

        UserInputResponse(String answer, boolean wasFreeform) {
            this.answer = answer
            this.wasFreeform = wasFreeform
        }
    }

    /** Ping response. */
    @ToString(includeNames = true)
    static class PingResponse {
        String message
        long timestamp
        Integer protocolVersion
    }

    /** Status response. */
    @ToString(includeNames = true)
    static class GetStatusResponse {
        String version
        int protocolVersion
    }

    /** Auth status response. */
    @ToString(includeNames = true)
    static class GetAuthStatusResponse {
        boolean isAuthenticated
        String authType
        String host
        String login
        String statusMessage
    }

    /** Model capabilities. */
    @ToString(includeNames = true)
    static class ModelCapabilities {
        ModelSupports supports
        ModelLimits limits

        @ToString(includeNames = true)
        static class ModelSupports {
            boolean vision
            boolean reasoningEffort
        }

        @ToString(includeNames = true)
        static class ModelLimits {
            Integer maxPromptTokens
            int maxContextWindowTokens
        }
    }

    /** Model information. */
    @ToString(includeNames = true)
    static class ModelInfo {
        String id
        String name
        ModelCapabilities capabilities
    }

    /** Session metadata. */
    @ToString(includeNames = true)
    static class SessionMetadata {
        String sessionId
        String startTime
        String modifiedTime
        String summary
        boolean isRemote
    }

    /** Session lifecycle event. */
    @ToString(includeNames = true)
    static class SessionLifecycleEvent {
        String type
        String sessionId
        Map<String, Object> metadata
    }

    /** Session event (generic). */
    @ToString(includeNames = true)
    static class SessionEvent {
        String type
        Map<String, Object> data = [:]
    }

    /** Provider configuration for BYOK. */
    @ToString(includeNames = true)
    static class ProviderConfig {
        String type
        String baseUrl
        String apiKey
        String bearerToken
        String wireApi
    }

    /** MCP server configuration. */
    @ToString(includeNames = true)
    static class MCPServerConfig {
        List<String> tools
        String type
        Integer timeout
        String command
        List<String> args
        Map<String, String> env
        String cwd
        String url
        Map<String, String> headers
    }

    /** Custom agent configuration. */
    @ToString(includeNames = true)
    static class CustomAgentConfig {
        String name
        String displayName
        String description
        List<String> tools
        String prompt
        Map<String, MCPServerConfig> mcpServers
        Boolean infer
        List<String> skills
    }

    /** Infinite session configuration. */
    @ToString(includeNames = true)
    static class InfiniteSessionConfig {
        Boolean enabled
        Double backgroundCompactionThreshold
        Double bufferExhaustionThreshold
    }

    /** Image options for generation requests. */
    @ToString(includeNames = true)
    static class ImageOptions {
        String size
        String quality
        String style

        ImageOptions() {}
        ImageOptions(String size, String quality, String style) {
            this.size = size
            this.quality = quality
            this.style = style
        }
    }

    /** Context passed to a command handler when a command is executed. */
    @ToString(includeNames = true)
    static class CommandContext {
        String sessionId
        String command
        String commandName
        String args
    }

    /** Definition of a slash command registered with the session. */
    @ToString(includeNames = true, excludes = ['handler'])
    static class CommandDefinition {
        String name
        String description
        Closure handler

        CommandDefinition() {}
        CommandDefinition(String name, String description, Closure handler) {
            this.name = name
            this.description = description
            this.handler = handler
        }
    }

    /** Context for an elicitation handler invocation. */
    @ToString(includeNames = true)
    static class ElicitationContext {
        String sessionId
        String message
        Object requestedSchema
        String mode
        String elicitationSource
        String url
    }

    /** Result returned from an elicitation request. */
    @ToString(includeNames = true)
    static class ElicitationResult {
        String action
        Map<String, Object> content

        ElicitationResult() {}
        ElicitationResult(String action, Map<String, Object> content = null) {
            this.action = action
            this.content = content
        }
    }

    /** Configuration for a custom session filesystem provider. */
    @ToString(includeNames = true)
    static class SessionFsConfig {
        String initialCwd
        String sessionStatePath
        String conventions

        SessionFsConfig() {}
        SessionFsConfig(String initialCwd, String sessionStatePath, String conventions) {
            this.initialCwd = initialCwd
            this.sessionStatePath = sessionStatePath
            this.conventions = conventions
        }
    }

    /** Error encountered during stop(). */
    @ToString(includeNames = true)
    static class StopError {
        String message
        StopError(String message) { this.message = message }
    }
}

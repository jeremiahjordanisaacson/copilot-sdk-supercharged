/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * Configuration classes and data transfer objects for the Copilot SDK.
 *
 * <p>
 * This package contains all the configuration, request, response, and data
 * transfer objects used throughout the SDK. These classes are designed for JSON
 * serialization with Jackson and provide fluent setter methods for convenient
 * configuration.
 *
 * <h2>Client Configuration</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.rpc.CopilotClientOptions} - Options for
 * configuring the {@link com.microsoft.copilot.CopilotClient}, including CLI
 * path, port, transport mode, and auto-start behavior.</li>
 * </ul>
 *
 * <h2>Session Configuration</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.rpc.CreateSessionOptions} - Configuration
 * for creating a new session, including model selection, tools, system message,
 * and MCP server configuration.</li>
 * <li>{@link com.microsoft.copilot.rpc.ResumeSessionOptions} - Configuration
 * for resuming an existing session.</li>
 * <li>{@link com.microsoft.copilot.rpc.InfiniteSessionConfig} - Configuration
 * for infinite sessions with automatic context compaction.</li>
 * <li>{@link com.microsoft.copilot.rpc.SystemMessageConfig} - System message
 * customization options.</li>
 * </ul>
 *
 * <h2>Message and Tool Configuration</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.rpc.MessageOptions} - Options for sending
 * messages, including prompt text and attachments.</li>
 * <li>{@link com.microsoft.copilot.rpc.ToolDefinition} - Definition of a custom
 * tool that can be invoked by the assistant.</li>
 * <li>{@link com.microsoft.copilot.rpc.ToolInvocation} - Represents a tool
 * invocation request from the assistant.</li>
 * <li>{@link com.microsoft.copilot.rpc.Attachment} - File attachment for
 * messages.</li>
 * </ul>
 *
 * <h2>Provider Configuration (BYOK)</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.rpc.ProviderConfig} - Configuration for
 * using your own API keys with custom providers (OpenAI, Azure, etc.).</li>
 * <li>{@link com.microsoft.copilot.rpc.AzureOptions} - Azure-specific
 * configuration options.</li>
 * </ul>
 *
 * <h2>Model Information</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.generated.rpc.Model} - Information about an
 * available AI model.</li>
 * <li>{@link com.microsoft.copilot.generated.rpc.ModelCapabilities} - Model
 * capabilities and limits.</li>
 * <li>{@link com.microsoft.copilot.generated.rpc.ModelPolicy} - Model policy
 * and state information.</li>
 * </ul>
 *
 * <h2>Custom Agents</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.rpc.CustomAgentConfig} - Configuration for
 * custom agents with specialized behaviors and tools.</li>
 * </ul>
 *
 * <h2>Permissions</h2>
 * <ul>
 * <li>{@link com.microsoft.copilot.rpc.PermissionHandler} - Handler for
 * permission requests from the assistant.</li>
 * <li>{@link com.microsoft.copilot.rpc.PermissionRequest} - A permission
 * request from the assistant.</li>
 * <li>{@link com.microsoft.copilot.rpc.PermissionRequestResult} - Result of a
 * permission request decision.</li>
 * </ul>
 *
 * <h2>Usage Example</h2>
 *
 * <pre>{@code
 * var config = new CreateSessionOptions().setModel("gpt-5.4").setEvents(new SessionEventOptions().setStreaming(true))
 * 		.setSystemMessage(new SystemMessageConfig().setMode(SystemMessageMode.APPEND)
 * 				.setContent("Be concise in your responses."))
 * 		.setTools(new SessionToolOptions()
 * 				.setDefinitions(List.of(ToolDefinition.create("my_tool", "Description", schema, handler))));
 *
 * var session = client.createSession(config).get();
 * }</pre>
 *
 * @see com.microsoft.copilot.CopilotClient
 * @see com.microsoft.copilot.CopilotSession
 */
package com.microsoft.copilot.rpc;

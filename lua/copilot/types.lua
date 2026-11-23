--- Type constructors for the Copilot SDK.
-- All types are represented as plain Lua tables. Constructor functions provide
-- default values and documentation for each field.

local M = {}

-- ---------------------------------------------------------------------------
-- Connection state constants
-- ---------------------------------------------------------------------------

M.ConnectionState = {
    DISCONNECTED = "disconnected",
    CONNECTING   = "connecting",
    CONNECTED    = "connected",
    ERROR        = "error",
}

-- ---------------------------------------------------------------------------
-- Session event type constants
-- ---------------------------------------------------------------------------

M.SessionEventType = {
    ABORT                         = "abort",
    ASSISTANT_INTENT              = "assistant.intent",
    ASSISTANT_MESSAGE             = "assistant.message",
    ASSISTANT_MESSAGE_DELTA       = "assistant.message_delta",
    ASSISTANT_REASONING           = "assistant.reasoning",
    ASSISTANT_REASONING_DELTA     = "assistant.reasoning_delta",
    ASSISTANT_TURN_END            = "assistant.turn_end",
    ASSISTANT_TURN_START          = "assistant.turn_start",
    ASSISTANT_USAGE               = "assistant.usage",
    HOOK_END                      = "hook.end",
    HOOK_START                    = "hook.start",
    PENDING_MESSAGES_MODIFIED     = "pending_messages.modified",
    SESSION_COMPACTION_COMPLETE   = "session.compaction_complete",
    SESSION_COMPACTION_START      = "session.compaction_start",
    SESSION_ERROR                 = "session.error",
    SESSION_HANDOFF               = "session.handoff",
    SESSION_IDLE                  = "session.idle",
    SESSION_INFO                  = "session.info",
    SESSION_MODEL_CHANGE          = "session.model_change",
    SESSION_RESUME                = "session.resume",
    SESSION_SHUTDOWN              = "session.shutdown",
    SESSION_SNAPSHOT_REWIND       = "session.snapshot_rewind",
    SESSION_START                 = "session.start",
    SESSION_TRUNCATION            = "session.truncation",
    SESSION_USAGE_INFO            = "session.usage_info",
    SKILL_INVOKED                 = "skill.invoked",
    SUBAGENT_COMPLETED            = "subagent.completed",
    SUBAGENT_FAILED               = "subagent.failed",
    SUBAGENT_SELECTED             = "subagent.selected",
    SUBAGENT_STARTED              = "subagent.started",
    SYSTEM_MESSAGE                = "system.message",
    TOOL_EXECUTION_COMPLETE       = "tool.execution_complete",
    TOOL_EXECUTION_PARTIAL_RESULT = "tool.execution_partial_result",
    TOOL_EXECUTION_PROGRESS       = "tool.execution_progress",
    TOOL_EXECUTION_START          = "tool.execution_start",
    TOOL_SHELL_OUTPUT             = "tool.shell_output",
    TOOL_USER_REQUESTED           = "tool.user_requested",
    USER_MESSAGE                  = "user.message",
}

-- ---------------------------------------------------------------------------
-- Session lifecycle event types
-- ---------------------------------------------------------------------------

M.SessionLifecycleEventType = {
    CREATED    = "session.created",
    DELETED    = "session.deleted",
    UPDATED    = "session.updated",
    FOREGROUND = "session.foreground",
    BACKGROUND = "session.background",
}

-- ---------------------------------------------------------------------------
-- Constructor helpers
-- ---------------------------------------------------------------------------

--- Create a new SessionEvent table.
-- SessionEvent is the primary event type dispatched during a session.
-- @param fields table with optional keys: type, id, parentId, timestamp, data, ephemeral
-- @return table SessionEvent
function M.SessionEvent(fields)
    fields = fields or {}
    return {
        type      = fields.type or "",
        id        = fields.id or "",
        parentId  = fields.parentId,        -- optional string
        agentId   = fields.agentId,         -- optional string
        timestamp = fields.timestamp or "",
        data      = fields.data or {},
        ephemeral = fields.ephemeral,        -- optional boolean
    }
end

--- Create a ToolResult table.
-- @param fields table with optional keys: textResultForLlm, binaryResultsForLlm, resultType, error, sessionLog, toolTelemetry
-- @return table ToolResult
function M.ToolResult(fields)
    fields = fields or {}
    return {
        textResultForLlm    = fields.textResultForLlm or "",
        binaryResultsForLlm = fields.binaryResultsForLlm,   -- optional array
        resultType           = fields.resultType or "success",
        error                = fields.error,                   -- optional string
        sessionLog           = fields.sessionLog,              -- optional string
        toolTelemetry        = fields.toolTelemetry,           -- optional table
    }
end

--- Create a ToolBinaryResult table.
-- @param fields table with keys: data, mimeType, type, description
-- @return table ToolBinaryResult
function M.ToolBinaryResult(fields)
    fields = fields or {}
    return {
        data        = fields.data or "",
        mimeType    = fields.mimeType or "",
        type        = fields.type or "",
        description = fields.description,   -- optional string
    }
end

--- Create a Tool table.
-- @param fields table with keys: name, description, parameters, handler, idempotent
-- @return table Tool
function M.Tool(fields)
    fields = fields or {}
    return {
        name        = fields.name or "",
        description = fields.description or "",
        parameters  = fields.parameters,    -- optional table (JSON Schema)
        handler     = fields.handler,       -- function(invocation) -> ToolResult, err
        idempotent  = fields.idempotent,    -- optional boolean, repeating a call has no additional effect
    }
end

--- Create a ToolInvocation table.
-- @param fields table with keys: sessionId, toolCallId, toolName, arguments
-- @return table ToolInvocation
function M.ToolInvocation(fields)
    fields = fields or {}
    return {
        sessionId  = fields.sessionId or "",
        toolCallId = fields.toolCallId or "",
        toolName   = fields.toolName or "",
        arguments  = fields.arguments,
    }
end

--- Create a ClientOptions table.
-- @param fields table with optional keys: cliPath, cwd, logLevel, env, githubToken, useLoggedInUser, sessionIdleTimeoutSeconds
-- @return table ClientOptions
function M.ClientOptions(fields)
    fields = fields or {}
    return {
        cliPath                    = fields.cliPath or "copilot",
        cwd                        = fields.cwd,
        logLevel                   = fields.logLevel or "info",
        env                        = fields.env,            -- optional table of key=value strings
        githubToken                = fields.githubToken,
        useLoggedInUser            = fields.useLoggedInUser, -- optional boolean
        autoStart                  = fields.autoStart ~= false,   -- default true
        autoRestart                = fields.autoRestart ~= false,  -- default true
        sessionIdleTimeoutSeconds  = fields.sessionIdleTimeoutSeconds, -- optional integer, server-wide idle timeout
        sessionFs                  = fields.sessionFs,                  -- optional table, session filesystem provider
        copilotHome                = fields.copilotHome,                -- optional string, copilot home directory path
        tcpConnectionToken         = fields.tcpConnectionToken,         -- optional string, TCP connection token
        requestHandler             = fields.requestHandler,            -- optional CopilotRequestHandler, intercepts outbound LLM requests
        bearerTokenProvider        = fields.bearerTokenProvider,       -- optional function(ProviderTokenArgs) -> string, BYOK bearer token
        clientInfo                 = fields.clientInfo,                -- optional ClientInfo, application identity sent on the connect handshake
        onGetTraceContext          = fields.onGetTraceContext,         -- optional function() -> { traceparent, tracestate }
        mode                       = fields.mode,                      -- optional string (ClientMode), "coding-agent" (default) or "empty"
        network                    = fields.network,                   -- optional NetworkConfig, host network callbacks
    }
end

--- Create a local/stdio MCP server configuration.
-- @param fields table with keys: command, args, tools, type, timeout, env, cwd
-- @return table MCPStdioServerConfig
function M.MCPStdioServerConfig(fields)
    fields = fields or {}
    return {
        tools   = fields.tools or {"*"},
        type    = fields.type or "local",
        timeout = fields.timeout,
        command = fields.command or "",
        args    = fields.args or {},
        env     = fields.env,
        cwd     = fields.cwd,
    }
end

--- Create a remote HTTP/SSE MCP server configuration.
-- @param fields table with keys: url, type, tools, timeout, headers
-- @return table MCPHTTPServerConfig
function M.MCPHTTPServerConfig(fields)
    fields = fields or {}
    return {
        tools   = fields.tools or {"*"},
        type    = fields.type or "http",
        timeout = fields.timeout,
        url     = fields.url or "",
        headers = fields.headers,
    }
end

-- ---------------------------------------------------------------------------
-- System message configuration types
-- ---------------------------------------------------------------------------

--- Known system prompt section identifiers for the "customize" mode.
M.SystemPromptSection = {
    IDENTITY            = "identity",
    TONE                = "tone",
    TOOL_EFFICIENCY     = "tool_efficiency",
    ENVIRONMENT_CONTEXT = "environment_context",
    CODE_CHANGE_RULES   = "code_change_rules",
    GUIDELINES          = "guidelines",
    SAFETY              = "safety",
    TOOL_INSTRUCTIONS   = "tool_instructions",
    CUSTOM_INSTRUCTIONS = "custom_instructions",
    LAST_INSTRUCTIONS   = "last_instructions",
}

--- Override action constants for system prompt sections.
M.SectionOverrideAction = {
    REPLACE = "replace",
    REMOVE  = "remove",
    APPEND  = "append",
    PREPEND = "prepend",
}

--- Create a SectionOverride table.
-- @param fields table with keys: action, content
-- @return table SectionOverride
function M.SectionOverride(fields)
    fields = fields or {}
    return {
        action  = fields.action or "replace",
        content = fields.content,              -- optional string
    }
end

--- Create a SystemMessageConfig table.
-- @param fields table with keys: mode, content, sections
-- @return table SystemMessageConfig
function M.SystemMessageConfig(fields)
    fields = fields or {}
    return {
        mode     = fields.mode,                -- "append", "replace", or "customize"
        content  = fields.content,             -- optional string
        sections = fields.sections,            -- optional table of section_id => SectionOverride
    }
end

--- Create a CommandContext table.
-- @param fields table with keys: sessionId, command, commandName, args
-- @return table CommandContext
function M.CommandContext(fields)
    fields = fields or {}
    return {
        sessionId   = fields.sessionId,
        command     = fields.command,
        commandName = fields.commandName,
        args        = fields.args or "",
    }
end

--- Create a CommandDefinition table.
-- @param fields table with keys: name, description, handler
-- @return table CommandDefinition
function M.CommandDefinition(fields)
    fields = fields or {}
    return {
        name        = fields.name,
        description = fields.description,
        handler     = fields.handler,    -- function(CommandContext)
    }
end

--- Create an ElicitationContext table.
-- @param fields table with keys: sessionId, message, requestedSchema, mode, elicitationSource, url
-- @return table ElicitationContext
function M.ElicitationContext(fields)
    fields = fields or {}
    return {
        sessionId        = fields.sessionId,
        message          = fields.message,
        requestedSchema  = fields.requestedSchema,  -- table or nil
        mode             = fields.mode,
        elicitationSource = fields.elicitationSource,
        url              = fields.url,
    }
end

--- Create an ElicitationResult table.
-- @param fields table with keys: action, content
-- @return table ElicitationResult
function M.ElicitationResult(fields)
    fields = fields or {}
    return {
        action  = fields.action,   -- "accept", "decline", or "cancel"
        content = fields.content,  -- table or nil
    }
end

--- Create a SessionConfig table.
-- @param fields table with optional keys (see Go SessionConfig)
-- @return table SessionConfig
function M.SessionConfig(fields)
    fields = fields or {}
    return {
        sessionId                       = fields.sessionId,
        model                           = fields.model,
        reasoningEffort                 = fields.reasoningEffort,
        configDir                       = fields.configDir,
        tools                           = fields.tools,              -- array of Tool
        systemMessage                   = fields.systemMessage,      -- {mode, content}
        availableTools                  = fields.availableTools,      -- array of strings
        excludedTools                   = fields.excludedTools,       -- array of strings
        onPermissionRequest             = fields.onPermissionRequest, -- function
        onUserInputRequest              = fields.onUserInputRequest,  -- function
        hooks                           = fields.hooks,               -- SessionHooks table
        workingDirectory                = fields.workingDirectory,
        streaming                       = fields.streaming,           -- boolean
        provider                        = fields.provider,            -- ProviderConfig
        mcpServers                      = fields.mcpServers,          -- table
        customAgents                    = fields.customAgents,        -- array
        skillDirectories                = fields.skillDirectories,    -- array
        disabledSkills                  = fields.disabledSkills,      -- array
        infiniteSessions                = fields.infiniteSessions,    -- InfiniteSessionConfig
        modelCapabilities               = fields.modelCapabilities,               -- table, model capabilities overrides
        enableConfigDiscovery           = fields.enableConfigDiscovery,           -- boolean, auto-discover MCP server configs (default: false)
        includeSubAgentStreamingEvents  = fields.includeSubAgentStreamingEvents,  -- boolean, include sub-agent streaming events (default: true)
        -- GitHub token for authentication. Overrides client-level token for this session only.
        gitHubToken                     = fields.gitHubToken,
        commands                        = fields.commands,                        -- array of CommandDefinition
        onElicitationRequest            = fields.onElicitationRequest,            -- function(ElicitationContext) -> ElicitationResult
        instructionDirectories          = fields.instructionDirectories,          -- array of strings, instruction directories
        -- Upstream-sync session options (parity with @github/copilot-sdk)
        enableCitations                 = fields.enableCitations,                 -- boolean, enable inline source citations
        excludedBuiltinAgents           = fields.excludedBuiltinAgents,           -- array of strings, built-in agents to exclude
        sessionLimits                   = fields.sessionLimits,                   -- SessionLimitsConfig, per-session AI-credit limits
        memory                          = fields.memory,                         -- MemoryConfiguration, persistent session memory
        otlpProtocol                    = fields.otlpProtocol,                    -- string, OTLP telemetry protocol
        enableWebSocketResponses        = fields.enableWebSocketResponses,        -- boolean, stream responses over WebSocket
        expAssignments                  = fields.expAssignments,                  -- table, experiment assignments
        -- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk)
        rewindEnabled                   = fields.rewindEnabled,                   -- boolean, enable session rewind
        additionalDirectories           = fields.additionalDirectories,           -- array of strings, additional session directories
        disabledMcpServers              = fields.disabledMcpServers,              -- array of strings, MCP servers to disable
        githubMcpToolConfig             = fields.githubMcpToolConfig,             -- table, GitHub MCP tool configuration
        canvasProvider                  = fields.canvasProvider,                  -- table or string, canvas provider
        customAgentsLocalOnly           = fields.customAgentsLocalOnly,           -- boolean, restrict custom agents to local only
        builtinPluginDirectories        = fields.builtinPluginDirectories,        -- array of strings, built-in plugin directories
        argsSchema                      = fields.argsSchema,                       -- table, agent-factory authoring schema
        decisionContext                 = fields.decisionContext,                 -- table, permission decision context
        toolSearch                      = fields.toolSearch,                       -- table or boolean, tool search configuration
        inProcess                       = fields.inProcess,                        -- boolean, use in-process FFI transport
        experimentalMode                = fields.experimentalMode,                 -- boolean, enable experimental mode
        contentExclusion                = fields.contentExclusion,                 -- boolean, enable content exclusion
        onMcpAuthRequest                = fields.onMcpAuthRequest,                -- function(ProviderTokenArgs) -> string, MCP OAuth host token handler
        -- v2.6.0 upstream-sync options (parity with @github/copilot-sdk)
        autoTier                        = fields.autoTier,                        -- string (AutoTier), sent inside capi with enableWebSocketResponses
        askUserVariant                  = fields.askUserVariant,                  -- string (AskUserVariant), "legacy" or "elicitation"
        gitHubTokenProvider             = fields.gitHubTokenProvider,             -- function(GitHubTokenProviderArgs) -> GitHubTokenResult; exclusive with gitHubToken
        sandboxConfig                   = fields.sandboxConfig,                   -- SandboxConfig table
        featureFlags                    = fields.featureFlags,                    -- table of name -> boolean, sent even when empty
        allowedModels                   = fields.allowedModels,                   -- array of strings, model allowlist
        authClientIdMetadataUrl         = fields.authClientIdMetadataUrl,         -- string, OAuth client ID metadata document URL
        name                            = fields.name,                            -- string, session name (create only)
        agentDirectories                = fields.agentDirectories,                -- array of strings, custom agent directories
        hookDirectories                 = fields.hookDirectories,                 -- array of strings, hook directories
        interactiveUser                 = fields.interactiveUser,                 -- boolean, an interactive user is present
        excludedManagedSettings         = fields.excludedManagedSettings,         -- array of strings, managed settings to ignore
        enforceManagedModelDefaults     = fields.enforceManagedModelDefaults,     -- boolean, enforce managed model defaults
        trustWorkingDirectory           = fields.trustWorkingDirectory,           -- string (WorkingDirectoryTrust), "session" or "user" (create only)
        refreshCustomInstructions       = fields.refreshCustomInstructions,       -- boolean, refresh custom instructions (create only)
        imageGeneration                 = fields.imageGeneration,                 -- ImageGenerationConfig table
        autopilot                       = fields.autopilot,                       -- AutopilotOptions table
        -- v3.0.0 (SDK protocol 4) options
        providers                       = fields.providers,                       -- array of NamedProviderConfig, named BYOK connections
        models                          = fields.models,                          -- array of ProviderModelConfig, select with "provider/id"
        defaultProvider                 = fields.defaultProvider,                 -- string, provider serving unqualified model names
        capi                            = fields.capi,                            -- CapiConfig table, e.g. { enabled = false }
        onShellAuthenticationRequest    = fields.onShellAuthenticationRequest,    -- function(ShellAuthenticationRequest, ctx) -> ShellAuthenticationResponse
        storeMemory                     = fields.storeMemory,                     -- function(StoredMemory, ctx), memory store callback
        permissionMode                  = fields.permissionMode,                  -- string (PermissionMode)
    }
end

--- Create a ResumeSessionConfig table.
-- @param fields table
-- @return table ResumeSessionConfig
function M.ResumeSessionConfig(fields)
    fields = fields or {}
    return {
        model                           = fields.model,
        reasoningEffort                 = fields.reasoningEffort,
        tools                           = fields.tools,
        systemMessage                   = fields.systemMessage,
        availableTools                  = fields.availableTools,
        excludedTools                   = fields.excludedTools,
        provider                        = fields.provider,
        onPermissionRequest             = fields.onPermissionRequest,
        onUserInputRequest              = fields.onUserInputRequest,
        hooks                           = fields.hooks,
        workingDirectory                = fields.workingDirectory,
        configDir                       = fields.configDir,
        streaming                       = fields.streaming,
        disableResume                   = fields.disableResume,
        mcpServers                      = fields.mcpServers,
        customAgents                    = fields.customAgents,
        skillDirectories                = fields.skillDirectories,
        disabledSkills                  = fields.disabledSkills,
        infiniteSessions                = fields.infiniteSessions,
        modelCapabilities               = fields.modelCapabilities,               -- table, model capabilities overrides
        enableConfigDiscovery           = fields.enableConfigDiscovery,           -- boolean, auto-discover MCP server configs (default: false)
        includeSubAgentStreamingEvents  = fields.includeSubAgentStreamingEvents,  -- boolean, include sub-agent streaming events (default: true)
        -- GitHub token for authentication. Overrides client-level token for this session only.
        gitHubToken                     = fields.gitHubToken,
        commands                        = fields.commands,                        -- array of CommandDefinition
        onElicitationRequest            = fields.onElicitationRequest,            -- function(ElicitationContext) -> ElicitationResult
        instructionDirectories          = fields.instructionDirectories,          -- array of strings, instruction directories
        -- v2.6.0 upstream-sync options (parity with @github/copilot-sdk)
        enableWebSocketResponses        = fields.enableWebSocketResponses,        -- boolean, sent inside capi
        autoTier                        = fields.autoTier,                        -- string (AutoTier), sent inside capi
        askUserVariant                  = fields.askUserVariant,                  -- string (AskUserVariant)
        gitHubTokenProvider             = fields.gitHubTokenProvider,             -- function(GitHubTokenProviderArgs) -> GitHubTokenResult; exclusive with gitHubToken
        sandboxConfig                   = fields.sandboxConfig,                   -- SandboxConfig table
        featureFlags                    = fields.featureFlags,                    -- table of name -> boolean, sent even when empty
        allowedModels                   = fields.allowedModels,                   -- array of strings, model allowlist
        authClientIdMetadataUrl         = fields.authClientIdMetadataUrl,         -- string, OAuth client ID metadata document URL
        agentDirectories                = fields.agentDirectories,                -- array of strings, custom agent directories
        hookDirectories                 = fields.hookDirectories,                 -- array of strings, hook directories
        interactiveUser                 = fields.interactiveUser,                 -- boolean
        excludedManagedSettings         = fields.excludedManagedSettings,         -- array of strings
        enforceManagedModelDefaults     = fields.enforceManagedModelDefaults,     -- boolean
        allowTranscriptRecovery         = fields.allowTranscriptRecovery,         -- boolean, recover from the transcript (resume only)
        imageGeneration                 = fields.imageGeneration,                 -- ImageGenerationConfig table
        autopilot                       = fields.autopilot,                       -- AutopilotOptions table
        -- v3.0.0 (SDK protocol 4) options
        providers                       = fields.providers,                       -- array of NamedProviderConfig
        models                          = fields.models,                          -- array of ProviderModelConfig
        defaultProvider                 = fields.defaultProvider,                 -- string
        clearDefaultProvider            = fields.clearDefaultProvider,            -- boolean, send defaultProvider = null to clear it
        capi                            = fields.capi,                            -- CapiConfig table
        onShellAuthenticationRequest    = fields.onShellAuthenticationRequest,    -- function(ShellAuthenticationRequest, ctx)
        storeMemory                     = fields.storeMemory,                     -- function(StoredMemory, ctx)
        permissionMode                  = fields.permissionMode,                  -- string (PermissionMode)
    }
end

--- Create a MessageOptions table.
-- @param fields table with keys: prompt, attachments, mode, agentMode, displayPrompt, responseFormat, imageOptions, requestHeaders, source, responseSchema, clientTurnId
-- @return table MessageOptions
function M.MessageOptions(fields)
    fields = fields or {}
    return {
        prompt         = fields.prompt or "",
        attachments    = fields.attachments,      -- optional array
        mode           = fields.mode,             -- optional string
        agentMode      = fields.agentMode,        -- optional string, agent-mode override for this message
        displayPrompt  = fields.displayPrompt,    -- optional string, alternate prompt text shown to the user
        responseFormat = fields.responseFormat,    -- optional string (ResponseFormat constant)
        imageOptions   = fields.imageOptions,     -- optional table (from image_options())
        requestHeaders = fields.requestHeaders,   -- optional table of string->string, custom HTTP headers
        source         = fields.source,           -- optional string (MessageSource), "user", "system" or "agent-<id>"
        responseSchema = fields.responseSchema,   -- optional table, JSON Schema for structured output (overrides responseFormat)
        clientTurnId   = fields.clientTurnId,     -- optional string, opaque turn id echoed on the turn's events
    }
end

--- Create a PingResponse table.
-- @param fields table with keys: message, timestamp, protocolVersion
-- @return table PingResponse
function M.PingResponse(fields)
    fields = fields or {}
    return {
        message         = fields.message or "",
        timestamp       = fields.timestamp or 0,
        protocolVersion = fields.protocolVersion,   -- optional number
    }
end

--- Create a GetStatusResponse table.
-- @param fields table with keys: version, protocolVersion
-- @return table GetStatusResponse
function M.GetStatusResponse(fields)
    fields = fields or {}
    return {
        version         = fields.version or "",
        protocolVersion = fields.protocolVersion or 0,
    }
end

--- Create a GetAuthStatusResponse table.
-- @param fields table
-- @return table GetAuthStatusResponse
function M.GetAuthStatusResponse(fields)
    fields = fields or {}
    return {
        isAuthenticated = fields.isAuthenticated or false,
        authType        = fields.authType,
        host            = fields.host,
        login           = fields.login,
        statusMessage   = fields.statusMessage,
    }
end

--- Create a ModelInfo table.
-- @param fields table
-- @return table ModelInfo
function M.ModelInfo(fields)
    fields = fields or {}
    return {
        id                        = fields.id or "",
        name                      = fields.name or "",
        capabilities              = fields.capabilities or {},
        policy                    = fields.policy,
        billing                   = fields.billing,
        supportedReasoningEfforts = fields.supportedReasoningEfforts,
        defaultReasoningEffort    = fields.defaultReasoningEffort,
    }
end

--- Create a SessionMetadata table.
-- @param fields table
-- @return table SessionMetadata
function M.SessionMetadata(fields)
    fields = fields or {}
    return {
        sessionId    = fields.sessionId or "",
        startTime    = fields.startTime or "",
        modifiedTime = fields.modifiedTime or "",
        summary      = fields.summary,
        isRemote     = fields.isRemote or false,
    }
end

--- Create a PermissionRequest table.
-- @param fields table with keys: kind, toolCallId, extra
-- @return table PermissionRequest
function M.PermissionRequest(fields)
    fields = fields or {}
    return {
        kind       = fields.kind or "",
        toolCallId = fields.toolCallId,
        extra      = fields.extra,
    }
end

--- Create a PermissionRequestResult table.
-- @param fields table with keys: kind, rules
-- @return table PermissionRequestResult
function M.PermissionRequestResult(fields)
    fields = fields or {}
    return {
        kind  = fields.kind or "",
        rules = fields.rules,
    }
end

--- Create a UserInputRequest table.
-- @param fields table with keys: question, choices, allowFreeform
-- @return table UserInputRequest
function M.UserInputRequest(fields)
    fields = fields or {}
    return {
        question      = fields.question or "",
        choices       = fields.choices,
        allowFreeform = fields.allowFreeform,
    }
end

--- Create a UserInputResponse table.
-- @param fields table with keys: answer, wasFreeform
-- @return table UserInputResponse
function M.UserInputResponse(fields)
    fields = fields or {}
    return {
        answer      = fields.answer or "",
        wasFreeform = fields.wasFreeform or false,
    }
end

--- Create a SessionHooks table.
-- onSubagentStart ("subagentStart") receives the base hook fields plus transcriptPath,
-- agentName, agentDisplayName, agentDescription and may return { additionalContext = ... }.
-- onSubagentStop ("subagentStop") also receives agentId, agentType, stopReason ("end_turn")
-- and response, and may return { decision = "block"|"allow", reason = ..., modifiedResponse = ... }.
-- @param fields table with optional function keys
-- @return table SessionHooks
function M.SessionHooks(fields)
    fields = fields or {}
    return {
        onPreToolUse          = fields.onPreToolUse,
        onPostToolUse         = fields.onPostToolUse,
        onUserPromptSubmitted = fields.onUserPromptSubmitted,
        onUserPromptTransformed = fields.onUserPromptTransformed,
        onSessionStart        = fields.onSessionStart,
        onSessionEnd          = fields.onSessionEnd,
        onErrorOccurred       = fields.onErrorOccurred,
        onPreMcpToolCall      = fields.onPreMcpToolCall,  -- function, pre-MCP-tool-call hook
        onSubagentStart       = fields.onSubagentStart,   -- function, subagent start hook
        onSubagentStop        = fields.onSubagentStop,    -- function, subagent stop hook
    }
end

--- Create a PreToolUseHookInput table.
-- @param fields table
-- @return table
function M.PreToolUseHookInput(fields)
    fields = fields or {}
    return {
        timestamp = fields.timestamp or 0,
        cwd       = fields.cwd or "",
        toolName  = fields.toolName or "",
        toolArgs  = fields.toolArgs,
    }
end

--- Create a PreToolUseHookOutput table.
-- @param fields table
-- @return table
function M.PreToolUseHookOutput(fields)
    fields = fields or {}
    return {
        permissionDecision       = fields.permissionDecision,
        permissionDecisionReason = fields.permissionDecisionReason,
        modifiedArgs             = fields.modifiedArgs,
        additionalContext        = fields.additionalContext,
        suppressOutput           = fields.suppressOutput,
    }
end

--- Create a PostToolUseHookInput table.
-- @param fields table
-- @return table
function M.PostToolUseHookInput(fields)
    fields = fields or {}
    return {
        timestamp  = fields.timestamp or 0,
        cwd        = fields.cwd or "",
        toolName   = fields.toolName or "",
        toolArgs   = fields.toolArgs,
        toolResult = fields.toolResult,
    }
end

--- Create a PostToolUseHookOutput table.
-- @param fields table
-- @return table
function M.PostToolUseHookOutput(fields)
    fields = fields or {}
    return {
        modifiedResult    = fields.modifiedResult,
        additionalContext = fields.additionalContext,
        suppressOutput    = fields.suppressOutput,
    }
end

--- Create a ProviderConfig table.
-- @param fields table with keys: type, wireApi, baseUrl, apiKey, bearerToken, azure, modelProvider, apiKeyCommand
-- @return table ProviderConfig
function M.ProviderConfig(fields)
    fields = fields or {}
    return {
        type          = fields.type,
        wireApi       = fields.wireApi,
        baseUrl       = fields.baseUrl or "",
        apiKey        = fields.apiKey,
        bearerToken   = fields.bearerToken,
        azure         = fields.azure,
        modelProvider = fields.modelProvider,   -- optional string (ModelProvider), e.g. "ollama"
        apiKeyCommand = fields.apiKeyCommand,   -- optional string, command whose stdout is the API key
    }
end

--- Create an InfiniteSessionConfig table.
-- @param fields table with optional keys: enabled, backgroundCompactionThreshold, bufferExhaustionThreshold
-- @return table InfiniteSessionConfig
function M.InfiniteSessionConfig(fields)
    fields = fields or {}
    return {
        enabled                      = fields.enabled,
        backgroundCompactionThreshold = fields.backgroundCompactionThreshold,
        bufferExhaustionThreshold     = fields.bufferExhaustionThreshold,
    }
end

--- Create a CustomAgentConfig table.
-- @param fields table with keys: name, description, prompt, skills
-- @return table CustomAgentConfig
function M.CustomAgentConfig(fields)
    fields = fields or {}
    return {
        name        = fields.name or "",
        description = fields.description,       -- optional string
        prompt      = fields.prompt,            -- optional string
        skills      = fields.skills,            -- optional array of skill names to preload
    }
end

--- Create a SessionLifecycleEvent table.
-- @param fields table with keys: type, sessionId, metadata
-- @return table SessionLifecycleEvent
function M.SessionLifecycleEvent(fields)
    fields = fields or {}
    return {
        type      = fields.type or "",
        sessionId = fields.sessionId or "",
        metadata  = fields.metadata,
    }
end

--- Create an Attachment table.
-- @param fields table with keys: displayName, path, type, filePath, selection, text
-- @return table Attachment
function M.Attachment(fields)
    fields = fields or {}
    return {
        displayName = fields.displayName or "",
        path        = fields.path,
        type        = fields.type or "file",
        filePath    = fields.filePath,
        selection   = fields.selection,
        text        = fields.text,
    }
end

-- ---------------------------------------------------------------------------
-- Response format constants
-- ---------------------------------------------------------------------------

M.ResponseFormat = {
    TEXT        = "text",
    IMAGE       = "image",
    JSON_OBJECT = "json_object",
}

-- ---------------------------------------------------------------------------
-- Image generation helpers
-- ---------------------------------------------------------------------------

--- Create image options for image generation.
-- @param opts table with optional fields: size, quality, style
-- @return table image options
function M.image_options(opts)
    opts = opts or {}
    return {
        size    = opts.size,
        quality = opts.quality,
        style   = opts.style,
    }
end

--- Parse assistant image data from a response.
-- @param data table with format, base64, url, revisedPrompt, width, height
-- @return table parsed image data
function M.parse_assistant_image_data(data)
    return {
        format        = data.format,
        base64        = data.base64,
        url           = data.url,
        revisedPrompt = data.revisedPrompt,
        width         = data.width,
        height        = data.height,
    }
end

--- Parse a content block from a mixed response.
-- @param block table with type and content fields
-- @return table parsed content block
function M.parse_content_block(block)
    if block.type == "image" and block.image then
        block.image = M.parse_assistant_image_data(block.image)
    end
    return block
end

-- ---------------------------------------------------------------------------
-- Session Filesystem Types
-- ---------------------------------------------------------------------------

--- Create a SessionFsConfig table.
-- @param fields table with keys: initialCwd, sessionStatePath, conventions
-- @return table SessionFsConfig
function M.SessionFsConfig(fields)
    fields = fields or {}
    return {
        initialCwd       = fields.initialCwd or "",
        sessionStatePath = fields.sessionStatePath or "",
        conventions      = fields.conventions or "posix",
    }
end

--- Create a SessionFsFileInfo table.
-- @param fields table with keys: name, size, isDirectory, isFile, createdAt, modifiedAt
-- @return table SessionFsFileInfo
function M.SessionFsFileInfo(fields)
    fields = fields or {}
    return {
        name        = fields.name or "",
        size        = fields.size or 0,
        isDirectory = fields.isDirectory or false,
        isFile      = fields.isFile or false,
        createdAt   = fields.createdAt,     -- optional string (ISO 8601)
        modifiedAt  = fields.modifiedAt,    -- optional string (ISO 8601)
    }
end

--- Session Filesystem Provider convention.
-- To implement a session filesystem provider in Lua, create a table with
-- the following function fields:
--   readFile(sessionId, path) → string
--   writeFile(sessionId, path, content) → nil
--   appendFile(sessionId, path, content) → nil
--   exists(sessionId, path) → boolean
--   stat(sessionId, path) → SessionFsFileInfo table
--   mkdir(sessionId, path, recursive) → nil
--   readdir(sessionId, path) → array of strings
--   readdirWithTypes(sessionId, path) → array of SessionFsFileInfo tables
--   rm(sessionId, path, recursive) → nil
--   rename(sessionId, oldPath, newPath) → nil
--
-- Each function should return nil, err_string on error.

-- ---------------------------------------------------------------------------
-- Upstream-sync feature types (parity with @github/copilot-sdk)
-- ---------------------------------------------------------------------------

--- Create a SessionLimitsConfig table (per-session AI-credit budget).
-- @param fields table with keys: maxAiCredits
-- @return table SessionLimitsConfig
function M.SessionLimitsConfig(fields)
    fields = fields or {}
    return {
        maxAiCredits = fields.maxAiCredits,   -- optional integer, cap on AI credits spent
    }
end

--- Create a MemoryConfiguration table (opt-in persistent session memory).
-- @param fields table with keys: enabled
-- @return table MemoryConfiguration
function M.MemoryConfiguration(fields)
    fields = fields or {}
    return {
        enabled = fields.enabled or false,    -- boolean, whether persistent memory is enabled
    }
end

--- Create a ProviderTokenArgs table (passed to a BYOK bearer_token_provider).
-- @param fields table with keys: sessionId
-- @return table ProviderTokenArgs
function M.ProviderTokenArgs(fields)
    fields = fields or {}
    return {
        sessionId = fields.sessionId or "",   -- the session the token is requested for
    }
end

--- Create a CopilotRequestHandler table.
-- Intercepts outbound LLM inference HTTP/WebSocket requests. Assign to
-- ClientOptions.requestHandler. Override send_request to mutate, replace, or
-- forward the request. BYOK providers may also set ClientOptions.bearerTokenProvider.
-- @param fields table with optional key: send_request (function(request, context) -> request)
-- @return table CopilotRequestHandler
function M.CopilotRequestHandler(fields)
    fields = fields or {}
    return {
        send_request = fields.send_request or function(request, _context) return request end,
    }
end

--- Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto").
M.ToolDefer = {
    AUTO  = "auto",
    NEVER = "never",
}

--- System-message section identifiers (used with system-message overrides).
-- The "preamble" section targets the identity preamble; the "preserve" action
-- protects an individually-addressable section from a group-level remove.
M.SystemMessageSection = {
    PREAMBLE          = "preamble",
    IDENTITY          = "identity",
    TOOL_INSTRUCTIONS = "tool_instructions",
    PRESERVE          = "preserve",
}

--- GitHub-anchored attachment variants.
M.GitHubAttachment = {
    GITHUB_COMMIT          = "GitHubCommit",
    GITHUB_RELEASE         = "GitHubRelease",
    GITHUB_ACTIONS_JOB     = "GitHubActionsJob",
    GITHUB_REPOSITORY      = "GitHubRepository",
    GITHUB_FILE_DIFF       = "GitHubFileDiff",
    GITHUB_TREE_COMPARISON = "GitHubTreeComparison",
    GITHUB_URL             = "GitHubUrl",
    GITHUB_FILE            = "GitHubFile",
    GITHUB_SNIPPET         = "GitHubSnippet",
}

-- ---------------------------------------------------------------------------
-- v2.6.0 feature types (parity with @github/copilot-sdk)
-- ---------------------------------------------------------------------------

--- Create a ClientInfo table: identity of the integrating application, sent on
-- the connect handshake (as editorName / editorVersion / extensionName /
-- extensionVersion) so the runtime attributes its telemetry to it.
-- @param fields table with keys: applicationName, applicationVersion, integrationName, integrationVersion
-- @return table ClientInfo
function M.ClientInfo(fields)
    fields = fields or {}
    return {
        applicationName    = fields.applicationName,
        applicationVersion = fields.applicationVersion,
        integrationName    = fields.integrationName,
        integrationVersion = fields.integrationVersion,
    }
end

--- Convert a ClientInfo table to its wire shape.
-- @param info table|nil ClientInfo
-- @return table|nil Wire table with only non-empty fields, or nil when all are empty
function M.client_info_to_wire(info)
    if type(info) ~= "table" then return nil end
    local wire, any = {}, false
    local mapping = {
        { "applicationName", "editorName" },
        { "applicationVersion", "editorVersion" },
        { "integrationName", "extensionName" },
        { "integrationVersion", "extensionVersion" },
    }
    for _, pair in ipairs(mapping) do
        local value = info[pair[1]]
        if type(value) == "string" and value ~= "" then
            wire[pair[2]] = value
            any = true
        end
    end
    if any then return wire end
    return nil
end

--- Auto routing preference used when the session model is "auto" (experimental).
M.AutoTier = {
    EFFICIENCY   = "efficiency",
    BALANCE      = "balance",
    INTELLIGENCE = "intelligence",
    FAST         = "fast",
}

--- Wire shape used for ask-user prompts.
M.AskUserVariant = {
    LEGACY      = "legacy",
    ELICITATION = "elicitation",
}

--- Scope of the working-directory trust granted at session creation.
M.WorkingDirectoryTrust = {
    SESSION = "session",
    USER    = "user",
}

--- BYOK provider families accepted by ProviderConfig.modelProvider.
M.ModelProvider = {
    OPENAI        = "openai",
    ANTHROPIC     = "anthropic",
    AZURE_OPENAI  = "azure_openai",
    OLLAMA        = "ollama",
    LM_STUDIO     = "lm_studio",
    FOUNDRY_LOCAL = "foundry_local",
    LLAMA_CPP     = "llama_cpp",
}

--- Message provenance for MessageOptions.source.
M.MessageSource = {
    USER   = "user",
    SYSTEM = "system",
}

--- Build the "agent-<id>" message source.
-- @param agent_id string
-- @return string
function M.MessageSource.agent(agent_id)
    return "agent-" .. tostring(agent_id)
end

--- Result helpers for a session gitHubTokenProvider, which receives
-- { host, sessionId, reason } and returns one of these tables.
M.GitHubTokenResult = {}

--- A token result. expiresIn is the remaining lifetime in seconds.
function M.GitHubTokenResult.token(access_token, expires_in, token_type)
    return { kind = "token", accessToken = access_token, expiresIn = expires_in, tokenType = token_type }
end

--- A cancellation result.
function M.GitHubTokenResult.cancelled()
    return { kind = "cancelled" }
end

--- Create a SandboxConfig table. allowBypass opts in to letting the agent request
-- that an individual command run outside the sandbox through the normal permission flow.
-- @param fields table with keys: enabled, userPolicy, addCurrentWorkingDirectory,
--   sandboxMcpServers, sandboxLspServers, sandboxExtensions, allowBypass
-- @return table SandboxConfig
function M.SandboxConfig(fields)
    fields = fields or {}
    return {
        enabled                    = fields.enabled and true or false,
        userPolicy                 = fields.userPolicy,
        addCurrentWorkingDirectory = fields.addCurrentWorkingDirectory,
        sandboxMcpServers          = fields.sandboxMcpServers,
        sandboxLspServers          = fields.sandboxLspServers,
        sandboxExtensions          = fields.sandboxExtensions,
        allowBypass                = fields.allowBypass,
    }
end

--- Create an ImageGenerationConfig table (session-level image generation).
-- @param fields table with keys: enabled, watermark, saveToDisk
-- @return table ImageGenerationConfig
function M.ImageGenerationConfig(fields)
    fields = fields or {}
    return {
        enabled    = fields.enabled,
        watermark  = fields.watermark,
        saveToDisk = fields.saveToDisk,
    }
end

--- Create an AutopilotOptions table (runtime-owned Autopilot continuation options).
-- @param fields table with keys: continuationEnabled, maxContinues
-- @return table AutopilotOptions
function M.AutopilotOptions(fields)
    fields = fields or {}
    return {
        continuationEnabled = fields.continuationEnabled,
        maxContinues        = fields.maxContinues,
    }
end

--- Create limit overrides for SessionConfig.modelCapabilities. The wire keys are
-- snake_case: max_prompt_tokens, max_output_tokens, max_context_window_tokens.
-- @param fields table with keys: maxPromptTokens, maxOutputTokens, maxContextWindowTokens
-- @return table
function M.ModelLimitsOverride(fields)
    fields = fields or {}
    return {
        max_prompt_tokens         = fields.maxPromptTokens,
        max_output_tokens         = fields.maxOutputTokens,
        max_context_window_tokens = fields.maxContextWindowTokens,
    }
end

--- Create per-property model capability overrides (SessionConfig.modelCapabilities).
-- @param fields table with keys: supports, limits (see ModelLimitsOverride)
-- @return table
function M.ModelCapabilitiesOverride(fields)
    fields = fields or {}
    return {
        supports = fields.supports,
        limits   = fields.limits,
    }
end

-- ---------------------------------------------------------------------------
-- v3.0.0 feature types (SDK protocol 4, parity with the Microsoft Copilot SDK 2.0)
-- ---------------------------------------------------------------------------

--- Client mode (ClientOptions.mode). "coding-agent" (the default) keeps the
-- CLI-like coding tools and defaults. "empty" requires every session to set
-- availableTools and applies conservative session defaults (no telemetry,
-- in-memory OAuth and embedding storage, no cross-session store or memory, no
-- on-demand instruction discovery, repository hooks or host git operations).
-- "copilot-cli" is a legacy alias of "coding-agent".
M.ClientMode = {
    CODING_AGENT = "coding-agent",
    EMPTY        = "empty",
    COPILOT_CLI  = "copilot-cli",
}

--- Create a NamedProviderConfig table (SessionConfig.providers). Select a
-- registered model with "provider/id" or set defaultProvider to route unqualified
-- model names through the connection.
-- @param fields table with keys: name, type, baseUrl, apiKey, apiKeyCommand,
--   bearerToken, wireApi, transport, azure, headers, modelProvider
-- @return table NamedProviderConfig
function M.NamedProviderConfig(fields)
    fields = fields or {}
    return {
        name          = fields.name,
        type          = fields.type,
        baseUrl       = fields.baseUrl,
        apiKey        = fields.apiKey,
        apiKeyCommand = fields.apiKeyCommand,
        bearerToken   = fields.bearerToken,
        wireApi       = fields.wireApi,
        transport     = fields.transport,
        azure         = fields.azure,          -- optional table, e.g. { apiVersion = "2024-10-21" }
        headers       = fields.headers,
        modelProvider = fields.modelProvider,
    }
end

--- Create a ProviderModelConfig table (SessionConfig.models). The canonical
-- selection id is "provider/id"; modelId is the known base model and wireModel
-- the name sent to the provider API (both default to id).
-- @param fields table with keys: provider, id, modelId, wireModel, name,
--   maxPromptTokens, maxContextWindowTokens, maxOutputTokens, capabilities
-- @return table ProviderModelConfig
function M.ProviderModelConfig(fields)
    fields = fields or {}
    return {
        provider               = fields.provider,
        id                     = fields.id,
        modelId                = fields.modelId,
        wireModel              = fields.wireModel,
        name                   = fields.name,
        maxPromptTokens        = fields.maxPromptTokens,
        maxContextWindowTokens = fields.maxContextWindowTokens,
        maxOutputTokens        = fields.maxOutputTokens,
        capabilities           = fields.capabilities,
    }
end

--- Create a CapiConfig table (SessionConfig.capi). Set enabled = false for a
-- BYOK-only model catalog.
-- @param fields table with key: enabled
-- @return table CapiConfig
function M.CapiConfig(fields)
    fields = fields or {}
    return { enabled = fields.enabled }
end

--- Parse the data of a live "tool.shell_output" event
-- (SessionEventType.TOOL_SHELL_OUTPUT).
-- @param data table Event data
-- @return table { toolCallId, text, sequence, stream } where stream is
--   "stdout", "stderr" or nil (stdout)
function M.ShellOutputData(data)
    data = data or {}
    local stream = data.stream
    if type(stream) ~= "string" then stream = nil end
    return {
        toolCallId = data.toolCallId,
        text       = data.text or "",
        sequence   = data.sequence or 0,
        stream     = stream,
    }
end

--- Input kinds of a shell authentication request.
M.ShellAuthenticationInputKind = {
    SECRET  = "secret",
    TEXT    = "text",
    CONFIRM = "confirm",
}

--- Answers to a shell authentication request (SessionConfig.onShellAuthenticationRequest).
M.ShellAuthenticationResponse = {}

--- Accept the prompt with a value.
function M.ShellAuthenticationResponse.accept(value)
    return { action = "accept", value = value }
end

--- Cancel the prompt.
function M.ShellAuthenticationResponse.cancel()
    return { action = "cancel" }
end

--- Traffic served by the NetworkConfig callbacks.
M.NetworkScope = {
    CONNECTION = "connection",
    RUNTIME    = "runtime",
}

--- Create a NetworkConfig table (ClientOptions.network). Each callback is
-- optional: resolveProxy({ url }) returns a PAC-style proxy list string (or nil to
-- keep the runtime's own resolution); authenticateNegotiate({ spn }) returns a
-- base64 SPNEGO token or nil; authenticateBasic({ proxy, realm, previousRejected })
-- returns { username, password } or nil. Tokens and passwords are never logged.
-- @param fields table with keys: scope, resolveProxy, authenticateNegotiate, authenticateBasic
-- @return table NetworkConfig
function M.NetworkConfig(fields)
    fields = fields or {}
    return {
        scope                 = fields.scope,
        resolveProxy          = fields.resolveProxy,
        authenticateNegotiate = fields.authenticateNegotiate,
        authenticateBasic     = fields.authenticateBasic,
    }
end

--- Parse a structured Policy Helper failure.
-- Helper-authored text is untrusted: render it as plain text and never execute it.
-- @param data table Wire PolicyHelperFailure
-- @return table { category, failureMode, message, previousPolicyRetained, reason,
--   authority, code, details, diagnostic, helpLink, remediation, severity }
function M.PolicyHelperFailure(data)
    data = data or {}
    local function opt(value)
        if value == nil or value == require("cjson").null then return nil end
        return value
    end
    return {
        category               = opt(data.category),
        failureMode            = opt(data.failureMode),
        message                = opt(data.message),
        previousPolicyRetained = data.previousPolicyRetained == true,
        reason                 = opt(data.reason),
        authority              = opt(data.authority),
        code                   = opt(data.code),
        details                = opt(data.details),
        diagnostic             = opt(data.diagnostic),
        helpLink               = opt(data.helpLink),
        remediation            = opt(data.remediation),
        severity               = opt(data.severity),
    }
end

--- Extract the Policy Helper failure from a JSON-RPC error table (the third value
-- returned by create_session / resume_session when they fail).
-- @param rpc_error table|nil { code, message, data }
-- @return table|nil PolicyHelperFailure
function M.policy_helper_failure(rpc_error)
    if type(rpc_error) ~= "table" or type(rpc_error.data) ~= "table" then
        return nil
    end
    local failure = rpc_error.data.policyHelperFailure
    if type(failure) ~= "table" then
        return nil
    end
    return M.PolicyHelperFailure(failure)
end

--- Session permission modes (SessionConfig.permissionMode, session:set_permission_mode).
M.PermissionMode = {
    MANUAL    = "manual",
    ASSISTED  = "assisted",
    ALLOW_ALL = "allow-all",
}

--- Known reasons a permission-mode change was refused.
M.PermissionModeRefusalReason = {
    MANAGED_POLICY = "managed_policy",
}

--- Parse the result of session:set_permission_mode().
-- @param data table Wire result
-- @return table { success, mode, reason } where reason explains a refusal when
--   known (PermissionModeRefusalReason.MANAGED_POLICY)
function M.PermissionModeSetResult(data)
    data = data or {}
    local reason = data.reason
    if type(reason) ~= "string" then reason = nil end
    local mode = data.mode
    if type(mode) ~= "string" then mode = nil end
    return {
        success = data.success == true,
        mode    = mode,
        reason  = reason,
    }
end

return M

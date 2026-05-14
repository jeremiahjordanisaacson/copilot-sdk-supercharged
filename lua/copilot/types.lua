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
-- Slash command input completion constants
-- ---------------------------------------------------------------------------

M.SlashCommandInputCompletion = {
    DIRECTORY = "directory",
}

-- ---------------------------------------------------------------------------
-- Slash command kind constants
-- ---------------------------------------------------------------------------

M.SlashCommandKind = {
    BUILTIN = "builtin",
    CLIENT  = "client",
    SKILL   = "skill",
}

-- ---------------------------------------------------------------------------
-- Model picker price category constants
-- ---------------------------------------------------------------------------

M.ModelPickerPriceCategory = {
    HIGH      = "high",
    LOW       = "low",
    MEDIUM    = "medium",
    VERY_HIGH = "very_high",
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
-- @param fields table with keys: name, description, parameters, handler
-- @return table Tool
function M.Tool(fields)
    fields = fields or {}
    return {
        name        = fields.name or "",
        description = fields.description or "",
        parameters  = fields.parameters,    -- optional table (JSON Schema)
        handler     = fields.handler,       -- function(invocation) -> ToolResult, err
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
        onGetTraceContext          = fields.onGetTraceContext,          -- optional function() -> TraceContext
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

--- Create a ModelCapabilitiesOverride table.
-- @param fields table with optional keys: supports, limits
-- @return table ModelCapabilitiesOverride
function M.ModelCapabilitiesOverride(fields)
    fields = fields or {}
    return {
        supports = fields.supports,    -- optional table { vision = bool, reasoningEffort = bool }
        limits   = fields.limits,      -- optional table { maxPromptTokens = number, maxContextWindowTokens = number, vision = table }
    }
end

--- Create a ModelCapabilitiesSupports table.
-- @param fields table with optional keys: vision, reasoningEffort
-- @return table
function M.ModelCapabilitiesSupports(fields)
    fields = fields or {}
    return {
        vision          = fields.vision,
        reasoningEffort = fields.reasoningEffort,
    }
end

--- Create a ModelCapabilitiesLimits table.
-- @param fields table with optional keys: maxPromptTokens, maxContextWindowTokens, vision
-- @return table
function M.ModelCapabilitiesLimits(fields)
    fields = fields or {}
    return {
        maxPromptTokens        = fields.maxPromptTokens,
        maxContextWindowTokens = fields.maxContextWindowTokens,
        vision                 = fields.vision,  -- optional ModelCapabilitiesVisionLimits
    }
end

--- Create a ModelCapabilitiesVisionLimits table.
-- @param fields table with optional keys: supportedMediaTypes, maxPromptImages, maxPromptImageSize
-- @return table
function M.ModelCapabilitiesVisionLimits(fields)
    fields = fields or {}
    return {
        supportedMediaTypes = fields.supportedMediaTypes,
        maxPromptImages     = fields.maxPromptImages,
        maxPromptImageSize  = fields.maxPromptImageSize,
    }
end

--- Create an ExitPlanModeRequest table.
-- @param fields table with keys: sessionId
-- @return table ExitPlanModeRequest
function M.ExitPlanModeRequest(fields)
    fields = fields or {}
    return {
        sessionId = fields.sessionId or "",
    }
end

--- Create an ExitPlanModeResponse table.
-- @param fields table with keys: approved
-- @return table ExitPlanModeResponse
function M.ExitPlanModeResponse(fields)
    fields = fields or {}
    return {
        approved = fields.approved ~= false,  -- default true
    }
end

--- Create a TraceContext table.
-- @param fields table with optional keys: traceparent, tracestate
-- @return table TraceContext
function M.TraceContext(fields)
    fields = fields or {}
    return {
        traceparent = fields.traceparent,  -- optional string
        tracestate  = fields.tracestate,   -- optional string
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
        onExitPlanMode                  = fields.onExitPlanMode,                  -- function(ExitPlanModeRequest) -> ExitPlanModeResponse
        instructionDirectories          = fields.instructionDirectories,          -- array of strings, instruction directories
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
        onExitPlanMode                  = fields.onExitPlanMode,                  -- function(ExitPlanModeRequest) -> ExitPlanModeResponse
        instructionDirectories          = fields.instructionDirectories,          -- array of strings, instruction directories
    }
end

--- Create a MessageOptions table.
-- @param fields table with keys: prompt, attachments, mode, responseFormat, imageOptions, requestHeaders
-- @return table MessageOptions
function M.MessageOptions(fields)
    fields = fields or {}
    return {
        prompt         = fields.prompt or "",
        attachments    = fields.attachments,      -- optional array
        mode           = fields.mode,             -- optional string
        responseFormat = fields.responseFormat,    -- optional string (ResponseFormat constant)
        imageOptions   = fields.imageOptions,     -- optional table (from image_options())
        requestHeaders = fields.requestHeaders,   -- optional table of string->string, custom HTTP headers
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

--- Create a SlashCommandInput table.
-- @param fields table with keys: hint, completion
-- @return table SlashCommandInput
function M.SlashCommandInput(fields)
    fields = fields or {}
    return {
        hint       = fields.hint or "",
        completion = fields.completion,   -- optional string (SlashCommandInputCompletion)
    }
end

--- Create a SlashCommandInfo table.
-- @param fields table
-- @return table SlashCommandInfo
function M.SlashCommandInfo(fields)
    fields = fields or {}
    return {
        allowDuringAgentExecution = fields.allowDuringAgentExecution or false,
        description               = fields.description or "",
        kind                      = fields.kind or "builtin",
        name                      = fields.name or "",
        aliases                   = fields.aliases,       -- optional array of strings
        experimental              = fields.experimental,  -- optional boolean
        input                     = fields.input,         -- optional SlashCommandInput table
    }
end

--- Create a CommandsInvokeRequest table.
-- @param fields table with keys: name, input
-- @return table CommandsInvokeRequest
function M.CommandsInvokeRequest(fields)
    fields = fields or {}
    return {
        name  = fields.name or "",
        input = fields.input,   -- optional string
    }
end

--- Create a CommandsListRequest table.
-- @param fields table with keys: includeBuiltins, includeClientCommands, includeSkills
-- @return table CommandsListRequest
function M.CommandsListRequest(fields)
    fields = fields or {}
    return {
        includeBuiltins       = fields.includeBuiltins,        -- optional boolean
        includeClientCommands = fields.includeClientCommands,   -- optional boolean
        includeSkills         = fields.includeSkills,           -- optional boolean
    }
end

--- Create a ModelBillingTokenPrices table.
-- @param fields table with keys: batchSize, cachePrice, inputPrice, outputPrice
-- @return table ModelBillingTokenPrices
function M.ModelBillingTokenPrices(fields)
    fields = fields or {}
    return {
        batchSize   = fields.batchSize,    -- optional integer
        cachePrice  = fields.cachePrice,   -- optional integer
        inputPrice  = fields.inputPrice,   -- optional integer
        outputPrice = fields.outputPrice,  -- optional integer
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
-- @param fields table with optional function keys
-- @return table SessionHooks
function M.SessionHooks(fields)
    fields = fields or {}
    return {
        onPreToolUse          = fields.onPreToolUse,
        onPostToolUse         = fields.onPostToolUse,
        onUserPromptSubmitted = fields.onUserPromptSubmitted,
        onSessionStart        = fields.onSessionStart,
        onSessionEnd          = fields.onSessionEnd,
        onErrorOccurred       = fields.onErrorOccurred,
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
-- @param fields table with keys: type, wireApi, baseUrl, apiKey, bearerToken, azure
-- @return table ProviderConfig
function M.ProviderConfig(fields)
    fields = fields or {}
    return {
        type        = fields.type,
        wireApi     = fields.wireApi,
        baseUrl     = fields.baseUrl or "",
        apiKey      = fields.apiKey,
        bearerToken = fields.bearerToken,
        azure       = fields.azure,
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

--- Experimental: Create a SkillsLoadDiagnostics table.
-- @param fields table with keys: errors, warnings
-- @return table SkillsLoadDiagnostics
function M.SkillsLoadDiagnostics(fields)
    fields = fields or {}
    return {
        errors   = fields.errors or {},
        warnings = fields.warnings or {},
    }
end

-- ---------------------------------------------------------------------------
-- Remote session mode constants
-- ---------------------------------------------------------------------------

--- Per-session remote mode. "off" disables remote, "export" exports session
--- events to Mission Control without enabling remote steering, "on" enables
--- both export and remote steering.
M.RemoteSessionMode = {
    EXPORT = "export",
    OFF    = "off",
    ON     = "on",
}

--- Experimental: Create a RemoteEnableRequest table.
-- @param fields table with keys: mode
-- @return table RemoteEnableRequest
function M.RemoteEnableRequest(fields)
    fields = fields or {}
    return {
        mode = fields.mode,  -- RemoteSessionMode or nil
    }
end

--- Experimental: Create a RemoteEnableResult table.
-- @param fields table with keys: remoteSteerable, url
-- @return table RemoteEnableResult
function M.RemoteEnableResult(fields)
    fields = fields or {}
    return {
        remoteSteerable = fields.remoteSteerable or false,
        url             = fields.url,  -- string or nil
    }
end

return M

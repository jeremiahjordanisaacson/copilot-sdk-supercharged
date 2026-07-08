classdef SessionConfig
    % SessionConfig  Configuration for a Copilot session.
    %
    %   cfg = copilot.SessionConfig('Model', 'gpt-5');
    %   cfg = copilot.SessionConfig('Model', 'gpt-5', 'SystemMessage', 'You are helpful.');
    %   cfg = copilot.SessionConfig('Model', 'gpt-5', 'Tools', {myTool1, myTool2});

    properties
        % Model identifier (e.g. 'gpt-5', 'claude-sonnet-4').
        Model (1,:) char = ''

        % System prompt sent at the start of the session.
        SystemMessage (1,:) char = ''

        % System-message mode: 'append' (default), 'replace', or 'customize'.
        SystemMessageMode (1,:) char = 'append'

        % Cell array of copilot.DefineTool objects exposed to this session.
        Tools (1,:) cell = {}

        % Session idle timeout in seconds (0 = no timeout).
        IdleTimeout (1,1) double = 0

        % Enable streaming deltas for assistant messages.
        Streaming (1,1) logical = true

        % Skills to preload into the agent context (cell of char).
        Skills (1,:) cell = {}

        % Tool names excluded from this session (cell of char).
        ExcludedTools (1,:) cell = {}

        % Arbitrary key-value metadata (containers.Map).
        Metadata

        % Custom request headers (containers.Map).
        RequestHeaders

        % Model capabilities override (struct or containers.Map).
        ModelCapabilities

        % Enable automatic config discovery.
        EnableConfigDiscovery (1,1) logical = false

        % Include sub-agent streaming events.
        IncludeSubAgentStreamingEvents (1,1) logical = false

        % MCP server configurations (struct or containers.Map).
        McpServers

        % Slash commands (cell array of structs with Name and Description).
        Commands (1,:) cell = {}

        % Response format: 'text', 'image', or 'json_object'.
        ResponseFormat (1,:) char = ''

        % Image generation options (struct with Size, Quality, Style).
        ImageOptions

        % Handler for elicitation requests (function handle or empty).
        ElicitationHandler

        % Directories containing instruction files (cell array of char).
        InstructionDirectories (1,:) cell = {}

        % --- Upstream-sync session configuration (parity with @github/copilot-sdk) ---

        % Emit inline source citations (wire: enableCitations).
        EnableCitations (1,1) logical = false

        % Built-in agents to exclude (cell of char) (wire: excludedBuiltinAgents).
        ExcludedBuiltinAgents (1,:) cell = {}

        % Per-session spending / credit limits (copilot.SessionLimits or struct).
        SessionLimits

        % Persistent session memory (copilot.MemoryConfiguration or struct).
        Memory

        % OTLP telemetry protocol, e.g. 'grpc' or 'http/protobuf' (wire: otlpProtocol).
        OtlpProtocol (1,:) char = ''

        % Stream responses over a WebSocket (wire: enableWebSocketResponses).
        EnableWebSocketResponses (1,1) logical = false

        % Experiment assignment overrides (containers.Map) (wire: expAssignments).
        ExpAssignments

        % Handler for MCP OAuth token requests (function handle or empty).
        OnMcpAuthRequest

        % BYOK bearer-token provider (function handle or empty).
        BearerTokenProvider

        % Post-tool-use hook (function handle or empty).
        OnPostToolUse

        % Pre-MCP-tool-call hook (function handle or empty).
        OnPreMcpToolCall

        % Custom HTTP request handler (function handle or empty).
        RequestHandler
    end

    methods
        function obj = SessionConfig(varargin)
            %SessionConfig  Create a session configuration.
            %
            %   cfg = copilot.SessionConfig('Model', 'gpt-5');

            p = inputParser;
            p.KeepUnmatched = true;
            p.addParameter('Model',              obj.Model);
            p.addParameter('SystemMessage',       obj.SystemMessage);
            p.addParameter('SystemMessageMode',   obj.SystemMessageMode);
            p.addParameter('Tools',               obj.Tools);
            p.addParameter('IdleTimeout',         obj.IdleTimeout);
            p.addParameter('Streaming',           obj.Streaming);
            p.addParameter('Skills',              obj.Skills);
            p.addParameter('ExcludedTools',       obj.ExcludedTools);
            p.addParameter('Metadata',            containers.Map());
            p.addParameter('RequestHeaders',      containers.Map());
            p.addParameter('ModelCapabilities',   struct());
            p.addParameter('EnableConfigDiscovery', obj.EnableConfigDiscovery);
            p.addParameter('IncludeSubAgentStreamingEvents', obj.IncludeSubAgentStreamingEvents);
            p.addParameter('McpServers',          struct());
            p.addParameter('Commands',            obj.Commands);
            p.addParameter('ResponseFormat',      obj.ResponseFormat);
            p.addParameter('ImageOptions',        struct());
            p.addParameter('ElicitationHandler',  []);
            p.addParameter('InstructionDirectories', obj.InstructionDirectories);
            p.addParameter('EnableCitations',          obj.EnableCitations);
            p.addParameter('ExcludedBuiltinAgents',    obj.ExcludedBuiltinAgents);
            p.addParameter('SessionLimits',            []);
            p.addParameter('Memory',                   []);
            p.addParameter('OtlpProtocol',             obj.OtlpProtocol);
            p.addParameter('EnableWebSocketResponses', obj.EnableWebSocketResponses);
            p.addParameter('ExpAssignments',           containers.Map());
            p.addParameter('OnMcpAuthRequest',         []);
            p.addParameter('BearerTokenProvider',      []);
            p.addParameter('OnPostToolUse',            []);
            p.addParameter('OnPreMcpToolCall',         []);
            p.addParameter('RequestHandler',           []);
            p.parse(varargin{:});

            obj.Model              = p.Results.Model;
            obj.SystemMessage      = p.Results.SystemMessage;
            obj.SystemMessageMode  = p.Results.SystemMessageMode;
            obj.Tools              = p.Results.Tools;
            obj.IdleTimeout        = p.Results.IdleTimeout;
            obj.Streaming          = p.Results.Streaming;
            obj.Skills             = p.Results.Skills;
            obj.ExcludedTools      = p.Results.ExcludedTools;
            obj.Metadata           = p.Results.Metadata;
            obj.RequestHeaders     = p.Results.RequestHeaders;
            obj.ModelCapabilities  = p.Results.ModelCapabilities;
            obj.EnableConfigDiscovery = p.Results.EnableConfigDiscovery;
            obj.IncludeSubAgentStreamingEvents = p.Results.IncludeSubAgentStreamingEvents;
            obj.McpServers         = p.Results.McpServers;
            obj.Commands           = p.Results.Commands;
            obj.ResponseFormat     = p.Results.ResponseFormat;
            obj.ImageOptions       = p.Results.ImageOptions;
            obj.ElicitationHandler = p.Results.ElicitationHandler;
            obj.InstructionDirectories = p.Results.InstructionDirectories;
            obj.EnableCitations          = p.Results.EnableCitations;
            obj.ExcludedBuiltinAgents    = p.Results.ExcludedBuiltinAgents;
            obj.SessionLimits            = p.Results.SessionLimits;
            obj.Memory                   = p.Results.Memory;
            obj.OtlpProtocol             = p.Results.OtlpProtocol;
            obj.EnableWebSocketResponses = p.Results.EnableWebSocketResponses;
            obj.ExpAssignments           = p.Results.ExpAssignments;
            obj.OnMcpAuthRequest         = p.Results.OnMcpAuthRequest;
            obj.BearerTokenProvider      = p.Results.BearerTokenProvider;
            obj.OnPostToolUse            = p.Results.OnPostToolUse;
            obj.OnPreMcpToolCall         = p.Results.OnPreMcpToolCall;
            obj.RequestHandler           = p.Results.RequestHandler;
        end

        function s = toStruct(obj)
            %toStruct  Serialize config for JSON-RPC.
            s = struct();
            if ~isempty(obj.Model)
                s.model = obj.Model;
            end
            if ~isempty(obj.SystemMessage)
                s.systemMessage = struct( ...
                    'content', obj.SystemMessage, ...
                    'mode',    obj.SystemMessageMode);
            end
            s.streaming = obj.Streaming;
            if obj.IdleTimeout > 0
                s.idleTimeout = obj.IdleTimeout;
            end
            if ~isempty(obj.Tools)
                toolDefs = cell(1, numel(obj.Tools));
                for i = 1:numel(obj.Tools)
                    toolDefs{i} = obj.Tools{i}.toStruct();
                end
                s.tools = toolDefs;
            end
            if ~isempty(obj.Skills)
                s.skills = obj.Skills;
            end
            if ~isempty(obj.ExcludedTools)
                s.excludedTools = obj.ExcludedTools;
            end
            if ~isempty(obj.InstructionDirectories)
                s.instructionDirectories = obj.InstructionDirectories;
            end
            % --- Upstream-sync session configuration passthroughs (camelCase wire keys) ---
            if obj.EnableCitations
                s.enableCitations = true;
            end
            if ~isempty(obj.ExcludedBuiltinAgents)
                s.excludedBuiltinAgents = obj.ExcludedBuiltinAgents;
            end
            if ~isempty(obj.SessionLimits)
                if isa(obj.SessionLimits, 'copilot.SessionLimits')
                    s.sessionLimits = obj.SessionLimits.toStruct();
                else
                    s.sessionLimits = obj.SessionLimits;
                end
            end
            if ~isempty(obj.Memory)
                if isa(obj.Memory, 'copilot.MemoryConfiguration')
                    s.memory = obj.Memory.toStruct();
                else
                    s.memory = obj.Memory;
                end
            end
            if ~isempty(obj.OtlpProtocol)
                s.otlpProtocol = obj.OtlpProtocol;
            end
            if obj.EnableWebSocketResponses
                s.enableWebSocketResponses = true;
            end
            if isa(obj.ExpAssignments, 'containers.Map') && obj.ExpAssignments.Count > 0
                s.expAssignments = obj.ExpAssignments;
            end
            if ~isempty(obj.OnMcpAuthRequest)
                s.mcpAuthHandler = true;
            end
            hooks = struct();
            if ~isempty(obj.OnPostToolUse)
                hooks.postToolUse = true;
            end
            if ~isempty(obj.OnPreMcpToolCall)
                hooks.preMcpToolCall = true;
            end
            if ~isempty(fieldnames(hooks))
                s.hooks = hooks;
            end
        end
    end
end

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

        % Enable/disable session telemetry.
        EnableSessionTelemetry

        % Handler for exit-plan-mode requests (function handle or empty).
        ExitPlanModeHandler
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
            p.addParameter('EnableSessionTelemetry', []);
            p.addParameter('ExitPlanModeHandler', []);
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
            obj.EnableSessionTelemetry = p.Results.EnableSessionTelemetry;
            obj.ExitPlanModeHandler = p.Results.ExitPlanModeHandler;
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
            if ~isempty(obj.EnableSessionTelemetry)
                s.enableSessionTelemetry = obj.EnableSessionTelemetry;
            end
            if ~isempty(obj.ExitPlanModeHandler)
                s.requestExitPlanMode = true;
            end
            if isstruct(obj.ModelCapabilities) && ~isempty(fieldnames(obj.ModelCapabilities))
                s.modelCapabilities = obj.ModelCapabilities;
            end
        end
    end
end

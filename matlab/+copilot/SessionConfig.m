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

        % Enable conversational rewind (restore an earlier turn) (wire: rewindEnabled).
        RewindEnabled (1,1) logical = false

        % Extra workspace directories to expose (cell of char) (wire: additionalDirectories).
        AdditionalDirectories (1,:) cell = {}

        % MCP servers to disable for this session (cell of char) (wire: disabledMcpServers).
        DisabledMcpServers (1,:) cell = {}

        % GitHub MCP tool configuration (struct or containers.Map) (wire: githubMcpToolConfig).
        GithubMcpToolConfig

        % Canvas provider configuration (struct or containers.Map) (wire: canvasProvider).
        CanvasProvider

        % Restrict custom agents to locally-defined ones only (wire: customAgentsLocalOnly).
        CustomAgentsLocalOnly (1,1) logical = false

        % Permission-reply decision context (struct or containers.Map) (wire: decisionContext).
        DecisionContext

        % User-prompt-transformed hook (function handle or empty) (wire: userPromptTransformed).
        OnUserPromptTransformed

        % Built-in plugin directories to load (cell of char) (wire: builtinPluginDirectories).
        BuiltinPluginDirectories (1,:) cell = {}

        % Agent-factory authoring arguments schema (struct) (wire: argsSchema).
        ArgsSchema

        % Reasoning effort control, e.g. 'low'/'medium'/'high' (wire: reasoningEffort).
        ReasoningEffort (1,:) char = ''

        % Tool-search configuration (struct or logical) (wire: toolSearch).
        ToolSearch

        % Use the in-process FFI transport instead of a spawned CLI (wire: inProcess).
        InProcess (1,1) logical = false

        % Enable experimental mode features (wire: experimentalMode).
        ExperimentalMode (1,1) logical = false

        % Enable content exclusion policy enforcement (wire: contentExclusion).
        ContentExclusion (1,1) logical = false

        % --- SDK protocol 4 / v3.0.0 session configuration ---

        % Tool allowlist (cell of char) (wire: tools.availableTools).  Required
        % on every create/resume when the client Mode is 'empty'.
        AvailableTools (1,:) cell = {}

        % Working directory of the session (wire: workingDirectory).
        WorkingDirectory (1,:) char = ''

        % Ask-user tool variant: 'legacy' or 'elicitation' (wire: askUserVariant).
        AskUserVariant (1,:) char = ''

        % GitHub token provider (function handle).  Called with the
        % gitHubToken.getToken request struct (registrationId, host,
        % sessionId, reason); return a token char, a struct with
        % accessToken / tokenType / expiresIn, or [] to cancel.
        GitHubTokenProvider

        % Command sandbox (copilot.SandboxConfig or struct) (wire: permissions.sandbox).
        Sandbox

        % Feature flag overrides (struct or containers.Map of logical) (wire: featureFlags).
        FeatureFlags

        % Model allowlist (cell of model ids) (wire: allowedModels).
        AllowedModels (1,:) cell = {}

        % OAuth client id metadata document URL for MCP servers.
        AuthClientIdMetadataUrl (1,:) char = ''

        % Model capabilities override limits.max_output_tokens (0 = unset).
        MaxOutputTokens (1,1) double = 0

        % Extra agent / hook directories (cell of char).
        AgentDirectories (1,:) cell = {}
        HookDirectories (1,:) cell = {}

        % Grant directory trust at creation: 'session' or 'user' (create only).
        TrustWorkingDirectory (1,:) char = ''

        % Image generation (copilot.ImageGenerationConfig or struct).
        ImageGeneration

        % Autopilot continuation (copilot.AutopilotConfig or struct).
        Autopilot

        % Sub-agent lifecycle hooks (function handles).  Called with the hook
        % input struct; return a struct output (or []).
        OnSubagentStart
        OnSubagentStop

        % Legacy single BYOK provider (copilot.ProviderConfig or struct).
        Provider

        % Named BYOK providers / models (cells of copilot.ProviderConfig and
        % copilot.ProviderModelConfig, or structs).
        Providers (1,:) cell = {}
        Models (1,:) cell = {}

        % Default named provider (wire: defaultProvider).
        DefaultProvider (1,:) char = ''

        % On resume, send an explicit null defaultProvider to clear it.
        ClearDefaultProvider (1,1) logical = false

        % Copilot API models: false disables them (BYOK only), [] = default.
        CapiEnabled = []

        % Shell authentication handler (function handle).  Called with the
        % shellAuthentication.request struct; return the value char to
        % accept, or [] to cancel.
        OnShellAuthenticationRequest

        % User input handler for the ask_user tool (function handle).  Called
        % with a copilot.UserInputRequest; return a copilot.UserInputResponse.
        % Setting it enables user input requests (wire: requestUserInput).
        OnUserInputRequest

        % Memory store callback (function handle) called with the
        % memoryStore.store request struct (subject, fact, citations, reason).
        StoreMemory

        % Resume only: do not emit a session.resume event (wire: suppressResumeEvent).
        DisableResume (1,1) logical = false

        % Resume only: continue work pending when the session was saved.
        ContinuePendingWork (1,1) logical = false

        % Resume only: rebuild a damaged session from its transcript.
        AllowTranscriptRecovery (1,1) logical = false
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
            p.addParameter('RewindEnabled',            obj.RewindEnabled);
            p.addParameter('AdditionalDirectories',    obj.AdditionalDirectories);
            p.addParameter('DisabledMcpServers',       obj.DisabledMcpServers);
            p.addParameter('GithubMcpToolConfig',      []);
            p.addParameter('CanvasProvider',           []);
            p.addParameter('CustomAgentsLocalOnly',    obj.CustomAgentsLocalOnly);
            p.addParameter('DecisionContext',          []);
            p.addParameter('OnUserPromptTransformed',  []);
            p.addParameter('BuiltinPluginDirectories', obj.BuiltinPluginDirectories);
            p.addParameter('ArgsSchema',               []);
            p.addParameter('ReasoningEffort',          obj.ReasoningEffort);
            p.addParameter('ToolSearch',               []);
            p.addParameter('InProcess',                obj.InProcess);
            p.addParameter('ExperimentalMode',         obj.ExperimentalMode);
            p.addParameter('ContentExclusion',         obj.ContentExclusion);
            p.addParameter('AvailableTools',           obj.AvailableTools);
            p.addParameter('WorkingDirectory',         obj.WorkingDirectory);
            p.addParameter('AskUserVariant',           obj.AskUserVariant);
            p.addParameter('GitHubTokenProvider',      []);
            p.addParameter('Sandbox',                  []);
            p.addParameter('FeatureFlags',             []);
            p.addParameter('AllowedModels',            obj.AllowedModels);
            p.addParameter('AuthClientIdMetadataUrl',  obj.AuthClientIdMetadataUrl);
            p.addParameter('MaxOutputTokens',          obj.MaxOutputTokens);
            p.addParameter('AgentDirectories',         obj.AgentDirectories);
            p.addParameter('HookDirectories',          obj.HookDirectories);
            p.addParameter('TrustWorkingDirectory',    obj.TrustWorkingDirectory);
            p.addParameter('ImageGeneration',          []);
            p.addParameter('Autopilot',                []);
            p.addParameter('OnSubagentStart',          []);
            p.addParameter('OnSubagentStop',           []);
            p.addParameter('Provider',                 []);
            p.addParameter('Providers',                obj.Providers);
            p.addParameter('Models',                   obj.Models);
            p.addParameter('DefaultProvider',          obj.DefaultProvider);
            p.addParameter('ClearDefaultProvider',     obj.ClearDefaultProvider);
            p.addParameter('CapiEnabled',              []);
            p.addParameter('OnShellAuthenticationRequest', []);
            p.addParameter('OnUserInputRequest',       []);
            p.addParameter('StoreMemory',              []);
            p.addParameter('DisableResume',            obj.DisableResume);
            p.addParameter('ContinuePendingWork',      obj.ContinuePendingWork);
            p.addParameter('AllowTranscriptRecovery',  obj.AllowTranscriptRecovery);
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
            obj.RewindEnabled            = p.Results.RewindEnabled;
            obj.AdditionalDirectories    = p.Results.AdditionalDirectories;
            obj.DisabledMcpServers       = p.Results.DisabledMcpServers;
            obj.GithubMcpToolConfig      = p.Results.GithubMcpToolConfig;
            obj.CanvasProvider           = p.Results.CanvasProvider;
            obj.CustomAgentsLocalOnly    = p.Results.CustomAgentsLocalOnly;
            obj.DecisionContext          = p.Results.DecisionContext;
            obj.OnUserPromptTransformed  = p.Results.OnUserPromptTransformed;
            obj.BuiltinPluginDirectories = p.Results.BuiltinPluginDirectories;
            obj.ArgsSchema               = p.Results.ArgsSchema;
            obj.ReasoningEffort          = p.Results.ReasoningEffort;
            obj.ToolSearch               = p.Results.ToolSearch;
            obj.InProcess                = p.Results.InProcess;
            obj.ExperimentalMode         = p.Results.ExperimentalMode;
            obj.ContentExclusion         = p.Results.ContentExclusion;
            obj.AvailableTools           = p.Results.AvailableTools;
            obj.WorkingDirectory         = p.Results.WorkingDirectory;
            obj.AskUserVariant           = p.Results.AskUserVariant;
            obj.GitHubTokenProvider      = p.Results.GitHubTokenProvider;
            obj.Sandbox                  = p.Results.Sandbox;
            obj.FeatureFlags             = p.Results.FeatureFlags;
            obj.AllowedModels            = p.Results.AllowedModels;
            obj.AuthClientIdMetadataUrl  = p.Results.AuthClientIdMetadataUrl;
            obj.MaxOutputTokens          = p.Results.MaxOutputTokens;
            obj.AgentDirectories         = p.Results.AgentDirectories;
            obj.HookDirectories          = p.Results.HookDirectories;
            obj.TrustWorkingDirectory    = p.Results.TrustWorkingDirectory;
            obj.ImageGeneration          = p.Results.ImageGeneration;
            obj.Autopilot                = p.Results.Autopilot;
            obj.OnSubagentStart          = p.Results.OnSubagentStart;
            obj.OnSubagentStop           = p.Results.OnSubagentStop;
            obj.Provider                 = p.Results.Provider;
            obj.Providers                = p.Results.Providers;
            obj.Models                   = p.Results.Models;
            obj.DefaultProvider          = p.Results.DefaultProvider;
            obj.ClearDefaultProvider     = p.Results.ClearDefaultProvider;
            obj.CapiEnabled              = p.Results.CapiEnabled;
            obj.OnShellAuthenticationRequest = p.Results.OnShellAuthenticationRequest;
            obj.OnUserInputRequest       = p.Results.OnUserInputRequest;
            obj.StoreMemory              = p.Results.StoreMemory;
            obj.DisableResume            = p.Results.DisableResume;
            obj.ContinuePendingWork      = p.Results.ContinuePendingWork;
            obj.AllowTranscriptRecovery  = p.Results.AllowTranscriptRecovery;
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
            % --- Newly-synced session configuration passthroughs (camelCase wire keys) ---
            if obj.RewindEnabled
                s.rewindEnabled = true;
            end
            if ~isempty(obj.AdditionalDirectories)
                s.additionalDirectories = obj.AdditionalDirectories;
            end
            if ~isempty(obj.DisabledMcpServers)
                s.disabledMcpServers = obj.DisabledMcpServers;
            end
            if ~isempty(obj.GithubMcpToolConfig)
                s.githubMcpToolConfig = obj.GithubMcpToolConfig;
            end
            if ~isempty(obj.CanvasProvider)
                s.canvasProvider = obj.CanvasProvider;
            end
            if obj.CustomAgentsLocalOnly
                s.customAgentsLocalOnly = true;
            end
            if ~isempty(obj.DecisionContext)
                s.decisionContext = obj.DecisionContext;
            end
            if ~isempty(obj.BuiltinPluginDirectories)
                s.builtinPluginDirectories = obj.BuiltinPluginDirectories;
            end
            if ~isempty(obj.ArgsSchema)
                s.argsSchema = obj.ArgsSchema;
            end
            if ~isempty(obj.ReasoningEffort)
                s.reasoningEffort = obj.ReasoningEffort;
            end
            if ~isempty(obj.ToolSearch)
                s.toolSearch = obj.ToolSearch;
            end
            if obj.InProcess
                s.inProcess = true;
            end
            if obj.ExperimentalMode
                s.experimentalMode = true;
            end
            if obj.ContentExclusion
                s.contentExclusion = true;
            end
            if ~isempty(obj.OnMcpAuthRequest)
                s.mcpAuthHandler = true;
            end
            % SDK protocol 4 signals registered hook handlers with a single
            % flag (hooks.hasHandlers); handlers run through hooks.invoke.
            if ~isempty(obj.OnPostToolUse) || ~isempty(obj.OnPreMcpToolCall) ...
                    || ~isempty(obj.OnUserPromptTransformed) ...
                    || ~isempty(obj.OnSubagentStart) || ~isempty(obj.OnSubagentStop)
                s.hooks = true;
            end

            % --- SDK protocol 4 / v3.0.0 options (flat keys; the client groups
            %     them with copilot.groupSessionParams before sending) ---
            if ~isempty(obj.AvailableTools)
                s.availableTools = obj.AvailableTools;
            end
            if ~isempty(obj.WorkingDirectory)
                s.workingDirectory = obj.WorkingDirectory;
            end
            if ~isempty(obj.AskUserVariant)
                s.askUserVariant = obj.AskUserVariant;
            end
            if ~isempty(obj.Sandbox)
                s.sandbox = copilot.SessionConfig.wireValue(obj.Sandbox);
            end
            if ~isempty(obj.FeatureFlags)
                s.featureFlags = obj.FeatureFlags;
            end
            if ~isempty(obj.AllowedModels)
                s.allowedModels = obj.AllowedModels;
            end
            if ~isempty(obj.AuthClientIdMetadataUrl)
                s.authClientIdMetadataUrl = obj.AuthClientIdMetadataUrl;
            end
            caps = obj.ModelCapabilities;
            if obj.MaxOutputTokens > 0
                if ~isstruct(caps)
                    caps = struct();
                end
                if ~isfield(caps, 'limits') || ~isstruct(caps.limits)
                    caps.limits = struct();
                end
                % The capabilities limits object uses snake_case wire keys.
                caps.limits.max_output_tokens = obj.MaxOutputTokens;
            end
            if (isstruct(caps) && ~isempty(fieldnames(caps))) ...
                    || (isa(caps, 'containers.Map') && caps.Count > 0)
                s.modelCapabilities = caps;
            end
            if ~isempty(obj.AgentDirectories)
                s.agentDirectories = obj.AgentDirectories;
            end
            if ~isempty(obj.HookDirectories)
                s.hookDirectories = obj.HookDirectories;
            end
            if ~isempty(obj.TrustWorkingDirectory)
                s.trustWorkingDirectory = obj.TrustWorkingDirectory;
            end
            imageGeneration = copilot.SessionConfig.wireValue(obj.ImageGeneration);
            if isstruct(imageGeneration) && ~isempty(fieldnames(imageGeneration))
                s.imageGeneration = imageGeneration;
            end
            autopilot = copilot.SessionConfig.wireValue(obj.Autopilot);
            if isstruct(autopilot) && ~isempty(fieldnames(autopilot))
                s.autopilot = autopilot;
            end
            if ~isempty(obj.Provider)
                s.provider = copilot.SessionConfig.wireValue(obj.Provider);
            end
            if ~isempty(obj.Providers)
                s.providers = copilot.SessionConfig.wireList(obj.Providers);
            end
            if ~isempty(obj.Models)
                s.models = copilot.SessionConfig.wireList(obj.Models);
            end
            if ~isempty(obj.DefaultProvider)
                s.defaultProvider = obj.DefaultProvider;
            end
            if ~isempty(obj.CapiEnabled)
                s.capi = struct('enabled', logical(obj.CapiEnabled));
            end
            if ~isempty(obj.OnShellAuthenticationRequest)
                s.requestShellAuthentication = true;
            end
            if ~isempty(obj.OnUserInputRequest)
                s.requestUserInput = true;
            end
            if ~isempty(obj.StoreMemory)
                s.hasMemoryStore = true;
            end
        end
    end

    methods (Static, Access = private)
        function value = wireValue(value)
            %wireValue  Serialize SDK option objects that define toStruct().
            if isobject(value) && ismethod(value, 'toStruct')
                value = value.toStruct();
            end
        end

        function items = wireList(items)
            %wireList  Serialize each element of a cell array of options.
            for i = 1:numel(items)
                items{i} = copilot.SessionConfig.wireValue(items{i});
            end
        end
    end
end

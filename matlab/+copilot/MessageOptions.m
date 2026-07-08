classdef MessageOptions
    % MessageOptions  Options for sending a message to a session.
    %
    %   opts = copilot.MessageOptions('Prompt', 'What is 2+2?');

    properties
        % The user prompt text.
        Prompt (1,:) char = ''

        % Custom request headers for this turn (containers.Map).
        RequestHeaders

        % Agent mode override for this message (wire: agentMode).
        AgentMode (1,:) char = ''

        % Alternate prompt shown in the UI (wire: displayPrompt).
        DisplayPrompt (1,:) char = ''

        % Timeout in seconds for sendAndWait (0 = no limit).
        Timeout (1,1) double = 120
    end

    methods
        function obj = MessageOptions(varargin)
            p = inputParser;
            p.addParameter('Prompt',         obj.Prompt);
            p.addParameter('RequestHeaders', containers.Map());
            p.addParameter('AgentMode',      obj.AgentMode);
            p.addParameter('DisplayPrompt',  obj.DisplayPrompt);
            p.addParameter('Timeout',        obj.Timeout);
            p.parse(varargin{:});
            obj.Prompt         = p.Results.Prompt;
            obj.RequestHeaders = p.Results.RequestHeaders;
            obj.AgentMode      = p.Results.AgentMode;
            obj.DisplayPrompt  = p.Results.DisplayPrompt;
            obj.Timeout        = p.Results.Timeout;
        end

        function s = toStruct(obj)
            s = struct('content', obj.Prompt);
            if ~isempty(obj.AgentMode)
                s.agentMode = obj.AgentMode;
            end
            if ~isempty(obj.DisplayPrompt)
                s.displayPrompt = obj.DisplayPrompt;
            end
            if isa(obj.RequestHeaders, 'containers.Map') && obj.RequestHeaders.Count > 0
                s.requestHeaders = obj.RequestHeaders;
            end
        end
    end
end

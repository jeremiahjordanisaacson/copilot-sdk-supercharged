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

        % Message source (upstream MessageSource): 'user', 'system' or
        % 'agent-<id>' (wire: source).
        Source (1,:) char = ''

        % Opaque caller-chosen id echoed on this turn's events (wire: clientTurnId).
        ClientTurnId (1,:) char = ''

        % JSON schema (struct) for structured output (upstream responseSchema).
        % Sent as responseFormat {type: 'json_schema', jsonSchema: {...}}.
        ResponseSchema = []

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
            p.addParameter('Source',         obj.Source);
            p.addParameter('ClientTurnId',   obj.ClientTurnId);
            p.addParameter('ResponseSchema', []);
            p.addParameter('Timeout',        obj.Timeout);
            p.parse(varargin{:});
            obj.Prompt         = p.Results.Prompt;
            obj.RequestHeaders = p.Results.RequestHeaders;
            obj.AgentMode      = p.Results.AgentMode;
            obj.DisplayPrompt  = p.Results.DisplayPrompt;
            obj.Source         = p.Results.Source;
            obj.ClientTurnId   = p.Results.ClientTurnId;
            obj.ResponseSchema = p.Results.ResponseSchema;
            obj.Timeout        = p.Results.Timeout;
        end

        function s = toStruct(obj)
            s = struct('content', obj.Prompt);
            s = obj.addOptional(s);
        end

        function s = toSendParams(obj)
            %toSendParams  session.send parameters (SDK protocol 4).
            s = struct('prompt', obj.Prompt);
            s = obj.addOptional(s);
        end
    end

    methods (Access = private)
        function s = addOptional(obj, s)
            if ~isempty(obj.AgentMode)
                s.agentMode = obj.AgentMode;
            end
            if ~isempty(obj.DisplayPrompt)
                s.displayPrompt = obj.DisplayPrompt;
            end
            if isa(obj.RequestHeaders, 'containers.Map') && obj.RequestHeaders.Count > 0
                s.requestHeaders = obj.RequestHeaders;
            end
            if ~isempty(obj.Source)
                s.source = obj.Source;
            end
            if ~isempty(obj.ClientTurnId)
                s.clientTurnId = obj.ClientTurnId;
            end
            if ~isempty(obj.ResponseSchema)
                s.responseFormat = struct( ...
                    'type', 'json_schema', ...
                    'jsonSchema', struct( ...
                        'name',   'response', ...
                        'strict', true, ...
                        'schema', obj.ResponseSchema));
            end
        end
    end
end

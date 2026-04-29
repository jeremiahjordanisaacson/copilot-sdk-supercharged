classdef MessageOptions
    % MessageOptions  Options for sending a message to a session.
    %
    %   opts = copilot.MessageOptions('Prompt', 'What is 2+2?');

    properties
        % The user prompt text.
        Prompt (1,:) char = ''

        % Custom request headers for this turn (containers.Map).
        RequestHeaders

        % Timeout in seconds for sendAndWait (0 = no limit).
        Timeout (1,1) double = 120
    end

    methods
        function obj = MessageOptions(varargin)
            p = inputParser;
            p.addParameter('Prompt',         obj.Prompt);
            p.addParameter('RequestHeaders', containers.Map());
            p.addParameter('Timeout',        obj.Timeout);
            p.parse(varargin{:});
            obj.Prompt         = p.Results.Prompt;
            obj.RequestHeaders = p.Results.RequestHeaders;
            obj.Timeout        = p.Results.Timeout;
        end

        function s = toStruct(obj)
            s = struct('content', obj.Prompt);
        end
    end
end

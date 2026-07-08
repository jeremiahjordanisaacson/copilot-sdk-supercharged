classdef SessionLimits
    % SessionLimits  Per-session spending / credit limits.
    %
    %   Parity with @github/copilot-sdk. Caps the resources a single session
    %   may consume before the server stops the turn.
    %
    %   limits = copilot.SessionLimits('MaxAiCredits', 5, 'MaxRequests', 100);

    properties
        % Maximum AI credits to spend in this session (0 = unlimited).
        MaxAiCredits (1,1) double = 0

        % Maximum number of model requests (0 = unlimited).
        MaxRequests (1,1) double = 0

        % Maximum number of tokens (0 = unlimited).
        MaxTokens (1,1) double = 0
    end

    methods
        function obj = SessionLimits(varargin)
            %SessionLimits  Create a session-limits configuration.
            p = inputParser;
            p.addParameter('MaxAiCredits', obj.MaxAiCredits);
            p.addParameter('MaxRequests',  obj.MaxRequests);
            p.addParameter('MaxTokens',    obj.MaxTokens);
            p.parse(varargin{:});
            obj.MaxAiCredits = p.Results.MaxAiCredits;
            obj.MaxRequests  = p.Results.MaxRequests;
            obj.MaxTokens    = p.Results.MaxTokens;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct();
            if obj.MaxAiCredits > 0
                s.maxAiCredits = obj.MaxAiCredits;
            end
            if obj.MaxRequests > 0
                s.maxRequests = obj.MaxRequests;
            end
            if obj.MaxTokens > 0
                s.maxTokens = obj.MaxTokens;
            end
        end
    end
end

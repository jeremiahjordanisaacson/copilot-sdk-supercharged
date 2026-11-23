classdef AutopilotConfig
    % AutopilotConfig  Autopilot continuation settings (wire: execution.autopilot).
    %
    %   ap = copilot.AutopilotConfig('ContinuationEnabled', true, 'MaxContinues', 5);
    %   cfg = copilot.SessionConfig('Autopilot', ap);

    properties
        % Let autopilot continue automatically after a turn ([] = default).
        ContinuationEnabled = []

        % Maximum automatic continuations (0 = runtime default) (wire: maxContinues).
        MaxContinues (1,1) double = 0
    end

    methods
        function obj = AutopilotConfig(varargin)
            p = inputParser;
            p.addParameter('ContinuationEnabled', []);
            p.addParameter('MaxContinues',        obj.MaxContinues);
            p.parse(varargin{:});
            obj.ContinuationEnabled = p.Results.ContinuationEnabled;
            obj.MaxContinues        = p.Results.MaxContinues;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct();
            if ~isempty(obj.ContinuationEnabled)
                s.continuationEnabled = logical(obj.ContinuationEnabled);
            end
            if obj.MaxContinues > 0
                s.maxContinues = obj.MaxContinues;
            end
        end
    end
end

classdef RemoteEnableRequest
    % RemoteEnableRequest  Experimental
    %   Request to enable or configure remote session mode.

    properties
        % Remote session mode (copilot.RemoteSessionMode or empty).
        Mode
    end

    methods
        function obj = RemoteEnableRequest(varargin)
            p = inputParser;
            p.addParameter('Mode', []);
            p.parse(varargin{:});
            obj.Mode = p.Results.Mode;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Mode)
                s.mode = obj.Mode.Value;
            end
        end
    end
end

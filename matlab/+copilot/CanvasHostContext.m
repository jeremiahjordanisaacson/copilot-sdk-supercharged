classdef CanvasHostContext
    % CanvasHostContext  Host context supplied alongside canvas callbacks.

    properties
        Capabilities
    end

    methods
        function obj = CanvasHostContext(varargin)
            p = inputParser;
            p.addParameter('Capabilities', []);
            p.parse(varargin{:});
            obj.Capabilities = p.Results.Capabilities;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Capabilities)
                s.capabilities = obj.Capabilities.toStruct();
            end
        end
    end
end

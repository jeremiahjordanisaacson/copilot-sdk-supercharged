classdef CanvasHostCapabilities
    % CanvasHostCapabilities  Canvas-related capabilities reported by the host.

    properties
        Canvases (1,1) logical = false
    end

    methods
        function obj = CanvasHostCapabilities(varargin)
            p = inputParser;
            p.addParameter('Canvases', obj.Canvases);
            p.parse(varargin{:});
            obj.Canvases = p.Results.Canvases;
        end

        function s = toStruct(obj)
            s = struct('canvases', obj.Canvases);
        end
    end
end

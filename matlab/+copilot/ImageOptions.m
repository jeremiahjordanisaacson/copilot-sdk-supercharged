classdef ImageOptions
    % ImageOptions  Image generation options.

    properties
        Size (1,:) char = ''
        Quality (1,:) char = ''
        Style (1,:) char = ''
    end

    methods
        function obj = ImageOptions(varargin)
            p = inputParser;
            p.addParameter('Size', obj.Size);
            p.addParameter('Quality', obj.Quality);
            p.addParameter('Style', obj.Style);
            p.parse(varargin{:});
            obj.Size = p.Results.Size;
            obj.Quality = p.Results.Quality;
            obj.Style = p.Results.Style;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Size)
                s.size = obj.Size;
            end
            if ~isempty(obj.Quality)
                s.quality = obj.Quality;
            end
            if ~isempty(obj.Style)
                s.style = obj.Style;
            end
        end
    end
end

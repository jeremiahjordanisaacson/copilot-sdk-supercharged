classdef SectionOverride
    % SectionOverride  Override operation for a single system prompt section.

    properties
        Action (1,:) char = ''
        Content (1,:) char = ''
    end

    methods
        function obj = SectionOverride(varargin)
            p = inputParser;
            p.addParameter('Action', obj.Action);
            p.addParameter('Content', obj.Content);
            p.parse(varargin{:});
            obj.Action = p.Results.Action;
            obj.Content = p.Results.Content;
        end

        function s = toStruct(obj)
            s = struct('action', obj.Action);
            if ~isempty(obj.Content)
                s.content = obj.Content;
            end
        end
    end
end

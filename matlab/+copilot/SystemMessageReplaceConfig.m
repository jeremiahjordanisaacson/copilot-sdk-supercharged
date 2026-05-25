classdef SystemMessageReplaceConfig
    % SystemMessageReplaceConfig  Replace-mode system message configuration.

    properties
        Mode (1,:) char = 'replace'
        Content (1,:) char = ''
    end

    methods
        function obj = SystemMessageReplaceConfig(varargin)
            p = inputParser;
            p.addParameter('Mode', obj.Mode);
            p.addParameter('Content', obj.Content);
            p.parse(varargin{:});
            obj.Mode = p.Results.Mode;
            obj.Content = p.Results.Content;
        end

        function s = toStruct(obj)
            s = struct('mode', obj.Mode, 'content', obj.Content);
        end
    end
end

classdef SystemMessageAppendConfig
    % SystemMessageAppendConfig  Append-mode system message configuration.

    properties
        Mode (1,:) char = ''
        Content (1,:) char = ''
    end

    methods
        function obj = SystemMessageAppendConfig(varargin)
            p = inputParser;
            p.addParameter('Mode', obj.Mode);
            p.addParameter('Content', obj.Content);
            p.parse(varargin{:});
            obj.Mode = p.Results.Mode;
            obj.Content = p.Results.Content;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Mode)
                s.mode = obj.Mode;
            end
            if ~isempty(obj.Content)
                s.content = obj.Content;
            end
        end
    end
end

classdef SystemMessageCustomizeConfig
    % SystemMessageCustomizeConfig  Customize-mode system message configuration.

    properties
        Mode (1,:) char = 'customize'
        Sections
        Content (1,:) char = ''
    end

    methods
        function obj = SystemMessageCustomizeConfig(varargin)
            p = inputParser;
            p.addParameter('Mode', obj.Mode);
            p.addParameter('Sections', struct());
            p.addParameter('Content', obj.Content);
            p.parse(varargin{:});
            obj.Mode = p.Results.Mode;
            obj.Sections = p.Results.Sections;
            obj.Content = p.Results.Content;
        end

        function s = toStruct(obj)
            s = struct('mode', obj.Mode);
            if ~isempty(obj.Sections)
                s.sections = obj.Sections;
            end
            if ~isempty(obj.Content)
                s.content = obj.Content;
            end
        end
    end
end

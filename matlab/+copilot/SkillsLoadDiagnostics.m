classdef SkillsLoadDiagnostics
    % SkillsLoadDiagnostics  Experimental
    %   Diagnostics from loading skills.

    properties
        % Error messages (cell array of char).
        Errors (1,:) cell = {}

        % Warning messages (cell array of char).
        Warnings (1,:) cell = {}
    end

    methods
        function obj = SkillsLoadDiagnostics(varargin)
            p = inputParser;
            p.addParameter('Errors', obj.Errors);
            p.addParameter('Warnings', obj.Warnings);
            p.parse(varargin{:});
            obj.Errors = p.Results.Errors;
            obj.Warnings = p.Results.Warnings;
        end

        function s = toStruct(obj)
            s = struct('errors', {obj.Errors}, 'warnings', {obj.Warnings});
        end
    end
end

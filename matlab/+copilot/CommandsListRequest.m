classdef CommandsListRequest
    % CommandsListRequest  Request to list available commands.

    properties
        % Include built-in commands.
        IncludeBuiltins

        % Include client commands.
        IncludeClientCommands

        % Include skill commands.
        IncludeSkills
    end

    methods
        function obj = CommandsListRequest(varargin)
            p = inputParser;
            p.addParameter('IncludeBuiltins', []);
            p.addParameter('IncludeClientCommands', []);
            p.addParameter('IncludeSkills', []);
            p.parse(varargin{:});
            obj.IncludeBuiltins = p.Results.IncludeBuiltins;
            obj.IncludeClientCommands = p.Results.IncludeClientCommands;
            obj.IncludeSkills = p.Results.IncludeSkills;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.IncludeBuiltins)
                s.includeBuiltins = obj.IncludeBuiltins;
            end
            if ~isempty(obj.IncludeClientCommands)
                s.includeClientCommands = obj.IncludeClientCommands;
            end
            if ~isempty(obj.IncludeSkills)
                s.includeSkills = obj.IncludeSkills;
            end
        end
    end
end

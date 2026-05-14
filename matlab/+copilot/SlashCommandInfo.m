classdef SlashCommandInfo
    % SlashCommandInfo  Information about a slash command.

    properties
        % Whether the command can run during agent execution.
        AllowDuringAgentExecution (1,1) logical = false

        % Description of the command.
        Description (1,:) char = ''

        % Kind of command ('builtin', 'client', 'skill').
        Kind (1,:) char = ''

        % Name of the command.
        Name (1,:) char = ''

        % Aliases for the command (cell array of char).
        Aliases (1,:) cell = {}

        % Whether the command is experimental.
        Experimental

        % Input specification (copilot.SlashCommandInput or empty).
        Input
    end

    methods
        function obj = SlashCommandInfo(varargin)
            p = inputParser;
            p.addParameter('AllowDuringAgentExecution', obj.AllowDuringAgentExecution);
            p.addParameter('Description', obj.Description);
            p.addParameter('Kind', obj.Kind);
            p.addParameter('Name', obj.Name);
            p.addParameter('Aliases', obj.Aliases);
            p.addParameter('Experimental', []);
            p.addParameter('Input', []);
            p.parse(varargin{:});
            obj.AllowDuringAgentExecution = p.Results.AllowDuringAgentExecution;
            obj.Description = p.Results.Description;
            obj.Kind = p.Results.Kind;
            obj.Name = p.Results.Name;
            obj.Aliases = p.Results.Aliases;
            obj.Experimental = p.Results.Experimental;
            obj.Input = p.Results.Input;
        end

        function s = toStruct(obj)
            s = struct();
            s.allowDuringAgentExecution = obj.AllowDuringAgentExecution;
            s.description = obj.Description;
            s.kind = obj.Kind;
            s.name = obj.Name;
            if ~isempty(obj.Aliases)
                s.aliases = obj.Aliases;
            end
            if ~isempty(obj.Experimental)
                s.experimental = obj.Experimental;
            end
            if ~isempty(obj.Input)
                s.input = obj.Input.toStruct();
            end
        end
    end
end

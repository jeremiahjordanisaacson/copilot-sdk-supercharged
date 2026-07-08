classdef SlashCommandInput
    % SlashCommandInput  Input specification for a slash command.

    properties
        % Hint string for the command input.
        Hint (1,:) char = ''

        % Completion type (e.g. 'directory').
        Completion (1,:) char = ''
    end

    methods
        function obj = SlashCommandInput(varargin)
            p = inputParser;
            p.addParameter('Hint', obj.Hint);
            p.addParameter('Completion', obj.Completion);
            p.parse(varargin{:});
            obj.Hint = p.Results.Hint;
            obj.Completion = p.Results.Completion;
        end

        function s = toStruct(obj)
            s = struct('hint', obj.Hint);
            if ~isempty(obj.Completion)
                s.completion = obj.Completion;
            end
        end
    end
end

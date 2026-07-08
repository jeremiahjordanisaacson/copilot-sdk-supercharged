classdef UserInputRequest
    % UserInputRequest  Request for user input from the agent.

    properties
        Question (1,:) char = ''
        Choices (1,:) cell = {}
        AllowFreeform = []
    end

    methods
        function obj = UserInputRequest(varargin)
            p = inputParser;
            p.addParameter('Question', obj.Question);
            p.addParameter('Choices', obj.Choices);
            p.addParameter('AllowFreeform', obj.AllowFreeform);
            p.parse(varargin{:});
            obj.Question = p.Results.Question;
            obj.Choices = p.Results.Choices;
            obj.AllowFreeform = p.Results.AllowFreeform;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Question)
                s.question = obj.Question;
            end
            if ~isempty(obj.Choices)
                s.choices = obj.Choices;
            end
            if ~isempty(obj.AllowFreeform)
                s.allowFreeform = obj.AllowFreeform;
            end
        end
    end
end

classdef UserInputResponse
    % UserInputResponse  Response to a user-input request.

    properties
        Answer (1,:) char = ''
        WasFreeform (1,1) logical = false
    end

    methods
        function obj = UserInputResponse(varargin)
            p = inputParser;
            p.addParameter('Answer', obj.Answer);
            p.addParameter('WasFreeform', obj.WasFreeform);
            p.parse(varargin{:});
            obj.Answer = p.Results.Answer;
            obj.WasFreeform = p.Results.WasFreeform;
        end

        function s = toStruct(obj)
            s = struct('answer', obj.Answer, 'wasFreeform', obj.WasFreeform);
        end
    end
end

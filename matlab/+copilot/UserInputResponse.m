classdef UserInputResponse
    % UserInputResponse  Response to a user-input request.

    properties
        Answer (1,:) char = ''
        WasFreeform (1,1) logical = false

        % Images attached to the answer (cell of copilot.UserInputImageAttachment).
        % Only valid when the request advertised ImageAttachmentConstraints;
        % sent only when non-empty.  Image-only answers use 'Answer', '' and
        % 'WasFreeform', true.
        Attachments (1,:) cell = {}
    end

    methods
        function obj = UserInputResponse(varargin)
            p = inputParser;
            p.addParameter('Answer', obj.Answer);
            p.addParameter('WasFreeform', obj.WasFreeform);
            p.addParameter('Attachments', obj.Attachments);
            p.parse(varargin{:});
            obj.Answer = p.Results.Answer;
            obj.WasFreeform = p.Results.WasFreeform;
            obj.Attachments = p.Results.Attachments;
        end

        function s = toStruct(obj)
            s = struct('answer', obj.Answer, 'wasFreeform', obj.WasFreeform);
            if ~isempty(obj.Attachments)
                items = obj.Attachments;
                for i = 1:numel(items)
                    if isobject(items{i}) && ismethod(items{i}, 'toStruct')
                        items{i} = items{i}.toStruct();
                    end
                end
                s.attachments = items;
            end
        end
    end
end

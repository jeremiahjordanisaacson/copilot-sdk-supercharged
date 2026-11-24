classdef UserInputRequest
    % UserInputRequest  Request for user input from the agent.
    %
    %   SessionConfig 'OnUserInputRequest' handlers receive this object for
    %   each userInput.request (the ask_user tool).

    properties
        Question (1,:) char = ''
        Choices (1,:) cell = {}
        AllowFreeform = []

        % Runtime id of the pending request ('' when not sent) (wire: requestId).
        RequestId (1,:) char = ''

        % copilot.UserInputImageAttachmentConstraints when the runtime accepts
        % image attachments in the answer, [] otherwise.
        ImageAttachmentConstraints = []

        % The request's opaque _meta object as decoded by jsondecode, or [].
        Meta = []
    end

    methods
        function obj = UserInputRequest(varargin)
            p = inputParser;
            p.addParameter('Question', obj.Question);
            p.addParameter('Choices', obj.Choices);
            p.addParameter('AllowFreeform', obj.AllowFreeform);
            p.addParameter('RequestId', obj.RequestId);
            p.addParameter('ImageAttachmentConstraints', []);
            p.addParameter('Meta', []);
            p.parse(varargin{:});
            obj.Question = p.Results.Question;
            obj.Choices = p.Results.Choices;
            obj.AllowFreeform = p.Results.AllowFreeform;
            obj.RequestId = p.Results.RequestId;
            obj.ImageAttachmentConstraints = p.Results.ImageAttachmentConstraints;
            obj.Meta = p.Results.Meta;
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
            if ~isempty(obj.RequestId)
                s.requestId = obj.RequestId;
            end
            if ~isempty(obj.ImageAttachmentConstraints)
                constraints = obj.ImageAttachmentConstraints;
                if isobject(constraints) && ismethod(constraints, 'toStruct')
                    constraints = constraints.toStruct();
                end
                s.imageAttachmentConstraints = constraints;
            end
            % Meta is not serialized: _meta is not a valid MATLAB field name.
        end
    end

    methods (Static)
        function obj = fromStruct(s)
            %fromStruct  Build from decoded userInput.request params.
            obj = copilot.UserInputRequest();
            if ~isstruct(s)
                return;
            end
            if isfield(s, 'question') && ~isempty(s.question)
                obj.Question = char(s.question);
            end
            if isfield(s, 'choices') && ~isempty(s.choices)
                obj.Choices = reshape(cellstr(s.choices), 1, []);
            end
            if isfield(s, 'allowFreeform') && ~isempty(s.allowFreeform)
                obj.AllowFreeform = logical(s.allowFreeform);
            end
            if isfield(s, 'requestId') && ~isempty(s.requestId)
                obj.RequestId = char(s.requestId);
            end
            if isfield(s, 'imageAttachmentConstraints') && isstruct(s.imageAttachmentConstraints)
                obj.ImageAttachmentConstraints = ...
                    copilot.UserInputImageAttachmentConstraints.fromStruct( ...
                        s.imageAttachmentConstraints);
            end
            % jsondecode renames the "_meta" key to x_meta.
            if isfield(s, 'x_meta')
                obj.Meta = s.x_meta;
            end
        end
    end
end

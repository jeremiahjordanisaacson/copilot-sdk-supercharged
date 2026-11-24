classdef UserInputImageAttachment
    % UserInputImageAttachment  Inline image attached to an ask_user answer.
    %
    %   image = copilot.UserInputImageAttachment( ...
    %       'Data', matlab.net.base64encode(bytes), 'MimeType', 'image/png');
    %   response = copilot.UserInputResponse('Answer', '', 'WasFreeform', true, ...
    %       'Attachments', {image});
    %
    %   Only valid when the request advertised ImageAttachmentConstraints.
    %   The wire type is always 'image'; the runtime enforces the MIME
    %   types, size and count limits.

    properties (Constant)
        % Wire type of every image attachment.
        Type (1,:) char = 'image'
    end

    properties
        % Base64-encoded image bytes.
        Data (1,:) char = ''

        % MIME type of the image, for example 'image/png' (wire: mimeType).
        MimeType (1,:) char = ''

        % Optional description of the image.
        Description (1,:) char = ''

        % Optional display name, for example a file name (wire: displayName).
        DisplayName (1,:) char = ''
    end

    methods
        function obj = UserInputImageAttachment(varargin)
            p = inputParser;
            p.addParameter('Data', obj.Data);
            p.addParameter('MimeType', obj.MimeType);
            p.addParameter('Description', obj.Description);
            p.addParameter('DisplayName', obj.DisplayName);
            p.parse(varargin{:});
            obj.Data = p.Results.Data;
            obj.MimeType = p.Results.MimeType;
            obj.Description = p.Results.Description;
            obj.DisplayName = p.Results.DisplayName;
        end

        function s = toStruct(obj)
            s = struct('type', obj.Type, 'data', obj.Data, 'mimeType', obj.MimeType);
            if ~isempty(obj.Description)
                s.description = obj.Description;
            end
            if ~isempty(obj.DisplayName)
                s.displayName = obj.DisplayName;
            end
        end
    end
end

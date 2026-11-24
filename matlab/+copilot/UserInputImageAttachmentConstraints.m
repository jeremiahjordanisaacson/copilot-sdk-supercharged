classdef UserInputImageAttachmentConstraints
    % UserInputImageAttachmentConstraints  Image support advertised by a
    % user input request (UserInputRequest.ImageAttachmentConstraints).
    %
    %   The request carries these constraints only when the runtime accepts
    %   image attachments in the answer (see copilot.UserInputImageAttachment).
    %   The runtime enforces the limits.

    properties
        % Accepted MIME types, for example {'image/png', 'image/jpeg'}.
        AllowedMimeTypes (1,:) cell = {}

        % Maximum decoded size of each image in bytes (wire: maxBytesPerImage).
        MaxBytesPerImage (1,1) double = 0

        % Maximum number of images in one answer (wire: maxCount).
        MaxCount (1,1) double = 0
    end

    methods
        function obj = UserInputImageAttachmentConstraints(varargin)
            p = inputParser;
            p.addParameter('AllowedMimeTypes', obj.AllowedMimeTypes);
            p.addParameter('MaxBytesPerImage', obj.MaxBytesPerImage);
            p.addParameter('MaxCount', obj.MaxCount);
            p.parse(varargin{:});
            obj.AllowedMimeTypes = p.Results.AllowedMimeTypes;
            obj.MaxBytesPerImage = p.Results.MaxBytesPerImage;
            obj.MaxCount = p.Results.MaxCount;
        end

        function s = toStruct(obj)
            s = struct('allowedMimeTypes', {obj.AllowedMimeTypes}, ...
                'maxBytesPerImage', obj.MaxBytesPerImage, 'maxCount', obj.MaxCount);
        end
    end

    methods (Static)
        function obj = fromStruct(s)
            %fromStruct  Build from a decoded imageAttachmentConstraints object.
            obj = copilot.UserInputImageAttachmentConstraints();
            if ~isstruct(s)
                return;
            end
            if isfield(s, 'allowedMimeTypes') && ~isempty(s.allowedMimeTypes)
                obj.AllowedMimeTypes = reshape(cellstr(s.allowedMimeTypes), 1, []);
            end
            if isfield(s, 'maxBytesPerImage') && ~isempty(s.maxBytesPerImage)
                obj.MaxBytesPerImage = double(s.maxBytesPerImage);
            end
            if isfield(s, 'maxCount') && ~isempty(s.maxCount)
                obj.MaxCount = double(s.maxCount);
            end
        end
    end
end

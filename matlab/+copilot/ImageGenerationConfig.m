classdef ImageGenerationConfig
    % ImageGenerationConfig  Image generation settings (wire: tools.imageGeneration).
    %
    %   img = copilot.ImageGenerationConfig('Enabled', true, 'SaveToDisk', false);
    %   cfg = copilot.SessionConfig('ImageGeneration', img);
    %
    %   Each option is omitted from the payload while it is empty ([]).

    properties
        % Opt in to image generation through an authorized Copilot image model.
        Enabled = []

        % Request a visible "Made with AI" label on generated images.
        Watermark = []

        % Save generated images under the session-state images folder (wire: saveToDisk).
        SaveToDisk = []
    end

    methods
        function obj = ImageGenerationConfig(varargin)
            p = inputParser;
            p.addParameter('Enabled',    []);
            p.addParameter('Watermark',  []);
            p.addParameter('SaveToDisk', []);
            p.parse(varargin{:});
            obj.Enabled    = p.Results.Enabled;
            obj.Watermark  = p.Results.Watermark;
            obj.SaveToDisk = p.Results.SaveToDisk;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct();
            if ~isempty(obj.Enabled)
                s.enabled = logical(obj.Enabled);
            end
            if ~isempty(obj.Watermark)
                s.watermark = logical(obj.Watermark);
            end
            if ~isempty(obj.SaveToDisk)
                s.saveToDisk = logical(obj.SaveToDisk);
            end
        end
    end
end

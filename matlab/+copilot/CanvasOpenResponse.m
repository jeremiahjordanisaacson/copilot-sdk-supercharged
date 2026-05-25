classdef CanvasOpenResponse
    % CanvasOpenResponse  Response returned by a canvas open handler.

    properties
        Url (1,:) char = ''
        Title (1,:) char = ''
        Status (1,:) char = ''
    end

    methods
        function obj = CanvasOpenResponse(varargin)
            p = inputParser;
            p.addParameter('Url', obj.Url);
            p.addParameter('Title', obj.Title);
            p.addParameter('Status', obj.Status);
            p.parse(varargin{:});
            obj.Url = p.Results.Url;
            obj.Title = p.Results.Title;
            obj.Status = p.Results.Status;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Url)
                s.url = obj.Url;
            end
            if ~isempty(obj.Title)
                s.title = obj.Title;
            end
            if ~isempty(obj.Status)
                s.status = obj.Status;
            end
        end
    end
end

classdef CanvasAction
    % CanvasAction  Metadata for an agent-callable canvas action.

    properties
        Name (1,:) char = ''
        Description (1,:) char = ''
        InputSchema
    end

    methods
        function obj = CanvasAction(varargin)
            p = inputParser;
            p.addParameter('Name', obj.Name);
            p.addParameter('Description', obj.Description);
            p.addParameter('InputSchema', struct());
            p.parse(varargin{:});
            obj.Name = p.Results.Name;
            obj.Description = p.Results.Description;
            obj.InputSchema = p.Results.InputSchema;
        end

        function s = toStruct(obj)
            s = struct('name', obj.Name, 'description', obj.Description);
            if ~isempty(obj.InputSchema)
                s.inputSchema = obj.InputSchema;
            end
        end
    end
end

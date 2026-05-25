classdef CanvasDeclaration
    % CanvasDeclaration  Declarative metadata for a canvas.

    properties
        Id (1,:) char = ''
        DisplayName (1,:) char = ''
        Description (1,:) char = ''
        InputSchema
        Actions (1,:) cell = {}
    end

    methods
        function obj = CanvasDeclaration(varargin)
            p = inputParser;
            p.addParameter('Id', obj.Id);
            p.addParameter('DisplayName', obj.DisplayName);
            p.addParameter('Description', obj.Description);
            p.addParameter('InputSchema', struct());
            p.addParameter('Actions', obj.Actions);
            p.parse(varargin{:});
            obj.Id = p.Results.Id;
            obj.DisplayName = p.Results.DisplayName;
            obj.Description = p.Results.Description;
            obj.InputSchema = p.Results.InputSchema;
            obj.Actions = p.Results.Actions;
        end

        function s = toStruct(obj)
            s = struct('id', obj.Id, 'displayName', obj.DisplayName, 'description', obj.Description);
            if ~isempty(obj.InputSchema)
                s.inputSchema = obj.InputSchema;
            end
            if ~isempty(obj.Actions)
                actions = cell(1, numel(obj.Actions));
                for i = 1:numel(obj.Actions)
                    actions{i} = obj.Actions{i}.toStruct();
                end
                s.actions = actions;
            end
        end
    end
end

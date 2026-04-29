classdef DefineTool
    % DefineTool  Declare a tool that the Copilot agent can invoke.
    %
    %   tool = copilot.DefineTool( ...
    %       'Name',        'get_weather', ...
    %       'Description', 'Return weather for a city', ...
    %       'Parameters',  struct('type','object', ...
    %                             'properties', struct('city', struct('type','string')), ...
    %                             'required', {{'city'}}), ...
    %       'Handler',     @(params) getWeather(params.city));

    properties
        % Unique tool name (snake_case recommended).
        Name (1,:) char = ''

        % Human-readable description shown to the agent.
        Description (1,:) char = ''

        % JSON Schema describing the tool parameters (struct).
        Parameters (1,1) struct = struct('type', 'object', 'properties', struct())

        % Function handle invoked when the agent calls the tool.
        % Signature: result = handler(paramsStruct)
        Handler function_handle = @(~) struct()

        % When true, this tool overrides a built-in tool with the same name.
        OverridesBuiltInTool (1,1) logical = false

        % When true, the SDK will not request permission before execution.
        SkipPermission (1,1) logical = false
    end

    methods
        function obj = DefineTool(varargin)
            %DefineTool  Create a tool definition using name-value pairs.

            p = inputParser;
            p.addParameter('Name',                 obj.Name);
            p.addParameter('Description',          obj.Description);
            p.addParameter('Parameters',           obj.Parameters);
            p.addParameter('Handler',              obj.Handler);
            p.addParameter('OverridesBuiltInTool', obj.OverridesBuiltInTool);
            p.addParameter('SkipPermission',       obj.SkipPermission);
            p.parse(varargin{:});

            obj.Name                = p.Results.Name;
            obj.Description         = p.Results.Description;
            obj.Parameters          = p.Results.Parameters;
            obj.Handler             = p.Results.Handler;
            obj.OverridesBuiltInTool = p.Results.OverridesBuiltInTool;
            obj.SkipPermission      = p.Results.SkipPermission;
        end

        function s = toStruct(obj)
            %toStruct  Convert the tool to a struct suitable for JSON-RPC.
            s.name        = obj.Name;
            s.description = obj.Description;
            s.parameters  = obj.Parameters;
            if obj.OverridesBuiltInTool
                s.overridesBuiltInTool = true;
            end
            if obj.SkipPermission
                s.skipPermission = true;
            end
        end
    end
end

classdef CanvasOpenContext
    % CanvasOpenContext  Context passed to a canvas open handler.

    properties
        SessionId (1,:) char = ''
        ExtensionId (1,:) char = ''
        CanvasId (1,:) char = ''
        InstanceId (1,:) char = ''
        Input
        Host
    end

    methods
        function obj = CanvasOpenContext(varargin)
            p = inputParser;
            p.addParameter('SessionId', obj.SessionId);
            p.addParameter('ExtensionId', obj.ExtensionId);
            p.addParameter('CanvasId', obj.CanvasId);
            p.addParameter('InstanceId', obj.InstanceId);
            p.addParameter('Input', []);
            p.addParameter('Host', []);
            p.parse(varargin{:});
            obj.SessionId = p.Results.SessionId;
            obj.ExtensionId = p.Results.ExtensionId;
            obj.CanvasId = p.Results.CanvasId;
            obj.InstanceId = p.Results.InstanceId;
            obj.Input = p.Results.Input;
            obj.Host = p.Results.Host;
        end

        function s = toStruct(obj)
            s = struct('sessionId', obj.SessionId, 'extensionId', obj.ExtensionId, ...
                'canvasId', obj.CanvasId, 'instanceId', obj.InstanceId, 'input', obj.Input);
            if ~isempty(obj.Host)
                s.host = obj.Host.toStruct();
            end
        end
    end
end

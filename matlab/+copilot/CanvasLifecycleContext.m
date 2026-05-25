classdef CanvasLifecycleContext
    % CanvasLifecycleContext  Lifecycle context for a canvas instance.

    properties
        SessionId (1,:) char = ''
        ExtensionId (1,:) char = ''
        CanvasId (1,:) char = ''
        InstanceId (1,:) char = ''
        Host
    end

    methods
        function obj = CanvasLifecycleContext(varargin)
            p = inputParser;
            p.addParameter('SessionId', obj.SessionId);
            p.addParameter('ExtensionId', obj.ExtensionId);
            p.addParameter('CanvasId', obj.CanvasId);
            p.addParameter('InstanceId', obj.InstanceId);
            p.addParameter('Host', []);
            p.parse(varargin{:});
            obj.SessionId = p.Results.SessionId;
            obj.ExtensionId = p.Results.ExtensionId;
            obj.CanvasId = p.Results.CanvasId;
            obj.InstanceId = p.Results.InstanceId;
            obj.Host = p.Results.Host;
        end

        function s = toStruct(obj)
            s = struct('sessionId', obj.SessionId, 'extensionId', obj.ExtensionId, ...
                'canvasId', obj.CanvasId, 'instanceId', obj.InstanceId);
            if ~isempty(obj.Host)
                s.host = obj.Host.toStruct();
            end
        end
    end
end

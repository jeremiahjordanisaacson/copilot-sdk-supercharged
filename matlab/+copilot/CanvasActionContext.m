classdef CanvasActionContext
    % CanvasActionContext  Context passed to a canvas action handler.

    properties
        SessionId (1,:) char = ''
        ExtensionId (1,:) char = ''
        CanvasId (1,:) char = ''
        InstanceId (1,:) char = ''
        ActionName (1,:) char = ''
        Input
        Host
    end

    methods
        function obj = CanvasActionContext(varargin)
            p = inputParser;
            p.addParameter('SessionId', obj.SessionId);
            p.addParameter('ExtensionId', obj.ExtensionId);
            p.addParameter('CanvasId', obj.CanvasId);
            p.addParameter('InstanceId', obj.InstanceId);
            p.addParameter('ActionName', obj.ActionName);
            p.addParameter('Input', []);
            p.addParameter('Host', []);
            p.parse(varargin{:});
            obj.SessionId = p.Results.SessionId;
            obj.ExtensionId = p.Results.ExtensionId;
            obj.CanvasId = p.Results.CanvasId;
            obj.InstanceId = p.Results.InstanceId;
            obj.ActionName = p.Results.ActionName;
            obj.Input = p.Results.Input;
            obj.Host = p.Results.Host;
        end

        function s = toStruct(obj)
            s = struct('sessionId', obj.SessionId, 'extensionId', obj.ExtensionId, ...
                'canvasId', obj.CanvasId, 'instanceId', obj.InstanceId, ...
                'actionName', obj.ActionName, 'input', obj.Input);
            if ~isempty(obj.Host)
                s.host = obj.Host.toStruct();
            end
        end
    end
end

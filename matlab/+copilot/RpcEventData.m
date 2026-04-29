classdef RpcEventData < event.EventData
    % RpcEventData  Carries a JSON-RPC notification to event listeners.

    properties
        Method (1,:) char   % e.g. 'session/event'
        Payload             % Decoded JSON struct
    end

    methods
        function obj = RpcEventData(method, payload)
            obj.Method  = method;
            obj.Payload = payload;
        end
    end
end

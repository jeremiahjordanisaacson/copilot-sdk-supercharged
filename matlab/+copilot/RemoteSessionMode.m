classdef RemoteSessionMode
    % RemoteSessionMode  Experimental
    %   Mode for remote session control.

    enumeration
        Export  ('export')
        Off     ('off')
        On      ('on')
    end

    properties (SetAccess = immutable)
        Value (1,:) char
    end

    methods
        function obj = RemoteSessionMode(val)
            obj.Value = val;
        end
    end
end

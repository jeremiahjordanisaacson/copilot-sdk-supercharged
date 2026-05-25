classdef ResponseFormat
    % ResponseFormat  Response format for messages.

    enumeration
        Text       ('text')
        Image      ('image')
        JsonObject ('json_object')
    end

    properties (SetAccess = immutable)
        Value (1,:) char
    end

    methods
        function obj = ResponseFormat(val)
            obj.Value = val;
        end
    end
end

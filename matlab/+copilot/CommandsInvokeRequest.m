classdef CommandsInvokeRequest
    % CommandsInvokeRequest  Request to invoke a command.

    properties
        % Name of the command to invoke.
        Name (1,:) char = ''

        % Input string for the command.
        Input (1,:) char = ''
    end

    methods
        function obj = CommandsInvokeRequest(varargin)
            p = inputParser;
            p.addParameter('Name', obj.Name);
            p.addParameter('Input', obj.Input);
            p.parse(varargin{:});
            obj.Name = p.Results.Name;
            obj.Input = p.Results.Input;
        end

        function s = toStruct(obj)
            s = struct('name', obj.Name);
            if ~isempty(obj.Input)
                s.input = obj.Input;
            end
        end
    end
end

classdef RemoteEnableResult
    % RemoteEnableResult  Experimental
    %   Result of enabling remote session mode.

    properties
        % Whether the session is remotely steerable.
        RemoteSteerable (1,1) logical = false

        % URL for the remote session (char or empty).
        Url (1,:) char = ''
    end

    methods
        function obj = RemoteEnableResult(varargin)
            p = inputParser;
            p.addParameter('RemoteSteerable', obj.RemoteSteerable);
            p.addParameter('Url', obj.Url);
            p.parse(varargin{:});
            obj.RemoteSteerable = p.Results.RemoteSteerable;
            obj.Url = p.Results.Url;
        end

        function s = toStruct(obj)
            s = struct('remoteSteerable', obj.RemoteSteerable);
            if ~isempty(obj.Url)
                s.url = obj.Url;
            end
        end
    end
end

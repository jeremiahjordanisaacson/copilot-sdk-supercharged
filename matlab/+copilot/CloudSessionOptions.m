classdef CloudSessionOptions
    % CloudSessionOptions  Options for creating a cloud session.

    properties
        Repository
    end

    methods
        function obj = CloudSessionOptions(varargin)
            p = inputParser;
            p.addParameter('Repository', []);
            p.parse(varargin{:});
            obj.Repository = p.Results.Repository;
        end

        function s = toStruct(obj)
            s = struct();
            if ~isempty(obj.Repository)
                s.repository = obj.Repository.toStruct();
            end
        end
    end
end

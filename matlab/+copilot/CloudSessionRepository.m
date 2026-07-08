classdef CloudSessionRepository
    % CloudSessionRepository  Repository metadata associated with a cloud session.

    properties
        Owner (1,:) char = ''
        Name (1,:) char = ''
        Branch (1,:) char = ''
    end

    methods
        function obj = CloudSessionRepository(varargin)
            p = inputParser;
            p.addParameter('Owner', obj.Owner);
            p.addParameter('Name', obj.Name);
            p.addParameter('Branch', obj.Branch);
            p.parse(varargin{:});
            obj.Owner = p.Results.Owner;
            obj.Name = p.Results.Name;
            obj.Branch = p.Results.Branch;
        end

        function s = toStruct(obj)
            s = struct('owner', obj.Owner, 'name', obj.Name);
            if ~isempty(obj.Branch)
                s.branch = obj.Branch;
            end
        end
    end
end

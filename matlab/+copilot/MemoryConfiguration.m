classdef MemoryConfiguration
    % MemoryConfiguration  Persistent cross-turn session memory configuration.
    %
    %   Parity with @github/copilot-sdk. Controls whether the session persists
    %   memory across turns and where that memory is stored.
    %
    %   mem = copilot.MemoryConfiguration('Enabled', true, 'MaxEntries', 50);

    properties
        % Enable persistent session memory.
        Enabled (1,1) logical = false

        % Maximum retained memory entries (0 = server default).
        MaxEntries (1,1) double = 0

        % Directory used for memory storage ('' = server default).
        Directory (1,:) char = ''
    end

    methods
        function obj = MemoryConfiguration(varargin)
            %MemoryConfiguration  Create a session-memory configuration.
            p = inputParser;
            p.addParameter('Enabled',    obj.Enabled);
            p.addParameter('MaxEntries', obj.MaxEntries);
            p.addParameter('Directory',  obj.Directory);
            p.parse(varargin{:});
            obj.Enabled    = p.Results.Enabled;
            obj.MaxEntries = p.Results.MaxEntries;
            obj.Directory  = p.Results.Directory;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct('enabled', obj.Enabled);
            if obj.MaxEntries > 0
                s.maxEntries = obj.MaxEntries;
            end
            if ~isempty(obj.Directory)
                s.directory = obj.Directory;
            end
        end
    end
end

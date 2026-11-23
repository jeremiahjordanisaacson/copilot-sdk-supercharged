classdef SandboxConfig
    % SandboxConfig  Command sandbox settings (wire: permissions.sandbox).
    %
    %   sandbox = copilot.SandboxConfig('Enabled', true, 'AllowBypass', true);
    %   cfg = copilot.SessionConfig('Sandbox', sandbox);
    %
    %   AllowBypass lets the agent ask to run a single command outside the
    %   sandbox.  Every such request still goes through the permission flow.

    properties
        % Whether sandboxing is enabled for the session.
        Enabled (1,1) logical = false

        % Let the agent request an unsandboxed run (wire: allowBypass).
        AllowBypass (1,1) logical = false

        % Auto-add the working directory to the read-write paths ([] = default).
        AddCurrentWorkingDirectory = []

        % User policy fragment merged into the base policy (struct or []).
        UserPolicy = []
    end

    methods
        function obj = SandboxConfig(varargin)
            p = inputParser;
            p.addParameter('Enabled',                    obj.Enabled);
            p.addParameter('AllowBypass',                obj.AllowBypass);
            p.addParameter('AddCurrentWorkingDirectory', []);
            p.addParameter('UserPolicy',                 []);
            p.parse(varargin{:});
            obj.Enabled                    = p.Results.Enabled;
            obj.AllowBypass                = p.Results.AllowBypass;
            obj.AddCurrentWorkingDirectory = p.Results.AddCurrentWorkingDirectory;
            obj.UserPolicy                 = p.Results.UserPolicy;
        end

        function s = toStruct(obj)
            %toStruct  Serialize for JSON-RPC (camelCase wire keys).
            s = struct('enabled', obj.Enabled);
            if obj.AllowBypass
                s.allowBypass = true;
            end
            if ~isempty(obj.AddCurrentWorkingDirectory)
                s.addCurrentWorkingDirectory = logical(obj.AddCurrentWorkingDirectory);
            end
            if ~isempty(obj.UserPolicy)
                s.userPolicy = obj.UserPolicy;
            end
        end
    end
end

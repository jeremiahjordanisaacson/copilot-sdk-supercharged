classdef PermissionsExtensionApproval
    % PermissionsExtensionApproval  User-scoped approval for one installed
    % plugin extension.
    %
    %   Returned by CopilotClient.extensionApprovalsGrant and
    %   CopilotClient.extensionApprovalsList.  Pass ExtensionName and
    %   InstallationKey back unchanged to CopilotClient.extensionApprovalsRevoke.

    properties
        % Stable identity, for example 'plugin:plugin-name:extension-name'.
        ExtensionName (1,:) char = ''

        % Plugin that provides the extension.
        PluginName (1,:) char = ''

        % Exact privileged capabilities the user approved (cell of char).
        Capabilities (1,:) cell = {}

        % When the approval was granted (RFC 3339 timestamp).
        GrantedAt (1,:) char = ''

        % Opaque installation key (wire: installationKey).
        InstallationKey (1,:) char = ''

        % Human-readable installation label ('' when absent).
        InstallationLabel (1,:) char = ''
    end

    methods (Static)
        function approval = fromStruct(s)
            %fromStruct  Build from a decoded PermissionsExtensionApproval object.
            approval = copilot.PermissionsExtensionApproval();
            if ~isstruct(s)
                return;
            end
            textFields = {'extensionName', 'ExtensionName'; 'pluginName', 'PluginName'; ...
                    'grantedAt', 'GrantedAt'; 'installationKey', 'InstallationKey'; ...
                    'installationLabel', 'InstallationLabel'};
            for i = 1:size(textFields, 1)
                if isfield(s, textFields{i, 1}) && ~isempty(s.(textFields{i, 1}))
                    approval.(textFields{i, 2}) = char(s.(textFields{i, 1}));
                end
            end
            if isfield(s, 'capabilities') && ~isempty(s.capabilities)
                approval.Capabilities = reshape(cellstr(s.capabilities), 1, []);
            end
        end
    end
end

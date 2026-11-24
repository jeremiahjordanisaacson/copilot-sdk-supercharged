classdef PermissionsExtensionApprovalsGrantResult
    % PermissionsExtensionApprovalsGrantResult  Result of
    % CopilotClient.extensionApprovalsGrant.

    properties
        % True when the pending request was eligible and persisted.
        Granted (1,1) logical = false

        % The stored approval (copilot.PermissionsExtensionApproval), or []
        % when none was persisted.
        Approval = []
    end

    methods (Static)
        function result = fromStruct(s)
            %fromStruct  Build from the decoded permissions.extensionApprovals.grant result.
            result = copilot.PermissionsExtensionApprovalsGrantResult();
            if ~isstruct(s)
                return;
            end
            if isfield(s, 'granted') && ~isempty(s.granted)
                result.Granted = logical(s.granted);
            end
            if isfield(s, 'approval') && isstruct(s.approval)
                result.Approval = copilot.PermissionsExtensionApproval.fromStruct(s.approval);
            end
        end
    end
end

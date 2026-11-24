classdef PermissionsExtensionApprovalsRevokeResult
    % PermissionsExtensionApprovalsRevokeResult  Result of
    % CopilotClient.extensionApprovalsRevoke.

    properties
        % True when a matching approval was revoked.
        Revoked (1,1) logical = false
    end

    methods (Static)
        function result = fromStruct(s)
            %fromStruct  Build from the decoded permissions.extensionApprovals.revoke result.
            result = copilot.PermissionsExtensionApprovalsRevokeResult();
            if isstruct(s) && isfield(s, 'revoked') && ~isempty(s.revoked)
                result.Revoked = logical(s.revoked);
            end
        end
    end
end

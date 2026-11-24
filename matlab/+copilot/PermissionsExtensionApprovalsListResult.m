classdef PermissionsExtensionApprovalsListResult
    % PermissionsExtensionApprovalsListResult  Result of
    % CopilotClient.extensionApprovalsList.

    properties
        % The approvals (cell of copilot.PermissionsExtensionApproval).
        Approvals (1,:) cell = {}
    end

    methods (Static)
        function result = fromStruct(s)
            %fromStruct  Build from the decoded permissions.extensionApprovals.list result.
            result = copilot.PermissionsExtensionApprovalsListResult();
            if ~isstruct(s) || ~isfield(s, 'approvals')
                return;
            end
            % jsondecode returns a struct array when every approval has the
            % same fields and a cell array otherwise.
            items = s.approvals;
            if isstruct(items)
                items = num2cell(items);
            end
            if ~iscell(items)
                return;
            end
            for i = 1:numel(items)
                if isstruct(items{i})
                    result.Approvals{end+1} = ...
                        copilot.PermissionsExtensionApproval.fromStruct(items{i});
                end
            end
        end
    end
end

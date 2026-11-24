classdef McpSecretsSaveResult
    % McpSecretsSaveResult  Result of CopilotClient.mcpSecretsSave.
    %
    %   result = client.mcpSecretsSave('my-server-token', token);
    %   if result.UsedKeychain
    %       disp('Stored in the OS keychain');
    %   end

    properties
        % True when the OS keychain stored the credential, false when the
        % runtime used its file backend (wire: usedKeychain).
        UsedKeychain (1,1) logical = false
    end

    methods (Static)
        function result = fromStruct(s)
            %fromStruct  Build from the decoded mcp.secrets.save result.
            result = copilot.McpSecretsSaveResult();
            if isstruct(s) && isfield(s, 'usedKeychain') && ~isempty(s.usedKeychain)
                result.UsedKeychain = logical(s.usedKeychain);
            end
        end
    end
end

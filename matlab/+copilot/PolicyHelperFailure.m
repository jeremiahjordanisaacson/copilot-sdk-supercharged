classdef PolicyHelperFailure
    % PolicyHelperFailure  Structured enterprise Policy Helper failure.
    %
    %   Read from error.data.policyHelperFailure when session.create or
    %   session.resume fails:
    %
    %   try
    %       session = client.createSession(cfg);
    %   catch me
    %       failure = client.LastPolicyHelperFailure;
    %       if ~isempty(failure)
    %           fprintf('%s (%s)\n', failure.Message, failure.Category);
    %       end
    %   end
    %
    %   Helper-authored text is untrusted: display it as plain text only.

    properties
        Category (1,:) char = ''
        FailureMode (1,:) char = ''
        Message (1,:) char = ''
        PreviousPolicyRetained (1,1) logical = false
        Reason (1,:) char = ''
        Severity (1,:) char = ''
        Authority (1,:) char = ''
        Code (1,:) char = ''
        Details = []
        Diagnostic (1,:) char = ''
        HelpLink = []
        Remediation (1,:) char = ''
    end

    methods (Static)
        function failure = fromRpcError(rpcError)
            %fromRpcError  Decode error.data.policyHelperFailure ([] if absent).
            failure = [];
            if ~isstruct(rpcError) || ~isfield(rpcError, 'data') ...
                    || ~isstruct(rpcError.data) ...
                    || ~isfield(rpcError.data, 'policyHelperFailure')
                return;
            end
            failure = copilot.PolicyHelperFailure.fromStruct( ...
                rpcError.data.policyHelperFailure);
        end

        function failure = fromStruct(s)
            %fromStruct  Build from a decoded PolicyHelperFailure object.
            failure = copilot.PolicyHelperFailure();
            if ~isstruct(s)
                return;
            end
            textFields = {'category', 'Category'; 'failureMode', 'FailureMode'; ...
                    'message', 'Message'; 'reason', 'Reason'; ...
                    'severity', 'Severity'; 'authority', 'Authority'; ...
                    'code', 'Code'; 'diagnostic', 'Diagnostic'; ...
                    'remediation', 'Remediation'};
            for i = 1:size(textFields, 1)
                if isfield(s, textFields{i, 1})
                    failure.(textFields{i, 2}) = char(s.(textFields{i, 1}));
                end
            end
            if isfield(s, 'previousPolicyRetained')
                failure.PreviousPolicyRetained = logical(s.previousPolicyRetained);
            end
            if isfield(s, 'details')
                failure.Details = s.details;
            end
            if isfield(s, 'helpLink')
                failure.HelpLink = s.helpLink;
            end
        end
    end
end

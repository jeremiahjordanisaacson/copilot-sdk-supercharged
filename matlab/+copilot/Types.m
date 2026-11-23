classdef Types
    % Types  Enumerated identifiers and constants for upstream-sync features.
    %
    %   Parity with @github/copilot-sdk (2026-07). Provides the constant
    %   identifiers referenced by SessionConfig / MessageOptions passthroughs
    %   (system-message sections, tool defer-loading strategies, GitHub
    %   attachment variants, and OTLP telemetry protocols).
    %
    %   Example:
    %       copilot.Types.SystemMessagePreamble   % 'preamble'
    %       copilot.Types.GitHubCommit            % 'GitHubCommit'

    properties (Constant)
        % --- System message section names ---
        % A "preamble" section is prepended; a "preserve" section survives
        % context compaction.
        SystemMessagePreamble (1,:) char = 'preamble'
        SystemMessagePreserve (1,:) char = 'preserve'

        % --- Tool defer-loading strategies ---
        % Eager tools load with the session; deferred tools load lazily.
        ToolDeferEager (1,:) char = 'eager'
        ToolDeferLazy  (1,:) char = 'lazy'

        % --- GitHub attachment variants ---
        GitHubCommit      (1,:) char = 'GitHubCommit'
        GitHubRepository  (1,:) char = 'GitHubRepository'
        GitHubPullRequest (1,:) char = 'GitHubPullRequest'
        GitHubIssue       (1,:) char = 'GitHubIssue'

        % --- OTLP telemetry protocols ---
        OtlpProtocolGrpc (1,:) char = 'grpc'
        OtlpProtocolHttp (1,:) char = 'http/protobuf'

        % --- SDK protocol 4 / v3.0.0 ---
        % Client modes (CopilotClientOptions 'Mode'); 'copilot-cli' is a
        % legacy alias of 'coding-agent'.
        ClientModeCodingAgent (1,:) char = 'coding-agent'
        ClientModeEmpty       (1,:) char = 'empty'
        ClientModeCopilotCli  (1,:) char = 'copilot-cli'

        % Auto routing tiers for CopilotSession.switchAutoTier.
        AutoTierFast         (1,:) char = 'fast'
        AutoTierEfficiency   (1,:) char = 'efficiency'
        AutoTierBalance      (1,:) char = 'balance'
        AutoTierIntelligence (1,:) char = 'intelligence'

        % Ask-user tool variants (SessionConfig 'AskUserVariant').
        AskUserVariantLegacy      (1,:) char = 'legacy'
        AskUserVariantElicitation (1,:) char = 'elicitation'

        % Message sources (MessageSource) for MessageOptions 'Source';
        % agent sources use 'agent-<id>'.
        MessageSourceUser   (1,:) char = 'user'
        MessageSourceSystem (1,:) char = 'system'

        % Working-directory trust grants (SessionConfig 'TrustWorkingDirectory').
        WorkingDirectoryTrustSession (1,:) char = 'session'
        WorkingDirectoryTrustUser    (1,:) char = 'user'

        % Streamed shell output event (data: toolCallId, text, sequence,
        % stream 'stdout'/'stderr').  Replaces tool.execution_partial_result.
        EventToolShellOutput (1,:) char = 'tool.shell_output'

        % Permission-mode refusal reason (setPermissionMode result .reason).
        PermissionModeReasonManagedPolicy (1,:) char = 'managed_policy'
    end
end

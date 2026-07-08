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
    end
end

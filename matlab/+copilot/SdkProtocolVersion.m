classdef SdkProtocolVersion
    % SdkProtocolVersion  Protocol version negotiated with the Copilot CLI.
    %
    %   The version must match the value in sdk-protocol-version.json at the
    %   repository root. Bump this constant when the upstream protocol changes.

    properties (Constant)
        VERSION = 3
    end
end

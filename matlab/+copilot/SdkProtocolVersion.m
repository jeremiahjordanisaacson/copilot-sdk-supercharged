classdef SdkProtocolVersion
    % SdkProtocolVersion  SDK protocol version negotiated with the Copilot CLI.
    %
    %   The version must match the value in sdk-protocol-version.json at the
    %   repository root.  SDK protocol 4 requires Copilot CLI 1.0.113 or
    %   newer; the startup handshake accepts MIN_VERSION through MAX_VERSION.

    properties (Constant)
        VERSION = 4
        MIN_VERSION = 4
        MAX_VERSION = 4
    end
end

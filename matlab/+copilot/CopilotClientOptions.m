classdef CopilotClientOptions
    % CopilotClientOptions  Configuration for the Copilot CLI process.
    %
    %   opts = copilot.CopilotClientOptions('CliPath', '/usr/local/bin/copilot');
    %   opts = copilot.CopilotClientOptions('CliUrl', 'http://localhost:3000');

    properties
        % Path to the Copilot CLI binary.  When empty the SDK searches
        % the system PATH and the COPILOT_CLI_PATH environment variable.
        CliPath (1,:) char = ''

        % Additional arguments passed to the CLI process.
        CliArgs (1,:) cell = {}

        % Working directory for the CLI process.
        Cwd (1,:) char = ''

        % URL of an external Copilot CLI server.  When set the SDK
        % connects over HTTP instead of spawning a local process.
        CliUrl (1,:) char = ''

        % Log level forwarded to the CLI ('debug', 'info', 'warn', 'error').
        LogLevel (1,:) char = 'info'

        % Extra environment variables passed to the CLI process
        % as a containers.Map with char keys and char values.
        Env

        % GitHub personal-access token used for authentication.
        GitHubToken (1,:) char = ''

        % When true, authenticate with the logged-in GitHub user.
        UseLoggedInUser (1,1) logical = false

        % Enable stdio transport (default).  Set to false when
        % connecting to a running CLI server via CliUrl.
        UseStdio (1,1) logical = true

        % TCP port for the CLI server.  Only used when UseStdio is false
        % and CliUrl is empty.
        Port (1,1) double = 0

        % Session idle timeout in seconds (0 = no timeout).
        SessionIdleTimeoutSeconds (1,1) double = 0

        % Session filesystem configuration (struct with InitialCwd,
        % SessionStatePath, Conventions fields).
        SessionFs

        % Path to the Copilot home directory.
        CopilotHome (1,:) char = ''

        % TCP connection token for authenticated server connections.
        TcpConnectionToken (1,:) char = ''
    end

    methods
        function obj = CopilotClientOptions(varargin)
            %CopilotClientOptions  Create options using name-value pairs.
            %
            %   opts = copilot.CopilotClientOptions('CliPath', '/usr/bin/copilot');

            p = inputParser;
            p.KeepUnmatched = true;
            p.addParameter('CliPath', obj.CliPath);
            p.addParameter('CliArgs', obj.CliArgs);
            p.addParameter('Cwd', obj.Cwd);
            p.addParameter('CliUrl', obj.CliUrl);
            p.addParameter('LogLevel', obj.LogLevel);
            p.addParameter('Env', containers.Map());
            p.addParameter('GitHubToken', obj.GitHubToken);
            p.addParameter('UseLoggedInUser', obj.UseLoggedInUser);
            p.addParameter('UseStdio', obj.UseStdio);
            p.addParameter('Port', obj.Port);
            p.addParameter('SessionIdleTimeoutSeconds', obj.SessionIdleTimeoutSeconds);
            p.addParameter('SessionFs', struct());
            p.addParameter('CopilotHome', obj.CopilotHome);
            p.addParameter('TcpConnectionToken', obj.TcpConnectionToken);
            p.parse(varargin{:});

            obj.CliPath          = p.Results.CliPath;
            obj.CliArgs          = p.Results.CliArgs;
            obj.Cwd              = p.Results.Cwd;
            obj.CliUrl           = p.Results.CliUrl;
            obj.LogLevel         = p.Results.LogLevel;
            obj.Env              = p.Results.Env;
            obj.GitHubToken      = p.Results.GitHubToken;
            obj.UseLoggedInUser  = p.Results.UseLoggedInUser;
            obj.UseStdio         = p.Results.UseStdio;
            obj.Port             = p.Results.Port;
            obj.SessionIdleTimeoutSeconds = p.Results.SessionIdleTimeoutSeconds;
            obj.SessionFs        = p.Results.SessionFs;
            obj.CopilotHome      = p.Results.CopilotHome;
            obj.TcpConnectionToken = p.Results.TcpConnectionToken;
        end

        function s = toStruct(obj)
            %toStruct  Convert options to a plain struct for serialization.
            s = struct( ...
                'cliPath',         obj.CliPath, ...
                'cliArgs',         {obj.CliArgs}, ...
                'cwd',             obj.Cwd, ...
                'cliUrl',          obj.CliUrl, ...
                'logLevel',        obj.LogLevel, ...
                'gitHubToken',     obj.GitHubToken, ...
                'useLoggedInUser', obj.UseLoggedInUser, ...
                'useStdio',        obj.UseStdio, ...
                'port',            obj.Port, ...
                'sessionIdleTimeoutSeconds', obj.SessionIdleTimeoutSeconds);
            if ~isempty(obj.CopilotHome)
                s.copilotHome = obj.CopilotHome;
            end
            if ~isempty(obj.TcpConnectionToken)
                s.tcpConnectionToken = obj.TcpConnectionToken;
            end
        end
    end
end

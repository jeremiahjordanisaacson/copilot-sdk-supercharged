classdef CopilotClient < handle
    % CopilotClient  MATLAB client for the GitHub Copilot CLI.
    %
    %   client = copilot.CopilotClient();
    %   client.start();
    %   session = client.createSession(copilot.SessionConfig('Model','gpt-5'));
    %   resp = session.sendAndWait(copilot.MessageOptions('Prompt','Hello'));
    %   disp(resp.Content);
    %   session.disconnect();
    %   client.stop();
    %
    %   See also copilot.CopilotClientOptions, copilot.SessionConfig,
    %            copilot.CopilotSession, copilot.DefineTool.

    properties
        % Client configuration.
        Options copilot.CopilotClientOptions
    end

    properties (SetAccess = private)
        % True after start() succeeds and before stop().
        IsRunning (1,1) logical = false
    end

    properties (Access = private)
        Process          % java.lang.Process
        RpcClient        % copilot.JsonRpcClient
        Sessions         % containers.Map  sessionId -> CopilotSession
    end

    events
        % Fired when the CLI process exits unexpectedly.
        ProcessExited
    end

    % ------------------------------------------------------------------ %
    %  Public API
    % ------------------------------------------------------------------ %
    methods
        function obj = CopilotClient(options)
            %CopilotClient  Create a client with the given options.
            %
            %   client = copilot.CopilotClient();
            %   client = copilot.CopilotClient(copilot.CopilotClientOptions(...
            %       'CliPath', '/usr/local/bin/copilot'));
            arguments
                options copilot.CopilotClientOptions = copilot.CopilotClientOptions()
            end
            obj.Options  = options;
            obj.Sessions = containers.Map();
        end

        function start(obj)
            %start  Launch the Copilot CLI process and initialize JSON-RPC.
            %
            %   client.start();
            if obj.IsRunning
                return;
            end

            cliPath = obj.resolveCliPath();
            args    = obj.buildCliArgs();

            % Build the command list for ProcessBuilder.
            cmdList = java.util.ArrayList();
            cmdList.add(java.lang.String(cliPath));
            for i = 1:numel(args)
                cmdList.add(java.lang.String(args{i}));
            end

            pb = java.lang.ProcessBuilder(cmdList);
            pb.redirectErrorStream(false);

            % Apply working directory.
            if ~isempty(obj.Options.Cwd)
                pb.directory(java.io.File(obj.Options.Cwd));
            end

            % Apply extra environment variables.
            if isa(obj.Options.Env, 'containers.Map') && obj.Options.Env.Count > 0
                env = pb.environment();
                keys = obj.Options.Env.keys();
                for i = 1:numel(keys)
                    env.put(java.lang.String(keys{i}), ...
                            java.lang.String(obj.Options.Env(keys{i})));
                end
            end

            obj.Process = pb.start();

            obj.RpcClient = copilot.JsonRpcClient(obj.Process);
            obj.RpcClient.start();

            % Negotiate the protocol version.
            try
                initResult = obj.RpcClient.request('initialize', struct( ...
                    'protocolVersion', copilot.SdkProtocolVersion.VERSION, ...
                    'clientInfo', struct( ...
                        'name',    'copilot-sdk-matlab', ...
                        'version', '2.0.2')));
                if isfield(initResult, 'protocolVersion')
                    if initResult.protocolVersion ~= copilot.SdkProtocolVersion.VERSION
                        warning('copilot:versionMismatch', ...
                            'Server protocol version %d differs from SDK version %d.', ...
                            initResult.protocolVersion, ...
                            copilot.SdkProtocolVersion.VERSION);
                    end
                end
            catch me
                obj.cleanup();
                rethrow(me);
            end

            obj.IsRunning = true;

            % Set up session filesystem provider if configured
            if isstruct(obj.Options.SessionFs) && isfield(obj.Options.SessionFs, 'InitialCwd')
                obj.RpcClient.request('sessionFs.setProvider', struct( ...
                    'initialCwd', obj.Options.SessionFs.InitialCwd, ...
                    'sessionStatePath', obj.Options.SessionFs.SessionStatePath, ...
                    'conventions', obj.Options.SessionFs.Conventions), 10);
            end
        end

        function stop(obj)
            %stop  Gracefully shut down the CLI process.
            %
            %   client.stop();
            if ~obj.IsRunning
                return;
            end

            % Disconnect all open sessions.
            sessionIds = obj.Sessions.keys();
            for i = 1:numel(sessionIds)
                try
                    s = obj.Sessions(sessionIds{i});
                    s.disconnect();
                catch
                end
            end
            obj.Sessions = containers.Map();

            % Send shutdown notification and tear down.
            try
                obj.RpcClient.notify('shutdown', struct());
            catch
            end

            obj.cleanup();
            obj.IsRunning = false;
        end

        function forceStop(obj)
            %forceStop  Kill the CLI process immediately.
            obj.cleanup();
            obj.IsRunning = false;
        end

        function session = createSession(obj, config)
            %createSession  Open a new Copilot session.
            %
            %   session = client.createSession(copilot.SessionConfig('Model','gpt-5'));
            arguments
                obj
                config copilot.SessionConfig = copilot.SessionConfig()
            end
            obj.assertRunning();

            params = config.toStruct();
            result = obj.RpcClient.request('session.create', params, 30);

            sessionId = '';
            if isfield(result, 'sessionId')
                sessionId = result.sessionId;
            elseif isfield(result, 'id')
                sessionId = result.id;
            end

            session = copilot.CopilotSession(obj.RpcClient, sessionId, config.Tools);
            obj.Sessions(sessionId) = session;
        end

        function session = resumeSession(obj, sessionId, config)
            %resumeSession  Reconnect to a previously disconnected session.
            %
            %   session = client.resumeSession('abc-123');
            arguments
                obj
                sessionId (1,:) char
                config copilot.SessionConfig = copilot.SessionConfig()
            end
            obj.assertRunning();

            params = config.toStruct();
            params.sessionId = sessionId;
            result = obj.RpcClient.request('session.resume', params, 30);

            sid = sessionId;
            if isfield(result, 'sessionId')
                sid = result.sessionId;
            end

            session = copilot.CopilotSession(obj.RpcClient, sid, config.Tools);
            obj.Sessions(sid) = session;
        end

        function metadata = getSessionMetadata(obj, sessionId)
            %getSessionMetadata  Retrieve metadata for a session.
            %
            %   meta = client.getSessionMetadata('abc-123');
            arguments
                obj
                sessionId (1,:) char
            end
            obj.assertRunning();
            metadata = obj.RpcClient.request('session.getMetadata', ...
                struct('sessionId', sessionId), 15);
        end

        function setForegroundSessionId(obj, sessionId)
            %setForegroundSessionId  Mark a session as the foreground session.
            arguments
                obj
                sessionId (1,:) char
            end
            obj.assertRunning();
            result = obj.RpcClient.request('session.setForeground', ...
                struct('sessionId', sessionId), 10);
            if isfield(result, 'success') && ~result.success
                errMsg = 'Unknown error';
                if isfield(result, 'error')
                    errMsg = result.error;
                end
                error('copilot:setForegroundFailed', ...
                    'Failed to set foreground session: %s', errMsg);
            end
        end

        function sessions = listSessions(obj, filter)
            %listSessions  List active sessions, optionally filtered.
            %
            %   list = client.listSessions();
            arguments
                obj
                filter = struct()
            end
            obj.assertRunning();
            sessions = obj.RpcClient.request('session.list', filter, 15);
        end

        function deleteSession(obj, sessionId)
            %deleteSession  Permanently delete a session.
            arguments
                obj
                sessionId (1,:) char
            end
            obj.assertRunning();
            obj.RpcClient.request('session.delete', ...
                struct('sessionId', sessionId), 15);
            if obj.Sessions.isKey(sessionId)
                obj.Sessions.remove(sessionId);
            end
        end

        function response = ping(obj, message)
            %ping  Send a ping and wait for the pong.
            %
            %   resp = client.ping('hello');
            arguments
                obj
                message (1,:) char = 'ping'
            end
            obj.assertRunning();
            response = obj.RpcClient.request('ping', ...
                struct('message', message), 10);
        end

        function status = getStatus(obj)
            %getStatus  Return the CLI server status.
            obj.assertRunning();
            status = obj.RpcClient.request('status.get', struct(), 10);
        end

        function authStatus = getAuthStatus(obj)
            %getAuthStatus  Return the current authentication status.
            obj.assertRunning();
            authStatus = obj.RpcClient.request('auth.getStatus', struct(), 10);
        end

        function models = listModels(obj)
            %listModels  Return available models.
            obj.assertRunning();
            models = obj.RpcClient.request('models.list', struct(), 15);
        end

        function sid = getLastSessionId(obj)
            %getLastSessionId  Return the last-used session identifier.
            obj.assertRunning();
            result = obj.RpcClient.request('session.getLastId', struct(), 10);
            sid = '';
            if isfield(result, 'sessionId')
                sid = result.sessionId;
            end
        end

        function sid = getForegroundSessionId(obj)
            %getForegroundSessionId  Return the foreground session identifier.
            obj.assertRunning();
            result = obj.RpcClient.request('session.getForeground', struct(), 10);
            sid = '';
            if isfield(result, 'sessionId')
                sid = result.sessionId;
            end
        end

        function delete(obj)
            %delete  Destructor -- ensures the process is cleaned up.
            obj.stop();
        end
    end

    % ------------------------------------------------------------------ %
    %  Private helpers
    % ------------------------------------------------------------------ %
    methods (Access = private)
        function assertRunning(obj)
            if ~obj.IsRunning
                error('copilot:notRunning', ...
                    'Client is not running. Call start() first.');
            end
        end

        function cliPath = resolveCliPath(obj)
            %resolveCliPath  Determine the CLI executable path.
            cliPath = obj.Options.CliPath;
            if isempty(cliPath)
                cliPath = getenv('COPILOT_CLI_PATH');
            end
            if isempty(cliPath)
                if ispc
                    [rc, out] = system('where copilot 2>nul');
                else
                    [rc, out] = system('which copilot 2>/dev/null');
                end
                if rc == 0
                    cliPath = strtrim(out);
                    % Take first result when multiple are found.
                    lines = splitlines(cliPath);
                    cliPath = lines{1};
                end
            end
            if isempty(cliPath)
                error('copilot:cliNotFound', ...
                    ['Copilot CLI not found. Set Options.CliPath, ', ...
                     'the COPILOT_CLI_PATH environment variable, or ', ...
                     'ensure "copilot" is on the system PATH.']);
            end
        end

        function args = buildCliArgs(obj)
            %buildCliArgs  Assemble CLI arguments.
            args = {'--server', '--stdio'};
            if ~isempty(obj.Options.LogLevel)
                args{end+1} = '--log-level';
                args{end+1} = obj.Options.LogLevel;
            end
            if ~isempty(obj.Options.GitHubToken)
                args{end+1} = '--github-token';
                args{end+1} = obj.Options.GitHubToken;
            end
            if obj.Options.UseLoggedInUser
                args{end+1} = '--use-logged-in-user';
            end
            args = [args, obj.Options.CliArgs];
        end

        function cleanup(obj)
            %cleanup  Destroy the process and RPC client.
            if ~isempty(obj.RpcClient)
                try obj.RpcClient.stop(); catch, end
                obj.RpcClient = [];
            end
            if ~isempty(obj.Process)
                try obj.Process.destroyForcibly(); catch, end
                obj.Process = [];
            end
        end
    end
end

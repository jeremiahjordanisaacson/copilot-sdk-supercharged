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

        % Policy Helper failure (copilot.PolicyHelperFailure) attached to the
        % last failed createSession / resumeSession, or [] when none.
        LastPolicyHelperFailure = []
    end

    properties (Access = private)
        Process          % java.lang.Process
        RpcClient        % copilot.JsonRpcClient
        Sessions         % containers.Map  sessionId -> CopilotSession
        TokenProviders   % containers.Map  registrationId -> function handle
        SessionCallbacks % containers.Map  sessionId -> struct of handlers
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
            obj.TokenProviders = containers.Map('KeyType', 'char', 'ValueType', 'any');
            obj.SessionCallbacks = containers.Map('KeyType', 'char', 'ValueType', 'any');
        end

        function start(obj)
            %start  Launch the Copilot CLI process and initialize JSON-RPC.
            %
            %   client.start();
            %
            %   The CLI must speak SDK protocol 4 (Copilot CLI 1.0.113 or
            %   newer); older CLIs are rejected.
            if obj.IsRunning
                return;
            end

            if ~any(strcmp(obj.Options.Mode, {'', 'coding-agent', 'copilot-cli', 'empty'}))
                error('copilot:invalidMode', ...
                    'Unknown client mode "%s". Use ''coding-agent'' or ''empty''.', ...
                    obj.Options.Mode);
            end

            cliPath = obj.resolveCliPath();
            args    = obj.buildCliArgs();

            % Build the command list for ProcessBuilder.  A ".js" entry point
            % (npm-loader.js) runs through node; other paths (the native
            % binary or the npm .bin shim) run directly.
            cmdList = java.util.ArrayList();
            if endsWith(lower(cliPath), '.js')
                cmdList.add(java.lang.String('node'));
            end
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
            obj.registerRequestHandlers();
            obj.RpcClient.start();

            % SDK protocol 4 handshake: connect (with clientInfo), falling
            % back to ping for runtimes without connect.  The server must
            % report exactly protocol version 4.
            try
                serverVersion = obj.negotiateProtocol();
                obj.verifyProtocolVersion(serverVersion);
            catch me
                obj.cleanup();
                rethrow(me);
            end

            obj.IsRunning = true;

            % Register network callbacks (network.setProvider).
            if obj.hasNetworkCallbacks()
                obj.RpcClient.request('network.setProvider', ...
                    obj.networkProviderParams(), 10);
            end

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
            %
            %   The flat options are translated to the SDK protocol 4 grouped
            %   payload (copilot.groupSessionParams) before sending.
            arguments
                obj
                config copilot.SessionConfig = copilot.SessionConfig()
            end
            obj.assertRunning();

            params = config.toStruct();
            obj.assertModeAllows(params);
            regId = obj.registerTokenProvider(config);
            if ~isempty(regId)
                params.gitHubTokenProviderRegistrationId = regId;
            end

            % Merge trace context into params.
            ctx = obj.getTraceContext();
            ctxFields = fieldnames(ctx);
            for i = 1:numel(ctxFields)
                params.(ctxFields{i}) = ctx.(ctxFields{i});
            end

            wire = copilot.groupSessionParams(params, false, obj.isEmptyMode());
            result = obj.sendSessionRequest('session.create', wire, regId);

            sessionId = '';
            if isfield(result, 'sessionId')
                sessionId = result.sessionId;
            elseif isfield(result, 'id')
                sessionId = result.id;
            end

            session = copilot.CopilotSession(obj.RpcClient, sessionId, ...
                config.Tools, config.ExitPlanModeHandler);

            % Register exitPlanMode.request handler if configured.
            if ~isempty(config.ExitPlanModeHandler)
                obj.RpcClient.onRequest('exitPlanMode.request', ...
                    @(p) session.handleExitPlanMode(p));
            end

            obj.attachSession(session, sessionId, config, regId);
        end

        function session = resumeSession(obj, sessionId, config)
            %resumeSession  Reconnect to a previously disconnected session.
            %
            %   session = client.resumeSession('abc-123');
            %   session = client.resumeSession('abc-123', copilot.SessionConfig( ...
            %       'DisableResume', true, 'AllowTranscriptRecovery', true));
            arguments
                obj
                sessionId (1,:) char
                config copilot.SessionConfig = copilot.SessionConfig()
            end
            obj.assertRunning();

            params = config.toStruct();
            params.sessionId = sessionId;
            if config.DisableResume
                params.disableResume = true;
            end
            if config.ContinuePendingWork
                params.continuePendingWork = true;
            end
            if config.AllowTranscriptRecovery
                params.allowTranscriptRecovery = true;
            end
            if config.ClearDefaultProvider && isempty(config.DefaultProvider)
                params.defaultProvider = NaN;  % encoded as JSON null
            end
            obj.assertModeAllows(params);
            regId = obj.registerTokenProvider(config);
            if ~isempty(regId)
                params.gitHubTokenProviderRegistrationId = regId;
            end

            wire = copilot.groupSessionParams(params, true, obj.isEmptyMode());
            result = obj.sendSessionRequest('session.resume', wire, regId);

            sid = sessionId;
            if isfield(result, 'sessionId')
                sid = result.sessionId;
            end

            session = copilot.CopilotSession(obj.RpcClient, sid, config.Tools);
            obj.attachSession(session, sid, config, regId);
        end

        function clearManagedSettingsCache(obj)
            %clearManagedSettingsCache  Clear cached enterprise managed settings.
            %
            %   client.clearManagedSettingsCache();   % managedSettings.clearCache
            obj.assertRunning();
            obj.RpcClient.request('managedSettings.clearCache', struct(), 15);
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
            %buildCliArgs  Assemble CLI arguments (headless stdio server).
            args = {'--headless', '--no-auto-update', '--stdio'};
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
            if obj.Options.Remote
                args{end+1} = '--remote';
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

        % -------------------------------------------------------------- %
        %  SDK protocol 4 handshake and session helpers
        % -------------------------------------------------------------- %

        function version = negotiateProtocol(obj)
            %negotiateProtocol  connect (with clientInfo), falling back to ping.
            params = struct();
            info = obj.clientInfoToWire();
            if ~isempty(info)
                params.clientInfo = info;
            end
            try
                result = obj.RpcClient.request('connect', params, 30);
            catch me
                lastError = obj.RpcClient.LastError;
                isUnknown = isstruct(lastError) && ( ...
                    (isfield(lastError, 'code') && lastError.code == -32601) || ...
                    (isfield(lastError, 'message') && ...
                     strcmp(lastError.message, 'Unhandled method connect')));
                if ~isUnknown
                    rethrow(me);
                end
                result = obj.RpcClient.request('ping', struct('message', 'ping'), 30);
            end
            version = [];
            if isstruct(result) && isfield(result, 'protocolVersion') ...
                    && ~isempty(result.protocolVersion)
                version = double(result.protocolVersion);
            end
        end

        function verifyProtocolVersion(~, serverVersion)
            %verifyProtocolVersion  Accept exactly SDK protocol version 4.
            supported = copilot.SdkProtocolVersion.VERSION;
            if isempty(serverVersion)
                error('copilot:protocolMismatch', ...
                    ['SDK protocol version mismatch: SDK supports version %d, ' ...
                     'but server does not report a protocol version. ' ...
                     'Please update your SDK or server to ensure compatibility.'], ...
                    supported);
            end
            if serverVersion < copilot.SdkProtocolVersion.MIN_VERSION ...
                    || serverVersion > copilot.SdkProtocolVersion.MAX_VERSION
                error('copilot:protocolMismatch', ...
                    ['SDK protocol version mismatch: SDK supports version %d, ' ...
                     'but server reports version %d. ' ...
                     'Please update your SDK or server to ensure compatibility.'], ...
                    supported, serverVersion);
            end
        end

        function info = clientInfoToWire(obj)
            %clientInfoToWire  clientInfo for connect; [] when every field is empty.
            info = [];
            source = obj.Options.ClientInfo;
            if ~isstruct(source)
                return;
            end
            names = {'EditorName', 'editorName'; 'EditorVersion', 'editorVersion'; ...
                     'ExtensionName', 'extensionName'; ...
                     'ExtensionVersion', 'extensionVersion'};
            wire = struct();
            for i = 1:size(names, 1)
                value = '';
                if isfield(source, names{i, 1})
                    value = source.(names{i, 1});
                elseif isfield(source, names{i, 2})
                    value = source.(names{i, 2});
                end
                if ~isempty(value)
                    wire.(names{i, 2}) = char(value);
                end
            end
            if ~isempty(fieldnames(wire))
                info = wire;
            end
        end

        function tf = isEmptyMode(obj)
            tf = strcmp(obj.Options.Mode, 'empty');
        end

        function assertModeAllows(obj, params)
            %assertModeAllows  Empty mode requires an explicit tool allowlist.
            if obj.isEmptyMode() && ~isfield(params, 'availableTools')
                error('copilot:emptyModeRequiresTools', ...
                    ['Client mode ''empty'' requires SessionConfig ''AvailableTools'' ' ...
                     'on every session create and resume.']);
            end
        end

        function regId = registerTokenProvider(obj, config)
            %registerTokenProvider  Register config.GitHubTokenProvider, if any.
            regId = '';
            if isempty(config.GitHubTokenProvider)
                return;
            end
            uuid = java.util.UUID.randomUUID();
            regId = char(uuid.toString());
            obj.TokenProviders(regId) = config.GitHubTokenProvider;
        end

        function result = sendSessionRequest(obj, method, params, regId)
            %sendSessionRequest  Send session.create/resume; keep failure details.
            obj.LastPolicyHelperFailure = [];
            try
                result = obj.RpcClient.request(method, params, 30);
            catch me
                obj.LastPolicyHelperFailure = ...
                    copilot.PolicyHelperFailure.fromRpcError(obj.RpcClient.LastError);
                if ~isempty(regId) && obj.TokenProviders.isKey(regId)
                    obj.TokenProviders.remove(regId);
                end
                rethrow(me);
            end
        end

        function attachSession(obj, session, sessionId, config, regId)
            %attachSession  Track a session and its callbacks.
            obj.SessionCallbacks(sessionId) = struct( ...
                'ShellAuthentication', {config.OnShellAuthenticationRequest}, ...
                'StoreMemory',         {config.StoreMemory}, ...
                'SubagentStart',       {config.OnSubagentStart}, ...
                'SubagentStop',        {config.OnSubagentStop});
            session.CloseCallback = @() obj.releaseSession(sessionId, regId);
            obj.Sessions(sessionId) = session;
        end

        function releaseSession(obj, sessionId, regId)
            %releaseSession  Drop callback registrations of a detached session.
            if obj.SessionCallbacks.isKey(sessionId)
                obj.SessionCallbacks.remove(sessionId);
            end
            if ~isempty(regId) && obj.TokenProviders.isKey(regId)
                obj.TokenProviders.remove(regId);
            end
        end

        function handler = sessionCallback(obj, params, name)
            handler = [];
            sessionId = fieldOr(params, 'sessionId', '');
            if ~isempty(sessionId) && obj.SessionCallbacks.isKey(sessionId)
                callbacks = obj.SessionCallbacks(sessionId);
                handler = callbacks.(name);
            end
        end

        % -------------------------------------------------------------- %
        %  Server-to-client requests
        % -------------------------------------------------------------- %

        function registerRequestHandlers(obj)
            rpc = obj.RpcClient;
            rpc.onRequest('gitHubToken.getToken', @(p) obj.handleGitHubToken(p));
            rpc.onRequest('shellAuthentication.request', ...
                @(p) obj.handleShellAuthentication(p));
            % Prompts are answered synchronously, so nothing is pending.
            rpc.onRequest('shellAuthentication.cancel', @(p) struct());
            rpc.onRequest('memoryStore.store', @(p) obj.handleMemoryStore(p));
            rpc.onRequest('hooks.invoke', @(p) obj.handleHooksInvoke(p));
            rpc.onRequest('network.resolveProxy', ...
                @(p) obj.handleNetwork('resolveProxy', p));
            rpc.onRequest('network.authenticateNegotiate', ...
                @(p) obj.handleNetwork('authenticateNegotiate', p));
            rpc.onRequest('network.authenticateBasic', ...
                @(p) obj.handleNetwork('authenticateBasic', p));
        end

        function result = handleGitHubToken(obj, params)
            %handleGitHubToken  Answer gitHubToken.getToken.
            regId = fieldOr(params, 'registrationId', '');
            if isempty(regId) || ~obj.TokenProviders.isKey(regId)
                error('copilot:noTokenProvider', ...
                    'No GitHub token provider is registered for "%s".', regId);
            end
            provider = obj.TokenProviders(regId);
            token = provider(params);
            if isempty(token)
                result = struct('kind', 'cancelled');
            elseif ischar(token) || isstring(token)
                result = struct('kind', 'token', 'accessToken', char(token));
            else
                result = token;
                if ~isfield(result, 'kind')
                    result.kind = 'token';
                end
            end
        end

        function result = handleShellAuthentication(obj, params)
            %handleShellAuthentication  Answer shellAuthentication.request.
            result = struct('action', 'cancel');
            handler = obj.sessionCallback(params, 'ShellAuthentication');
            if isempty(handler)
                return;
            end
            reply = handler(params);
            if ischar(reply) || isstring(reply)
                result = struct('action', 'accept', 'value', char(reply));
            elseif isstruct(reply) && isfield(reply, 'action')
                result = reply;
            end
        end

        function result = handleMemoryStore(obj, params)
            %handleMemoryStore  Answer memoryStore.store (replies null).
            handler = obj.sessionCallback(params, 'StoreMemory');
            if isempty(handler)
                error('copilot:noMemoryStore', 'Memory store owner is unavailable.');
            end
            handler(params);
            result = [];
        end

        function result = handleHooksInvoke(obj, params)
            %handleHooksInvoke  Answer hooks.invoke for sub-agent lifecycle hooks.
            result = struct();
            switch fieldOr(params, 'hookType', '')
                case 'subagentStart'
                    handler = obj.sessionCallback(params, 'SubagentStart');
                case 'subagentStop'
                    handler = obj.sessionCallback(params, 'SubagentStop');
                otherwise
                    handler = [];
            end
            if isempty(handler)
                return;
            end
            output = handler(fieldOr(params, 'input', struct()));
            if ~isempty(output)
                result = struct('output', output);
            end
        end

        function result = handleNetwork(obj, kind, params)
            %handleNetwork  Answer network.* callbacks ({} declines).
            result = struct();
            network = obj.Options.Network;
            if ~isstruct(network)
                return;
            end
            switch kind
                case 'resolveProxy'
                    if isfield(network, 'ResolveProxy') && ~isempty(network.ResolveProxy)
                        proxy = network.ResolveProxy(fieldOr(params, 'url', ''));
                        if ~isempty(proxy)
                            result = struct('proxy', char(proxy));
                        end
                    end
                case 'authenticateNegotiate'
                    if isfield(network, 'AuthenticateNegotiate') ...
                            && ~isempty(network.AuthenticateNegotiate)
                        token = network.AuthenticateNegotiate(fieldOr(params, 'spn', ''));
                        if ~isempty(token)
                            result = struct('token', char(token));
                        end
                    end
                case 'authenticateBasic'
                    if isfield(network, 'AuthenticateBasic') ...
                            && ~isempty(network.AuthenticateBasic)
                        credentials = network.AuthenticateBasic( ...
                            fieldOr(params, 'proxy', ''), ...
                            fieldOr(params, 'realm', ''), ...
                            logical(fieldOr(params, 'previousRejected', false)));
                        if isstruct(credentials) && ~isempty(fieldnames(credentials))
                            result = credentials;
                        end
                    end
            end
        end

        function tf = hasNetworkCallbacks(obj)
            network = obj.Options.Network;
            tf = isstruct(network) && ( ...
                (isfield(network, 'ResolveProxy') && ~isempty(network.ResolveProxy)) || ...
                (isfield(network, 'AuthenticateNegotiate') && ~isempty(network.AuthenticateNegotiate)) || ...
                (isfield(network, 'AuthenticateBasic') && ~isempty(network.AuthenticateBasic)));
        end

        function params = networkProviderParams(obj)
            %networkProviderParams  network.setProvider params ({} marks a callback).
            network = obj.Options.Network;
            params = struct();
            if isfield(network, 'Scope') && ~isempty(network.Scope)
                params.scope = char(network.Scope);
            end
            if isfield(network, 'ResolveProxy') && ~isempty(network.ResolveProxy)
                params.resolveProxy = struct();
            end
            if isfield(network, 'AuthenticateNegotiate') && ~isempty(network.AuthenticateNegotiate)
                params.authenticateNegotiate = struct();
            end
            if isfield(network, 'AuthenticateBasic') && ~isempty(network.AuthenticateBasic)
                params.authenticateBasic = struct();
            end
        end

        function ctx = getTraceContext(obj)
            %getTraceContext  Invoke the OnGetTraceContext provider.
            ctx = struct();
            if ~isempty(obj.Options.OnGetTraceContext)
                try
                    ctx = obj.Options.OnGetTraceContext();
                catch
                    ctx = struct();
                end
            end
        end
    end
end

function value = fieldOr(s, name, default)
%fieldOr  s.(name) when present and non-empty, otherwise default.
    value = default;
    if isstruct(s) && isfield(s, name) && ~isempty(s.(name))
        value = s.(name);
    end
end

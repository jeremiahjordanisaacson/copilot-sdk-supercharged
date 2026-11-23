classdef CopilotSession < handle
    % CopilotSession  A single conversation with the Copilot agent.
    %
    %   Use CopilotClient.createSession() to obtain a session; do not
    %   instantiate this class directly.
    %
    %   session.send(copilot.MessageOptions('Prompt', 'Hello'));
    %   response = session.sendAndWait(copilot.MessageOptions('Prompt', 'Hi'));
    %   session.disconnect();

    properties (SetAccess = ?copilot.CopilotClient)
        % Unique session identifier assigned by the server.
        SessionId (1,:) char = ''

        % Called once when the session is detached (releases the client's
        % callback registrations for this session).
        CloseCallback = []
    end

    properties (Access = private)
        RpcClient       copilot.JsonRpcClient
        Tools           cell = {}
        Listener        % event listener handle
        Messages        cell = {}
        IdleFuture      % struct used for sendAndWait blocking
        ExitPlanModeHandler  % function handle for exit-plan-mode requests
    end

    events
        % Fired for every session event (assistant.message, tool_call, etc.).
        SessionEvent

        % Fired when the agent becomes idle after processing a turn.
        TurnComplete
    end

    methods (Access = {?copilot.CopilotClient})
        function obj = CopilotSession(rpcClient, sessionId, tools, exitPlanModeHandler)
            %CopilotSession  Internal constructor.
            arguments
                rpcClient   copilot.JsonRpcClient
                sessionId   (1,:) char
                tools       cell = {}
                exitPlanModeHandler = []
            end
            obj.RpcClient = rpcClient;
            obj.SessionId = sessionId;
            obj.Tools     = tools;
            obj.ExitPlanModeHandler = exitPlanModeHandler;

            % Subscribe to RPC notifications and route session events.
            obj.Listener = addlistener(rpcClient, 'Notification', ...
                @(~, evt) obj.onNotification(evt));
        end
    end

    methods
        function send(obj, msgOpts)
            %send  Send a user message without waiting for the response.
            %
            %   session.send(copilot.MessageOptions('Prompt', 'Hello'));
            arguments
                obj
                msgOpts copilot.MessageOptions
            end
            params = msgOpts.toSendParams();
            params.sessionId = obj.SessionId;
            obj.RpcClient.request('session.send', params, 30);
        end

        function response = sendAndWait(obj, msgOpts)
            %sendAndWait  Send a message and block until the turn completes.
            %
            %   resp = session.sendAndWait(copilot.MessageOptions('Prompt','Hi'));
            %   disp(resp.Content);
            arguments
                obj
                msgOpts copilot.MessageOptions
            end

            % Prepare a blocking future.
            obj.IdleFuture = struct('done', false, 'content', '', 'raw', []);

            params = msgOpts.toSendParams();
            params.sessionId = obj.SessionId;
            obj.RpcClient.request('session.send', params, 30);

            timeout = msgOpts.Timeout;
            if timeout <= 0
                timeout = 600;
            end
            deadline = tic;
            while ~obj.IdleFuture.done && toc(deadline) < timeout
                pause(0.05);
            end

            if ~obj.IdleFuture.done
                error('copilot:timeout', ...
                    'sendAndWait timed out after %g seconds.', timeout);
            end

            response = struct( ...
                'Content', obj.IdleFuture.content, ...
                'Raw',     obj.IdleFuture.raw);
        end

        function abort(obj)
            %abort  Cancel the active agent turn (session.abort).
            %
            %   Failures arrive as JSON-RPC errors and are raised.
            obj.RpcClient.request('session.abort', ...
                struct('sessionId', obj.SessionId), 15);
        end

        function msgs = getMessages(obj)
            %getMessages  Return all messages collected for this session.
            msgs = obj.Messages;
        end

        function detach(obj)
            %detach  Detach this connection from the session (session.detach).
            %
            %   The session stays on disk; call CopilotClient.resumeSession()
            %   to reconnect later.  Releases the session's callbacks.
            try
                obj.RpcClient.request('session.detach', ...
                    struct('sessionId', obj.SessionId), 15);
            catch me
                obj.release();
                rethrow(me);
            end
            obj.release();
        end

        function switchAutoTier(obj, tier)
            %switchAutoTier  Request an Auto routing tier for future turns.
            %
            %   session.switchAutoTier('balance');
            %
            %   Tier is 'fast', 'efficiency', 'balance' or 'intelligence'
            %   (see copilot.Types.AutoTier*).
            arguments
                obj
                tier (1,:) char
            end
            obj.RpcClient.request('session.model.switchAutoTier', ...
                struct('sessionId', obj.SessionId, 'autoTier', tier), 15);
        end

        function snapshot = recheckPolicyHelper(obj)
            %recheckPolicyHelper  Re-run the enterprise Policy Helper.
            %
            %   snapshot = session.recheckPolicyHelper();
            snapshot = obj.RpcClient.request( ...
                'session.managedSettings.recheckPolicyHelper', ...
                struct('sessionId', obj.SessionId), 30);
        end

        function result = setPermissionMode(obj, mode)
            %setPermissionMode  Change the permission mode.
            %
            %   result = session.setPermissionMode('autopilot');
            %   if ~result.success && isfield(result, 'reason')
            %       % result.reason is 'managed_policy' when policy refused it
            %   end
            arguments
                obj
                mode (1,:) char
            end
            result = obj.RpcClient.request('session.permissions.setMode', ...
                struct('sessionId', obj.SessionId, 'mode', mode), 15);
        end

        function disconnect(obj)
            %disconnect  Detach this session from the server.
            %
            %   The session is removed from the client's registry.  Call
            %   CopilotClient.resumeSession() to reconnect later.
            try
                obj.detach();
            catch
                % Ignore errors during teardown.
            end
            if ~isempty(obj.Listener)
                delete(obj.Listener);
                obj.Listener = [];
            end
        end
    end

    methods (Access = private)
        function release(obj)
            %release  Run the client's close callback once.
            if ~isempty(obj.CloseCallback)
                callback = obj.CloseCallback;
                obj.CloseCallback = [];
                callback();
            end
        end

        function onNotification(obj, evt)
            %onNotification  Handle incoming JSON-RPC notifications.
            if ~isa(evt, 'copilot.RpcEventData')
                return;
            end

            payload = evt.Payload;
            if ~isfield(payload, 'params')
                return;
            end
            params = payload.params;

            % Only handle events for this session.
            if isfield(params, 'sessionId')
                if ~strcmp(params.sessionId, obj.SessionId)
                    return;
                end
            end

            method = evt.Method;

            % ---- SDK protocol 4 session events: {sessionId, event} ----
            % Unknown event types (including tool.shell_output and the
            % removed tool.execution_partial_result) are passed through.
            if strcmp(method, 'session.event')
                obj.routeSessionEvent('session/event', params);
                return;
            end

            % ---- Tool invocation ----
            if strcmp(method, 'session/tool_call')
                obj.handleToolCall(params);
                return;
            end

            % ---- Session events ----
            if startsWith(method, 'session/')
                obj.routeSessionEvent(method, params);
            end
        end

        function routeSessionEvent(obj, method, params)
            %routeSessionEvent  Fire MATLAB events for session notifications.

            % Accumulate assistant content.
            if strcmp(method, 'session/event')
                if isfield(params, 'event')
                    evtType = '';
                    agentId = '';
                    if isfield(params.event, 'type')
                        evtType = params.event.type;
                    end
                    if isfield(params.event, 'agentId')
                        agentId = params.event.agentId;
                    end

                    % Collect assistant messages.
                    if strcmp(evtType, 'assistant.message')
                        obj.Messages{end+1} = params.event;
                        if ~isempty(obj.IdleFuture)
                            if isfield(params.event, 'data') ...
                                    && isstruct(params.event.data) ...
                                    && isfield(params.event.data, 'content')
                                obj.IdleFuture.content = params.event.data.content;
                            elseif isfield(params.event, 'content')
                                obj.IdleFuture.content = params.event.content;
                            end
                            obj.IdleFuture.raw = params.event;
                        end
                    end

                    % Detect turn completion.
                    if strcmp(evtType, 'session.idle')
                        if ~isempty(obj.IdleFuture)
                            obj.IdleFuture.done = true;
                        end
                        notify(obj, 'TurnComplete');
                    end
                end
            end

            notify(obj, 'SessionEvent', copilot.RpcEventData(method, params));
        end

        function handleToolCall(obj, params)
            %handleToolCall  Execute a local tool and return the result.
            toolName = '';
            if isfield(params, 'name')
                toolName = params.name;
            end
            callId = '';
            if isfield(params, 'callId')
                callId = params.callId;
            end
            toolArgs = struct();
            if isfield(params, 'arguments')
                toolArgs = params.arguments;
            end

            % Find the matching tool.
            result = '';
            found  = false;
            for i = 1:numel(obj.Tools)
                t = obj.Tools{i};
                if strcmp(t.Name, toolName)
                    found = true;
                    try
                        result = t.Handler(toolArgs);
                    catch me
                        result = struct('error', me.message);
                    end
                    break;
                end
            end
            if ~found
                result = struct('error', sprintf('Unknown tool: %s', toolName));
            end

            % Return the result to the server.
            if isstruct(result) || iscell(result)
                content = jsonencode(result);
            elseif isnumeric(result)
                content = num2str(result);
            else
                content = char(result);
            end

            obj.RpcClient.notify('session/tool_result', struct( ...
                'sessionId', obj.SessionId, ...
                'callId',    callId, ...
                'result',    content));
        end

        function result = handleExitPlanMode(obj, params)
            %handleExitPlanMode  Handle an exit-plan-mode request.
            if isempty(obj.ExitPlanModeHandler)
                result = struct('approved', true);
                return;
            end
            try
                result = obj.ExitPlanModeHandler(params);
            catch
                result = struct('approved', true);
            end
        end
    end
end

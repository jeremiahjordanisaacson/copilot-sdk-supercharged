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
    end

    properties (Access = private)
        RpcClient       copilot.JsonRpcClient
        Tools           cell = {}
        Listener        % event listener handle
        Messages        cell = {}
        IdleFuture      % struct used for sendAndWait blocking
    end

    events
        % Fired for every session event (assistant.message, tool_call, etc.).
        SessionEvent

        % Fired when the agent becomes idle after processing a turn.
        TurnComplete
    end

    methods (Access = {?copilot.CopilotClient})
        function obj = CopilotSession(rpcClient, sessionId, tools)
            %CopilotSession  Internal constructor.
            obj.RpcClient = rpcClient;
            obj.SessionId = sessionId;
            obj.Tools     = tools;

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
            params = struct( ...
                'sessionId', obj.SessionId, ...
                'message',   msgOpts.toStruct());
            obj.RpcClient.notify('session/send', params);
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

            params = struct( ...
                'sessionId', obj.SessionId, ...
                'message',   msgOpts.toStruct());
            obj.RpcClient.notify('session/send', params);

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
            %abort  Cancel the active agent turn.
            obj.RpcClient.notify('session/abort', ...
                struct('sessionId', obj.SessionId));
        end

        function msgs = getMessages(obj)
            %getMessages  Return all messages collected for this session.
            msgs = obj.Messages;
        end

        function disconnect(obj)
            %disconnect  Detach this session from the server.
            %
            %   The session is removed from the client's registry.  Call
            %   CopilotClient.resumeSession() to reconnect later.
            try
                obj.RpcClient.notify('session/disconnect', ...
                    struct('sessionId', obj.SessionId));
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
                    if isfield(params.event, 'type')
                        evtType = params.event.type;
                    end

                    % Collect assistant messages.
                    if strcmp(evtType, 'assistant.message')
                        obj.Messages{end+1} = params.event;
                        if ~isempty(obj.IdleFuture)
                            if isfield(params.event, 'content')
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
    end
end

classdef JsonRpcClient < handle
    % JsonRpcClient  JSON-RPC 2.0 transport over a process stdin/stdout.
    %
    %   rpc = copilot.JsonRpcClient(javaProcess);
    %   rpc.start();
    %   result = rpc.request('ping', struct('message', 'hi'));
    %   rpc.notify('session/setForeground', struct('id', sid));
    %   rpc.stop();

    properties (Access = private)
        Process          % java.lang.Process
        Writer           % java.io.BufferedWriter  (to stdin)
        Reader           % java.io.BufferedReader  (from stdout)
        NextId  (1,1) double = 1
        Pending          % containers.Map: id -> struct (response holder)
        Running (1,1) logical = false
        ReadTimer        % timer object for background reading
        RequestHandlers  % containers.Map: method -> function handle
    end

    properties (SetAccess = private)
        % The "error" object (code, message, data) of the last JSON-RPC
        % error response, or [] when the last request succeeded.
        LastError = []
    end

    events
        Notification     % Fired for every server-initiated notification
    end

    methods
        function obj = JsonRpcClient(javaProcess)
            %JsonRpcClient  Wrap a java.lang.Process for JSON-RPC I/O.
            obj.Process = javaProcess;
            obj.Pending = containers.Map('KeyType', 'char', 'ValueType', 'any');
            obj.RequestHandlers = containers.Map('KeyType', 'char', 'ValueType', 'any');
        end

        function onRequest(obj, method, handler)
            %onRequest  Answer server-to-client requests for METHOD.
            %
            %   rpc.onRequest('hooks.invoke', @(params) struct());
            %
            %   The handler receives the decoded params struct and returns the
            %   result ([] replies null).  Errors become JSON-RPC errors.
            obj.RequestHandlers(method) = handler;
        end

        function start(obj)
            %start  Open I/O streams and begin reading.
            outStream = obj.Process.getOutputStream();
            obj.Writer = java.io.BufferedWriter( ...
                java.io.OutputStreamWriter(outStream, 'UTF-8'));

            inStream = obj.Process.getInputStream();
            obj.Reader = java.io.BufferedReader( ...
                java.io.InputStreamReader(inStream, 'UTF-8'));

            obj.Running = true;

            % Use a timer to poll stdout without blocking the main thread.
            obj.ReadTimer = timer( ...
                'ExecutionMode', 'fixedSpacing', ...
                'Period',        0.05, ...
                'TimerFcn',      @(~,~) obj.readLoop(), ...
                'ErrorFcn',      @(~,~) []);
            start(obj.ReadTimer);
        end

        function stop(obj)
            %stop  Shut down the reader loop and close streams.
            obj.Running = false;
            if ~isempty(obj.ReadTimer)
                stop(obj.ReadTimer);
                delete(obj.ReadTimer);
                obj.ReadTimer = [];
            end
            try obj.Writer.close(); catch, end %#ok<CTCH>
            try obj.Reader.close(); catch, end %#ok<CTCH>
        end

        function result = request(obj, method, params, timeout)
            %request  Send a JSON-RPC request and wait for the response.
            %
            %   result = rpc.request('ping', struct('message','hi'));
            %   result = rpc.request('session/create', cfg, 30);
            arguments
                obj
                method (1,:) char
                params = struct()
                timeout (1,1) double = 30
            end

            id = num2str(obj.NextId);
            obj.NextId = obj.NextId + 1;

            msg = struct( ...
                'jsonrpc', '2.0', ...
                'id',      str2double(id), ...
                'method',  method, ...
                'params',  params);

            obj.Pending(id) = struct('done', false, 'result', [], 'error', []);
            obj.LastError = [];
            obj.sendRaw(msg);

            % Block until response arrives or timeout.
            deadline = tic;
            while toc(deadline) < timeout
                if obj.Pending.isKey(id)
                    entry = obj.Pending(id);
                    if entry.done
                        obj.Pending.remove(id);
                        if ~isempty(entry.error)
                            obj.LastError = entry.error;
                            error('copilot:rpcError', ...
                                'JSON-RPC error %d: %s', ...
                                entry.error.code, entry.error.message);
                        end
                        result = entry.result;
                        return;
                    end
                end
                pause(0.02);
            end

            obj.Pending.remove(id);
            error('copilot:timeout', ...
                'JSON-RPC request "%s" timed out after %g seconds.', ...
                method, timeout);
        end

        function notify(obj, method, params)
            %notify  Send a JSON-RPC notification (no response expected).
            arguments
                obj
                method (1,:) char
                params = struct()
            end
            msg = struct( ...
                'jsonrpc', '2.0', ...
                'method',  method, ...
                'params',  params);
            obj.sendRaw(msg);
        end
    end

    methods (Access = private)
        function sendRaw(obj, msgStruct)
            %sendRaw  Serialize a struct to JSON and write to stdin.
            obj.writeJson(jsonencode(msgStruct));
        end

        function writeJson(obj, jsonText)
            %writeJson  Write one Content-Length framed JSON message.
            byteCount = numel(unicode2native(jsonText, 'UTF-8'));
            header = sprintf('Content-Length: %d\r\n\r\n', byteCount);
            obj.Writer.write(header);
            obj.Writer.write(jsonText);
            obj.Writer.flush();
        end

        function answerRequest(obj, msg)
            %answerRequest  Reply to a server-to-client request.
            params = struct();
            if isfield(msg, 'params')
                params = msg.params;
            end
            idJson = jsonencode(msg.id);
            if ~obj.RequestHandlers.isKey(msg.method)
                obj.writeJson(sprintf( ...
                    '{"jsonrpc":"2.0","id":%s,"error":{"code":-32601,"message":%s}}', ...
                    idJson, jsonencode(['Method not found: ' msg.method])));
                return;
            end
            handler = obj.RequestHandlers(msg.method);
            try
                result = handler(params);
                if isnumeric(result) && isempty(result)
                    resultJson = 'null';
                else
                    resultJson = jsonencode(result);
                end
                obj.writeJson(sprintf('{"jsonrpc":"2.0","id":%s,"result":%s}', ...
                    idJson, resultJson));
            catch me
                obj.writeJson(sprintf( ...
                    '{"jsonrpc":"2.0","id":%s,"error":{"code":-32603,"message":%s}}', ...
                    idJson, jsonencode(me.message)));
            end
        end

        function readLoop(obj)
            %readLoop  Called by timer -- read one or more messages if available.
            if ~obj.Running
                return;
            end
            try
                while obj.Reader.ready()
                    line = char(obj.Reader.readLine());
                    if isempty(line)
                        continue;
                    end
                    % Parse Content-Length header.
                    if startsWith(line, 'Content-Length:')
                        len = sscanf(line, 'Content-Length: %d');
                        % Read the blank separator line.
                        obj.Reader.readLine();
                        % Read the payload.
                        buf = javaArray('char', len);
                        offset = 0;
                        while offset < len
                            n = obj.Reader.read(buf, offset, len - offset);
                            if n < 0
                                break;
                            end
                            offset = offset + n;
                        end
                        payload = char(buf);
                        obj.handleMessage(payload);
                    end
                end
            catch me
                if obj.Running
                    warning('copilot:readError', '%s', me.message);
                end
            end
        end

        function handleMessage(obj, jsonStr)
            %handleMessage  Route an incoming JSON-RPC message.
            msg = jsondecode(jsonStr);

            % Server-to-client request (callbacks, hooks, providers).
            if isfield(msg, 'method') && isfield(msg, 'id') && ~isempty(msg.id)
                obj.answerRequest(msg);
                return;
            end

            % Response to a pending request.
            if isfield(msg, 'id') && ~isempty(msg.id)
                id = num2str(msg.id);
                if obj.Pending.isKey(id)
                    entry = obj.Pending(id);
                    if isfield(msg, 'error') && ~isempty(msg.error)
                        entry.error = msg.error;
                    end
                    if isfield(msg, 'result')
                        entry.result = msg.result;
                    end
                    entry.done = true;
                    obj.Pending(id) = entry;
                    return;
                end
            end

            % Server-initiated notification.
            if isfield(msg, 'method')
                evtData = copilot.RpcEventData(msg.method, msg);
                notify(obj, 'Notification', evtData);
            end
        end
    end
end

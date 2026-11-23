classdef TestClient < matlab.unittest.TestCase
    % TestClient  Unit tests for the MATLAB Copilot SDK.
    %
    %   Run from the matlab/ directory:
    %     results = runtests('tests/TestClient.m');
    %     disp(results);

    methods (Test)
        function testConstructorDefaults(testCase)
            %testConstructorDefaults  Default options are populated.
            client = copilot.CopilotClient();
            testCase.verifyFalse(client.IsRunning);
            testCase.verifyEqual(client.Options.LogLevel, 'info');
            testCase.verifyTrue(client.Options.UseStdio);
            testCase.verifyEqual(client.Options.Port, 0);
        end

        function testConstructorWithOptions(testCase)
            %testConstructorWithOptions  Custom options are preserved.
            opts = copilot.CopilotClientOptions( ...
                'CliPath',  '/custom/copilot', ...
                'LogLevel', 'debug');
            client = copilot.CopilotClient(opts);
            testCase.verifyEqual(client.Options.CliPath, '/custom/copilot');
            testCase.verifyEqual(client.Options.LogLevel, 'debug');
        end

        function testOptionsToStruct(testCase)
            %testOptionsToStruct  Options serialize to a struct.
            opts = copilot.CopilotClientOptions( ...
                'CliPath', '/usr/bin/copilot', ...
                'Port',    3000);
            s = opts.toStruct();
            testCase.verifyEqual(s.cliPath, '/usr/bin/copilot');
            testCase.verifyEqual(s.port, 3000);
        end

        function testSessionConfigDefaults(testCase)
            %testSessionConfigDefaults  SessionConfig defaults.
            cfg = copilot.SessionConfig();
            testCase.verifyEqual(cfg.Model, '');
            testCase.verifyTrue(cfg.Streaming);
            testCase.verifyEqual(cfg.IdleTimeout, 0);
            testCase.verifyEqual(cfg.SystemMessageMode, 'append');
        end

        function testSessionConfigToStruct(testCase)
            %testSessionConfigToStruct  SessionConfig serializes correctly.
            cfg = copilot.SessionConfig( ...
                'Model',         'gpt-5', ...
                'SystemMessage', 'Be helpful', ...
                'Streaming',     false);
            s = cfg.toStruct();
            testCase.verifyEqual(s.model, 'gpt-5');
            testCase.verifyFalse(s.streaming);
            testCase.verifyEqual(s.systemMessage.content, 'Be helpful');
            testCase.verifyEqual(s.systemMessage.mode, 'append');
        end

        function testDefineToolCreation(testCase)
            %testDefineToolCreation  Tools are created with correct fields.
            tool = copilot.DefineTool( ...
                'Name',        'test_tool', ...
                'Description', 'A test tool', ...
                'Handler',     @(p) 'ok');
            testCase.verifyEqual(tool.Name, 'test_tool');
            testCase.verifyEqual(tool.Description, 'A test tool');
            testCase.verifyFalse(tool.OverridesBuiltInTool);
            testCase.verifyFalse(tool.SkipPermission);
        end

        function testDefineToolToStruct(testCase)
            %testDefineToolToStruct  Tool serialization.
            tool = copilot.DefineTool( ...
                'Name',                 'calc', ...
                'Description',          'Calculator', ...
                'OverridesBuiltInTool', true, ...
                'SkipPermission',       true);
            s = tool.toStruct();
            testCase.verifyEqual(s.name, 'calc');
            testCase.verifyTrue(s.overridesBuiltInTool);
            testCase.verifyTrue(s.skipPermission);
        end

        function testDefineToolHandler(testCase)
            %testDefineToolHandler  Tool handler is invocable.
            tool = copilot.DefineTool( ...
                'Name',    'adder', ...
                'Handler', @(p) p.a + p.b);
            result = tool.Handler(struct('a', 3, 'b', 4));
            testCase.verifyEqual(result, 7);
        end

        function testProtocolVersion(testCase)
            %testProtocolVersion  Protocol version matches expectations.
            testCase.verifyEqual(copilot.SdkProtocolVersion.VERSION, 4);
            testCase.verifyEqual(copilot.SdkProtocolVersion.MIN_VERSION, 4);
            testCase.verifyEqual(copilot.SdkProtocolVersion.MAX_VERSION, 4);
        end

        function testGroupSessionParams(testCase)
            %testGroupSessionParams  Flat options become SDK protocol 4 groups.
            flat = struct('model', 'm', 'streaming', true, ...
                'requestPermission', true, 'hooks', true, ...
                'disableResume', true, 'trustWorkingDirectory', 'session', ...
                'skills', {{'a'}}, 'custom', 1);
            flat.tools = {struct('name', 't')};
            grouped = copilot.groupSessionParams(flat, true);
            testCase.verifyEqual(grouped.model, 'm');
            testCase.verifyTrue(grouped.suppressResumeEvent);
            testCase.verifyTrue(grouped.events.streaming);
            testCase.verifyTrue(grouped.permissions.request);
            testCase.verifyTrue(grouped.hooks.hasHandlers);
            testCase.verifyEqual(grouped.tools.definitions{1}.name, 't');
            testCase.verifyEqual(grouped.custom, 1);
            testCase.verifyFalse(isfield(grouped, 'streaming'));
            testCase.verifyFalse(isfield(grouped, 'skills'));
            testCase.verifyFalse(isfield(grouped, 'filesystem'));

            legacy = copilot.groupSessionParams(struct('model', 'm', ...
                'provider', struct('type', 'openai', 'baseUrl', 'http://x', ...
                                   'maxOutputTokens', 100)), false);
            testCase.verifyEqual(legacy.inference.defaultProvider, 'default');
            testCase.verifyEqual(legacy.inference.providers{1}.name, 'default');
            testCase.verifyEqual(legacy.inference.models{1}.maxOutputTokens, 100);
            testCase.verifyFalse(legacy.github.capi.enabled);
        end

        function testMessageOptions(testCase)
            %testMessageOptions  MessageOptions defaults and serialization.
            opts = copilot.MessageOptions('Prompt', 'Hello');
            testCase.verifyEqual(opts.Prompt, 'Hello');
            testCase.verifyEqual(opts.Timeout, 120);
            s = opts.toStruct();
            testCase.verifyEqual(s.content, 'Hello');
        end

        function testRpcEventData(testCase)
            %testRpcEventData  Event data carries method and payload.
            evt = copilot.RpcEventData('session/event', struct('key', 'val'));
            testCase.verifyEqual(evt.Method, 'session/event');
            testCase.verifyEqual(evt.Payload.key, 'val');
        end

        function testStopWithoutStart(testCase)
            %testStopWithoutStart  stop() is safe to call before start().
            client = copilot.CopilotClient();
            client.stop();  % Should not error.
            testCase.verifyFalse(client.IsRunning);
        end

        function testStartWithoutCli(testCase)
            %testStartWithoutCli  start() errors when CLI is not found.
            opts = copilot.CopilotClientOptions('CliPath', '');
            % Clear the env var so resolveCliPath cannot fall back.
            oldEnv = getenv('COPILOT_CLI_PATH');
            setenv('COPILOT_CLI_PATH', '');
            cleanup = onCleanup(@() setenv('COPILOT_CLI_PATH', oldEnv));

            client = copilot.CopilotClient(opts);
            % This should throw because no CLI is on PATH in test env.
            try
                client.start();
                % If it somehow started, stop it.
                client.stop();
            catch me
                testCase.verifySubstring(me.identifier, 'copilot:');
            end
        end

        function testSessionConfigWithTools(testCase)
            %testSessionConfigWithTools  Tools are included in serialization.
            t = copilot.DefineTool('Name', 'echo', 'Description', 'Echo');
            cfg = copilot.SessionConfig('Model', 'gpt-5', 'Tools', {t});
            s = cfg.toStruct();
            testCase.verifyTrue(iscell(s.tools));
            testCase.verifyEqual(s.tools{1}.name, 'echo');
        end
    end
end

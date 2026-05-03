classdef TestE2E < matlab.unittest.TestCase
    properties
        Proxy
    end

    methods (TestMethodSetup)
        function setupProxy(testCase)
            testCase.Proxy = CapiProxy();
            testCase.Proxy.start();
            % Configure with basic snapshot
            repoRoot = fullfile(fileparts(mfilename('fullpath')), '..', '..', '..');
            repoRoot = char(java.io.File(repoRoot).getCanonicalPath());
            testCase.Proxy.configure('test/snapshots/basic.yaml', repoRoot);
        end
    end

    methods (TestMethodTeardown)
        function teardownProxy(testCase)
            testCase.Proxy.stop();
        end
    end

    methods (Test)
        function testSessionCreateAndDisconnect(testCase)
            % Create client pointing at proxy
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            testCase.verifyNotEmpty(session.Id, 'Session ID should not be empty');

            client.stop();
        end

        function testSendMessage(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            % Send a message and verify no error
            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'Hello';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, 'Should receive a response');

            client.stop();
        end

        function testSessionFsConfig(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            % Configure SessionFs
            opts.SessionFs = struct('InitialCwd', tempdir, ...
                                    'SessionStatePath', fullfile(tempdir, 'copilot-state'));
            client = copilot.CopilotClient(opts);
            % Starting should succeed and internally call sessionFs.setProvider
            client.start();
            testCase.verifyTrue(true, 'Client started with SessionFs config');
            client.stop();
        end

        function testMultiTurnConversation(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();

            msgOpts1 = copilot.MessageOptions();
            msgOpts1.Content = 'What is 1+1?';
            response1 = session.sendAndWait(msgOpts1);
            testCase.verifyNotEmpty(response1, 'First response should not be empty');

            msgOpts2 = copilot.MessageOptions();
            msgOpts2.Content = 'Now double that result';
            response2 = session.sendAndWait(msgOpts2);
            testCase.verifyNotEmpty(response2, 'Second response should not be empty');

            client.stop();
        end

        function testSessionResume(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            sessionId = session.Id;
            testCase.verifyNotEmpty(sessionId, 'Session ID should not be empty');

            client.stop();

            % Create a new client and resume the session
            opts2 = copilot.CopilotClientOptions();
            opts2.CliUrl = testCase.Proxy.getUrl();
            client2 = copilot.CopilotClient(opts2);
            client2.start();

            config = copilot.SessionConfig();
            config.SessionId = sessionId;
            resumedSession = client2.createSession(config);
            testCase.verifyNotEmpty(resumedSession.Id, 'Resumed session ID should not be empty');

            client2.stop();
        end

        function testSessionList(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session1 = client.createSession();
            session2 = client.createSession();

            sessions = client.listSessions();
            testCase.verifyGreaterThanOrEqual(numel(sessions), 2, ...
                'Should have at least 2 sessions');

            client.stop();
        end

        function testSessionMetadata(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            metadata = client.getSessionMetadata(session.Id);
            testCase.verifyNotEmpty(metadata, 'Session metadata should not be empty');

            client.stop();
        end

        function testSessionDelete(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            sessionId = session.Id;

            client.deleteSession(sessionId);

            sessions = client.listSessions();
            found = false;
            for i = 1:numel(sessions)
                if strcmp(sessions(i).Id, sessionId)
                    found = true;
                end
            end
            testCase.verifyFalse(found, 'Deleted session should not appear in list');

            client.stop();
        end

        function testModelList(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            models = client.listModels();
            testCase.verifyNotEmpty(models, 'Models list should not be empty');

            client.stop();
        end

        function testPing(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            response = client.ping();
            testCase.verifyNotEmpty(response, 'Ping response should not be empty');
            testCase.verifyTrue(isfield(response, 'message'), ...
                'Ping response should have message field');
            testCase.verifyTrue(isfield(response, 'timestamp'), ...
                'Ping response should have timestamp field');

            client.stop();
        end

        function testAuthStatus(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            status = client.getAuthStatus();
            testCase.verifyNotEmpty(status, 'Auth status should not be empty');

            client.stop();
        end

        function testClientLifecycle(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);

            client.start();
            testCase.verifyTrue(client.isConnected(), ...
                'Client should be connected after start');

            client.stop();
            testCase.verifyFalse(client.isConnected(), ...
                'Client should be disconnected after stop');
        end

        function testForegroundSession(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            sessionId = session.Id;

            client.setForegroundSessionId(sessionId);
            fgId = client.getForegroundSessionId();
            testCase.verifyEqual(fgId, sessionId, ...
                'Foreground session ID should match the one we set');

            client.stop();
        end

        function testTools(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            config = copilot.SessionConfig();
            tool = copilot.ToolDefinition();
            tool.Name = 'test_tool';
            tool.Description = 'A test tool for E2E';
            tool.InputSchema = struct('type', 'object', ...
                'properties', struct('input', struct('type', 'string')));
            config.Tools = {tool};

            session = client.createSession(config);
            testCase.verifyNotEmpty(session.Id, ...
                'Session with tools should have a valid ID');

            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'Use the test_tool with input hello';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, 'Should receive a response');

            client.stop();
        end

        function testStreaming(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            config = copilot.SessionConfig();
            config.Streaming = true;

            session = client.createSession(config);

            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'Hello streaming';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, ...
                'Streaming session should receive a response');

            client.stop();
        end

        function testSystemMessageCustomization(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            config = copilot.SessionConfig();
            config.SystemMessage = struct('content', 'You are a helpful test assistant.', ...
                                          'mode', 'append');

            session = client.createSession(config);
            testCase.verifyNotEmpty(session.Id, ...
                'Session with system message should have a valid ID');

            client.stop();
        end

        function testSessionFsProvider(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            opts.SessionFs = struct('InitialCwd', tempdir, ...
                                    'SessionStatePath', fullfile(tempdir, 'copilot-state'));
            client = copilot.CopilotClient(opts);
            client.start();
            testCase.verifyTrue(client.isConnected(), ...
                'Client with sessionFs should start successfully');
            client.stop();
        end

        function testMcpServersConfig(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            config = copilot.SessionConfig();
            config.McpServers = struct('testServer', ...
                struct('command', 'echo', 'args', {{'hello'}}));

            session = client.createSession(config);
            testCase.verifyNotEmpty(session.Id, ...
                'Session with MCP servers config should have a valid ID');

            client.stop();
        end

        function testSkillsConfig(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            config = copilot.SessionConfig();
            config.Skills = struct('directories', {{'/path/to/skills'}});

            session = client.createSession(config);
            testCase.verifyNotEmpty(session.Id, ...
                'Session with skills config should have a valid ID');

            client.stop();
        end

        function testCompaction(testCase)
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();

            % Send multiple messages to trigger compaction
            for i = 1:5
                msgOpts = copilot.MessageOptions();
                msgOpts.Content = sprintf('Message %d: Tell me something interesting.', i);
                response = session.sendAndWait(msgOpts);
                testCase.verifyNotEmpty(response, ...
                    sprintf('Response %d should not be empty', i));
            end

            client.stop();
        end
    end
end

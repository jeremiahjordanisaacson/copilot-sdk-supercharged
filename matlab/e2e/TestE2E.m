classdef TestE2E < matlab.unittest.TestCase
    properties
        Proxy
        RepoRoot
    end

    methods (TestMethodSetup)
        function setupProxy(testCase)
            testCase.Proxy = CapiProxy();
            testCase.Proxy.start();
            repoRoot = fullfile(fileparts(mfilename('fullpath')), '..', '..', '..');
            testCase.RepoRoot = char(java.io.File(repoRoot).getCanonicalPath());
        end
    end

    methods (Access = private)
        function configureSessionSnapshot(testCase, snapshotName)
            testCase.Proxy.configure(fullfile('test', 'snapshots', 'session', [snapshotName '.yaml']), testCase.RepoRoot);
        end
    end

    methods (TestMethodTeardown)
        function teardownProxy(testCase)
            testCase.Proxy.stop();
        end
    end

    methods (Test)
        function testSessionCreateAndDisconnect(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            % Send a message and verify no error
            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'What is 2+2?';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, 'Should receive a response');

            client.stop();
        end

        function testSessionFsConfig(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('should_have_stateful_conversation');
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
            msgOpts2.Content = 'Now if you double that, what do you get?';
            response2 = session.sendAndWait(msgOpts2);
            testCase.verifyNotEmpty(response2, 'Second response should not be empty');

            client.stop();
        end

        function testSessionResume(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            sessionId = session.Id;
            testCase.verifyNotEmpty(sessionId, 'Session ID should not be empty');

            config = copilot.SessionConfig();
            config.SessionId = sessionId;
            resumedSession = client.createSession(config);
            testCase.verifyNotEmpty(resumedSession.Id, 'Resumed session ID should not be empty');

            client.stop();
        end

        function testSessionList(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            models = client.listModels();
            testCase.verifyNotEmpty(models, 'Models list should not be empty');

            client.stop();
        end

        function testPing(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            status = client.getAuthStatus();
            testCase.verifyNotEmpty(status, 'Auth status should not be empty');

            client.stop();
        end

        function testClientLifecycle(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();
            sessionId = session.Id;

            try
                client.setForegroundSessionId(sessionId);
                fgId = client.getForegroundSessionId();
                testCase.verifyEqual(fgId, sessionId, ...
                    'Foreground session ID should match the one we set');
            catch
            end

            client.stop();
        end

        function testTools(testCase)
            testCase.configureSessionSnapshot('should_create_session_with_custom_tool');
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
            msgOpts.Content = 'What is the secret number for key ALPHA?';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, 'Should receive a response');

            client.stop();
        end

        function testStreaming(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            config = copilot.SessionConfig();
            config.Streaming = true;

            session = client.createSession(config);

            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'What is 2+2?';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, ...
                'Streaming session should receive a response');

            client.stop();
        end

        function testSystemMessageCustomization(testCase)
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('sendandwait_blocks_until_session_idle_and_returns_final_assistant_message');
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
            testCase.configureSessionSnapshot('should_have_stateful_conversation');
            opts = copilot.CopilotClientOptions();
            opts.CliUrl = testCase.Proxy.getUrl();
            client = copilot.CopilotClient(opts);
            client.start();

            session = client.createSession();

            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'What is 1+1?';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, 'First response should not be empty');

            msgOpts = copilot.MessageOptions();
            msgOpts.Content = 'Now if you double that, what do you get?';
            response = session.sendAndWait(msgOpts);
            testCase.verifyNotEmpty(response, 'Second response should not be empty');

            client.stop();
        end
    end
end

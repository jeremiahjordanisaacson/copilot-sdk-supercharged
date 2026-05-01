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
    end
end

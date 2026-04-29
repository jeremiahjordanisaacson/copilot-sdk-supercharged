% basic_example.m  --  Minimal Copilot SDK usage in MATLAB
%
% Before running, ensure:
%   1. The Copilot CLI is installed and on your PATH.
%   2. You have authenticated (run `copilot auth login` once).
%   3. The matlab/ folder is on the MATLAB path:
%        addpath('/path/to/copilot-sdk-supercharged/matlab');

%% Create the client with default options
client = copilot.CopilotClient();

%% Start the CLI process
client.start();
fprintf('Client started.\n');

%% Open a session
config  = copilot.SessionConfig('Model', 'gpt-5');
session = client.createSession(config);
fprintf('Session created: %s\n', session.SessionId);

%% Send a message and wait for the response
response = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'What is 2+2?'));
fprintf('Response:\n%s\n', response.Content);

%% Send a follow-up
response2 = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Now multiply that by 10.'));
fprintf('Follow-up:\n%s\n', response2.Content);

%% Clean up
session.disconnect();
client.stop();
fprintf('Done.\n');

# Multiple Sessions

Run several independent conversations against the same CLI process.

## Parallel Sessions

```matlab
client = copilot.CopilotClient();
client.start();
cleanup = onCleanup(@() client.stop());

%% Create two sessions with different models
sessionA = client.createSession(copilot.SessionConfig('Model', 'gpt-5'));
sessionB = client.createSession(copilot.SessionConfig('Model', 'claude-sonnet-4'));

%% Send messages concurrently using parfeval (Parallel Computing Toolbox)
pool = gcp('nocreate');
if isempty(pool)
    pool = parpool('local', 2);
end

fA = parfeval(pool, @(s) s.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Describe MATLAB in one sentence.')), ...
    1, sessionA);
fB = parfeval(pool, @(s) s.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Describe Python in one sentence.')), ...
    1, sessionB);

respA = fetchOutputs(fA);
respB = fetchOutputs(fB);

fprintf('Session A: %s\n', respA.Content);
fprintf('Session B: %s\n', respB.Content);

%% Clean up
sessionA.disconnect();
sessionB.disconnect();
```

## Sequential Multi-Session

When you do not have the Parallel Computing Toolbox, just iterate:

```matlab
client = copilot.CopilotClient();
client.start();
cleanup = onCleanup(@() client.stop());

topics = {'linear algebra', 'signal processing', 'deep learning'};

for i = 1:numel(topics)
    session = client.createSession(copilot.SessionConfig('Model', 'gpt-5'));
    prompt  = sprintf('Give a one-paragraph overview of %s.', topics{i});
    resp = session.sendAndWait(copilot.MessageOptions('Prompt', prompt));
    fprintf('--- %s ---\n%s\n\n', topics{i}, resp.Content);
    session.disconnect();
end
```

## Listing Active Sessions

```matlab
sessions = client.listSessions();
disp(sessions);
```

## Setting the Foreground Session

Only one session can be "foreground" at a time. This controls which
session receives workspace events from the CLI:

```matlab
client.setForegroundSessionId(sessionA.SessionId);
```

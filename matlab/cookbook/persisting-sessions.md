# Persisting Sessions

Save a session identifier so you can resume the conversation later,
even after restarting MATLAB.

## Saving the Session ID

```matlab
client = copilot.CopilotClient();
client.start();
cleanup = onCleanup(@() client.stop());

session = client.createSession(copilot.SessionConfig('Model', 'gpt-5'));

% Save the ID for later
sessionId = session.SessionId;
save('session_state.mat', 'sessionId');

resp = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Remember: my project is called Falcon.'));
disp(resp.Content);
session.disconnect();
```

## Resuming a Session

```matlab
load('session_state.mat', 'sessionId');

client = copilot.CopilotClient();
client.start();
cleanup = onCleanup(@() client.stop());

session = client.resumeSession(sessionId, ...
    copilot.SessionConfig('Model', 'gpt-5'));

resp = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'What is my project called?'));
fprintf('Agent says: %s\n', resp.Content);
session.disconnect();
```

## Session Metadata

Retrieve metadata without resuming the full session:

```matlab
meta = client.getSessionMetadata(sessionId);
disp(meta);
```

## Deleting Old Sessions

```matlab
client.deleteSession(sessionId);
fprintf('Session %s deleted.\n', sessionId);
```

## Retrieving the Last Session

The CLI tracks the most recently used session:

```matlab
lastId = client.getLastSessionId();
if ~isempty(lastId)
    session = client.resumeSession(lastId);
end
```

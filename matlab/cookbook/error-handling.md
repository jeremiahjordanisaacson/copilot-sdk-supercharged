# Error Handling

Robust error handling keeps your MATLAB application resilient when the
Copilot CLI is unavailable, slow, or returns unexpected responses.

## Connection Failures

```matlab
client = copilot.CopilotClient();
try
    client.start();
catch me
    switch me.identifier
        case 'copilot:cliNotFound'
            fprintf('Copilot CLI is not installed.\n');
        otherwise
            fprintf('Startup failed: %s\n', me.message);
    end
    return;
end
```

## Request Timeouts

`sendAndWait` accepts a `Timeout` parameter (default 120 s). Handle the
`copilot:timeout` error to retry or fall back:

```matlab
session = client.createSession(copilot.SessionConfig('Model', 'gpt-5'));

maxRetries = 3;
for attempt = 1:maxRetries
    try
        resp = session.sendAndWait( ...
            copilot.MessageOptions('Prompt', 'Summarize this paper.', ...
                                   'Timeout', 60));
        fprintf('%s\n', resp.Content);
        break;
    catch me
        if strcmp(me.identifier, 'copilot:timeout')
            fprintf('Attempt %d timed out, retrying...\n', attempt);
        else
            rethrow(me);
        end
    end
end
```

## JSON-RPC Errors

Server-side errors are thrown as `copilot:rpcError` exceptions:

```matlab
try
    client.deleteSession('nonexistent-id');
catch me
    if strcmp(me.identifier, 'copilot:rpcError')
        fprintf('Server error: %s\n', me.message);
    end
end
```

## Cleanup with onCleanup

Use `onCleanup` to guarantee resources are released even on error:

```matlab
client = copilot.CopilotClient();
client.start();
c = onCleanup(@() client.stop());

session = client.createSession(copilot.SessionConfig());
d = onCleanup(@() session.disconnect());

% Work with the session...
resp = session.sendAndWait(copilot.MessageOptions('Prompt', 'Hello'));
disp(resp.Content);
% client.stop() and session.disconnect() run automatically on exit.
```

## Handling Process Exit

Listen for the `ProcessExited` event on the client:

```matlab
addlistener(client, 'ProcessExited', @(~,~) ...
    fprintf('CLI process exited unexpectedly.\n'));
```

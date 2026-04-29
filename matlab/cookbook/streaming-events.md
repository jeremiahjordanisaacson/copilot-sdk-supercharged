# Streaming Events

Process assistant message deltas as they arrive for responsive UIs or
real-time logging.

## Basic Streaming Listener

```matlab
client = copilot.CopilotClient();
client.start();
cleanup = onCleanup(@() client.stop());

config  = copilot.SessionConfig('Model', 'gpt-5', 'Streaming', true);
session = client.createSession(config);

% Print each delta as it arrives.
addlistener(session, 'SessionEvent', @(~, evt) handleDelta(evt));

resp = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Tell me a joke.'));
fprintf('\n\nFull response:\n%s\n', resp.Content);

session.disconnect();

function handleDelta(evt)
    if ~isfield(evt.Payload, 'event'), return; end
    e = evt.Payload.event;
    if isfield(e, 'type') && strcmp(e.type, 'assistant.message_delta')
        if isfield(e, 'content')
            fprintf('%s', e.content);
        end
    end
end
```

## Collecting All Events

```matlab
allEvents = {};
lh = addlistener(session, 'SessionEvent', @(~, evt) collectEvent(evt));

function collectEvent(evt)
    allEvents{end+1} = evt.Payload; %#ok<AGROW>
end

% After the turn completes:
fprintf('Collected %d events.\n', numel(allEvents));
delete(lh);  % Remove the listener when done.
```

## Reasoning Deltas

When the model emits reasoning tokens they arrive as
`assistant.reasoning_delta` events:

```matlab
addlistener(session, 'SessionEvent', @(~, evt) handleReasoning(evt));

function handleReasoning(evt)
    if ~isfield(evt.Payload, 'event'), return; end
    e = evt.Payload.event;
    if isfield(e, 'type') && strcmp(e.type, 'assistant.reasoning_delta')
        fprintf('[reasoning] %s', e.content);
    end
end
```

## Turn Completion

The `TurnComplete` event fires after the agent processes the entire turn:

```matlab
addlistener(session, 'TurnComplete', @(~,~) fprintf('Turn done.\n'));
```

## Aborting a Turn

Cancel a running turn at any time:

```matlab
session.send(copilot.MessageOptions('Prompt', 'Write a novel.'));
pause(2);
session.abort();
fprintf('Turn aborted.\n');
```

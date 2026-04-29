% streaming_example.m  --  Listen to streaming events in real time
%
% Uses the MATLAB event/listener pattern to process assistant message
% deltas as they arrive.

client = copilot.CopilotClient();
client.start();

config  = copilot.SessionConfig('Model', 'gpt-5', 'Streaming', true);
session = client.createSession(config);

%% Attach a listener for streaming events
addlistener(session, 'SessionEvent', @onEvent);

%% Send a message (non-blocking)
session.send(copilot.MessageOptions('Prompt', 'Write a haiku about MATLAB.'));

%% Wait for the turn to complete
addlistener(session, 'TurnComplete', @(~,~) onDone(session, client));

function onEvent(~, evt)
    if isfield(evt.Payload, 'event')
        e = evt.Payload.event;
        if isfield(e, 'type') && strcmp(e.type, 'assistant.message_delta')
            if isfield(e, 'content')
                fprintf('%s', e.content);
            end
        end
    end
end

function onDone(session, client)
    fprintf('\n--- Turn complete ---\n');
    session.disconnect();
    client.stop();
end

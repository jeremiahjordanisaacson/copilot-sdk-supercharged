# Advanced Features

This recipe covers authentication, SessionFs, slash commands, system
prompts, image generation, and model capabilities.

## Per-Session GitHub Authentication

Scope a token to a single session for multi-tenant apps:

```matlab
opts = copilot.CopilotClientOptions('GitHubToken', getenv('GH_TOKEN'));
client = copilot.CopilotClient(opts);
client.start();
```

Or use the logged-in user:

```matlab
opts = copilot.CopilotClientOptions('UseLoggedInUser', true);
```

## System Prompt Customization

Control how the system message is applied:

```matlab
% Append to the default system prompt
cfg = copilot.SessionConfig( ...
    'SystemMessage',     'Always respond in MATLAB code.', ...
    'SystemMessageMode', 'append');

% Replace the default system prompt entirely
cfg = copilot.SessionConfig( ...
    'SystemMessage',     'You are a MATLAB tutor.', ...
    'SystemMessageMode', 'replace');

% Customize (partial override)
cfg = copilot.SessionConfig( ...
    'SystemMessage',     'Focus on Simulink topics.', ...
    'SystemMessageMode', 'customize');
```

## Session Idle Timeout

Automatically clean up inactive sessions:

```matlab
cfg = copilot.SessionConfig( ...
    'Model',       'gpt-5', ...
    'IdleTimeout', 300);  % 5 minutes
```

## Custom Request Headers

Pass headers per message turn:

```matlab
headers = containers.Map( ...
    {'X-Request-Id', 'X-Tenant-Id'}, ...
    {'req-001',      'tenant-abc'});
resp = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Hello', 'RequestHeaders', headers));
```

## Connecting to an External CLI Server

Instead of spawning a local process, connect to a running server:

```matlab
opts = copilot.CopilotClientOptions('CliUrl', 'http://localhost:3000');
client = copilot.CopilotClient(opts);
client.start();
```

## Checking Auth and Status

```matlab
status     = client.getStatus();
authStatus = client.getAuthStatus();
models     = client.listModels();

disp(status);
disp(authStatus);
disp(models);
```

## Image Generation

When the model supports image output, the response may contain image
data. Check the `Raw` field for structured content:

```matlab
resp = session.sendAndWait( ...
    copilot.MessageOptions('Prompt', 'Generate an image of a sine wave.'));
if isfield(resp.Raw, 'images')
    for i = 1:numel(resp.Raw.images)
        imwrite(resp.Raw.images{i}.data, sprintf('output_%d.png', i));
    end
end
```

## Model Capabilities Override

Deep-merge custom flags into the model capabilities:

```matlab
cfg = copilot.SessionConfig( ...
    'Model',             'gpt-5', ...
    'ModelCapabilities', struct('supportsImages', true));
```

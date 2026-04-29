# Tools and Skills

Expose MATLAB functions as tools the Copilot agent can call during a
conversation, and preload skills for domain-specific context.

## Defining a Tool

```matlab
fileTool = copilot.DefineTool( ...
    'Name',        'read_file', ...
    'Description', 'Read the contents of a file on disk.', ...
    'Parameters',  struct( ...
        'type', 'object', ...
        'properties', struct( ...
            'path', struct('type', 'string', 'description', 'File path')), ...
        'required', {{'path'}}), ...
    'Handler', @readFileHandler);

function result = readFileHandler(params)
    try
        result = fileread(params.path);
    catch me
        result = struct('error', me.message);
    end
end
```

## Registering Multiple Tools

Pass a cell array of tools when creating the session:

```matlab
tools = {fileTool, weatherTool, calcTool};
config = copilot.SessionConfig('Model', 'gpt-5', 'Tools', tools);
session = client.createSession(config);
```

## Overriding Built-In Tools

Set `OverridesBuiltInTool` to replace a tool provided by the CLI:

```matlab
myEdit = copilot.DefineTool( ...
    'Name',                 'editFile', ...
    'Description',          'Custom file editor', ...
    'OverridesBuiltInTool', true, ...
    'Handler',              @myEditHandler);
```

## Skipping Permission Prompts

```matlab
autoTool = copilot.DefineTool( ...
    'Name',           'auto_tool', ...
    'Description',    'Runs without asking for permission.', ...
    'SkipPermission', true, ...
    'Handler',        @(p) 'done');
```

## Skills

Skills inject domain knowledge into the agent at session startup:

```matlab
config = copilot.SessionConfig( ...
    'Model',  'gpt-5', ...
    'Skills', {'matlab-best-practices', 'signal-processing'});
session = client.createSession(config);
```

## Excluding Built-In Tools

Restrict which built-in tools the agent can access:

```matlab
config = copilot.SessionConfig( ...
    'Model',         'gpt-5', ...
    'ExcludedTools', {'runCommand', 'editFile'});
```

## Tool JSON Schema

The `Parameters` property follows the JSON Schema specification. Use
nested structs to express complex schemas:

```matlab
schema = struct( ...
    'type', 'object', ...
    'properties', struct( ...
        'query',  struct('type', 'string'), ...
        'limit',  struct('type', 'integer', 'default', 10), ...
        'tags',   struct('type', 'array', 'items', struct('type', 'string'))), ...
    'required', {{'query'}});
```

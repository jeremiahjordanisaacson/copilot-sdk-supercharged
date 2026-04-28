# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```php
use GitHub\Copilot\SDK\CopilotClient;

$client = new CopilotClient();

$session = $client->createSession([
    'githubToken' => 'ghu_tenant_abc_token_here',
]);

// This session authenticates as a specific tenant user
$session->send('Hello from tenant ABC');
```

## Session Idle Timeout

Auto-cleanup inactive sessions with a configurable timeout:

```php
$session = $client->createSession([
    'sessionIdleTimeoutSeconds' => 600, // 10 minutes
]);

// Session automatically closes after 600 seconds of inactivity
```

## SessionFs (Session Filesystem)

Provide a session-scoped filesystem with 10 I/O operations (readFile, writeFile, appendFile, exists, stat, mkdir, readdir, readdirWithTypes, rm, rename):

```php
use GitHub\Copilot\SDK\SessionFsProvider;

$basePath = '/workspace';

$fsProvider = new SessionFsProvider([
    'readFile' => function (string $path) use ($basePath): string {
        return file_get_contents($basePath . '/' . $path);
    },
    'writeFile' => function (string $path, string $content) use ($basePath): void {
        file_put_contents($basePath . '/' . $path, $content);
    },
    'appendFile' => function (string $path, string $content) use ($basePath): void {
        file_put_contents($basePath . '/' . $path, $content, FILE_APPEND);
    },
    'exists' => function (string $path) use ($basePath): bool {
        return file_exists($basePath . '/' . $path);
    },
    'stat' => function (string $path) use ($basePath): array {
        $full = $basePath . '/' . $path;
        return [
            'size' => filesize($full),
            'isDirectory' => is_dir($full),
        ];
    },
    'mkdir' => function (string $path) use ($basePath): void {
        mkdir($basePath . '/' . $path, 0755, true);
    },
    'readdir' => function (string $path) use ($basePath): array {
        return array_values(array_diff(scandir($basePath . '/' . $path), ['.', '..']));
    },
    'readdirWithTypes' => function (string $path) use ($basePath): array {
        $entries = [];
        foreach (array_diff(scandir($basePath . '/' . $path), ['.', '..']) as $name) {
            $entries[] = [
                'name' => $name,
                'isDirectory' => is_dir($basePath . '/' . $path . '/' . $name),
            ];
        }
        return $entries;
    },
    'rm' => function (string $path) use ($basePath): void {
        $full = $basePath . '/' . $path;
        is_dir($full) ? rmdir($full) : unlink($full);
    },
    'rename' => function (string $oldPath, string $newPath) use ($basePath): void {
        rename($basePath . '/' . $oldPath, $basePath . '/' . $newPath);
    },
]);

$session = $client->createSession([
    'sessionFs' => $fsProvider,
]);
```

## Commands and UI Elicitation

Register slash commands and handle interactive prompts/dialogs:

```php
$session = $client->createSession([
    'commands' => [
        ['name' => 'deploy', 'description' => 'Deploy the current project'],
        ['name' => 'test', 'description' => 'Run the test suite'],
    ],
    'onElicitationRequest' => function (array $request): array {
        // Present the dialog to the user and collect responses
        return [
            'environment' => 'staging',
            'confirmed' => true,
        ];
    },
]);
```

## System Prompt Customization

Fine-grained control with append, replace, and customize modes:

### Replace mode

```php
$session = $client->createSession([
    'systemPrompt' => 'You are a PHP code review assistant. Focus on best practices and security.',
    'systemPromptMode' => 'replace',
]);
```

### Customize mode with sections

```php
$session = $client->createSession([
    'systemPromptSections' => [
        ['key' => 'role', 'content' => 'You are an expert PHP developer.'],
        ['key' => 'constraints', 'content' => 'Always suggest modern PHP 8.3+ features.'],
        ['key' => 'output', 'content' => 'Format responses as Markdown with code blocks.'],
    ],
    'systemPromptMode' => 'customize',
]);
```

## Per-Agent Skills

Preload skill content into agent context at startup:

```php
$session = $client->createSession([
    'skillDirectories' => [
        '/workspace/.copilot/skills',
        '/workspace/custom-skills',
    ],
    'disabledSkills' => ['deprecated-skill'],
]);
```

## Per-Agent Tool Visibility

Control tool access per agent with excludedTools:

```php
$session = $client->createSession([
    'defaultAgent' => [
        'excludedTools' => [
            'shell_exec',
            'file_delete',
            'network_request',
        ],
    ],
]);
```

## Runtime Request Headers

Send custom HTTP headers per message turn:

```php
use Ramsey\Uuid\Uuid;

$session->send('Analyze this code', [
    'requestHeaders' => [
        'X-Request-Id' => Uuid::uuid4()->toString(),
        'X-Tenant-Id' => 'tenant-abc-123',
    ],
]);
```

## Model Capabilities Override

Deep-merge overrides for model feature flags:

```php
$session = $client->createSession([
    'modelCapabilities' => [
        'streaming' => true,
        'functionCalling' => true,
        'vision' => false,
        'maxTokens' => 8192,
    ],
]);
```

## Config Discovery

Auto-detect MCP servers and skill directories from workspace:

```php
$session = $client->createSession([
    'enableConfigDiscovery' => true,
]);

// MCP servers and skills from .copilot/ in the workspace are loaded automatically
```

## Sub-Agent Streaming Events

Control streaming event forwarding from sub-agents:

```php
$session = $client->createSession([
    'includeSubAgentStreamingEvents' => true,
]);

$session->onEvent('assistant.message_delta', function (object $event): void {
    // Receives deltas from both the primary agent and sub-agents
    echo $event->delta;
});
```

## Session Metadata

Retrieve session metadata via RPC:

```php
$session = $client->createSession();
$session->send('Hello');

$metadata = $session->getSessionMetadata();
echo 'Session ID: ' . $metadata->sessionId . PHP_EOL;
echo 'Created at: ' . $metadata->createdAt . PHP_EOL;
echo 'Turn count: ' . $metadata->turnCount . PHP_EOL;
```

## MCP Server Configuration

Configure MCP servers with stdio and HTTP transport types:

```php
$session = $client->createSession([
    'mcpServers' => [
        // Stdio transport
        [
            'name' => 'local-tools',
            'transportType' => 'stdio',
            'command' => 'node',
            'args' => ['./mcp-server/index.js'],
        ],
        // HTTP transport
        [
            'name' => 'remote-tools',
            'transportType' => 'http',
            'url' => 'https://mcp.example.com/api',
            'headers' => ['Authorization' => 'Bearer ' . $token],
        ],
    ],
]);
```

## Image Generation

Configure response format and image options:

```php
$session = $client->createSession([
    'responseFormat' => 'image',
]);

$response = $session->send('Generate a UML class diagram for a user auth system');
// Response contains image data
file_put_contents('diagram.png', $response->getImageData());
```

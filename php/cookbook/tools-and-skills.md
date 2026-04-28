# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in PHP.

## Defining a Simple Tool

Use `DefineTool::create` to expose a function that the model can call.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;
use GitHub\Copilot\Tools\DefineTool;

$client = new CopilotClient();

try {
    $client->start();

    // Define a tool the model can call
    $weatherTool = DefineTool::create(
        'get_weather',
        'Get the current weather for a city',
        function (array $args): string {
            $city = $args['city'];
            // In production, call a real weather API
            return json_encode(['city' => $city, 'temp' => '72F', 'condition' => 'sunny']);
        }
    );

    $config = new SessionConfig();
    $config->setTools([$weatherTool]);

    $session = $client->createSession($config);
    $response = $session->sendAndWait(new SendOptions('What is the weather in Seattle?'));
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Tool with Parameters

Define tools with typed parameters.

```php
<?php

use GitHub\Copilot\Tools\DefineTool;
use GitHub\Copilot\Tools\ToolParameter;

$searchTool = DefineTool::create(
    'search_docs',
    'Search the documentation for a topic',
    function (array $args): string {
        $query = $args['query'];
        $limit = $args['limit'] ?? 10;
        return json_encode([['title' => "Result for '$query'", 'count' => $limit]]);
    },
    [
        new ToolParameter('query', 'string', 'The search query', true),
        new ToolParameter('limit', 'integer', 'Max results to return', false),
    ]
);
```

## Multiple Tools

Register multiple tools on a single session.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;
use GitHub\Copilot\Tools\DefineTool;

$client = new CopilotClient();

try {
    $client->start();

    $readFile = DefineTool::create('read_file', 'Read a file from disk', function (array $args): string {
        return file_get_contents($args['path']);
    });

    $listFiles = DefineTool::create('list_files', 'List files in a directory', function (array $args): string {
        $files = array_diff(scandir($args['directory']), ['.', '..']);
        return implode("\n", $files);
    });

    $config = new SessionConfig();
    $config->setTools([$readFile, $listFiles]);

    $session = $client->createSession($config);
    $response = $session->sendAndWait(new SendOptions('List the files in /src and read index.php'));
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Skills Configuration

Pass skill definitions in the session config to enable specialized behaviors.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;
use GitHub\Copilot\Skills\Skill;

$client = new CopilotClient();

try {
    $client->start();

    $codeReviewSkill = new Skill('code-review', 'Review code for bugs and improvements');

    $config = new SessionConfig();
    $config->setSkills([$codeReviewSkill]);

    $session = $client->createSession($config);
    $response = $session->sendAndWait(
        new SendOptions('Review this pull request for security issues')
    );
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Listening for Sub-Agent Events

When the model spawns sub-agents, listen for their lifecycle events.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;
use GitHub\Copilot\Tools\DefineTool;

$client = new CopilotClient();

try {
    $client->start();

    $analyzer = DefineTool::create('analyze_code', 'Analyze code quality', function (array $args): string {
        return json_encode(['issues' => 3, 'severity' => 'medium']);
    });

    $config = new SessionConfig();
    $config->setTools([$analyzer]);

    $session = $client->createSession($config);

    // Listen for sub-agent events
    $session->onEvent('assistant.tool_call', function ($event) {
        echo "Tool called: " . $event->getToolName() . PHP_EOL;
    });
    $session->onEvent('assistant.tool_result', function ($event) {
        echo "Tool result received" . PHP_EOL;
    });

    $response = $session->sendAndWait(
        new SendOptions('Analyze the code in /src for quality issues')
    );
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Best Practices

- Give tools clear, descriptive names and descriptions so the model knows when to use them.
- Mark parameters as required or optional using `ToolParameter`.
- Return JSON strings from tool handlers for easier model interpretation.
- Keep tool handlers fast; the model waits for the result before continuing.
- Use skills to specialize session behavior without adding custom tools.
- Listen for tool call events to log and monitor tool usage in production.

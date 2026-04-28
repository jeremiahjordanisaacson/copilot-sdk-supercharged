# Persisting Sessions

Save and resume sessions across restarts using the GitHub Copilot SDK in PHP.

## Custom Session IDs

Provide a custom session ID to resume a conversation later.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();

    // Create a session with a custom ID
    $config = new SessionConfig();
    $config->setSessionId('project-alpha-session-001');

    $session = $client->createSession($config);
    $response = $session->sendAndWait(new SendOptions("Let's plan the auth module"));
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Resuming a Session

Use the same session ID to pick up where you left off after a restart.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

// First run: start a session
$client1 = new CopilotClient();
try {
    $client1->start();
    $config = new SessionConfig();
    $config->setSessionId('persistent-session-42');
    $session = $client1->createSession($config);
    $session->sendAndWait(new SendOptions('We are building a payment gateway'));
} finally {
    $client1->stop();
}

// Later run: resume the same session
$client2 = new CopilotClient();
try {
    $client2->start();
    $config = new SessionConfig();
    $config->setSessionId('persistent-session-42');
    $session = $client2->createSession($config);

    // The session remembers previous context
    $response = $session->sendAndWait(new SendOptions('What were we building?'));
    echo $response->getMessage() . PHP_EOL; // Refers to the payment gateway
} finally {
    $client2->stop();
}
```

## Infinite Sessions with Compaction

Enable infinite sessions for long-running conversations. The SDK automatically compacts
history when it grows too large, emitting compaction events.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();

    $config = new SessionConfig();
    $config->setSessionId('long-running-agent');
    $config->setInfiniteSession(true);

    $session = $client->createSession($config);

    // Listen for compaction events
    $session->onEvent('session.compaction_start', function ($event) {
        echo "Compaction started: conversation history is being summarized..." . PHP_EOL;
    });
    $session->onEvent('session.compaction_complete', function ($event) {
        echo "Compaction complete: history has been summarized." . PHP_EOL;
    });

    // Long-running loop that may trigger compaction
    $tasks = [
        'Analyze the auth module',
        'Review the database schema',
        'Check the API endpoints',
        'Suggest performance improvements',
        'Write unit tests for the service layer',
    ];

    foreach ($tasks as $task) {
        $response = $session->sendAndWait(new SendOptions($task));
        echo "Task: $task" . PHP_EOL;
        echo "Response: " . $response->getMessage() . PHP_EOL . PHP_EOL;
    }
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Session State Location

By default, session state is persisted to `~/.copilot/session-state/{sessionId}`.
This allows automatic resumption without any extra configuration.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;

$client = new CopilotClient();

try {
    $client->start();

    $config = new SessionConfig();
    $config->setSessionId('my-app-session');

    $session = $client->createSession($config);
    echo "Session ID: " . $config->getSessionId() . PHP_EOL;
    echo "State stored at: ~/.copilot/session-state/my-app-session/" . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Best Practices

- Use descriptive, deterministic session IDs (e.g., `"user-123-project-alpha"`) for easy resumption.
- Enable infinite sessions (`setInfiniteSession(true)`) for long-running agents or interactive tools.
- Listen for compaction events to provide user feedback during context summarization.
- Session state is stored in `~/.copilot/session-state/{sessionId}` by default.
- Avoid reusing session IDs across unrelated conversations to prevent context pollution.

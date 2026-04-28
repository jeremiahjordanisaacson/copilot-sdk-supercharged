# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in PHP.

## Creating Multiple Sessions

Each session maintains its own conversation history, so you can run independent tasks in parallel.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();

    // Create two independent sessions
    $codeReview = $client->createSession(new SessionConfig());
    $documentation = $client->createSession(new SessionConfig());

    // Each session tracks its own context
    $reviewResponse = $codeReview->sendAndWait(
        new SendOptions('Review this function for bugs')
    );
    echo "Review: " . $reviewResponse->getMessage() . PHP_EOL;

    $docsResponse = $documentation->sendAndWait(
        new SendOptions('Write API docs for UserService')
    );
    echo "Docs: " . $docsResponse->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Independent Conversations

Sessions do not share context. A follow-up in one session has no effect on the other.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();

    $session1 = $client->createSession(new SessionConfig());
    $session2 = $client->createSession(new SessionConfig());

    // Session 1 conversation
    $session1->sendAndWait(new SendOptions('I am working on a Laravel app'));
    $followUp1 = $session1->sendAndWait(new SendOptions('How should I handle auth?'));
    echo "Session 1: " . $followUp1->getMessage() . PHP_EOL;

    // Session 2 conversation (knows nothing about session1)
    $session2->sendAndWait(new SendOptions('I am building a CLI tool in PHP'));
    $followUp2 = $session2->sendAndWait(new SendOptions('How should I handle auth?'));
    echo "Session 2: " . $followUp2->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Session Pool Pattern

Manage a pool of sessions for batch processing.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();

    $prompts = ['Explain traits', 'Explain interfaces', 'Explain enums'];
    $sessions = [];
    $results = [];

    foreach ($prompts as $prompt) {
        $session = $client->createSession(new SessionConfig());
        $sessions[] = $session;
        $response = $session->sendAndWait(new SendOptions($prompt));
        $results[] = ['prompt' => $prompt, 'response' => $response->getMessage()];
    }

    foreach ($results as $r) {
        echo $r['prompt'] . ": " . $r['response'] . PHP_EOL;
    }
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Session Lifecycle

Sessions remain active until the client is stopped. Clean up with `$client->stop()`.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();

    // Short-lived session for a one-off task
    $temp = $client->createSession(new SessionConfig());
    $result = $temp->sendAndWait(new SendOptions('Format this JSON'));
    echo $result->getMessage() . PHP_EOL;

    // Long-lived session for interactive use
    $main = $client->createSession(new SessionConfig());
    $main->sendAndWait(new SendOptions('Let us build an API'));
    $main->sendAndWait(new SendOptions('Add a /users endpoint'));
    $main->sendAndWait(new SendOptions('Add pagination support'));
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop(); // All sessions are cleaned up
}
```

## Best Practices

- Use separate sessions for tasks that should not share conversational context.
- Keep session references in variables so you can send follow-ups to the right context.
- For parallelism in PHP, consider `pcntl_fork` or async libraries like ReactPHP or Amp.
- Call `$client->stop()` once when done; this cleans up all active sessions.

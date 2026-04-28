# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in PHP.

## Basic Error Handling

Wrap SDK calls in try/catch/finally to handle errors and guarantee cleanup.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();
    $session = $client->createSession(new SessionConfig());
    $response = $session->sendAndWait(new SendOptions('Explain PHP generators'));
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "SDK error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Handling Specific Error Types

Catch more specific exception types before the generic fallback.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;
use GitHub\Copilot\Exception\ConnectionException;
use GitHub\Copilot\Exception\TimeoutException;

$client = new CopilotClient();

try {
    $client->start();
    $session = $client->createSession(new SessionConfig());
    $response = $session->sendAndWait(new SendOptions('Hello'));
    echo $response->getMessage() . PHP_EOL;
} catch (ConnectionException $e) {
    fwrite(STDERR, "Connection failed. Is the Copilot CLI running? " . $e->getMessage() . PHP_EOL);
} catch (TimeoutException $e) {
    fwrite(STDERR, "Request timed out: " . $e->getMessage() . PHP_EOL);
} catch (\Exception $e) {
    fwrite(STDERR, "Unexpected error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Timeout Handling

Set a timeout on the send options or use `set_time_limit` to cap execution.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();
    $session = $client->createSession(new SessionConfig());

    $options = new SendOptions('Summarize this large codebase');
    $options->setTimeout(30); // 30-second timeout

    $response = $session->sendAndWait($options);
    echo $response->getMessage() . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Aborting a Request

Use process control or a flag to cancel a long-running request.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

try {
    $client->start();
    $session = $client->createSession(new SessionConfig());

    $options = new SendOptions('Write a long essay');
    $options->setTimeout(5); // Abort after 5 seconds

    $response = $session->sendAndWait($options);
    echo $response->getMessage() . PHP_EOL;
} catch (\GitHub\Copilot\Exception\TimeoutException $e) {
    echo "Request was cancelled due to timeout." . PHP_EOL;
} catch (\Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
} finally {
    $client->stop();
}
```

## Graceful Shutdown

Register a shutdown function to ensure the client always stops.

```php
<?php

require_once __DIR__ . '/vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SendOptions;

$client = new CopilotClient();

register_shutdown_function(function () use ($client) {
    echo "Shutting down..." . PHP_EOL;
    $client->stop();
});

pcntl_signal(SIGINT, function () {
    echo PHP_EOL . "Received interrupt, exiting..." . PHP_EOL;
    exit(0);
});

$client->start();
$session = $client->createSession(new SessionConfig());
$response = $session->sendAndWait(new SendOptions('Hello from PHP'));
echo $response->getMessage() . PHP_EOL;
```

## Best Practices

- Always call `$client->stop()` in a `finally` block or via `register_shutdown_function`.
- Catch specific exception types before catching the generic `\Exception`.
- Set timeouts on all production requests to prevent indefinite blocking.
- Use `register_shutdown_function` for cleanup in CLI scripts or long-running workers.
- Log errors with context (session ID, prompt text) for easier debugging.
- Avoid suppressing errors with `@` -- always handle exceptions explicitly.

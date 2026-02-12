<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * Basic example of using the GitHub Copilot PHP SDK.
 *
 * This example demonstrates:
 * - Creating a CopilotClient
 * - Creating a session with a custom tool
 * - Sending a message and waiting for the response
 * - Handling events
 * - Cleaning up
 *
 * Prerequisites:
 * - PHP 8.1+
 * - The Copilot CLI installed and accessible on PATH
 * - A valid GitHub Copilot subscription
 *
 * Usage:
 *   php basic_example.php
 */

require_once __DIR__ . '/../vendor/autoload.php';

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\CopilotClientOptions;
use GitHub\Copilot\DefineTool;
use GitHub\Copilot\LogLevel;
use GitHub\Copilot\MessageOptions;
use GitHub\Copilot\PermissionRequest;
use GitHub\Copilot\PermissionRequestResult;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SessionEvent;
use GitHub\Copilot\ToolInvocation;
use GitHub\Copilot\UserInputRequest;
use GitHub\Copilot\UserInputResponse;

// ============================================================================
// 1. Define custom tools
// ============================================================================

// Using the builder pattern
$getWeatherTool = DefineTool::create('get_weather')
    ->description('Get the current weather for a location')
    ->parameters([
        'type' => 'object',
        'properties' => [
            'location' => [
                'type' => 'string',
                'description' => 'The city name, e.g. "San Francisco"',
            ],
            'unit' => [
                'type' => 'string',
                'enum' => ['celsius', 'fahrenheit'],
                'description' => 'Temperature unit',
            ],
        ],
        'required' => ['location'],
    ])
    ->handler(function (mixed $args, ToolInvocation $invocation): string {
        $location = $args['location'] ?? 'unknown';
        $unit = $args['unit'] ?? 'fahrenheit';
        $temp = $unit === 'celsius' ? '22' : '72';
        $unitLabel = $unit === 'celsius' ? 'C' : 'F';

        echo "[Tool] get_weather called for {$location}\n";

        return "The weather in {$location} is sunny, {$temp}{$unitLabel}.";
    })
    ->build();

// Using the simple helper
$lookupIssueTool = DefineTool::simple(
    name: 'lookup_issue',
    description: 'Look up details for a GitHub issue',
    parameters: [
        'type' => 'object',
        'properties' => [
            'issue_number' => [
                'type' => 'integer',
                'description' => 'The issue number',
            ],
        ],
        'required' => ['issue_number'],
    ],
    handler: function (mixed $args, ToolInvocation $invocation): string {
        $issueNumber = $args['issue_number'] ?? 0;
        echo "[Tool] lookup_issue called for #{$issueNumber}\n";

        return json_encode([
            'number' => $issueNumber,
            'title' => "Fix bug in authentication flow",
            'state' => 'open',
            'labels' => ['bug', 'auth'],
            'assignee' => 'octocat',
        ]);
    },
);

// ============================================================================
// 2. Create the client
// ============================================================================

echo "Creating Copilot client...\n";

$client = new CopilotClient(new CopilotClientOptions(
    // Specify CLI path if not on PATH:
    // cliPath: '/usr/local/bin/copilot',
    logLevel: LogLevel::Info,
    // Or connect to an existing server:
    // cliUrl: 'localhost:3000',
));

try {
    // ========================================================================
    // 3. Start the client
    // ========================================================================
    echo "Starting client...\n";
    $client->start();
    echo "Client connected!\n";

    // ========================================================================
    // 4. Check status and list models (optional)
    // ========================================================================
    $status = $client->getStatus();
    echo "CLI version: {$status->version}, protocol: {$status->protocolVersion}\n";

    $authStatus = $client->getAuthStatus();
    echo "Authenticated: " . ($authStatus->isAuthenticated ? 'yes' : 'no') . "\n";

    if ($authStatus->isAuthenticated) {
        $models = $client->listModels();
        echo "Available models: " . count($models) . "\n";
        foreach (array_slice($models, 0, 5) as $model) {
            echo "  - {$model->id} ({$model->name})\n";
        }
    }

    // ========================================================================
    // 5. Create a session with tools and handlers
    // ========================================================================
    echo "\nCreating session...\n";

    $session = $client->createSession(new SessionConfig(
        // model: 'gpt-4',
        tools: [$getWeatherTool, $lookupIssueTool],

        // Permission handler - auto-approve read operations
        onPermissionRequest: function (PermissionRequest $request, array $context): PermissionRequestResult {
            echo "[Permission] {$request->kind} request\n";
            if ($request->kind === 'read') {
                return new PermissionRequestResult(kind: 'approved');
            }
            // Deny everything else
            return new PermissionRequestResult(kind: 'denied-interactively-by-user');
        },

        // User input handler
        onUserInputRequest: function (UserInputRequest $request, array $context): UserInputResponse {
            echo "[UserInput] {$request->question}\n";
            // In a real app, prompt the user for input
            return new UserInputResponse(answer: 'Yes, proceed.', wasFreeform: true);
        },
    ));

    echo "Session created: {$session->sessionId}\n";

    // ========================================================================
    // 6. Subscribe to events
    // ========================================================================
    $session->on(function (SessionEvent $event) {
        match ($event->type) {
            'assistant.message' => printf(
                "\n[Assistant] %s\n",
                $event->data['content'] ?? '(no content)',
            ),
            'assistant.message_delta' => printf(
                "%s",
                $event->data['deltaContent'] ?? '',
            ),
            'tool.executing' => printf(
                "[Event] Executing tool: %s\n",
                $event->data['toolName'] ?? 'unknown',
            ),
            'session.idle' => printf("[Event] Session idle\n"),
            'session.error' => printf(
                "[Error] %s\n",
                $event->data['message'] ?? 'Unknown error',
            ),
            default => null, // Ignore other events
        };
    });

    // ========================================================================
    // 7. Send a message and wait for response
    // ========================================================================
    echo "\nSending message...\n";

    $response = $session->sendAndWait(
        new MessageOptions(prompt: 'What is the weather in San Francisco?'),
        timeout: 120.0,
    );

    if ($response !== null) {
        echo "\nFinal response: " . ($response->data['content'] ?? '(no content)') . "\n";
    } else {
        echo "\nNo assistant message received.\n";
    }

    // ========================================================================
    // 8. Send another message
    // ========================================================================
    echo "\nSending follow-up message...\n";

    $response2 = $session->sendAndWait(
        new MessageOptions(prompt: 'Can you look up issue #42?'),
        timeout: 120.0,
    );

    if ($response2 !== null) {
        echo "\nFinal response: " . ($response2->data['content'] ?? '(no content)') . "\n";
    }

    // ========================================================================
    // 9. Get conversation history
    // ========================================================================
    $messages = $session->getMessages();
    echo "\nConversation history: " . count($messages) . " events\n";

    // ========================================================================
    // 10. Clean up
    // ========================================================================
    echo "\nDestroying session...\n";
    $session->destroy();

} catch (\Throwable $e) {
    echo "Error: {$e->getMessage()}\n";
    echo $e->getTraceAsString() . "\n";
} finally {
    echo "Stopping client...\n";
    $errors = $client->stop();
    if (!empty($errors)) {
        echo "Cleanup errors:\n";
        foreach ($errors as $error) {
            echo "  - {$error}\n";
        }
    }
    echo "Done.\n";
}

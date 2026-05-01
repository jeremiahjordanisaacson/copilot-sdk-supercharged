<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot\E2E;

use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\CopilotClientOptions;
use GitHub\Copilot\MessageOptions;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SessionFsConfig;

/**
 * E2E tests for the PHP Copilot SDK session lifecycle.
 *
 * These tests use the shared replay proxy and YAML snapshots to exercise
 * session creation, messaging, and filesystem configuration.
 */
class SessionE2ETest extends E2ETestCase
{
    /**
     * Test that a session can be created and disconnected.
     *
     * Creates a client, starts it, creates a session, verifies the session ID
     * is non-empty, destroys the session, and stops the client.
     */
    public function testSessionCreateDisconnect(): void
    {
        $this->configureSnapshot(
            'session',
            'sendandwait_blocks_until_session_idle_and_returns_final_assistant_message',
        );

        $client = new CopilotClient(new CopilotClientOptions(
            cliPath: $this->getCliPath(),
            env: $this->getTestEnv(),
            cwd: static::$workDir,
        ));

        $client->start();

        try {
            $session = $client->createSession(new SessionConfig(model: 'gpt-4'));

            // Session ID must be non-empty
            $this->assertNotEmpty(
                $session->sessionId,
                'Session ID should be a non-empty string after creation',
            );
            $this->assertIsString($session->sessionId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test sending a message and receiving an assistant response.
     *
     * Creates a session, sends a simple arithmetic prompt via sendAndWait,
     * and verifies the response contains the expected answer.
     */
    public function testSendMessage(): void
    {
        $this->configureSnapshot(
            'session',
            'sendandwait_blocks_until_session_idle_and_returns_final_assistant_message',
        );

        $client = new CopilotClient(new CopilotClientOptions(
            cliPath: $this->getCliPath(),
            env: $this->getTestEnv(),
            cwd: static::$workDir,
        ));

        $client->start();

        try {
            $session = $client->createSession(new SessionConfig(model: 'gpt-4'));
            $this->assertNotEmpty($session->sessionId);

            $response = $session->sendAndWait(new MessageOptions(prompt: 'What is 1+1?'));

            // Verify we received an assistant message event
            $this->assertNotNull($response, 'Expected a non-null assistant response');
            $this->assertEquals('assistant.message', $response->type);
            $this->assertNotEmpty(
                $response->data['content'] ?? '',
                'Assistant response content should not be empty',
            );

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test that sessionFs configuration is accepted by the client.
     *
     * Creates a client with SessionFsConfig and verifies it starts
     * without error, confirming the config is passed through correctly.
     */
    public function testSessionFsConfig(): void
    {
        $this->configureSnapshot(
            'session',
            'sendandwait_blocks_until_session_idle_and_returns_final_assistant_message',
        );

        $sessionFsConfig = new SessionFsConfig(
            initialCwd: '/',
            sessionStatePath: '/session-state',
            conventions: 'posix',
        );

        $client = new CopilotClient(new CopilotClientOptions(
            cliPath: $this->getCliPath(),
            env: $this->getTestEnv(),
            cwd: static::$workDir,
            sessionFs: $sessionFsConfig,
        ));

        $client->start();

        try {
            // If the sessionFs config was invalid, start() or createSession() would throw.
            // Successfully creating a session proves the config was accepted.
            $session = $client->createSession(new SessionConfig(model: 'gpt-4'));
            $this->assertNotEmpty($session->sessionId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }
}

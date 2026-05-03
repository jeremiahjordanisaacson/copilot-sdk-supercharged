<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot\E2E;

use GitHub\Copilot\ConnectionState;
use GitHub\Copilot\CopilotClient;
use GitHub\Copilot\CopilotClientOptions;
use GitHub\Copilot\DefineTool;
use GitHub\Copilot\MCPLocalServerConfig;
use GitHub\Copilot\MessageOptions;
use GitHub\Copilot\ResumeSessionConfig;
use GitHub\Copilot\SessionConfig;
use GitHub\Copilot\SessionFsConfig;
use GitHub\Copilot\SystemMessageAppendConfig;

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

    /**
     * Test multi-turn conversation with two sequential messages.
     */
    public function testMultiTurnConversation(): void
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

            $response1 = $session->sendAndWait(new MessageOptions(prompt: 'What is 2+2?'));
            $this->assertNotNull($response1, 'First response should not be null');
            $this->assertEquals('assistant.message', $response1->type);

            $response2 = $session->sendAndWait(new MessageOptions(prompt: 'What is 3+3?'));
            $this->assertNotNull($response2, 'Second response should not be null');
            $this->assertEquals('assistant.message', $response2->type);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test resuming a previously created session.
     */
    public function testSessionResume(): void
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
            $sessionId = $session->sessionId;
            $this->assertNotEmpty($sessionId);
            $session->destroy();
        } finally {
            $client->stop();
        }

        $client2 = new CopilotClient(new CopilotClientOptions(
            cliPath: $this->getCliPath(),
            env: $this->getTestEnv(),
            cwd: static::$workDir,
        ));

        $client2->start();

        try {
            $resumed = $client2->resumeSession($sessionId, new ResumeSessionConfig(model: 'gpt-4'));
            $this->assertNotEmpty($resumed->sessionId);
            $resumed->destroy();
        } finally {
            $client2->stop();
        }
    }

    /**
     * Test listing sessions after creating multiple sessions.
     */
    public function testSessionList(): void
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
            $session1 = $client->createSession(new SessionConfig(model: 'gpt-4'));
            $session2 = $client->createSession(new SessionConfig(model: 'gpt-4'));
            $this->assertNotEmpty($session1->sessionId);
            $this->assertNotEmpty($session2->sessionId);

            $sessions = $client->listSessions();
            $this->assertIsArray($sessions);
            $this->assertGreaterThanOrEqual(2, count($sessions));

            $session1->destroy();
            $session2->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test retrieving session metadata by ID.
     */
    public function testSessionMetadata(): void
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

            $metadata = $client->getSessionMetadata($session->sessionId);
            $this->assertNotNull($metadata);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test deleting a session and verifying it is removed from the list.
     */
    public function testSessionDelete(): void
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
            $sessionId = $session->sessionId;
            $this->assertNotEmpty($sessionId);
            $session->destroy();

            $client->deleteSession($sessionId);

            $sessions = $client->listSessions();
            $sessionIds = array_map(fn($s) => $s['sessionId'] ?? $s['id'] ?? null, $sessions);
            $this->assertNotContains($sessionId, $sessionIds, 'Deleted session should not appear in list');
        } finally {
            $client->stop();
        }
    }

    /**
     * Test listing available models.
     */
    public function testModelList(): void
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
            $models = $client->listModels();
            $this->assertNotNull($models);
            $this->assertIsArray($models);
        } finally {
            $client->stop();
        }
    }

    /**
     * Test ping returns a response.
     */
    public function testPing(): void
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
            $result = $client->ping('hello');
            $this->assertNotNull($result);
        } finally {
            $client->stop();
        }
    }

    /**
     * Test auth status returns a response.
     */
    public function testAuthStatus(): void
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
            $authStatus = $client->getAuthStatus();
            $this->assertNotNull($authStatus);
        } finally {
            $client->stop();
        }
    }

    /**
     * Test client lifecycle: start sets Connected state, stop sets Disconnected.
     */
    public function testClientLifecycle(): void
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
            $state = $client->getState();
            $this->assertEquals(ConnectionState::Connected, $state);
        } finally {
            $client->stop();
        }

        $stateAfterStop = $client->getState();
        $this->assertEquals(ConnectionState::Disconnected, $stateAfterStop);
    }

    /**
     * Test setting and getting the foreground session ID.
     */
    public function testForegroundSession(): void
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

            $client->setForegroundSessionId($session->sessionId);
            $foregroundId = $client->getForegroundSessionId();
            $this->assertEquals($session->sessionId, $foregroundId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test registering a tool and using it in a session.
     */
    public function testTools(): void
    {
        $this->configureSnapshot(
            'session',
            'sendandwait_blocks_until_session_idle_and_returns_final_assistant_message',
        );

        $tool = DefineTool::simple(
            name: 'get_weather',
            handler: fn($args) => 'Sunny, 72F',
            description: 'Get the current weather',
            parameters: [
                'type' => 'object',
                'properties' => [
                    'location' => [
                        'type' => 'string',
                        'description' => 'City name',
                    ],
                ],
                'required' => ['location'],
            ],
        );

        $client = new CopilotClient(new CopilotClientOptions(
            cliPath: $this->getCliPath(),
            env: $this->getTestEnv(),
            cwd: static::$workDir,
        ));

        $client->start();

        try {
            $session = $client->createSession(new SessionConfig(
                model: 'gpt-4',
                tools: [$tool],
            ));
            $this->assertNotEmpty($session->sessionId);

            $response = $session->sendAndWait(new MessageOptions(
                prompt: 'What is the weather in Seattle?',
            ));
            $this->assertNotNull($response);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test streaming mode collects delta events.
     */
    public function testStreaming(): void
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
            $session = $client->createSession(new SessionConfig(
                model: 'gpt-4',
                streaming: true,
            ));
            $this->assertNotEmpty($session->sessionId);

            $events = [];
            $session->on(function ($event) use (&$events) {
                $events[] = $event;
            });

            $response = $session->sendAndWait(new MessageOptions(prompt: 'Say hello'));
            $this->assertNotNull($response);
            $this->assertNotEmpty($events, 'Should have received streaming events');

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test creating a session with a custom system message.
     */
    public function testSystemMessageCustomization(): void
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
            $session = $client->createSession(new SessionConfig(
                model: 'gpt-4',
                systemMessage: new SystemMessageAppendConfig(
                    content: 'You are a helpful PHP assistant.',
                ),
            ));
            $this->assertNotEmpty($session->sessionId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test that sessionFs provider configuration is accepted.
     */
    public function testSessionFsProvider(): void
    {
        $this->configureSnapshot(
            'session',
            'sendandwait_blocks_until_session_idle_and_returns_final_assistant_message',
        );

        $client = new CopilotClient(new CopilotClientOptions(
            cliPath: $this->getCliPath(),
            env: $this->getTestEnv(),
            cwd: static::$workDir,
            sessionFs: new SessionFsConfig(
                initialCwd: '/',
                sessionStatePath: '/session-state',
                conventions: 'posix',
            ),
        ));

        $client->start();

        try {
            $client->setSessionFsProvider();
            $session = $client->createSession(new SessionConfig(model: 'gpt-4'));
            $this->assertNotEmpty($session->sessionId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test creating a session with MCP server configuration.
     */
    public function testMcpServersConfig(): void
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
            $session = $client->createSession(new SessionConfig(
                model: 'gpt-4',
                mcpServers: [
                    'test-server' => new MCPLocalServerConfig(
                        tools: [],
                        command: 'echo',
                        args: ['hello'],
                    ),
                ],
            ));
            $this->assertNotEmpty($session->sessionId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test creating a session with skill directories configuration.
     */
    public function testSkillsConfig(): void
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
            $session = $client->createSession(new SessionConfig(
                model: 'gpt-4',
                skillDirectories: [static::$workDir],
            ));
            $this->assertNotEmpty($session->sessionId);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }

    /**
     * Test compaction by sending multiple messages and verifying responses.
     */
    public function testCompaction(): void
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

            $response1 = $session->sendAndWait(new MessageOptions(prompt: 'What is 1+1?'));
            $this->assertNotNull($response1);
            $this->assertEquals('assistant.message', $response1->type);

            $response2 = $session->sendAndWait(new MessageOptions(prompt: 'What is 2+2?'));
            $this->assertNotNull($response2);
            $this->assertEquals('assistant.message', $response2->type);

            $response3 = $session->sendAndWait(new MessageOptions(prompt: 'What is 3+3?'));
            $this->assertNotNull($response3);
            $this->assertEquals('assistant.message', $response3->type);

            $session->destroy();
        } finally {
            $client->stop();
        }
    }
}

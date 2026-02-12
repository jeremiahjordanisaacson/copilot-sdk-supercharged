<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * Represents a single conversation session with the Copilot CLI.
 *
 * A session maintains conversation state, handles events, and manages tool execution.
 * Sessions are created via CopilotClient::createSession() or resumed via
 * CopilotClient::resumeSession().
 *
 * Usage:
 *
 *     $session = $client->createSession();
 *
 *     $session->on(function (SessionEvent $event) {
 *         if ($event->type === 'assistant.message') {
 *             echo $event->data['content'] ?? '';
 *         }
 *     });
 *
 *     $response = $session->sendAndWait(new MessageOptions(prompt: 'Hello!'));
 *     echo $response?->data['content'] ?? 'No response';
 *
 *     $session->destroy();
 */
class CopilotSession
{
    /** @var callable[] Wildcard event handlers */
    private array $eventHandlers = [];

    /** @var array<string, callable[]> Typed event handlers keyed by event type */
    private array $typedEventHandlers = [];

    /** @var array<string, callable> Tool handlers keyed by tool name */
    private array $toolHandlers = [];

    /** @var callable|null Permission request handler */
    private $permissionHandler = null;

    /** @var callable|null User input request handler */
    private $userInputHandler = null;

    /** @var SessionHooks|null Hook handlers */
    private ?SessionHooks $hooks = null;

    /**
     * Creates a new CopilotSession instance.
     *
     * @param string $sessionId The unique session identifier
     * @param JsonRpcClient $connection The JSON-RPC client connection
     * @param string|null $workspacePath Path to session workspace directory
     * @internal Use CopilotClient::createSession() to create sessions.
     */
    public function __construct(
        public readonly string $sessionId,
        private readonly JsonRpcClient $connection,
        private readonly ?string $workspacePath = null,
    ) {}

    /**
     * Path to the session workspace directory when infinite sessions are enabled.
     */
    public function getWorkspacePath(): ?string
    {
        return $this->workspacePath;
    }

    /**
     * Sends a message to this session.
     *
     * @param MessageOptions $options The message options
     * @return string The message ID
     * @throws \RuntimeException If sending fails
     */
    public function send(MessageOptions $options): string
    {
        $params = array_merge(
            ['sessionId' => $this->sessionId],
            $options->toArray(),
        );

        $response = $this->connection->request('session.send', $params);
        return $response['messageId'] ?? '';
    }

    /**
     * Sends a message and waits until the session becomes idle.
     *
     * Events are still delivered to handlers while waiting.
     *
     * @param MessageOptions $options The message options
     * @param float $timeout Timeout in seconds (default: 60)
     * @return SessionEvent|null The final assistant message event, or null
     * @throws \RuntimeException On timeout or error
     */
    public function sendAndWait(MessageOptions $options, float $timeout = 60.0): ?SessionEvent
    {
        $idle = false;
        $error = null;
        $lastAssistantMessage = null;

        // Register handler BEFORE sending to avoid race conditions
        $unsubscribe = $this->on(function (SessionEvent $event) use (&$idle, &$error, &$lastAssistantMessage) {
            if ($event->type === 'assistant.message') {
                $lastAssistantMessage = $event;
            } elseif ($event->type === 'session.idle') {
                $idle = true;
            } elseif ($event->type === 'session.error') {
                $error = new \RuntimeException($event->data['message'] ?? 'Session error');
            }
        });

        try {
            $this->send($options);

            $deadline = microtime(true) + $timeout;

            while (!$idle && $error === null) {
                $remaining = $deadline - microtime(true);
                if ($remaining <= 0) {
                    throw new \RuntimeException(
                        "Timeout after {$timeout}s waiting for session.idle"
                    );
                }

                // Process messages from the server, which triggers event dispatch
                $this->connection->processNextMessage(min($remaining, 0.1));
            }

            if ($error !== null) {
                throw $error;
            }

            return $lastAssistantMessage;
        } finally {
            $unsubscribe();
        }
    }

    /**
     * Subscribe to all events from this session.
     *
     * @param callable $handler fn(SessionEvent): void
     * @return callable Unsubscribe function
     */
    public function on(callable $handler): callable
    {
        $this->eventHandlers[] = $handler;
        return function () use ($handler) {
            $this->eventHandlers = array_values(array_filter(
                $this->eventHandlers,
                fn($h) => $h !== $handler,
            ));
        };
    }

    /**
     * Subscribe to a specific event type from this session.
     *
     * @param string $eventType e.g. "assistant.message", "session.idle"
     * @param callable $handler fn(SessionEvent): void
     * @return callable Unsubscribe function
     */
    public function onEvent(string $eventType, callable $handler): callable
    {
        if (!isset($this->typedEventHandlers[$eventType])) {
            $this->typedEventHandlers[$eventType] = [];
        }
        $this->typedEventHandlers[$eventType][] = $handler;

        return function () use ($eventType, $handler) {
            $this->typedEventHandlers[$eventType] = array_values(array_filter(
                $this->typedEventHandlers[$eventType] ?? [],
                fn($h) => $h !== $handler,
            ));
        };
    }

    /**
     * Dispatches an event to all registered handlers.
     *
     * @internal Called by CopilotClient when server sends session.event notification.
     */
    public function dispatchEvent(SessionEvent $event): void
    {
        // Dispatch to typed handlers
        foreach ($this->typedEventHandlers[$event->type] ?? [] as $handler) {
            try {
                $handler($event);
            } catch (\Throwable) {
                // Ignore handler errors
            }
        }

        // Dispatch to wildcard handlers
        foreach ($this->eventHandlers as $handler) {
            try {
                $handler($event);
            } catch (\Throwable) {
                // Ignore handler errors
            }
        }
    }

    /**
     * Registers custom tool handlers for this session.
     *
     * @param Tool[]|null $tools Tool definitions with handlers
     * @internal
     */
    public function registerTools(?array $tools): void
    {
        $this->toolHandlers = [];
        if ($tools === null) {
            return;
        }
        foreach ($tools as $tool) {
            if ($tool->handler !== null) {
                $this->toolHandlers[$tool->name] = $tool->handler;
            }
        }
    }

    /**
     * Retrieves a registered tool handler by name.
     *
     * @internal
     */
    public function getToolHandler(string $name): ?callable
    {
        return $this->toolHandlers[$name] ?? null;
    }

    /**
     * Registers a handler for permission requests.
     *
     * @param callable|null $handler fn(PermissionRequest, array{sessionId:string}): PermissionRequestResult
     * @internal
     */
    public function registerPermissionHandler(?callable $handler): void
    {
        $this->permissionHandler = $handler;
    }

    /**
     * Registers a handler for user input requests.
     *
     * @param callable|null $handler fn(UserInputRequest, array{sessionId:string}): UserInputResponse
     * @internal
     */
    public function registerUserInputHandler(?callable $handler): void
    {
        $this->userInputHandler = $handler;
    }

    /**
     * Registers hook handlers.
     *
     * @internal
     */
    public function registerHooks(?SessionHooks $hooks): void
    {
        $this->hooks = $hooks;
    }

    /**
     * Handle a permission request from the server.
     *
     * @internal
     * @return array The permission result array
     */
    public function handlePermissionRequestInternal(array $request): array
    {
        if ($this->permissionHandler === null) {
            return ['kind' => 'denied-no-approval-rule-and-could-not-request-from-user'];
        }

        $permRequest = PermissionRequest::fromArray($request);
        $result = ($this->permissionHandler)($permRequest, ['sessionId' => $this->sessionId]);

        if ($result instanceof PermissionRequestResult) {
            return $result->toArray();
        }

        return $result;
    }

    /**
     * Handle a user input request from the server.
     *
     * @internal
     */
    public function handleUserInputRequestInternal(UserInputRequest $request): UserInputResponse
    {
        if ($this->userInputHandler === null) {
            throw new \RuntimeException('User input requested but no handler registered');
        }

        $result = ($this->userInputHandler)($request, ['sessionId' => $this->sessionId]);

        if ($result instanceof UserInputResponse) {
            return $result;
        }

        throw new \RuntimeException('User input handler must return UserInputResponse');
    }

    /**
     * Handle a hooks invocation from the server.
     *
     * @internal
     * @return mixed The hook output or null
     */
    public function handleHooksInvokeInternal(string $hookType, mixed $input): mixed
    {
        if ($this->hooks === null) {
            return null;
        }

        $handlerMap = [
            'preToolUse' => $this->hooks->onPreToolUse,
            'postToolUse' => $this->hooks->onPostToolUse,
            'userPromptSubmitted' => $this->hooks->onUserPromptSubmitted,
            'sessionStart' => $this->hooks->onSessionStart,
            'sessionEnd' => $this->hooks->onSessionEnd,
            'errorOccurred' => $this->hooks->onErrorOccurred,
        ];

        $handler = $handlerMap[$hookType] ?? null;
        if ($handler === null) {
            return null;
        }

        try {
            $result = $handler($input, ['sessionId' => $this->sessionId]);
            if ($result === null) {
                return null;
            }
            // If result has a toArray method, use it
            if (is_object($result) && method_exists($result, 'toArray')) {
                return $result->toArray();
            }
            return $result;
        } catch (\Throwable) {
            return null;
        }
    }

    /**
     * Retrieves all events and messages from this session's history.
     *
     * @return SessionEvent[]
     * @throws \RuntimeException If the connection fails
     */
    public function getMessages(): array
    {
        $response = $this->connection->request('session.getMessages', [
            'sessionId' => $this->sessionId,
        ]);

        return array_map(
            fn(array $e) => SessionEvent::fromArray($e),
            $response['events'] ?? [],
        );
    }

    /**
     * Destroys this session and releases all associated resources.
     */
    public function destroy(): void
    {
        $this->connection->request('session.destroy', [
            'sessionId' => $this->sessionId,
        ]);
        $this->eventHandlers = [];
        $this->typedEventHandlers = [];
        $this->toolHandlers = [];
        $this->permissionHandler = null;
        $this->userInputHandler = null;
        $this->hooks = null;
    }

    /**
     * Aborts the currently processing message.
     */
    public function abort(): void
    {
        $this->connection->request('session.abort', [
            'sessionId' => $this->sessionId,
        ]);
    }
}

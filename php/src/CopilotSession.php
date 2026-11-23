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

    /** @var callable|null Exit plan mode handler */
    private $exitPlanModeHandler = null;

    /** @var callable|null Trace context provider */
    private $traceContextProvider = null;

    /** @var callable[] Callbacks run once after destroy() releases the session */
    private array $destroyCallbacks = [];

    /** @var callable|null Shell authentication handler */
    private $shellAuthenticationHandler = null;

    /** @var array<string, bool> Pending shell authentication request IDs (true once cancelled) */
    private array $pendingShellAuthentications = [];

    /** @var callable|null Memory store callback */
    private $memoryStoreHandler = null;

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

        if ($this->traceContextProvider !== null) {
            try {
                $tc = ($this->traceContextProvider)();
                if ($tc instanceof TraceContext) {
                    $tcArr = $tc->toArray();
                } elseif (is_array($tc)) {
                    $tcArr = $tc;
                } else {
                    $tcArr = [];
                }
                if (!empty($tcArr['traceparent'])) {
                    $params['traceparent'] = $tcArr['traceparent'];
                }
                if (!empty($tcArr['tracestate'])) {
                    $params['tracestate'] = $tcArr['tracestate'];
                }
            } catch (\Throwable) {
                // Ignore trace context errors
            }
        }

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
     * Sends a message requesting structured output and waits until the session is idle.
     *
     * Sets the message's responseSchema, waits via sendAndWait(), and returns the final
     * assistant content decoded as JSON (objects become associative arrays).
     *
     * @param MessageOptions|string $options The message options or prompt text
     * @param array<string,mixed>|object $schema JSON Schema the response must satisfy
     * @param float $timeout Timeout in seconds (default: 60)
     * @return mixed The decoded JSON value
     * @throws \RuntimeException If the run completes without assistant content
     * @throws \JsonException If the content is not valid JSON
     */
    public function sendAndWaitStructured(MessageOptions|string $options, array|object $schema, float $timeout = 60.0): mixed
    {
        if (is_string($options)) {
            $options = new MessageOptions(prompt: $options);
        }

        $structured = new MessageOptions(
            prompt: $options->prompt,
            attachments: $options->attachments,
            mode: $options->mode,
            responseFormat: $options->responseFormat,
            imageOptions: $options->imageOptions,
            requestHeaders: $options->requestHeaders,
            agentMode: $options->agentMode,
            displayPrompt: $options->displayPrompt,
            source: $options->source,
            responseSchema: $schema,
            clientTurnId: $options->clientTurnId,
        );

        $event = $this->sendAndWait($structured, $timeout);
        $content = $event?->data['content'] ?? null;
        if (!is_string($content) || trim($content) === '') {
            throw new \RuntimeException('The requested run completed without a structured assistant response');
        }

        return json_decode($content, true, 512, JSON_THROW_ON_ERROR);
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
     * Registers a handler for exit plan mode requests.
     * @internal
     */
    public function registerExitPlanModeHandler(?callable $handler): void
    {
        $this->exitPlanModeHandler = $handler;
    }

    /**
     * Handle an exit plan mode request from the server.
     * @internal
     */
    public function handleExitPlanModeRequestInternal(array $request): array
    {
        if ($this->exitPlanModeHandler === null) {
            return ['approved' => true];
        }

        try {
            $exitRequest = ExitPlanModeRequest::fromArray($request);
            $result = ($this->exitPlanModeHandler)($exitRequest);
            if ($result instanceof ExitPlanModeResponse) {
                return $result->toArray();
            }
            return $result;
        } catch (\Throwable) {
            return ['approved' => true];
        }
    }

    /**
     * Registers a trace context provider.
     * @internal
     */
    public function registerTraceContextProvider(?callable $provider): void
    {
        $this->traceContextProvider = $provider;
    }

    /**
     * Registers a callback run once after destroy() releases this session.
     * @internal
     */
    public function registerDestroyCallback(callable $callback): void
    {
        $this->destroyCallbacks[] = $callback;
    }

    /**
     * Registers the shell authentication handler.
     *
     * @param callable|null $handler fn(ShellAuthenticationRequest, array{sessionId:string}): ShellAuthenticationResponse|array|null
     * @internal
     */
    public function registerShellAuthenticationHandler(?callable $handler): void
    {
        $this->shellAuthenticationHandler = $handler;
    }

    /**
     * Asks the registered handler for a shell program's credentials. A failure, a
     * malformed answer, or a cancellation answers "cancel" so the program fails
     * instead of receiving a wrong value. The answer is never logged.
     *
     * @internal
     * @return array{action:string,value?:string}
     */
    public function handleShellAuthenticationRequestInternal(ShellAuthenticationRequest $request): array
    {
        $handler = $this->shellAuthenticationHandler;
        if ($handler === null || isset($this->pendingShellAuthentications[$request->requestId])) {
            return ['action' => 'cancel'];
        }

        $this->pendingShellAuthentications[$request->requestId] = false;
        try {
            $result = $handler($request, ['sessionId' => $this->sessionId]);
        } catch (\Throwable $e) {
            // The error text could contain the answer, so only its type is logged.
            fwrite(STDERR, '[CopilotSDK] Shell authentication handler failed (' . get_class($e) . ")\n");
            $result = null;
        } finally {
            $cancelled = $this->pendingShellAuthentications[$request->requestId] ?? true;
            unset($this->pendingShellAuthentications[$request->requestId]);
        }

        if ($cancelled) {
            return ['action' => 'cancel'];
        }
        if ($result instanceof ShellAuthenticationResponse) {
            $result = $result->toArray();
        }
        if (is_array($result) && ($result['action'] ?? null) === 'accept' && is_string($result['value'] ?? null)) {
            return ['action' => 'accept', 'value' => $result['value']];
        }
        return ['action' => 'cancel'];
    }

    /**
     * Cancels a pending shell authentication prompt; it then answers "cancel".
     * @internal
     */
    public function cancelShellAuthenticationRequestInternal(string $requestId): void
    {
        if (array_key_exists($requestId, $this->pendingShellAuthentications)) {
            $this->pendingShellAuthentications[$requestId] = true;
        }
    }

    /**
     * Registers the memory store callback.
     *
     * @param callable|null $handler fn(StoredMemory, array{sessionId:string}): void
     * @internal
     */
    public function registerMemoryStore(?callable $handler): void
    {
        $this->memoryStoreHandler = $handler;
    }

    /**
     * Hands a memory the agent asked to store to the registered callback.
     * @internal
     */
    public function handleMemoryStoreInternal(StoredMemory $memory): void
    {
        if ($this->memoryStoreHandler === null) {
            throw new \RuntimeException('No memory store is registered');
        }
        try {
            ($this->memoryStoreHandler)($memory, ['sessionId' => $this->sessionId]);
        } catch (\Throwable $e) {
            // The runtime reports a generic failure to the model; keep the cause visible here.
            fwrite(STDERR, "[CopilotSDK] Memory store callback failed: {$e->getMessage()}\n");
            throw new \RuntimeException('Memory store callback failed');
        }
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
            'userPromptTransformed' => $this->hooks->onUserPromptTransformed,
            'sessionStart' => $this->hooks->onSessionStart,
            'sessionEnd' => $this->hooks->onSessionEnd,
            'errorOccurred' => $this->hooks->onErrorOccurred,
            'subagentStart' => $this->hooks->onSubagentStart,
            'subagentStop' => $this->hooks->onSubagentStop,
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
     * Retrieves metadata for this session.
     *
     * @return array The session metadata
     * @throws \RuntimeException If the connection fails
     */
    public function getMetadata(): array
    {
        return $this->connection->request('session.getMetadata', [
            'sessionId' => $this->sessionId,
        ]);
    }

    /**
     * Changes the Auto routing preference without changing the selected model (experimental).
     *
     * The runtime records the request and commits it on a later turn that uses the "auto" model.
     *
     * @param string|null $autoTier "efficiency", "balance", "intelligence" or "fast" (see AutoTier);
     *        null returns to the provider's default Auto routing
     * @return array Result with status, effectiveAutoTier, pendingAutoTier, activatingAutoTier, supersededAutoTier
     */
    public function setAutoTier(?string $autoTier): array
    {
        $result = $this->connection->request('session.model.switchAutoTier', [
            'sessionId' => $this->sessionId,
            'autoTier' => $autoTier,
        ]);
        return is_array($result) ? $result : [];
    }

    /**
     * Retrieves the managed (enterprise policy) settings in effect for this session.
     *
     * @return array The managed settings
     */
    public function getManagedSettings(): array
    {
        $result = $this->connection->request('session.managedSettings.get', [
            'sessionId' => $this->sessionId,
        ]);
        return is_array($result) ? $result : [];
    }

    /**
     * Asks the runtime to re-run the managed-settings policy helper for this session.
     *
     * @return array The raw result
     */
    public function recheckPolicyHelper(): array
    {
        $result = $this->connection->request('session.managedSettings.recheckPolicyHelper', [
            'sessionId' => $this->sessionId,
        ]);
        return is_array($result) ? $result : [];
    }

    /**
     * Changes the session's permission mode.
     *
     * @param string $mode "manual", "assisted" or "allow-all" (see PermissionMode)
     * @return PermissionModeSetResult Whether the mode was applied, the authoritative mode
     *         afterwards, and the refusal reason when known ("managed_policy")
     */
    public function setPermissionMode(string $mode): PermissionModeSetResult
    {
        $result = $this->connection->request('session.permissions.setMode', [
            'sessionId' => $this->sessionId,
            'mode' => $mode,
        ]);
        return PermissionModeSetResult::fromArray(is_array($result) ? $result : []);
    }

    /**
     * Destroys this session and releases all associated resources.
     *
     * Sends `session.detach` (falling back to `session.destroy` on older CLIs), so
     * on-disk session state is preserved and the session can be resumed later.
     */
    public function destroy(): void
    {
        try {
            $this->connection->request('session.detach', [
                'sessionId' => $this->sessionId,
            ]);
        } catch (JsonRpcException $e) {
            if ($e->getCode() !== -32601) {
                throw $e;
            }
            $this->connection->request('session.destroy', [
                'sessionId' => $this->sessionId,
            ]);
        }
        $this->eventHandlers = [];
        $this->typedEventHandlers = [];
        $this->toolHandlers = [];
        $this->permissionHandler = null;
        $this->userInputHandler = null;
        $this->hooks = null;
        $this->exitPlanModeHandler = null;
        $this->traceContextProvider = null;
        $this->memoryStoreHandler = null;
        $this->shellAuthenticationHandler = null;
        foreach (array_keys($this->pendingShellAuthentications) as $requestId) {
            $this->pendingShellAuthentications[$requestId] = true;
        }

        $callbacks = $this->destroyCallbacks;
        $this->destroyCallbacks = [];
        foreach ($callbacks as $callback) {
            try {
                $callback();
            } catch (\Throwable) {
                // Ignore cleanup errors
            }
        }
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

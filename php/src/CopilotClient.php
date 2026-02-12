<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * Main client for interacting with the Copilot CLI.
 *
 * The CopilotClient manages the connection to the Copilot CLI server and provides
 * methods to create and manage conversation sessions. It can either spawn a CLI
 * server process or connect to an existing server.
 *
 * Usage:
 *
 *     $client = new CopilotClient();
 *     $client->start();
 *
 *     $session = $client->createSession(new SessionConfig(model: 'gpt-4'));
 *
 *     $response = $session->sendAndWait(new MessageOptions(prompt: 'Hello!'));
 *     echo $response?->data['content'] ?? 'No response';
 *
 *     $session->destroy();
 *     $client->stop();
 */
class CopilotClient
{
    private CopilotClientOptions $options;
    private ConnectionState $state = ConnectionState::Disconnected;
    private JsonRpcClient|null $connection = null;

    /** @var resource|null The CLI process handle */
    private $process = null;

    /** @var resource|null Stdin pipe to CLI process */
    private $stdinPipe = null;

    /** @var resource|null Stdout pipe from CLI process */
    private $stdoutPipe = null;

    /** @var resource|null Stderr pipe from CLI process */
    private $stderrPipe = null;

    /** @var resource|null TCP socket for TCP mode */
    private $socket = null;

    private ?int $actualPort = null;
    private string $actualHost = 'localhost';
    private bool $isExternalServer = false;

    /** @var array<string, CopilotSession> */
    private array $sessions = [];

    /** @var ModelInfo[]|null */
    private ?array $modelsCache = null;

    /** @var callable[] Session lifecycle handlers (wildcard) */
    private array $sessionLifecycleHandlers = [];

    /** @var array<string, callable[]> Typed session lifecycle handlers */
    private array $typedLifecycleHandlers = [];

    /**
     * Creates a new CopilotClient instance.
     *
     * @param CopilotClientOptions|null $options Configuration options
     * @throws \InvalidArgumentException If mutually exclusive options are provided
     */
    public function __construct(?CopilotClientOptions $options = null)
    {
        $this->options = $options ?? new CopilotClientOptions();

        // Validate mutually exclusive options
        if ($this->options->cliUrl !== null && ($this->options->useStdio || $this->options->cliPath !== null)) {
            throw new \InvalidArgumentException('cliUrl is mutually exclusive with useStdio and cliPath');
        }

        if ($this->options->cliUrl !== null && ($this->options->githubToken !== null || $this->options->useLoggedInUser !== null)) {
            throw new \InvalidArgumentException(
                'githubToken and useLoggedInUser cannot be used with cliUrl (external server manages its own auth)'
            );
        }

        // Parse cliUrl if provided
        if ($this->options->cliUrl !== null) {
            $parsed = $this->parseCliUrl($this->options->cliUrl);
            $this->actualHost = $parsed['host'];
            $this->actualPort = $parsed['port'];
            $this->isExternalServer = true;
        }
    }

    /**
     * Starts the CLI server and establishes a connection.
     *
     * @throws \RuntimeException If the server fails to start or connection fails
     */
    public function start(): void
    {
        if ($this->state === ConnectionState::Connected) {
            return;
        }

        $this->state = ConnectionState::Connecting;

        try {
            if (!$this->isExternalServer) {
                $this->startCliServer();
            }

            $this->connectToServer();
            $this->verifyProtocolVersion();

            $this->state = ConnectionState::Connected;
        } catch (\Throwable $e) {
            $this->state = ConnectionState::Error;
            throw $e;
        }
    }

    /**
     * Stops the CLI server and closes all active sessions.
     *
     * @return string[] Array of error messages encountered during cleanup
     */
    public function stop(): array
    {
        $errors = [];

        // Destroy all active sessions with retry logic
        foreach ($this->sessions as $sessionId => $session) {
            $lastError = null;
            for ($attempt = 1; $attempt <= 3; $attempt++) {
                try {
                    $session->destroy();
                    $lastError = null;
                    break;
                } catch (\Throwable $e) {
                    $lastError = $e->getMessage();
                    if ($attempt < 3) {
                        usleep(100_000 * (int) pow(2, $attempt - 1)); // Exponential backoff
                    }
                }
            }
            if ($lastError !== null) {
                $errors[] = "Failed to destroy session {$sessionId} after 3 attempts: {$lastError}";
            }
        }
        $this->sessions = [];

        // Close connection
        if ($this->connection !== null) {
            try {
                $this->connection->stop();
            } catch (\Throwable $e) {
                $errors[] = "Failed to stop connection: {$e->getMessage()}";
            }
            $this->connection = null;
        }

        // Clear models cache
        $this->modelsCache = null;

        // Close TCP socket
        if ($this->socket !== null) {
            try {
                @fclose($this->socket);
            } catch (\Throwable $e) {
                $errors[] = "Failed to close socket: {$e->getMessage()}";
            }
            $this->socket = null;
        }

        // Close process pipes and terminate
        $this->closeProcessPipes();

        if ($this->process !== null && !$this->isExternalServer) {
            try {
                $status = proc_get_status($this->process);
                if ($status['running']) {
                    proc_terminate($this->process);
                    // Wait briefly for graceful shutdown
                    usleep(100_000);
                    $status = proc_get_status($this->process);
                    if ($status['running']) {
                        proc_terminate($this->process, 9); // SIGKILL
                    }
                }
                proc_close($this->process);
            } catch (\Throwable $e) {
                $errors[] = "Failed to terminate CLI process: {$e->getMessage()}";
            }
            $this->process = null;
        }

        $this->state = ConnectionState::Disconnected;
        $this->actualPort = null;

        return $errors;
    }

    /**
     * Forcefully stops the CLI server without graceful cleanup.
     */
    public function forceStop(): void
    {
        $this->sessions = [];

        if ($this->connection !== null) {
            try {
                $this->connection->stop();
            } catch (\Throwable) {
                // Ignore
            }
            $this->connection = null;
        }

        $this->modelsCache = null;

        if ($this->socket !== null) {
            try {
                @fclose($this->socket);
            } catch (\Throwable) {
                // Ignore
            }
            $this->socket = null;
        }

        $this->closeProcessPipes();

        if ($this->process !== null && !$this->isExternalServer) {
            try {
                proc_terminate($this->process, 9); // SIGKILL
                proc_close($this->process);
            } catch (\Throwable) {
                // Ignore
            }
            $this->process = null;
        }

        $this->state = ConnectionState::Disconnected;
        $this->actualPort = null;
    }

    /**
     * Creates a new conversation session with the Copilot CLI.
     *
     * @param SessionConfig|null $config Session configuration
     * @return CopilotSession The created session
     * @throws \RuntimeException If not connected
     */
    public function createSession(?SessionConfig $config = null): CopilotSession
    {
        $config ??= new SessionConfig();

        if ($this->connection === null) {
            if ($this->options->autoStart) {
                $this->start();
            } else {
                throw new \RuntimeException('Client not connected. Call start() first.');
            }
        }

        $params = $config->toServerParams();
        $response = $this->connection->request('session.create', $params);

        $sessionId = $response['sessionId'] ?? '';
        $workspacePath = $response['workspacePath'] ?? null;

        $session = new CopilotSession($sessionId, $this->connection, $workspacePath);
        $session->registerTools($config->tools);
        if ($config->onPermissionRequest !== null) {
            $session->registerPermissionHandler($config->onPermissionRequest);
        }
        if ($config->onUserInputRequest !== null) {
            $session->registerUserInputHandler($config->onUserInputRequest);
        }
        if ($config->hooks !== null) {
            $session->registerHooks($config->hooks);
        }
        $this->sessions[$sessionId] = $session;

        return $session;
    }

    /**
     * Resumes an existing conversation session by its ID.
     *
     * @param string $sessionId The ID of the session to resume
     * @param ResumeSessionConfig|null $config Optional configuration
     * @return CopilotSession The resumed session
     * @throws \RuntimeException If not connected or session not found
     */
    public function resumeSession(string $sessionId, ?ResumeSessionConfig $config = null): CopilotSession
    {
        $config ??= new ResumeSessionConfig();

        if ($this->connection === null) {
            if ($this->options->autoStart) {
                $this->start();
            } else {
                throw new \RuntimeException('Client not connected. Call start() first.');
            }
        }

        $params = $config->toServerParams($sessionId);
        $response = $this->connection->request('session.resume', $params);

        $resumedSessionId = $response['sessionId'] ?? $sessionId;
        $workspacePath = $response['workspacePath'] ?? null;

        $session = new CopilotSession($resumedSessionId, $this->connection, $workspacePath);
        $session->registerTools($config->tools);
        if ($config->onPermissionRequest !== null) {
            $session->registerPermissionHandler($config->onPermissionRequest);
        }
        if ($config->onUserInputRequest !== null) {
            $session->registerUserInputHandler($config->onUserInputRequest);
        }
        if ($config->hooks !== null) {
            $session->registerHooks($config->hooks);
        }
        $this->sessions[$resumedSessionId] = $session;

        return $session;
    }

    /**
     * Gets the current connection state.
     */
    public function getState(): ConnectionState
    {
        return $this->state;
    }

    /**
     * Sends a ping to the server.
     *
     * @param string|null $message Optional message to include
     * @return PingResponse The ping response
     * @throws \RuntimeException If not connected
     */
    public function ping(?string $message = null): PingResponse
    {
        $this->ensureConnected();
        $params = [];
        if ($message !== null) {
            $params['message'] = $message;
        }
        $result = $this->connection->request('ping', $params);
        return PingResponse::fromArray($result);
    }

    /**
     * Get CLI status including version and protocol information.
     *
     * @return GetStatusResponse
     * @throws \RuntimeException If not connected
     */
    public function getStatus(): GetStatusResponse
    {
        $this->ensureConnected();
        $result = $this->connection->request('status.get', []);
        return GetStatusResponse::fromArray($result);
    }

    /**
     * Get current authentication status.
     *
     * @return GetAuthStatusResponse
     * @throws \RuntimeException If not connected
     */
    public function getAuthStatus(): GetAuthStatusResponse
    {
        $this->ensureConnected();
        $result = $this->connection->request('auth.getStatus', []);
        return GetAuthStatusResponse::fromArray($result);
    }

    /**
     * List available models with their metadata.
     * Results are cached after the first successful call.
     *
     * @return ModelInfo[]
     * @throws \RuntimeException If not connected
     */
    public function listModels(): array
    {
        $this->ensureConnected();

        if ($this->modelsCache !== null) {
            return $this->modelsCache;
        }

        $result = $this->connection->request('models.list', []);
        $models = array_map(
            fn(array $m) => ModelInfo::fromArray($m),
            $result['models'] ?? [],
        );

        $this->modelsCache = $models;
        return $models;
    }

    /**
     * Gets the ID of the most recently updated session.
     *
     * @return string|null The session ID, or null if no sessions exist
     * @throws \RuntimeException If not connected
     */
    public function getLastSessionId(): ?string
    {
        $this->ensureConnected();
        $response = $this->connection->request('session.getLastId', []);
        return $response['sessionId'] ?? null;
    }

    /**
     * Deletes a session and its data from disk.
     *
     * @param string $sessionId The session ID to delete
     * @throws \RuntimeException If deletion fails
     */
    public function deleteSession(string $sessionId): void
    {
        $this->ensureConnected();
        $response = $this->connection->request('session.delete', ['sessionId' => $sessionId]);

        $success = $response['success'] ?? false;
        if (!$success) {
            $error = $response['error'] ?? 'Unknown error';
            throw new \RuntimeException("Failed to delete session {$sessionId}: {$error}");
        }

        unset($this->sessions[$sessionId]);
    }

    /**
     * Lists all available sessions.
     *
     * @return SessionMetadata[]
     * @throws \RuntimeException If not connected
     */
    public function listSessions(): array
    {
        $this->ensureConnected();
        $response = $this->connection->request('session.list', []);
        return array_map(
            fn(array $s) => SessionMetadata::fromArray($s),
            $response['sessions'] ?? [],
        );
    }

    /**
     * Gets the foreground session ID (TUI+server mode).
     *
     * @return string|null The foreground session ID
     * @throws \RuntimeException If not connected
     */
    public function getForegroundSessionId(): ?string
    {
        $this->ensureConnected();
        $response = $this->connection->request('session.getForeground', []);
        return $response['sessionId'] ?? null;
    }

    /**
     * Sets the foreground session (TUI+server mode).
     *
     * @param string $sessionId The session ID to set as foreground
     * @throws \RuntimeException If the operation fails
     */
    public function setForegroundSessionId(string $sessionId): void
    {
        $this->ensureConnected();
        $response = $this->connection->request('session.setForeground', ['sessionId' => $sessionId]);
        $success = $response['success'] ?? false;
        if (!$success) {
            throw new \RuntimeException($response['error'] ?? 'Failed to set foreground session');
        }
    }

    /**
     * Subscribe to session lifecycle events (typed).
     *
     * @param string $eventType e.g. "session.created", "session.deleted"
     * @param callable $handler fn(SessionLifecycleEvent): void
     * @return callable Unsubscribe function
     */
    public function onLifecycleEvent(string $eventType, callable $handler): callable
    {
        if (!isset($this->typedLifecycleHandlers[$eventType])) {
            $this->typedLifecycleHandlers[$eventType] = [];
        }
        $this->typedLifecycleHandlers[$eventType][] = $handler;

        return function () use ($eventType, $handler) {
            $this->typedLifecycleHandlers[$eventType] = array_filter(
                $this->typedLifecycleHandlers[$eventType] ?? [],
                fn($h) => $h !== $handler,
            );
        };
    }

    /**
     * Subscribe to all session lifecycle events.
     *
     * @param callable $handler fn(SessionLifecycleEvent): void
     * @return callable Unsubscribe function
     */
    public function onLifecycle(callable $handler): callable
    {
        $this->sessionLifecycleHandlers[] = $handler;
        return function () use ($handler) {
            $this->sessionLifecycleHandlers = array_filter(
                $this->sessionLifecycleHandlers,
                fn($h) => $h !== $handler,
            );
        };
    }

    /**
     * Process messages from the server. Call this in a loop to keep
     * the connection alive and handle events.
     *
     * @param float $timeout Maximum time to process in seconds
     */
    public function processMessages(float $timeout = 0.1): void
    {
        if ($this->connection !== null) {
            $this->connection->processMessages($timeout);
        }
    }

    // ========================================================================
    // Private methods
    // ========================================================================

    private function ensureConnected(): void
    {
        if ($this->connection === null) {
            throw new \RuntimeException('Client not connected');
        }
    }

    /**
     * Parse CLI URL into host and port.
     *
     * @return array{host: string, port: int}
     */
    private function parseCliUrl(string $url): array
    {
        // Remove protocol
        $cleanUrl = preg_replace('#^https?://#', '', $url);

        // Just a port number
        if (preg_match('/^\d+$/', $cleanUrl)) {
            return ['host' => 'localhost', 'port' => (int) $cleanUrl];
        }

        $parts = explode(':', $cleanUrl);
        if (count($parts) !== 2) {
            throw new \InvalidArgumentException(
                "Invalid cliUrl format: {$url}. Expected \"host:port\", \"http://host:port\", or \"port\""
            );
        }

        $host = $parts[0] ?: 'localhost';
        $port = (int) $parts[1];

        if ($port <= 0 || $port > 65535) {
            throw new \InvalidArgumentException("Invalid port in cliUrl: {$url}");
        }

        return ['host' => $host, 'port' => $port];
    }

    /**
     * Verify the server's protocol version matches the SDK.
     */
    private function verifyProtocolVersion(): void
    {
        $expectedVersion = SdkProtocolVersion::get();
        $pingResult = $this->ping();
        $serverVersion = $pingResult->protocolVersion;

        if ($serverVersion === null) {
            throw new \RuntimeException(
                "SDK protocol version mismatch: SDK expects version {$expectedVersion}, "
                . "but server does not report a protocol version. Please update your server."
            );
        }

        if ($serverVersion !== $expectedVersion) {
            throw new \RuntimeException(
                "SDK protocol version mismatch: SDK expects version {$expectedVersion}, "
                . "but server reports version {$serverVersion}. Please update your SDK or server."
            );
        }
    }

    /**
     * Start the CLI server process.
     */
    private function startCliServer(): void
    {
        $cliPath = $this->options->cliPath ?? $this->findCliPath();

        if (!file_exists($cliPath)) {
            throw new \RuntimeException("Copilot CLI not found at {$cliPath}");
        }

        $args = $this->options->cliArgs ?? [];
        $args = array_merge($args, [
            '--headless',
            '--no-auto-update',
            '--log-level',
            $this->options->logLevel->value,
        ]);

        if ($this->options->useStdio) {
            $args[] = '--stdio';
        } elseif ($this->options->port !== null && $this->options->port > 0) {
            $args[] = '--port';
            $args[] = (string) $this->options->port;
        }

        // Auth flags
        if ($this->options->githubToken !== null) {
            $args[] = '--auth-token-env';
            $args[] = 'COPILOT_SDK_AUTH_TOKEN';
        }
        $useLoggedInUser = $this->options->useLoggedInUser ?? ($this->options->githubToken !== null ? false : true);
        if (!$useLoggedInUser) {
            $args[] = '--no-auto-login';
        }

        // Build command
        $command = escapeshellarg($cliPath);
        foreach ($args as $arg) {
            $command .= ' ' . escapeshellarg($arg);
        }

        // Environment
        $env = $this->options->env ?? null;
        if ($this->options->githubToken !== null) {
            if ($env === null) {
                $env = getenv();
                if (!is_array($env)) {
                    $env = [];
                }
            }
            $env['COPILOT_SDK_AUTH_TOKEN'] = $this->options->githubToken;
        }
        // Remove NODE_DEBUG to avoid polluting stdout
        if ($env !== null) {
            unset($env['NODE_DEBUG']);
        }

        $descriptors = [
            0 => ['pipe', 'r'], // stdin
            1 => ['pipe', 'w'], // stdout
            2 => ['pipe', 'w'], // stderr
        ];

        $cwd = $this->options->cwd ?? getcwd() ?: null;

        $this->process = proc_open($command, $descriptors, $pipes, $cwd, $env);
        if ($this->process === false) {
            $this->process = null;
            throw new \RuntimeException("Failed to start CLI server: {$command}");
        }

        $this->stdinPipe = $pipes[0];
        $this->stdoutPipe = $pipes[1];
        $this->stderrPipe = $pipes[2];

        // For TCP mode, we need to read the port announcement from stdout
        if (!$this->options->useStdio) {
            stream_set_blocking($this->stdoutPipe, false);
            $output = '';
            $deadline = microtime(true) + 10.0;

            while (microtime(true) < $deadline) {
                $chunk = @fread($this->stdoutPipe, 4096);
                if ($chunk !== false && $chunk !== '') {
                    $output .= $chunk;
                    if (preg_match('/listening on port (\d+)/i', $output, $matches)) {
                        $this->actualPort = (int) $matches[1];
                        break;
                    }
                }

                // Check if process is still running
                $status = proc_get_status($this->process);
                if (!$status['running']) {
                    throw new \RuntimeException("CLI server exited with code {$status['exitcode']}");
                }

                usleep(50_000); // 50ms
            }

            if ($this->actualPort === null) {
                throw new \RuntimeException('Timeout waiting for CLI server to announce port');
            }
        }
    }

    /**
     * Connect to the CLI server.
     */
    private function connectToServer(): void
    {
        if ($this->options->useStdio && !$this->isExternalServer) {
            $this->connectViaStdio();
        } else {
            $this->connectViaTcp();
        }
    }

    /**
     * Connect via stdio pipes.
     */
    private function connectViaStdio(): void
    {
        if ($this->stdoutPipe === null || $this->stdinPipe === null) {
            throw new \RuntimeException('CLI process pipes not available');
        }

        // Set stdout to non-blocking for the JSON-RPC client
        stream_set_blocking($this->stdoutPipe, true);

        $this->connection = new JsonRpcClient($this->stdoutPipe, $this->stdinPipe);
        $this->attachConnectionHandlers();
    }

    /**
     * Connect via TCP socket.
     */
    private function connectViaTcp(): void
    {
        if ($this->actualPort === null) {
            throw new \RuntimeException('Server port not available');
        }

        $address = "tcp://{$this->actualHost}:{$this->actualPort}";
        $errno = 0;
        $errstr = '';
        $this->socket = @stream_socket_client($address, $errno, $errstr, 10.0);

        if ($this->socket === false) {
            $this->socket = null;
            throw new \RuntimeException("Failed to connect to CLI server at {$address}: [{$errno}] {$errstr}");
        }

        $this->connection = new JsonRpcClient($this->socket, $this->socket);
        $this->attachConnectionHandlers();
    }

    /**
     * Attach notification and request handlers to the JSON-RPC connection.
     */
    private function attachConnectionHandlers(): void
    {
        if ($this->connection === null) {
            return;
        }

        // Handle notifications from the server
        $this->connection->setNotificationHandler(function (string $method, array $params): void {
            match ($method) {
                'session.event' => $this->handleSessionEventNotification($params),
                'session.lifecycle' => $this->handleSessionLifecycleNotification($params),
                default => null, // Ignore unknown notifications
            };
        });

        // Handle incoming requests from the server
        $this->connection->setRequestHandler('tool.call', function (array $params): array {
            return $this->handleToolCallRequest($params);
        });

        $this->connection->setRequestHandler('permission.request', function (array $params): array {
            return $this->handlePermissionRequest($params);
        });

        $this->connection->setRequestHandler('userInput.request', function (array $params): array {
            return $this->handleUserInputRequest($params);
        });

        $this->connection->setRequestHandler('hooks.invoke', function (array $params): array {
            return $this->handleHooksInvoke($params);
        });
    }

    private function handleSessionEventNotification(array $notification): void
    {
        $sessionId = $notification['sessionId'] ?? null;
        $event = $notification['event'] ?? null;

        if (!is_string($sessionId) || !is_array($event)) {
            return;
        }

        $session = $this->sessions[$sessionId] ?? null;
        if ($session !== null) {
            $session->dispatchEvent(SessionEvent::fromArray($event));
        }
    }

    private function handleSessionLifecycleNotification(array $notification): void
    {
        $type = $notification['type'] ?? null;
        $sessionId = $notification['sessionId'] ?? null;

        if (!is_string($type) || !is_string($sessionId)) {
            return;
        }

        $event = SessionLifecycleEvent::fromArray($notification);

        // Dispatch to typed handlers
        foreach ($this->typedLifecycleHandlers[$type] ?? [] as $handler) {
            try {
                $handler($event);
            } catch (\Throwable) {
                // Ignore handler errors
            }
        }

        // Dispatch to wildcard handlers
        foreach ($this->sessionLifecycleHandlers as $handler) {
            try {
                $handler($event);
            } catch (\Throwable) {
                // Ignore handler errors
            }
        }
    }

    private function handleToolCallRequest(array $params): array
    {
        $sessionId = $params['sessionId'] ?? null;
        $toolCallId = $params['toolCallId'] ?? null;
        $toolName = $params['toolName'] ?? null;
        $arguments = $params['arguments'] ?? null;

        if (!is_string($sessionId) || !is_string($toolCallId) || !is_string($toolName)) {
            throw new \RuntimeException('Invalid tool call payload');
        }

        $session = $this->sessions[$sessionId] ?? null;
        if ($session === null) {
            throw new \RuntimeException("Unknown session {$sessionId}");
        }

        $handler = $session->getToolHandler($toolName);
        if ($handler === null) {
            return [
                'result' => [
                    'textResultForLlm' => "Tool '{$toolName}' is not supported by this client instance.",
                    'resultType' => 'failure',
                    'error' => "tool '{$toolName}' not supported",
                    'toolTelemetry' => new \stdClass(),
                ],
            ];
        }

        try {
            $invocation = new ToolInvocation(
                sessionId: $sessionId,
                toolCallId: $toolCallId,
                toolName: $toolName,
                arguments: $arguments,
            );
            $result = $handler($arguments, $invocation);
            return ['result' => $this->normalizeToolResult($result)];
        } catch (\Throwable $e) {
            return [
                'result' => [
                    'textResultForLlm' => 'Invoking this tool produced an error. Detailed information is not available.',
                    'resultType' => 'failure',
                    'error' => $e->getMessage(),
                    'toolTelemetry' => new \stdClass(),
                ],
            ];
        }
    }

    private function handlePermissionRequest(array $params): array
    {
        $sessionId = $params['sessionId'] ?? null;
        $permissionRequest = $params['permissionRequest'] ?? null;

        if (!is_string($sessionId) || !is_array($permissionRequest)) {
            throw new \RuntimeException('Invalid permission request payload');
        }

        $session = $this->sessions[$sessionId] ?? null;
        if ($session === null) {
            throw new \RuntimeException("Session not found: {$sessionId}");
        }

        try {
            $result = $session->handlePermissionRequestInternal($permissionRequest);
            return ['result' => $result];
        } catch (\Throwable) {
            return [
                'result' => [
                    'kind' => 'denied-no-approval-rule-and-could-not-request-from-user',
                ],
            ];
        }
    }

    private function handleUserInputRequest(array $params): array
    {
        $sessionId = $params['sessionId'] ?? null;
        $question = $params['question'] ?? null;

        if (!is_string($sessionId) || !is_string($question)) {
            throw new \RuntimeException('Invalid user input request payload');
        }

        $session = $this->sessions[$sessionId] ?? null;
        if ($session === null) {
            throw new \RuntimeException("Session not found: {$sessionId}");
        }

        $request = new UserInputRequest(
            question: $question,
            choices: $params['choices'] ?? null,
            allowFreeform: $params['allowFreeform'] ?? null,
        );

        $result = $session->handleUserInputRequestInternal($request);
        return $result->toArray();
    }

    private function handleHooksInvoke(array $params): array
    {
        $sessionId = $params['sessionId'] ?? null;
        $hookType = $params['hookType'] ?? null;
        $input = $params['input'] ?? [];

        if (!is_string($sessionId) || !is_string($hookType)) {
            throw new \RuntimeException('Invalid hooks invoke payload');
        }

        $session = $this->sessions[$sessionId] ?? null;
        if ($session === null) {
            throw new \RuntimeException("Session not found: {$sessionId}");
        }

        $output = $session->handleHooksInvokeInternal($hookType, $input);
        return ['output' => $output];
    }

    /**
     * Normalize any tool result into a ToolResultObject array format.
     */
    private function normalizeToolResult(mixed $result): array
    {
        if ($result === null) {
            return [
                'textResultForLlm' => 'Tool returned no result',
                'resultType' => 'failure',
                'error' => 'tool returned no result',
                'toolTelemetry' => new \stdClass(),
            ];
        }

        // ToolResultObject passes through
        if ($result instanceof ToolResultObject) {
            return $result->toArray();
        }

        // Array with textResultForLlm and resultType duck-typing
        if (is_array($result) && isset($result['textResultForLlm'], $result['resultType'])) {
            return $result;
        }

        // Convert to string
        $textResult = is_string($result) ? $result : json_encode($result, JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE);

        return [
            'textResultForLlm' => $textResult,
            'resultType' => 'success',
            'toolTelemetry' => new \stdClass(),
        ];
    }

    /**
     * Attempt to find the CLI path.
     */
    private function findCliPath(): string
    {
        // Check common locations
        $candidates = [
            'copilot',
            'github-copilot',
        ];

        // Check PATH
        foreach ($candidates as $candidate) {
            $which = PHP_OS_FAMILY === 'Windows'
                ? @shell_exec("where {$candidate} 2>NUL")
                : @shell_exec("which {$candidate} 2>/dev/null");
            if ($which !== null && trim($which) !== '') {
                return trim(explode("\n", $which)[0]);
            }
        }

        return 'copilot'; // Default, may fail at spawn time
    }

    /**
     * Close process pipes.
     */
    private function closeProcessPipes(): void
    {
        if ($this->stdinPipe !== null) {
            @fclose($this->stdinPipe);
            $this->stdinPipe = null;
        }
        if ($this->stdoutPipe !== null) {
            @fclose($this->stdoutPipe);
            $this->stdoutPipe = null;
        }
        if ($this->stderrPipe !== null) {
            @fclose($this->stderrPipe);
            $this->stderrPipe = null;
        }
    }
}

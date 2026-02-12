<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * Minimal JSON-RPC 2.0 client for stdio and TCP transport.
 *
 * Uses Content-Length header framing (Language Server Protocol style).
 * Messages are framed as:
 *   Content-Length: <byte-count>\r\n
 *   \r\n
 *   <json-payload>
 *
 * Supports:
 * - Sending requests (with response) and notifications (fire-and-forget)
 * - Receiving responses, notifications, and incoming requests from the server
 * - Registering request handlers for server-initiated RPC calls
 */
class JsonRpcClient
{
    /** @var resource|null The input stream (stdout from process, or socket) */
    private $inputStream = null;

    /** @var resource|null The output stream (stdin to process, or socket) */
    private $outputStream = null;

    /** @var array<string, array{resolve: callable, reject: callable}> Pending request futures */
    private array $pendingRequests = [];

    /** @var callable|null Handler for incoming notifications: fn(string $method, array $params) */
    private $notificationHandler = null;

    /** @var array<string, callable> Handlers for incoming requests: fn(array $params): array */
    private array $requestHandlers = [];

    /** @var int Auto-incrementing request ID */
    private int $nextId = 1;

    /** @var bool Whether the client is running */
    private bool $running = false;

    /** @var string Read buffer for partial reads */
    private string $readBuffer = '';

    /**
     * Create a JSON-RPC client with given input/output streams.
     *
     * @param resource $inputStream Stream to read messages from
     * @param resource $outputStream Stream to write messages to
     */
    public function __construct($inputStream, $outputStream)
    {
        $this->inputStream = $inputStream;
        $this->outputStream = $outputStream;
        $this->running = true;
    }

    /**
     * Set handler for incoming notifications from the server.
     *
     * @param callable $handler fn(string $method, array $params): void
     */
    public function setNotificationHandler(callable $handler): void
    {
        $this->notificationHandler = $handler;
    }

    /**
     * Register a handler for incoming requests from the server.
     *
     * @param string $method The RPC method name
     * @param callable $handler fn(array $params): array
     */
    public function setRequestHandler(string $method, callable $handler): void
    {
        $this->requestHandlers[$method] = $handler;
    }

    /**
     * Send a JSON-RPC request and wait for the response.
     *
     * This method processes incoming messages while waiting, so notifications
     * and incoming requests are handled during the wait.
     *
     * @param string $method The RPC method name
     * @param array $params Parameters for the method
     * @param float $timeout Timeout in seconds (default: 60)
     * @return mixed The result from the response
     * @throws JsonRpcException On JSON-RPC error response
     * @throws \RuntimeException On timeout or connection error
     */
    public function request(string $method, array $params = [], float $timeout = 60.0): mixed
    {
        $id = (string) $this->nextId++;

        $message = [
            'jsonrpc' => '2.0',
            'id' => $id,
            'method' => $method,
            'params' => $params,
        ];

        $this->sendMessage($message);

        $result = null;
        $error = null;
        $resolved = false;

        $this->pendingRequests[$id] = [
            'resolve' => function (mixed $value) use (&$result, &$resolved) {
                $result = $value;
                $resolved = true;
            },
            'reject' => function (\Throwable $e) use (&$error, &$resolved) {
                $error = $e;
                $resolved = true;
            },
        ];

        $deadline = microtime(true) + $timeout;

        try {
            while (!$resolved && $this->running) {
                $remaining = $deadline - microtime(true);
                if ($remaining <= 0) {
                    throw new \RuntimeException(
                        "Timeout after {$timeout}s waiting for response to '{$method}' (id={$id})"
                    );
                }

                $this->processNextMessage(min($remaining, 0.1));
            }
        } finally {
            unset($this->pendingRequests[$id]);
        }

        if ($error !== null) {
            throw $error;
        }

        return $result;
    }

    /**
     * Send a JSON-RPC notification (no response expected).
     *
     * @param string $method The RPC method name
     * @param array $params Parameters for the method
     */
    public function notify(string $method, array $params = []): void
    {
        $message = [
            'jsonrpc' => '2.0',
            'method' => $method,
            'params' => $params,
        ];
        $this->sendMessage($message);
    }

    /**
     * Process incoming messages for a specified duration.
     *
     * Call this in a loop to keep handling server notifications and requests
     * when you are not waiting on a specific request.
     *
     * @param float $timeout Maximum time to wait for a message in seconds
     * @return bool True if a message was processed
     */
    public function processNextMessage(float $timeout = 0.1): bool
    {
        if (!$this->running || $this->inputStream === null) {
            return false;
        }

        $message = $this->readMessage($timeout);
        if ($message === null) {
            return false;
        }

        $this->handleMessage($message);
        return true;
    }

    /**
     * Process all available messages without blocking.
     *
     * @param float $maxTime Maximum total processing time in seconds
     */
    public function processMessages(float $maxTime = 1.0): void
    {
        $deadline = microtime(true) + $maxTime;
        while (microtime(true) < $deadline) {
            if (!$this->processNextMessage(0.01)) {
                break;
            }
        }
    }

    /**
     * Stop the client.
     */
    public function stop(): void
    {
        $this->running = false;

        // Reject all pending requests
        foreach ($this->pendingRequests as $id => $callbacks) {
            ($callbacks['reject'])(new \RuntimeException('Client stopped'));
        }
        $this->pendingRequests = [];
    }

    /**
     * Check if the client is running.
     */
    public function isRunning(): bool
    {
        return $this->running;
    }

    // ========================================================================
    // Private methods
    // ========================================================================

    /**
     * Send a JSON-RPC message with Content-Length header framing.
     */
    private function sendMessage(array $message): void
    {
        if ($this->outputStream === null) {
            throw new \RuntimeException('Output stream is not available');
        }

        $content = json_encode($message, JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE);
        if ($content === false) {
            throw new \RuntimeException('Failed to encode JSON-RPC message: ' . json_last_error_msg());
        }

        $contentBytes = strlen($content);
        $frame = "Content-Length: {$contentBytes}\r\n\r\n{$content}";

        $written = @fwrite($this->outputStream, $frame);
        if ($written === false) {
            throw new \RuntimeException('Failed to write to output stream');
        }
        @fflush($this->outputStream);
    }

    /**
     * Read a single JSON-RPC message with Content-Length header framing.
     *
     * @param float $timeout Maximum time to wait in seconds
     * @return array|null Parsed JSON message, or null if timeout/EOF
     */
    private function readMessage(float $timeout): ?array
    {
        if ($this->inputStream === null) {
            return null;
        }

        // Set stream to non-blocking for timeout support
        stream_set_blocking($this->inputStream, false);

        $deadline = microtime(true) + $timeout;

        // Read the Content-Length header
        $headerLine = $this->readLineWithTimeout($deadline);
        if ($headerLine === null) {
            return null;
        }

        $headerLine = trim($headerLine);
        if ($headerLine === '') {
            // Skip empty lines and try again (within timeout)
            if (microtime(true) < $deadline) {
                return $this->readMessage(max(0, $deadline - microtime(true)));
            }
            return null;
        }

        if (!str_starts_with($headerLine, 'Content-Length:')) {
            // Unexpected data, skip and try again
            if (microtime(true) < $deadline) {
                return $this->readMessage(max(0, $deadline - microtime(true)));
            }
            return null;
        }

        $contentLength = (int) trim(substr($headerLine, strlen('Content-Length:')));
        if ($contentLength <= 0) {
            return null;
        }

        // Read the empty line separator
        $this->readLineWithTimeout($deadline);

        // Read exact content bytes
        $content = $this->readExactBytes($contentLength, $deadline);
        if ($content === null) {
            return null;
        }

        $decoded = json_decode($content, true);
        if (!is_array($decoded)) {
            return null;
        }

        return $decoded;
    }

    /**
     * Read a line from the input stream with timeout.
     */
    private function readLineWithTimeout(float $deadline): ?string
    {
        $line = '';
        while (microtime(true) < $deadline) {
            // Check if there is data in the buffer first
            if ($this->readBuffer !== '') {
                $nlPos = strpos($this->readBuffer, "\n");
                if ($nlPos !== false) {
                    $line = substr($this->readBuffer, 0, $nlPos + 1);
                    $this->readBuffer = substr($this->readBuffer, $nlPos + 1);
                    return $line;
                }
                // No newline in buffer, pull it out and keep reading
                $line .= $this->readBuffer;
                $this->readBuffer = '';
            }

            $read = [$this->inputStream];
            $write = null;
            $except = null;
            $remaining = max(0, $deadline - microtime(true));
            $sec = (int) $remaining;
            $usec = (int) (($remaining - $sec) * 1000000);

            $ready = @stream_select($read, $write, $except, $sec, $usec);
            if ($ready === false || $ready === 0) {
                // Timeout or error
                if ($line !== '') {
                    return $line; // Return what we have
                }
                return null;
            }

            $chunk = @fread($this->inputStream, 4096);
            if ($chunk === false || $chunk === '') {
                if (feof($this->inputStream)) {
                    $this->running = false;
                    return null;
                }
                usleep(1000); // 1ms sleep to avoid busy-loop
                continue;
            }

            // Check for newline in chunk
            $nlPos = strpos($chunk, "\n");
            if ($nlPos !== false) {
                $line .= substr($chunk, 0, $nlPos + 1);
                $this->readBuffer .= substr($chunk, $nlPos + 1);
                return $line;
            }

            $line .= $chunk;
        }

        if ($line !== '') {
            return $line;
        }
        return null;
    }

    /**
     * Read exactly N bytes from the input stream with timeout.
     */
    private function readExactBytes(int $numBytes, float $deadline): ?string
    {
        $data = '';

        // Consume from read buffer first
        if ($this->readBuffer !== '') {
            $fromBuffer = min($numBytes, strlen($this->readBuffer));
            $data = substr($this->readBuffer, 0, $fromBuffer);
            $this->readBuffer = substr($this->readBuffer, $fromBuffer);
            $numBytes -= $fromBuffer;
        }

        while ($numBytes > 0 && microtime(true) < $deadline) {
            $read = [$this->inputStream];
            $write = null;
            $except = null;
            $remaining = max(0, $deadline - microtime(true));
            $sec = (int) $remaining;
            $usec = (int) (($remaining - $sec) * 1000000);

            $ready = @stream_select($read, $write, $except, $sec, $usec);
            if ($ready === false || $ready === 0) {
                return null;
            }

            $chunk = @fread($this->inputStream, min($numBytes, 65536));
            if ($chunk === false || $chunk === '') {
                if (feof($this->inputStream)) {
                    $this->running = false;
                    return null;
                }
                usleep(1000);
                continue;
            }

            $data .= $chunk;
            $numBytes -= strlen($chunk);
        }

        if ($numBytes > 0) {
            return null; // Timed out
        }

        return $data;
    }

    /**
     * Handle an incoming message (response, notification, or request).
     */
    private function handleMessage(array $message): void
    {
        $hasId = array_key_exists('id', $message);
        $hasMethod = array_key_exists('method', $message);

        // Response to our request
        if ($hasId && !$hasMethod) {
            $id = (string) $message['id'];
            $callbacks = $this->pendingRequests[$id] ?? null;
            if ($callbacks === null) {
                return;
            }

            if (isset($message['error'])) {
                $error = $message['error'];
                ($callbacks['reject'])(new JsonRpcException(
                    code: $error['code'] ?? -1,
                    rpcMessage: $error['message'] ?? 'Unknown error',
                    data: $error['data'] ?? null,
                ));
            } elseif (array_key_exists('result', $message)) {
                ($callbacks['resolve'])($message['result']);
            } else {
                ($callbacks['reject'])(new \RuntimeException('Invalid JSON-RPC response'));
            }
            return;
        }

        // Notification from server (no id)
        if ($hasMethod && !$hasId) {
            if ($this->notificationHandler !== null) {
                $method = $message['method'];
                $params = $message['params'] ?? [];
                try {
                    ($this->notificationHandler)($method, $params);
                } catch (\Throwable $e) {
                    // Ignore notification handler errors
                    fwrite(STDERR, "[CopilotSDK] Notification handler error: {$e->getMessage()}\n");
                }
            }
            return;
        }

        // Incoming request from server (has both id and method)
        if ($hasId && $hasMethod) {
            $this->handleIncomingRequest($message);
            return;
        }
    }

    /**
     * Handle an incoming request from the server.
     */
    private function handleIncomingRequest(array $message): void
    {
        $method = $message['method'];
        $id = $message['id'];
        $params = $message['params'] ?? [];

        $handler = $this->requestHandlers[$method] ?? null;
        if ($handler === null) {
            $this->sendErrorResponse($id, -32601, "Method not found: {$method}");
            return;
        }

        try {
            $result = $handler($params);
            if ($result === null) {
                $result = [];
            }
            $this->sendResponse($id, $result);
        } catch (JsonRpcException $e) {
            $this->sendErrorResponse($id, $e->getCode(), $e->getRpcMessage(), $e->getData());
        } catch (\Throwable $e) {
            $this->sendErrorResponse($id, -32603, $e->getMessage());
        }
    }

    /**
     * Send a success response.
     */
    private function sendResponse(string|int $requestId, mixed $result): void
    {
        $response = [
            'jsonrpc' => '2.0',
            'id' => $requestId,
            'result' => $result,
        ];
        $this->sendMessage($response);
    }

    /**
     * Send an error response.
     */
    private function sendErrorResponse(string|int $requestId, int $code, string $message, mixed $data = null): void
    {
        $error = [
            'code' => $code,
            'message' => $message,
        ];
        if ($data !== null) {
            $error['data'] = $data;
        }
        $response = [
            'jsonrpc' => '2.0',
            'id' => $requestId,
            'error' => $error,
        ];
        $this->sendMessage($response);
    }
}

// ============================================================================
// JSON-RPC Exception
// ============================================================================

class JsonRpcException extends \RuntimeException
{
    private string $rpcMessage;
    private mixed $data;

    public function __construct(int $code, string $rpcMessage, mixed $data = null)
    {
        $this->rpcMessage = $rpcMessage;
        $this->data = $data;
        parent::__construct("JSON-RPC Error {$code}: {$rpcMessage}", $code);
    }

    public function getRpcMessage(): string
    {
        return $this->rpcMessage;
    }

    public function getData(): mixed
    {
        return $this->data;
    }
}

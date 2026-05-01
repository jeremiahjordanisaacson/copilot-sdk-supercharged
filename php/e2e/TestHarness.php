<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot\E2E;

/**
 * Manages a replaying CAPI proxy for E2E tests.
 *
 * Spawns the shared test harness server from test/harness/server.ts and provides
 * methods to configure snapshot replay and stop the proxy.
 */
class TestHarness
{
    /** @var resource|null */
    private $process = null;

    /** @var resource|null */
    private $stdout = null;

    private ?string $proxyUrl = null;

    /**
     * Start the replay proxy server.
     *
     * @return string The proxy URL
     * @throws \RuntimeException If the proxy fails to start
     */
    public function start(): string
    {
        if ($this->proxyUrl !== null) {
            return $this->proxyUrl;
        }

        $serverPath = realpath(__DIR__ . '/../../test/harness/server.ts');
        if ($serverPath === false) {
            throw new \RuntimeException('Cannot find test/harness/server.ts from ' . __DIR__);
        }

        $serverDir = dirname($serverPath);
        $isWindows = PHP_OS_FAMILY === 'Windows';
        $npxCmd = $isWindows ? 'cmd /c npx' : 'npx';

        $descriptors = [
            0 => ['pipe', 'r'],  // stdin
            1 => ['pipe', 'w'],  // stdout
            2 => ['file', 'php://stderr', 'w'],  // stderr → parent stderr
        ];

        $this->process = proc_open(
            "$npxCmd tsx $serverPath",
            $descriptors,
            $pipes,
            $serverDir,
        );

        if (!is_resource($this->process)) {
            throw new \RuntimeException('Failed to spawn replay proxy process');
        }

        fclose($pipes[0]); // close stdin
        $this->stdout = $pipes[1];

        // Read until we get the "Listening:" line
        $line = fgets($this->stdout);
        if ($line === false) {
            $this->kill();
            throw new \RuntimeException('Failed to read proxy URL from stdout');
        }

        if (preg_match('/Listening:\s*(http:\/\/\S+)/', trim($line), $matches)) {
            $this->proxyUrl = $matches[1];
            return $this->proxyUrl;
        }

        $this->kill();
        throw new \RuntimeException("Unexpected proxy output: $line");
    }

    /**
     * Configure the proxy for a specific test snapshot.
     *
     * @param string $snapshotPath Absolute path to the YAML snapshot file
     * @param string $workDir Working directory for the test
     * @throws \RuntimeException If configuration fails
     */
    public function configure(string $snapshotPath, string $workDir): void
    {
        if ($this->proxyUrl === null) {
            throw new \RuntimeException('Proxy not started');
        }

        $payload = json_encode([
            'filePath' => $snapshotPath,
            'workDir' => $workDir,
        ]);

        $context = stream_context_create([
            'http' => [
                'method' => 'POST',
                'header' => "Content-Type: application/json\r\n",
                'content' => $payload,
                'timeout' => 10,
            ],
        ]);

        $result = @file_get_contents("{$this->proxyUrl}/config", false, $context);
        if ($result === false) {
            throw new \RuntimeException('Failed to configure replay proxy');
        }
    }

    /**
     * Stop the replay proxy server.
     */
    public function stop(): void
    {
        if ($this->proxyUrl !== null) {
            // Best-effort stop request
            $context = stream_context_create([
                'http' => [
                    'method' => 'POST',
                    'header' => "Content-Type: application/json\r\n",
                    'content' => '{}',
                    'timeout' => 5,
                ],
            ]);
            @file_get_contents("{$this->proxyUrl}/stop", false, $context);
        }

        $this->kill();
    }

    /**
     * Get the proxy URL.
     */
    public function getProxyUrl(): ?string
    {
        return $this->proxyUrl;
    }

    private function kill(): void
    {
        if ($this->stdout !== null) {
            @fclose($this->stdout);
            $this->stdout = null;
        }

        if ($this->process !== null) {
            @proc_terminate($this->process);
            @proc_close($this->process);
            $this->process = null;
        }

        $this->proxyUrl = null;
    }
}

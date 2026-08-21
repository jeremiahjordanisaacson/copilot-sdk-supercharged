<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot\E2E;

use PHPUnit\Framework\TestCase;

/**
 * Base test case for Copilot PHP SDK E2E tests.
 *
 * Manages the shared replay proxy lifecycle and provides helper methods
 * for configuring test snapshots and retrieving environment variables.
 */
abstract class E2ETestCase extends TestCase
{
    protected static TestHarness $harness;
    protected static string $proxyUrl;
    protected static string $repoRoot;
    protected static string $snapshotsDir;
    protected static string $workDir;

    public static function setUpBeforeClass(): void
    {
        parent::setUpBeforeClass();

        static::$repoRoot = realpath(__DIR__ . '/../..') ?: '';
        static::$snapshotsDir = static::$repoRoot . DIRECTORY_SEPARATOR . 'test'
            . DIRECTORY_SEPARATOR . 'snapshots';

        // Create an isolated work directory for tests
        static::$workDir = sys_get_temp_dir() . DIRECTORY_SEPARATOR
            . 'copilot-php-e2e-' . bin2hex(random_bytes(8));
        if (!is_dir(static::$workDir)) {
            mkdir(static::$workDir, 0755, true);
        }

        static::$harness = new TestHarness();
        static::$proxyUrl = static::$harness->start();
    }

    public static function tearDownAfterClass(): void
    {
        static::$harness->stop();

        // Clean up work directory
        if (is_dir(static::$workDir)) {
            static::removeDirectory(static::$workDir);
        }

        parent::tearDownAfterClass();
    }

    /**
     * Configure the proxy for a specific test snapshot.
     *
     * @param string $category Snapshot category (e.g., "session")
     * @param string $testName Snapshot name (e.g., "should_create_and_disconnect_sessions")
     */
    protected function configureSnapshot(string $category, string $testName): void
    {
        $snapshotPath = static::$snapshotsDir . DIRECTORY_SEPARATOR . $category
            . DIRECTORY_SEPARATOR . $testName . '.yaml';

        static::$harness->configure($snapshotPath, static::$workDir);
    }

    /**
     * Get environment variables for isolated testing.
     *
     * @return array<string, string>
     */
    protected function getTestEnv(): array
    {
        // Start from the full parent environment so the spawned CLI inherits
        // PATH (needed by the `#!/usr/bin/env node` shebang to locate node),
        // HOME, and other essentials. proc_open() with an explicit env array
        // does NOT inherit the parent environment, so omitting PATH here makes
        // the CLI fail to exec and the first stdin write breaks the pipe.
        $env = getenv();
        if (!is_array($env)) {
            $env = [];
        }

        $env['COPILOT_API_URL'] = static::$proxyUrl;
        $env['COPILOT_HOME'] = static::$workDir;
        $env['GH_CONFIG_DIR'] = static::$workDir;
        $env['XDG_CONFIG_HOME'] = static::$workDir;
        $env['XDG_STATE_HOME'] = static::$workDir;

        // Provide a fake token so the CLI authenticates against the replay proxy.
        $env['GH_TOKEN'] = $env['GH_TOKEN'] ?? 'fake-test-token';
        $env['GITHUB_TOKEN'] = $env['GITHUB_TOKEN'] ?? 'fake-test-token';

        // Route the CLI's HTTPS calls to GitHub hosts through the harness CONNECT
        // proxy so the auth/user-login fetch is served by the replay proxy instead
        // of hitting the real api.github.com (which returns "401 Bad credentials").
        // Loopback is exempted so CAPI calls to the replay proxy (127.0.0.1) go
        // direct rather than through the CONNECT proxy.
        $connectProxyUrl = static::$harness->getConnectProxyUrl();
        if ($connectProxyUrl !== null) {
            $env['HTTPS_PROXY'] = $connectProxyUrl;
            $env['https_proxy'] = $connectProxyUrl;
            $env['NO_PROXY'] = '127.0.0.1,localhost,::1';
            $env['no_proxy'] = '127.0.0.1,localhost,::1';
        }
        $caFilePath = static::$harness->getCaFilePath();
        if ($caFilePath !== null) {
            $env['NODE_EXTRA_CA_CERTS'] = $caFilePath;
            $env['SSL_CERT_FILE'] = $caFilePath;
        }

        return $env;
    }

    /**
     * Get the CLI path for tests.
     */
    protected function getCliPath(): string
    {
        $envPath = getenv('COPILOT_CLI_PATH');
        if ($envPath !== false && file_exists($envPath)) {
            return realpath($envPath) ?: $envPath;
        }

        $githubModules = static::$repoRoot . DIRECTORY_SEPARATOR . 'nodejs'
            . DIRECTORY_SEPARATOR . 'node_modules' . DIRECTORY_SEPARATOR . '@github';

        // As of CLI 1.0.64-1 the runnable index.js ships in a platform-specific
        // package (e.g. @github/copilot-linux-x64); prefer it when present.
        foreach (glob($githubModules . DIRECTORY_SEPARATOR . 'copilot-*' . DIRECTORY_SEPARATOR . 'index.js') ?: [] as $match) {
            if (strpos($match, 'language-server') === false && file_exists($match)) {
                return realpath($match) ?: $match;
            }
        }

        $nodeCliPath = $githubModules . DIRECTORY_SEPARATOR . 'copilot'
            . DIRECTORY_SEPARATOR . 'index.js';

        if (file_exists($nodeCliPath)) {
            return realpath($nodeCliPath) ?: $nodeCliPath;
        }

        return 'copilot';
    }

    private static function removeDirectory(string $dir): void
    {
        if (!is_dir($dir)) {
            return;
        }

        $items = new \RecursiveIteratorIterator(
            new \RecursiveDirectoryIterator($dir, \RecursiveDirectoryIterator::SKIP_DOTS),
            \RecursiveIteratorIterator::CHILD_FIRST,
        );

        foreach ($items as $item) {
            if ($item->isDir()) {
                @rmdir($item->getPathname());
            } else {
                @unlink($item->getPathname());
            }
        }

        @rmdir($dir);
    }
}

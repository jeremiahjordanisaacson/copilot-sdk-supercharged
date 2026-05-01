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
        return [
            'COPILOT_API_URL' => static::$proxyUrl,
            'COPILOT_HOME' => static::$workDir,
            'XDG_CONFIG_HOME' => static::$workDir,
            'XDG_STATE_HOME' => static::$workDir,
        ];
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

        $nodeCliPath = static::$repoRoot . DIRECTORY_SEPARATOR . 'nodejs'
            . DIRECTORY_SEPARATOR . 'node_modules' . DIRECTORY_SEPARATOR . '@github'
            . DIRECTORY_SEPARATOR . 'copilot' . DIRECTORY_SEPARATOR . 'index.js';

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

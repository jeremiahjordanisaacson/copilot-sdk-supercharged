/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

#include <string>

#ifdef _WIN32
#include <windows.h>
#endif

namespace e2e {

/// Manages a replaying CAPI proxy for E2E tests.
///
/// Spawns the shared test harness server (test/harness/server.ts) and provides
/// configuration helpers so SDK tests can run against recorded snapshot exchanges.
class TestHarness {
public:
    TestHarness();
    ~TestHarness();

    // Non-copyable
    TestHarness(const TestHarness&) = delete;
    TestHarness& operator=(const TestHarness&) = delete;

    /// Start the replay proxy server.
    /// Returns true on success (proxy URL available via getProxyUrl()).
    bool start();

    /// Configure the proxy for a specific test snapshot.
    /// @param snapshotPath  Absolute path to the snapshot YAML file.
    /// @param workDir       Working directory for the test.
    bool configure(const std::string& snapshotPath, const std::string& workDir);

    /// Gracefully stop the proxy server.
    void stop();

    /// Returns the proxy URL (e.g. "http://127.0.0.1:12345").
    /// Empty if the harness hasn't been started.
    const std::string& getProxyUrl() const { return proxyUrl_; }

    /// Returns the absolute path to the repository root.
    const std::string& getRepoRoot() const { return repoRoot_; }

    /// Build a snapshot path relative to the repo root.
    std::string snapshotPath(const std::string& relativePath) const;

private:
    std::string proxyUrl_;
    std::string repoRoot_;

#ifdef _WIN32
    HANDLE processHandle_ = nullptr;
    HANDLE stdoutRead_ = nullptr;
#else
    pid_t pid_ = -1;
    int stdoutFd_ = -1;
#endif
};

} // namespace e2e

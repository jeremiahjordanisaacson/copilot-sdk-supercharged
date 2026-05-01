/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#include "test_harness.h"

#include <array>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <regex>
#include <sstream>
#include <stdexcept>

#ifdef _WIN32
#include <windows.h>
#else
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

namespace e2e {

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Compute the repository root from the e2e directory.
/// This file lives at <repo>/cpp/e2e/test_harness.cpp so repo root is ../../.
static std::string computeRepoRoot() {
    // Use the REPO_ROOT env var if set (e.g. from CMake), otherwise derive from __FILE__
    const char* envRoot = std::getenv("COPILOT_REPO_ROOT");
    if (envRoot && envRoot[0] != '\0') {
        return std::string(envRoot);
    }

    // Fallback: derive from __FILE__ at compile time
    std::string thisFile(__FILE__);
#ifdef _WIN32
    char sep = '\\';
#else
    char sep = '/';
#endif
    // Go up two directories: e2e/ -> cpp/ -> repo root
    auto pos = thisFile.rfind(sep);
    if (pos != std::string::npos) pos = thisFile.rfind(sep, pos - 1);
    if (pos != std::string::npos) pos = thisFile.rfind(sep, pos - 1);
    if (pos != std::string::npos) {
        return thisFile.substr(0, pos);
    }

    // Last resort: assume CWD is repo root
    return ".";
}

/// Execute a shell command and return its exit code.
static int shellExec(const std::string& cmd) {
    return std::system(cmd.c_str());
}

/// HTTP POST via curl (simplest cross-platform approach for test utilities).
static bool httpPost(const std::string& url, const std::string& jsonBody) {
    std::ostringstream cmd;
#ifdef _WIN32
    // Escape double quotes for Windows cmd
    std::string escaped = jsonBody;
    for (size_t i = 0; i < escaped.size(); ++i) {
        if (escaped[i] == '"') {
            escaped.insert(i, "\\");
            ++i;
        }
    }
    cmd << "curl -s -X POST -H \"Content-Type: application/json\" -d \"" << escaped << "\" \"" << url << "\" > nul 2>&1";
#else
    cmd << "curl -s -X POST -H 'Content-Type: application/json' -d '" << jsonBody << "' '" << url << "' > /dev/null 2>&1";
#endif
    return shellExec(cmd.str()) == 0;
}

// ---------------------------------------------------------------------------
// TestHarness implementation
// ---------------------------------------------------------------------------

TestHarness::TestHarness() : repoRoot_(computeRepoRoot()) {}

TestHarness::~TestHarness() {
    stop();
}

bool TestHarness::start() {
    if (!proxyUrl_.empty()) {
        return true; // Already started
    }

    std::string serverPath = repoRoot_;
#ifdef _WIN32
    serverPath += "\\test\\harness\\server.ts";
#else
    serverPath += "/test/harness/server.ts";
#endif

    std::string harnessDir = repoRoot_;
#ifdef _WIN32
    harnessDir += "\\test\\harness";
#else
    harnessDir += "/test/harness";
#endif

#ifdef _WIN32
    // Windows: use CreateProcess with a pipe for stdout
    SECURITY_ATTRIBUTES sa{};
    sa.nLength = sizeof(sa);
    sa.bInheritHandle = TRUE;

    HANDLE hReadPipe = nullptr;
    HANDLE hWritePipe = nullptr;
    if (!CreatePipe(&hReadPipe, &hWritePipe, &sa, 0)) {
        std::cerr << "TestHarness: CreatePipe failed\n";
        return false;
    }
    SetHandleInformation(hReadPipe, HANDLE_FLAG_INHERIT, 0);

    STARTUPINFOA si{};
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdOutput = hWritePipe;
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);

    PROCESS_INFORMATION pi{};
    std::string cmdLine = "npx tsx \"" + serverPath + "\"";

    if (!CreateProcessA(nullptr, const_cast<char*>(cmdLine.c_str()), nullptr, nullptr,
                        TRUE, 0, nullptr, harnessDir.c_str(), &si, &pi)) {
        std::cerr << "TestHarness: CreateProcess failed\n";
        CloseHandle(hReadPipe);
        CloseHandle(hWritePipe);
        return false;
    }
    CloseHandle(pi.hThread);
    CloseHandle(hWritePipe);

    processHandle_ = pi.hProcess;
    stdoutRead_ = hReadPipe;

    // Read the first line from stdout
    std::string line;
    char ch;
    DWORD bytesRead;
    while (ReadFile(hReadPipe, &ch, 1, &bytesRead, nullptr) && bytesRead == 1) {
        if (ch == '\n') break;
        if (ch != '\r') line += ch;
    }
#else
    // Unix: use pipe + fork+exec
    int pipefd[2];
    if (pipe(pipefd) != 0) {
        std::cerr << "TestHarness: pipe failed\n";
        return false;
    }

    pid_t childPid = fork();
    if (childPid < 0) {
        std::cerr << "TestHarness: fork failed\n";
        close(pipefd[0]);
        close(pipefd[1]);
        return false;
    }

    if (childPid == 0) {
        // Child: redirect stdout to pipe
        close(pipefd[0]);
        dup2(pipefd[1], STDOUT_FILENO);
        close(pipefd[1]);

        if (chdir(harnessDir.c_str()) != 0) {
            _exit(1);
        }

        execlp("npx", "npx", "tsx", serverPath.c_str(), nullptr);
        _exit(1);
    }

    // Parent
    close(pipefd[1]);
    pid_ = childPid;
    stdoutFd_ = pipefd[0];

    // Read the first line
    std::string line;
    char ch;
    while (read(pipefd[0], &ch, 1) == 1) {
        if (ch == '\n') break;
        if (ch != '\r') line += ch;
    }
#endif

    // Parse "Listening: http://..."
    std::regex re(R"(Listening:\s*(http://[^\s]+))");
    std::smatch match;
    if (!std::regex_search(line, match, re)) {
        std::cerr << "TestHarness: unexpected output: " << line << "\n";
        stop();
        return false;
    }

    proxyUrl_ = match[1].str();
    std::cout << "TestHarness: proxy started at " << proxyUrl_ << "\n";
    return true;
}

bool TestHarness::configure(const std::string& snapshotPath, const std::string& workDir) {
    if (proxyUrl_.empty()) return false;

    std::ostringstream json;
    json << "{\"filePath\":\"" << snapshotPath << "\",\"workDir\":\"" << workDir << "\"}";

    return httpPost(proxyUrl_ + "/config", json.str());
}

void TestHarness::stop() {
    if (proxyUrl_.empty()) return;

    // Best-effort graceful shutdown
    httpPost(proxyUrl_ + "/stop?skipWritingCache=true", "{}");

#ifdef _WIN32
    if (processHandle_) {
        WaitForSingleObject(processHandle_, 5000);
        TerminateProcess(processHandle_, 1);
        CloseHandle(processHandle_);
        processHandle_ = nullptr;
    }
    if (stdoutRead_) {
        CloseHandle(stdoutRead_);
        stdoutRead_ = nullptr;
    }
#else
    if (pid_ > 0) {
        int status;
        // Give the process a moment to exit gracefully
        for (int i = 0; i < 50; ++i) {
            if (waitpid(pid_, &status, WNOHANG) != 0) break;
            usleep(100000); // 100ms
        }
        kill(pid_, SIGTERM);
        waitpid(pid_, &status, 0);
        pid_ = -1;
    }
    if (stdoutFd_ >= 0) {
        close(stdoutFd_);
        stdoutFd_ = -1;
    }
#endif

    proxyUrl_.clear();
}

std::string TestHarness::snapshotPath(const std::string& relativePath) const {
    std::string path = repoRoot_;
#ifdef _WIN32
    path += "\\";
#else
    path += "/";
#endif
    path += relativePath;
    return path;
}

} // namespace e2e

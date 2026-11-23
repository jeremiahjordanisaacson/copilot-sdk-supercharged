/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#include "test_harness.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dirent.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

/* -------------------------------------------------------------------------- */
/* Helpers                                                                    */
/* -------------------------------------------------------------------------- */

/// Compute the repository root from the build environment.
static void compute_repo_root(char *out, int out_size) {
    const char *env_root = getenv("COPILOT_REPO_ROOT");
    if (env_root && env_root[0] != '\0') {
        snprintf(out, (size_t)out_size, "%s", env_root);
        return;
    }

    /* Derive from __FILE__: <repo>/c/e2e/test_harness.c -> <repo> */
    const char *src = __FILE__;
    int len = (int)strlen(src);

#ifdef _WIN32
    char sep = '\\';
#else
    char sep = '/';
#endif

    int slashes = 0;
    int pos = len - 1;
    while (pos >= 0 && slashes < 3) {
        if (src[pos] == sep || src[pos] == '/' || src[pos] == '\\') {
            slashes++;
            if (slashes == 3) break;
        }
        pos--;
    }

    if (slashes == 3 && pos >= 0) {
        int copy_len = pos < out_size - 1 ? pos : out_size - 1;
        memcpy(out, src, (size_t)copy_len);
        out[copy_len] = '\0';
    } else {
        snprintf(out, (size_t)out_size, ".");
    }
}

/// HTTP POST via curl (simplest cross-platform approach for test utilities).
static int http_post(const char *url, const char *json_body) {
    char cmd[2048];
#ifdef _WIN32
    snprintf(cmd, sizeof(cmd),
             "curl -s -X POST -H \"Content-Type: application/json\" "
             "-d \"%s\" \"%s\" > nul 2>&1",
             json_body, url);
#else
    snprintf(cmd, sizeof(cmd),
             "curl -s -X POST -H 'Content-Type: application/json' "
             "-d '%s' '%s' > /dev/null 2>&1",
             json_body, url);
#endif
    return system(cmd);
}

/* -------------------------------------------------------------------------- */
/* Public API                                                                 */
/* -------------------------------------------------------------------------- */

void test_harness_init(test_harness_t *harness) {
    memset(harness, 0, sizeof(*harness));
#ifndef _WIN32
    harness->pid = -1;
    harness->stdout_fd = -1;
#endif
    compute_repo_root(harness->repo_root, (int)sizeof(harness->repo_root));
}

int test_harness_start(test_harness_t *harness) {
    if (harness->proxy_url[0] != '\0') {
        return 0; /* Already started */
    }

    char server_path[1024];
    char harness_dir[1024];
#ifdef _WIN32
    snprintf(server_path, sizeof(server_path), "%s\\test\\harness\\server.ts", harness->repo_root);
    snprintf(harness_dir, sizeof(harness_dir), "%s\\test\\harness", harness->repo_root);
#else
    snprintf(server_path, sizeof(server_path), "%s/test/harness/server.ts", harness->repo_root);
    snprintf(harness_dir, sizeof(harness_dir), "%s/test/harness", harness->repo_root);
#endif

    char line[512];
    memset(line, 0, sizeof(line));

#ifdef _WIN32
    SECURITY_ATTRIBUTES sa;
    memset(&sa, 0, sizeof(sa));
    sa.nLength = sizeof(sa);
    sa.bInheritHandle = TRUE;

    HANDLE hReadPipe = NULL;
    HANDLE hWritePipe = NULL;
    if (!CreatePipe(&hReadPipe, &hWritePipe, &sa, 0)) {
        fprintf(stderr, "test_harness: CreatePipe failed\n");
        return -1;
    }
    SetHandleInformation(hReadPipe, HANDLE_FLAG_INHERIT, 0);

    STARTUPINFOA si;
    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdOutput = hWritePipe;
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);

    PROCESS_INFORMATION pi;
    memset(&pi, 0, sizeof(pi));

    char cmd_line[2048];
    snprintf(cmd_line, sizeof(cmd_line), "npx tsx \"%s\"", server_path);

    if (!CreateProcessA(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, harness_dir, &si, &pi)) {
        fprintf(stderr, "test_harness: CreateProcess failed\n");
        CloseHandle(hReadPipe);
        CloseHandle(hWritePipe);
        return -1;
    }
    CloseHandle(pi.hThread);
    CloseHandle(hWritePipe);

    harness->process_handle = pi.hProcess;
    harness->stdout_read = hReadPipe;

    /* Read the first line */
    {
        int idx = 0;
        char ch;
        DWORD bytes_read;
        while (ReadFile(hReadPipe, &ch, 1, &bytes_read, NULL) && bytes_read == 1) {
            if (ch == '\n') break;
            if (ch != '\r' && idx < (int)sizeof(line) - 1) {
                line[idx++] = ch;
            }
        }
        line[idx] = '\0';
    }
#else
    int pipefd[2];
    if (pipe(pipefd) != 0) {
        fprintf(stderr, "test_harness: pipe failed\n");
        return -1;
    }

    pid_t child = fork();
    if (child < 0) {
        fprintf(stderr, "test_harness: fork failed\n");
        close(pipefd[0]);
        close(pipefd[1]);
        return -1;
    }

    if (child == 0) {
        /* Child: redirect stdout to pipe */
        close(pipefd[0]);
        dup2(pipefd[1], STDOUT_FILENO);
        close(pipefd[1]);

        if (chdir(harness_dir) != 0) _exit(1);
        execlp("npx", "npx", "tsx", server_path, (char *)NULL);
        _exit(1);
    }

    /* Parent */
    close(pipefd[1]);
    harness->pid = child;
    harness->stdout_fd = pipefd[0];

    /* Read the first line */
    {
        int idx = 0;
        char ch;
        while (read(pipefd[0], &ch, 1) == 1) {
            if (ch == '\n') break;
            if (ch != '\r' && idx < (int)sizeof(line) - 1) {
                line[idx++] = ch;
            }
        }
        line[idx] = '\0';
    }
#endif

    /* Parse "Listening: http://..." */
    const char *prefix = "Listening: ";
    char *found = strstr(line, prefix);
    if (!found) {
        fprintf(stderr, "test_harness: unexpected output: %s\n", line);
        test_harness_stop(harness);
        return -1;
    }

    const char *url_start = found + strlen(prefix);
    snprintf(harness->proxy_url, sizeof(harness->proxy_url), "%s", url_start);

    /* Trim trailing whitespace */
    int url_len = (int)strlen(harness->proxy_url);
    while (url_len > 0 && (harness->proxy_url[url_len - 1] == ' ' ||
                           harness->proxy_url[url_len - 1] == '\r' ||
                           harness->proxy_url[url_len - 1] == '\n')) {
        harness->proxy_url[--url_len] = '\0';
    }

    printf("test_harness: proxy started at %s\n", harness->proxy_url);
    return 0;
}

int test_harness_configure(test_harness_t *harness,
                           const char *snapshot_path,
                           const char *work_dir) {
    if (harness->proxy_url[0] == '\0') return -1;

    char url[1024];
    snprintf(url, sizeof(url), "%s/config", harness->proxy_url);

    char json_body[2048];
    snprintf(json_body, sizeof(json_body),
             "{\\\"filePath\\\":\\\"%s\\\",\\\"workDir\\\":\\\"%s\\\"}",
             snapshot_path, work_dir);

    /* Use a simpler JSON for non-Windows */
#ifndef _WIN32
    snprintf(json_body, sizeof(json_body),
             "{\"filePath\":\"%s\",\"workDir\":\"%s\"}",
             snapshot_path, work_dir);
#endif

    return http_post(url, json_body);
}

void test_harness_stop(test_harness_t *harness) {
    if (harness->proxy_url[0] == '\0') return;

    /* Best-effort graceful shutdown */
    {
        char url[1024];
        snprintf(url, sizeof(url), "%s/stop?skipWritingCache=true", harness->proxy_url);
        http_post(url, "{}");
    }

#ifdef _WIN32
    if (harness->process_handle) {
        WaitForSingleObject(harness->process_handle, 5000);
        TerminateProcess(harness->process_handle, 1);
        CloseHandle(harness->process_handle);
        harness->process_handle = NULL;
    }
    if (harness->stdout_read) {
        CloseHandle(harness->stdout_read);
        harness->stdout_read = NULL;
    }
#else
    if (harness->pid > 0) {
        int status;
        int i;
        for (i = 0; i < 50; ++i) {
            if (waitpid(harness->pid, &status, WNOHANG) != 0) break;
            usleep(100000);
        }
        kill(harness->pid, SIGTERM);
        waitpid(harness->pid, &status, 0);
        harness->pid = -1;
    }
    if (harness->stdout_fd >= 0) {
        close(harness->stdout_fd);
        harness->stdout_fd = -1;
    }
#endif

    harness->proxy_url[0] = '\0';
}

const char *test_harness_get_url(const test_harness_t *harness) {
    return harness->proxy_url;
}

const char *test_harness_get_repo_root(const test_harness_t *harness) {
    return harness->repo_root;
}

int test_harness_snapshot_path(const test_harness_t *harness,
                               const char *relative_path,
                               char *out_path,
                               int out_size) {
#ifdef _WIN32
    return snprintf(out_path, (size_t)out_size, "%s\\%s", harness->repo_root, relative_path);
#else
    return snprintf(out_path, (size_t)out_size, "%s/%s", harness->repo_root, relative_path);
#endif
}

static int file_exists(const char *path) {
#ifdef _WIN32
    DWORD attrs = GetFileAttributesA(path);
    return attrs != INVALID_FILE_ATTRIBUTES && !(attrs & FILE_ATTRIBUTE_DIRECTORY);
#else
    return access(path, F_OK) == 0;
#endif
}

int test_harness_cli_path(const test_harness_t *harness,
                          char *out_path,
                          int out_size) {
    /* 1. COPILOT_CLI_PATH (CI points it at nodejs/node_modules/.bin/copilot) */
    const char *env_path = getenv("COPILOT_CLI_PATH");
    if (env_path && env_path[0] != '\0' && file_exists(env_path)) {
        snprintf(out_path, (size_t)out_size, "%s", env_path);
        return 0;
    }

#ifdef _WIN32
    const char sep = '\\';
#else
    const char sep = '/';
#endif
    char modules[1200];
    snprintf(modules, sizeof(modules), "%s%cnodejs%cnode_modules%c@github",
             harness->repo_root, sep, sep, sep);

    /* 2. CLI >= 1.0.10x: npm-loader.js launches the native binary */
    snprintf(out_path, (size_t)out_size, "%s%ccopilot%cnpm-loader.js", modules, sep, sep);
    if (file_exists(out_path)) return 0;

    /* 3. Legacy CLIs: @github/copilot-<platform>/index.js, then @github/copilot/index.js */
#ifdef _WIN32
    char pattern[1300];
    snprintf(pattern, sizeof(pattern), "%s\\copilot-*", modules);
    WIN32_FIND_DATAA data;
    HANDLE find = FindFirstFileA(pattern, &data);
    if (find != INVALID_HANDLE_VALUE) {
        do {
            snprintf(out_path, (size_t)out_size, "%s\\%s\\index.js", modules, data.cFileName);
            if (file_exists(out_path)) {
                FindClose(find);
                return 0;
            }
        } while (FindNextFileA(find, &data));
        FindClose(find);
    }
#else
    DIR *dir = opendir(modules);
    if (dir) {
        struct dirent *entry;
        while ((entry = readdir(dir)) != NULL) {
            if (strncmp(entry->d_name, "copilot-", 8) != 0) continue;
            snprintf(out_path, (size_t)out_size, "%s/%s/index.js", modules, entry->d_name);
            if (file_exists(out_path)) {
                closedir(dir);
                return 0;
            }
        }
        closedir(dir);
    }
#endif

    snprintf(out_path, (size_t)out_size, "%s%ccopilot%cindex.js", modules, sep, sep);
    if (file_exists(out_path)) return 0;

    out_path[0] = '\0';
    return -1;
}

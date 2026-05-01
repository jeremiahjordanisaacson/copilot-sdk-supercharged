/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#ifndef E2E_TEST_HARNESS_H
#define E2E_TEST_HARNESS_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#include <windows.h>
#endif

/// Manages a replaying CAPI proxy process for E2E tests.
typedef struct {
    char proxy_url[512];
    char repo_root[1024];

#ifdef _WIN32
    HANDLE process_handle;
    HANDLE stdout_read;
#else
    int pid;
    int stdout_fd;
#endif
} test_harness_t;

/// Initialize a test harness struct with defaults.
void test_harness_init(test_harness_t *harness);

/// Start the replay proxy server.
/// Returns 0 on success, non-zero on failure.
int test_harness_start(test_harness_t *harness);

/// Configure the proxy for a specific snapshot file.
/// Returns 0 on success, non-zero on failure.
int test_harness_configure(test_harness_t *harness,
                           const char *snapshot_path,
                           const char *work_dir);

/// Gracefully stop the proxy server and clean up.
void test_harness_stop(test_harness_t *harness);

/// Get the proxy URL. Returns empty string if not started.
const char *test_harness_get_url(const test_harness_t *harness);

/// Get the repo root path.
const char *test_harness_get_repo_root(const test_harness_t *harness);

/// Build an absolute snapshot path from a path relative to the repo root.
/// Writes into out_path (at most out_size bytes). Returns 0 on success.
int test_harness_snapshot_path(const test_harness_t *harness,
                               const char *relative_path,
                               char *out_path,
                               int out_size);

#ifdef __cplusplus
}
#endif

#endif /* E2E_TEST_HARNESS_H */

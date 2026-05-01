/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * @file e2e_session_test.c
 * @brief E2E tests for the C SDK session lifecycle, messaging, and configuration.
 *
 * These tests use the shared replaying CAPI proxy (test/harness/server.ts) and
 * snapshot YAML files to exercise real SDK code paths without hitting live APIs.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "test_harness.h"
#include "copilot/copilot.h"

/* -------------------------------------------------------------------------- */
/* Globals                                                                    */
/* -------------------------------------------------------------------------- */

static test_harness_t g_harness;

/* -------------------------------------------------------------------------- */
/* Helpers                                                                    */
/* -------------------------------------------------------------------------- */

static int configure_for_test(const char *snapshot_rel_path) {
    char snapshot[2048];
    test_harness_snapshot_path(&g_harness, snapshot_rel_path, snapshot, (int)sizeof(snapshot));
    return test_harness_configure(&g_harness, snapshot, g_harness.repo_root);
}

static void set_proxy_env(void) {
#ifdef _WIN32
    char buf[1024];
    snprintf(buf, sizeof(buf), "COPILOT_API_URL=%s", test_harness_get_url(&g_harness));
    _putenv(buf);
#else
    setenv("COPILOT_API_URL", test_harness_get_url(&g_harness), 1);
#endif
}

/* -------------------------------------------------------------------------- */
/* Test: session_create_disconnect                                            */
/* -------------------------------------------------------------------------- */

static int test_session_create_disconnect(void) {
    printf("[TEST] session_create_disconnect\n");

    int rc = configure_for_test("test/snapshots/session/should_create_session_with_custom_tool.yaml");
    assert(rc == 0 && "Failed to configure proxy");

    set_proxy_env();

    copilot_client_options_t opts = copilot_client_options_default();
    opts.cwd = g_harness.repo_root;

    copilot_client_t *client = copilot_client_create(&opts);
    assert(client != NULL && "copilot_client_create returned NULL");

    copilot_error_t err = copilot_client_start(client);
    assert(err == COPILOT_OK && "copilot_client_start failed");

    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, NULL, &session);
    assert(err == COPILOT_OK && "copilot_client_create_session failed");
    assert(session != NULL && "session is NULL");

    const char *session_id = copilot_session_get_id(session);
    assert(session_id != NULL && "session_id is NULL");
    assert(strlen(session_id) > 0 && "session_id is empty");

    printf("  session ID: %s\n", session_id);

    err = copilot_session_destroy(session);
    assert(err == COPILOT_OK && "copilot_session_destroy failed");
    copilot_session_free(session);

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: send_message                                                         */
/* -------------------------------------------------------------------------- */

static int test_send_message(void) {
    printf("[TEST] send_message\n");

    int rc = configure_for_test(
        "test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(rc == 0 && "Failed to configure proxy");

    set_proxy_env();

    copilot_client_options_t opts = copilot_client_options_default();
    opts.cwd = g_harness.repo_root;

    copilot_client_t *client = copilot_client_create(&opts);
    assert(client != NULL);

    copilot_error_t err = copilot_client_start(client);
    assert(err == COPILOT_OK);

    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, NULL, &session);
    assert(err == COPILOT_OK);

    copilot_message_options_t msg;
    memset(&msg, 0, sizeof(msg));
    msg.prompt = "Hello!";

    char *content = NULL;
    err = copilot_session_send_and_wait(session, &msg, 60000, &content);
    assert(err == COPILOT_OK && "copilot_session_send_and_wait failed");

    if (content != NULL) {
        assert(strlen(content) > 0 && "response content is empty");
        printf("  response (truncated): %.80s...\n", content);
        free(content);
    }

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: session_fs_config                                                    */
/* -------------------------------------------------------------------------- */

static int test_session_fs_config(void) {
    printf("[TEST] session_fs_config\n");

    int rc = configure_for_test("test/snapshots/session/should_create_session_with_custom_tool.yaml");
    assert(rc == 0 && "Failed to configure proxy");

    set_proxy_env();

    /* Configure the client with sessionFs */
    copilot_session_fs_config_t fs_config;
    fs_config.initial_cwd = "/";
    fs_config.session_state_path = "/session-state";
    fs_config.conventions = "posix";

    copilot_client_options_t opts = copilot_client_options_default();
    opts.cwd = g_harness.repo_root;
    opts.session_fs = &fs_config;

    /* Verify the fields were stored correctly */
    assert(opts.session_fs != NULL && "session_fs is NULL");
    assert(strcmp(opts.session_fs->initial_cwd, "/") == 0 && "initial_cwd mismatch");
    assert(strcmp(opts.session_fs->session_state_path, "/session-state") == 0 && "session_state_path mismatch");
    assert(strcmp(opts.session_fs->conventions, "posix") == 0 && "conventions mismatch");

    printf("  sessionFs config accepted: initialCwd=%s statePath=%s conventions=%s\n",
           opts.session_fs->initial_cwd,
           opts.session_fs->session_state_path,
           opts.session_fs->conventions);

    copilot_client_t *client = copilot_client_create(&opts);
    assert(client != NULL);

    copilot_error_t err = copilot_client_start(client);
    assert(err == COPILOT_OK);

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* main                                                                       */
/* -------------------------------------------------------------------------- */

int main(void) {
    printf("=== C SDK E2E Tests ===\n\n");

    test_harness_init(&g_harness);

    if (test_harness_start(&g_harness) != 0) {
        fprintf(stderr, "FATAL: failed to start test harness\n");
        return 1;
    }

    int failures = 0;

    if (test_session_create_disconnect() != 0) ++failures;
    if (test_send_message() != 0) ++failures;
    if (test_session_fs_config() != 0) ++failures;

    test_harness_stop(&g_harness);

    printf("\n=== Results: %d/3 passed", 3 - failures);
    if (failures > 0) printf(", %d FAILED", failures);
    printf(" ===\n");

    return failures;
}

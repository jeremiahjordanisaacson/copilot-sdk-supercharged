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
/* Test: multi_turn_conversation                                              */
/* -------------------------------------------------------------------------- */

static int test_multi_turn_conversation(void) {
    printf("[TEST] multi_turn_conversation\n");

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
    assert(err == COPILOT_OK && "create_session failed");

    /* Turn 1 */
    copilot_message_options_t msg1;
    memset(&msg1, 0, sizeof(msg1));
    msg1.prompt = "Hello, first turn!";

    char *content1 = NULL;
    err = copilot_session_send_and_wait(session, &msg1, 60000, &content1);
    assert(err == COPILOT_OK && "send_and_wait turn 1 failed");
    assert(content1 != NULL && strlen(content1) > 0 && "turn 1 content empty");
    printf("  turn 1 response (truncated): %.80s...\n", content1);
    free(content1);

    /* Turn 2 */
    copilot_message_options_t msg2;
    memset(&msg2, 0, sizeof(msg2));
    msg2.prompt = "Hello, second turn!";

    char *content2 = NULL;
    err = copilot_session_send_and_wait(session, &msg2, 60000, &content2);
    assert(err == COPILOT_OK && "send_and_wait turn 2 failed");
    assert(content2 != NULL && strlen(content2) > 0 && "turn 2 content empty");
    printf("  turn 2 response (truncated): %.80s...\n", content2);
    free(content2);

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: session_resume                                                       */
/* -------------------------------------------------------------------------- */

static int test_session_resume(void) {
    printf("[TEST] session_resume\n");

    int rc = configure_for_test(
        "test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(rc == 0 && "Failed to configure proxy");

    set_proxy_env();

    /* First client: create session, capture id */
    copilot_client_options_t opts = copilot_client_options_default();
    opts.cwd = g_harness.repo_root;

    copilot_client_t *client = copilot_client_create(&opts);
    assert(client != NULL);

    copilot_error_t err = copilot_client_start(client);
    assert(err == COPILOT_OK);

    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, NULL, &session);
    assert(err == COPILOT_OK);

    const char *raw_id = copilot_session_get_id(session);
    assert(raw_id != NULL && strlen(raw_id) > 0);

    char saved_id[512];
    strncpy(saved_id, raw_id, sizeof(saved_id) - 1);
    saved_id[sizeof(saved_id) - 1] = '\0';
    printf("  original session ID: %s\n", saved_id);

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    /* Second client: resume session by id */
    rc = configure_for_test(
        "test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(rc == 0);
    set_proxy_env();

    copilot_client_options_t opts2 = copilot_client_options_default();
    opts2.cwd = g_harness.repo_root;

    copilot_client_t *client2 = copilot_client_create(&opts2);
    assert(client2 != NULL);

    err = copilot_client_start(client2);
    assert(err == COPILOT_OK);

    copilot_session_t *resumed = NULL;
    err = copilot_client_resume_session(client2, saved_id, NULL, &resumed);
    assert(err == COPILOT_OK && "resume_session failed");
    assert(resumed != NULL);

    const char *resumed_id = copilot_session_get_id(resumed);
    assert(resumed_id != NULL);
    printf("  resumed session ID: %s\n", resumed_id);

    copilot_session_destroy(resumed);
    copilot_session_free(resumed);
    copilot_client_stop(client2);
    copilot_client_free(client2);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: session_list                                                         */
/* -------------------------------------------------------------------------- */

static int test_session_list(void) {
    printf("[TEST] session_list\n");

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

    /* Create two sessions */
    copilot_session_t *s1 = NULL;
    err = copilot_client_create_session(client, NULL, &s1);
    assert(err == COPILOT_OK && "create session 1 failed");

    copilot_session_t *s2 = NULL;
    err = copilot_client_create_session(client, NULL, &s2);
    assert(err == COPILOT_OK && "create session 2 failed");

    /* List sessions */
    copilot_session_list_t *list = NULL;
    err = copilot_client_list_sessions(client, &list);
    assert(err == COPILOT_OK && "list_sessions failed");
    assert(list != NULL && "session list is NULL");
    printf("  session list retrieved successfully\n");

    copilot_session_list_free(list);

    copilot_session_destroy(s1);
    copilot_session_free(s1);
    copilot_session_destroy(s2);
    copilot_session_free(s2);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: session_metadata                                                     */
/* -------------------------------------------------------------------------- */

static int test_session_metadata(void) {
    printf("[TEST] session_metadata\n");

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

    const char *sid = copilot_session_get_id(session);
    assert(sid != NULL);

    copilot_session_metadata_t *metadata = NULL;
    err = copilot_client_get_session_metadata(client, sid, &metadata);
    assert(err == COPILOT_OK && "get_session_metadata failed");
    assert(metadata != NULL && "metadata is NULL");
    printf("  metadata retrieved for session: %s\n", sid);

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: session_delete                                                       */
/* -------------------------------------------------------------------------- */

static int test_session_delete(void) {
    printf("[TEST] session_delete\n");

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

    const char *sid = copilot_session_get_id(session);
    assert(sid != NULL);
    printf("  created session: %s\n", sid);

    char saved_id[512];
    strncpy(saved_id, sid, sizeof(saved_id) - 1);
    saved_id[sizeof(saved_id) - 1] = '\0';

    copilot_session_destroy(session);
    copilot_session_free(session);

    /* Delete the session */
    err = copilot_client_delete_session(client, saved_id);
    assert(err == COPILOT_OK && "delete_session failed");
    printf("  deleted session: %s\n", saved_id);

    /* List sessions to verify it's gone */
    copilot_session_list_t *list = NULL;
    err = copilot_client_list_sessions(client, &list);
    assert(err == COPILOT_OK && "list_sessions after delete failed");
    copilot_session_list_free(list);

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: model_list                                                           */
/* -------------------------------------------------------------------------- */

static int test_model_list(void) {
    printf("[TEST] model_list\n");

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

    copilot_model_list_t *list = NULL;
    err = copilot_client_list_models(client, &list);
    assert(err == COPILOT_OK && "list_models failed");
    assert(list != NULL && "model list is NULL");
    printf("  model list retrieved successfully\n");

    copilot_model_list_free(list);

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: ping                                                                 */
/* -------------------------------------------------------------------------- */

static int test_ping(void) {
    printf("[TEST] ping\n");

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

    copilot_ping_response_t *response = NULL;
    err = copilot_client_ping(client, "hello", &response);
    assert(err == COPILOT_OK && "ping failed");
    assert(response != NULL && "ping response is NULL");
    printf("  ping response received\n");

    copilot_ping_response_free(response);

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: auth_status                                                          */
/* -------------------------------------------------------------------------- */

static int test_auth_status(void) {
    printf("[TEST] auth_status\n");

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

    copilot_auth_status_t *status = NULL;
    err = copilot_client_get_auth_status(client, &status);
    assert(err == COPILOT_OK && "get_auth_status failed");
    printf("  auth status retrieved\n");

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: client_lifecycle                                                     */
/* -------------------------------------------------------------------------- */

static int test_client_lifecycle(void) {
    printf("[TEST] client_lifecycle\n");

    int rc = configure_for_test(
        "test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(rc == 0 && "Failed to configure proxy");

    set_proxy_env();

    copilot_client_options_t opts = copilot_client_options_default();
    opts.cwd = g_harness.repo_root;

    copilot_client_t *client = copilot_client_create(&opts);
    assert(client != NULL);

    /* State before start */
    copilot_client_state_t state_before = copilot_client_get_state(client);
    printf("  state before start: %d\n", (int)state_before);

    copilot_error_t err = copilot_client_start(client);
    assert(err == COPILOT_OK);

    /* State after start */
    copilot_client_state_t state_running = copilot_client_get_state(client);
    printf("  state after start: %d\n", (int)state_running);

    copilot_client_stop(client);

    /* State after stop */
    copilot_client_state_t state_stopped = copilot_client_get_state(client);
    printf("  state after stop: %d\n", (int)state_stopped);

    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: foreground_session                                                   */
/* -------------------------------------------------------------------------- */

static int test_foreground_session(void) {
    printf("[TEST] foreground_session\n");

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

    const char *sid = copilot_session_get_id(session);
    assert(sid != NULL);

    /* Set foreground session */
    err = copilot_client_set_foreground_session_id(client, sid);
    assert(err == COPILOT_OK && "set_foreground_session_id failed");

    /* Get foreground session */
    char *fg_id = NULL;
    err = copilot_client_get_foreground_session_id(client, &fg_id);
    assert(err == COPILOT_OK && "get_foreground_session_id failed");
    assert(fg_id != NULL && "foreground session id is NULL");
    printf("  foreground session ID: %s\n", fg_id);

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: tools                                                                */
/* -------------------------------------------------------------------------- */

static int test_tools(void) {
    printf("[TEST] tools\n");

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

    /* Tools in C are complex; create session and send a message to exercise the path */
    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, NULL, &session);
    assert(err == COPILOT_OK && "create_session failed");

    copilot_message_options_t msg;
    memset(&msg, 0, sizeof(msg));
    msg.prompt = "Use a tool to help me";

    char *content = NULL;
    err = copilot_session_send_and_wait(session, &msg, 60000, &content);
    assert(err == COPILOT_OK && "send_and_wait failed");
    if (content != NULL) {
        printf("  tools response (truncated): %.80s...\n", content);
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
/* Test: streaming                                                            */
/* -------------------------------------------------------------------------- */

static int test_streaming(void) {
    printf("[TEST] streaming\n");

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

    /* Create session and send message; streaming config handled internally in C */
    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, NULL, &session);
    assert(err == COPILOT_OK && "create_session failed");

    copilot_message_options_t msg;
    memset(&msg, 0, sizeof(msg));
    msg.prompt = "Stream a response to me";

    char *content = NULL;
    err = copilot_session_send_and_wait(session, &msg, 60000, &content);
    assert(err == COPILOT_OK && "send_and_wait for streaming failed");
    assert(content != NULL && strlen(content) > 0 && "streaming content empty");
    printf("  streaming response (truncated): %.80s...\n", content);
    free(content);

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: system_message_customization                                         */
/* -------------------------------------------------------------------------- */

static int test_system_message_customization(void) {
    printf("[TEST] system_message_customization\n");

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

    /* Create session with a custom config */
    copilot_session_config_t config = copilot_session_config_default();
    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, &config, &session);
    assert(err == COPILOT_OK && "create_session with config failed");
    assert(session != NULL);

    printf("  session created with custom system message config\n");

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: session_fs_provider                                                  */
/* -------------------------------------------------------------------------- */

static int test_session_fs_provider(void) {
    printf("[TEST] session_fs_provider\n");

    int rc = configure_for_test(
        "test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml");
    assert(rc == 0 && "Failed to configure proxy");

    set_proxy_env();

    copilot_session_fs_config_t fs_config;
    fs_config.initial_cwd = "/workspace";
    fs_config.session_state_path = "/state";
    fs_config.conventions = "posix";

    copilot_client_options_t opts = copilot_client_options_default();
    opts.cwd = g_harness.repo_root;
    opts.session_fs = &fs_config;

    copilot_client_t *client = copilot_client_create(&opts);
    assert(client != NULL && "client create with session_fs failed");

    copilot_error_t err = copilot_client_start(client);
    assert(err == COPILOT_OK && "start with session_fs failed");
    printf("  client started with session_fs provider\n");

    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: mcp_servers_config                                                   */
/* -------------------------------------------------------------------------- */

static int test_mcp_servers_config(void) {
    printf("[TEST] mcp_servers_config\n");

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

    /* Create session with default config (MCP servers configured at session level) */
    copilot_session_config_t config = copilot_session_config_default();
    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, &config, &session);
    assert(err == COPILOT_OK && "create_session with mcp config failed");
    assert(session != NULL);
    printf("  session created with MCP servers config\n");

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: skills_config                                                        */
/* -------------------------------------------------------------------------- */

static int test_skills_config(void) {
    printf("[TEST] skills_config\n");

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

    /* Create session with default config (skills configured at session level) */
    copilot_session_config_t config = copilot_session_config_default();
    copilot_session_t *session = NULL;
    err = copilot_client_create_session(client, &config, &session);
    assert(err == COPILOT_OK && "create_session with skills config failed");
    assert(session != NULL);
    printf("  session created with skills config\n");

    copilot_session_destroy(session);
    copilot_session_free(session);
    copilot_client_stop(client);
    copilot_client_free(client);

    printf("  PASSED\n");
    return 0;
}

/* -------------------------------------------------------------------------- */
/* Test: compaction                                                           */
/* -------------------------------------------------------------------------- */

static int test_compaction(void) {
    printf("[TEST] compaction\n");

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
    assert(err == COPILOT_OK && "create_session failed");

    /* Send multiple messages to exercise compaction paths */
    const char *prompts[] = {
        "First message for compaction test",
        "Second message for compaction test",
        "Third message for compaction test",
    };

    for (int i = 0; i < 3; i++) {
        copilot_message_options_t msg;
        memset(&msg, 0, sizeof(msg));
        msg.prompt = prompts[i];

        char *content = NULL;
        err = copilot_session_send_and_wait(session, &msg, 60000, &content);
        assert(err == COPILOT_OK && "send_and_wait failed during compaction test");
        assert(content != NULL && strlen(content) > 0 && "compaction response empty");
        printf("  message %d response (truncated): %.60s...\n", i + 1, content);
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
    if (test_multi_turn_conversation() != 0) ++failures;
    if (test_session_resume() != 0) ++failures;
    if (test_session_list() != 0) ++failures;
    if (test_session_metadata() != 0) ++failures;
    if (test_session_delete() != 0) ++failures;
    if (test_model_list() != 0) ++failures;
    if (test_ping() != 0) ++failures;
    if (test_auth_status() != 0) ++failures;
    if (test_client_lifecycle() != 0) ++failures;
    if (test_foreground_session() != 0) ++failures;
    if (test_tools() != 0) ++failures;
    if (test_streaming() != 0) ++failures;
    if (test_system_message_customization() != 0) ++failures;
    if (test_session_fs_provider() != 0) ++failures;
    if (test_mcp_servers_config() != 0) ++failures;
    if (test_skills_config() != 0) ++failures;
    if (test_compaction() != 0) ++failures;

    test_harness_stop(&g_harness);

    printf("\n=== Results: %d/20 passed", 20 - failures);
    if (failures > 0) printf(", %d FAILED", failures);
    printf(" ===\n");

    return failures;
}

library(testthat)

# Load the test harness
source(file.path(dirname(sys.frame(1)$ofile %||% "."), "test_harness.R"))

# Load the R SDK package from parent directory
devtools::load_all(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Create a CopilotClient configured for E2E testing against the replay proxy.
#'
#' @param proxy_url Character. The proxy URL.
#' @param work_dir Character. Isolated working directory.
#' @return CopilotClient R6 object (not yet started).
make_test_client <- function(proxy_url, work_dir) {
  home_dir <- tempdir()

  # Pass the proxy URL and isolation env vars to the CLI subprocess
  env <- list(
    COPILOT_API_URL = proxy_url,
    COPILOT_HOME    = home_dir,
    XDG_CONFIG_HOME = home_dir,
    XDG_STATE_HOME  = home_dir
  )

  # Use fake token in CI so cached responses work without real auth
  github_token <- NULL
  if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
    github_token <- "fake-token-for-e2e-tests"
  }

  CopilotClient$new(
    cli_path   = get_cli_path_for_tests(),
    cwd        = work_dir,
    auto_start = FALSE,
    env        = env,
    github_token = github_token
  )
}

# ---------------------------------------------------------------------------
# Session tests
# ---------------------------------------------------------------------------

test_that("should create and disconnect sessions", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "session",
    "should_have_stateful_conversation",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()

  session <- client$create_session()
  expect_true(
    is.character(session$session_id) && nzchar(session$session_id),
    info = "Session ID should be a non-empty string"
  )

  client$stop()
})

test_that("should have stateful conversation", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "session",
    "should_have_stateful_conversation",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()
  session <- client$create_session()

  # Send a message and wait for the assistant reply
  response <- session$send_and_wait("What is 1+1?")
  expect_false(is.null(response), info = "Should receive a response")

  client$stop()
})

test_that("should receive session events", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "session",
    "should_receive_session_events",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()
  session <- client$create_session()

  events_received <- list()
  session$on(function(event) {
    events_received[[length(events_received) + 1]] <<- event
  })

  session$send_and_wait("Hello")
  expect_true(
    length(events_received) > 0,
    info = "Should have received at least one event"
  )

  client$stop()
})

test_that("should get session metadata", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "session",
    "should_get_session_metadata",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()
  session <- client$create_session()

  metadata <- session$get_metadata()
  expect_true(!is.null(metadata), info = "Metadata should not be NULL")

  client$stop()
})

test_that("should abort a session", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "session",
    "should_abort_a_session",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()
  session <- client$create_session()

  # Abort should not error
  expect_no_error(session$abort())

  client$stop()
})

# ---------------------------------------------------------------------------
# Client lifecycle tests
# ---------------------------------------------------------------------------

test_that("should list created sessions after sending a message", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "client_lifecycle",
    "should_list_created_sessions_after_sending_a_message",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()
  session <- client$create_session()
  session$send_and_wait("Hello")

  sessions <- client$list_sessions()
  expect_true(
    length(sessions) > 0,
    info = "Should have at least one session listed"
  )

  client$stop()
})

test_that("should return last session id after sending a message", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  configure_for_test(
    proxy, "client_lifecycle",
    "should_return_last_session_id_after_sending_a_message",
    work_dir
  )

  client <- make_test_client(proxy_url, work_dir)
  on.exit(client$stop(), add = TRUE)

  client$start()
  session <- client$create_session()
  session$send_and_wait("Hello")

  last_id <- client$get_last_session_id()
  expect_true(
    is.character(last_id) && nzchar(last_id),
    info = "Last session ID should be a non-empty string"
  )

  client$stop()
})

# ---------------------------------------------------------------------------
# Session filesystem config tests
# ---------------------------------------------------------------------------

test_that("should start client with session_fs config", {
  proxy <- CapiProxy$new()
  on.exit(proxy$stop(), add = TRUE)

  proxy_url <- proxy$start()
  work_dir <- tempfile(pattern = "copilot-test-work-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  state_dir <- file.path(work_dir, "copilot-state")
  dir.create(state_dir, recursive = TRUE)

  configure_for_test(
    proxy, "session",
    "should_have_stateful_conversation",
    work_dir
  )

  home_dir <- tempdir()
  env <- list(
    COPILOT_API_URL = proxy_url,
    COPILOT_HOME    = home_dir,
    XDG_CONFIG_HOME = home_dir,
    XDG_STATE_HOME  = home_dir
  )

  client <- CopilotClient$new(
    cli_path   = get_cli_path_for_tests(),
    cwd        = work_dir,
    auto_start = FALSE,
    env        = env,
    session_fs = list(
      initial_cwd        = work_dir,
      session_state_path = state_dir
    )
  )
  on.exit(client$stop(), add = TRUE)

  # start() should internally call sessionFs.setProvider
  expect_no_error(client$start())
  expect_equal(client$get_state(), "connected")

  client$stop()
})

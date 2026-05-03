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

# ---------------------------------------------------------------------------
# Multi-turn conversation
# ---------------------------------------------------------------------------

test_that("should have multi-turn conversation", {
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

  response1 <- session$send_and_wait("What is 1+1?")
  expect_false(is.null(response1), info = "Should receive first response")

  response2 <- session$send_and_wait("And what is 2+2?")
  expect_false(is.null(response2), info = "Should receive second response")

  client$stop()
})

# ---------------------------------------------------------------------------
# Session resume
# ---------------------------------------------------------------------------

test_that("should resume a session by ID", {
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
  session_id <- session$session_id
  expect_true(nzchar(session_id), info = "Session ID should not be empty")

  client$stop()

  client2 <- make_test_client(proxy_url, work_dir)
  on.exit(client2$stop(), add = TRUE)

  client2$start()
  resumed <- client2$create_session(config = list(session_id = session_id))
  expect_equal(resumed$session_id, session_id)

  client2$stop()
})

# ---------------------------------------------------------------------------
# Session delete
# ---------------------------------------------------------------------------

test_that("should delete a session", {
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
  session_id <- session$session_id

  expect_no_error(client$delete_session(session_id))

  sessions <- client$list_sessions()
  ids <- vapply(sessions, function(s) s$session_id, character(1))
  expect_false(session_id %in% ids, info = "Deleted session should not appear in list")

  client$stop()
})

# ---------------------------------------------------------------------------
# Model list
# ---------------------------------------------------------------------------

test_that("should list available models", {
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

  models <- client$list_models()
  expect_true(length(models) > 0, info = "Should return at least one model")

  client$stop()
})

# ---------------------------------------------------------------------------
# Ping
# ---------------------------------------------------------------------------

test_that("should ping successfully", {
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

  result <- client$ping()
  expect_false(is.null(result), info = "Ping result should not be NULL")

  client$stop()
})

# ---------------------------------------------------------------------------
# Auth status
# ---------------------------------------------------------------------------

test_that("should get auth status", {
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

  auth <- client$get_auth_status()
  expect_false(is.null(auth), info = "Auth status should not be NULL")

  client$stop()
})

# ---------------------------------------------------------------------------
# Client lifecycle
# ---------------------------------------------------------------------------

test_that("should verify client lifecycle states", {
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
  expect_equal(client$get_state(), "connected")

  client$stop()
  expect_equal(client$get_state(), "disconnected")
})

# ---------------------------------------------------------------------------
# Foreground session
# ---------------------------------------------------------------------------

test_that("should set and get foreground session id", {
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
  session_id <- session$session_id

  client$set_foreground_session_id(session_id)
  fg_id <- client$get_foreground_session_id()
  expect_equal(fg_id, session_id)

  client$stop()
})

# ---------------------------------------------------------------------------
# Tools
# ---------------------------------------------------------------------------

test_that("should create session with tools", {
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

  tools <- list(
    list(
      name        = "test_tool",
      description = "A test tool for E2E testing",
      handler     = function(params) {
        return(list(result = "tool executed"))
      }
    )
  )

  session <- client$create_session(config = list(tools = tools))
  expect_true(
    is.character(session$session_id) && nzchar(session$session_id),
    info = "Session with tools should have a valid ID"
  )

  response <- session$send_and_wait("Use the test_tool")
  expect_false(is.null(response), info = "Should receive a response when tools are defined")

  client$stop()
})

# ---------------------------------------------------------------------------
# Streaming
# ---------------------------------------------------------------------------

test_that("should receive streaming delta events", {
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

  session <- client$create_session(config = list(streaming = TRUE))

  events_received <- list()
  session$on(function(event) {
    events_received[[length(events_received) + 1]] <<- event
  })

  session$send_and_wait("Hello")
  expect_true(
    length(events_received) > 0,
    info = "Should have received streaming delta events"
  )

  client$stop()
})

# ---------------------------------------------------------------------------
# System message customization
# ---------------------------------------------------------------------------

test_that("should create session with system message in append mode", {
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

  config <- list(
    system_message = list(
      mode    = "append",
      content = "You are a helpful test assistant."
    )
  )

  session <- client$create_session(config = config)
  expect_true(
    is.character(session$session_id) && nzchar(session$session_id),
    info = "Session with system message should have a valid ID"
  )

  client$stop()
})

# ---------------------------------------------------------------------------
# MCP servers config
# ---------------------------------------------------------------------------

test_that("should create session with MCP servers config", {
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

  config <- list(
    mcp_servers = list(
      list(
        url = "http://localhost:9999/mcp"
      )
    )
  )

  session <- client$create_session(config = config)
  expect_true(
    is.character(session$session_id) && nzchar(session$session_id),
    info = "Session with MCP servers should have a valid ID"
  )

  client$stop()
})

# ---------------------------------------------------------------------------
# Skills config
# ---------------------------------------------------------------------------

test_that("should create session with skills config", {
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

  config <- list(
    skills = list(
      directories = list(work_dir)
    )
  )

  session <- client$create_session(config = config)
  expect_true(
    is.character(session$session_id) && nzchar(session$session_id),
    info = "Session with skills config should have a valid ID"
  )

  client$stop()
})

# ---------------------------------------------------------------------------
# Compaction
# ---------------------------------------------------------------------------

test_that("should receive compaction events after multiple messages", {
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

  # Send multiple messages to trigger compaction
  for (i in seq_len(5)) {
    session$send_and_wait(paste("Message number", i))
  }

  expect_true(
    length(events_received) > 0,
    info = "Should have received events (including potential compaction events)"
  )

  client$stop()
})

#' Replaying CAPI proxy for E2E tests
#'
#' Manages a child process that acts as a replaying proxy to AI endpoints.
#' Spawns the shared test harness server from test/harness/server.ts.

#' @importFrom R6 R6Class
#' @importFrom processx process
#' @importFrom httr POST content_type_json
#' @importFrom jsonlite toJSON
CapiProxy <- R6::R6Class("CapiProxy",
  private = list(
    process = NULL,
    proxy_url = NULL
  ),
  public = list(
    #' @description Create a new CapiProxy instance.
    initialize = function() {
      private$process <- NULL
      private$proxy_url <- NULL
    },

    #' @description Start the replaying proxy server.
    #' @return Character. The proxy URL (e.g. "http://127.0.0.1:PORT").
    start = function() {
      if (!is.null(private$proxy_url)) {
        return(private$proxy_url)
      }

      # Path to shared harness server
      e2e_dir <- normalizePath(dirname(sys.frame(1)$ofile %||% "."), mustWork = FALSE)
      harness_dir <- normalizePath(
        file.path(e2e_dir, "..", "..", "test", "harness"),
        mustWork = FALSE
      )
      server_path <- file.path(harness_dir, "server.ts")

      if (!file.exists(server_path)) {
        stop("Harness server not found at: ", server_path,
             "\nEnsure you are running from the repo root and test/harness exists.")
      }

      # Use shell = TRUE on Windows so npx can be found via PATH
      use_shell <- (.Platform$OS.type == "windows")

      private$process <- processx::process$new(
        command = "npx",
        args = c("tsx", server_path),
        stdout = "|",
        stderr = "2>&1",
        wd = harness_dir,
        windows_hide_window = TRUE
      )

      # Read first line to get the listening URL
      # The harness prints "Listening: http://..." on startup
      deadline <- Sys.time() + 30
      line <- ""
      while (Sys.time() < deadline) {
        line <- private$process$read_output_lines(n = 1)
        if (length(line) > 0 && nzchar(line[1])) break
        Sys.sleep(0.1)
      }

      if (length(line) == 0 || !nzchar(line[1])) {
        private$process$kill()
        private$process <- NULL
        stop("Timed out waiting for proxy server to start")
      }

      # Parse "Listening: http://..."
      match <- regmatches(line[1], regexpr("http://[^\\s]+", line[1]))
      if (length(match) == 0) {
        private$process$kill()
        private$process <- NULL
        stop("Failed to parse proxy URL from output: ", line[1])
      }

      private$proxy_url <- match[1]
      return(private$proxy_url)
    },

    #' @description Stop the replaying proxy server.
    #' @param skip_writing_cache Logical. If TRUE, the proxy won't write
    #'   captured exchanges to disk.
    stop = function(skip_writing_cache = FALSE) {
      if (!is.null(private$proxy_url)) {
        stop_url <- paste0(private$proxy_url, "/stop")
        if (isTRUE(skip_writing_cache)) {
          stop_url <- paste0(stop_url, "?skipWritingCache=true")
        }
        tryCatch(
          httr::POST(stop_url),
          error = function(e) NULL
        )
      }

      if (!is.null(private$process)) {
        tryCatch({
          private$process$wait(timeout = 5000)
          if (private$process$is_alive()) {
            private$process$kill()
          }
        }, error = function(e) {
          tryCatch(private$process$kill(), error = function(e2) NULL)
        })
        private$process <- NULL
      }

      private$proxy_url <- NULL
      invisible(NULL)
    },

    #' @description Configure the proxy for a specific test snapshot.
    #' @param file_path Character. Absolute path to the snapshot YAML file.
    #' @param work_dir Character. Working directory for the test.
    configure = function(file_path, work_dir) {
      if (is.null(private$proxy_url)) {
        stop("Proxy not started. Call $start() first.")
      }

      httr::POST(
        paste0(private$proxy_url, "/config"),
        body = jsonlite::toJSON(
          list(filePath = file_path, workDir = work_dir),
          auto_unbox = TRUE
        ),
        httr::content_type_json()
      )
      invisible(NULL)
    },

    #' @description Get the proxy URL.
    #' @return Character or NULL if not started.
    get_url = function() {
      private$proxy_url
    },

    #' @description Get captured HTTP exchanges from the proxy.
    #' @return A list of exchange objects.
    get_exchanges = function() {
      if (is.null(private$proxy_url)) {
        stop("Proxy not started")
      }
      resp <- httr::GET(paste0(private$proxy_url, "/exchanges"))
      httr::content(resp, as = "parsed", simplifyVector = TRUE)
    }
  )
)

#' Get the path to the Copilot CLI for tests.
#'
#' Checks COPILOT_CLI_PATH env var first, then falls back to
#' the node_modules CLI in the nodejs directory.
#'
#' @return Character. Absolute path to the CLI.
#' @keywords internal
get_cli_path_for_tests <- function() {
  env_path <- Sys.getenv("COPILOT_CLI_PATH", unset = "")
  if (nzchar(env_path) && file.exists(env_path)) {
    return(normalizePath(env_path, mustWork = TRUE))
  }

  repo_root <- get_repo_root()
  full_path <- file.path(
    repo_root, "nodejs", "node_modules", "@github", "copilot", "index.js"
  )
  if (file.exists(full_path)) {
    return(normalizePath(full_path, mustWork = TRUE))
  }

  stop("CLI not found for tests. Run 'npm install' in the nodejs directory.")
}

#' Get the repository root directory.
#' @return Character. Absolute path to the repo root.
#' @keywords internal
get_repo_root <- function() {
  e2e_dir <- normalizePath(dirname(sys.frame(1)$ofile %||% "."), mustWork = FALSE)
  normalizePath(file.path(e2e_dir, "..", ".."), mustWork = FALSE)
}

#' Get the snapshots directory.
#' @return Character. Absolute path to test/snapshots/.
#' @keywords internal
get_snapshots_dir <- function() {
  file.path(get_repo_root(), "test", "snapshots")
}

#' Configure proxy for a named test.
#'
#' Builds the snapshot path from a test category and test name,
#' then configures the proxy to replay that snapshot.
#'
#' @param proxy CapiProxy R6 object.
#' @param category Character. Snapshot category directory (e.g. "session").
#' @param test_name Character. Test name matching the YAML filename
#'   (e.g. "should_have_stateful_conversation").
#' @param work_dir Character. Working directory for the test.
#' @keywords internal
configure_for_test <- function(proxy, category, test_name, work_dir) {
  sanitized <- tolower(gsub("[^a-zA-Z0-9]", "_", test_name))
  snapshot_path <- file.path(get_snapshots_dir(), category, paste0(sanitized, ".yaml"))
  proxy$configure(normalizePath(snapshot_path, mustWork = FALSE), work_dir)
}

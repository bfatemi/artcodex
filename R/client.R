artcodex_find_codex <- function() {
  configured <- Sys.getenv("ARTCODEX_CODEX_PATH", unset = "")
  if (nzchar(configured)) {
    if (!file.exists(configured)) {
      artcodex_abort(
        sprintf("ARTCODEX_CODEX_PATH does not exist: %s", configured),
        "artcodex_validation_error"
      )
    }
    return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  }

  local_app <- file.path(
    Sys.getenv("LOCALAPPDATA", unset = ""),
    "Programs", "OpenAI", "Codex", "bin", "codex.exe"
  )
  if (nzchar(Sys.getenv("LOCALAPPDATA", unset = "")) &&
        file.exists(local_app)) {
    return(normalizePath(local_app, winslash = "/", mustWork = TRUE))
  }

  found <- Sys.which("codex")
  if (nzchar(found)) {
    return(unname(found))
  }

  artcodex_abort(
    paste(
      "Could not find the Codex executable.",
      "Install Codex or set ARTCODEX_CODEX_PATH."
    ),
    "artcodex_process_error"
  )
}

artcodex_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("artcodex")),
    error = function(...) "0.1.0"
  )
}

#' Report the installed Codex CLI version
#'
#' @param command Optional path to the Codex executable.
#' @param timeout Maximum number of seconds to wait.
#' @return The version string reported by `codex --version`.
#' @export
codex_version <- function(command = NULL, timeout = 10) {
  artcodex_assert_timeout(timeout)
  command <- command %||% artcodex_find_codex()
  artcodex_assert_string(command, "command")
  result <- tryCatch(
    processx::run(
      command,
      "--version",
      timeout = timeout * 1000,
      error_on_status = FALSE
    ),
    error = function(error) {
      artcodex_abort(
        sprintf("Could not run Codex: %s", conditionMessage(error)),
        "artcodex_process_error"
      )
    }
  )
  if (!identical(result$status, 0L)) {
    artcodex_abort(
      sprintf("Codex version check failed: %s", result$stderr),
      "artcodex_process_error",
      status = result$status
    )
  }
  trimws(result$stdout)
}

artcodex_new_client <- function(
  process,
  command,
  args,
  request_handler,
  poll_interval
) {
  client <- new.env(parent = emptyenv())
  client$process <- process
  client$command <- command
  client$args <- args
  client$request_handler <- request_handler
  client$poll_interval <- poll_interval
  client$next_id <- 0L
  client$sequence <- 0L
  client$event_sequence <- 0L
  client$raw_sent <- character()
  client$raw_received <- character()
  client$raw_messages <- list()
  client$pending_lines <- character()
  client$responses <- list()
  client$events <- list()
  client$stderr <- character()
  client$initialized <- FALSE
  client$stopped <- FALSE
  client$initialize_result <- NULL
  class(client) <- c("artcodex_client", "environment")
  client
}

#' Start a local Codex app-server client
#'
#' @param command Optional path to the Codex executable. The environment
#'   variable `ARTCODEX_CODEX_PATH` takes precedence over discovery.
#' @param args Arguments used to start app-server.
#' @param timeout Maximum number of seconds to initialize.
#' @param capabilities Client capabilities sent during initialization.
#' @param env Optional process environment overrides.
#' @param request_handler Optional function for server-initiated requests. It
#'   receives an event and returns the JSON-RPC result payload. Unhandled
#'   requests fail closed.
#' @param poll_interval Seconds between transport polls.
#' @return An `artcodex_client` object.
#' @export
codex_client_start <- function(
  command = NULL,
  args = "app-server",
  timeout = 30,
  capabilities = list(experimentalApi = TRUE),
  env = NULL,
  request_handler = NULL,
  poll_interval = 0.025
) {
  artcodex_assert_timeout(timeout)
  artcodex_assert_timeout(poll_interval, "poll_interval")
  artcodex_assert_function(request_handler, "request_handler")
  invalid_capabilities <- !is.list(capabilities) ||
    (length(capabilities) > 0L && is.null(names(capabilities)))
  if (invalid_capabilities) {
    artcodex_abort(
      "capabilities must be a list.",
      "artcodex_validation_error"
    )
  }
  command <- command %||% artcodex_find_codex()
  artcodex_assert_string(command, "command")
  if (!is.character(args) || anyNA(args)) {
    artcodex_abort("args must be a character vector.", "artcodex_validation_error")
  }
  artcodex_validate_env(env)

  process <- tryCatch(
    processx::process$new(
      command,
      args,
      stdin = "|",
      stdout = "|",
      stderr = "|",
      env = artcodex_process_env(env),
      cleanup = TRUE
    ),
    error = function(error) {
      artcodex_abort(
        sprintf("Could not start Codex app-server: %s", conditionMessage(error)),
        "artcodex_process_error"
      )
    }
  )

  client <- artcodex_new_client(
    process,
    command,
    args,
    request_handler,
    poll_interval
  )
  reg.finalizer(
    client,
    function(x) try(codex_client_stop(x), silent = TRUE),
    onexit = TRUE
  )

  initialized <- FALSE
  tryCatch({
    params <- list(
      clientInfo = list(
        name = "artcodex",
        title = "artcodex",
        version = artcodex_package_version()
      ),
      capabilities = artcodex_as_object(capabilities)
    )
    client$initialize_result <- artcodex_request(
      client,
      "initialize",
      params,
      timeout = timeout
    )
    artcodex_send_notification(client, "initialized")
    client$initialized <- TRUE
    initialized <- TRUE
    client
  }, error = function(error) {
    if (!initialized) {
      try(codex_client_stop(client), silent = TRUE)
    }
    stop(error)
  })
}

#' Test whether a Codex client process is alive
#'
#' @param client An `artcodex_client`.
#' @return A single logical value.
#' @export
codex_client_is_alive <- function(client) {
  if (!inherits(client, "artcodex_client") || is.null(client$process)) {
    return(FALSE)
  }
  !isTRUE(client$stopped) && isTRUE(client$process$is_alive())
}

#' Inspect a Codex client
#'
#' @param client An `artcodex_client`.
#' @return A serializable list describing process and server state.
#' @export
codex_client_info <- function(client) {
  artcodex_assert_client(client, require_alive = FALSE)
  list(
    initialized = isTRUE(client$initialized),
    alive = codex_client_is_alive(client),
    command = client$command,
    args = client$args,
    server = client$initialize_result,
    event_count = length(client$events),
    message_count = length(client$raw_messages),
    stderr = client$stderr
  )
}

#' Clear retained client events and protocol history
#'
#' @param client An `artcodex_client`.
#' @return The client, invisibly.
#' @export
codex_client_clear_history <- function(client) {
  artcodex_assert_client(client, require_alive = FALSE)
  client$raw_sent <- character()
  client$raw_received <- character()
  client$raw_messages <- list()
  client$events <- list()
  client$stderr <- character()
  invisible(client)
}

#' Stop a Codex client process
#'
#' @param client An `artcodex_client`.
#' @param timeout Seconds to allow for a clean exit before terminating it.
#' @return The client, invisibly.
#' @export
codex_client_stop <- function(client, timeout = 2) {
  artcodex_assert_timeout(timeout)
  artcodex_assert_client(client, require_alive = FALSE)
  process <- client$process
  if (is.null(process) || isTRUE(client$stopped)) {
    return(invisible(client))
  }

  if (isTRUE(process$is_alive())) {
    try(process$close_input(), silent = TRUE)
    try(process$wait(timeout = timeout * 1000), silent = TRUE)
  }
  if (isTRUE(process$is_alive())) {
    try(process$kill_tree(), silent = TRUE)
    try(process$wait(timeout = 1000), silent = TRUE)
  }
  if (isTRUE(process$is_alive())) {
    try(process$kill(), silent = TRUE)
  }
  client$stopped <- TRUE
  invisible(client)
}

#' @export
print.artcodex_client <- function(x, ...) {
  info <- codex_client_info(x)
  cat("<artcodex_client>", if (info$alive) "running" else "stopped", "\n")
  if (!is.null(info$server$userAgent)) {
    cat("  server:", info$server$userAgent, "\n")
  }
  cat("  events:", info$event_count, " messages:", info$message_count, "\n")
  invisible(x)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

artcodex_compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

artcodex_encode_json <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

artcodex_decode_json <- function(line) {
  jsonlite::fromJSON(line, simplifyVector = FALSE)
}

artcodex_empty_object <- function() {
  structure(list(), names = character())
}

artcodex_as_object <- function(x) {
  if (is.list(x) && length(x) == 0L) artcodex_empty_object() else x
}

artcodex_encode_key <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

artcodex_merge_params <- function(params, extra, reserved = names(params)) {
  if (is.null(extra)) {
    return(params)
  }
  duplicate <- intersect(reserved, names(extra))
  if (length(duplicate) > 0L) {
    artcodex_abort(
      sprintf(
        "extra cannot replace named arguments: %s.",
        paste(duplicate, collapse = ", ")
      ),
      "artcodex_validation_error"
    )
  }
  c(params, extra)
}

artcodex_monotonic_time <- function() {
  unname(proc.time()[["elapsed"]])
}

artcodex_deadline <- function(timeout) {
  artcodex_monotonic_time() + timeout
}

artcodex_remaining <- function(deadline) {
  max(0, deadline - artcodex_monotonic_time())
}

artcodex_utc_now <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

artcodex_abort <- function(message, class = "artcodex_error", ...) {
  classes <- unique(c(class, "artcodex_error", "error", "condition"))
  condition <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = classes
  )
  stop(condition)
}

artcodex_assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    artcodex_abort(
      sprintf("%s must be TRUE or FALSE.", name),
      "artcodex_validation_error"
    )
  }
  invisible(x)
}

artcodex_assert_timeout <- function(x, name = "timeout") {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
        !is.finite(x) || x <= 0) {
    artcodex_abort(
      sprintf("%s must be one finite positive number of seconds.", name),
      "artcodex_validation_error"
    )
  }
  invisible(as.numeric(x))
}

artcodex_assert_string <- function(x, name, allow_empty = FALSE) {
  valid <- is.character(x) && length(x) == 1L && !is.na(x)
  if (valid && !allow_empty) {
    valid <- nzchar(x)
  }
  if (!valid) {
    qualifier <- if (allow_empty) "a string" else "one non-empty string"
    artcodex_abort(
      sprintf("%s must be %s.", name, qualifier),
      "artcodex_validation_error"
    )
  }
  invisible(x)
}

artcodex_assert_optional_string <- function(x, name) {
  if (!is.null(x)) {
    artcodex_assert_string(x, name)
  }
  invisible(x)
}

artcodex_assert_function <- function(x, name) {
  if (!is.null(x) && !is.function(x)) {
    artcodex_abort(
      sprintf("%s must be a function or NULL.", name),
      "artcodex_validation_error"
    )
  }
  invisible(x)
}

artcodex_validate_env <- function(env) {
  invalid <- !is.null(env) && (
    !is.character(env) ||
      anyNA(env) ||
      is.null(names(env)) ||
      any(!nzchar(names(env)))
  )
  if (invalid) {
    artcodex_abort(
      "env must be a named character vector or NULL.",
      "artcodex_validation_error"
    )
  }
  invisible(env)
}

artcodex_process_env <- function(env) {
  if (is.null(env)) {
    return(NULL)
  }
  process_env <- Sys.getenv()
  process_env[names(env)] <- env
  process_env
}

artcodex_normalize_cwd <- function(cwd) {
  if (is.null(cwd)) {
    return(NULL)
  }
  artcodex_assert_string(cwd, "cwd")
  if (!dir.exists(cwd)) {
    artcodex_abort(
      sprintf("cwd does not exist or is not a directory: %s", cwd),
      "artcodex_validation_error"
    )
  }
  normalizePath(cwd, winslash = "/", mustWork = TRUE)
}

artcodex_validate_sandbox <- function(sandbox) {
  if (is.null(sandbox)) {
    return(NULL)
  }
  choices <- c("read-only", "workspace-write", "danger-full-access")
  artcodex_assert_string(sandbox, "sandbox")
  if (!sandbox %in% choices) {
    artcodex_abort(
      sprintf("sandbox must be one of: %s.", paste(choices, collapse = ", ")),
      "artcodex_validation_error"
    )
  }
  sandbox
}

artcodex_validate_approval_policy <- function(policy) {
  if (is.null(policy)) {
    return(policy)
  }
  if (is.list(policy)) {
    invalid_names <- is.null(names(policy)) || any(!nzchar(names(policy)))
    if (length(policy) > 0L && invalid_names) {
      artcodex_abort(
        "A granular approval_policy must be a named list.",
        "artcodex_validation_error"
      )
    }
    return(artcodex_as_object(policy))
  }
  choices <- c("untrusted", "on-failure", "on-request", "never")
  artcodex_assert_string(policy, "approval_policy")
  if (!policy %in% choices) {
    artcodex_abort(
      sprintf(
        "approval_policy must be a granular policy list or one of: %s.",
        paste(choices, collapse = ", ")
      ),
      "artcodex_validation_error"
    )
  }
  policy
}

artcodex_assert_client <- function(client, require_alive = TRUE) {
  if (!inherits(client, "artcodex_client")) {
    artcodex_abort(
      "client must be an artcodex_client.",
      "artcodex_validation_error"
    )
  }
  if (require_alive && !codex_client_is_alive(client)) {
    artcodex_abort(
      "The Codex app-server client is not running.",
      "artcodex_process_error"
    )
  }
  invisible(client)
}

artcodex_assert_thread <- function(thread) {
  if (!inherits(thread, "artcodex_thread")) {
    artcodex_abort(
      "thread must be an artcodex_thread.",
      "artcodex_validation_error"
    )
  }
  artcodex_assert_client(thread$client)
  invisible(thread)
}

artcodex_slice <- function(x, start) {
  if (length(x) < start) {
    if (is.character(x)) {
      return(character())
    }
    return(list())
  }
  x[start:length(x)]
}

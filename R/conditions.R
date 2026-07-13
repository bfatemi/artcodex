#' Test whether a result completed successfully
#'
#' @param x An `artcodex_result`.
#' @return A single logical value.
#' @export
codex_succeeded <- function(x) {
  inherits(x, "artcodex_result") && isTRUE(x$success)
}

artcodex_request_error_message <- function(error) {
  if (is.null(error)) {
    return("Unknown JSON-RPC error.")
  }
  message <- error$message %||% "Unknown JSON-RPC error."
  code <- error$code
  if (is.null(code)) message else sprintf("%s (code %s)", message, code)
}

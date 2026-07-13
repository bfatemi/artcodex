#' Generate the Codex app-server JSON Schema bundle
#'
#' The generated files can be used to inspect or validate the exact protocol
#' exposed by the installed Codex CLI.
#'
#' @param out Output directory for generated schema files.
#' @param command Optional path to the Codex executable.
#' @param command_args Optional arguments inserted before the app-server schema
#'   command. This is mainly useful with a custom launcher.
#' @param experimental If `TRUE`, include experimental methods and fields.
#' @param overwrite If `TRUE`, allow generated files to be written into an
#'   existing non-empty output directory.
#' @param timeout Maximum number of seconds to wait.
#' @return The normalized output directory path, invisibly.
#' @export
codex_generate_schema <- function(
  out = file.path(tempdir(), "artcodex-schema"),
  command = NULL,
  command_args = character(),
  experimental = TRUE,
  overwrite = FALSE,
  timeout = 60
) {
  artcodex_assert_string(out, "out")
  artcodex_assert_flag(experimental, "experimental")
  artcodex_assert_flag(overwrite, "overwrite")
  artcodex_assert_timeout(timeout)
  command <- command %||% artcodex_find_codex()
  artcodex_assert_string(command, "command")
  if (!is.character(command_args) || anyNA(command_args)) {
    artcodex_abort(
      "command_args must be a character vector.",
      "artcodex_validation_error"
    )
  }

  if (dir.exists(out) && length(list.files(out, all.files = TRUE)) > 0L) {
    if (!overwrite) {
      artcodex_abort(
        "out already exists and is not empty; set overwrite = TRUE to replace it.",
        "artcodex_validation_error"
      )
    }
  }
  if (!dir.create(out, recursive = TRUE, showWarnings = FALSE) &&
        !dir.exists(out)) {
    artcodex_abort(
      sprintf("Could not create schema output directory: %s", out),
      "artcodex_process_error"
    )
  }

  args <- c(
    command_args,
    "app-server", "generate-json-schema", "--out", out
  )
  if (experimental) {
    args <- c(args, "--experimental")
  }
  result <- tryCatch(
    processx::run(
      command,
      args,
      timeout = timeout * 1000,
      error_on_status = FALSE
    ),
    error = function(error) {
      artcodex_abort(
        sprintf("Could not generate app-server schema: %s", conditionMessage(error)),
        "artcodex_process_error"
      )
    }
  )
  if (!identical(result$status, 0L)) {
    artcodex_abort(
      sprintf(
        "Could not generate app-server schema: %s",
        trimws(paste(result$stderr, result$stdout))
      ),
      "artcodex_process_error",
      status = result$status
    )
  }
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}

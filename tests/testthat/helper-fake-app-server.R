fake_app_server_command <- function(scenario = "success") {
  command <- Sys.which("Rscript")
  if (!nzchar(command)) {
    stop("Rscript is required for deterministic app-server tests.")
  }
  fixture <- normalizePath(
    testthat::test_path("fixtures", "fake-app-server.R"),
    winslash = "/",
    mustWork = TRUE
  )
  list(
    command = unname(command),
    args = c("--vanilla", fixture, scenario)
  )
}

fake_client <- function(scenario = "success", ...) {
  launcher <- fake_app_server_command(scenario)
  codex_client_start(
    command = launcher$command,
    args = launcher$args,
    timeout = 5,
    poll_interval = 0.01,
    ...
  )
}

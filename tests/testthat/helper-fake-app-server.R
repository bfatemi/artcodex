fake_rscript_path <- function() {
  executable <- if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }
  normalizePath(
    file.path(R.home("bin"), executable),
    winslash = "/",
    mustWork = TRUE
  )
}

fake_app_server_command <- function(scenario = "success") {
  fixture <- normalizePath(
    testthat::test_path("fixtures", "fake-app-server.R"),
    winslash = "/",
    mustWork = TRUE
  )
  list(
    command = fake_rscript_path(),
    args = c("--vanilla", fixture, scenario),
    env = c(R_TESTS = "")
  )
}

fake_client <- function(scenario = "success", ...) {
  launcher <- fake_app_server_command(scenario)
  codex_client_start(
    command = launcher$command,
    args = launcher$args,
    env = launcher$env,
    timeout = 5,
    poll_interval = 0.01,
    ...
  )
}

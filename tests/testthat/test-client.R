test_that("client lifecycle exposes health and serializable information", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)

  expect_s3_class(client, "artcodex_client")
  expect_true(codex_client_is_alive(client))
  expect_output(print(client), "running")

  info <- codex_client_info(client)
  expect_true(info$initialized)
  expect_true(info$alive)
  expect_equal(info$server$userAgent, "fake-codex/1.0.0")
  expect_gt(info$message_count, 0L)

  codex_client_clear_history(client)
  expect_equal(codex_client_info(client)$message_count, 0L)
  expect_length(client$events, 0L)

  codex_client_stop(client)
  expect_false(codex_client_is_alive(client))
  expect_output(print(client), "stopped")
  expect_invisible(codex_client_stop(client))
})

test_that("client validates launch configuration", {
  launcher <- fake_app_server_command()

  expect_error(
    codex_client_start(
      command = launcher$command,
      args = launcher$args,
      capabilities = "invalid"
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_client_start(
      command = launcher$command,
      args = 1
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_client_start(
      command = launcher$command,
      args = launcher$args,
      request_handler = "invalid"
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_client_start(
      command = launcher$command,
      args = launcher$args,
      env = list(PATH = "invalid")
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_client_start(
      command = launcher$command,
      args = launcher$args,
      env = "invalid"
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_client_start(command = tempfile("missing-command-")),
    class = "artcodex_process_error"
  )
  expect_error(
    fake_client("init-error"),
    "simulated initialize failure",
    class = "artcodex_request_error"
  )
})

test_that("client stop closes an alive process before terminating it", {
  state <- new.env(parent = emptyenv())
  state$alive <- TRUE
  state$closed <- FALSE
  state$killed <- FALSE

  process <- list(
    is_alive = function() state$alive,
    close_input = function() {
      state$closed <- TRUE
      state$alive <- FALSE
    },
    wait = function(timeout) invisible(timeout),
    kill_tree = function() {
      state$killed <- TRUE
      state$alive <- FALSE
    },
    kill = function() {
      state$killed <- TRUE
      state$alive <- FALSE
    }
  )
  client <- new.env(parent = emptyenv())
  client$process <- process
  client$stopped <- FALSE
  class(client) <- "artcodex_client"

  codex_client_stop(client)

  expect_true(state$closed)
  expect_false(state$killed)
})

test_that("client stop terminates a process that ignores input closure", {
  state <- new.env(parent = emptyenv())
  state$alive <- TRUE
  state$tree_killed <- FALSE
  process <- list(
    is_alive = function() state$alive,
    close_input = function() invisible(TRUE),
    wait = function(timeout) invisible(timeout),
    kill_tree = function() {
      state$tree_killed <- TRUE
      state$alive <- FALSE
    },
    kill = function() state$alive <- FALSE
  )
  client <- new.env(parent = emptyenv())
  client$process <- process
  client$stopped <- FALSE
  class(client) <- "artcodex_client"

  codex_client_stop(client, timeout = 0.01)

  expect_true(state$tree_killed)
  expect_false(state$alive)
})

test_that("Codex executable override is validated", {
  old <- Sys.getenv("ARTCODEX_CODEX_PATH", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("ARTCODEX_CODEX_PATH")
    } else {
      Sys.setenv(ARTCODEX_CODEX_PATH = old)
    }
  }, add = TRUE)
  Sys.setenv(ARTCODEX_CODEX_PATH = tempfile("missing-codex-"))

  expect_error(codex_version(), class = "artcodex_validation_error")
})

test_that("Codex version command returns a trimmed version string", {
  version <- codex_version(command = fake_rscript_path(), timeout = 5)

  expect_match(version, "Rscript (R) version", fixed = TRUE)
  expect_error(
    codex_version(command = tempfile("missing-command-")),
    class = "artcodex_process_error"
  )
})

test_that("invalid client and thread objects fail with package conditions", {
  expect_false(codex_client_is_alive(list()))
  expect_error(
    codex_client_info(list()),
    class = "artcodex_validation_error"
  )
  expect_error(
    artcodex:::artcodex_assert_thread(list()),
    class = "artcodex_validation_error"
  )
})

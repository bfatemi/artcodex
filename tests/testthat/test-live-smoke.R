test_that("optional live Codex smoke test can complete one read-only turn", {
  skip_if_not(
    identical(Sys.getenv("ARTCODEX_LIVE_TESTS"), "1"),
    "Set ARTCODEX_LIVE_TESTS=1 to run the live Codex smoke test."
  )

  client <- codex_client_start(timeout = 30)
  on.exit(codex_client_stop(client), add = TRUE)

  model <- Sys.getenv("ARTCODEX_LIVE_MODEL", unset = "")
  effort <- Sys.getenv("ARTCODEX_LIVE_REASONING_EFFORT", unset = "")
  config <- if (nzchar(effort)) {
    list(model_reasoning_effort = effort)
  } else {
    NULL
  }

  thread <- codex_thread_start(
    client,
    cwd = getwd(),
    model = if (nzchar(model)) model else NULL,
    sandbox = "read-only",
    approval_policy = "never",
    config = config,
    timeout = 30
  )

  result <- codex_run(
    "Reply with exactly: artcodex live smoke ok",
    thread = thread,
    timeout = 180
  )

  expect_true(codex_succeeded(result))
  expect_match(result$final_response, "artcodex live smoke ok", fixed = TRUE)
})

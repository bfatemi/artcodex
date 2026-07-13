test_that("JSON, list, timing, and slice helpers are deterministic", {
  compact <- artcodex:::artcodex_compact_list(list(a = 1, b = NULL, c = FALSE))
  encoded <- artcodex:::artcodex_encode_json(compact)
  decoded <- artcodex:::artcodex_decode_json(encoded)
  deadline <- artcodex:::artcodex_deadline(1)

  expect_named(compact, c("a", "c"))
  expect_equal(decoded$a, 1)
  expect_false(decoded$c)
  expect_gt(artcodex:::artcodex_remaining(deadline), 0)
  expect_s3_class(artcodex:::artcodex_utc_now(), "POSIXct")
  expect_equal(artcodex:::artcodex_slice(list(1, 2, 3), 2), list(2, 3))
  expect_equal(artcodex:::artcodex_slice(character(), 1), character())
  expect_match(
    artcodex:::artcodex_encode_json(artcodex:::artcodex_empty_object()),
    "{}",
    fixed = TRUE
  )
})

test_that("parameter merging rejects collisions", {
  expect_equal(
    artcodex:::artcodex_merge_params(list(a = 1), list(b = 2)),
    list(a = 1, b = 2)
  )
  expect_equal(
    artcodex:::artcodex_merge_params(list(a = 1), NULL),
    list(a = 1)
  )
  expect_error(
    artcodex:::artcodex_merge_params(list(a = 1), list(a = 2)),
    class = "artcodex_validation_error"
  )
})

test_that("scalar validators accept valid values and reject invalid values", {
  expect_invisible(artcodex:::artcodex_assert_flag(TRUE, "flag"))
  expect_error(
    artcodex:::artcodex_assert_flag(NA, "flag"),
    class = "artcodex_validation_error"
  )
  expect_invisible(artcodex:::artcodex_assert_timeout(0.1))
  expect_error(
    artcodex:::artcodex_assert_timeout(0),
    class = "artcodex_validation_error"
  )
  expect_invisible(artcodex:::artcodex_assert_string("", "text", TRUE))
  expect_error(
    artcodex:::artcodex_assert_string("", "text"),
    class = "artcodex_validation_error"
  )
  expect_invisible(artcodex:::artcodex_assert_optional_string(NULL, "text"))
  expect_error(
    artcodex:::artcodex_assert_optional_string(1, "text"),
    class = "artcodex_validation_error"
  )
  expect_invisible(artcodex:::artcodex_assert_function(identity, "callback"))
  expect_error(
    artcodex:::artcodex_assert_function(1, "callback"),
    class = "artcodex_validation_error"
  )
})

test_that("working directory and policy validators normalize known values", {
  normalized <- artcodex:::artcodex_normalize_cwd(tempdir())

  expect_true(dir.exists(normalized))
  expect_null(artcodex:::artcodex_normalize_cwd(NULL))
  expect_error(
    artcodex:::artcodex_normalize_cwd(tempfile("missing-dir-")),
    class = "artcodex_validation_error"
  )
  expect_equal(
    artcodex:::artcodex_validate_sandbox("workspace-write"),
    "workspace-write"
  )
  expect_null(artcodex:::artcodex_validate_sandbox(NULL))
  expect_error(
    artcodex:::artcodex_validate_sandbox("unknown"),
    class = "artcodex_validation_error"
  )
  granular <- list(reject = list(sandboxApproval = TRUE))
  expect_identical(
    artcodex:::artcodex_validate_approval_policy(granular),
    granular
  )
  expect_equal(
    artcodex:::artcodex_validate_approval_policy("never"),
    "never"
  )
  expect_error(
    artcodex:::artcodex_validate_approval_policy("always"),
    class = "artcodex_validation_error"
  )
})

test_that("child process environment values are merged as overrides", {
  env <- artcodex:::artcodex_process_env(c(ARTCODEX_FIXTURE = "yes"))

  expect_equal(env[["ARTCODEX_FIXTURE"]], "yes")
  expect_equal(env[["PATH"]], Sys.getenv("PATH"))
  expect_null(artcodex:::artcodex_process_env(NULL))
})

test_that("public success and error helpers are status aware", {
  result <- structure(list(success = TRUE), class = "artcodex_result")

  expect_true(codex_succeeded(result))
  expect_false(codex_succeeded(list(success = TRUE)))
  expect_equal(
    artcodex:::artcodex_request_error_message(NULL),
    "Unknown JSON-RPC error."
  )
  expect_equal(
    artcodex:::artcodex_request_error_message(list(message = "bad", code = 5)),
    "bad (code 5)"
  )
  expect_equal(
    artcodex:::artcodex_request_error_message(list(message = "bad")),
    "bad"
  )
})

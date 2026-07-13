test_that("thread lifecycle starts, resumes, and archives state", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)

  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)
  resumed <- codex_thread_resume(
    client,
    thread$thread_id,
    cwd = tempdir(),
    timeout = 2
  )

  expect_s3_class(thread, "artcodex_thread")
  expect_equal(thread$thread_id, "thread_fake")
  expect_equal(resumed$thread_id, "thread_fake")
  expect_output(print(thread), "thread_fake")
  expect_invisible(codex_thread_archive(thread, timeout = 2))
  expect_invisible(codex_turn_interrupt(thread, "turn_arbitrary", timeout = 2))
})

test_that("a successful turn returns a complete serializable record", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  methods <- character()
  result <- codex_run(
    "Say hello.",
    thread = thread,
    timeout = 2,
    on_event = function(event) methods <<- c(methods, event$method)
  )

  expect_s3_class(result, "artcodex_result")
  expect_true(codex_succeeded(result))
  expect_equal(result$status, "completed")
  expect_equal(result$thread_id, "thread_fake")
  expect_equal(result$turn_id, "turn_fake")
  expect_equal(result$final_response, "hello from fake codex")
  expect_equal(as.character(result), "hello from fake codex")
  expect_equal(result$usage$total$totalTokens, 7)
  expect_equal(result$transcript[[1L]]$role, "user")
  expect_equal(result$transcript[[2L]]$role, "assistant")
  expect_equal(result$transcript[[2L]]$content, "hello from fake codex")
  expect_gt(length(result$events), 0L)
  expect_gt(length(result$raw_messages), 0L)
  expect_equal(length(result$raw_messages), length(result$raw_jsonl))
  expect_type(result$stderr, "character")
  expect_false("thread/started" %in% methods)
  expect_null(result$error)
  expect_output(print(result), "hello from fake codex")
  expect_silent(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null"))
})

test_that("one-call mode starts and cleans up its own client", {
  launcher <- fake_app_server_command()

  result <- codex_run(
    "Say hello.",
    cwd = tempdir(),
    command = launcher$command,
    client_args = launcher$args,
    client_env = launcher$env,
    timeout = 2,
    client_timeout = 2
  )

  expect_true(codex_succeeded(result))
  expect_equal(result$final_response, "hello from fake codex")
  expect_false("client" %in% names(result))

  expect_error(
    codex_run(
      "",
      command = tempfile("missing-command-"),
      client_args = character()
    ),
    class = "artcodex_validation_error"
  )
})

test_that("automatically created threads are ephemeral and conservative", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)

  result <- codex_run(
    "Defaults.",
    client = client,
    config = list(),
    base_instructions = "Base instructions.",
    developer_instructions = "Developer instructions.",
    timeout = 2
  )
  thread_request <- Filter(
    function(record) identical(record$message$method, "thread/start"),
    client$raw_messages
  )[[1L]]$message

  expect_true(codex_succeeded(result))
  expect_true(thread_request$params$ephemeral)
  expect_equal(thread_request$params$sandbox, "read-only")
  expect_equal(thread_request$params$approvalPolicy, "never")
  expect_equal(thread_request$params$baseInstructions, "Base instructions.")
  expect_equal(
    thread_request$params$developerInstructions,
    "Developer instructions."
  )
  expect_match(
    artcodex:::artcodex_encode_json(thread_request$params),
    '"config":{}',
    fixed = TRUE
  )
})

test_that("the original thread-first call shape remains supported", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run(thread, "Say hello.", timeout = 2)

  expect_true(codex_succeeded(result))
  expect_equal(result$final_response, "hello from fake codex")
})

test_that("structured output is parsed and malformed output fails safely", {
  schema <- list(
    type = "object",
    required = list("summary", "count"),
    properties = list(
      summary = list(type = "string"),
      count = list(type = "integer")
    )
  )

  client <- fake_client("structured")
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)
  result <- codex_run(
    "Return JSON.",
    thread = thread,
    output_schema = schema,
    timeout = 2
  )
  expect_true(codex_succeeded(result))
  expect_equal(result$output$summary, "ready")
  expect_equal(result$output$count, 2)

  invalid_client <- fake_client("invalid-structured")
  on.exit(codex_client_stop(invalid_client), add = TRUE)
  invalid_thread <- codex_thread_start(
    invalid_client,
    cwd = tempdir(),
    timeout = 2
  )
  invalid <- codex_run(
    "Return JSON.",
    thread = invalid_thread,
    output_schema = schema,
    timeout = 2
  )
  expect_false(codex_succeeded(invalid))
  expect_equal(invalid$status, "completed")
  expect_equal(invalid$error$type, "output_parse_error")
})

test_that("failed turns preserve server error details", {
  client <- fake_client("failed")
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run("Fail.", thread = thread, timeout = 2)

  expect_false(codex_succeeded(result))
  expect_equal(result$status, "failed")
  expect_match(result$error$message, "simulated turn failure")
  expect_error(
    codex_run("Fail again.", thread = thread, timeout = 2, stop_on_error = TRUE),
    "simulated turn failure",
    class = "artcodex_turn_error"
  )
})

test_that("approval requests fail closed or use a caller handler", {
  declined_client <- fake_client("approval")
  on.exit(codex_client_stop(declined_client), add = TRUE)
  declined_thread <- codex_thread_start(
    declined_client,
    cwd = tempdir(),
    timeout = 2
  )
  declined <- codex_run("Ask approval.", thread = declined_thread, timeout = 2)

  expect_equal(declined$final_response, "approval: decline")
  expect_length(declined$approvals, 1L)
  expect_equal(
    declined$approvals[[1L]]$response$result$decision,
    "decline"
  )

  accepted_client <- fake_client("approval")
  on.exit(codex_client_stop(accepted_client), add = TRUE)
  accepted_thread <- codex_thread_start(
    accepted_client,
    cwd = tempdir(),
    timeout = 2
  )
  accepted <- codex_run(
    "Ask approval.",
    thread = accepted_thread,
    timeout = 2,
    request_handler = function(event) {
      if (grepl("requestApproval", event$method, fixed = TRUE)) {
        return(list(decision = "accept"))
      }
      NULL
    }
  )

  expect_equal(accepted$final_response, "approval: accept")
})

test_that("a failing server-request handler returns a protocol error response", {
  client <- fake_client("approval")
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run(
    "Ask approval.",
    thread = thread,
    timeout = 2,
    request_handler = function(event) stop(event$method)
  )

  expect_true(codex_succeeded(result))
  expect_equal(result$final_response, "approval: error")
  sent_errors <- Filter(
    function(record) !is.null(record$message$error),
    result$raw_messages
  )
  expect_length(sent_errors, 1L)
  expect_match(sent_errors[[1L]]$message$error$message, "handler failed")
})

test_that("dynamic client tools can be answered by an R callback", {
  client <- fake_client("dynamic-tool")
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run(
    "Call the fixture tool.",
    thread = thread,
    timeout = 2,
    request_handler = function(event) {
      if (identical(event$method, "item/tool/call")) {
        return(list(success = TRUE, contentItems = list()))
      }
      NULL
    }
  )

  expect_true(codex_succeeded(result))
  expect_equal(result$final_response, "dynamic tool success: true")
  expect_true(any(vapply(result$events, `[[`, character(1), "method") == "item/tool/call"))
})

test_that("turn timeout sends an interruption request", {
  client <- fake_client("hang")
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run("Wait forever.", thread = thread, timeout = 0.2)

  expect_false(codex_succeeded(result))
  expect_equal(result$status, "interrupted")
  expect_equal(result$error$type, "artcodex_timeout_error")
  sent_methods <- vapply(
    Filter(function(record) identical(record$direction, "sent"), result$raw_messages),
    function(record) {
      if (is.null(record$message$method)) "" else record$message$method
    },
    character(1)
  )
  expect_true("turn/interrupt" %in% sent_methods)
})

test_that("buffered completion survives an immediate process exit", {
  client <- fake_client("exit-buffer")
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run("Exit after replying.", thread = thread, timeout = 2)

  expect_true(codex_succeeded(result))
  expect_equal(result$final_response, "buffer survived process exit")
})

test_that("local images use the current app-server input shape", {
  image <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), image)
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  result <- codex_run(
    "Inspect this image.",
    thread = thread,
    images = image,
    timeout = 2
  )
  turn_request <- Filter(
    function(record) identical(record$message$method, "turn/start"),
    result$raw_messages
  )[[1L]]$message

  expect_equal(turn_request$params$input[[2L]]$type, "localImage")
  expect_equal(
    turn_request$params$input[[2L]]$path,
    normalizePath(image, winslash = "/")
  )
  expect_error(
    codex_run("Missing.", thread = thread, images = tempfile(), timeout = 2),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Bad images.", thread = thread, images = 1, timeout = 2),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Directory.", thread = thread, images = tempdir(), timeout = 2),
    class = "artcodex_validation_error"
  )
})

test_that("thread and turn arguments are validated before execution", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)

  expect_error(
    codex_thread_start(client, sandbox = "unbounded"),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_thread_resume(client, ""),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_thread_start(client, config = "invalid"),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_thread_start(client, config = list("invalid")),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_thread_start(client, extra = list(tempdir())),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_thread_start(client, cwd = tempdir(), extra = list(cwd = "other")),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Prompt", client = client, thread = list()),
    "Supply thread or client",
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Prompt", client = client, command = "codex"),
    class = "artcodex_validation_error"
  )
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)
  expect_error(
    codex_run("Prompt", thread = thread, output_schema = "invalid"),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Prompt", thread = thread, output_schema = list("invalid")),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Prompt", thread = thread, config = list(test = TRUE)),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run(
      "Prompt",
      thread = thread,
      developer_instructions = "Not turn-scoped."
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Prompt", thread = thread, sandbox = "workspace-write"),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run("Prompt", thread = thread, ephemeral = FALSE),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_run(
      "Prompt",
      thread = thread,
      approval_policy = list("invalid")
    ),
    class = "artcodex_validation_error"
  )
})

test_that("progress and callback failures are observable", {
  client <- fake_client()
  on.exit(codex_client_stop(client), add = TRUE)
  thread <- codex_thread_start(client, cwd = tempdir(), timeout = 2)

  messages <- testthat::capture_messages(
    result <- codex_run("Progress.", thread = thread, timeout = 2, progress = TRUE)
  )
  expect_true(codex_succeeded(result))
  expect_true(any(grepl("turn/started", messages, fixed = TRUE)))

  callback_failure <- codex_run(
    "Callback failure.",
    thread = thread,
    timeout = 2,
    on_event = function(event) stop(event$method)
  )
  expect_false(codex_succeeded(callback_failure))
  expect_equal(callback_failure$error$type, "artcodex_callback_error")
})

test_that("item normalization identifies tools and approval events", {
  items <- list(
    list(id = "message", type = "agentMessage", text = "done"),
    list(id = "command", type = "commandExecution", command = "echo ok")
  )
  approvals <- list(
    list(
      server_request = TRUE,
      method = "item/fileChange/requestApproval"
    ),
    list(server_request = FALSE, method = "item/completed")
  )

  expect_equal(
    artcodex:::artcodex_extract_tool_calls(items)[[1L]]$id,
    "command"
  )
  expect_length(artcodex:::artcodex_extract_approvals(approvals), 1L)
})

test_that("turn event filtering excludes lifecycle and unrelated events", {
  matching <- list(
    method = "item/completed",
    params = list(threadId = "thread_1", turnId = "turn_1")
  )
  global <- list(method = "warning", params = list(message = "notice"))
  lifecycle <- list(
    method = "thread/started",
    params = list(thread = list(id = "thread_1"))
  )
  other <- list(
    method = "turn/completed",
    params = list(
      threadId = "thread_2",
      turn = list(id = "turn_2")
    )
  )

  expect_true(
    artcodex:::artcodex_event_matches_turn(matching, "thread_1", "turn_1")
  )
  expect_true(
    artcodex:::artcodex_event_matches_turn(global, "thread_1", "turn_1")
  )
  expect_false(
    artcodex:::artcodex_event_matches_turn(lifecycle, "thread_1", "turn_1")
  )
  expect_false(
    artcodex:::artcodex_event_matches_turn(other, "thread_1", "turn_1")
  )
})

test_that("JSON-RPC message constructors use app-server framing", {
  request <- artcodex:::artcodex_make_request(
    id = "7",
    method = "turn/start",
    params = list(threadId = "thread_1")
  )
  notification <- artcodex:::artcodex_make_notification("initialized")
  response <- artcodex:::artcodex_make_response("8", list(ok = TRUE))
  error <- artcodex:::artcodex_make_error_response("9", "no", -1)
  numeric_response <- artcodex:::artcodex_make_response(10, NULL)
  empty_request <- artcodex:::artcodex_make_request("11", "empty")

  expect_equal(request$id, "7")
  expect_equal(request$method, "turn/start")
  expect_equal(request$params$threadId, "thread_1")
  expect_null(request$jsonrpc)
  expect_equal(notification, list(method = "initialized"))
  expect_true(response$result$ok)
  expect_equal(error$error$code, -1)
  expect_identical(numeric_response$id, 10)
  expect_true(artcodex:::artcodex_is_response(numeric_response))
  expect_match(
    artcodex:::artcodex_encode_json(empty_request),
    '"params":{}',
    fixed = TRUE
  )
})

test_that("responses that arrive out of order remain available", {
  client <- fake_client("out-of-order")
  on.exit(codex_client_stop(client), add = TRUE)

  cached <- artcodex:::artcodex_wait_for_response(client, "999", timeout = 1)

  expect_equal(cached$marker, "cached")
})

test_that("event parsing prefers final messages and falls back to deltas", {
  events <- list(
    list(
      method = "item/completed",
      params = list(item = list(
        id = "one",
        type = "agentMessage",
        text = "progress",
        phase = "commentary"
      ))
    ),
    list(
      method = "item/completed",
      params = list(item = list(
        id = "two",
        type = "agentMessage",
        text = "final answer",
        phase = "final_answer"
      ))
    )
  )
  deltas <- list(
    list(method = "item/agentMessage/delta", params = list(delta = "one ")),
    list(method = "item/agentMessage/delta", params = list(delta = "two"))
  )

  expect_equal(
    artcodex:::artcodex_extract_final_response(events),
    "final answer"
  )
  expect_equal(
    artcodex:::artcodex_extract_final_response(deltas),
    "one two"
  )
})

test_that("default server-request responses fail closed", {
  approval <- artcodex:::artcodex_default_request_result(
    "item/commandExecution/requestApproval"
  )
  elicitation <- artcodex:::artcodex_default_request_result(
    "mcpServer/elicitation/request"
  )
  tool <- artcodex:::artcodex_default_request_result("item/tool/call")
  unknown <- artcodex:::artcodex_default_request_result("unknown/request")
  user_input <- artcodex:::artcodex_default_request_result(
    "item/tool/requestUserInput"
  )

  expect_equal(approval$result$decision, "decline")
  expect_equal(elicitation$result$action, "decline")
  expect_false(tool$result$success)
  expect_equal(unknown$error$code, -32601)
  expect_match(
    artcodex:::artcodex_encode_json(user_input),
    '"answers":{}',
    fixed = TRUE
  )
})

test_that("transport reports invalid JSON, request errors, and process exits", {
  invalid <- fake_client("invalid-json")
  on.exit(codex_client_stop(invalid), add = TRUE)
  expect_error(
    codex_thread_start(invalid, timeout = 2),
    class = "artcodex_protocol_error"
  )

  rejected <- fake_client("request-error")
  on.exit(codex_client_stop(rejected), add = TRUE)
  expect_error(
    codex_thread_start(rejected, timeout = 2),
    "simulated request error",
    class = "artcodex_request_error"
  )

  crashed <- fake_client("crash")
  on.exit(codex_client_stop(crashed), add = TRUE)
  expect_error(
    codex_thread_start(crashed, timeout = 2),
    "simulated app-server crash",
    class = "artcodex_process_error"
  )
})

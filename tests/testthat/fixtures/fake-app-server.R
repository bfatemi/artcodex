suppressPackageStartupMessages(library(jsonlite))

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

arguments <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(arguments) > 0L) arguments[[1L]] else "success"

if ("generate-json-schema" %in% arguments) {
  if (identical(scenario, "schema-fail")) {
    cat("simulated schema failure\n", file = stderr())
    quit(save = "no", status = 4L)
  }
  out_index <- match("--out", arguments) + 1L
  out <- arguments[[out_index]]
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    toJSON(
      list(
        title = "FakeProtocol",
        experimental = "--experimental" %in% arguments
      ),
      auto_unbox = TRUE
    ),
    file.path(out, "fake-protocol.json")
  )
  quit(save = "no", status = 0L)
}

emit <- function(message) {
  cat(
    toJSON(message, auto_unbox = TRUE, null = "null", digits = NA),
    "\n",
    sep = ""
  )
  flush(stdout())
}

thread_id <- "thread_fake"
turn_id <- "turn_fake"
waiting_for <- NULL
input <- file("stdin", open = "r")
on.exit(close(input), add = TRUE)

thread_payload <- function() {
  list(
    id = thread_id,
    turns = list(),
    preview = "",
    cliVersion = "fake-1.0.0",
    createdAt = 0,
    ephemeral = FALSE,
    modelProvider = "fake"
  )
}

turn_payload <- function(status, items = list(), error = NULL) {
  payload <- list(id = turn_id, status = status, items = items)
  if (!is.null(error)) {
    payload$error <- error
  }
  payload
}

complete_turn <- function(text = "hello from fake codex", status = "completed") {
  item <- list(
    id = "message_fake",
    type = "agentMessage",
    text = text,
    phase = "final_answer"
  )
  if (identical(status, "completed")) {
    emit(list(
      method = "item/agentMessage/delta",
      params = list(
        threadId = thread_id,
        turnId = turn_id,
        itemId = item$id,
        delta = text
      )
    ))
    emit(list(
      method = "item/completed",
      params = list(
        threadId = thread_id,
        turnId = turn_id,
        completedAtMs = 1,
        item = item
      )
    ))
    emit(list(
      method = "thread/tokenUsage/updated",
      params = list(
        threadId = thread_id,
        turnId = turn_id,
        tokenUsage = list(
          total = list(inputTokens = 3, outputTokens = 4, totalTokens = 7),
          last = list(inputTokens = 3, outputTokens = 4, totalTokens = 7),
          modelContextWindow = 1000
        )
      )
    ))
    turn <- turn_payload(status, list(item))
  } else {
    turn <- turn_payload(status)
  }
  emit(list(
    method = "turn/completed",
    params = list(threadId = thread_id, turn = turn)
  ))
}

fail_turn <- function() {
  turn <- turn_payload(
    "failed",
    error = list(message = "simulated turn failure", additionalDetails = "fixture")
  )
  emit(list(
    method = "turn/completed",
    params = list(threadId = thread_id, turn = turn)
  ))
}

repeat {
  line <- readLines(input, n = 1L, warn = FALSE)
  if (length(line) == 0L) {
    break
  }
  if (!nzchar(line)) {
    next
  }
  message <- fromJSON(line, simplifyVector = FALSE)
  method <- message$method

  if (is.null(method) && identical(as.character(message$id), "approval_fake")) {
    decision <- message$result$decision %||% "error"
    complete_turn(sprintf("approval: %s", decision))
    waiting_for <- NULL
    next
  }
  if (is.null(method) && identical(as.character(message$id), "tool_fake")) {
    success <- isTRUE(message$result$success)
    complete_turn(sprintf("dynamic tool success: %s", tolower(as.character(success))))
    waiting_for <- NULL
    next
  }

  if (identical(method, "initialize")) {
    if (identical(scenario, "init-error")) {
      emit(list(
        id = message$id,
        error = list(code = -32001, message = "simulated initialize failure")
      ))
      next
    }
    if (identical(scenario, "out-of-order")) {
      emit(list(id = "999", result = list(marker = "cached")))
    }
    emit(list(
      id = message$id,
      result = list(
        codexHome = tempdir(),
        platformFamily = .Platform$OS.type,
        platformOs = Sys.info()[["sysname"]],
        userAgent = "fake-codex/1.0.0"
      )
    ))
  } else if (identical(method, "initialized")) {
    next
  } else if (identical(method, "thread/start")) {
    if (identical(scenario, "crash")) {
      cat("simulated app-server crash\n", file = stderr())
      flush(stderr())
      quit(save = "no", status = 7L)
    }
    if (identical(scenario, "invalid-json")) {
      cat("{not-json\n")
      flush(stdout())
      next
    }
    if (identical(scenario, "request-error")) {
      emit(list(
        id = message$id,
        error = list(code = -32000, message = "simulated request error")
      ))
      next
    }
    emit(list(
      method = "thread/started",
      params = list(thread = thread_payload())
    ))
    emit(list(
      id = message$id,
      result = list(
        thread = thread_payload(),
        cwd = message$params$cwd,
        model = message$params$model,
        approvalPolicy = message$params$approvalPolicy,
        sandbox = list(mode = message$params$sandbox)
      )
    ))
  } else if (identical(method, "thread/resume")) {
    thread_id <- message$params$threadId
    emit(list(id = message$id, result = list(thread = thread_payload())))
  } else if (identical(method, "thread/archive")) {
    emit(list(id = message$id, result = NULL))
  } else if (identical(method, "turn/start")) {
    emit(list(
      id = message$id,
      result = list(turn = turn_payload("inProgress"))
    ))
    emit(list(
      method = "turn/started",
      params = list(
        threadId = thread_id,
        turn = turn_payload("inProgress")
      )
    ))

    if (identical(scenario, "approval")) {
      waiting_for <- "approval"
      emit(list(
        id = "approval_fake",
        method = "item/commandExecution/requestApproval",
        params = list(
          threadId = thread_id,
          turnId = turn_id,
          itemId = "command_fake",
          command = "echo test"
        )
      ))
    } else if (identical(scenario, "dynamic-tool")) {
      waiting_for <- "tool"
      emit(list(
        id = "tool_fake",
        method = "item/tool/call",
        params = list(
          threadId = thread_id,
          turnId = turn_id,
          callId = "call_fake",
          tool = "fixture_tool",
          arguments = list(value = 1)
        )
      ))
    } else if (identical(scenario, "failed")) {
      fail_turn()
    } else if (identical(scenario, "structured")) {
      complete_turn('{"summary":"ready","count":2}')
    } else if (identical(scenario, "invalid-structured")) {
      complete_turn("not json")
    } else if (identical(scenario, "hang")) {
      next
    } else if (identical(scenario, "exit-buffer")) {
      complete_turn("buffer survived process exit")
      quit(save = "no", status = 0L)
    } else {
      complete_turn()
    }
  } else if (identical(method, "turn/interrupt")) {
    complete_turn(status = "interrupted")
    emit(list(id = message$id, result = list()))
  } else {
    emit(list(
      id = message$id,
      error = list(code = -32601, message = paste("unknown method", method))
    ))
  }
}

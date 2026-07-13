artcodex_agent_messages <- function(events, turn = NULL) {
  items <- if (is.null(turn$items)) list() else turn$items
  for (event in events) {
    if (identical(event$method, "item/completed") &&
          identical(event$params$item$type, "agentMessage")) {
      items <- c(items, list(event$params$item))
    }
  }

  messages <- Filter(
    function(item) identical(item$type, "agentMessage"),
    items
  )
  if (length(messages) == 0L) {
    return(list())
  }

  ids <- vapply(
    messages,
    function(item) item$id %||% paste0("anonymous-", item$text %||% ""),
    character(1)
  )
  messages[!duplicated(ids, fromLast = TRUE)]
}

artcodex_extract_final_response <- function(events, turn = NULL) {
  messages <- artcodex_agent_messages(events, turn)
  if (length(messages) > 0L) {
    final <- Filter(
      function(item) identical(item$phase, "final_answer"),
      messages
    )
    source <- if (length(final) > 0L) final else messages
    return(utils::tail(
      vapply(source, function(item) item$text %||% "", character(1)),
      1L
    ))
  }

  deltas <- vapply(
    Filter(
      function(event) identical(event$method, "item/agentMessage/delta"),
      events
    ),
    function(event) event$params$delta %||% "",
    character(1)
  )
  paste(deltas, collapse = "")
}

artcodex_extract_items <- function(events, turn = NULL) {
  items <- if (is.null(turn$items)) list() else turn$items
  completed <- lapply(
    Filter(
      function(event) identical(event$method, "item/completed"),
      events
    ),
    function(event) event$params$item
  )
  completed <- Filter(Negate(is.null), completed)
  items <- c(items, completed)
  if (length(items) == 0L) {
    return(list())
  }
  ids <- vapply(
    items,
    function(item) item$id %||% artcodex_encode_key(item),
    character(1)
  )
  items[!duplicated(ids, fromLast = TRUE)]
}

artcodex_extract_tool_calls <- function(items) {
  non_tools <- c("agentMessage", "userMessage", "reasoning", "plan")
  Filter(
    function(item) !is.null(item$type) && !item$type %in% non_tools,
    items
  )
}

artcodex_extract_approvals <- function(events, raw_messages = list()) {
  approvals <- Filter(
    function(event) {
      approval <- grepl("approval", event$method, ignore.case = TRUE)
      elicitation <- identical(
        event$method,
        "mcpServer/elicitation/request"
      )
      isTRUE(event$server_request) && (approval || elicitation)
    },
    events
  )
  lapply(approvals, function(approval) {
    responses <- Filter(
      function(record) {
        identical(record$direction, "sent") &&
          artcodex_is_response(record$message, approval$id)
      },
      raw_messages
    )
    if (length(responses) > 0L) {
      approval$response <- utils::tail(responses, 1L)[[1L]]$message
    }
    approval
  })
}

artcodex_extract_usage <- function(events, thread_id, turn_id) {
  matching <- Filter(
    function(event) {
      identical(event$method, "thread/tokenUsage/updated") &&
        identical(event$params$threadId, thread_id) &&
        (is.null(turn_id) || identical(event$params$turnId, turn_id))
    },
    events
  )
  if (length(matching) == 0L) {
    return(NULL)
  }
  utils::tail(matching, 1L)[[1L]]$params$tokenUsage
}

artcodex_event_matches_turn <- function(event, thread_id, turn_id) {
  if (identical(event$method, "thread/started")) {
    return(FALSE)
  }
  params <- event$params
  event_thread <- params$threadId %||% params$thread$id
  event_turn <- params$turnId %||% params$turn$id
  if (!is.null(event_thread) && !identical(event_thread, thread_id)) {
    return(FALSE)
  }
  if (!is.null(event_turn) && !is.null(turn_id) &&
        !identical(event_turn, turn_id)) {
    return(FALSE)
  }
  TRUE
}

artcodex_parse_output <- function(text, output_schema) {
  if (is.null(output_schema)) {
    return(list(value = NULL, error = NULL))
  }
  if (is.null(text) || !nzchar(text)) {
    return(list(
      value = NULL,
      error = list(
        type = "output_parse_error",
        message = "Codex returned no JSON for the requested structured output."
      )
    ))
  }
  tryCatch(
    list(value = artcodex_decode_json(text), error = NULL),
    error = function(error) {
      list(
        value = NULL,
        error = list(
          type = "output_parse_error",
          message = sprintf(
            "Codex returned a final response that was not valid JSON: %s",
            conditionMessage(error)
          )
        )
      )
    }
  )
}

artcodex_condition_details <- function(error) {
  details <- as.list(error)
  details$message <- NULL
  details$call <- NULL
  details <- details[!vapply(details, is.environment, logical(1))]
  list(
    type = class(error)[[1L]],
    message = conditionMessage(error),
    details = if (length(details) > 0L) details else NULL
  )
}

artcodex_build_transcript <- function(input, events, turn) {
  transcript <- list(list(role = "user", content = input))
  messages <- artcodex_agent_messages(events, turn)
  assistant <- lapply(messages, function(item) {
    artcodex_compact_list(list(
      role = "assistant",
      content = item$text %||% "",
      phase = item$phase,
      item_id = item$id
    ))
  })
  c(transcript, assistant)
}

artcodex_turn_error <- function(turn) {
  error <- turn$error
  if (is.null(error)) {
    if (identical(turn$status, "interrupted")) {
      return(list(type = "turn_interrupted", message = "The Codex turn was interrupted."))
    }
    if (!identical(turn$status, "completed")) {
      return(list(
        type = "turn_failed",
        message = sprintf("The Codex turn ended with status '%s'.", turn$status)
      ))
    }
    return(NULL)
  }
  list(
    type = "turn_failed",
    message = error$message %||% "The Codex turn failed.",
    details = error
  )
}

artcodex_new_result <- function(
  thread_id,
  turn_id,
  turn,
  events,
  raw_messages,
  started_at,
  started_clock,
  output_schema = NULL,
  error = NULL,
  stderr = character(),
  input = list()
) {
  final_response <- artcodex_extract_final_response(events, turn)
  parsed <- artcodex_parse_output(final_response, output_schema)
  if (is.null(error)) {
    error <- artcodex_turn_error(turn)
  }
  if (is.null(error)) {
    error <- parsed$error
  }
  items <- artcodex_extract_items(events, turn)
  finished_at <- artcodex_utc_now()
  status <- turn$status %||% if (is.null(error)) "completed" else "failed"

  structure(
    list(
      success = identical(status, "completed") && is.null(error),
      status = status,
      final_response = final_response,
      output = parsed$value,
      thread_id = thread_id,
      turn_id = turn_id,
      error = error,
      usage = artcodex_extract_usage(events, thread_id, turn_id),
      items = items,
      tool_calls = artcodex_extract_tool_calls(items),
      approvals = artcodex_extract_approvals(events, raw_messages),
      input = input,
      transcript = artcodex_build_transcript(input, events, turn),
      events = events,
      turn = turn,
      raw_messages = raw_messages,
      raw_jsonl = vapply(
        raw_messages,
        function(record) record$json %||% "",
        character(1)
      ),
      stderr = stderr,
      timing = list(
        started_at = started_at,
        finished_at = finished_at,
        duration_seconds = artcodex_monotonic_time() - started_clock
      )
    ),
    class = c("artcodex_result", "list")
  )
}

#' @export
print.artcodex_result <- function(x, ...) {
  cat("<artcodex_result>", x$status, "\n")
  cat("  thread:", x$thread_id %||% "<unknown>", "\n")
  cat("  turn:  ", x$turn_id %||% "<unknown>", "\n")
  if (!is.null(x$final_response) && nzchar(x$final_response)) {
    cat("\n", x$final_response, "\n", sep = "")
  } else if (!is.null(x$error$message)) {
    cat("  error: ", x$error$message, "\n", sep = "")
  }
  invisible(x)
}

#' @export
as.character.artcodex_result <- function(x, ...) {
  x$final_response %||% ""
}

artcodex_make_request <- function(id, method, params = list()) {
  list(
    id = as.character(id),
    method = method,
    params = artcodex_as_object(params)
  )
}

artcodex_make_notification <- function(method, params = NULL) {
  artcodex_compact_list(list(method = method, params = params))
}

artcodex_make_response <- function(id, result) {
  list(id = id, result = result)
}

artcodex_make_error_response <- function(id, message, code = -32603) {
  list(
    id = id,
    error = list(code = code, message = message)
  )
}

artcodex_next_id <- function(client) {
  client$next_id <- client$next_id + 1L
  as.character(client$next_id)
}

artcodex_record_message <- function(client, direction, line, message) {
  client$sequence <- client$sequence + 1L
  record <- list(
    sequence = client$sequence,
    timestamp = artcodex_utc_now(),
    direction = direction,
    json = line,
    message = message
  )
  client$raw_messages <- c(client$raw_messages, list(record))
  if (identical(direction, "sent")) {
    client$raw_sent <- c(client$raw_sent, line)
  } else {
    client$raw_received <- c(client$raw_received, line)
  }
  record
}

artcodex_write_message <- function(client, message) {
  artcodex_assert_client(client)
  line <- as.character(artcodex_encode_json(message))
  tryCatch(
    client$process$write_input(paste0(line, "\n")),
    error = function(error) {
      artcodex_abort(
        sprintf("Could not write to Codex app-server: %s", conditionMessage(error)),
        "artcodex_process_error"
      )
    }
  )
  artcodex_record_message(client, "sent", line, message)
  invisible(line)
}

artcodex_send_request <- function(client, method, params = list()) {
  id <- artcodex_next_id(client)
  artcodex_write_message(client, artcodex_make_request(id, method, params))
  id
}

artcodex_send_notification <- function(client, method, params = NULL) {
  artcodex_write_message(client, artcodex_make_notification(method, params))
}

artcodex_send_response <- function(client, id, result) {
  artcodex_write_message(client, artcodex_make_response(id, result))
}

artcodex_send_error_response <- function(client, id, message, code = -32603) {
  artcodex_write_message(
    client,
    artcodex_make_error_response(id, message = message, code = code)
  )
}

artcodex_read_stream <- function(process, stream) {
  reader <- if (identical(stream, "output")) {
    process$read_output_lines
  } else {
    process$read_error_lines
  }
  tryCatch(reader(), error = function(...) character())
}

artcodex_poll_lines <- function(client, timeout_seconds) {
  timeout_ms <- as.integer(max(0, timeout_seconds) * 1000)
  status <- tryCatch(
    client$process$poll_io(timeout_ms),
    error = function(error) {
      artcodex_abort(
        sprintf("Could not poll Codex app-server: %s", conditionMessage(error)),
        "artcodex_process_error"
      )
    }
  )

  output_ready <- unname(status[["output"]]) %in% c("ready", "closed")
  error_ready <- unname(status[["error"]]) %in% c("ready", "closed")
  alive <- isTRUE(client$process$is_alive())

  stderr_lines <- if (error_ready || !alive) {
    artcodex_read_stream(client$process, "error")
  } else {
    character()
  }
  if (length(stderr_lines) > 0L) {
    client$stderr <- c(client$stderr, stderr_lines)
  }

  if (output_ready || !alive) {
    return(artcodex_read_stream(client$process, "output"))
  }
  character()
}

artcodex_process_exit_error <- function(client) {
  stderr_lines <- artcodex_read_stream(client$process, "error")
  if (length(stderr_lines) > 0L) {
    client$stderr <- c(client$stderr, stderr_lines)
  }
  status <- tryCatch(client$process$get_exit_status(), error = function(...) NA)
  details <- paste(client$stderr, collapse = "\n")
  if (!nzchar(details)) {
    details <- "No stderr output was captured."
  }
  artcodex_abort(
    sprintf("Codex app-server exited with status %s. %s", status, details),
    "artcodex_process_error",
    status = status,
    stderr = client$stderr
  )
}

artcodex_next_line <- function(client, timeout_seconds) {
  if (length(client$pending_lines) == 0L) {
    client$pending_lines <- artcodex_poll_lines(client, timeout_seconds)
  }

  if (length(client$pending_lines) == 0L) {
    if (!isTRUE(client$process$is_alive())) {
      # A process can exit immediately after filling its pipe. Drain once more.
      client$pending_lines <- artcodex_read_stream(client$process, "output")
    }
    if (length(client$pending_lines) == 0L &&
          !isTRUE(client$process$is_alive())) {
      artcodex_process_exit_error(client)
    }
  }

  if (length(client$pending_lines) == 0L) {
    return(NULL)
  }
  line <- client$pending_lines[[1L]]
  client$pending_lines <- client$pending_lines[-1L]
  line
}

artcodex_decode_incoming <- function(client, line) {
  message <- tryCatch(
    artcodex_decode_json(line),
    error = function(error) {
      artcodex_record_message(client, "received", line, NULL)
      artcodex_abort(
        sprintf("Codex app-server emitted invalid JSON: %s", conditionMessage(error)),
        "artcodex_protocol_error",
        line = line
      )
    }
  )
  artcodex_record_message(client, "received", line, message)
  message
}

artcodex_is_response <- function(message, id = NULL) {
  response <- is.list(message) &&
    "id" %in% names(message) &&
    any(c("result", "error") %in% names(message))
  if (!response || is.null(id)) {
    return(response)
  }
  identical(as.character(message$id), as.character(id))
}

artcodex_is_server_request <- function(message) {
  is.list(message) &&
    all(c("id", "method") %in% names(message))
}

artcodex_is_notification <- function(message) {
  is.list(message) &&
    !("id" %in% names(message)) &&
    "method" %in% names(message)
}

artcodex_cache_response <- function(client, message) {
  client$responses[[as.character(message$id)]] <- message
  invisible(message)
}

artcodex_take_response <- function(client, id) {
  key <- as.character(id)
  response <- client$responses[[key]]
  client$responses[[key]] <- NULL
  response
}

artcodex_new_event <- function(client, message, server_request = FALSE) {
  client$event_sequence <- client$event_sequence + 1L
  list(
    sequence = client$event_sequence,
    timestamp = artcodex_utc_now(),
    method = message$method,
    params = message$params,
    id = message$id,
    server_request = server_request,
    raw = message
  )
}

artcodex_default_request_result <- function(method) {
  if (method %in% c(
    "item/commandExecution/requestApproval",
    "item/fileChange/requestApproval",
    "execCommandApproval",
    "applyPatchApproval"
  )) {
    return(list(result = list(decision = "decline")))
  }
  if (identical(method, "mcpServer/elicitation/request")) {
    return(list(result = list(action = "decline", content = NULL)))
  }
  if (identical(method, "item/tool/requestUserInput")) {
    return(list(result = list(answers = artcodex_empty_object())))
  }
  if (identical(method, "item/tool/call")) {
    return(list(result = list(success = FALSE, contentItems = list())))
  }
  list(
    error = list(
      code = -32601,
      message = sprintf("artcodex does not handle server request '%s'.", method)
    )
  )
}

artcodex_answer_server_request <- function(
  client,
  event,
  request_handler = NULL
) {
  handler <- request_handler %||% client$request_handler
  answer <- NULL
  if (is.function(handler)) {
    answer <- tryCatch(
      handler(event),
      error = function(error) {
        list(
          error = list(
            code = -32603,
            message = sprintf(
              "artcodex request handler failed: %s",
              conditionMessage(error)
            )
          )
        )
      }
    )
  }
  if (is.null(answer)) {
    answer <- artcodex_default_request_result(event$method)
  } else if (!is.list(answer)) {
    answer <- list(result = answer)
  } else if (!any(c("result", "error") %in% names(answer))) {
    answer <- list(result = artcodex_as_object(answer))
  }

  if (!is.null(answer$error)) {
    if (!is.list(answer$error)) {
      answer$error <- list(message = as.character(answer$error))
    }
    artcodex_send_error_response(
      client,
      event$id,
      answer$error$message %||% "Request rejected by artcodex.",
      code = answer$error$code %||% -32603
    )
  } else {
    artcodex_send_response(client, event$id, answer$result)
  }
  invisible(answer)
}

artcodex_handle_message <- function(
  client,
  message,
  on_event = NULL,
  progress = FALSE,
  request_handler = NULL
) {
  if (artcodex_is_response(message)) {
    return(list(type = "response", value = artcodex_cache_response(client, message)))
  }

  if (artcodex_is_notification(message) ||
        artcodex_is_server_request(message)) {
    server_request <- artcodex_is_server_request(message)
    event <- artcodex_new_event(client, message, server_request)
    client$events <- c(client$events, list(event))
    if (isTRUE(progress)) {
      base::message(event$method)
    }
    if (is.function(on_event)) {
      tryCatch(
        on_event(event),
        error = function(error) {
          artcodex_abort(
            sprintf("on_event callback failed: %s", conditionMessage(error)),
            "artcodex_callback_error"
          )
        }
      )
    }
    if (server_request) {
      artcodex_answer_server_request(client, event, request_handler)
    }
    return(list(type = "event", value = event))
  }

  artcodex_abort(
    "Codex app-server emitted an unrecognized protocol message.",
    "artcodex_protocol_error",
    protocol_message = message
  )
}

artcodex_pump_once <- function(
  client,
  timeout_seconds,
  on_event = NULL,
  progress = FALSE,
  request_handler = NULL
) {
  line <- artcodex_next_line(client, timeout_seconds)
  if (is.null(line)) {
    return(NULL)
  }
  message <- artcodex_decode_incoming(client, line)
  artcodex_handle_message(
    client,
    message,
    on_event = on_event,
    progress = progress,
    request_handler = request_handler
  )
}

artcodex_response_result <- function(response, id) {
  if (!is.null(response$error)) {
    artcodex_abort(
      sprintf(
        "Codex app-server request %s failed: %s",
        id,
        artcodex_request_error_message(response$error)
      ),
      "artcodex_request_error",
      id = id,
      code = response$error$code,
      data = response$error$data
    )
  }
  response$result
}

artcodex_wait_for_response <- function(
  client,
  id,
  timeout = 30,
  on_event = NULL,
  progress = FALSE,
  request_handler = NULL
) {
  deadline <- artcodex_deadline(timeout)
  repeat {
    response <- artcodex_take_response(client, id)
    if (!is.null(response)) {
      return(artcodex_response_result(response, id))
    }
    remaining <- artcodex_remaining(deadline)
    if (remaining <= 0) {
      artcodex_abort(
        sprintf("Timed out waiting for app-server response %s.", id),
        "artcodex_timeout_error",
        id = id
      )
    }
    artcodex_pump_once(
      client,
      min(client$poll_interval, remaining),
      on_event = on_event,
      progress = progress,
      request_handler = request_handler
    )
  }
}

artcodex_find_event <- function(client, predicate, after_sequence) {
  for (event in client$events) {
    if (event$sequence > after_sequence && isTRUE(predicate(event))) {
      return(event)
    }
  }
  NULL
}

artcodex_wait_for_event <- function(
  client,
  predicate,
  after_sequence = 0L,
  timeout = 300,
  on_event = NULL,
  progress = FALSE,
  request_handler = NULL
) {
  deadline <- artcodex_deadline(timeout)
  repeat {
    event <- artcodex_find_event(client, predicate, after_sequence)
    if (!is.null(event)) {
      return(event)
    }
    remaining <- artcodex_remaining(deadline)
    if (remaining <= 0) {
      artcodex_abort(
        "Timed out waiting for a matching app-server event.",
        "artcodex_timeout_error"
      )
    }
    artcodex_pump_once(
      client,
      min(client$poll_interval, remaining),
      on_event = on_event,
      progress = progress,
      request_handler = request_handler
    )
  }
}

artcodex_request <- function(
  client,
  method,
  params = list(),
  timeout = 30,
  on_event = NULL,
  progress = FALSE,
  request_handler = NULL
) {
  artcodex_assert_client(client)
  artcodex_assert_timeout(timeout)
  artcodex_assert_string(method, "method")
  id <- artcodex_send_request(client, method, params)
  artcodex_wait_for_response(
    client,
    id,
    timeout = timeout,
    on_event = on_event,
    progress = progress,
    request_handler = request_handler
  )
}

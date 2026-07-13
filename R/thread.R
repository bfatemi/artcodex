artcodex_new_thread <- function(client, response) {
  thread <- response$thread
  thread_id <- thread$id %||% response$threadId
  if (is.null(thread_id)) {
    artcodex_abort(
      "Codex app-server did not return a thread id.",
      "artcodex_protocol_error",
      response = response
    )
  }
  structure(
    list(
      client = client,
      thread_id = thread_id,
      thread = thread,
      response = response
    ),
    class = c("artcodex_thread", "list")
  )
}

artcodex_assert_named_list <- function(x, name) {
  if (is.null(x)) {
    return(invisible(x))
  }
  invalid <- !is.list(x)
  if (!invalid && length(x) > 0L) {
    invalid <- is.null(names(x)) || any(!nzchar(names(x)))
  }
  if (invalid) {
    artcodex_abort(
      sprintf("%s must be a named list or NULL.", name),
      "artcodex_validation_error"
    )
  }
  invisible(x)
}

artcodex_thread_params <- function(
  cwd,
  model,
  sandbox,
  approval_policy,
  config,
  base_instructions,
  developer_instructions,
  ephemeral,
  extra
) {
  artcodex_assert_optional_string(model, "model")
  artcodex_assert_optional_string(base_instructions, "base_instructions")
  artcodex_assert_optional_string(
    developer_instructions,
    "developer_instructions"
  )
  artcodex_assert_named_list(config, "config")
  if (!is.null(ephemeral)) {
    artcodex_assert_flag(ephemeral, "ephemeral")
  }
  artcodex_assert_named_list(extra, "extra")

  params <- artcodex_compact_list(list(
    cwd = artcodex_normalize_cwd(cwd),
    model = model,
    sandbox = artcodex_validate_sandbox(sandbox),
    approvalPolicy = artcodex_validate_approval_policy(approval_policy),
    config = artcodex_as_object(config),
    baseInstructions = base_instructions,
    developerInstructions = developer_instructions,
    ephemeral = ephemeral
  ))
  artcodex_merge_params(
    params,
    extra,
    reserved = c(
      "cwd", "model", "sandbox", "approvalPolicy", "config",
      "baseInstructions", "developerInstructions", "ephemeral"
    )
  )
}

#' Start a Codex thread
#'
#' A thread holds conversation state and settings across one or more turns.
#' Defaults are deliberately conservative: read-only access and no approval
#' prompts.
#'
#' @param client An `artcodex_client`.
#' @param cwd Working directory available to Codex.
#' @param model Optional model override.
#' @param sandbox Sandbox mode: `"read-only"`, `"workspace-write"`, or
#'   `"danger-full-access"`.
#' @param approval_policy Approval policy. The default, `"never"`, prevents an
#'   unattended pipeline from waiting for approval input.
#' @param config Optional Codex configuration overrides as a list.
#' @param base_instructions Optional replacement base instructions.
#' @param developer_instructions Optional additional developer instructions.
#' @param ephemeral If `TRUE`, do not persist the thread.
#' @param extra Optional named list of additional protocol fields. Named fields
#'   already represented by arguments cannot be replaced through `extra`.
#' @param timeout Maximum number of seconds to wait.
#' @return An `artcodex_thread` object.
#' @export
codex_thread_start <- function(
  client,
  cwd = getwd(),
  model = NULL,
  sandbox = "read-only",
  approval_policy = "never",
  config = NULL,
  base_instructions = NULL,
  developer_instructions = NULL,
  ephemeral = FALSE,
  extra = NULL,
  timeout = 30
) {
  artcodex_assert_client(client)
  artcodex_assert_timeout(timeout)
  params <- artcodex_thread_params(
    cwd,
    model,
    sandbox,
    approval_policy,
    config,
    base_instructions,
    developer_instructions,
    ephemeral,
    extra
  )
  response <- artcodex_request(client, "thread/start", params, timeout)
  artcodex_new_thread(client, response)
}

#' Resume a persisted Codex thread
#'
#' @inheritParams codex_thread_start
#' @param thread_id Identifier returned by an earlier Codex thread.
#' @return An `artcodex_thread` object.
#' @export
codex_thread_resume <- function(
  client,
  thread_id,
  cwd = NULL,
  model = NULL,
  sandbox = NULL,
  approval_policy = NULL,
  config = NULL,
  base_instructions = NULL,
  developer_instructions = NULL,
  extra = NULL,
  timeout = 30
) {
  artcodex_assert_client(client)
  artcodex_assert_string(thread_id, "thread_id")
  artcodex_assert_timeout(timeout)
  params <- artcodex_thread_params(
    cwd,
    model,
    sandbox,
    approval_policy,
    config,
    base_instructions,
    developer_instructions,
    ephemeral = NULL,
    extra = extra
  )
  params <- c(list(threadId = thread_id), params)
  response <- artcodex_request(client, "thread/resume", params, timeout)
  artcodex_new_thread(client, response)
}

#' Archive a Codex thread
#'
#' @param thread An `artcodex_thread`.
#' @param timeout Maximum number of seconds to wait.
#' @return The server response, invisibly.
#' @export
codex_thread_archive <- function(thread, timeout = 30) {
  artcodex_assert_thread(thread)
  artcodex_assert_timeout(timeout)
  response <- artcodex_request(
    thread$client,
    "thread/archive",
    list(threadId = thread$thread_id),
    timeout
  )
  invisible(response)
}

#' Interrupt a running Codex turn
#'
#' @param thread An `artcodex_thread`.
#' @param turn_id Identifier of the running turn.
#' @param timeout Maximum number of seconds to wait.
#' @return The server response, invisibly.
#' @export
codex_turn_interrupt <- function(thread, turn_id, timeout = 10) {
  artcodex_assert_thread(thread)
  artcodex_assert_string(turn_id, "turn_id")
  artcodex_assert_timeout(timeout)
  response <- artcodex_request(
    thread$client,
    "turn/interrupt",
    list(threadId = thread$thread_id, turnId = turn_id),
    timeout
  )
  invisible(response)
}

artcodex_input <- function(prompt, images = NULL) {
  artcodex_assert_string(prompt, "prompt", allow_empty = TRUE)
  if (is.null(images)) {
    images <- character()
  }
  if (!is.character(images) || anyNA(images)) {
    artcodex_abort(
      "images must be a character vector of local image paths.",
      "artcodex_validation_error"
    )
  }
  if (length(images) > 0L && any(!file.exists(images))) {
    missing <- images[!file.exists(images)]
    artcodex_abort(
      sprintf("Local image does not exist: %s", missing[[1L]]),
      "artcodex_validation_error"
    )
  }
  if (length(images) > 0L && any(file.info(images)$isdir %in% TRUE)) {
    artcodex_abort(
      "images must contain file paths, not directories.",
      "artcodex_validation_error"
    )
  }
  if (!nzchar(prompt) && length(images) == 0L) {
    artcodex_abort(
      "prompt cannot be empty unless at least one image is supplied.",
      "artcodex_validation_error"
    )
  }
  input <- list(list(type = "text", text = prompt))
  image_input <- lapply(images, function(path) {
    list(
      type = "localImage",
      path = normalizePath(path, winslash = "/", mustWork = TRUE)
    )
  })
  c(input, image_input)
}

artcodex_turn_start_params <- function(
  thread_id,
  prompt,
  images,
  cwd,
  model,
  approval_policy,
  output_schema,
  extra
) {
  artcodex_assert_optional_string(model, "model")
  artcodex_assert_named_list(output_schema, "output_schema")
  artcodex_assert_named_list(extra, "turn_extra")
  params <- artcodex_compact_list(list(
    threadId = thread_id,
    input = artcodex_input(prompt, images),
    cwd = artcodex_normalize_cwd(cwd),
    model = model,
    approvalPolicy = artcodex_validate_approval_policy(approval_policy),
    outputSchema = artcodex_as_object(output_schema)
  ))
  artcodex_merge_params(
    params,
    extra,
    reserved = c(
      "threadId", "input", "cwd", "model", "approvalPolicy", "outputSchema"
    )
  )
}

artcodex_find_started_turn <- function(client, thread_id, after_sequence) {
  matching <- Filter(
    function(event) {
      event$sequence > after_sequence &&
        identical(event$method, "turn/started") &&
        identical(event$params$threadId, thread_id)
    },
    client$events
  )
  if (length(matching) == 0L) {
    return(NULL)
  }
  utils::tail(matching, 1L)[[1L]]$params$turn$id
}

artcodex_interrupt_safely <- function(thread, turn_id) {
  if (is.null(turn_id) || !codex_client_is_alive(thread$client)) {
    return(FALSE)
  }
  tryCatch(
    {
      codex_turn_interrupt(thread, turn_id, timeout = 2)
      TRUE
    },
    error = function(...) FALSE
  )
}

artcodex_run_thread <- function(
  thread,
  prompt,
  images,
  cwd,
  model,
  approval_policy,
  output_schema,
  turn_extra,
  timeout,
  progress,
  on_event,
  request_handler
) {
  client <- thread$client
  event_start <- client$event_sequence
  message_start <- length(client$raw_messages) + 1L
  stderr_start <- length(client$stderr) + 1L
  started_at <- artcodex_utc_now()
  started_clock <- artcodex_monotonic_time()
  deadline <- artcodex_deadline(timeout)
  turn_id <- NULL
  turn <- list(status = "failed", items = list())
  error <- NULL

  params <- artcodex_turn_start_params(
    thread$thread_id,
    prompt,
    images,
    cwd,
    model,
    approval_policy,
    output_schema,
    turn_extra
  )

  tryCatch({
    response <- artcodex_request(
      client,
      "turn/start",
      params,
      timeout = artcodex_remaining(deadline),
      on_event = on_event,
      progress = progress,
      request_handler = request_handler
    )
    turn_id <- response$turn$id
    if (is.null(turn_id)) {
      artcodex_abort(
        "Codex app-server did not return a turn id.",
        "artcodex_protocol_error",
        response = response
      )
    }
    completed <- artcodex_wait_for_event(
      client,
      predicate = function(event) {
        identical(event$method, "turn/completed") &&
          identical(event$params$threadId, thread$thread_id) &&
          identical(event$params$turn$id, turn_id)
      },
      after_sequence = event_start,
      timeout = artcodex_remaining(deadline),
      on_event = on_event,
      progress = progress,
      request_handler = request_handler
    )
    turn <- completed$params$turn
  }, interrupt = function(condition) {
    error <<- artcodex_condition_details(condition)
    turn_id <<- turn_id %||% artcodex_find_started_turn(
      client,
      thread$thread_id,
      event_start
    )
    if (artcodex_interrupt_safely(thread, turn_id)) {
      turn$status <<- "interrupted"
    }
  }, error = function(condition) {
    error <<- artcodex_condition_details(condition)
    turn_id <<- turn_id %||% artcodex_find_started_turn(
      client,
      thread$thread_id,
      event_start
    )
    if (artcodex_interrupt_safely(thread, turn_id)) {
      turn$status <<- "interrupted"
    }
  })

  events <- Filter(
    function(event) {
      event$sequence > event_start &&
        artcodex_event_matches_turn(event, thread$thread_id, turn_id)
    },
    client$events
  )
  raw_messages <- artcodex_slice(client$raw_messages, message_start)
  stderr <- artcodex_slice(client$stderr, stderr_start)
  artcodex_new_result(
    thread_id = thread$thread_id,
    turn_id = turn_id,
    turn = turn,
    events = events,
    raw_messages = raw_messages,
    started_at = started_at,
    started_clock = started_clock,
    output_schema = output_schema,
    error = error,
    stderr = stderr,
    input = params$input
  )
}

#' Run Codex from R
#'
#' With only a prompt, `codex_run()` starts and cleans up a local app-server and
#' thread automatically. Pass an existing thread to retain conversation state
#' across calls.
#'
#' @param prompt One text prompt. For compatibility, an `artcodex_thread` may be
#'   passed first and the prompt second as `codex_run(thread, prompt)`.
#' @param thread Optional existing `artcodex_thread`.
#' @param client Optional existing `artcodex_client`. When supplied without a
#'   thread, a new thread is created on this client.
#' @param images Optional character vector of local image paths.
#' @param cwd Working directory for a newly created thread. For an existing
#'   thread, `NULL` keeps its current working directory.
#' @param model Optional model override.
#' @param sandbox Sandbox mode for a newly created thread.
#'   Automatic threads default to `"read-only"`. This cannot be changed on an
#'   existing thread through `codex_run()`.
#' @param approval_policy Optional approval policy for the thread or turn.
#'   Automatic threads default to `"never"`; existing threads retain their
#'   policy when this is `NULL`.
#' @param ephemeral Whether an automatically created thread should be
#'   non-persistent. Automatic threads default to `TRUE`.
#' @param output_schema Optional JSON Schema list for structured output. Parsed
#'   JSON is returned in `result$output`.
#' @param config Optional Codex configuration overrides for a new thread.
#' @param base_instructions Optional replacement base instructions for a new
#'   thread.
#' @param developer_instructions Optional additional developer instructions for
#'   a new thread.
#' @param thread_extra Optional additional fields for `thread/start`.
#' @param turn_extra Optional additional fields for `turn/start`.
#' @param timeout Maximum number of seconds for the turn.
#' @param client_timeout Maximum number of seconds for client initialization.
#' @param progress If `TRUE`, print event names while the turn runs.
#' @param on_event Optional function called for each streamed event.
#' @param request_handler Optional function that answers server-initiated
#'   requests. Unhandled requests fail closed.
#' @param stop_on_error If `TRUE`, throw an `artcodex_turn_error` when the result
#'   is unsuccessful. The default returns a serializable failure result.
#' @param command Optional path to the Codex executable for an automatic client.
#' @param client_args Command-line arguments for an automatic client. This is
#'   primarily useful for testing or custom app-server launchers.
#' @param client_env Optional process environment for an automatic client.
#' @return An `artcodex_result`. Use `codex_succeeded()` to test success.
#' @examples
#' \dontrun{
#' result <- codex_run("Summarize the R files in this project.")
#' result$final_response
#'
#' client <- codex_client_start()
#' thread <- codex_thread_start(client)
#' first <- codex_run("Inspect the package.", thread = thread)
#' second <- codex_run("Now suggest three improvements.", thread = thread)
#' codex_client_stop(client)
#' }
#' @export
codex_run <- function(
  prompt,
  thread = NULL,
  client = NULL,
  images = NULL,
  cwd = if (is.null(thread)) getwd() else NULL,
  model = NULL,
  sandbox = NULL,
  approval_policy = NULL,
  ephemeral = NULL,
  output_schema = NULL,
  config = NULL,
  base_instructions = NULL,
  developer_instructions = NULL,
  thread_extra = NULL,
  turn_extra = NULL,
  timeout = 300,
  client_timeout = 30,
  progress = FALSE,
  on_event = NULL,
  request_handler = NULL,
  stop_on_error = FALSE,
  command = NULL,
  client_args = "app-server",
  client_env = NULL
) {
  if (inherits(prompt, "artcodex_thread")) {
    old_thread <- prompt
    prompt <- thread
    thread <- old_thread
  }
  artcodex_assert_string(prompt, "prompt", allow_empty = TRUE)
  artcodex_assert_timeout(timeout)
  artcodex_assert_timeout(client_timeout, "client_timeout")
  artcodex_assert_flag(progress, "progress")
  artcodex_assert_flag(stop_on_error, "stop_on_error")
  artcodex_assert_function(on_event, "on_event")
  artcodex_assert_function(request_handler, "request_handler")
  if (!is.null(thread) && !is.null(client)) {
    artcodex_abort(
      "Supply thread or client, not both.",
      "artcodex_validation_error"
    )
  }

  creating_thread <- is.null(thread)
  if (creating_thread) {
    sandbox <- sandbox %||% "read-only"
    approval_policy <- approval_policy %||% "never"
    ephemeral <- ephemeral %||% TRUE
    artcodex_thread_params(
      cwd,
      model,
      sandbox,
      approval_policy,
      config,
      base_instructions,
      developer_instructions,
      ephemeral = ephemeral,
      extra = thread_extra
    )
    artcodex_turn_start_params(
      "preflight",
      prompt,
      images,
      cwd = NULL,
      model = NULL,
      approval_policy = NULL,
      output_schema = output_schema,
      extra = turn_extra
    )
  } else {
    artcodex_assert_thread(thread)
    if (!is.null(sandbox) || !is.null(ephemeral)) {
      artcodex_abort(
        "sandbox and ephemeral only apply when creating a thread.",
        "artcodex_validation_error"
      )
    }
    has_thread_options <- !is.null(config) ||
      !is.null(base_instructions) ||
      !is.null(developer_instructions) ||
      !is.null(thread_extra)
    if (has_thread_options) {
      artcodex_abort(
        paste(
          "config, instructions, and thread_extra only apply when",
          "creating a thread."
        ),
        "artcodex_validation_error"
      )
    }
    artcodex_turn_start_params(
      thread$thread_id,
      prompt,
      images,
      cwd,
      model,
      approval_policy,
      output_schema,
      turn_extra
    )
  }

  owns_client <- is.null(thread) && is.null(client)
  custom_launcher <- !is.null(command) ||
    !identical(client_args, "app-server") ||
    !is.null(client_env)
  if (owns_client) {
    client <- codex_client_start(
      command = command,
      args = client_args,
      timeout = client_timeout,
      request_handler = request_handler,
      env = client_env
    )
    on.exit(codex_client_stop(client), add = TRUE)
  } else if (custom_launcher) {
    artcodex_abort(
      paste(
        "command, client_args, and client_env can only be used when",
        "codex_run() starts the client."
      ),
      "artcodex_validation_error"
    )
  }

  if (is.null(thread)) {
    artcodex_assert_client(client)
    thread <- codex_thread_start(
      client,
      cwd = cwd,
      model = model,
      sandbox = sandbox,
      approval_policy = approval_policy,
      config = config,
      base_instructions = base_instructions,
      developer_instructions = developer_instructions,
      ephemeral = ephemeral,
      extra = thread_extra,
      timeout = client_timeout
    )
    turn_cwd <- NULL
    turn_model <- NULL
    turn_approval <- NULL
  } else {
    turn_cwd <- cwd
    turn_model <- model
    turn_approval <- approval_policy
  }

  result <- artcodex_run_thread(
    thread,
    prompt,
    images,
    turn_cwd,
    turn_model,
    turn_approval,
    output_schema,
    turn_extra,
    timeout,
    progress,
    on_event,
    request_handler
  )
  if (isTRUE(stop_on_error) && !codex_succeeded(result)) {
    artcodex_abort(
      result$error$message %||% "The Codex turn failed.",
      "artcodex_turn_error",
      result = result
    )
  }
  result
}

#' @export
print.artcodex_thread <- function(x, ...) {
  cat("<artcodex_thread>", x$thread_id, "\n")
  cat(
    "  client:",
    if (codex_client_is_alive(x$client)) "running" else "stopped",
    "\n"
  )
  invisible(x)
}

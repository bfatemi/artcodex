test_that("schema generation supports current and custom launchers", {
  launcher <- fake_app_server_command()
  out <- tempfile("artcodex-schema-")

  path <- codex_generate_schema(
    out = out,
    command = launcher$command,
    command_args = launcher$args[1:2],
    env = launcher$env,
    timeout = 5
  )
  schema <- jsonlite::read_json(file.path(path, "fake-protocol.json"))

  expect_true(dir.exists(path))
  expect_true(schema$experimental)
  expect_error(
    codex_generate_schema(
      out = out,
      command = launcher$command,
      command_args = launcher$args[1:2],
      env = launcher$env
    ),
    "already exists",
    class = "artcodex_validation_error"
  )

  codex_generate_schema(
    out = out,
    command = launcher$command,
    command_args = launcher$args[1:2],
    experimental = FALSE,
    overwrite = TRUE,
    timeout = 5,
    env = launcher$env
  )
  replaced <- jsonlite::read_json(file.path(out, "fake-protocol.json"))
  expect_false(replaced$experimental)
})

test_that("schema generation reports validation and process failures", {
  launcher <- fake_app_server_command()

  expect_error(
    codex_generate_schema(out = 1),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_generate_schema(
      out = tempfile("schema-"),
      command = launcher$command,
      command_args = 1
    ),
    class = "artcodex_validation_error"
  )
  expect_error(
    codex_generate_schema(
      out = tempfile("schema-"),
      command = launcher$command,
      command_args = c(launcher$args[1:2], "schema-fail"),
      timeout = 5,
      env = launcher$env
    ),
    "simulated schema failure",
    class = "artcodex_process_error"
  )
  expect_error(
    codex_generate_schema(
      out = tempfile("schema-"),
      command = tempfile("missing-command-")
    ),
    class = "artcodex_process_error"
  )
})

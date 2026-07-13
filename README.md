# artcodex

<!-- badges: start -->
[![R-CMD-check](https://github.com/bfatemi/artcodex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bfatemi/artcodex/actions/workflows/R-CMD-check.yaml)
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE.md)
<!-- badges: end -->

`artcodex` lets R start and control a local Codex session. It sends work to the
Codex app-server, streams what happens, and returns a structured record that a
script or pipeline can inspect, save, or pass to another step.

This is a community-maintained R client. It is not an official OpenAI package,
and it does not include Codex itself.

## Requirements

- R 4.1 or later.
- A locally installed Codex CLI with `codex app-server` support.
- A working Codex sign-in or other authentication configured for that CLI.

If `codex` is not on `PATH`, set `ARTCODEX_CODEX_PATH` to the executable.

## Installation

```r
# install.packages("pak")
pak::pak("bfatemi/artcodex")
```

## One prompt

For ordinary use, one function owns the process and cleanup:

```r
library(artcodex)

result <- codex_run(
  "Inspect the R files and return a five-bullet package summary.",
  cwd = getwd()
)

if (codex_succeeded(result)) {
  cat(result$final_response)
}
```

The defaults are read-only file access and no interactive approval prompts.
Writing to the project must be requested explicitly:

```r
result <- codex_run(
  "Update NEWS.md for the current release and run the tests.",
  cwd = getwd(),
  sandbox = "workspace-write",
  approval_policy = "never"
)
```

## Structured output

Pass a JSON Schema when the next pipeline step needs data instead of prose:

```r
schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = list("summary", "risk"),
  properties = list(
    summary = list(type = "string"),
    risk = list(type = "string", enum = list("low", "medium", "high"))
  )
)

result <- codex_run(
  "Assess this package.",
  output_schema = schema
)

result$output$summary
result$output$risk
```

## Reusable sessions

Pipelines can own a client and retain conversation state across turns:

```r
client <- codex_client_start()
on.exit(codex_client_stop(client), add = TRUE)

thread <- codex_thread_start(
  client,
  cwd = getwd(),
  sandbox = "read-only",
  approval_policy = "never"
)

inventory <- codex_run("Inventory the package.", thread = thread)
review <- codex_run("Review the three highest-risk areas.", thread = thread)

codex_thread_archive(thread)
```

Persisted threads can be restored with `codex_thread_resume()`. Running turns
can be stopped with `codex_turn_interrupt()`.

## Result contract

`codex_run()` returns an `artcodex_result`. It is a plain, serializable list
with stable top-level fields:

- `success`, `status`, and `error` describe the outcome.
- `final_response` contains the final text.
- `output` contains parsed JSON when `output_schema` was supplied.
- `thread_id` and `turn_id` support traceability and resumption.
- `events`, `items`, `tool_calls`, and `approvals` expose execution details.
- `input` and `transcript` provide a normalized conversation record.
- `usage` contains the latest token-usage event when Codex emits one.
- `raw_messages` and `raw_jsonl` retain the protocol exchange for diagnosis.
- `stderr` retains app-server diagnostic output observed during the turn.
- `timing` records start, finish, and elapsed duration.

Unsuccessful turns return a failure result by default, which is useful in
batch jobs. Set `stop_on_error = TRUE` when an R error should stop execution.
Automatically created one-call threads are ephemeral, so they do not add
conversation history to the local Codex store. Explicit threads are persistent
by default and can be resumed.

## Tools and approvals

Codex can only use tools, plugins, skills, and MCP servers available to the
local Codex runtime. Installing `artcodex` does not grant access to integrations
that have not been configured and authenticated there.

The client automatically answers unhandled approval and elicitation requests
conservatively. Advanced callers can supply a `request_handler` function to
return a protocol response for each server request. Every such request remains
available in `result$events`, and approval-related requests are also collected
in `result$approvals`.

## Protocol and development

The client uses JSON-RPC-style messages over the app-server's default standard
input/output transport. `codex_generate_schema()` exports the protocol schema
from the installed CLI, which is useful when validating a new Codex release.

Deterministic tests use a local fake app-server and need no account. A gated
live smoke test is available to maintainers:

```r
Sys.setenv(ARTCODEX_LIVE_TESTS = "1")
testthat::test_file("tests/testthat/test-live-smoke.R")
```

`ARTCODEX_LIVE_MODEL` and `ARTCODEX_LIVE_REASONING_EFFORT` can override local
model settings for this smoke test when validating an older CLI release.

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development workflow and
[SECURITY.md](SECURITY.md) for private vulnerability reporting.

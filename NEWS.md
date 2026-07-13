# artcodex 0.1.0

## First public release

- Added a one-call `codex_run()` interface with conservative read-only defaults.
- Added explicit app-server client lifecycle management and health information.
- Added thread start, resume, archive, and turn interruption helpers.
- Added streamed events, configurable server-request handling, and fail-closed
  defaults for unattended execution.
- Added local image input and JSON Schema-constrained structured output.
- Added status-aware, serializable results with items, tool calls, approvals,
  transcripts, usage, raw protocol messages, app-server diagnostics,
  identifiers, errors, and timing.
- Added protocol schema generation for compatibility checks.
- Added deterministic fake-server tests, a gated live smoke test, continuous
  integration, and CRAN-oriented package checks.

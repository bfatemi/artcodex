# Contributing to artcodex

Thanks for improving `artcodex`. Bug reports, protocol compatibility findings,
documentation fixes, and focused pull requests are welcome.

## Before opening an issue

Search existing issues, then include the operating system, R version, Codex CLI
version from `codex_version()`, a minimal example, and the complete R error. Do
not post credentials, authentication files, private prompts, or raw messages
that contain sensitive project content.

## Development workflow

1. Fork and clone the repository.
2. Install development dependencies with
   `pak::pak(c("deps::.", "devtools", "rcmdcheck", "roxygen2"))`.
3. Add or update deterministic tests under `tests/testthat/`.
4. Run `devtools::document()`, `testthat::test_local()`,
   `lintr::lint_package()`, and `rcmdcheck::rcmdcheck()`.
5. Update `NEWS.md` for behavior visible to package users.
6. Open a focused pull request describing the protocol and user impact.

Unit tests must not depend on a Codex account. Live tests remain opt-in through
`ARTCODEX_LIVE_TESTS=1` and must use read-only settings unless the test itself
explicitly verifies a write boundary.

By participating, you agree to follow [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

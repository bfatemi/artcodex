## R CMD check results

0 errors | 0 warnings | 2 notes

The first note identifies this as a new submission.

The second note is:

```
checking for future file timestamps ... NOTE
unable to verify current time
```

This appears to be an environmental clock-verification limitation on the local
Windows check machine. The package source contains no future-dated files.

## Test environments

- Windows, R release
- Ubuntu, R release
- Ubuntu, R devel

## Downstream dependencies

This is a new package and has no downstream dependencies.

## External software

Most tests use a deterministic fake app-server implemented with `Rscript`.
Tests that require the external Codex CLI, authentication, or network access are
disabled on CRAN and run only when `ARTCODEX_LIVE_TESTS=1` is set explicitly.

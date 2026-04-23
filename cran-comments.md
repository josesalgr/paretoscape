## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission

This is a resubmission of `multiscape`.

In this version we addressed the issues raised in the previous CRAN review:
- added references for the methodological background in the `Description` field;
- revised examples to avoid unnecessary `\dontrun{}` usage;
- simplified and corrected examples so they run more robustly during checks;
- clarified the availability of optional solver backends in the `Description`.

## CRAN check notes

### 1. checking package dependencies ... NOTE

Packages suggested but not available for checking:
`Rsymphony`, `Rcplex`, `slam`, `gurobi`

These packages are listed in `Suggests` because they provide optional solver-specific functionality and are used conditionally.

The `Description` field explicitly states how non-mainstream suggested packages can be obtained:
- `'gurobi'` is distributed with the Gurobi Optimizer installation;
- `'rcbc'` is available from GitHub at <https://github.com/dirkschumacher/rcbc>.

### 2. checking installed package size ... NOTE

Installed size is 9.0Mb, mainly due to example data, documentation, and compiled code.

### 3. checking for future file timestamps ... NOTE

This NOTE was observed in a local Windows check and appears to be environment-specific.

## Test environments

* local Windows 10 x64, R 4.4.1
* GitHub Actions:
  * ubuntu-latest (oldrel-1, release, devel)
  * windows-latest (release)
  * macos-latest (release)

## Downstream dependencies

There are no reverse dependencies.

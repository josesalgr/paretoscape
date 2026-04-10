## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission 

This is the first release of `multiscape`.

Main changes in this version include:

## CRAN check notes

There are 3 NOTE(s):

### 1. checking package dependencies ... NOTE

Packages suggested but not available for checking:
`Rsymphony`, `Rcplex`, `slam`, `gurobi`

These packages are listed in `Suggests` because they provide optional solver-specific functionality. The package is designed so that solver backends are used conditionally, depending on which solver package is available in the user environment.


### 2. checking installed package size ... NOTE

Installed size is 9.0Mb.

This size is mainly due to:

* `extdata` (2.3Mb), which contains example data used to illustrate the package workflow;
* `help` (3.2Mb), due to package documentation;
* `libs` (1.8Mb), due to compiled code required by the package.

We believe this size is reasonable for the functionality provided.

### 3. checking for future file timestamps ... NOTE

`unable to verify current time`

This appears to be related to the local checking environment on Windows and not to the package contents.

## Test environments

* local Windows 10 x64, R 4.4.1
* GitHub Actions:
  * ubuntu-latest (oldrel-1, release, devel)
  * windows-latest (release)
  * macos-latest (release)

## Downstream dependencies

There are no reverse dependencies.

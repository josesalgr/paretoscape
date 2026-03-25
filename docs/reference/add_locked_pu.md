# Add locked planning units to a problem

Define planning units that must be included in, or excluded from, the
optimization problem.

This function updates the planning-unit table stored in `x$data$pu` by
creating or replacing the logical columns `locked_in` and `locked_out`.
These columns are later used by the model builder when translating the
`Problem` object into optimization constraints.

Lock information may be supplied directly as logical vectors, as vectors
of planning-unit ids, or by referencing columns in the raw planning-unit
data originally passed to
[`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md).
In addition, a Marxan-style `pu_status` specification can be provided,
where status code `2` denotes locked-in planning units and status code
`3` denotes locked-out planning units.

## Usage

``` r
add_locked_pu(
  x,
  locked_in = NULL,
  locked_out = NULL,
  pu_status = NULL,
  overwrite = TRUE,
  status_overrides = FALSE
)
```

## Arguments

- x:

  A `Problem` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md).

- locked_in:

  Optional locked-in specification. It may be `NULL`, a column name in
  `x$data$pu_data_raw`, a logical vector, or a vector of planning-unit
  ids.

- locked_out:

  Optional locked-out specification. It may be `NULL`, a column name in
  `x$data$pu_data_raw`, a logical vector, or a vector of planning-unit
  ids.

- pu_status:

  Optional Marxan-style status specification. It may be `NULL`, a column
  name in `x$data$pu_data_raw`, or a vector of length `nrow(x$data$pu)`.
  Values `2` indicate locked-in planning units and values `3` indicate
  locked-out planning units.

- overwrite:

  Logical. If `TRUE`, replace any existing `locked_in` and `locked_out`
  columns in `x$data$pu`. If `FALSE`, merge new values with existing
  ones using logical OR.

- status_overrides:

  Logical. If `TRUE`, `pu_status` overrides explicit or existing
  `locked_in` and `locked_out` assignments. If `FALSE`, explicit
  `locked_in` and `locked_out` inputs take precedence over `pu_status`.

## Value

An updated `Problem` object in which `x$data$pu` contains logical
columns `locked_in` and `locked_out`.

## Details

Let \\\mathcal{P}\\ denote the set of planning units and let \\w_i \in
\\0,1\\\\ denote the binary variable indicating whether planning unit
\\i \in \mathcal{P}\\ is selected by the model.

This function defines two subsets:

- \\\mathcal{P}^{in} \subseteq \mathcal{P}\\, the planning units that
  must be included,

- \\\mathcal{P}^{out} \subseteq \mathcal{P}\\, the planning units that
  must be excluded.

Conceptually, these sets correspond to the following conditions:

- if \\i \in \mathcal{P}^{in}\\, then \\w_i = 1\\,

- if \\i \in \mathcal{P}^{out}\\, then \\w_i = 0\\.

These constraints are not imposed immediately by this function; instead,
they are stored in `x$data$pu$locked_in` and `x$data$pu$locked_out` and
enforced later when building the optimization model.

**Philosophy**

The role of
[`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
is to construct and normalize the basic inputs of the planning problem.
Locking planning units is treated as a separate modelling step so that
users can define or revise selection restrictions after the `Problem`
object has already been created.

**Supported input formats**

For `locked_in` and `locked_out`, the function accepts:

- `NULL`, meaning that no explicit update is supplied for that side,

- a single character string, interpreted as a column name in
  `x$data$pu_data_raw`,

- a logical vector of length `nrow(x$data$pu)`,

- a vector of planning-unit ids.

For `pu_status`, the function accepts:

- `NULL`,

- a single character string, interpreted as a column name in
  `x$data$pu_data_raw`,

- a vector of length `nrow(x$data$pu)` containing status codes.

When `pu_status` is used, values are interpreted as follows:

- `2`: locked in,

- `3`: locked out,

- all other values: treated as free.

**Priority rules**

By default, explicit `locked_in` and `locked_out` inputs take precedence
over `pu_status`. This means that `pu_status` is first translated into
candidate locked-in and locked-out sets, and then explicit `locked_in`
and `locked_out` assignments are applied on top of those values.

If `status_overrides = TRUE`, then `pu_status` overrides any existing or
explicitly supplied `locked_in` and `locked_out` assignments.

**Overwrite behaviour**

If `overwrite = TRUE`, the function replaces any existing `locked_in`
and `locked_out` columns in `x$data$pu`.

If `overwrite = FALSE`, new locked values are merged with any existing
values using logical OR. This means that already locked planning units
remain locked unless the object is rebuilt or overwritten explicitly.

**Consistency checks**

The function checks that no planning unit is simultaneously assigned to
both `locked_in` and `locked_out`. If such conflicts are found, an error
is raised.

## See also

[`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md),
[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_locked_actions`](https://josesalgr.github.io/mosap/reference/add_locked_actions.md)

## Examples

``` r
pu <- data.frame(
  id = 1:5,
  cost = c(2, 3, 1, 4, 2),
  lock_col = c(TRUE, FALSE, FALSE, TRUE, FALSE),
  status_col = c(0, 2, 0, 3, 0)
)

features <- data.frame(
  id = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4, 5),
  feature = c("sp1", "sp2", "sp1", "sp2", "sp1", "sp2"),
  amount = c(1, 2, 1, 3, 2, 1)
)

p <- inputData(
  pu = pu,
  features = features,
  dist_features = dist_features
)
#> Error: features$id must be numeric/integer ids (got non-numeric strings).

# 1) Lock by planning-unit ids
p1 <- add_locked_pu(
  x = p,
  locked_in = c(1, 3),
  locked_out = c(5)
)
#> Error: object 'p' not found

p1$data$pu[, c("id", "locked_in", "locked_out")]
#> Error: object 'p1' not found

# 2) Read lock information from raw PU data columns
p2 <- add_locked_pu(
  x = p,
  locked_in = "lock_col",
  overwrite = TRUE
)
#> Error: object 'p' not found

p2$data$pu[, c("id", "locked_in", "locked_out")]
#> Error: object 'p2' not found

# 3) Use Marxan-style status codes
p3 <- add_locked_pu(
  x = p,
  pu_status = "status_col",
  overwrite = TRUE
)
#> Error: object 'p' not found

p3$data$pu[, c("id", "locked_in", "locked_out")]
#> Error: object 'p3' not found

# 4) Use logical vectors
p4 <- add_locked_pu(
  x = p,
  locked_in = c(TRUE, FALSE, TRUE, FALSE, FALSE),
  locked_out = c(FALSE, FALSE, FALSE, TRUE, FALSE),
  overwrite = TRUE
)
#> Error: object 'p' not found

p4$data$pu[, c("id", "locked_in", "locked_out")]
#> Error: object 'p4' not found
```

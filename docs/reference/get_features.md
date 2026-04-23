# Get feature summary from a solution

Extract the per-feature summary table from a `Solution` or `SolutionSet`
object returned by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

The returned table summarizes, for each feature, the total amount
available in the landscape together with the positive and negative
contributions induced by the selected actions in the solution.

## Usage

``` r
get_features(x, run = NULL)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

- run:

  Optional positive integer giving the run index to extract from a
  `SolutionSet`. If `NULL`, all runs are returned when available.

## Value

A `data.frame` with one row per feature, or one row per feature–run
combination when multiple runs are present. The returned table always
includes the columns `total_available`, `benefit`, `loss`, `net`, and
`total`.

## Details

This function reads the feature summary stored in `x$summary$features`.
It errors if that table is missing.

Let \\B_f\\ denote the total baseline amount available in the landscape
for feature \\f\\. Let \\G_f\\ denote the total positive contribution
induced by selected actions, and let \\L_f\\ denote the total negative
contribution. Then the returned table is intended to summarize
quantities of the form:

\$\$ \mathrm{net}\_f = G_f - L_f, \$\$

\$\$ \mathrm{total}\_f = B_f + \mathrm{net}\_f. \$\$

In the stored summary, these quantities are typically represented by the
columns:

- `total_available`, corresponding to \\B_f\\,

- `benefit`, corresponding to \\G_f\\,

- `loss`, corresponding to \\L_f\\,

- `net`, corresponding to \\G_f - L_f\\,

- `total`, corresponding to \\B_f + G_f - L_f\\.

If any of the core numeric columns `total_available`, `benefit`, or
`loss` are missing, they are created and filled with zero. If `net` is
missing, it is computed as `benefit - loss`. If `total` is missing, it
is computed as `total_available + net`.

Thus, this function guarantees that the returned table contains the
columns `total_available`, `benefit`, `loss`, `net`, and `total`, even
if some of them were absent from the stored summary.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. If the result contains a `run_id` column but only
a single run is present and `run` was not requested explicitly, the
`run_id` column is removed for convenience.

This function summarizes feature outcomes in the solution. It is
different from
[`get_targets`](https://josesalgr.github.io/multiscape/reference/get_targets.md),
which focuses on target achievement rather than total feature balance.

## See also

[`get_pu`](https://josesalgr.github.io/multiscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/multiscape/reference/get_actions.md),
[`get_targets`](https://josesalgr.github.io/multiscape/reference/get_targets.md)

## Examples

``` r
# \donttest{
if (requireNamespace("rcbc", quietly = TRUE)) {
  pu_tbl <- data.frame(
    id = 1:4,
    cost = c(1, 2, 3, 4)
  )

  feat_tbl <- data.frame(
    id = 1:2,
    name = c("feature_1", "feature_2")
  )

  dist_feat_tbl <- data.frame(
    pu = c(1, 1, 2, 3, 4),
    feature = c(1, 2, 2, 1, 2),
    amount = c(5, 2, 3, 4, 1)
  )

  actions_df <- data.frame(
    id = "conservation",
    name = "conservation"
  )

  effects_df <- data.frame(
    pu = c(1, 2, 3, 4),
    action = "conservation",
    feature = c(1, 1, 2, 2),
    benefit = c(2, 1, 1, 2),
    loss = c(0, 0, 0, 0)
  )

  p <- create_problem(
    pu = pu_tbl,
    features = feat_tbl,
    dist_features = dist_feat_tbl,
    cost = "cost"
  ) |>
    add_actions(actions_df, cost = 0) |>
    add_effects(effects_df) |>
    add_constraint_targets_relative(0.2) |>
    add_objective_min_cost() |>
    set_solver_cbc(time_limit = 10)

  sol <- solve(p)

  get_features(sol)
}
#>   feature feature_name total_available benefit loss net total
#> 1       1    feature_1               9       2    0   2    11
#> 2       2    feature_2               6       2    0   2     8
# }
```

# Get planning-unit results from a solution

Extract the planning-unit summary table from a `Solution` or
`SolutionSet` object returned by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

The returned table summarizes solution values at the planning-unit level
and typically includes a `selected` indicator showing whether each
planning unit is selected in the solution.

## Usage

``` r
get_pu(x, only_selected = FALSE, run = NULL)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

- only_selected:

  Logical. If `TRUE`, return only rows where `selected == 1`. Default is
  `FALSE`.

- run:

  Optional positive integer giving the run index to extract from a
  `SolutionSet`. If `NULL`, all runs are returned when available.

## Value

A `data.frame` containing the stored planning-unit summary. Typical
columns include planning-unit identifiers, optional labels, and a
`selected` indicator.

## Details

This function reads the planning-unit summary stored in `x$summary$pu`.
It does not reconstruct the table from the raw decision vector; it
simply returns the stored summary after optional filtering.

Let \\w_i\\ denote the planning-unit selection variable for planning
unit \\i\\. In standard `multiscape` workflows, the `selected` column is
the user-facing representation of that planning-unit decision, typically
coded as `0` or `1`.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. This requires the summary table to contain a
`run_id` column.

If `only_selected = TRUE`, only rows with `selected == 1` are returned.
This requires the summary table to contain a `selected` column.

This function is intended for user-facing inspection of planning-unit
results. For the raw model variable vector, use
[`get_solution_vector`](https://josesalgr.github.io/multiscape/reference/get_solution_vector.md).

## See also

[`get_actions`](https://josesalgr.github.io/multiscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/multiscape/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/multiscape/reference/get_targets.md),
[`get_solution_vector`](https://josesalgr.github.io/multiscape/reference/get_solution_vector.md)

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

  get_pu(sol)
  get_pu(sol, only_selected = TRUE)
}
#>   id cost locked_in locked_out internal_id selected
#> 1  1    1     FALSE      FALSE           1        1
#> 4  4    4     FALSE      FALSE           4        1
# }
```

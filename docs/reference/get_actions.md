# Get action results from a solution

Extract the action-allocation summary table from a `Solution` or
`SolutionSet` object returned by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

The returned table summarizes solution values at the planning
unit–action level and typically includes a `selected` indicator showing
whether each feasible `(pu, action)` pair is selected in the solution.

## Usage

``` r
get_actions(x, only_selected = FALSE, run = NULL)
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

A `data.frame` containing the stored action-allocation summary. Typical
columns include planning-unit ids, action ids, optional labels, and a
`selected` indicator.

## Details

This function reads the action summary stored in `x$summary$actions`. It
does not reconstruct the table from the raw decision vector; it simply
returns the stored summary after optional filtering.

Let \\x\_{ia}\\ denote the decision variable associated with selecting
action \\a\\ in planning unit \\i\\. In standard `multiscape` workflows,
the `selected` column is the user-facing representation of that
decision, typically coded as `0` or `1`.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. This requires the summary table to contain a
`run_id` column.

If `only_selected = TRUE`, only rows with `selected == 1` are returned.
This requires the summary table to contain a `selected` column.

This function is intended for user-facing inspection of action
allocations. For the raw model variable vector, use
[`get_solution_vector`](https://josesalgr.github.io/multiscape/reference/get_solution_vector.md).

## See also

[`get_pu`](https://josesalgr.github.io/multiscape/reference/get_pu.md),
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

  get_actions(sol)
  get_actions(sol, only_selected = TRUE)
}
#>   pu       action cost status selected
#> 1  1 conservation    0      0        1
#> 4  4 conservation    0      0        1
# }
```
